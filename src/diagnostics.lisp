(in-package :sextant)

;;; ============================================================
;;; Diagnostics
;;; Compiles buffer text and captures SBCL conditions as LSP
;;; diagnostics (warnings, errors, style hints).
;;; ============================================================

;;; LSP DiagnosticSeverity constants
(defconstant +severity-error+       1)
(defconstant +severity-warning+     2)
(defconstant +severity-information+ 3)
(defconstant +severity-hint+        4)

(defvar *diagnostics-debounce-time* 0.5
  "Seconds to wait after last change before running diagnostics.")

(defvar *diagnostics-timer* nil
  "Timer for debounced diagnostics.")

(defvar *diagnostics-lock* (bt:make-lock "diagnostics-lock")
  "Lock for diagnostics state.")

;;; --- Condition capture during compilation ---

(defstruct captured-condition
  "A condition captured during buffer compilation."
  (message "" :type string)
  (severity +severity-warning+ :type integer)
  (position nil)
  (source-form nil))

(defun condition-severity (condition)
  "Map a CL condition to an LSP diagnostic severity."
  (cond
    ((typep condition 'style-warning)  +severity-hint+)
    ((typep condition 'warning)        +severity-warning+)
    ((typep condition 'error)          +severity-error+)
    (t                                 +severity-information+)))

;;; --- Source location extraction from SBCL compiler internals ---

(defun extract-compiler-context-position (text)
  "Extract source position from SBCL's *compiler-error-context*.
Returns (line . col) or NIL."
  (when (and (boundp 'sb-c::*compiler-error-context*)
             (symbol-value 'sb-c::*compiler-error-context*))
    (let ((ctx (symbol-value 'sb-c::*compiler-error-context*)))
      (or
       ;; COMPILER-ERROR-CONTEXT has FILE-POSITION (byte offset)
       (when (typep ctx 'sb-c::compiler-error-context)
         (let ((fpos (ignore-errors
                       (slot-value ctx 'sb-c::file-position))))
           (when (and fpos (integerp fpos) (> fpos 0))
             (offset-to-line-col text (min fpos (length text))))))
       ;; IR nodes (COMBINATION, BIND, etc.) have SOURCE-PATH
       ;; containing (ORIGINAL-SOURCE-START form-idx subform...)
       (when (ignore-errors (slot-boundp ctx 'sb-c::source-path))
         (let ((sp (ignore-errors (slot-value ctx 'sb-c::source-path))))
           (when (and sp (listp sp))
             (extract-position-from-source-path sp text))))))))

(defun extract-position-from-source-path (source-path text)
  "Convert an SBCL source-path to (line . col).
SOURCE-PATH is like (ORIGINAL-SOURCE-START form-idx subform-idx ...)."
  ;; Find ORIGINAL-SOURCE-START marker and get the top-level form index
  (let ((oss-pos (position 'sb-c::original-source-start source-path)))
    (when oss-pos
      (let* ((indices (subseq source-path (1+ oss-pos)))
             (form-idx (first indices)))
        (when (and form-idx (integerp form-idx))
          ;; Find the start position of the nth top-level form
          (find-nth-toplevel-form-position text form-idx))))))

(defun skip-whitespace-and-comments (stream)
  "Advance STREAM past whitespace and line comments.
Returns the file-position of the first non-whitespace, non-comment character."
  (loop
    (let ((c (peek-char nil stream nil nil)))
      (cond
        ((null c) (return (file-position stream)))
        ((member c '(#\Space #\Tab #\Newline #\Return #\Page))
         (read-char stream))
        ((char= c #\;)
         ;; Skip to end of line
         (loop for ch = (read-char stream nil nil)
               while (and ch (not (char= ch #\Newline)))))
        ;; Skip #| ... |# block comments
        ((char= c #\#)
         (let ((next (progn (read-char stream)
                            (peek-char nil stream nil nil))))
           (if (and next (char= next #\|))
               (progn
                 (read-char stream) ; consume |
                 (let ((depth 1))
                   (loop while (> depth 0)
                         for ch = (read-char stream nil nil)
                         while ch
                         do (cond
                              ((and (char= ch #\#)
                                    (eql (peek-char nil stream nil nil) #\|))
                               (read-char stream)
                               (incf depth))
                              ((and (char= ch #\|)
                                    (eql (peek-char nil stream nil nil) #\#))
                               (read-char stream)
                               (decf depth))))))
               ;; Not a block comment - back up and return
               (progn
                 (file-position stream (1- (file-position stream)))
                 (return (file-position stream))))))
        (t (return (file-position stream)))))))

(defun find-nth-toplevel-form-position (text n)
  "Find the character position of the Nth top-level form (0-indexed) in TEXT.
Returns (line . col) or NIL."
  (handler-case
      (with-input-from-string (stream text)
        (let ((form-count 0))
          (loop
            ;; Skip whitespace and comments to find actual form start
            (let ((pos (skip-whitespace-and-comments stream)))
              ;; Read the next form
              (let ((form (read stream nil :eof)))
                (when (eq form :eof)
                  (return nil))
                (when (= form-count n)
                  ;; This is the form we want
                  (return (offset-to-line-col text (min pos (length text)))))
                (incf form-count))))))
    (error () nil)))

(defun extract-position-from-condition (condition text)
  "Try to extract a source position from CONDITION.
Returns (line . col) or NIL."
  ;; First try SBCL compiler context (most accurate)
  (let ((ctx-pos (extract-compiler-context-position text)))
    (when ctx-pos
      (return-from extract-position-from-condition ctx-pos)))
  ;; Fall back to parsing the condition message
  (let ((msg (princ-to-string condition)))
    ;; Try to find 'at N' or 'position N' patterns
    (multiple-value-bind (match groups)
        (cl-ppcre:scan-to-strings "(?i)(?:at|position)\\s+(\\d+)" msg)
      (declare (ignore match))
      (when groups
        (let ((offset (ignore-errors (parse-integer (aref groups 0)))))
          (when (and offset (> offset 0))
            (return-from extract-position-from-condition
              (offset-to-line-col text (min offset (length text))))))))
    ;; Try 'line N' pattern
    (multiple-value-bind (match groups)
        (cl-ppcre:scan-to-strings "(?i)line\\s+(\\d+)" msg)
      (declare (ignore match))
      (when groups
        (let ((line-num (ignore-errors (parse-integer (aref groups 0)))))
          (when (and line-num (> line-num 0))
            (return-from extract-position-from-condition
              (cons (1- line-num) 0))))))
    nil))

(defun extract-symbol-from-warning (condition)
  "Try to extract a symbol name from a warning condition message."
  (let ((msg (princ-to-string condition)))
    ;; Match SBCL patterns like:
    ;;   'The variable X is defined but never used'
    ;;   'undefined function: PKG::SYMBOL'
    ;;   'undefined variable: PKG::SYMBOL'
    ;;   'The function FOO is called with...'
    (or
     ;; 'undefined function/variable: PKG::SYMBOL' - extract just the symbol part
     (multiple-value-bind (match groups)
         (cl-ppcre:scan-to-strings
          "(?i)undefined (?:function|variable):\\s+(?:[A-Z0-9-]+::)?([A-Z0-9*+/<>=.!?_-]+)" msg)
       (declare (ignore match))
       (when groups (aref groups 0)))
     ;; 'The variable/function X is ...'
     (multiple-value-bind (match groups)
         (cl-ppcre:scan-to-strings
          "(?i)The (?:variable|function)\\s+([A-Z0-9*+/<>=.!?_-]+)\\s+is" msg)
       (declare (ignore match))
       (when groups (aref groups 0)))
     ;; 'Constant "..." conflicts' - extract the function containing it
     (multiple-value-bind (match groups)
         (cl-ppcre:scan-to-strings
          "(?i)Constant\\s+\"([^\"]+)\"" msg)
       (declare (ignore match))
       (when groups (aref groups 0))))))

(defun find-symbol-position-in-text (text symbol-name)
  "Find the best occurrence of SYMBOL-NAME in TEXT.
Prefers occurrences in code (not comments). Returns (line . col) or NIL."
  (when (and text symbol-name (> (length symbol-name) 0))
    (let* ((downcased (string-downcase text))
           (target (string-downcase symbol-name)))
      ;; Search for the symbol, skipping comment lines
      (let ((pos 0))
        (loop
          (let ((found (search target downcased :start2 pos)))
            (unless found (return nil))
            ;; Check if this occurrence is inside a comment
            (let ((line-start (or (position #\Newline text :end found :from-end t) -1)))
              (let ((before-on-line (subseq text (1+ line-start) found)))
                (unless (search ";" before-on-line)
                  ;; Not in a comment - return this position
                  (return (offset-to-line-col text found)))))
            (setf pos (1+ found))))))))

(defvar *diagnostics-temp-file*
  (merge-pathnames "sextant-diag.lisp" (uiop:temporary-directory))
  "Temp file path for compile-file based diagnostics.")

(defvar *diagnostics-fasl-file*
  (merge-pathnames "sextant-diag.fasl" (uiop:temporary-directory))
  "Temp fasl output path.")

(defun noise-warning-p (message)
  "Return T if MESSAGE is ASDF/SBCL startup noise we should ignore."
  (or (search "redefining" message)
      (search "ASDF" message)))

(defun check-reader-errors (text)
  "Pre-check TEXT for reader errors (unbalanced parens, bad syntax).
Returns a list of captured-condition structs, empty if text reads cleanly."
  (let ((errors nil))
    (handler-case
        (with-input-from-string (stream text)
          (loop for form = (read stream nil :eof)
                until (eq form :eof)))
      (end-of-file (c)
        (push (make-captured-condition
               :message "Unexpected end of file - unbalanced parentheses?"
               :severity +severity-error+
               :position (cons (count #\Newline text) 0))
              errors))
      (reader-error (c)
        (push (make-captured-condition
               :message (princ-to-string c)
               :severity +severity-error+
               :position (extract-position-from-condition c text))
              errors))
      (error (c)
        (push (make-captured-condition
               :message (format nil "Read error: ~a" (princ-to-string c))
               :severity +severity-error+
               :position nil)
              errors)))
    (nreverse errors)))

(defun compile-buffer-for-diagnostics (text uri)
  "Compile TEXT via compile-file and collect diagnostics.
Returns a list of captured-condition structs."
  (declare (ignore uri))
  ;; Pre-check for reader errors (compile-file swallows these)
  (let ((reader-errors (check-reader-errors text)))
    (when reader-errors
      (return-from compile-buffer-for-diagnostics reader-errors)))
  (let ((conditions nil)
        (tmp-src *diagnostics-temp-file*)
        (tmp-fasl *diagnostics-fasl-file*))
    (unwind-protect
         (progn
           ;; Write buffer text to temp file
           (with-open-file (f tmp-src :direction :output
                                      :if-exists :supersede
                                      :external-format :utf-8)
             (write-string text f))
           ;; Compile with condition handlers
           (handler-bind
               ((warning
                  (lambda (c)
                    (let ((msg (princ-to-string c)))
                      (unless (noise-warning-p msg)
                        (let* ((sym (extract-symbol-from-warning c))
                               (sym-pos (when sym
                                          (find-symbol-position-in-text text sym)))
                               (ctx-pos (extract-position-from-condition c text)))
                          (push (make-captured-condition
                                 :message msg
                                 :severity (condition-severity c)
                                 ;; Prefer symbol-level position over form-level
                                 :position (or sym-pos ctx-pos)
                                 :source-form sym)
                                conditions))))
                    (muffle-warning c)))
                (error
                  (lambda (c)
                    (let ((msg (princ-to-string c)))
                      (push (make-captured-condition
                             :message msg
                             :severity +severity-error+
                             :position (or (extract-position-from-condition c text)
                                           (let ((sym (extract-symbol-from-warning c)))
                                             (when sym
                                               (find-symbol-position-in-text text sym)))))
                            conditions))
                    ;; Return what we have so far
                    (return-from compile-buffer-for-diagnostics
                      (nreverse conditions)))))
             (let ((*error-output* (make-broadcast-stream)))
               (compile-file tmp-src
                             :output-file tmp-fasl
                             :print nil
                             :verbose nil))))
      ;; Cleanup temp files
      (ignore-errors (delete-file tmp-src))
      (ignore-errors (delete-file tmp-fasl)))
    (nreverse conditions)))

;;; --- Convert captured conditions to LSP diagnostics ---

(defun token-length-at (text offset)
  "Return the length of the Lisp token starting at OFFSET in TEXT.
Used to highlight the full symbol in the editor."
  (let ((len (length text))
        (end offset))
    (when (< offset len)
      ;; If it starts with (, highlight to matching )
      (if (char= (char text offset) #\()
          (let ((depth 1))
            (incf end)
            (loop while (and (< end len) (> depth 0))
                  do (let ((c (char text end)))
                       (cond ((char= c #\() (incf depth))
                             ((char= c #\)) (decf depth))))
                     (incf end)))
          ;; Otherwise scan to end of symbol
          (loop while (and (< end len)
                           (symbol-char-p (char text end)))
                do (incf end))))
    (max 1 (- end offset))))

(defun make-diagnostic-range (position text &optional source-form)
  "Create an LSP range from a captured position.
POSITION may be (line . col), an integer offset, or NIL.
SOURCE-FORM, if provided, is used to determine highlight width."
  (let ((line 0) (col 0) (end-col 1))
    (cond
      ;; (line . col) pair
      ((consp position)
       (setf line (car position)
             col (cdr position))
       ;; Calculate highlight width
       (let ((offset (line-col-to-offset text line col)))
         (setf end-col (+ col (if source-form
                                   (length (string-downcase
                                            (princ-to-string source-form)))
                                   (token-length-at text offset))))))
      ;; Character offset
      ((integerp position)
       (let ((lc (offset-to-line-col text (min position (length text)))))
         (setf line (car lc)
               col (cdr lc)
               end-col (+ col (token-length-at text position)))))
      ;; No position - highlight full first line
      (t nil))
    (make-json-object
     "start" (make-json-object "line" line "character" col)
     "end" (make-json-object "line" line "character" end-col))))

(defun captured-condition-to-diagnostic (cc text)
  "Convert a captured-condition to an LSP Diagnostic JSON object."
  (let* ((msg (captured-condition-message cc))
         (cleaned (clean-diagnostic-message msg))
         (code (diagnostic-code msg))
         (base (make-json-object
                "range" (make-diagnostic-range (captured-condition-position cc) text
                                               (captured-condition-source-form cc))
                "severity" (captured-condition-severity cc)
                "source" "sextant"
                "message" cleaned)))
    (when code
      (push (cons "code" code) base))
    base))

;;; --- Message cleanup ---

(defun clean-diagnostic-message (message)
  "Clean up SBCL diagnostic messages for display.
Removes sandbox package prefixes and SBCL manual references."
  (let ((cleaned message))
    ;; Remove sandbox package prefixes (SEXTANT::, COMMON-LISP-USER::, etc.)
    (setf cleaned (cl-ppcre:regex-replace-all
                   "(?:SEXTANT|COMMON-LISP-USER|SEXTANT-DIAG-[0-9]+)::" cleaned ""))
    ;; Clean up extra whitespace from removals
    (setf cleaned (cl-ppcre:regex-replace-all "\\s+" cleaned " "))
    (string-trim '(#\Space) cleaned)))

(defun diagnostic-code (message)
  "Return a short diagnostic code string based on the warning MESSAGE."
  (cond
    ((search "never used" message)      "unused-variable")
    ((search "undefined function" message) "undefined-function")
    ((search "undefined variable" message) "undefined-variable")
    ((search "is called with" message)  "wrong-argument-count")
    ((search "conflicts with its asserted type" message) "type-mismatch")
    ((search "Unexpected end of file" message) "unbalanced-parens")
    ((search "Read error" message)      "reader-error")
    (t nil)))

;;; --- Public interface ---

(defun compute-diagnostics (uri text)
  "Compute diagnostics for document at URI with content TEXT.
Returns a list of LSP Diagnostic JSON objects."
  (lsp-log "Computing diagnostics for ~a (~d chars)" uri (length text))
  (let ((conditions (handler-case
                        (compile-buffer-for-diagnostics text uri)
                      (error (e)
                        (lsp-log "Diagnostics error: ~a" e)
                        nil))))
    (lsp-log "Found ~d diagnostics" (length conditions))
    (mapcar (lambda (cc) (captured-condition-to-diagnostic cc text))
            conditions)))

(defun publish-diagnostics (uri diagnostics)
  "Send textDocument/publishDiagnostics notification to the client."
  (let ((notification
          (make-notification
           "textDocument/publishDiagnostics"
           (make-json-object
            "uri" uri
            "diagnostics" diagnostics))))
    (write-lsp-message notification *lsp-output*)))

(defun run-diagnostics (uri)
  "Compute and publish diagnostics for document URI."
  (let ((text (document-text uri)))
    (when text
      (let ((diagnostics (compute-diagnostics uri text)))
        (publish-diagnostics uri diagnostics)))))

;;; --- Debounced diagnostics (avoid running on every keystroke) ---

(defun schedule-diagnostics (uri)
  "Schedule diagnostics for URI with debouncing.
Cancels any pending run and waits before executing."
  (bt:with-lock-held (*diagnostics-lock*)
    ;; Cancel existing timer
    (when *diagnostics-timer*
      (ignore-errors (bt:destroy-thread *diagnostics-timer*))
      (setf *diagnostics-timer* nil))
    ;; Schedule new run
    (setf *diagnostics-timer*
          (bt:make-thread
           (lambda ()
             (sleep *diagnostics-debounce-time*)
             (handler-case
                 (run-diagnostics uri)
               (error (e)
                 (lsp-log "Diagnostics thread error: ~a" e)))
             (bt:with-lock-held (*diagnostics-lock*)
               (setf *diagnostics-timer* nil)))
           :name "sextant-diagnostics"))))
