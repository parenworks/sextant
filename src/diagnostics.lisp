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

(defun extract-source-path-position (text)
  "Extract a precise source position from SBCL's source-path data when available.
Source-paths are set on IR nodes and encode the exact form path.
Returns (line . col) or NIL."
  (when (and (boundp 'sb-c::*compiler-error-context*)
             (symbol-value 'sb-c::*compiler-error-context*))
    (let ((ctx (symbol-value 'sb-c::*compiler-error-context*)))
      (when (ignore-errors (slot-boundp ctx 'sb-c::source-path))
        (let ((sp (ignore-errors (slot-value ctx 'sb-c::source-path))))
          (when (and sp (listp sp))
            (extract-position-from-source-path sp text)))))))

(defun extract-compiler-context-position (text)
  "Extract source position from SBCL's *compiler-error-context*.
Tries source-path first (precise), then file-position (form-level only).
Returns (line . col) or NIL."
  (when (and (boundp 'sb-c::*compiler-error-context*)
             (symbol-value 'sb-c::*compiler-error-context*))
    (let ((ctx (symbol-value 'sb-c::*compiler-error-context*)))
      (or
       ;; Source-path: precise sub-form position (IR nodes only)
       (when (ignore-errors (slot-boundp ctx 'sb-c::source-path))
         (let ((sp (ignore-errors (slot-value ctx 'sb-c::source-path))))
           (when (and sp (listp sp))
             (extract-position-from-source-path sp text))))
       ;; File-position: position after reading the enclosing top-level form.
       ;; Rough — points to the form boundary, not the exact error site.
       (when (typep ctx 'sb-c::compiler-error-context)
         (let ((fpos (ignore-errors
                       (slot-value ctx 'sb-c::file-position))))
           (when (and fpos (integerp fpos) (> fpos 0))
             (offset-to-line-col text (min fpos (length text))))))))))


(defun extract-position-from-source-path (source-path text)
  "Convert an SBCL source-path to (line . col).
SOURCE-PATH is like (ORIGINAL-SOURCE-START form-idx subform-idx ...) where
each index after the top-level form index navigates into a nested sub-form."
  (let ((oss-pos (position 'sb-c::original-source-start source-path)))
    (when oss-pos
      (let* ((indices (subseq source-path (1+ oss-pos)))
             (form-idx (first indices))
             (sub-indices (rest indices)))
        (when (and form-idx (integerp form-idx))
          (navigate-source-path text form-idx sub-indices))))))

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

(defun read-into-nth-subform (stream idx)
  "With STREAM at or before the opening paren of a list form, position STREAM
at the start of the IDX-th element (0-indexed) within that list.
Returns the file-position of the IDX-th sub-form, or NIL if not found."
  (handler-case
      (progn
        ;; Skip whitespace to the opening paren
        (peek-char t stream nil nil)
        (let ((c (peek-char nil stream nil nil)))
          (unless (and c (char= c #\())
            (return-from read-into-nth-subform nil)))
        (read-char stream) ; consume (
        ;; Skip IDX sub-forms
        (dotimes (i idx)
          (let ((sub (read stream nil :eof)))
            (when (eq sub :eof)
              (return-from read-into-nth-subform nil))))
        ;; Skip whitespace to the IDX-th sub-form
        (peek-char t stream nil nil)
        (file-position stream))
    (error () nil)))

(defun navigate-source-path (text form-idx sub-indices)
  "Find the precise source position indicated by FORM-IDX and SUB-INDICES.
Locates the FORM-IDX-th top-level form, then descends into sub-forms following
each index in SUB-INDICES using stream file-positions. This avoids both
(length list) on dotted pairs and princ-to-string ambiguity for repeated forms.
Returns (line . col) or NIL."
  (handler-case
      (with-input-from-string (stream text)
        ;; Advance to the target top-level form, recording its start position
        (let ((form-start nil))
          (dotimes (i (1+ form-idx))
            (skip-whitespace-and-comments stream)
            (let ((pos (file-position stream)))
              (let ((form (read stream nil :eof)))
                (when (eq form :eof) (return-from navigate-source-path nil))
                (when (= i form-idx)
                  (setf form-start pos)))))
          (unless form-start
            (return-from navigate-source-path nil))
          ;; No sub-indices: return position of the top-level form itself
          (when (null sub-indices)
            (return-from navigate-source-path
              (offset-to-line-col text (min form-start (length text)))))
          ;; Descend into sub-forms by tracking stream file-positions
          (let ((current-pos form-start))
            (dolist (idx sub-indices)
              (file-position stream current-pos)
              (let ((sub-pos (read-into-nth-subform stream idx)))
                (unless sub-pos
                  (return-from navigate-source-path
                    (offset-to-line-col text (min form-start (length text)))))
                (setf current-pos sub-pos)))
            (offset-to-line-col text (min current-pos (length text))))))
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

(defun position-in-comment-or-string-p (text offset)
  "Return T if OFFSET in TEXT falls inside a line comment, block comment, string
literal, or pipe-escaped symbol (|...|).
Handles #\\x character literals to avoid false positives from #\\; etc.
Note: #; (read-time suppress) is not handled and is a known limitation."
  (let ((i 0)
        (len (length text))
        (in-string nil)
        (escape nil))
    (loop while (< i offset)
          do (let ((c (char text i)))
               (cond
                 ;; Inside a string: only escape and closing quote matter
                 (in-string
                  (cond
                    (escape (setf escape nil))
                    ((char= c #\\) (setf escape t))
                    ((char= c #\") (setf in-string nil))))
                 ;; Dispatching macros starting with #
                 ((char= c #\#)
                  (let ((next (and (< (1+ i) len) (char text (1+ i)))))
                    (cond
                      ;; #| ... |# nestable block comment
                      ((and next (char= next #\|))
                       (incf i 2) ; consume #|
                       (let ((depth 1))
                         (loop while (and (< i offset) (> depth 0))
                               do (cond
                                    ((and (char= (char text i) #\#)
                                          (< (1+ i) len)
                                          (char= (char text (1+ i)) #\|))
                                     (incf depth) (incf i 2))
                                    ((and (char= (char text i) #\|)
                                          (< (1+ i) len)
                                          (char= (char text (1+ i)) #\#))
                                     (decf depth) (incf i 2))
                                    (t (incf i))))
                         (when (> depth 0)
                           (return-from position-in-comment-or-string-p t)))
                       (decf i)) ; outer loop will incf
                      ;; #\x character literal: consume #, \, and one char (total 3)
                      ((and next (char= next #\\))
                       (incf i 2)) ; consume #\ and the literal char; outer incf passes it
                      ;; Other # forms: let the outer loop advance normally
                      (t nil))))
                 ;; Pipe-escaped symbol: |...| — treat interior as non-code
                 ((char= c #\|)
                  (incf i) ; consume opening |
                  (loop
                    (when (>= i offset)
                      (return-from position-in-comment-or-string-p t))
                    (when (>= i len) (return))
                    (let ((pc (char text i)))
                      (cond
                        ((char= pc #\\) (incf i 2)) ; escaped char inside pipes
                        ((char= pc #\|) (return))   ; end of pipe symbol
                        (t (incf i)))))
                  ;; i now points to closing | (or ran past len); decf so outer incf passes it
                  (decf i))
                 ;; Line comment: ; ... \n
                 ((char= c #\;)
                  (let ((eol (or (position #\Newline text :start i) len)))
                    (when (< offset eol)
                      (return-from position-in-comment-or-string-p t))
                    (setf i eol)
                    (decf i))) ; outer loop will incf
                 ;; String open
                 ((char= c #\") (setf in-string t))))
             (incf i))
    ;; If we ended still inside a string literal, the offset is inside it
    in-string))

(defun find-symbol-position-in-text (text symbol-name)
  "Find the first occurrence of SYMBOL-NAME in TEXT that is in code (not a
comment or string) and is a whole-symbol match (word boundaries).
Returns (line . col) or NIL."
  (when (and text symbol-name (> (length symbol-name) 0))
    (let* ((downcased (string-downcase text))
           (target (string-downcase symbol-name))
           (tlen (length target))
           (tlen-text (length text))
           (pos 0))
      (loop
        (let ((found (search target downcased :start2 pos)))
          (unless found (return nil))
          ;; Word-boundary check: chars immediately outside the match must not
          ;; be symbol constituents, except that ':' and '#' are valid left
          ;; boundaries because they are package/uninterned prefixes (pkg::foo,
          ;; pkg:foo, #:foo) — not continuations of a different symbol name.
          (let ((left-ok  (or (zerop found)
                              (let ((lc (char text (1- found))))
                                (or (not (symbol-char-p lc))
                                    (char= lc #\:)
                                    (char= lc #\#)))))
                (right-ok (or (>= (+ found tlen) tlen-text)
                              (not (symbol-char-p (char text (+ found tlen)))))))
            (when (and left-ok right-ok
                       (not (position-in-comment-or-string-p text found)))
              (return (offset-to-line-col text found))))
          (setf pos (1+ found)))))))

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
      (end-of-file ()
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
                        (let* ((sym     (extract-symbol-from-warning c))
                               ;; Best: symbol search with word-boundary + comment/string awareness
                               (sym-pos (when sym (find-symbol-position-in-text text sym)))
                               ;; Fallback: SBCL source-path (imprecise for style warnings)
                               (sp-pos  (when (null sym-pos)
                                          (extract-source-path-position text)))
                               ;; Rough fallback: enclosing form boundary
                               (ctx-pos (when (and (null sym-pos) (null sp-pos))
                                          (extract-position-from-condition c text))))
                          (push (make-captured-condition
                                 :message msg
                                 :severity (condition-severity c)
                                 :position (or sym-pos sp-pos ctx-pos)
                                 :source-form sym)
                                conditions))))
                    (muffle-warning c)))
                (error
                  (lambda (c)
                    (let* ((msg     (princ-to-string c))
                           (sym     (extract-symbol-from-warning c))
                           (sym-pos (when sym (find-symbol-position-in-text text sym)))
                           (sp-pos  (when (null sym-pos)
                                      (extract-source-path-position text)))
                           (ctx-pos (when (and (null sym-pos) (null sp-pos))
                                      (extract-position-from-condition c text))))
                      (push (make-captured-condition
                             :message msg
                             :severity +severity-error+
                             :position (or sym-pos sp-pos ctx-pos))
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
            "diagnostics" (if (null diagnostics)
                              (json-empty-array)
                              diagnostics)))))
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
