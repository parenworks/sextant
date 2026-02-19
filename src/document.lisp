(in-package :sextant)

;;; ============================================================
;;; Document Management
;;; Tracks open files and provides symbol-at-position lookup
;;; ============================================================

(defvar *documents* (make-hash-table :test 'equal)
  "Map of URI -> document content (string).")

(defun uri-to-path (uri)
  "Convert a file:// URI to a filesystem path."
  (if (and (>= (length uri) 7)
           (string= "file://" (subseq uri 0 7)))
      (subseq uri 7)
      uri))

(defun path-to-uri (path)
  "Convert a filesystem path to a file:// URI."
  (if (and (>= (length path) 7)
           (string= "file://" (subseq path 0 7)))
      path
      (concatenate 'string "file://" path)))

(defun document-open (uri text)
  "Register an opened document."
  (setf (gethash uri *documents*) text)
  (lsp-log "Document opened: ~a (~d chars)" uri (length text)))

(defun document-change (uri text)
  "Update document content (full sync)."
  (setf (gethash uri *documents*) text))

(defun apply-incremental-change (uri change)
  "Apply a single incremental CHANGE to the document at URI.
CHANGE is a JSON object with 'range' and 'text' keys.
Range has 'start' and 'end' positions, each with 'line' and 'character'."
  (let ((text (gethash uri *documents*))
        (range (json-get change "range"))
        (new-text (json-get change "text")))
    (when text
      (if range
          ;; Incremental: replace the range
          (let* ((start-pos (json-get range "start"))
                 (end-pos (json-get range "end"))
                 (start-offset (line-col-to-offset text
                                                    (json-get start-pos "line")
                                                    (json-get start-pos "character")))
                 (end-offset (line-col-to-offset text
                                                  (json-get end-pos "line")
                                                  (json-get end-pos "character"))))
            (setf (gethash uri *documents*)
                  (concatenate 'string
                               (subseq text 0 start-offset)
                               new-text
                               (subseq text end-offset))))
          ;; No range means full replacement
          (setf (gethash uri *documents*) new-text)))))

(defun document-close (uri)
  "Remove a closed document."
  (remhash uri *documents*))

(defun document-text (uri)
  "Get the current text of a document."
  (gethash uri *documents*))

(defun line-col-to-offset (text line col)
  "Convert 0-based LINE and COL to a character offset in TEXT."
  (let ((pos 0)
        (current-line 0))
    (loop while (and (< pos (length text))
                     (< current-line line))
          do (when (char= (char text pos) #\Newline)
               (incf current-line))
             (incf pos))
    (min (+ pos col) (length text))))

(defun offset-to-line-col (text offset)
  "Convert character OFFSET to (line . col) in TEXT."
  (let ((line 0)
        (col 0))
    (loop for i from 0 below (min offset (length text))
          do (if (char= (char text i) #\Newline)
                 (progn (incf line) (setf col 0))
                 (incf col)))
    (cons line col)))

(defun symbol-at-position (text line col)
  "Extract the Lisp symbol at LINE, COL in TEXT.
Returns the symbol string or NIL."
  (let* ((offset (line-col-to-offset text line col))
         (len (length text)))
    (when (and (> len 0) (<= offset len))
      ;; Find start of symbol
      (let ((start offset)
            (end offset))
        ;; Scan backward
        (loop while (and (> start 0)
                         (symbol-char-p (char text (1- start))))
              do (decf start))
        ;; Scan forward
        (loop while (and (< end len)
                         (symbol-char-p (char text end)))
              do (incf end))
        (when (> end start)
          (subseq text start end))))))

(defun symbol-char-p (c)
  "Return T if C can be part of a Lisp symbol."
  (and (not (member c '(#\Space #\Tab #\Newline #\Return
                         #\( #\) #\' #\" #\` #\, #\;)))
       (graphic-char-p c)))

;;; ============================================================
;;; S-expression Range Utilities
;;; ============================================================

(defun find-sexp-at (text offset)
  "Find the innermost s-expression containing OFFSET in TEXT.
Returns (start . end) character offsets, or NIL."
  (let ((len (length text))
        (best-start nil)
        (best-end nil))
    ;; Walk through finding all parens, track nesting
    (let ((paren-stack nil)
          (in-string nil)
          (escape nil))
      (loop for i from 0 below len
            for c = (char text i)
            do (cond
                 (escape (setf escape nil))
                 ((char= c #\\) (setf escape t))
                 ((char= c #\")
                  (if in-string
                      (setf in-string nil)
                      (setf in-string t)))
                 (in-string nil)
                 ((char= c #\;)
                  ;; Skip comment to end of line
                  (loop while (and (< i (1- len))
                                   (not (char= (char text (1+ i)) #\Newline)))
                        do (incf i)))
                 ((char= c #\()
                  (push i paren-stack))
                 ((char= c #\))
                  (when paren-stack
                    (let ((start (pop paren-stack))
                          (end (1+ i)))
                      ;; If offset is within this sexp, track it
                      (when (and (<= start offset) (<= offset end))
                        (when (or (null best-start)
                                  (> start best-start))
                          (setf best-start start
                                best-end end)))))))))
    (when best-start
      (cons best-start best-end))))

(defun find-all-enclosing-sexps (text offset)
  "Find all s-expressions enclosing OFFSET, innermost first.
Returns a list of (start . end) pairs."
  (let ((len (length text))
        (paren-pairs nil)
        (in-string nil)
        (escape nil)
        (paren-stack nil))
    ;; First pass: collect all matching paren pairs
    (loop for i from 0 below len
          for c = (char text i)
          do (cond
               (escape (setf escape nil))
               ((char= c #\\) (setf escape t))
               ((char= c #\")
                (if in-string
                    (setf in-string nil)
                    (setf in-string t)))
               (in-string nil)
               ((char= c #\;)
                (loop while (and (< i (1- len))
                                 (not (char= (char text (1+ i)) #\Newline)))
                      do (incf i)))
               ((char= c #\()
                (push i paren-stack))
               ((char= c #\))
                (when paren-stack
                  (let ((start (pop paren-stack)))
                    (push (cons start (1+ i)) paren-pairs))))))
    ;; Filter to those containing offset, sort innermost first
    (let ((enclosing (remove-if-not
                      (lambda (pair)
                        (and (<= (car pair) offset)
                             (<= offset (cdr pair))))
                      paren-pairs)))
      (sort enclosing #'> :key (lambda (p) (car p))))))

(defun find-all-symbol-occurrences (text symbol-name)
  "Find all occurrences of SYMBOL-NAME in TEXT.
Returns list of (start-offset . end-offset) pairs."
  (let ((results nil)
        (len (length text))
        (slen (length symbol-name))
        (uname (string-upcase symbol-name)))
    (loop for i from 0 below len
          do (when (and (<= (+ i slen) len)
                        (string-equal uname (subseq text i (+ i slen)))
                        ;; Check boundaries
                        (or (zerop i) (not (symbol-char-p (char text (1- i)))))
                        (or (= (+ i slen) len) (not (symbol-char-p (char text (+ i slen))))))
               (push (cons i (+ i slen)) results)))
    (nreverse results)))

(defun find-top-level-forms (text)
  "Find all top-level forms in TEXT.
Returns list of (start-offset end-offset start-line end-line) tuples."
  (let ((len (length text))
        (forms nil)
        (in-string nil)
        (escape nil)
        (depth 0)
        (form-start nil))
    (loop for i from 0 below len
          for c = (char text i)
          do (cond
               (escape (setf escape nil))
               ((char= c #\\) (setf escape t))
               ((char= c #\")
                (if in-string
                    (setf in-string nil)
                    (setf in-string t)))
               (in-string nil)
               ((char= c #\;)
                ;; Skip comment
                (loop while (and (< i (1- len))
                                 (not (char= (char text (1+ i)) #\Newline)))
                      do (incf i)))
               ((char= c #\()
                (when (zerop depth)
                  (setf form-start i))
                (incf depth))
               ((char= c #\))
                (decf depth)
                (when (and (zerop depth) form-start)
                  (let ((start-line (count #\Newline text :end form-start))
                        (end-line (count #\Newline text :end (1+ i))))
                    (push (list form-start (1+ i) start-line end-line) forms))
                  (setf form-start nil)))))
    (nreverse forms)))

(defun format-lisp-text (text)
  "Format Common Lisp TEXT with proper indentation.
Returns the formatted text string."
  (let ((lines (split-string-by-newline text))
        (result nil)
        (depth 0))
    (dolist (line lines)
      (let* ((trimmed (string-trim '(#\Space #\Tab) line))
             ;; Count how many net parens close at the start
             (leading-closes (count-leading-closes trimmed)))
        ;; Decrease depth for leading close parens
        (decf depth leading-closes)
        (when (< depth 0) (setf depth 0))
        ;; Indent
        (let ((indent (make-string (* 2 depth) :initial-element #\Space)))
          (push (if (zerop (length trimmed))
                    ""
                    (concatenate 'string indent trimmed))
                result))
        ;; Update depth based on all parens in this line
        (let ((net (count-net-parens trimmed)))
          (incf depth (+ net leading-closes))
          (when (< depth 0) (setf depth 0)))))
    (format nil "~{~a~^~%~}" (nreverse result))))

(defun split-string-by-newline (string)
  "Split STRING by newline characters."
  (let ((result nil)
        (start 0))
    (loop for i from 0 below (length string)
          do (when (char= (char string i) #\Newline)
               (push (subseq string start i) result)
               (setf start (1+ i))))
    (push (subseq string start) result)
    (nreverse result)))

(defun count-net-parens (line)
  "Count net open parens in LINE (open minus close), respecting strings and comments."
  (let ((net 0)
        (in-string nil)
        (escape nil))
    (loop for c across line
          do (cond
               (escape (setf escape nil))
               ((char= c #\\) (setf escape t))
               ((char= c #\")
                (if in-string
                    (setf in-string nil)
                    (setf in-string t)))
               (in-string nil)
               ((char= c #\;) (return))
               ((char= c #\() (incf net))
               ((char= c #\)) (decf net))))
    net))

(defun count-leading-closes (line)
  "Count close parens at the start of LINE (after whitespace)."
  (let ((count 0))
    (loop for c across line
          do (cond
               ((member c '(#\Space #\Tab)) nil)
               ((char= c #\)) (incf count))
               (t (return))))
    count))

(defun find-references-in-documents (sym-name)
  "Search all open documents for occurrences of SYM-NAME.
Returns a list of LSP Location objects."
  (let ((results nil))
    (maphash
     (lambda (doc-uri doc-text)
       (let ((occurrences (find-all-symbol-occurrences doc-text sym-name)))
         (dolist (occ occurrences)
           (let ((start-lc (offset-to-line-col doc-text (car occ)))
                 (end-lc (offset-to-line-col doc-text (cdr occ))))
             (push (make-json-object
                    "uri" doc-uri
                    "range" (make-json-object
                             "start" (make-json-object
                                      "line" (car start-lc)
                                      "character" (cdr start-lc))
                             "end" (make-json-object
                                    "line" (car end-lc)
                                    "character" (cdr end-lc))))
                   results)))))
     *documents*)
    (nreverse results)))

(defun find-definition-in-documents (name)
  "Search all open documents for a (def* NAME ...) form.
Returns (uri line col) or NIL."
  (let ((uname (string-upcase name))
        (result nil))
    (maphash
     (lambda (uri text)
       (unless result
         (let ((len (length text)))
           (loop for i from 0 below len
                 do (when (and (char= (char text i) #\()
                               (< (+ i 4) len)
                               (string-equal "def" (subseq text (1+ i)
                                                            (min (+ i 4) len))))
                      (let* ((space-pos (position-if
                                         (lambda (ch) (member ch '(#\Space #\Tab #\Newline)))
                                         text :start (1+ i)))
                             (name-start (when space-pos
                                           (position-if-not
                                            (lambda (ch) (member ch '(#\Space #\Tab #\Newline)))
                                            text :start space-pos)))
                             (name-end (when name-start
                                         (position-if
                                          (lambda (ch) (member ch '(#\Space #\Tab #\Newline #\( #\))))
                                          text :start name-start))))
                        (when (and name-start name-end
                                   (string-equal uname (subseq text name-start name-end)))
                          (let* ((def-line (count #\Newline text :end name-start))
                                 (prev-nl (position #\Newline text :end name-start :from-end t))
                                 (line-start (if prev-nl (1+ prev-nl) 0))
                                 (def-col (- name-start line-start)))
                            (setf result (list uri def-line def-col))))))))))
     *documents*)
    result))

(defun find-symbol-range-at (text line col)
  "Find the range of the symbol at LINE, COL in TEXT.
Returns (start-line start-col end-line end-col) or NIL."
  (let* ((offset (line-col-to-offset text line col))
         (len (length text)))
    (when (and (> len 0) (<= offset len))
      (let ((start offset)
            (end offset))
        (loop while (and (> start 0)
                         (symbol-char-p (char text (1- start))))
              do (decf start))
        (loop while (and (< end len)
                         (symbol-char-p (char text end)))
              do (incf end))
        (when (> end start)
          (let ((start-lc (offset-to-line-col text start))
                (end-lc (offset-to-line-col text end)))
            (list (car start-lc) (cdr start-lc)
                  (car end-lc) (cdr end-lc))))))))
