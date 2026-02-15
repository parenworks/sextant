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
