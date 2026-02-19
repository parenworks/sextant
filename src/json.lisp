(in-package :sextant)

;;; ============================================================
;;; Minimal JSON reader/writer
;;; No external dependencies - just enough for LSP JSON-RPC
;;; ============================================================

;;; --- JSON Writing ---

(defun json-write (obj stream)
  "Write OBJ as JSON to STREAM."
  (etypecase obj
    (null (write-string "null" stream))
    ((eql t) (write-string "true" stream))
    ((eql :false) (write-string "false" stream))
    ((eql :empty-array) (write-string "[]" stream))
    ((eql :null) (write-string "null" stream))
    (integer (format stream "~d" obj))
    (float (format stream "~f" obj))
    (string (json-write-string obj stream))
    (keyword (json-write-string (string-downcase (symbol-name obj)) stream))
    (hash-table (json-write-object obj stream))
    (list (if (json-alist-p obj)
              (json-write-alist obj stream)
              (json-write-array obj stream)))))

(defun json-write-string (s stream)
  "Write S as a JSON string with escaping."
  (write-char #\" stream)
  (loop for c across s do
    (case c
      (#\" (write-string "\\\"" stream))
      (#\\ (write-string "\\\\" stream))
      (#\Newline (write-string "\\n" stream))
      (#\Return (write-string "\\r" stream))
      (#\Tab (write-string "\\t" stream))
      (t (if (< (char-code c) 32)
             (format stream "\\u~4,'0x" (char-code c))
             (write-char c stream)))))
  (write-char #\" stream))

(defun json-write-object (ht stream)
  "Write hash-table HT as a JSON object."
  (write-char #\{ stream)
  (let ((first t))
    (maphash (lambda (k v)
               (if first (setf first nil) (write-char #\, stream))
               (json-write-string (etypecase k
                                    (string k)
                                    (keyword (string-downcase (symbol-name k))))
                                  stream)
               (write-char #\: stream)
               (json-write v stream))
             ht))
  (write-char #\} stream))

(defun json-alist-p (list)
  "Return T if LIST looks like an alist (list of (key . value) pairs)."
  (and (consp list)
       (consp (first list))
       (or (stringp (car (first list)))
           (keywordp (car (first list))))))

(defun json-write-alist (alist stream)
  "Write ALIST as a JSON object."
  (write-char #\{ stream)
  (let ((first t))
    (dolist (pair alist)
      (if first (setf first nil) (write-char #\, stream))
      (json-write-string (etypecase (car pair)
                           (string (car pair))
                           (keyword (string-downcase (symbol-name (car pair)))))
                         stream)
      (write-char #\: stream)
      (json-write (cdr pair) stream)))
  (write-char #\} stream))

(defun json-write-array (list stream)
  "Write LIST as a JSON array."
  (write-char #\[ stream)
  (let ((first t))
    (dolist (item list)
      (if first (setf first nil) (write-char #\, stream))
      (json-write item stream)))
  (write-char #\] stream))

(defun json-to-string (obj)
  "Serialize OBJ to a JSON string."
  (with-output-to-string (s)
    (json-write obj s)))

;;; --- JSON Reading ---

(defun json-parse (string)
  "Parse a JSON STRING into Lisp objects.
Objects become alists, arrays become lists, strings stay strings,
numbers become numbers, true->T, false->:FALSE, null->NIL."
  (let ((pos 0)
        (len (length string)))
    (labels
        ((peek ()
           (when (< pos len) (char string pos)))
         (advance ()
           (prog1 (char string pos) (incf pos)))
         (skip-ws ()
           (loop while (and (< pos len)
                            (member (char string pos) '(#\Space #\Tab #\Newline #\Return)))
                 do (incf pos)))
         (expect (c)
           (skip-ws)
           (unless (and (< pos len) (char= (advance) c))
             (error "JSON parse error: expected ~c at position ~d" c pos)))
         (read-value ()
           (skip-ws)
           (let ((c (peek)))
             (case c
               (#\" (read-json-string))
               (#\{ (read-object))
               (#\[ (read-array))
               (#\t (read-literal "true" t))
               (#\f (read-literal "false" :false))
               (#\n (read-literal "null" nil))
               (t (if (or (digit-char-p c) (char= c #\-))
                      (read-number)
                      (error "JSON parse error: unexpected ~c at ~d" c pos))))))
         (read-json-string ()
           (advance) ; skip opening "
           (with-output-to-string (s)
             (loop
               (let ((c (advance)))
                 (cond
                   ((char= c #\") (return))
                   ((char= c #\\)
                    (let ((esc (advance)))
                      (case esc
                        (#\" (write-char #\" s))
                        (#\\ (write-char #\\ s))
                        (#\/ (write-char #\/ s))
                        (#\n (write-char #\Newline s))
                        (#\r (write-char #\Return s))
                        (#\t (write-char #\Tab s))
                        (#\b (write-char #\Backspace s))
                        (#\f (write-char #\Page s))
                        (#\u (let ((code (parse-integer string :start pos :end (+ pos 4) :radix 16)))
                               (incf pos 4)
                               (write-char (code-char code) s))))))
                   (t (write-char c s)))))))
         (read-object ()
           (advance) ; skip {
           (skip-ws)
           (if (and (< pos len) (char= (peek) #\}))
               (progn (advance) nil)
               (let ((result nil))
                 (loop
                   (skip-ws)
                   (let ((key (read-json-string)))
                     (skip-ws)
                     (expect #\:)
                     (let ((val (read-value)))
                       (push (cons key val) result)))
                   (skip-ws)
                   (let ((c (advance)))
                     (cond
                       ((char= c #\}) (return (nreverse result)))
                       ((char= c #\,)) ; continue
                       (t (error "JSON parse error: expected , or } at ~d" pos))))))))
         (read-array ()
           (advance) ; skip [
           (skip-ws)
           (if (and (< pos len) (char= (peek) #\]))
               (progn (advance) nil)
               (let ((result nil))
                 (loop
                   (push (read-value) result)
                   (skip-ws)
                   (let ((c (advance)))
                     (cond
                       ((char= c #\]) (return (nreverse result)))
                       ((char= c #\,)) ; continue
                       (t (error "JSON parse error: expected , or ] at ~d" pos))))))))
         (read-number ()
           (let ((start pos))
             (when (and (< pos len) (char= (peek) #\-)) (advance))
             (loop while (and (< pos len) (digit-char-p (peek))) do (advance))
             (if (and (< pos len) (char= (peek) #\.))
                 (progn
                   (advance)
                   (loop while (and (< pos len) (digit-char-p (peek))) do (advance))
                   (read-from-string (subseq string start pos)))
                 (parse-integer string :start start :end pos))))
         (read-literal (expected value)
           (let ((elen (length expected)))
             (unless (string= string expected :start1 pos :end1 (min (+ pos elen) len))
               (error "JSON parse error: expected ~a at ~d" expected pos))
             (incf pos elen)
             value)))
      (read-value))))

;;; --- Helpers ---

(defun json-get (alist key)
  "Get value for KEY (string) from JSON alist."
  (cdr (assoc key alist :test #'string=)))

(defun make-json-object (&rest pairs)
  "Create a JSON object (alist) from alternating key value pairs.
Keys should be strings."
  (loop for (k v) on pairs by #'cddr
        collect (cons k v)))
