(in-package :sextant)

;;; ============================================================
;;; Lisp Introspection
;;; Queries the running SBCL image for symbol info
;;; No Swank needed - we ARE the Lisp image
;;; ============================================================

(defun find-symbol-in-packages (name)
  "Find a symbol by NAME string, searching common packages.
Returns (values symbol package) or NIL."
  (let ((uname (string-upcase name)))
    ;; Check if it has a package qualifier
    (let ((colon (position #\: name)))
      (when colon
        (let* ((pkg-name (subseq name 0 colon))
               (sym-name (string-upcase
                          (string-left-trim ":" (subseq name colon))))
               (pkg (find-package (string-upcase pkg-name))))
          (when pkg
            (multiple-value-bind (sym status) (find-symbol sym-name pkg)
              (when status
                (return-from find-symbol-in-packages (values sym pkg))))))))
    ;; Search standard packages
    (dolist (pkg-name '("COMMON-LISP" "COMMON-LISP-USER" "KEYWORD"))
      (let ((pkg (find-package pkg-name)))
        (when pkg
          (multiple-value-bind (sym status) (find-symbol uname pkg)
            (when status
              (return-from find-symbol-in-packages (values sym pkg)))))))
    ;; Search all packages
    (dolist (pkg (list-all-packages))
      (multiple-value-bind (sym status) (find-symbol uname pkg)
        (when (eq status :external)
          (return-from find-symbol-in-packages (values sym pkg)))))))

(defun symbol-hover-info (name)
  "Get hover documentation for symbol NAME. Returns a string or NIL."
  (multiple-value-bind (sym pkg) (find-symbol-in-packages name)
    (when sym
      (with-output-to-string (s)
        (let ((pkg-name (when pkg (package-name pkg))))
          ;; Header
          (format s "**~a~a**~%"
                  (if pkg-name (format nil "~(~a~):" pkg-name) "")
                  (string-downcase (symbol-name sym)))
          (format s "~%")
          ;; Type info
          (cond
            ((fboundp sym)
             (let ((fn (symbol-function sym)))
               (cond
                 ((macro-function sym)
                  (format s "*Macro*~%"))
                 ((typep fn 'generic-function)
                  (format s "*Generic Function*~%"))
                 ((special-operator-p sym)
                  (format s "*Special Operator*~%"))
                 (t
                  (format s "*Function*~%")))
               ;; Arglist
               (let ((arglist (handler-case
                                  (sb-introspect:function-lambda-list sym)
                                (error () nil))))
                 (when arglist
                   (format s "```lisp~%(~(~a~)~{ ~(~a~)~})~%```~%"
                           (string-downcase (symbol-name sym))
                           arglist)))))
            ((boundp sym)
             (format s "*Variable*~%")
             (format s "Value: ~s~%" (symbol-value sym)))
            ((find-class sym nil)
             (format s "*Class*~%"))
            (t
             (format s "*Symbol*~%")))
          ;; Documentation
          (let ((doc (or (documentation sym 'function)
                         (documentation sym 'variable)
                         (documentation sym 'type)
                         (documentation sym 'structure)
                         (documentation sym 'setf))))
            (when doc
              (format s "~%---~%~a~%" doc))))))))

(defun symbol-completions (prefix &optional (limit 50))
  "Return a list of completion candidates matching PREFIX.
Each entry is (name kind detail)."
  (let ((uprefix (string-upcase prefix))
        (results nil)
        (count 0))
    (dolist (pkg (list-all-packages))
      (when (>= count limit) (return))
      (do-external-symbols (sym pkg)
        (when (>= count limit) (return))
        (let ((name (symbol-name sym)))
          (when (and (>= (length name) (length uprefix))
                     (string= uprefix name :end2 (length uprefix)))
            (push (list (string-downcase name)
                        (symbol-completion-kind sym)
                        (package-name pkg))
                  results)
            (incf count)))))
    (nreverse results)))

(defun symbol-completion-kind (sym)
  "Return LSP CompletionItemKind number for SYM."
  (cond
    ((and (fboundp sym) (macro-function sym)) 14)      ; Keyword (macro)
    ((and (fboundp sym) (special-operator-p sym)) 14)  ; Keyword
    ((and (fboundp sym)
          (typep (symbol-function sym) 'generic-function)) 3) ; Function
    ((fboundp sym) 3)                                   ; Function
    ((find-class sym nil) 7)                            ; Class
    ((boundp sym) 6)                                    ; Variable
    (t 6)))                                             ; Variable

(defun symbol-definition-location (name)
  "Find the source location of symbol NAME.
Returns (path line col) or NIL."
  (multiple-value-bind (sym) (find-symbol-in-packages name)
    (when (and sym (fboundp sym))
      (let ((source (handler-case
                        (sb-introspect:find-definition-sources-by-name
                         sym :function)
                      (error () nil))))
        (when (and source (first source))
          (let* ((src (first source))
                 (namestring (sb-introspect:definition-source-pathname src))
                 (form-path (sb-introspect:definition-source-form-number src)))
            (when namestring
              (let ((path (namestring namestring)))
                (list path (or form-path 0) 0)))))))))

(defun symbol-signature (name)
  "Get function signature for NAME. Returns (name arglist doc) or NIL."
  (multiple-value-bind (sym) (find-symbol-in-packages name)
    (when (and sym (fboundp sym))
      (let ((arglist (handler-case
                         (sb-introspect:function-lambda-list sym)
                       (error () nil)))
            (doc (documentation sym 'function)))
        (when arglist
          (list (string-downcase (symbol-name sym))
                arglist
                doc))))))
