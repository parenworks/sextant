(in-package :sextant)

;;; ============================================================
;;; Lisp Introspection
;;; Queries the source index first, then the running image.
;;; sb-introspect calls are behind #+sbcl for portability.
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
  ;; Try index first for arglist info
  (let ((index-entries (index-lookup-definitions name)))
    (multiple-value-bind (sym pkg) (find-symbol-in-packages name)
      (when (or sym index-entries)
        (with-output-to-string (s)
          (let ((pkg-name (cond
                            (pkg (package-name pkg))
                            (index-entries (index-entry-package (first index-entries))))))
            ;; Header
            (format s "**~a~a**~%"
                    (if pkg-name (format nil "~(~a~):" pkg-name) "")
                    (string-downcase (if sym (symbol-name sym) name)))
            (format s "~%")
            ;; Type info
            (cond
              ((and sym (fboundp sym))
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
                 ;; Arglist: prefer index, fall back to sb-introspect
                 (let ((arglist (or (and index-entries
                                        (index-entry-arglist (first index-entries)))
                                    #+sbcl
                                    (handler-case
                                        (sb-introspect:function-lambda-list sym)
                                      (error () nil)))))
                   (when arglist
                     (format s "```lisp~%(~(~a~)~{ ~(~a~)~})~%```~%"
                             (string-downcase (if sym (symbol-name sym) name))
                             arglist)))))
              ;; Index-only definition (not in running image)
              (index-entries
               (let* ((entry (first index-entries))
                      (kind (index-entry-kind entry)))
                 (format s "*~a* (from source)~%"
                         (string-capitalize (symbol-name kind)))
                 (when (index-entry-arglist entry)
                   (format s "```lisp~%(~(~a~)~{ ~(~a~)~})~%```~%"
                           (string-downcase name)
                           (index-entry-arglist entry)))))
              ((and sym (boundp sym))
               (format s "*Variable*~%")
               (format s "Value: ~s~%" (symbol-value sym)))
              ((and sym (find-class sym nil))
               (format s "*Class*~%"))
              (t
               (format s "*Symbol*~%")))
            ;; Documentation (runtime only)
            (when sym
              (let ((doc (or (documentation sym 'function)
                             (documentation sym 'variable)
                             (documentation sym 'type)
                             (documentation sym 'structure)
                             (documentation sym 'setf))))
                (when doc
                  (format s "~%---~%~a~%" doc))))))))))

(defun symbol-completions (prefix &optional (limit 50))
  "Return a list of completion candidates matching PREFIX.
Each entry is (name kind detail).
Merges results from the source index and the running image."
  (let ((uprefix (string-upcase prefix))
        (results nil)
        (seen (make-hash-table :test 'equal))
        (count 0))
    ;; 1. Source index results (project-local symbols)
    (let ((index-results (index-completions prefix limit)))
      (dolist (r index-results)
        (when (< count limit)
          (let ((name (string-upcase (first r))))
            (unless (gethash name seen)
              (setf (gethash name seen) t)
              (push r results)
              (incf count))))))
    ;; 2. Runtime image results (standard CL, loaded libraries)
    (dolist (pkg (list-all-packages))
      (when (>= count limit) (return))
      (do-external-symbols (sym pkg)
        (when (>= count limit) (return))
        (let ((name (symbol-name sym)))
          (when (and (>= (length name) (length uprefix))
                     (string= uprefix name :end2 (length uprefix))
                     (not (gethash name seen)))
            (setf (gethash name seen) t)
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
  ;; 1. Check source index first
  (let ((entries (index-lookup-definitions name)))
    (when entries
      (let ((entry (first entries)))
        (return-from symbol-definition-location
          (list (index-entry-file entry)
                (index-entry-line entry)
                (index-entry-col entry))))))
  ;; 2. Fall back to sb-introspect on SBCL
  #+sbcl
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

;;; --- Diagnostics ---

(defun compile-and-collect-diagnostics (text)
  "Compile TEXT in a sandbox and return a list of diagnostics.
Each diagnostic is (line character message severity)
where severity: 1=error, 2=warning, 3=info, 4=hint."
  (let ((diagnostics nil)
        (pkg (find-package "COMMON-LISP-USER")))
    (handler-case
        (with-input-from-string (stream text)
          (let ((*package* pkg)
                (*read-eval* nil))
            (loop for form = (handler-case (read stream nil :eof)
                               (end-of-file () :eof)
                               (reader-error (e)
                                 (push (list 0 0
                                             (format nil "Read error: ~a" e)
                                             1)
                                       diagnostics)
                                 :eof)
                               (error (e)
                                 (push (list 0 0
                                             (format nil "Read error: ~a" e)
                                             1)
                                       diagnostics)
                                 :eof))
                  until (eq form :eof)
                  do (handler-case
                         (let ((warnings nil))
                           (handler-bind
                               ((warning (lambda (w)
                                           (push (format nil "~a" w) warnings)
                                           (muffle-warning w))))
                             (compile nil `(lambda () ,form)))
                           (dolist (w (nreverse warnings))
                             (let ((loc (find-form-line text form)))
                               (push (list (car loc) (cdr loc) w 2)
                                     diagnostics))))
                       (error (e)
                         (let ((loc (find-form-line text form)))
                           (push (list (car loc) (cdr loc)
                                       (format nil "~a" e)
                                       1)
                                 diagnostics)))))))
      (error (e)
        (push (list 0 0 (format nil "~a" e) 1) diagnostics)))
    (nreverse diagnostics)))

(defun find-form-line (text form)
  "Try to find the line number of FORM in TEXT. Returns (line . col).
Falls back to (0 . 0) if not found."
  (declare (ignore form text))
  ;; Without source tracking in READ, best we can do is (0 . 0)
  ;; A more sophisticated approach would use source-tracking read
  (cons 0 0))

;;; --- References ---

(defun find-symbol-references (name)
  "Find all references to symbol NAME.
Uses source index first, falls back to sb-introspect on SBCL.
Returns a list of (path line col) entries."
  (let ((results nil))
    ;; 1. Source index references
    (let ((refs (index-lookup-references name)))
      (dolist (ref refs)
        (push (list (ref-entry-file ref)
                    (ref-entry-line ref)
                    (ref-entry-col ref))
              results)))
    ;; 2. Fall back to / augment with sb-introspect on SBCL
    #+sbcl
    (multiple-value-bind (sym) (find-symbol-in-packages name)
      (when sym
        ;; Who calls this function?
        (when (fboundp sym)
          (handler-case
              (let ((callers (sb-introspect:who-calls sym)))
                (dolist (caller callers)
                  (let ((source (sb-introspect:definition-source-pathname caller)))
                    (when source
                      (let ((path (namestring source))
                            (form-num (sb-introspect:definition-source-form-number caller)))
                        (push (list path (or form-num 0) 0) results))))))
            (error () nil)))
        ;; Who binds this variable?
        (when (boundp sym)
          (handler-case
              (let ((binders (sb-introspect:who-binds sym)))
                (dolist (binder binders)
                  (let ((source (sb-introspect:definition-source-pathname binder)))
                    (when source
                      (let ((path (namestring source))
                            (form-num (sb-introspect:definition-source-form-number binder)))
                        (push (list path (or form-num 0) 0) results))))))
            (error () nil)))
        ;; Who references this variable?
        (handler-case
            (let ((refs (sb-introspect:who-references sym)))
              (dolist (ref refs)
                (let ((source (sb-introspect:definition-source-pathname ref)))
                  (when source
                    (let ((path (namestring source))
                          (form-num (sb-introspect:definition-source-form-number ref)))
                      (push (list path (or form-num 0) 0) results))))))
          (error () nil))
        ;; Who macroexpands this?
        (when (macro-function sym)
          (handler-case
              (let ((expanders (sb-introspect:who-macroexpands sym)))
                (dolist (expander expanders)
                  (let ((source (sb-introspect:definition-source-pathname expander)))
                    (when source
                      (let ((path (namestring source))
                            (form-num (sb-introspect:definition-source-form-number expander)))
                        (push (list path (or form-num 0) 0) results))))))
            (error () nil)))))
    ;; Deduplicate
    (remove-duplicates results :test #'equal)))

;;; --- Workspace Symbols ---

(defun search-workspace-symbols (query &optional (limit 100))
  "Search all known symbols matching QUERY string.
Returns list of (name kind container-name path line col)."
  (let ((results nil)
        (seen (make-hash-table :test 'equal))
        (count 0))
    ;; 1. Source index results (project symbols with accurate locations)
    (let ((index-results (index-search-symbols query limit)))
      (dolist (entry index-results)
        (when (< count limit)
          (let ((name (index-entry-name entry)))
            (unless (gethash name seen)
              (setf (gethash name seen) t)
              (push (list (string-downcase name)
                          (index-kind-to-lsp-symbol-kind (index-entry-kind entry))
                          (string-downcase (index-entry-package entry))
                          (index-entry-file entry)
                          (index-entry-line entry)
                          (index-entry-col entry))
                    results)
              (incf count))))))
    ;; 2. Runtime image symbols (standard CL + loaded libraries)
    (let ((uquery (string-upcase query)))
      (dolist (pkg (list-all-packages))
        (when (>= count limit) (return))
        (do-symbols (sym pkg)
          (when (>= count limit) (return))
          (let ((name (symbol-name sym)))
            (when (and (search uquery name)
                       (not (gethash name seen)))
              (setf (gethash name seen) t)
              (let ((kind (symbol-lsp-kind sym))
                    (pkg-name (package-name pkg))
                    (loc #+sbcl
                         (handler-case
                             (when (fboundp sym)
                               (let ((sources (sb-introspect:find-definition-sources-by-name
                                               sym :function)))
                                 (when (and sources (first sources))
                                   (let* ((src (first sources))
                                          (path (sb-introspect:definition-source-pathname src))
                                          (form-num (sb-introspect:definition-source-form-number src)))
                                     (when path
                                       (list (namestring path) (or form-num 0) 0))))))
                           (error () nil))
                         #-sbcl nil))
                (push (list (string-downcase name)
                            kind
                            (string-downcase pkg-name)
                            (if loc (first loc) nil)
                            (if loc (second loc) 0)
                            (if loc (third loc) 0))
                      results)
                (incf count)))))))
    (nreverse results)))

(defun symbol-lsp-kind (sym)
  "Return LSP SymbolKind number for SYM."
  (cond
    ((and (fboundp sym) (macro-function sym)) 14)         ; Constant (macro)
    ((and (fboundp sym) (special-operator-p sym)) 14)     ; Constant
    ((and (fboundp sym)
          (typep (symbol-function sym) 'generic-function)) 6) ; Method
    ((fboundp sym) 12)                                     ; Function
    ((find-class sym nil) 5)                               ; Class
    ((boundp sym) 13)                                      ; Variable
    (t 13)))                                               ; Variable

;;; --- Call Hierarchy ---

(defun symbol-incoming-calls (name)
  "Find functions that call the symbol NAME.
Returns list of (caller-name path line col)."
  (let ((results nil))
    ;; Source index references can serve as incoming call approximation
    (let ((refs (index-lookup-references name)))
      (dolist (ref refs)
        (push (list (format nil "ref@~a:~d" (file-namestring (ref-entry-file ref))
                            (ref-entry-line ref))
                    (ref-entry-file ref)
                    (ref-entry-line ref)
                    (ref-entry-col ref))
              results)))
    ;; sb-introspect gives semantic caller info on SBCL
    #+sbcl
    (multiple-value-bind (sym) (find-symbol-in-packages name)
      (when (and sym (fboundp sym))
        (handler-case
            (let ((callers (sb-introspect:who-calls sym)))
              (dolist (caller callers)
                (let ((source (sb-introspect:definition-source-pathname caller)))
                  (when source
                    (let* ((path (namestring source))
                           (form-num (or (sb-introspect:definition-source-form-number caller) 0))
                           (plist (sb-introspect:definition-source-plist caller))
                           (caller-name (or (getf plist :name)
                                            (format nil "form-~d" form-num))))
                      (push (list (format nil "~(~a~)" caller-name)
                                  path form-num 0)
                            results))))))
          (error () nil))))
    (remove-duplicates results :test #'equal)))

(defun symbol-outgoing-calls (name)
  "Find functions that the symbol NAME calls.
This is harder - we compile and inspect the function's references.
Returns list of (callee-name path line col)."
  (multiple-value-bind (sym) (find-symbol-in-packages name)
    (when (and sym (fboundp sym))
      (let ((results nil))
        ;; Use who-calls in reverse - look at the function's constants
        ;; This is an approximation
        (handler-case
            (let ((callees (sb-introspect:who-calls sym)))
              (declare (ignore callees))
              ;; For outgoing, we'd need to analyze the function body
              ;; SBCL doesn't directly support this, so return empty for now
              nil)
          (error () nil))
        results))))

;;; --- Semantic Token Classification ---

(defun classify-symbol (name)
  "Classify a symbol NAME for semantic tokens.
Returns one of: function, macro, special-form, variable, parameter,
class, keyword, comment, or nil."
  (multiple-value-bind (sym pkg) (find-symbol-in-packages name)
    (cond
      ((null sym) nil)
      ((and pkg (string= (package-name pkg) "KEYWORD")) :keyword)
      ((and (fboundp sym) (macro-function sym)) :macro)
      ((and (fboundp sym) (special-operator-p sym)) :special-form)
      ((and (fboundp sym)
            (typep (symbol-function sym) 'generic-function)) :method)
      ((fboundp sym) :function)
      ((find-class sym nil) :class)
      ((and (boundp sym)
            (constantp sym)) :constant)
      ((boundp sym)
       (if (eql (char (symbol-name sym) 0) #\*)
           :special-variable
           :variable))
      (t :variable))))

;;; --- Inlay Hint Helpers ---

(defun function-keyword-params (name)
  "Get keyword parameters for function NAME.
Returns list of keyword parameter names, or NIL."
  (let ((arglist (get-function-arglist name)))
    (when arglist
      (let ((collecting nil)
            (keywords nil))
        (dolist (arg arglist)
          (cond
            ((eq arg '&key) (setf collecting t))
            ((member arg '(&rest &optional &allow-other-keys &aux &body &whole &environment))
             (setf collecting nil))
            (collecting
             (push (cond
                     ((symbolp arg) arg)
                     ((consp arg) (if (consp (car arg))
                                      (caar arg)
                                      (car arg)))
                     (t arg))
                   keywords))))
        (nreverse keywords)))))

(defun get-function-arglist (name)
  "Get the arglist for function NAME from index or runtime.
Returns the lambda list or NIL."
  ;; 1. Source index
  (let ((entries (index-lookup-definitions name)))
    (when entries
      (let ((arglist (index-entry-arglist (first entries))))
        (when arglist (return-from get-function-arglist arglist)))))
  ;; 2. sb-introspect fallback
  #+sbcl
  (multiple-value-bind (sym) (find-symbol-in-packages name)
    (when (and sym (fboundp sym))
      (handler-case
          (sb-introspect:function-lambda-list sym)
        (error () nil)))))

(defun function-reference-count (name)
  "Count how many places reference symbol NAME."
  (let ((count 0))
    ;; Source index references
    (let ((refs (index-lookup-references name)))
      (incf count (length refs)))
    ;; sb-introspect augmentation on SBCL
    #+sbcl
    (multiple-value-bind (sym) (find-symbol-in-packages name)
      (when sym
        (when (fboundp sym)
          (handler-case
              (incf count (length (sb-introspect:who-calls sym)))
            (error () nil)))
        (handler-case
            (incf count (length (sb-introspect:who-references sym)))
          (error () nil))
        (when (boundp sym)
          (handler-case
              (incf count (length (sb-introspect:who-binds sym)))
            (error () nil)))
        (when (macro-function sym)
          (handler-case
              (incf count (length (sb-introspect:who-macroexpands sym)))
            (error () nil)))))
    count))

(defun symbol-signature (name)
  "Get function signature for NAME. Returns (name arglist doc) or NIL."
  (let ((arglist (get-function-arglist name))
        (doc nil))
    ;; Try to get documentation from runtime
    (multiple-value-bind (sym) (find-symbol-in-packages name)
      (when sym
        (setf doc (documentation sym 'function))))
    (when arglist
      (list (string-downcase name)
            arglist
            doc))))
