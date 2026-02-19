(in-package :sextant)

;;; ============================================================
;;; LSP Method Handlers
;;; Dispatches JSON-RPC methods to implementation functions
;;; ============================================================

;;; --- Semantic Token Legends (must be defined before capabilities) ---

(defvar *semantic-token-types*
  '("function" "macro" "keyword" "variable" "parameter"
    "class" "method" "comment" "string" "number")
  "Semantic token types we support.")

(defvar *semantic-token-modifiers*
  '("definition" "declaration" "deprecated" "readonly" "static"
    "defaultLibrary")
  "Semantic token modifiers we support.")

(defvar *server-capabilities*
  (make-json-object
   "textDocumentSync" (make-json-object
                       "openClose" t
                       "change" 2  ; Incremental sync
                       "save" t)
   "hoverProvider" t
   "completionProvider" (make-json-object
                         "triggerCharacters" (list "(" ":" "-")
                         "resolveProvider" t)
   "definitionProvider" t
   "referencesProvider" t
   "documentSymbolProvider" t
   "workspaceSymbolProvider" t
   "signatureHelpProvider" (make-json-object
                            "triggerCharacters" (list " " "("))
   "documentHighlightProvider" t
   "selectionRangeProvider" t
   "foldingRangeProvider" t
   "documentFormattingProvider" t
   "renameProvider" (make-json-object
                     "prepareProvider" t)
   "codeActionProvider" t
   "semanticTokensProvider" (make-json-object
                              "full" t
                              "legend" (make-json-object
                                        "tokenTypes" *semantic-token-types*
                                        "tokenModifiers" *semantic-token-modifiers*))
   "inlayHintProvider" t
   "callHierarchyProvider" t
   "codeLensProvider" (make-json-object
                        "resolveProvider" :false)
   "linkedEditingRangeProvider" t
   )
  "LSP server capabilities we advertise.")

(defun handle-request (method id params)
  "Handle an LSP request (expects a response)."
  (lsp-log "Request: ~a id=~a" method id)
  (handler-case
      (let ((result
              (cond
                ((string= method "initialize")
                 (handle-initialize params))
                ((string= method "shutdown")
                 (handle-shutdown))
                ((string= method "textDocument/hover")
                 (handle-hover params))
                ((string= method "textDocument/completion")
                 (handle-completion params))
                ((string= method "textDocument/definition")
                 (handle-definition params))
                ((string= method "textDocument/signatureHelp")
                 (handle-signature-help params))
                ((string= method "textDocument/references")
                 (handle-references params))
                ((string= method "textDocument/documentSymbol")
                 (handle-document-symbol params))
                ((string= method "workspace/symbol")
                 (handle-workspace-symbol params))
                ((string= method "completionItem/resolve")
                 (handle-completion-resolve params))
                ((string= method "textDocument/documentHighlight")
                 (handle-document-highlight params))
                ((string= method "textDocument/selectionRange")
                 (handle-selection-range params))
                ((string= method "textDocument/foldingRange")
                 (handle-folding-range params))
                ((string= method "textDocument/formatting")
                 (handle-formatting params))
                ((string= method "textDocument/prepareRename")
                 (handle-prepare-rename params))
                ((string= method "textDocument/rename")
                 (handle-rename params))
                ((string= method "textDocument/codeAction")
                 (handle-code-action params))
                ((string= method "textDocument/semanticTokens/full")
                 (handle-semantic-tokens-full params))
                ((string= method "textDocument/inlayHint")
                 (handle-inlay-hint params))
                ((string= method "textDocument/prepareCallHierarchy")
                 (handle-call-hierarchy-prepare params))
                ((string= method "callHierarchy/incomingCalls")
                 (handle-call-hierarchy-incoming params))
                ((string= method "callHierarchy/outgoingCalls")
                 (handle-call-hierarchy-outgoing params))
                ((string= method "textDocument/codeLens")
                 (handle-code-lens params))
                ((string= method "textDocument/linkedEditingRange")
                 (handle-linked-editing-range params))
                (t
                 (lsp-log "Unhandled request: ~a" method)
                 nil))))
        (make-response id result))
    (error (e)
      (lsp-log "Handler error for ~a: ~a" method e)
      (make-error-response id -32603 (format nil "~a" e)))))

(defun handle-notification (method params)
  "Handle an LSP notification (no response expected)."
  (lsp-log "Notification: ~a" method)
  (cond
    ((string= method "initialized") nil)
    ((string= method "exit") (sb-ext:exit :code 0))
    ((string= method "textDocument/didOpen")
     (handle-did-open params))
    ((string= method "textDocument/didChange")
     (handle-did-change params))
    ((string= method "textDocument/didClose")
     (handle-did-close params))
    ((string= method "textDocument/didSave")
     (handle-did-save params))
    (t (lsp-log "Unhandled notification: ~a" method))))

;;; --- Initialize ---

(defun handle-initialize (params)
  "Handle initialize request."
  (declare (ignore params))
  (make-json-object
   "capabilities" *server-capabilities*
   "serverInfo" (make-json-object
                 "name" "Sextant"
                 "version" "0.1.0")))

;;; --- Shutdown ---

(defvar *shutdown-requested* nil)

(defun handle-shutdown ()
  (setf *shutdown-requested* t)
  nil)

;;; --- Hover ---

(defun handle-hover (params)
  "Handle textDocument/hover - show symbol documentation."
  (let* ((td (json-get params "textDocument"))
         (uri (json-get td "uri"))
         (pos (json-get params "position"))
         (line (json-get pos "line"))
         (col (json-get pos "character"))
         (text (document-text uri)))
    (when text
      (let ((sym-name (symbol-at-position text line col)))
        (when sym-name
          (lsp-log "Hover: ~a at ~d:~d" sym-name line col)
          (let ((info (symbol-hover-info sym-name)))
            (when info
              (make-json-object
               "contents" (make-json-object
                           "kind" "markdown"
                           "value" info)))))))))

;;; --- Completion ---

(defun handle-completion (params)
  "Handle textDocument/completion - provide symbol completions."
  (let* ((td (json-get params "textDocument"))
         (uri (json-get td "uri"))
         (pos (json-get params "position"))
         (line (json-get pos "line"))
         (col (json-get pos "character"))
         (text (document-text uri)))
    (when text
      (let ((prefix (symbol-at-position text line col)))
        (when (and prefix (>= (length prefix) 2))
          (lsp-log "Complete: ~a" prefix)
          (let ((completions (symbol-completions prefix)))
            (make-json-object
             "isIncomplete" :false
             "items" (mapcar
                      (lambda (c)
                        (destructuring-bind (name kind pkg) c
                          (make-json-object
                           "label" name
                           "kind" kind
                           "detail" (format nil "~(~a~)" pkg))))
                      completions))))))))

;;; --- Go to Definition ---

(defun handle-definition (params)
  "Handle textDocument/definition - jump to symbol source."
  (let* ((td (json-get params "textDocument"))
         (uri (json-get td "uri"))
         (pos (json-get params "position"))
         (line (json-get pos "line"))
         (col (json-get pos "character"))
         (text (document-text uri)))
    (when text
      (let ((sym-name (symbol-at-position text line col)))
        (when sym-name
          (lsp-log "Definition: ~a" sym-name)
          ;; Try SBCL introspection first, then fall back to open documents
          (let ((loc (or (symbol-definition-location sym-name)
                         (find-definition-in-documents sym-name))))
            (when loc
              (destructuring-bind (path-or-uri def-line def-col) loc
                (make-json-object
                 "uri" (if (and (>= (length path-or-uri) 7)
                                (string= "file://" (subseq path-or-uri 0 7)))
                           path-or-uri
                           (path-to-uri path-or-uri))
                 "range" (make-json-object
                          "start" (make-json-object "line" def-line
                                                    "character" def-col)
                          "end" (make-json-object "line" def-line
                                                  "character" def-col)))))))))))

;;; --- Signature Help ---

(defun handle-signature-help (params)
  "Handle textDocument/signatureHelp - show function arglist."
  (let* ((td (json-get params "textDocument"))
         (uri (json-get td "uri"))
         (pos (json-get params "position"))
         (line (json-get pos "line"))
         (col (json-get pos "character"))
         (text (document-text uri)))
    (when text
      (let ((sym-name (enclosing-function-name text line col)))
        (when sym-name
          (lsp-log "Signature: ~a" sym-name)
          (let ((sig (symbol-signature sym-name)))
            (when sig
              (destructuring-bind (name arglist doc) sig
                (make-json-object
                 "signatures" (list
                               (make-json-object
                                "label" (format nil "(~a~{ ~(~a~)~})" name arglist)
                                "documentation" (or doc "")))
                 "activeSignature" 0
                 "activeParameter" 0))))))))  )

(defun enclosing-function-name (text line col)
  "Find the function name of the enclosing form at LINE, COL."
  (let* ((offset (line-col-to-offset text line col))
         (pos offset))
    ;; Scan backward for opening paren
    (let ((depth 0))
      (loop while (> pos 0)
            do (decf pos)
               (let ((c (char text pos)))
                 (cond
                   ((char= c #\)) (incf depth))
                   ((char= c #\()
                    (if (> depth 0)
                        (decf depth)
                        ;; Found our opening paren, read the symbol after it
                        (let ((sym-start (1+ pos)))
                          (let ((sym-end sym-start))
                            (loop while (and (< sym-end (length text))
                                             (symbol-char-p (char text sym-end)))
                                  do (incf sym-end))
                            (when (> sym-end sym-start)
                              (return-from enclosing-function-name
                                (subseq text sym-start sym-end)))))))))))
    nil))

;;; --- References ---

(defun handle-references (params)
  "Handle textDocument/references - find all references to symbol."
  (let* ((td (json-get params "textDocument"))
         (uri (json-get td "uri"))
         (pos (json-get params "position"))
         (line (json-get pos "line"))
         (col (json-get pos "character"))
         (text (document-text uri)))
    (when text
      (let ((sym-name (symbol-at-position text line col)))
        (when sym-name
          (lsp-log "References: ~a" sym-name)
          ;; Try SBCL introspection first, fall back to document search
          (let ((refs (find-symbol-references sym-name)))
            (or (when refs
                  (mapcar (lambda (ref)
                            (destructuring-bind (path ref-line ref-col) ref
                              (make-json-object
                               "uri" (path-to-uri path)
                               "range" (make-json-object
                                        "start" (make-json-object "line" ref-line
                                                                  "character" ref-col)
                                        "end" (make-json-object "line" ref-line
                                                                "character" ref-col)))))
                          refs))
                (find-references-in-documents sym-name))))))))

;;; --- Document Symbols ---

(defun handle-document-symbol (params)
  "Handle textDocument/documentSymbol - return top-level forms as symbols."
  (let* ((td (json-get params "textDocument"))
         (uri (json-get td "uri"))
         (text (document-text uri)))
    (when text
      (extract-document-symbols text))))

(defun extract-document-symbols (text)
  "Parse TEXT for top-level def* forms and return LSP DocumentSymbol list."
  (let ((symbols nil)
        (pos 0)
        (len (length text)))
    (loop while (< pos len)
          do (multiple-value-bind (match-start match-end name kind line)
                 (find-next-definition text pos)
               (if match-start
                   (progn
                     (let ((end-line (or (find-form-end-line text match-start) line)))
                       (push (make-json-object
                              "name" name
                              "kind" kind
                              "range" (make-json-object
                                        "start" (make-json-object "line" line "character" 0)
                                        "end" (make-json-object "line" end-line "character" 0))
                              "selectionRange" (make-json-object
                                                 "start" (make-json-object "line" line "character" 0)
                                                 "end" (make-json-object "line" line "character" 0)))
                             symbols))
                     (setf pos match-end))
                   (return))))
    (nreverse symbols)))

(defun find-next-definition (text start)
  "Find the next (def...) form in TEXT starting at START.
Returns (values match-start match-end name symbol-kind line) or NIL."
  (let ((len (length text)))
    (loop for i from start below len
          do (when (and (char= (char text i) #\()
                        (< (+ i 4) len)
                        (string-equal "def" (subseq text (1+ i)
                                                     (min (+ i 4) len))))
               ;; Found a (def... - read the def-type and name
               (let* ((space-pos (position-if
                                  (lambda (c) (member c '(#\Space #\Tab #\Newline)))
                                  text :start (1+ i)))
                      (def-type (when space-pos
                                  (string-downcase (subseq text (1+ i) space-pos))))
                      (name-start (when space-pos
                                    (position-if-not
                                     (lambda (c) (member c '(#\Space #\Tab #\Newline)))
                                     text :start space-pos)))
                      (name-end (when name-start
                                  (position-if
                                   (lambda (c) (member c '(#\Space #\Tab #\Newline #\( #\))))
                                   text :start name-start)))
                      (name (when (and name-start name-end)
                              (subseq text name-start name-end)))
                      (line (count #\Newline text :end i)))
                 (when (and def-type name (> (length name) 0))
                   (let ((kind (def-type-to-symbol-kind def-type)))
                     (return (values i (or name-end (1+ i)) name kind line)))))))))

(defun def-type-to-symbol-kind (def-type)
  "Map a Common Lisp def-type string to LSP SymbolKind."
  (cond
    ((string= def-type "defun") 12)           ; Function
    ((string= def-type "defmacro") 12)        ; Function
    ((string= def-type "defgeneric") 12)      ; Function
    ((string= def-type "defmethod") 6)        ; Method
    ((string= def-type "defvar") 13)          ; Variable
    ((string= def-type "defparameter") 13)    ; Variable
    ((string= def-type "defconstant") 14)     ; Constant
    ((string= def-type "defclass") 5)         ; Class
    ((string= def-type "defstruct") 23)       ; Struct
    ((string= def-type "defpackage") 4)       ; Package
    ((string= def-type "define-condition") 5) ; Class
    ((string= def-type "deftype") 26)         ; TypeParameter
    (t 12)))                                   ; Function (default)

(defun find-form-end-line (text start)
  "Find the line number where the top-level form starting at START ends."
  (let ((depth 0)
        (len (length text))
        (in-string nil)
        (escape nil))
    (loop for i from start below len
          for c = (char text i)
          do (cond
               (escape (setf escape nil))
               ((char= c #\\) (setf escape t))
               ((char= c #\") (setf in-string (not in-string)))
               (in-string nil)
               ((char= c #\;)
                ;; Skip to end of line
                (loop for j from i below len
                      until (char= (char text j) #\Newline)
                      finally (setf i j)))
               ((char= c #\() (incf depth))
               ((char= c #\))
                (decf depth)
                (when (<= depth 0)
                  (return (count #\Newline text :end (1+ i))))))
          finally (return (count #\Newline text :end len)))))

;;; --- Workspace Symbols ---

(defun handle-workspace-symbol (params)
  "Handle workspace/symbol - search symbols across all packages."
  (let ((query (json-get params "query")))
    (when (and query (>= (length query) 2))
      (lsp-log "Workspace symbol: ~a" query)
      (let ((results (search-workspace-symbols query)))
        (mapcar (lambda (entry)
                  (destructuring-bind (name kind container-name path line col) entry
                    (let ((loc (if path
                                   (make-json-object
                                    "uri" (path-to-uri path)
                                    "range" (make-json-object
                                             "start" (make-json-object "line" line "character" col)
                                             "end" (make-json-object "line" line "character" col)))
                                   ;; No source location - use a dummy
                                   (make-json-object
                                    "uri" "file:///unknown"
                                    "range" (make-json-object
                                             "start" (make-json-object "line" 0 "character" 0)
                                             "end" (make-json-object "line" 0 "character" 0))))))
                      (make-json-object
                       "name" name
                       "kind" kind
                       "containerName" container-name
                       "location" loc))))
                results)))))

;;; --- Completion Resolve ---

(defun handle-completion-resolve (params)
  "Handle completionItem/resolve - add documentation to a completion item."
  (let* ((label (json-get params "label"))
         (info (when label (symbol-hover-info label))))
    ;; Return the item with documentation added
    (if info
        (append params (list (cons "documentation"
                                   (make-json-object
                                    "kind" "markdown"
                                    "value" info))))
        params)))

;;; --- Document Highlight ---

(defun handle-document-highlight (params)
  "Handle textDocument/documentHighlight - highlight symbol occurrences."
  (let* ((td (json-get params "textDocument"))
         (uri (json-get td "uri"))
         (pos (json-get params "position"))
         (line (json-get pos "line"))
         (col (json-get pos "character"))
         (text (document-text uri)))
    (when text
      (let ((sym-name (symbol-at-position text line col)))
        (when sym-name
          (lsp-log "Highlight: ~a" sym-name)
          (let ((occurrences (find-all-symbol-occurrences text sym-name)))
            (mapcar (lambda (occ)
                      (let ((start-lc (offset-to-line-col text (car occ)))
                            (end-lc (offset-to-line-col text (cdr occ))))
                        (make-json-object
                         "range" (make-json-object
                                  "start" (make-json-object "line" (car start-lc)
                                                            "character" (cdr start-lc))
                                  "end" (make-json-object "line" (car end-lc)
                                                          "character" (cdr end-lc)))
                         "kind" 1)))  ; 1 = Text
                    occurrences)))))))

;;; --- Selection Range ---

(defun handle-selection-range (params)
  "Handle textDocument/selectionRange - expand selection by s-expression."
  (let* ((td (json-get params "textDocument"))
         (uri (json-get td "uri"))
         (positions (json-get params "positions"))
         (text (document-text uri)))
    (when text
      (mapcar (lambda (pos)
                (let* ((line (json-get pos "line"))
                       (col (json-get pos "character"))
                       (offset (line-col-to-offset text line col))
                       (enclosing (find-all-enclosing-sexps text offset)))
                  (if enclosing
                      ;; Build nested selection ranges from innermost to outermost
                      (let ((result nil))
                        (dolist (sexp (reverse enclosing))
                          (let ((start-lc (offset-to-line-col text (car sexp)))
                                (end-lc (offset-to-line-col text (cdr sexp))))
                            (setf result
                                  (make-json-object
                                   "range" (make-json-object
                                            "start" (make-json-object
                                                     "line" (car start-lc)
                                                     "character" (cdr start-lc))
                                            "end" (make-json-object
                                                   "line" (car end-lc)
                                                   "character" (cdr end-lc)))
                                   "parent" result))))
                        result)
                      ;; No enclosing sexp - return current position
                      (make-json-object
                       "range" (make-json-object
                                "start" (make-json-object "line" line "character" col)
                                "end" (make-json-object "line" line "character" col))))))
              positions))))

;;; --- Folding Range ---

(defun handle-folding-range (params)
  "Handle textDocument/foldingRange - fold top-level forms."
  (let* ((td (json-get params "textDocument"))
         (uri (json-get td "uri"))
         (text (document-text uri)))
    (when text
      (let ((forms (find-top-level-forms text)))
        (remove nil
                (mapcar (lambda (form)
                          (destructuring-bind (start-off end-off start-line end-line) form
                            (declare (ignore start-off end-off))
                            (when (> end-line start-line)
                              (make-json-object
                               "startLine" start-line
                               "endLine" end-line
                               "kind" "region"))))
                        forms))))))

;;; --- Formatting ---

(defun handle-formatting (params)
  "Handle textDocument/formatting - format the entire document."
  (let* ((td (json-get params "textDocument"))
         (uri (json-get td "uri"))
         (text (document-text uri)))
    (when text
      (let* ((formatted (format-lisp-text text))
             (line-count (count #\Newline text)))
        (when (string/= text formatted)
          (list (make-json-object
                 "range" (make-json-object
                          "start" (make-json-object "line" 0 "character" 0)
                          "end" (make-json-object "line" (1+ line-count) "character" 0))
                 "newText" formatted)))))))

;;; --- Rename ---

(defun handle-prepare-rename (params)
  "Handle textDocument/prepareRename - check if rename is possible."
  (let* ((td (json-get params "textDocument"))
         (uri (json-get td "uri"))
         (pos (json-get params "position"))
         (line (json-get pos "line"))
         (col (json-get pos "character"))
         (text (document-text uri)))
    (when text
      (let ((range (find-symbol-range-at text line col)))
        (when range
          (destructuring-bind (sl sc el ec) range
            (make-json-object
             "range" (make-json-object
                      "start" (make-json-object "line" sl "character" sc)
                      "end" (make-json-object "line" el "character" ec))
             "placeholder" (symbol-at-position text line col))))))))

(defun handle-rename (params)
  "Handle textDocument/rename - rename symbol in document."
  (let* ((td (json-get params "textDocument"))
         (uri (json-get td "uri"))
         (pos (json-get params "position"))
         (line (json-get pos "line"))
         (col (json-get pos "character"))
         (new-name (json-get params "newName"))
         (text (document-text uri)))
    (when text
      (let ((old-name (symbol-at-position text line col)))
        (when old-name
          (lsp-log "Rename: ~a -> ~a" old-name new-name)
          (let ((occurrences (find-all-symbol-occurrences text old-name)))
            (when occurrences
              (make-json-object
               "changes" (list
                          (cons uri
                                (mapcar (lambda (occ)
                                          (let ((start-lc (offset-to-line-col text (car occ)))
                                                (end-lc (offset-to-line-col text (cdr occ))))
                                            (make-json-object
                                             "range" (make-json-object
                                                      "start" (make-json-object
                                                               "line" (car start-lc)
                                                               "character" (cdr start-lc))
                                                      "end" (make-json-object
                                                             "line" (car end-lc)
                                                             "character" (cdr end-lc)))
                                             "newText" new-name)))
                                        occurrences)))))))))))

;;; --- Code Actions ---

(defun handle-code-action (params)
  "Handle textDocument/codeAction - suggest code fixes and refactorings."
  (let* ((td (json-get params "textDocument"))
         (uri (json-get td "uri"))
         (range (json-get params "range"))
         (text (document-text uri))
         (actions nil))
    (when text
      (let* ((start-pos (json-get range "start"))
             (line (json-get start-pos "line"))
             (col (json-get start-pos "character"))
             (sym-name (symbol-at-position text line col)))
        ;; Action: Add package prefix
        (when sym-name
          (multiple-value-bind (sym pkg) (find-symbol-in-packages sym-name)
            (when (and sym pkg
                       (not (find #\: sym-name)))
              (let ((pkg-name (string-downcase (package-name pkg))))
                (unless (string= pkg-name "common-lisp")
                  (push (make-json-object
                         "title" (format nil "Add package prefix: ~a:~a" pkg-name sym-name)
                         "kind" "quickfix"
                         "edit" (make-json-object
                                 "changes" (list
                                            (cons uri
                                                  (let ((range-info (find-symbol-range-at text line col)))
                                                    (when range-info
                                                      (destructuring-bind (sl sc el ec) range-info
                                                        (list (make-json-object
                                                               "range" (make-json-object
                                                                        "start" (make-json-object "line" sl "character" sc)
                                                                        "end" (make-json-object "line" el "character" ec))
                                                               "newText" (format nil "~a:~a" pkg-name sym-name))))))))))
                        actions))))))
        ;; Action: Export symbol (if in a defun/defvar etc)
        (when sym-name
          (push (make-json-object
                 "title" (format nil "Export symbol: ~a" sym-name)
                 "kind" "refactor")
                actions))
        ;; Action: Insert defpackage template
        (when (and (zerop line) (zerop col)
                   (not (search "(defpackage" text :test #'char-equal))
                   (not (search "(in-package" text :test #'char-equal)))
          (push (make-json-object
                 "title" "Insert defpackage template"
                 "kind" "refactor"
                 "edit" (make-json-object
                         "changes" (list
                                    (cons uri
                                          (list (make-json-object
                                                 "range" (make-json-object
                                                          "start" (make-json-object "line" 0 "character" 0)
                                                          "end" (make-json-object "line" 0 "character" 0))
                                                 "newText" (format nil "(defpackage :my-package~%  (:use :cl)~%  (:export))~%~%(in-package :my-package)~%~%")))))))
                actions))))
    (nreverse actions)))

;;; --- Semantic Tokens ---

(defun semantic-token-type-index (type)
  "Get the index of TYPE in *semantic-token-types*."
  (or (position type *semantic-token-types* :test #'string=) 0))

(defun handle-semantic-tokens-full (params)
  "Handle textDocument/semanticTokens/full - provide semantic tokens."
  (let* ((td (json-get params "textDocument"))
         (uri (json-get td "uri"))
         (text (document-text uri)))
    (when text
      (let ((tokens (collect-semantic-tokens text)))
        (make-json-object
         "data" (or tokens :empty-array))))))

(defun collect-semantic-tokens (text)
  "Collect semantic tokens from TEXT.
Returns a flat list of integers in LSP semantic token format:
deltaLine, deltaStartChar, length, tokenType, tokenModifiers."
  (let ((len (length text))
        (tokens nil)
        (prev-line 0)
        (prev-col 0)
        (i 0)
        (in-string nil)
        (escape nil))
    (loop while (< i len)
          for c = (char text i)
          do (cond
               (escape
                (setf escape nil)
                (incf i))
               ((char= c #\\)
                (setf escape t)
                (incf i))
               ;; String literals
               ((and (char= c #\") (not in-string))
                (let ((start i))
                  (setf in-string t)
                  (incf i)
                  (loop while (and (< i len) in-string)
                        do (let ((sc (char text i)))
                             (cond
                               ((char= sc #\\) (incf i 2))
                               ((char= sc #\")
                                (setf in-string nil)
                                (incf i))
                               (t (incf i)))))
                  (let* ((start-lc (offset-to-line-col text start))
                         (sline (car start-lc))
                         (scol (cdr start-lc))
                         (token-len (- i start))
                         (delta-line (- sline prev-line))
                         (delta-col (if (zerop delta-line) (- scol prev-col) scol)))
                    (push delta-line tokens)
                    (push delta-col tokens)
                    (push token-len tokens)
                    (push (semantic-token-type-index "string") tokens)
                    (push 0 tokens)
                    (setf prev-line sline prev-col scol))))
               ;; Comments
               ((char= c #\;)
                (let ((start i))
                  (loop while (and (< i len) (not (char= (char text i) #\Newline)))
                        do (incf i))
                  (let* ((start-lc (offset-to-line-col text start))
                         (sline (car start-lc))
                         (scol (cdr start-lc))
                         (token-len (- i start))
                         (delta-line (- sline prev-line))
                         (delta-col (if (zerop delta-line) (- scol prev-col) scol)))
                    (push delta-line tokens)
                    (push delta-col tokens)
                    (push token-len tokens)
                    (push (semantic-token-type-index "comment") tokens)
                    (push 0 tokens)
                    (setf prev-line sline prev-col scol))))
               ;; Symbols
               ((symbol-char-p c)
                (let ((start i))
                  (loop while (and (< i len) (symbol-char-p (char text i)))
                        do (incf i))
                  (let* ((sym-text (subseq text start i))
                         (classification (classify-symbol sym-text)))
                    (when classification
                      (let* ((start-lc (offset-to-line-col text start))
                             (sline (car start-lc))
                             (scol (cdr start-lc))
                             (token-len (- i start))
                             (token-type (case classification
                                           (:function "function")
                                           (:macro "macro")
                                           (:special-form "keyword")
                                           (:method "method")
                                           (:keyword "keyword")
                                           (:class "class")
                                           (:constant "number")
                                           (:special-variable "variable")
                                           (:variable "variable")
                                           (t "variable")))
                             (modifiers (case classification
                                          (:constant (ash 1 3))  ; readonly
                                          (:special-variable (ash 1 3))  ; readonly
                                          (t 0)))
                             (delta-line (- sline prev-line))
                             (delta-col (if (zerop delta-line) (- scol prev-col) scol)))
                        (push delta-line tokens)
                        (push delta-col tokens)
                        (push token-len tokens)
                        (push (semantic-token-type-index token-type) tokens)
                        (push modifiers tokens)
                        (setf prev-line sline prev-col scol))))))
               ;; Numbers
               ((or (digit-char-p c)
                    (and (char= c #\-) (< (1+ i) len) (digit-char-p (char text (1+ i)))))
                (let ((start i))
                  (incf i)
                  (loop while (and (< i len)
                                   (or (digit-char-p (char text i))
                                       (char= (char text i) #\.)
                                       (char= (char text i) #\/)))
                        do (incf i))
                  (let* ((start-lc (offset-to-line-col text start))
                         (sline (car start-lc))
                         (scol (cdr start-lc))
                         (token-len (- i start))
                         (delta-line (- sline prev-line))
                         (delta-col (if (zerop delta-line) (- scol prev-col) scol)))
                    (push delta-line tokens)
                    (push delta-col tokens)
                    (push token-len tokens)
                    (push (semantic-token-type-index "number") tokens)
                    (push 0 tokens)
                    (setf prev-line sline prev-col scol))))
               (t (incf i))))
    (nreverse tokens)))

;;; --- Inlay Hints ---

(defun handle-inlay-hint (params)
  "Handle textDocument/inlayHint - show keyword arg names inline."
  (let* ((td (json-get params "textDocument"))
         (uri (json-get td "uri"))
         (range (json-get params "range"))
         (text (document-text uri)))
    (when text
      (let* ((start-pos (json-get range "start"))
             (end-pos (json-get range "end"))
             (start-offset (line-col-to-offset text
                                                (json-get start-pos "line")
                                                (json-get start-pos "character")))
             (end-offset (line-col-to-offset text
                                              (json-get end-pos "line")
                                              (json-get end-pos "character")))
             (hints nil))
        ;; Scan for function calls in the visible range
        (let ((i start-offset)
              (len (min end-offset (length text))))
          (loop while (< i len)
                do (when (and (char= (char text i) #\()
                              (< (1+ i) len)
                              (symbol-char-p (char text (1+ i))))
                     ;; Found a function call - get the function name
                     (let ((fn-start (1+ i))
                           (fn-end (1+ i)))
                       (loop while (and (< fn-end len)
                                        (symbol-char-p (char text fn-end)))
                             do (incf fn-end))
                       (let* ((fn-name (subseq text fn-start fn-end))
                              (arglist (handler-case
                                           (multiple-value-bind (sym)
                                               (find-symbol-in-packages fn-name)
                                             (when (and sym (fboundp sym))
                                               (sb-introspect:function-lambda-list sym)))
                                         (error () nil))))
                         (when arglist
                           ;; Show parameter names for positional args
                           (let ((arg-index 0)
                                 (pos fn-end)
                                 (param-names (remove-if
                                               (lambda (a)
                                                 (and (symbolp a)
                                                      (char= (char (symbol-name a) 0) #\&)))
                                               arglist)))
                             (loop while (and (< pos len)
                                              (< arg-index (length param-names))
                                              (not (char= (char text pos) #\))))
                                   do ;; Skip whitespace
                                      (loop while (and (< pos len)
                                                       (member (char text pos)
                                                               '(#\Space #\Tab #\Newline)))
                                            do (incf pos))
                                      (when (and (< pos len)
                                                 (not (char= (char text pos) #\)))
                                                 (< arg-index (length param-names)))
                                        (let ((param (nth arg-index param-names)))
                                          (when (and param (symbolp param)
                                                    (not (char= (char (symbol-name param) 0) #\&)))
                                            (let ((pos-lc (offset-to-line-col text pos)))
                                              (push (make-json-object
                                                     "position" (make-json-object
                                                                 "line" (car pos-lc)
                                                                 "character" (cdr pos-lc))
                                                     "label" (format nil "~(~a~):" param)
                                                     "kind" 2  ; Parameter
                                                     "paddingRight" t)
                                                    hints))))
                                        ;; Skip this argument (sexp or atom)
                                        (if (char= (char text pos) #\()
                                            ;; Skip sexp
                                            (let ((d 0))
                                              (loop while (< pos len)
                                                    do (let ((c (char text pos)))
                                                         (cond
                                                           ((char= c #\() (incf d))
                                                           ((char= c #\))
                                                            (decf d)
                                                            (when (zerop d)
                                                              (incf pos)
                                                              (return)))))
                                                       (incf pos)))
                                            ;; Skip atom
                                            (loop while (and (< pos len)
                                                             (not (member (char text pos)
                                                                          '(#\Space #\Tab #\Newline #\( #\)))))
                                                  do (incf pos)))
                                        (incf arg-index))))))))
                   (incf i)))
        (nreverse hints)))))

;;; --- Call Hierarchy ---

(defun handle-call-hierarchy-prepare (params)
  "Handle textDocument/prepareCallHierarchy."
  (let* ((td (json-get params "textDocument"))
         (uri (json-get td "uri"))
         (pos (json-get params "position"))
         (line (json-get pos "line"))
         (col (json-get pos "character"))
         (text (document-text uri)))
    (when text
      (let ((sym-name (symbol-at-position text line col)))
        (when sym-name
          (let ((range (find-symbol-range-at text line col)))
            (when range
              (destructuring-bind (sl sc el ec) range
                (list (make-json-object
                       "name" sym-name
                       "kind" 12  ; Function
                       "uri" uri
                       "range" (make-json-object
                                "start" (make-json-object "line" sl "character" sc)
                                "end" (make-json-object "line" el "character" ec))
                       "selectionRange" (make-json-object
                                         "start" (make-json-object "line" sl "character" sc)
                                         "end" (make-json-object "line" el "character" ec))))))))))))

(defun handle-call-hierarchy-incoming (params)
  "Handle callHierarchy/incomingCalls."
  (let* ((item (json-get params "item"))
         (name (json-get item "name")))
    (when name
      (lsp-log "Incoming calls: ~a" name)
      (let ((callers (symbol-incoming-calls name)))
        (mapcar (lambda (caller)
                  (destructuring-bind (caller-name path line col) caller
                    (make-json-object
                     "from" (make-json-object
                             "name" caller-name
                             "kind" 12
                             "uri" (path-to-uri path)
                             "range" (make-json-object
                                      "start" (make-json-object "line" line "character" col)
                                      "end" (make-json-object "line" line "character" col))
                             "selectionRange" (make-json-object
                                               "start" (make-json-object "line" line "character" col)
                                               "end" (make-json-object "line" line "character" col)))
                     "fromRanges" (list (make-json-object
                                         "start" (make-json-object "line" line "character" col)
                                         "end" (make-json-object "line" line "character" col))))))
                callers)))))

(defun handle-call-hierarchy-outgoing (params)
  "Handle callHierarchy/outgoingCalls."
  (let* ((item (json-get params "item"))
         (name (json-get item "name")))
    (when name
      (lsp-log "Outgoing calls: ~a" name)
      (let ((callees (symbol-outgoing-calls name)))
        (mapcar (lambda (callee)
                  (destructuring-bind (callee-name path line col) callee
                    (make-json-object
                     "to" (make-json-object
                           "name" callee-name
                           "kind" 12
                           "uri" (path-to-uri path)
                           "range" (make-json-object
                                    "start" (make-json-object "line" line "character" col)
                                    "end" (make-json-object "line" line "character" col))
                           "selectionRange" (make-json-object
                                             "start" (make-json-object "line" line "character" col)
                                             "end" (make-json-object "line" line "character" col)))
                     "fromRanges" (list (make-json-object
                                         "start" (make-json-object "line" line "character" col)
                                         "end" (make-json-object "line" line "character" col))))))
                callees)))))

;;; --- Code Lens ---

(defun handle-code-lens (params)
  "Handle textDocument/codeLens - show reference counts above definitions."
  (let* ((td (json-get params "textDocument"))
         (uri (json-get td "uri"))
         (text (document-text uri)))
    (when text
      (let ((symbols (extract-document-symbols text))
            (lenses nil))
        (dolist (sym symbols)
          (let* ((name (json-get sym "name"))
                 (range (json-get sym "range"))
                 (start (json-get range "start"))
                 (line (json-get start "line")))
            (push (make-json-object
                   "range" (make-json-object
                            "start" (make-json-object "line" line "character" 0)
                            "end" (make-json-object "line" line "character" 0))
                   "command" (make-json-object
                              "title" (let ((count (function-reference-count name)))
                                        (if count
                                            (format nil "~d reference~:p" count)
                                            "0 references"))
                              "command" ""))
                  lenses)))
        (nreverse lenses)))))

;;; --- Linked Editing Range ---

(defun handle-linked-editing-range (params)
  "Handle textDocument/linkedEditingRange - edit matching symbols."
  (let* ((td (json-get params "textDocument"))
         (uri (json-get td "uri"))
         (pos (json-get params "position"))
         (line (json-get pos "line"))
         (col (json-get pos "character"))
         (text (document-text uri)))
    (when text
      (let ((sym-name (symbol-at-position text line col)))
        (when sym-name
          (let ((occurrences (find-all-symbol-occurrences text sym-name)))
            (when (> (length occurrences) 1)
              (make-json-object
               "ranges" (mapcar (lambda (occ)
                                  (let ((start-lc (offset-to-line-col text (car occ)))
                                        (end-lc (offset-to-line-col text (cdr occ))))
                                    (make-json-object
                                     "start" (make-json-object "line" (car start-lc)
                                                               "character" (cdr start-lc))
                                     "end" (make-json-object "line" (car end-lc)
                                                             "character" (cdr end-lc)))))
                                occurrences)))))))))

;;; --- Diagnostics ---

(defun publish-diagnostics (uri)
  "Compile the document at URI and publish diagnostics."
  (let ((text (document-text uri)))
    (when text
      (let ((diags (compile-and-collect-diagnostics text)))
        (let ((notification
                (make-notification
                 "textDocument/publishDiagnostics"
                 (make-json-object
                  "uri" uri
                  "diagnostics" (or (mapcar
                                     (lambda (d)
                                       (destructuring-bind (line col message severity) d
                                         (make-json-object
                                          "range" (make-json-object
                                                    "start" (make-json-object "line" line "character" col)
                                                    "end" (make-json-object "line" line "character" col))
                                          "severity" severity
                                          "source" "sextant"
                                          "message" message)))
                                     diags)
                                    :empty-array)))))
          (write-lsp-message notification *lsp-output*))))))

;;; --- Document Sync ---

(defun handle-did-open (params)
  (let* ((td (json-get params "textDocument"))
         (uri (json-get td "uri"))
         (text (json-get td "text")))
    (document-open uri text)
    (publish-diagnostics uri)))

(defun handle-did-change (params)
  (let* ((td (json-get params "textDocument"))
         (uri (json-get td "uri"))
         (changes (json-get params "contentChanges")))
    (when changes
      (dolist (change changes)
        (apply-incremental-change uri change))
      (publish-diagnostics uri))))

(defun handle-did-save (params)
  (let* ((td (json-get params "textDocument"))
         (uri (json-get td "uri")))
    (publish-diagnostics uri)))

(defun handle-did-close (params)
  (let* ((td (json-get params "textDocument"))
         (uri (json-get td "uri")))
    (document-close uri)))
