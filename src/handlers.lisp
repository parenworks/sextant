(in-package :sextant)

;;; ============================================================
;;; LSP Method Handlers
;;; Dispatches JSON-RPC methods to implementation functions
;;; ============================================================

(defvar *server-capabilities*
  (make-json-object
   "textDocumentSync" (make-json-object
                       "openClose" t
                       "change" 1  ; Full sync
                       "save" t)
   "hoverProvider" t
   "completionProvider" (make-json-object
                         "triggerCharacters" (list "(" ":" "-"))
   "definitionProvider" t
   "signatureHelpProvider" (make-json-object
                            "triggerCharacters" (list " " "("))
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
    ((string= method "textDocument/didSave") nil)
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
          (let ((loc (symbol-definition-location sym-name)))
            (when loc
              (destructuring-bind (path def-line def-col) loc
                (make-json-object
                 "uri" (path-to-uri path)
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

;;; --- Document Sync ---

(defun handle-did-open (params)
  (let* ((td (json-get params "textDocument"))
         (uri (json-get td "uri"))
         (text (json-get td "text")))
    (document-open uri text)))

(defun handle-did-change (params)
  (let* ((td (json-get params "textDocument"))
         (uri (json-get td "uri"))
         (changes (json-get params "contentChanges"))
         (text (when (and changes (first changes))
                 (json-get (first changes) "text"))))
    (when text
      (document-change uri text))))

(defun handle-did-close (params)
  (let* ((td (json-get params "textDocument"))
         (uri (json-get td "uri")))
    (document-close uri)))
