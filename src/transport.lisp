(in-package :sextant)

;;; ============================================================
;;; LSP JSON-RPC Transport Layer
;;; Reads/writes LSP messages over stdio (stdin/stdout)
;;; Format: Content-Length: N\r\n\r\n{json}
;;; ============================================================

(defvar *lsp-input* *standard-input*
  "Input stream for LSP messages.")

(defvar *lsp-output* *standard-output*
  "Output stream for LSP messages.")

(defvar *lsp-log* nil
  "Stream for debug logging. NIL to disable.")

(defun lsp-log (fmt &rest args)
  "Log a debug message if logging is enabled."
  (when *lsp-log*
    (apply #'format *lsp-log* fmt args)
    (terpri *lsp-log*)
    (force-output *lsp-log*)))

(defun read-lsp-message (&optional (stream *lsp-input*))
  "Read one LSP message from STREAM. Returns parsed JSON alist or NIL on EOF."
  (let ((content-length nil))
    ;; Read headers
    (loop for line = (read-line stream nil nil)
          while line
          do (let ((trimmed (string-trim '(#\Return #\Newline #\Space) line)))
               (when (zerop (length trimmed))
                 (return))
               (when (cl-ppcre:scan "(?i)^content-length:\\s*(\\d+)" trimmed)
                 (multiple-value-bind (match groups)
                     (cl-ppcre:scan-to-strings "(?i)^content-length:\\s*(\\d+)" trimmed)
                   (declare (ignore match))
                   (when groups
                     (setf content-length (parse-integer (aref groups 0))))))))
    (unless content-length
      (return-from read-lsp-message nil))
    ;; Read body
    (let ((buf (make-string content-length)))
      (let ((n (read-sequence buf stream)))
        (when (< n content-length)
          (return-from read-lsp-message nil)))
      (lsp-log "<<< ~a" buf)
      (handler-case (json-parse buf)
        (error (e)
          (lsp-log "JSON parse error: ~a" e)
          nil)))))

(defun write-lsp-message (obj &optional (stream *lsp-output*))
  "Write OBJ as an LSP JSON-RPC message to STREAM."
  (let ((body (json-to-string obj)))
    (lsp-log ">>> ~a" body)
    (format stream "Content-Length: ~d~c~c~c~c~a"
            (length (babel:string-to-octets body :encoding :utf-8))
            #\Return #\Linefeed #\Return #\Linefeed
            body)
    (force-output stream)))

(defun make-response (id result)
  "Create a JSON-RPC response for request ID with RESULT."
  (make-json-object "jsonrpc" "2.0"
                    "id" id
                    "result" result))

(defun make-error-response (id code message)
  "Create a JSON-RPC error response."
  (make-json-object "jsonrpc" "2.0"
                    "id" id
                    "error" (make-json-object "code" code
                                              "message" message)))

(defun make-notification (method &optional params)
  "Create a JSON-RPC notification (no id)."
  (if params
      (make-json-object "jsonrpc" "2.0"
                        "method" method
                        "params" params)
      (make-json-object "jsonrpc" "2.0"
                        "method" method)))
