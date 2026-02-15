(in-package :sextant)

;;; ============================================================
;;; LSP Server Main Loop
;;; Reads messages from stdin, dispatches, writes responses
;;; ============================================================

(defun start-server (&key (log-file nil))
  "Start the Sextant LSP server on stdio."
  (when log-file
    (setf *lsp-log* (open log-file :direction :output
                                    :if-exists :append
                                    :if-does-not-exist :create)))
  (lsp-log "Sextant LSP server starting...")
  (unwind-protect
      (loop
        (let ((msg (read-lsp-message *lsp-input*)))
          (unless msg
            (lsp-log "EOF on input, exiting")
            (return))
          (let ((method (json-get msg "method"))
                (id (json-get msg "id"))
                (params (json-get msg "params")))
            (cond
              ;; Request (has id and method)
              ((and id method)
               (let ((response (handle-request method id params)))
                 (write-lsp-message response *lsp-output*)))
              ;; Response to our request (has id, no method) - ignore for now
              ((and id (not method))
               (lsp-log "Got response for id ~a" id))
              ;; Notification (has method, no id)
              (method
               (handle-notification method params))
              (t
               (lsp-log "Unknown message format"))))))
    (when *lsp-log*
      (lsp-log "Sextant LSP server shutting down")
      (close *lsp-log*)
      (setf *lsp-log* nil))))
