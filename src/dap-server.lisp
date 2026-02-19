(in-package :sextant)

;;; ============================================================
;;; DAP Server
;;; TCP listener that runs alongside the LSP server
;;; Reuses the same Content-Length JSON-RPC transport
;;; ============================================================

(defvar *dap-port* 6009
  "TCP port for the DAP server.")

(defvar *dap-server-socket* nil
  "The DAP server's listening socket.")

(defvar *dap-thread* nil
  "The thread running the DAP server.")

(defun start-dap-server (&key (port *dap-port*))
  "Start the DAP TCP server on PORT in a background thread.
Called from the LSP server's start-server function."
  (setf *dap-port* port)
  (setf *dap-thread*
        (bt:make-thread
         (lambda ()
           (lsp-log "DAP server starting on port ~d..." port)
           (handler-case
               (let ((server (make-instance 'sb-bsd-sockets:inet-socket
                                            :type :stream :protocol :tcp)))
                 (setf (sb-bsd-sockets:sockopt-reuse-address server) t)
                 (sb-bsd-sockets:socket-bind server #(127 0 0 1) port)
                 (sb-bsd-sockets:socket-listen server 1)
                 (setf *dap-server-socket* server)
                 (lsp-log "DAP server listening on localhost:~d" port)
                 ;; Accept loop — handle one client at a time
                 (loop
                   (handler-case
                       (let ((client (sb-bsd-sockets:socket-accept server)))
                         (lsp-log "DAP client connected")
                         (bt:make-thread
                          (lambda ()
                            (handle-dap-connection client))
                          :name "dap-client-handler"))
                     (error (e)
                       (lsp-log "DAP accept error: ~a" e)
                       (return)))))
             (error (e)
               (lsp-log "DAP server error: ~a" e))))
         :name "dap-server")))

(defun stop-dap-server ()
  "Stop the DAP TCP server."
  (when *dap-server-socket*
    (handler-case
        (sb-bsd-sockets:socket-close *dap-server-socket*)
      (error () nil))
    (setf *dap-server-socket* nil))
  (lsp-log "DAP server stopped"))

(defun handle-dap-connection (socket)
  "Handle a single DAP client connection on SOCKET."
  (let* ((stream (sb-bsd-sockets:socket-make-stream
                  socket :input t :output t
                  :element-type 'character
                  :buffering :line)))
    (setf *dap-output-stream* stream)
    ;; Set up the stopped callback to send DAP events
    (setf *dap-stopped-callback*
          (lambda (condition thread)
            (declare (ignore thread))
            (lsp-log "DAP stopped: ~a" condition)
            ;; Send stopped event
            (send-dap-event "stopped"
                            (make-json-object
                             "reason" "exception"
                             "description" (format nil "~a" condition)
                             "threadId" 1
                             "allThreadsStopped" t
                             "text" (format nil "~a" (type-of condition))))
            ;; Show restarts in debug console
            (let ((restarts (get-condition-restarts)))
              (send-dap-output "console"
                               (format nil "~%Condition: ~a~%~%" condition))
              (send-dap-output "console" "Available restarts:~%")
              (dolist (r restarts)
                (send-dap-output "console"
                                 (format nil "  :restart ~d  [~a] ~a~%"
                                         (getf r :index)
                                         (getf r :name)
                                         (getf r :description)))))))
    (unwind-protect
        (loop
          (let ((msg (handler-case
                         (read-lsp-message stream)
                       (error (e)
                         (lsp-log "DAP read error: ~a" e)
                         nil))))
            (unless msg
              (lsp-log "DAP client disconnected")
              (return))
            (handle-dap-message msg)))
      ;; Cleanup
      (setf *dap-output-stream* nil)
      (setf *dap-stopped-callback* nil)
      (uninstall-dap-debugger-hook)
      (handler-case
          (sb-bsd-sockets:socket-close socket)
        (error () nil))
      (lsp-log "DAP connection closed"))))
