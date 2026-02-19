(in-package :sextant)

;;; ============================================================
;;; DAP Request/Event Handlers
;;; Implements the Debug Adapter Protocol message handling
;;; ============================================================

(defvar *dap-seq* 0
  "Sequence counter for DAP events.")

(defvar *dap-output-stream* nil
  "The stream to write DAP messages to.")

(defvar *dap-initialized* nil
  "Whether the DAP session has been initialized.")

(defvar *dap-client-id* nil
  "The client's adapter ID from initialize request.")

;;; --- DAP Message Construction ---

(defun dap-next-seq ()
  "Get the next DAP sequence number."
  (incf *dap-seq*))

(defun make-dap-response (request-seq command &key body (success t) message)
  "Create a DAP response message."
  (let ((resp (make-json-object
               "seq" (dap-next-seq)
               "type" "response"
               "request_seq" request-seq
               "success" (if success t :false)
               "command" command)))
    (when body
      (push (cons "body" body) resp))
    (when (and message (not success))
      (push (cons "message" message) resp))
    resp))

(defun make-dap-event (event &optional body)
  "Create a DAP event message."
  (let ((evt (make-json-object
              "seq" (dap-next-seq)
              "type" "event"
              "event" event)))
    (when body
      (push (cons "body" body) evt))
    evt))

(defun send-dap-event (event &optional body)
  "Send a DAP event to the client."
  (when *dap-output-stream*
    (write-lsp-message (make-dap-event event body) *dap-output-stream*)))

(defun send-dap-output (category text)
  "Send an output event to the DAP client."
  (send-dap-event "output"
                   (make-json-object
                    "category" category
                    "output" text)))

;;; --- DAP Request Dispatch ---

(defun handle-dap-message (msg)
  "Dispatch a DAP message to the appropriate handler."
  (let ((msg-type (json-get msg "type"))
        (command (json-get msg "command"))
        (seq (json-get msg "seq"))
        (arguments (json-get msg "arguments")))
    (cond
      ((string= msg-type "request")
       (lsp-log "DAP request: ~a (seq=~a)" command seq)
       (let ((response (handle-dap-request command seq arguments)))
         (when response
           (write-lsp-message response *dap-output-stream*))))
      (t
       (lsp-log "DAP unknown message type: ~a" msg-type)))))

(defun handle-dap-request (command seq arguments)
  "Handle a DAP request and return a response."
  (handler-case
      (cond
        ((string= command "initialize")
         (handle-dap-initialize seq arguments))
        ((string= command "launch")
         (handle-dap-launch seq arguments))
        ((string= command "attach")
         (handle-dap-attach seq arguments))
        ((string= command "disconnect")
         (handle-dap-disconnect seq arguments))
        ((string= command "setBreakpoints")
         (handle-dap-set-breakpoints seq arguments))
        ((string= command "setFunctionBreakpoints")
         (handle-dap-set-function-breakpoints seq arguments))
        ((string= command "setExceptionBreakpoints")
         (handle-dap-set-exception-breakpoints seq arguments))
        ((string= command "configurationDone")
         (handle-dap-configuration-done seq))
        ((string= command "threads")
         (handle-dap-threads seq))
        ((string= command "stackTrace")
         (handle-dap-stack-trace seq arguments))
        ((string= command "scopes")
         (handle-dap-scopes seq arguments))
        ((string= command "variables")
         (handle-dap-variables seq arguments))
        ((string= command "continue")
         (handle-dap-continue seq arguments))
        ((string= command "next")
         (handle-dap-next seq arguments))
        ((string= command "stepIn")
         (handle-dap-step-in seq arguments))
        ((string= command "stepOut")
         (handle-dap-step-out seq arguments))
        ((string= command "evaluate")
         (handle-dap-evaluate seq arguments))
        (t
         (lsp-log "DAP unhandled command: ~a" command)
         (make-dap-response seq command :success nil
                            :message (format nil "Unhandled command: ~a" command))))
    (error (e)
      (lsp-log "DAP handler error for ~a: ~a" command e)
      (make-dap-response seq command :success nil
                         :message (format nil "~a" e)))))

;;; --- Lifecycle Handlers ---

(defun handle-dap-initialize (seq arguments)
  "Handle DAP initialize request — negotiate capabilities."
  (setf *dap-client-id* (json-get arguments "clientID"))
  (lsp-log "DAP initialize from client: ~a" *dap-client-id*)
  (let ((response (make-dap-response seq "initialize"
                    :body (make-json-object
                           "supportsConfigurationDoneRequest" t
                           "supportsFunctionBreakpoints" t
                           "supportsExceptionInfoRequest" t
                           "supportsEvaluateForHovers" t
                           "supportsSetVariable" :false
                           "supportsRestartRequest" :false
                           "supportsTerminateRequest" t
                           "supportsCompletionsRequest" :false
                           "exceptionBreakpointFilters"
                           (list (make-json-object
                                  "filter" "all"
                                  "label" "All Conditions"
                                  "description" "Break on all CL conditions"
                                  "default" :false)
                                 (make-json-object
                                  "filter" "errors"
                                  "label" "Errors Only"
                                  "description" "Break only on errors"
                                  "default" t))))))
    ;; Send initialized event after response
    (bt:make-thread
     (lambda ()
       (sleep 0.1)
       (send-dap-event "initialized"))
     :name "dap-initialized-sender")
    response))

(defun handle-dap-launch (seq arguments)
  "Handle DAP launch request — load and optionally run a file."
  (let ((program (json-get arguments "program"))
        (stop-on-entry (json-get arguments "stopOnEntry")))
    (install-dap-debugger-hook)
    (setf *dap-debugger-active* t)
    (when program
      (lsp-log "DAP launch: loading ~a" program)
      (bt:make-thread
       (lambda ()
         (handler-case
             (progn
               (send-dap-output "console"
                                (format nil "Loading ~a into Sextant image...~%" program))
               (load program)
               (send-dap-output "console"
                                (format nil "Loaded ~a successfully.~%" program))
               (unless stop-on-entry
                 (send-dap-event "terminated")))
           (error (e)
             (send-dap-output "stderr" (format nil "Error loading ~a: ~a~%" program e))
             (send-dap-event "terminated"))))
       :name "dap-launch-thread"))
    (make-dap-response seq "launch")))

(defun handle-dap-attach (seq arguments)
  "Handle DAP attach request — hook into the running SBCL image."
  (declare (ignore arguments))
  (install-dap-debugger-hook)
  (setf *dap-debugger-active* t)
  (send-dap-output "console" (format nil "Attached to Sextant SBCL image.~%"))
  (send-dap-output "console"
                    (format nil "SBCL ~a, ~a packages loaded.~%"
                            (lisp-implementation-version)
                            (length (list-all-packages))))
  (make-dap-response seq "attach"))

(defun handle-dap-disconnect (seq arguments)
  "Handle DAP disconnect request — detach debugger."
  (declare (ignore arguments))
  (uninstall-dap-debugger-hook)
  (setf *dap-initialized* nil)
  ;; If a thread is waiting, let it continue
  (dap-signal-continue)
  (make-dap-response seq "disconnect"))

(defun handle-dap-configuration-done (seq)
  "Handle DAP configurationDone request."
  (setf *dap-initialized* t)
  (lsp-log "DAP configuration done")
  (make-dap-response seq "configurationDone"))

;;; --- Breakpoint Handlers ---

(defun handle-dap-set-breakpoints (seq arguments)
  "Handle DAP setBreakpoints request."
  (let* ((source (json-get arguments "source"))
         (path (json-get source "path"))
         (breakpoints (json-get arguments "breakpoints"))
         (result-bps nil))
    (when path
      (let ((lines nil))
        (dolist (bp breakpoints)
          (let ((line (json-get bp "line")))
            (push line lines)
            (push (make-json-object
                   "verified" t
                   "line" line
                   "source" source)
                  result-bps)))
        (setf (gethash path *dap-breakpoints*) (nreverse lines))))
    (make-dap-response seq "setBreakpoints"
      :body (make-json-object
             "breakpoints" (nreverse result-bps)))))

(defun handle-dap-set-function-breakpoints (seq arguments)
  "Handle DAP setFunctionBreakpoints request."
  (clrhash *dap-function-breakpoints*)
  (let ((breakpoints (json-get arguments "breakpoints"))
        (result-bps nil))
    (dolist (bp breakpoints)
      (let ((name (json-get bp "name")))
        (setf (gethash name *dap-function-breakpoints*) t)
        (push (make-json-object
               "verified" t)
              result-bps)))
    (make-dap-response seq "setFunctionBreakpoints"
      :body (make-json-object
             "breakpoints" (nreverse result-bps)))))

(defun handle-dap-set-exception-breakpoints (seq arguments)
  "Handle DAP setExceptionBreakpoints request."
  (let ((filters (json-get arguments "filters")))
    (setf *dap-break-on-exceptions*
          (or (member "all" filters :test #'string=)
              (member "errors" filters :test #'string=)))
    (lsp-log "DAP exception breakpoints: ~a (active: ~a)" filters *dap-break-on-exceptions*)
    (make-dap-response seq "setExceptionBreakpoints")))

;;; --- Inspection Handlers ---

(defun handle-dap-threads (seq)
  "Handle DAP threads request."
  (let ((threads (list-all-threads)))
    (make-dap-response seq "threads"
      :body (make-json-object
             "threads" (mapcar (lambda (th)
                                 (make-json-object
                                  "id" (getf th :id)
                                  "name" (or (getf th :name) "unnamed")))
                               threads)))))

(defun handle-dap-stack-trace (seq arguments)
  "Handle DAP stackTrace request."
  (declare (ignore arguments))
  (let ((frames (or *dap-current-frames* nil)))
    (make-dap-response seq "stackTrace"
      :body (make-json-object
             "stackFrames"
             (mapcar (lambda (frame)
                       (let ((result (make-json-object
                                      "id" (getf frame :frame-index)
                                      "name" (getf frame :name)
                                      "line" (or (getf frame :line) 0)
                                      "column" (or (getf frame :column) 0))))
                         (when (getf frame :file)
                           (push (cons "source"
                                       (make-json-object
                                        "name" (file-namestring (getf frame :file))
                                        "path" (getf frame :file)))
                                 result))
                         result))
                     frames)
             "totalFrames" (length frames)))))

(defun handle-dap-scopes (seq arguments)
  "Handle DAP scopes request — return scope info for a frame."
  (let* ((frame-id (json-get arguments "frameId"))
         (locals-ref (allocate-var-ref frame-id :locals)))
    (make-dap-response seq "scopes"
      :body (make-json-object
             "scopes" (list (make-json-object
                             "name" "Locals"
                             "variablesReference" locals-ref
                             "expensive" :false))))))

(defun handle-dap-variables (seq arguments)
  "Handle DAP variables request — return variables for a scope."
  (let* ((var-ref (json-get arguments "variablesReference"))
         (ref-info (gethash var-ref *dap-var-ref-map*))
         (variables nil))
    (when ref-info
      (let ((frame-index (first ref-info)))
        (setf variables (get-frame-locals frame-index))))
    (make-dap-response seq "variables"
      :body (make-json-object
             "variables"
             (mapcar (lambda (var)
                       (make-json-object
                        "name" (getf var :name)
                        "value" (getf var :value)
                        "type" (getf var :type)
                        "variablesReference" 0))
                     (or variables nil))))))

;;; --- Execution Control ---

(defun handle-dap-continue (seq arguments)
  "Handle DAP continue request — resume execution."
  (declare (ignore arguments))
  (setf *dap-stepping-mode* nil)
  (dap-signal-continue)
  (send-dap-event "continued" (make-json-object "threadId" 1 "allThreadsContinued" t))
  (make-dap-response seq "continue"
    :body (make-json-object "allThreadsContinued" t)))

(defun handle-dap-next (seq arguments)
  "Handle DAP next (step over) request."
  (declare (ignore arguments))
  (setf *dap-stepping-mode* :over)
  (dap-signal-continue)
  (make-dap-response seq "next"))

(defun handle-dap-step-in (seq arguments)
  "Handle DAP stepIn request."
  (declare (ignore arguments))
  (setf *dap-stepping-mode* :into)
  (dap-signal-continue)
  (make-dap-response seq "stepIn"))

(defun handle-dap-step-out (seq arguments)
  "Handle DAP stepOut request."
  (declare (ignore arguments))
  (setf *dap-stepping-mode* :out)
  (dap-signal-continue)
  (make-dap-response seq "stepOut"))

;;; --- Evaluate ---

(defun handle-dap-evaluate (seq arguments)
  "Handle DAP evaluate request — eval expression in debug context."
  (let* ((expression (json-get arguments "expression"))
         (context (json-get arguments "context")))
    (lsp-log "DAP evaluate: ~a (context: ~a)" expression context)
    ;; Handle restart commands
    (cond
      ((and expression
            (or (cl-ppcre:scan "^:r(?:estart)?\\s+(\\d+)" expression)
                (cl-ppcre:scan "^:restart\\s+(\\d+)" expression)))
       (multiple-value-bind (match groups)
           (cl-ppcre:scan-to-strings ":r(?:estart)?\\s+(\\d+)" expression)
         (declare (ignore match))
         (when groups
           (let ((index (parse-integer (aref groups 0))))
             (send-dap-output "console"
                              (format nil "Invoking restart ~d...~%" index))
             ;; Invoke restart in a separate thread to not block DAP
             (bt:make-thread
              (lambda ()
                (invoke-restart-by-index index))
              :name "dap-restart-invoker"))))
       (make-dap-response seq "evaluate"
         :body (make-json-object
                "result" (format nil "Invoking restart...")
                "variablesReference" 0)))
      ;; Handle :restarts command — show available restarts
      ((and expression (string= (string-trim " " expression) ":restarts"))
       (let ((restarts (get-condition-restarts)))
         (let ((text (with-output-to-string (s)
                       (format s "Available restarts:~%")
                       (dolist (r restarts)
                         (format s "  ~d: [~a] ~a~%"
                                 (getf r :index)
                                 (getf r :name)
                                 (getf r :description))))))
           (make-dap-response seq "evaluate"
             :body (make-json-object
                    "result" text
                    "variablesReference" 0)))))
      ;; Regular evaluation
      (t
       (let ((result (eval-in-context expression)))
         (make-dap-response seq "evaluate"
           :body (make-json-object
                  "result" result
                  "variablesReference" 0)))))))
