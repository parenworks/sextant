(in-package :sextant)

;;; ============================================================
;;; SBCL Debugger Integration
;;; Hooks into SBCL's condition system and debug internals
;;; ============================================================

(defvar *dap-debugger-active* nil
  "Whether the DAP debugger is currently active.")

(defvar *dap-stopped-callback* nil
  "Function called when a condition is caught. Takes (reason thread).")

(defvar *dap-breakpoints* (make-hash-table :test 'equal)
  "Hash of file-path -> list of line numbers for breakpoints.")

(defvar *dap-function-breakpoints* (make-hash-table :test 'equal)
  "Hash of function-name -> t for function breakpoints.")

(defvar *dap-break-on-exceptions* nil
  "Whether to break on all conditions.")

(defvar *dap-current-condition* nil
  "The condition currently being debugged.")

(defvar *dap-current-frames* nil
  "Cached stack frames for the current stopped state.")

(defvar *dap-stepping-mode* nil
  "Current stepping mode: nil, :into, :over, :out.")

(defvar *dap-frame-vars-cache* (make-hash-table)
  "Cache of frame-id -> variable list.")

(defvar *dap-next-var-ref* 1
  "Next variable reference ID for DAP.")

(defvar *dap-var-ref-map* (make-hash-table)
  "Map of variable reference ID -> (frame-index scope-type).")

(defvar *dap-stop-reason* nil
  "Why the debugger last stopped: :exception, :breakpoint, :step, :entry.")

(defvar *dap-installed-bp-symbols* (make-hash-table :test 'equal)
  "Map of encapsulation key -> function symbol, for cleanup.
Key is (file-path . line) for line breakpoints, or function name string
for function breakpoints.")

(defvar *dap-installed-fn-bps* (make-hash-table :test 'equal)
  "Set of function names (strings) that currently have a breakpoint
encapsulation installed.")

(defun dap-stop (reason &optional condition)
  "Capture frames, notify the client, and block until told to continue.
REASON is one of :exception, :breakpoint, :step, :entry.
CONDITION is the condition object (for :exception) or NIL."
  (setf *dap-stop-reason* reason)
  (setf *dap-current-condition* condition)
  (setf *dap-current-frames* (capture-stack-frames))
  (clrhash *dap-frame-vars-cache*)
  (setf *dap-next-var-ref* 1)
  (clrhash *dap-var-ref-map*)
  (when *dap-stopped-callback*
    (funcall *dap-stopped-callback* reason
             sb-thread:*current-thread*))
  ;; Block this thread until the debugger tells us to continue
  (dap-wait-for-continue))

(defun install-dap-debugger-hook ()
  "Install our custom debugger hook to intercept conditions."
  (setf sb-ext:*invoke-debugger-hook*
        (lambda (condition hook)
          (declare (ignore hook))
          (when (and *dap-debugger-active*
                     *dap-stopped-callback*
                     (or *dap-break-on-exceptions*
                         (typep condition 'error)))
            (dap-stop :exception condition))))
  (lsp-log "DAP debugger hook installed"))

(defun uninstall-dap-debugger-hook ()
  "Remove our custom debugger hook and all installed breakpoints."
  (setf sb-ext:*invoke-debugger-hook* nil)
  (setf *dap-debugger-active* nil)
  (remove-all-breakpoints)
  (lsp-log "DAP debugger hook removed"))

(defvar *dap-continue-lock* (bt:make-lock "dap-continue-lock"))
(defvar *dap-continue-cv* (bt:make-condition-variable :name "dap-continue-cv"))
(defvar *dap-should-continue* nil)

(defun dap-wait-for-continue ()
  "Block the current thread until the DAP client sends a continue command."
  (setf *dap-should-continue* nil)
  (bt:with-lock-held (*dap-continue-lock*)
    (loop until *dap-should-continue*
          do (bt:condition-wait *dap-continue-cv* *dap-continue-lock*))))

(defun dap-signal-continue ()
  "Signal the debugged thread to continue execution."
  (bt:with-lock-held (*dap-continue-lock*)
    (setf *dap-should-continue* t)
    (bt:condition-notify *dap-continue-cv*)))

;;; --- Breakpoint Installation ---

(defun find-function-for-breakpoint (file-path line)
  "Find the function symbol whose definition encloses LINE in FILE-PATH.
LINE is 1-based (DAP convention). Returns a symbol or NIL."
  (let ((entry (index-find-enclosing-definition file-path (1- line))))
    (when entry
      (let ((name (index-entry-name entry)))
        (find-symbol-in-packages name)))))

(defun make-breakpoint-wrapper (fn-sym)
  "Create a breakpoint wrapper function for FN-SYM.
The wrapper signals a breakpoint stop when the debugger is active,
then calls the original function."
  (lambda (&rest args)
    (if (and *dap-debugger-active* *dap-stopped-callback*)
        (progn
          (dap-stop :breakpoint nil)
          (apply fn-sym args))
        (apply fn-sym args))))

(defun install-line-breakpoint (file-path line)
  "Install a breakpoint at FILE-PATH:LINE by encapsulating the
enclosing function. Returns T if a breakpoint was installed, NIL
if no enclosing function was found."
  (let ((fn-sym (find-function-for-breakpoint file-path line)))
    (when (and fn-sym (fboundp fn-sym))
      (let ((key (cons file-path line)))
        ;; Don't double-install on the same function
        (unless (gethash key *dap-installed-bp-symbols*)
          (handler-case
              (progn
                (sb-int:encapsulate fn-sym 'sextant-dap-bp
                  (make-breakpoint-wrapper fn-sym))
                (setf (gethash key *dap-installed-bp-symbols*) fn-sym)
                (lsp-log "Breakpoint installed: ~a:~d -> ~a" file-path line fn-sym)
                t)
            (error (e)
              (lsp-log "Failed to install breakpoint ~a:~d: ~a" file-path line e)
              nil)))))))

(defun remove-line-breakpoint (file-path line)
  "Remove a breakpoint at FILE-PATH:LINE."
  (let ((key (cons file-path line)))
    (let ((fn-sym (gethash key *dap-installed-bp-symbols*)))
      (when fn-sym
        (handler-case
            (sb-int:unencapsulate fn-sym 'sextant-dap-bp)
          (error () nil))
        (remhash key *dap-installed-bp-symbols*)
        (lsp-log "Breakpoint removed: ~a:~d" file-path line)))))

(defun install-function-breakpoint (fn-name)
  "Install a function breakpoint on the function named FN-NAME (string).
Returns T if installed, NIL if the function was not found."
  (multiple-value-bind (sym) (find-symbol-in-packages fn-name)
    (when (and sym (fboundp sym))
      (unless (gethash fn-name *dap-installed-fn-bps*)
        (handler-case
            (progn
              (sb-int:encapsulate sym 'sextant-dap-fn-bp
                (make-breakpoint-wrapper sym))
              (setf (gethash fn-name *dap-installed-fn-bps*) sym)
              (lsp-log "Function breakpoint installed: ~a" fn-name)
              t)
          (error (e)
            (lsp-log "Failed to install function breakpoint ~a: ~a" fn-name e)
            nil))))))

(defun remove-function-breakpoint (fn-name)
  "Remove a function breakpoint on the function named FN-NAME."
  (let ((sym (gethash fn-name *dap-installed-fn-bps*)))
    (when sym
      (handler-case
          (sb-int:unencapsulate sym 'sextant-dap-fn-bp)
        (error () nil))
      (remhash fn-name *dap-installed-fn-bps*)
      (lsp-log "Function breakpoint removed: ~a" fn-name))))

(defun remove-all-breakpoints ()
  "Remove all installed line and function breakpoints."
  (maphash (lambda (key fn-sym)
             (declare (ignore key))
             (handler-case
                 (sb-int:unencapsulate fn-sym 'sextant-dap-bp)
               (error () nil)))
           *dap-installed-bp-symbols*)
  (clrhash *dap-installed-bp-symbols*)
  (maphash (lambda (fn-name sym)
             (declare (ignore fn-name))
             (handler-case
                 (sb-int:unencapsulate sym 'sextant-dap-fn-bp)
               (error () nil)))
           *dap-installed-fn-bps*)
  (clrhash *dap-installed-fn-bps*))

(defun sync-line-breakpoints (file-path lines)
  "Synchronize line breakpoints for FILE-PATH to match the given LINES list.
Installs new breakpoints and removes ones that are no longer present."
  (let ((old-lines (gethash file-path *dap-breakpoints*))
        (new-lines (sort (copy-list lines) #'<)))
    ;; Remove breakpoints that are no longer present
    (dolist (old-line old-lines)
      (unless (member old-line new-lines)
        (remove-line-breakpoint file-path old-line)))
    ;; Install new breakpoints
    (dolist (new-line new-lines)
      (unless (member new-line old-lines)
        (install-line-breakpoint file-path new-line)))
    ;; Update the stored hash
    (setf (gethash file-path *dap-breakpoints*) new-lines)))

(defun sync-function-breakpoints (fn-names)
  "Synchronize function breakpoints to match FN-NAMES (list of strings)."
  ;; Remove breakpoints no longer present
  (maphash (lambda (fn-name sym)
             (declare (ignore sym))
             (unless (member fn-name fn-names :test #'string=)
               (remove-function-breakpoint fn-name)))
           *dap-installed-fn-bps*)
  ;; Install new breakpoints
  (dolist (fn-name fn-names)
    (unless (gethash fn-name *dap-installed-fn-bps*)
      (install-function-breakpoint fn-name))))

(defun install-pending-breakpoints ()
  "Install all breakpoints that have been requested via setBreakpoints
and setFunctionBreakpoints but not yet installed (e.g. because the
debugger was not active when they were set)."
  (maphash (lambda (path lines)
             (dolist (line lines)
               (install-line-breakpoint path line)))
           *dap-breakpoints*)
  (maphash (lambda (fn-name dummy)
             (declare (ignore dummy))
             (install-function-breakpoint fn-name))
           *dap-function-breakpoints*))

;;; --- Stack Frame Capture ---

(defun code-location-to-line (location)
  "Extract a 1-based line number from an SBCL code-location.
Uses the debug-source start-positions array to find the character
offset of the enclosing toplevel form, then counts newlines in the
source file to convert that to a line number.
Returns 0 if the line cannot be determined."
  (handler-case
      (let* ((source (sb-di:code-location-debug-source location))
             (namestring (sb-di:debug-source-namestring source))
             (tlo (sb-di:code-location-toplevel-form-offset location))
             (start-positions (sb-di:debug-source-start-positions source)))
        (when (and namestring start-positions
                   (integerp tlo)
                   (< tlo (length start-positions)))
          (let ((char-offset (aref start-positions tlo)))
            (handler-case
                (with-open-file (s namestring :direction :input
                                        :if-does-not-exist nil)
                  (if s
                      (let ((content (make-string
                                      (min char-offset (file-length s)))))
                        (read-sequence content s)
                        (1+ (count #\Newline content)))
                      0))
              (error () 0)))))
    (error () 0)))

(defun capture-stack-frames ()
  "Capture the current backtrace as a list of frame info alists.
Each entry: (:name :file :line :column :frame-index)"
  (let ((frames nil)
        (index 0))
    (handler-case
        (sb-debug:map-backtrace
         (lambda (frame)
           (let* ((debug-fun (handler-case
                                 (sb-di:frame-debug-fun frame)
                               (error () nil)))
                  (name (if debug-fun
                            (handler-case
                                (sb-di:debug-fun-name debug-fun)
                              (error () "unknown"))
                            "unknown"))
                  (location (handler-case
                                (sb-di:frame-code-location frame)
                              (error () nil)))
                  (source (when location
                            (handler-case
                                (sb-di:code-location-debug-source location)
                              (error () nil))))
                  (file (when source
                          (handler-case
                              (sb-di:debug-source-namestring source)
                            (error () nil))))
                  (line (if location
                            (code-location-to-line location)
                            0)))
             (push (list :name (format nil "~a" name)
                         :file file
                         :line line
                         :column 0
                         :frame-index index)
                   frames)
             (incf index)))
         :from :current-frame)
      (error (e)
        (lsp-log "Error capturing frames: ~a" e)))
    (nreverse frames)))

(defun get-frame-locals (frame-index)
  "Get local variables for a stack frame by index.
Returns list of (:name :value :type)."
  (or (gethash frame-index *dap-frame-vars-cache*)
      (let ((vars nil))
        (handler-case
            (sb-debug:map-backtrace
             (lambda (frame)
               (when (= (sb-di:frame-number frame) frame-index)
                 (handler-case
                     (let ((debug-fun (sb-di:frame-debug-fun frame)))
                       (handler-case
                           (sb-di:do-debug-fun-vars (var debug-fun)
                             (handler-case
                                 (let* ((loc (sb-di:frame-code-location frame))
                                        (valid (sb-di:debug-var-validity var loc)))
                                   (when (eq valid :valid)
                                     (let ((name (sb-di:debug-var-symbol var))
                                           (value (sb-di:debug-var-value var frame)))
                                       (push (list :name (format nil "~a" name)
                                                   :value (format nil "~s" value)
                                                   :type (format nil "~a" (type-of value)))
                                             vars))))
                               (error () nil)))
                         (error () nil)))
                   (error () nil))
                 (return-from get-frame-locals
                   (setf (gethash frame-index *dap-frame-vars-cache*)
                         (nreverse vars)))))
             :from :current-frame)
          (error (e)
            (lsp-log "Error getting frame locals: ~a" e)))
        (setf (gethash frame-index *dap-frame-vars-cache*)
              (nreverse vars)))))

(defun allocate-var-ref (frame-index scope-type)
  "Allocate a variable reference ID for a frame/scope pair."
  (let ((ref *dap-next-var-ref*))
    (incf *dap-next-var-ref*)
    (setf (gethash ref *dap-var-ref-map*)
          (list frame-index scope-type))
    ref))

(defun get-condition-restarts ()
  "Get available restarts for the current condition.
Returns list of (:name :description :index)."
  (when *dap-current-condition*
    (let ((restarts (compute-restarts *dap-current-condition*))
          (result nil)
          (index 0))
      (dolist (restart restarts)
        (push (list :name (format nil "~a" (restart-name restart))
                    :description (format nil "~a" restart)
                    :index index)
              result)
        (incf index))
      (nreverse result))))

(defun invoke-restart-by-index (index)
  "Invoke a restart by its index in the current restart list."
  (when *dap-current-condition*
    (let ((restarts (compute-restarts *dap-current-condition*)))
      (when (< index (length restarts))
        (invoke-restart (nth index restarts))))))

(defun eval-in-context (expression &key (allow-debugger nil))
  "Evaluate an expression string in the current debug context.
When ALLOW-DEBUGGER is true, let errors propagate to the debugger hook."
  (if allow-debugger
      ;; Let errors trigger the debugger hook (for REPL eval)
      (let ((form (read-from-string expression)))
        (format nil "~s" (eval form)))
      ;; Catch errors and return as text (for hover eval etc.)
      (handler-case
          (let ((form (read-from-string expression)))
            (format nil "~s" (eval form)))
        (error (e)
          (format nil "Error: ~a" e)))))

(defun list-all-threads ()
  "List all SBCL threads. Returns list of (:id :name)."
  (let ((threads (sb-thread:list-all-threads))
        (result nil)
        (id 1))
    (dolist (thread threads)
      (push (list :id id
                  :name (sb-thread:thread-name thread))
            result)
      (incf id))
    (nreverse result)))
