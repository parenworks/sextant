(in-package :sextant)

;;; ============================================================
;;; Entry Point
;;; ============================================================

(defun main ()
  "Entry point for the Sextant LSP server."
  (let ((log-file (or (uiop:getenv "SEXTANT_LOG")
                      "/tmp/sextant.log")))
    (start-server :log-file log-file)))
