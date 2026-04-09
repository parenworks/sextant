;;; test-diagnostics.lisp - Test file for Sextant LSP diagnostics
;;; Open this file in Emacs with eglot/lsp-mode connected to sextant.
;;; You should see inline warnings and errors for each issue below.

;; 1. Unused variable - Y is never referenced
(defun add-one (x y)
  (+ x 1))

;; 2. Undefined function - this-does-not-exist is not defined
(defun call-fake (x)
  (this-does-not-exist x))

;; 3. Wrong argument count - format wants at least 2 args
(defun bad-call ()
  (+ 1 2 3)
  (list 1 2)
  (gethash))

;; 4. Type mismatch - adding a string to a number
(defun type-error-example (x)
  (+ x "not a number"))

;; 5. Undefined variable reference
(defun uses-unbound ()
  (+ mystery-variable 42))

;; 6. This function is clean - no warnings expected
(defun perfectly-fine (x y)
  (+ x y))
