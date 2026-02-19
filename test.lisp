;;; Sextant LSP Test File
;;; Open this in Neovim to test all LSP capabilities
;;;
;;; Lines marked with ← tell you where to place your cursor.
;;; "cursor on X" means put cursor anywhere on that word.

(defpackage :sextant-test
  (:use :cl)
  (:export #:greet
           #:add-numbers))

(in-package :sextant-test)

;;; ================================================================
;;; TEST 1: Hover — press K with cursor on the symbol
;;; ================================================================

(defvar *greeting* "Hello"            ; ← K on *greeting* to see docs
  "The default greeting string.")

(defparameter *count* 0               ; ← K on *count*
  "A mutable counter.")

;;; ================================================================
;;; TEST 2: Go to Definition — press gd with cursor on the symbol
;;; ================================================================

(defun greet (name)
  "Greet someone by NAME."
  (format nil "~a, ~a!" *greeting* name))

(defun add-numbers (a b)
  "Add two numbers A and B."
  (+ a b))

(defun fancy-greet (name &key (times 1) (loud nil))
  "Greet NAME multiple times, optionally LOUD."
  (dotimes (i times)
    (let ((msg (greet name)))          ; ← gd on greet → jumps to line 31
      (if loud
          (format t "~a~%" (string-upcase msg))
          (format t "~a~%" msg)))))

;;; ================================================================
;;; TEST 3: Find References — press gr with cursor on the symbol
;;; ================================================================

(defun test-references ()
  (greet "Alice")                      ; ← gr on greet → shows all call sites
  (greet "Bob")
  (add-numbers 1 2))                   ; ← gr on add-numbers

;;; ================================================================
;;; TEST 4: Signature Help — type '(format ' in insert mode
;;; ================================================================
;;; Go to a blank line below, enter insert mode, type:  (format
;;; then press space — you should see the arglist popup.
;;;
;;; Try here:


;;; ================================================================
;;; TEST 5: Completions — type a prefix in insert mode
;;; ================================================================
;;; Go to a blank line below, enter insert mode, type:  (str
;;; Wait for the completion popup.
;;;
;;; Try here:


;;; ================================================================
;;; TEST 6: Document Symbols — press <leader>ss
;;; ================================================================
;;; You should see an outline listing all def* forms in this file.

(defclass my-class ()
  ((name :initarg :name :accessor my-name)
   (value :initarg :value :accessor my-value))
  (:documentation "A simple test class."))

(defmethod describe-object ((obj my-class) stream)
  (format stream "my-class: name=~a value=~a"
          (my-name obj) (my-value obj)))

;;; ================================================================
;;; TEST 7: Workspace Symbols — press <leader>sS, type "format"
;;; ================================================================
;;; Should show format, formatter, etc. from all packages.

;;; ================================================================
;;; TEST 8: Document Highlight — move cursor onto a symbol
;;; ================================================================

(defun helper (x) (* x x))
(defun use-helper (a b)
  (+ (helper a) (helper b)))           ; ← cursor on helper → all 3 highlighted

;;; ================================================================
;;; TEST 9: Folding — zM to fold all, zR to unfold, zc/zo per form
;;; ================================================================

(defun long-function ()
  "This function has many lines for folding."
  (let ((a 1)
        (b 2)
        (c 3))
    (+ a b c)
    (- a b c)
    (* a b c)
    (/ a b c)
    (format nil "done")))              ; ← zc on line 105 to fold this defun

;;; ================================================================
;;; TEST 10: Formatting — press <leader>cf
;;; ================================================================
;;; The function below is intentionally mis-indented. Format to fix.

(defun messy-function (x)
(let ((y (* x 2)))
(if (> y 10)
(format t "big: ~a~%" y)
(format t "small: ~a~%" y))))

;;; ================================================================
;;; TEST 11: Rename — press <leader>cr with cursor on helper
;;; ================================================================
;;; Cursor on helper below, rename to "square":

(defun rename-test ()
  (helper 5)                           ; ← <leader>cr on helper, type "square"
  (helper 10))

;;; ================================================================
;;; TEST 12: Code Actions — press <leader>ca with cursor on symbol
;;; ================================================================

(defun code-action-test ()
  (format t "hello~%")                 ; ← <leader>ca on format → "Add package prefix"
  (+ 1 2))

;;; ================================================================
;;; TEST 13: Diagnostics — uncomment the broken line, then save
;;; ================================================================
;;; Uncomment the line below and save (:w) to see an error diagnostic:
;; (defun broken-function (x) (+ x y))

;;; ================================================================
;;; TEST 14: Inlay Hints — :lua vim.lsp.inlay_hint.enable(true)
;;; ================================================================
;;; After enabling, you should see parameter names at call sites:

(defun test-inlay ()
  (add-numbers 10 20)                  ; ← should show a: b: before args
  (format t "~a~%" (greet "World")))   ; ← should show name: before "World"

;;; ================================================================
;;; TEST 15: Call Hierarchy — :lua vim.lsp.buf.incoming_calls()
;;; ================================================================
;;; Put cursor on greet below, then run the command:

(defun call-hierarchy-test ()
  (greet "test"))                      ; ← cursor on greet, then run command

;;; ================================================================
;;; TEST 16: Code Lens — should appear automatically
;;; ================================================================
;;; Look above each defun for "N references" annotations.

;;; ================================================================
;;; TEST 17: Selection Range — needs custom keymap
;;; ================================================================
;;; Not bound by default in LazyVim. If you bind it, cursor inside
;;; a nested sexp and expand outward by s-expression.
