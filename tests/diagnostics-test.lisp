(defpackage :sextant/tests
  (:use :cl :fiveam)
  (:import-from :sextant
                #:compile-buffer-for-diagnostics
                #:captured-condition-position
                #:captured-condition-source-form
                #:make-diagnostic-range
                #:json-get
                #:find-symbol-in-packages)
  (:export #:run-tests))

(in-package :sextant/tests)

(def-suite :sextant-tests)
(in-suite :sextant-tests)

(defparameter *type-mismatch-fixture*
  (format nil "(defun bad-add (x)~%  (+ x \"not-a-number\"))~%")
  "Line 1 (0-indexed) has a type mismatch: adding a string to a number.
Reproduces TODO.md bug #1 on unpatched master: position collapses to NIL,
make-diagnostic-range then defaults to line 0, col 0.")

(test diagnostics-position-mid-file
  (let* ((conditions (compile-buffer-for-diagnostics *type-mismatch-fixture* "file:///fixture.lisp"))
         (cc (first conditions))
         (pos (and cc (captured-condition-position cc))))
    (is (not (null cc)))
    (is (not (null pos)))
    (is (eql 1 (car pos)))
    (let ((range (make-diagnostic-range pos *type-mismatch-fixture*
                                         (captured-condition-source-form cc))))
      (is (eql 1 (json-get (json-get range "start") "line"))))))

(defun run-tests ()
  (run! :sextant-tests))
