(in-package :sextant/tests)

(in-suite :sextant-tests)

(test find-symbol-resolves-internal-subpackage-symbol
  "Reproduces TODO.md bug #4 on unpatched master: find-symbol-in-packages only
matched :external symbols once it fell through to the all-packages search, so
a function defined in a project-local package without being exported (the
common case for subpackage code) was invisible to hover/completion, unlike
the same function defined in COMMON-LISP-USER."
  (let* ((pkg (or (find-package "SEXTANT-TESTS-FIXTURE-PKG")
                   (make-package "SEXTANT-TESTS-FIXTURE-PKG" :use nil)))
         (sym (intern "SEXTANT-TESTS-FIXTURE-FN" pkg)))
    (unwind-protect
         (progn
           (setf (symbol-function sym) (lambda () 42))
           (is (eq :internal (nth-value 1 (find-symbol "SEXTANT-TESTS-FIXTURE-FN" pkg))))
           (multiple-value-bind (found-sym found-pkg)
               (find-symbol-in-packages "sextant-tests-fixture-fn")
             (is (eq sym found-sym))
             (is (eq pkg found-pkg))))
      (delete-package pkg))))
