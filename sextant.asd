(defsystem "sextant"
  :version "0.1.0"
  :author "Glenn Thompson"
  :license "MIT"
  :description "A Common Lisp Language Server Protocol (LSP) implementation"
  :depends-on ("alexandria"
               "bordeaux-threads"
               "cl-ppcre"
               "babel"
               "swank")
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "json")
                 (:file "transport")
                 (:file "document")
                 (:file "source-index")
                 (:file "lisp-introspection")
                 (:file "diagnostics")
                 (:file "handlers")
                 (:file "debugger")
                 (:file "dap-handlers")
                 (:file "dap-server")
                 (:file "server")
                 (:file "main"))))
  :in-order-to ((test-op (test-op "sextant/tests"))))

(defsystem "sextant/tests"
  :depends-on ("sextant" "fiveam")
  :serial t
  :components ((:module "tests"
                :components
                ((:file "diagnostics-test")
                 (:file "introspection-test"))))
  :perform (test-op (op c)
             (unless (uiop:symbol-call :sextant/tests '#:run-tests)
               (error "sextant/tests: fiveam suite reported failures"))))
