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
                 (:file "lisp-introspection")
                 (:file "handlers")
                 (:file "server")
                 (:file "main")))))
