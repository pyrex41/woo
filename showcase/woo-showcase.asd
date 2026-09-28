(defsystem "woo-showcase"
  :version "0.1.0"
  :author "Woo Team"
  :license "MIT"
  :depends-on ("woo"
               "ningle"
               "lack"
               "lack-middleware-static"
               "djula"
               "jonathan"
               "trivial-utf-8"
               "cl-ppcre"
               ;; Delayed responses on woo's event loop
               "lev"
               "cffi"
               "bordeaux-threads")
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "limits")
                 (:file "benchmark-data" :depends-on ("package"))
                 (:file "app" :depends-on ("package" "benchmark-data" "limits")))))
  :description "Web app showcasing Woo's performance")
