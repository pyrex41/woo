(defsystem "woo-lack-compat"
  :description "Managed Clack lifecycle and Lack compatibility profile for Woo"
  :license "MIT"
  :depends-on ("woo" "clack" "lack" "lack-request" "lack-response"
               "trivial-gray-streams")
  :serial t
  :components ((:file "src/compat/package")
               (:file "src/compat/runtime")
               (:file "src/compat/http"))
  :in-order-to ((test-op (test-op "woo-lack-compat/tests"))))

(defsystem "woo-lack-compat/tests"
  :depends-on ("woo-lack-compat" "rove" "dexador" "hunchentoot" "clack-handler-hunchentoot")
  :serial t
  :components ((:file "t/compat/contract") (:file "t/compat/lifecycle"))
  :perform (test-op (op c) (unless (uiop:symbol-call :rove :run c)
                            (error "Managed Clack gate failed"))))
