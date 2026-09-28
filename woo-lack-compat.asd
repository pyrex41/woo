(defsystem "woo-lack-compat"
  :description "Managed Clack lifecycle and Lack compatibility profile for Woo"
  :license "MIT"
  :depends-on ("woo" "clack" "lack" "lack-request" "lack-response"
               "lack-middleware-session" "lack-middleware-mount"
               "lack-middleware-auth-basic" "lack-middleware-csrf"
               "lack-middleware-static" "salza2" "zstd" "trivial-gray-streams")
  :serial t
  :components ((:file "src/compat/package")
               (:file "src/compat/runtime")
               (:file "src/compat/http")
               (:file "src/compat/middleware"))
  :in-order-to ((test-op (test-op "woo-lack-compat/tests"))))

(defsystem "woo-lack-compat/tests"
  :depends-on ("woo-lack-compat" "rove" "dexador" "hunchentoot" "clack-handler-hunchentoot" "chipz"
               "websocket-driver-server" "lack-session-store-dbi"
               "lack-session-store-redis" "lack-middleware-dbpool" "dbd-sqlite3")
  :serial t
  :components ((:file "t/compat/contract") (:file "t/compat/lifecycle")
               (:file "t/compat/middleware") (:file "t/compat/services"))
  :perform (test-op (op c) (unless (uiop:symbol-call :rove :run c)
                            (error "Lack compatibility gate failed"))))
