(defsystem "woo-test"
  :depends-on ("woo"
               "clack-test"
               "rove")
  :components
  ((:file "t/test-support")
   (:file "t/event-loop")
   (:file "t/woo")
   (:file "t/ipv6")
   (:file "t/file-size")
   (:file "t/worker")
   (:file "t/response"))
  :perform (test-op (op c) (unless (symbol-call '#:rove '#:run c)
               (error "Woo test gate failed"))))
