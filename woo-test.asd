(defsystem "woo-test"
  :depends-on ("woo"
               "clack-test"
               "rove")
  :components
  ((:file "t/woo")
   (:file "t/ipv6")
   (:file "t/body-limit")
   (:file "t/tls-stream")
   (:file "t/tlsretry")
   (:file "t/tlsreadretry"))
  :perform (test-op (op c) (symbol-call '#:rove '#:run c)))
