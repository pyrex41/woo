(defsystem "woo-test"
  :depends-on ("woo"
               "clack-test"
               "rove")
  :components
  ((:file "t/file-size")
   (:file "t/woo")
   (:file "t/ipv6"))
  :perform (test-op (op c) (symbol-call '#:rove '#:run c)))
