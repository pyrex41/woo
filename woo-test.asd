(defsystem "woo-test"
  :depends-on ("woo"
               "clack-test"
               "rove")
  :components
  ((:file "t/woo")
   (:file "t/ipv6")
   ;; HTTP/2 tests
   (:file "t/hpack")
   (:file "t/http2-frames")
   (:file "t/http2-stream")
   (:file "t/http2-connection")
   (:file "t/http2-clack")
   ;; WebSocket tests
   (:file "t/websocket")
   (:file "showcase-limits" :pathname "showcase/src/limits")
   (:file "t/showcase" :depends-on ("showcase-limits"))
   ;; Property-based protocol tests (in-repo generator/shrinker)
   (:module "t-prop"
    :pathname "t/prop"
    :components
    ((:file "core")
     (:file "quickcheck" :depends-on ("core"))
     (:file "properties" :depends-on ("core"))
     (:file "qc" :depends-on ("quickcheck"))))
   ;; Differential oracle against Go's HTTP/2 stack.
   (:file "t/diff/diff")
   ;; Coverage-guided fuzz and mutation testing of the pure codecs.
   (:file "t/fuzz/guided")
   (:file "t/mutate/mutate")
   ;; SSL/ALPN tests
   (:file "t/alpn" :if-feature (:not :woo-no-ssl)))
  :perform (test-op (op c) (symbol-call '#:rove '#:run c)))
