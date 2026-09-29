(defsystem "woo-test"
  :depends-on ("woo"
               "clack-test"
               "lack-request"
               "rove")
  :components
  ((:file "t/woo")
   (:file "t/body-limit")
   (:file "t/ipv6")
   (:file "t/file-size")
   (:file "t/worker")
   (:file "t/response" :depends-on ("t/woo"))
   ;; HTTP/2 tests
   (:file "t/hpack")
   (:file "t/http2-frames")
   (:file "t/http2-stream")
   (:file "t/http2-connection")
   (:file "t/http2-clack")
   (:file "t/http2-e2e")
   ;; WebSocket tests
   (:file "t/websocket")
   (:file "t/websocket-e2e")
   ;; Property-based protocol tests (in-repo generator/shrinker)
   (:module "t-prop"
    :pathname "t/prop"
    :components
    ((:file "core")
     (:file "quickcheck" :depends-on ("core"))
     (:file "properties" :depends-on ("core"))
     (:file "qc" :depends-on ("quickcheck"))))
   ;; Differential oracle against Go's HTTP/2 stack.
   (:file "t/diff/diff" :depends-on ("t-prop"))
   ;; Coverage-guided fuzz and mutation testing of the pure codecs.
   (:file "t/fuzz/guided" :depends-on ("t-prop"))
   (:file "t/mutate/mutate" :depends-on ("t-prop"))
   ;; SSL/ALPN tests
   (:file "t/alpn" :if-feature (:not :woo-no-ssl))
   (:file "t/tls-stream" :if-feature (:not :woo-no-ssl))
   (:file "t/tlsretry" :if-feature (:not :woo-no-ssl))
   (:file "t/tlsreadretry" :if-feature (:not :woo-no-ssl)))
  :perform (test-op (op c)
             (unless (symbol-call '#:rove '#:run c)
               (error "Woo test gate failed"))))

;; Showcase code is an explicit optional gate, separate from protocol tests.
(defsystem "woo-test/showcase"
  :depends-on ("woo-test")
  :components
  ((:file "showcase-limits" :pathname "showcase/src/limits")
   (:file "t/showcase" :depends-on ("showcase-limits"))
   (:file "t/showcase-e2e" :depends-on ("t/showcase")))
  :perform (test-op (op c)
             (unless (symbol-call '#:rove '#:run c)
               (error "Woo showcase test gate failed"))))
