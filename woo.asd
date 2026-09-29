(defsystem "woo"
  :version "0.12.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("lev"
               "clack-socket"
               "swap-bytes"
               "cffi"
               "static-vectors"
               "bordeaux-threads"
               "fast-http"
               "quri"
               "fast-io"
               "smart-buffer"
               "trivial-utf-8"
               "trivial-mimes"
               "vom"
               "alexandria"
               (:feature :sbcl "sb-posix")
               (:feature (:and :linux (:not :asdf3)) "uiop")
               (:feature :sbcl "sb-concurrency")
               (:feature (:not :sbcl) "cl-speedy-queue")
               (:feature (:not :woo-no-ssl) "cl+ssl"))
  :components ((:module "src"
                :components
                ((:file "woo" :depends-on ("ev" "response" "worker" "ssl" "signal" "specials" "util" "http2" "dispatch"))
                 (:file "response" :depends-on ("ev"))
                 (:file "ev" :depends-on ("ev-packages"))
                 (:file "worker" :depends-on ("ev" "queue" "specials"))
                 (:file "queue")
                 (:module "ev-packages"
                  :pathname "ev"
                  :depends-on ("syscall" "llsocket")
                  :components
                  ((:file "event-loop")
                   (:file "socket" :depends-on ("event-loop" "condition" "util"))
                   (:file "tcp" :depends-on ("event-loop" "socket" "util" "condition"))
                   (:file "condition")
                   (:file "util")))
                 (:file "ssl-alpn" :depends-on ("ev-packages")
                  :if-feature (:not :woo-no-ssl))
                 (:file "ssl"
                  :depends-on ("ev-packages" "ssl-alpn")
                  :if-feature (:not :woo-no-ssl))
                 (:file "dispatch" :depends-on ("ev-packages"))
                 (:module "http2" :pathname "http2"
                  :depends-on ("ev-packages" "dispatch")
                  :components ((:file "constants")
                               (:file "hpack" :depends-on ("constants"))
                               (:file "frames" :depends-on ("constants"))
                               (:file "stream" :depends-on ("constants"))
                               (:file "connection" :depends-on ("constants" "frames" "hpack" "stream"))
                               (:file "clack" :depends-on ("constants" "frames" "hpack" "stream" "connection"))))
                 (:module "llsocket"
                  :depends-on ("syscall")
                  :serial t
                  :components
                  ((:file "package")
                   (:cffi-grovel-file "grovel")
                   (:file "cffi")))
                 (:module "syscall"
                  :serial t
                  :components
                  ((:file "package")
                   (:cffi-grovel-file "types")
                   (:file "main")))
                 (:file "signal" :depends-on ("ev" "worker" "specials"))
                 (:file "specials")
                 (:file "util"))))
  :description "An asynchronous HTTP server written in Common Lisp"
  :in-order-to ((test-op (test-op "woo-test"))))
