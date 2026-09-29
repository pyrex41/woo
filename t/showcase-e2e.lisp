(in-package :cl-user)
(defpackage woo-test.showcase-e2e
  (:use :cl :rove)
  (:import-from :trivial-utf-8 :string-to-utf-8-bytes)
  (:import-from :woo-test.showcase :showcase-loaded-p)
  (:import-from :woo-test.websocket-e2e
                :with-server :ws-open :expected-head :*sample-accept*
                :available-octets :conn-close :send-octets :client-frame
                :octets :read-frame :close-body :close-code :read-until-eof
                :octets-string :node-available-p :check-node-results
                :run-node-client :make-conn :crlf))
(in-package :woo-test.showcase-e2e)

;;; The showcase app: /ws/echo and /api/benchmarks over a real server.

(defun showcase-handler ()
  ;; Drop the access log middleware's output from the test log.
  (let ((handler (symbol-value (find-symbol "*HANDLER*" :woo-showcase))))
    (lambda (env)
      (let ((*standard-output* (make-broadcast-stream)))
        (funcall handler env)))))

(defmacro with-showcase ((port) &body body)
  `(if (not (showcase-loaded-p))
       (skip "woo-showcase is not loadable (missing dependency); run (ql:quickload :woo-showcase) once. Showcase e2e tests skipped")
       (with-server (,port (showcase-handler))
         ,@body)))

(defun http-get (port path)
  "Open a connection and send GET PATH with Connection: close."
  (let* ((socket (usocket:socket-connect "127.0.0.1" port :element-type '(unsigned-byte 8)))
         (conn (make-conn :socket socket :stream (usocket:socket-stream socket))))
    (send-octets conn (string-to-utf-8-bytes
                       (crlf (format nil "GET ~A HTTP/1.1" path)
                             "Host: 127.0.0.1"
                             "Connection: close"
                             "")))
    conn))

(deftest e2e-showcase-websocket
  (with-showcase (port)
    (testing "/ws/echo upgrades with nothing after the 101"
      (multiple-value-bind (conn head) (ws-open port "/ws/echo")
        (unwind-protect
             (progn
               (ok (equal head (expected-head *sample-accept*)))
               (ok (zerop (length (available-octets conn 0.3)))
                   "ningle's 200 is not written into the WebSocket stream"))
          (conn-close conn))))
    (testing "binary is echoed as binary, even when it is not UTF-8"
      (let ((conn (ws-open port "/ws/echo")))
        (unwind-protect
             (progn
               (send-octets conn (client-frame 2 (octets #xFF #xFE #x00 #x80)))
               (multiple-value-bind (opcode payload) (read-frame conn)
                 (ok (eql opcode 2))
                 (ok (equalp payload (octets #xFF #xFE #x00 #x80))))
               (send-octets conn (client-frame 1 (string-to-utf-8-bytes (format nil "caf~C" (code-char #xE9)))))
               (multiple-value-bind (opcode payload) (read-frame conn)
                 (ok (eql opcode 1) "text stays text")
                 (ok (equalp payload (string-to-utf-8-bytes (format nil "caf~C" (code-char #xE9)))))))
          (conn-close conn))))
    (testing "the server answers the client's close"
      (let ((conn (ws-open port "/ws/echo")))
        (unwind-protect
             (progn
               (send-octets conn (client-frame 8 (close-body 1001 "leaving")))
               (multiple-value-bind (opcode payload) (read-frame conn)
                 (ok (eql opcode 8) "a close frame comes back")
                 (ok (eql (and payload (close-code payload)) 1001)))
               (ok (equalp (read-until-eof conn 3) #()) "and the socket is closed"))
          (conn-close conn))))))

(deftest e2e-showcase-slow-resource-does-not-block
  (with-showcase (port)
    (testing "a 1 s /api/resource request does not stall a concurrent request"
      (let* ((start (get-internal-real-time))
             (slow (http-get port "/api/resource/7?delay=1000")))
        (unwind-protect
             (progn
               (sleep 0.1)                ; let the server start the slow one
               (let* ((fast (http-get port "/api/benchmarks"))
                      (fast-res (unwind-protect (read-until-eof fast 5)
                                  (conn-close fast)))
                      (fast-elapsed (/ (- (get-internal-real-time) start)
                                       internal-time-units-per-second)))
                 (ok (and (vectorp fast-res)
                          (search "HTTP/1.1 200" (octets-string fast-res)))
                     "the fast request succeeds")
                 (ok (< fast-elapsed 0.7)
                     (format nil "the fast request finished after ~,2Fs, before the slow one"
                             fast-elapsed)))
               (let* ((slow-res (read-until-eof slow 5))
                      (slow-elapsed (/ (- (get-internal-real-time) start)
                                       internal-time-units-per-second))
                      (text (if (vectorp slow-res) (octets-string slow-res) "")))
                 (ok (search "HTTP/1.1 200" text) "the slow request succeeds")
                 (ok (search "\"id\":\"7\"" text))
                 (ok (search "\"delay\":1000" text))
                 (ok (>= slow-elapsed 0.95)
                     (format nil "the slow request still waited (~,2Fs)" slow-elapsed))))
          (conn-close slow))))))

(deftest e2e-showcase-node-client
  (cond
    ((not (node-available-p))
     (skip "node with a global WebSocket is not on PATH; showcase Node e2e tests skipped"))
    (t
     (with-showcase (port)
       (check-node-results (run-node-client port "showcase")
                           '("benchmarks-json" "showcase-handshake" "showcase-text-echo"
                             "showcase-binary-echo" "showcase-client-close"))))))
