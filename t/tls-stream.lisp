(in-package :cl-user)
(defpackage woo-test.tls-stream
  (:use :cl :rove))
(in-package :woo-test.tls-stream)

#+sbcl
(defun check-tls-error-close (app status)
  "Require server EOF on a keepalive request, including a shutdown retry."
  (let ((calls 0)
        (shutdown woo.ev.socket::*ssl-shutdown-function*)
        (ssl-error woo.ev.socket::*ssl-error-function*)
        (clack.test:*clack-test-handler* :woo)
        (clack.test:*enable-debug* nil)
        (clack.test:*use-https* t)
        (clack.test:*clackup-additional-args*
          (list :ssl-cert-file #P"t/certs/localhost.crt"
                :ssl-key-file #P"t/certs/localhost.key")))
    (unwind-protect
         (progn
           ;; The server runs in another thread; lexical special bindings here
           ;; would not affect its FFI indirections.
           (setf woo.ev.socket::*ssl-shutdown-function*
                 (lambda (handle)
                   (if (= (incf calls) 1) -1 (funcall shutdown handle)))
                 woo.ev.socket::*ssl-error-function*
                 (lambda (handle result)
                   (if (and (= calls 1) (= result -1))
                       cl+ssl::+ssl-error-want-write+
                       (funcall ssl-error handle result))))
           (clack.test:testing-app "completed TLS error closes gracefully" app
             (let ((sock nil) (tls nil))
               (unwind-protect
                    (sb-ext:with-timeout 5
                      (setf sock (usocket:socket-connect
                                  "127.0.0.1" clack.test:*clack-test-access-port*
                                  :element-type '(unsigned-byte 8))
                            tls (cl+ssl:make-ssl-client-stream
                                 (usocket:socket-stream sock)
                                 :hostname "localhost" :verify nil))
                      (write-sequence
                       (trivial-utf-8:string-to-utf-8-bytes
                        (woo-test::crlf-lines "GET / HTTP/1.1" "Host: localhost"
                                              "Connection: keep-alive" "")) tls)
                      (force-output tls)
                      (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                                               :adjustable t :fill-pointer 0)))
                        ;; An abortive TLS close raises a condition; a retained
                        ;; keepalive connection times out. Neither counts as EOF.
                        (loop for byte = (read-byte tls nil nil)
                              while byte
                              do (when (>= (length bytes) 4096)
                                   (error "TLS error response exceeds test budget"))
                                 (vector-push-extend byte bytes))
                        (let ((response (trivial-utf-8:utf-8-bytes-to-string bytes)))
                          (ok (search (format nil "HTTP/1.1 ~D " status) response))
                          (ok (search "Content-Length: 0" response))))
                      (ok (>= calls 2) "server retried SSL_shutdown after WANT_WRITE"))
                 (when tls (ignore-errors (close tls :abort t)))
                 (when sock (ignore-errors (usocket:socket-close sock)))))))
      (setf woo.ev.socket::*ssl-shutdown-function* shutdown
            woo.ev.socket::*ssl-error-function* ssl-error))))

#+sbcl
(deftest tls-completed-error-paths-retry-shutdown
  (let ((missing (merge-pathnames
                  (format nil "woo-missing-~A" (gensym))
                  (uiop:temporary-directory))))
    (when (probe-file missing) (error "Missing-path fixture already exists"))
    (check-tls-error-close (lambda (env) (declare (ignore env))
                            (list 200 nil missing)) 404))
  ;; A returned producer fails after Clack middleware has returned. This
  ;; reaches Woo's legacy-response-failed rather than a middleware 500.
  (check-tls-error-close
   (lambda (env) (declare (ignore env))
     (lambda (responder) (declare (ignore responder))
       (error "producer failed before headers"))) 500))

(deftest tls-static-stream-pumps-bounded-chunks
  (testing "TLS pathname sends keep at most one 64 KiB chunk pending"
    (let* ((path (merge-pathnames
                  (format nil "woo-tls-stream-~D.bin" (random 1000000))
                  #p"/tmp/"))
           (size (* 3 65536))
           (watchers (make-array 3 :initial-element (cffi:null-pointer)))
           (socket nil))
      (unwind-protect
           (progn
             (with-open-file (out path :direction :output
                                       :element-type '(unsigned-byte 8)
                                       :if-exists :supersede)
               (dotimes (i size)
                 (write-byte (mod i 251) out)))
             (setf socket
                   (woo.ev.socket::%make-socket
                    :fd -1 :last-activity 0.0d0 :open-p t
                    :watchers watchers))
             (with-open-file (in path :element-type '(unsigned-byte 8))
               (woo.ev.socket:start-static-stream socket in size)
               (woo.ev.socket::pump-static-stream socket)
               (ok (= (woo.ev.socket::socket-send-stream-offset socket) 65536))
               (ok (<= (fast-io::output-buffer-len
                        (woo.ev.socket::socket-buffer socket))
                       65536))))
        (when (and socket (woo.ev.socket::socket-send-stream socket))
          (ignore-errors (close (woo.ev.socket::socket-send-stream socket)
                                :abort t)))
        (when (probe-file path)
          (delete-file path))))))

(deftest tls-pending-write-isolated-from-new-output
  (testing "application writes do not mutate bytes awaiting TLS retry"
    (let* ((socket (woo.ev.socket::%make-socket
                    :fd -1 :last-activity 0.0d0 :open-p t
                    :watchers (make-array 3 :initial-element (cffi:null-pointer))))
           (pending (make-array 4 :element-type '(unsigned-byte 8)
                                :initial-contents '(1 2 3 4))))
      (setf (woo.ev.socket::socket-pending-write-data socket) pending
            (woo.ev.socket::socket-pending-write-offset socket) 1)
      (woo.ev.socket:write-socket-data
       socket (make-array 2 :element-type '(unsigned-byte 8)
                          :initial-contents '(9 8)))
      (ok (equalp pending #(1 2 3 4)))
      (ok (= (woo.ev.socket::socket-pending-write-offset socket) 1))
      (ok (= (fast-io::output-buffer-len
              (woo.ev.socket::socket-buffer socket)) 2)))))

(deftest tls-ffi-retry-hooks-are-injectable
  (testing "WANT results can be supplied without changing the OpenSSL ABI"
    (let ((calls 0))
      (let ((woo.ev.socket::*ssl-write-function*
              (lambda (handle pointer length)
                (declare (ignore handle pointer length))
                (incf calls)
                -1))
            (woo.ev.socket::*ssl-error-function*
              (lambda (handle result)
                (declare (ignore handle result))
                cl+ssl::+ssl-error-want-read+)))
        (ok (= (funcall woo.ev.socket::*ssl-write-function* nil nil 8) -1))
        (ok (= (funcall woo.ev.socket::*ssl-error-function* nil -1)
               cl+ssl::+ssl-error-want-read+))
        (ok (= calls 1))))))
