(in-package :cl-user)
(defpackage woo-test
  (:use :cl
        :rove))
(in-package :woo-test)

(deftest woo-server-tests
  (clack.test.suite:run-server-tests :woo))

(deftest http2-preface-detection
  (testing "complete PRI preface is HTTP/2"
    (let ((preface woo.http2.constants:+connection-preface+))
      (ok (eq (woo:looks-like-http2-preface preface 0 (length preface)) :http2)
          "24-byte connection preface")
      (ok (woo:http2-connection-preface-match preface 0 24))))
  (testing "partial matching prefix needs more data"
    (let ((partial (subseq woo.http2.constants:+connection-preface+ 0 8)))
      (ok (eq (woo:looks-like-http2-preface partial 0 8) :need-more))))
  (testing "HTTP/1 request is not a preface"
    (let ((get (trivial-utf-8:string-to-utf-8-bytes "GET / HTTP/1.1")))
      (ok (eq (woo:looks-like-http2-preface get 0 (length get)) :http1))))
  (testing "empty buffer waits"
    (let ((empty (make-array 0 :element-type '(unsigned-byte 8))))
      (ok (eq (woo:looks-like-http2-preface empty 0 0) :need-more)))))

;;; Protocol detection buffers the first bytes in an adjustable vector and
;;; replays them to the HTTP/1 parser, which needs a simple octet vector.
;;; Exercise the replay over a real socket: a request complete in the first
;;; packet, and one whose first packet ("P") is a preface prefix, so it is
;;; buffered and appended to before detection finishes.

(defun detection-app (env)
  (let* ((len (getf env :content-length))
         (body (if (and len (plusp len))
                   (let ((buf (make-array len :element-type '(unsigned-byte 8))))
                     (read-sequence buf (getf env :raw-body))
                     (trivial-utf-8:utf-8-bytes-to-string buf))
                   "")))
    (list 200 '(:content-type "text/plain")
          (list (format nil "~A ~A [~A]"
                        (getf env :request-method) (getf env :path-info) body)))))

(defun raw-exchange (port &rest packets)
  "Send each string in PACKETS as its own write, pausing between them, then
   read the response until EOF (bounded to 5s). Returns it as a string."
  (let ((sock (handler-case (usocket:socket-connect "127.0.0.1" port
                                                   :element-type '(unsigned-byte 8))
                (error () (return-from raw-exchange "")))))
    (unwind-protect
         (let ((stream (usocket:socket-stream sock))
               (out (make-array 0 :element-type '(unsigned-byte 8)
                                  :adjustable t :fill-pointer 0)))
           (loop for (packet . more) on packets
                 do (write-sequence (trivial-utf-8:string-to-utf-8-bytes packet) stream)
                    (force-output stream)
                    (when more (sleep 0.2)))
           (handler-case
               (sb-ext:with-timeout 5
                 (loop for byte = (read-byte stream nil nil)
                       while byte
                       do (vector-push-extend byte out)))
             (sb-ext:timeout () nil)
             (error () nil))
           (map 'string #'code-char out))
      (usocket:socket-close sock))))

(defun crlf-lines (&rest lines)
  (format nil "~{~A~C~C~}"
          (loop for l in lines append (list l #\Return #\Newline))))

(defmacro with-server-thread-errors ((errors) &body body)
  "Collect conditions that reach the debugger in any thread into ERRORS and
   abort that thread, instead of letting --disable-debugger quit the image."
  (let ((old (gensym)))
    `(let ((,errors '())
           (,old sb-ext:*invoke-debugger-hook*))
       (unwind-protect
            (progn
              (setf sb-ext:*invoke-debugger-hook*
                    (lambda (c hook)
                      (declare (ignore hook))
                      (push c ,errors)
                      (sb-thread:abort-thread)))
              ,@body)
         (setf sb-ext:*invoke-debugger-hook* ,old)))))

(deftest http1-replay-after-protocol-detection
  (let ((clack.test:*clack-test-handler* :woo)
        (clack.test:*enable-debug* nil)
        (post (crlf-lines "POST /echo HTTP/1.1" "Host: localhost"
                          "Content-Type: text/plain" "Content-Length: 5"
                          "Connection: close" "")))
    (with-server-thread-errors (errors)
      (clack.test:testing-app "HTTP/1 requests survive the detection replay"
          #'detection-app
        (let ((port clack.test:*clack-test-port*))
          (let ((res (raw-exchange port (concatenate 'string post "hello"))))
            (ok (search "HTTP/1.1 200" res) "headers and body in the first packet")
            (ok (search "POST /echo [hello]" res)))
          (let ((res (raw-exchange port "P" (concatenate 'string (subseq post 1) "world"))))
            (ok (search "HTTP/1.1 200" res) "first packet is a preface prefix")
            (ok (search "POST /echo [world]" res)))
          (let ((res (raw-exchange port "GET"
                                   (crlf-lines " /split HTTP/1.1" "Host: localhost"
                                               "Connection: close" ""))))
            (ok (search "HTTP/1.1 200" res) "request line split after the method")
            (ok (search "GET /split []" res)))
          (ok (null errors)
              (format nil "no server thread errors: ~{~A~^; ~}" errors)))))))

(deftest woo-ssl-server-tests
  (let ((clack.test:*clackup-additional-args*
          '(:ssl-cert-file #P"t/certs/localhost.crt"
            :ssl-key-file #P"t/certs/localhost.key"))
        (dex:*not-verify-ssl* t)
        (clack.test:*use-https* t))
    (clack.test.suite:run-server-tests :woo)))
