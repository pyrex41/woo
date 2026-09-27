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

;;; The HTTP/1 reader splits heads and fixed-length body boundaries, but
;;; never scans inside a body for apparent CR LF CR LFs. Each body stays in
;;; one write per socket read; splitting within a large body would reopen
;;; smart-buffer's temporary file repeatedly.

(defun bare-socket ()
  (woo.ev.socket::%make-socket
   :fd 0
   :last-activity 0.0d0
   :open-p t
   :watchers (make-array 3 :initial-element (cffi:null-pointer))))

(deftest http1-body-is-not-split
  (testing "a 1.2 MB body of CR LF CR LFs reaches the body buffer once per read"
    (let* ((n (* 1200 1024))
           (body (let ((v (make-array n :element-type '(unsigned-byte 8))))
                   (dotimes (i n v) (setf (aref v i) (if (evenp i) 13 10)))))
           (request (concatenate '(simple-array (unsigned-byte 8) (*))
                                 (trivial-utf-8:string-to-utf-8-bytes
                                  (format nil "POST /p HTTP/1.1~C~CHost: x~C~CContent-Length: ~D~C~C~C~C"
                                          #\Return #\Newline #\Return #\Newline n
                                          #\Return #\Newline #\Return #\Newline))
                                 body))
           (read-size 65536)
           (reads (ceiling (length request) read-size))
           (writes 0)
           (received nil)
           (socket (bare-socket))
           (write-fn (symbol-function 'smart-buffer:write-to-buffer))
           (elapsed nil))
      (unwind-protect
           (let ((woo.specials:*app*
                   (lambda (env)
                     (let* ((in (getf env :raw-body))
                            (buf (make-array n :element-type '(unsigned-byte 8))))
                       (setf received (and in (subseq buf 0 (read-sequence buf in)))))
                     ;; A delayed response that never comes: nothing to write.
                     (lambda (responder) (declare (ignore responder)))))
                 (woo.specials:*debug* t)
                 (start (get-internal-real-time)))
             (setf (symbol-function 'smart-buffer:write-to-buffer)
                   (lambda (&rest args)
                     (incf writes)
                     (apply write-fn args)))
             (woo::setup-parser socket)
             (loop for s from 0 below (length request) by read-size
                   do (woo::read-cb socket (subseq request s (min (length request)
                                                                  (+ s read-size)))))
             (setf elapsed (/ (- (get-internal-real-time) start)
                              internal-time-units-per-second)))
        (setf (symbol-function 'smart-buffer:write-to-buffer) write-fn))
      (ok (equalp received body) "the app gets the whole body")
      (ok (<= writes (1+ reads))
          (format nil "~D body writes for ~D reads" writes reads))
      (ok (< elapsed 10) (format nil "parsed in ~,2Fs" elapsed)))))

(deftest http1-upgrade-after-fixed-body
  (testing "an upgrade behind a POST body keeps the early WebSocket octets"
    (let* ((seen nil)
           (socket (bare-socket))
           (request (trivial-utf-8:string-to-utf-8-bytes
                     (format nil "POST /first HTTP/1.1~C~CHost: x~C~CContent-Length: 4~C~C~C~CabcdGET /ws HTTP/1.1~C~CHost: x~C~CConnection: Upgrade~C~CUpgrade: websocket~C~C~C~Cxyz"
                             #\Return #\Newline #\Return #\Newline #\Return #\Newline
                             #\Return #\Newline #\Return #\Newline #\Return #\Newline
                             #\Return #\Newline #\Return #\Newline #\Return #\Newline))))
      (let ((woo.specials:*app*
              (lambda (env)
                (push (getf env :path-info) seen)
                (lambda (responder) (declare (ignore responder)))))
            (woo.specials:*debug* t))
        (woo::setup-parser socket)
        (woo::read-cb socket request)
        (ok (equal (reverse seen) '("/first" "/ws")))
        (ok (equalp (woo.websocket:take-pending-websocket-data socket)
                    (trivial-utf-8:string-to-utf-8-bytes "xyz")))))))

(deftest http1-pipelined-heads-are-one-piece
  (testing "16 pipelined GETs in one read go to fast-http in one call"
    (let* ((head (trivial-utf-8:string-to-utf-8-bytes
                  ;; Upgrade-Insecure-Requests is not an Upgrade field.
                  (format nil "GET /a HTTP/1.1~C~CHost: x~C~CUpgrade-Insecure-Requests: 1~C~C~C~C"
                          #\Return #\Newline #\Return #\Newline #\Return #\Newline
                          #\Return #\Newline)))
           (read (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
                        (make-list 16 :initial-element head)))
           (calls 0)
           (requests 0)
           (socket (bare-socket))
           (parse-fn (symbol-function 'fast-http:parse-request)))
      (unwind-protect
           (let ((woo.specials:*app* (lambda (env)
                                       (declare (ignore env))
                                       (incf requests)
                                       (lambda (responder) (declare (ignore responder)))))
                 (woo.specials:*debug* t))
             ;; MAKE-PARSER takes the function when SETUP-PARSER runs.
             (setf (symbol-function 'fast-http:parse-request)
                   (lambda (&rest args)
                     (incf calls)
                     (apply parse-fn args)))
             (woo::setup-parser socket)
             (woo::read-cb socket read))
        (setf (symbol-function 'fast-http:parse-request) parse-fn))
      (ok (= requests 16) "every request is parsed")
      (ok (= calls 1) (format nil "~D fast-http calls" calls)))))

(deftest graceful-stop-by-server-thread
  "A host can stop Woo's event loop without destroying its thread."
  (let ((port (+ 51000 (random 1000)))
        (thread nil)
        (client nil))
    (unwind-protect
         (progn
           (setf thread
                 (bt2:make-thread
                  (lambda ()
                    (woo:run (lambda (env)
                               (declare (ignore env))
                               '(200 () ("ok")))
                             :port port :debug nil))
                  :name "woo-graceful-stop-test"))
           (loop repeat 500
                 until (gethash thread woo::*stop-controls*)
                 do (sleep 0.01))
           (ok (gethash thread woo::*stop-controls*)
               "the running server is addressable by its thread")
           (setf client (usocket:socket-connect "127.0.0.1" port))
           ;; Keep an accepted socket open while the loop is stopped.
           (sleep 0.05)
           (ok (woo:stop-gracefully thread)
               "the thread stop request is accepted")
           (loop repeat 500 while (bt2:thread-alive-p thread)
                 do (sleep 0.01))
           (ok (not (bt2:thread-alive-p thread))
               "the server thread exits within five seconds")
           (unless (bt2:thread-alive-p thread)
             (bt2:join-thread thread))
           (ok (null (sb-ext:with-timeout 5
                       (read-char (usocket:socket-stream client) nil nil)))
               "the accepted socket closes on stop")
           (ok (null (gethash thread woo::*stop-controls*))
               "the thread control is removed after cleanup"))
      (when client
        (ignore-errors (usocket:socket-close client)))
      (when (and thread (bt2:thread-alive-p thread))
        (bt2:destroy-thread thread)))))

(deftest graceful-stop-closes-worker-sockets
  "A clustered shutdown closes every accepted client, including idle requests."
  (let ((port (+ 52000 (random 1000)))
        (thread nil)
        (clients nil))
    (unwind-protect
         (progn
           (setf thread
                 (bt2:make-thread
                  (lambda ()
                    (woo:run (lambda (env)
                               (declare (ignore env))
                               '(200 () ("ok")))
                             :port port :worker-num 2 :debug nil))
                  :name "woo-worker-stop-test"))
           (loop repeat 500
                 until (gethash thread woo::*stop-controls*)
                 do (sleep 0.01))
           (ok (gethash thread woo::*stop-controls*)
               "clustered server registers a graceful stop control")
           (loop repeat 12 do
             (let ((client (usocket:socket-connect "127.0.0.1" port)))
               (push client clients)
               (write-string (format nil "GET /pending HTTP/1.1~C~CHost: localhost~C~C"
                                     #\Return #\Linefeed #\Return #\Linefeed)
                             (usocket:socket-stream client))
               (force-output (usocket:socket-stream client))))
           (sleep 0.1)
           (ok (woo:stop-gracefully thread) "clustered stop request is accepted")
           (loop repeat 1200 while (bt2:thread-alive-p thread)
                 do (sleep 0.01))
           (ok (not (bt2:thread-alive-p thread))
               "clustered server exits within twelve seconds")
           (unless (bt2:thread-alive-p thread)
             (bt2:join-thread thread))
           (dolist (client clients)
             (ok (null (sb-ext:with-timeout 5
                         (read-char (usocket:socket-stream client) nil nil)))
                 "an accepted worker socket closes on stop")))
      (dolist (client clients)
        (ignore-errors (usocket:socket-close client)))
      (when (and thread (bt2:thread-alive-p thread))
        (bt2:destroy-thread thread)))))

#+sbcl
(deftest event-loop-backend-descriptors-close
  "libev's loop destructor releases its kernel backend descriptors."
  (let ((before (length (directory #P"/dev/fd/*"))))
    (dotimes (i 12)
      (declare (ignore i))
      (woo.ev:with-event-loop ()))
    (dotimes (i 12)
      (declare (ignore i))
      (handler-case
          (woo.ev:with-event-loop (:cleanup-fn (lambda () (error "cleanup failed"))))
        (error () nil)))
    (let ((after (length (directory #P"/dev/fd/*"))))
      (ok (<= after (1+ before))
          (format nil "descriptors before=~D after=~D" before after)))))
