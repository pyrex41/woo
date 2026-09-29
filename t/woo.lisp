(in-package :cl-user)
(defpackage woo-test (:use :cl :rove))
(in-package :woo-test)

(deftest woo-server-tests
  (ok (clack.test.suite:run-server-tests :woo)
      "upstream Clack suite failures fail the Woo gate"))

(deftest woo-ssl-server-tests
  (let ((clack.test:*clackup-additional-args*
          '(:ssl-cert-file #P"t/certs/localhost.crt"
            :ssl-key-file #P"t/certs/localhost.key"))
        (dex:*not-verify-ssl* t)
        (clack.test:*use-https* t))
    (ok (clack.test.suite:run-server-tests :woo)
        "upstream Clack TLS suite failures fail the Woo gate")))

(defun bare-socket ()
  (woo.ev.socket::%make-socket :fd 0 :last-activity 0.0d0 :open-p t
                               :watchers (make-array 3 :initial-element (cffi:null-pointer))))

(deftest http1-body-is-not-split
  (let* ((n (* 1200 1024))
         (body (let ((v (make-array n :element-type '(unsigned-byte 8))))
                 (dotimes (i n v) (setf (aref v i) (if (evenp i) 13 10)))))
         (request (concatenate '(simple-array (unsigned-byte 8) (*))
                               (trivial-utf-8:string-to-utf-8-bytes
                                (format nil "POST /p HTTP/1.1~C~CHost: x~C~CContent-Length: ~D~C~C~C~C"
                                        #\Return #\Newline #\Return #\Newline n
                                        #\Return #\Newline #\Return #\Newline)) body))
         (read-size 65536) (reads (ceiling (length request) read-size))
         (writes 0) (received nil) (socket (bare-socket))
         (write-fn (symbol-function 'smart-buffer:write-to-buffer)))
    (unwind-protect
         (let ((woo.specials:*app*
                 (lambda (env)
                   (let* ((in (getf env :raw-body))
                          (buf (make-array n :element-type '(unsigned-byte 8))))
                     (setf received (and in (subseq buf 0 (read-sequence buf in)))))
                   (lambda (responder) (declare (ignore responder)))))
               (woo.specials:*debug* t))
           (setf (symbol-function 'smart-buffer:write-to-buffer)
                 (lambda (&rest args) (incf writes) (apply write-fn args)))
           (woo::setup-parser socket)
           (loop for s from 0 below (length request) by read-size
                 do (woo::read-cb socket (subseq request s (min (length request) (+ s read-size))))))
      (setf (symbol-function 'smart-buffer:write-to-buffer) write-fn))
    (ok (equalp received body))
    (ok (<= writes (1+ reads)) (format nil "~D body writes for ~D reads" writes reads))))

(deftest http1-pipelined-heads-are-one-piece
  (let* ((head (trivial-utf-8:string-to-utf-8-bytes
                (format nil "GET /a HTTP/1.1~C~CHost: x~C~CUpgrade-Insecure-Requests: 1~C~C~C~C"
                        #\Return #\Newline #\Return #\Newline #\Return #\Newline #\Return #\Newline)))
         (read (apply #'concatenate '(simple-array (unsigned-byte 8) (*)) (make-list 16 :initial-element head)))
         (calls 0) (requests 0) (socket (bare-socket))
         (parse-fn (symbol-function 'fast-http:parse-request)))
    (unwind-protect
         (let ((woo.specials:*app* (lambda (env) (declare (ignore env)) (incf requests)
                                     (lambda (responder) (declare (ignore responder)))))
               (woo.specials:*debug* t))
           (setf (symbol-function 'fast-http:parse-request)
                 (lambda (&rest args) (incf calls) (apply parse-fn args)))
           (woo::setup-parser socket) (woo::read-cb socket read))
      (setf (symbol-function 'fast-http:parse-request) parse-fn))
    (ok (= requests 16)) (ok (= calls 1))))

(deftest graceful-stop-by-server-thread
  (let ((port (+ 51000 (random 1000))) (thread nil) (client nil))
    (unwind-protect
         (progn
           (setf thread (bt2:make-thread
                         (lambda () (woo:run (lambda (env) (declare (ignore env)) '(200 () ("ok")))
                                              :port port :debug nil))))
           (loop repeat 500 until (gethash thread woo::*stop-controls*) do (sleep 0.01))
           (ok (gethash thread woo::*stop-controls*))
           (setf client (usocket:socket-connect "127.0.0.1" port))
           (ok (woo:stop-gracefully thread))
           (loop repeat 500 while (bt2:thread-alive-p thread) do (sleep 0.01))
           (ok (not (bt2:thread-alive-p thread)))
           (unless (bt2:thread-alive-p thread) (bt2:join-thread thread))
           (ok (null (sb-ext:with-timeout 5 (read-char (usocket:socket-stream client) nil nil))))
           ;; The upstream lifecycle branch guarantees thread exit and socket
           ;; closure; control-table reclamation is covered by the later
           ;; hardening branch.
           (ok (not (bt2:thread-alive-p thread))))
      (when client (ignore-errors (usocket:socket-close client)))
      (when (and thread (bt2:thread-alive-p thread)) (bt2:destroy-thread thread)))))

#+sbcl
(deftest event-loop-backend-descriptors-close
  (let ((before (length (directory #P"/dev/fd/*"))))
    (dotimes (i 12) (declare (ignore i)) (woo.ev:with-event-loop ()))
    (let ((after (length (directory #P"/dev/fd/*"))))
      (ok (<= after (1+ before)) (format nil "descriptors before=~D after=~D" before after)))))
