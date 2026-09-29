(in-package :cl-user)
(defpackage woo-test
  (:use :cl
        :rove))
(in-package :woo-test)

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

