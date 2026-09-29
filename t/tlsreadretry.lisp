(in-package :cl-user)
(defpackage woo-test.tlsreadretry
  (:use :cl
        :rove))
(in-package :woo-test.tlsreadretry)

#+sbcl
(deftest tls-read-want-write-keeps-write-readiness
  (testing "SSL_read WANT_WRITE stops read polling and preserves write retry"
    (multiple-value-bind (read-fd write-fd) (sb-posix:pipe)
      (let ((socket nil)
            (read-calls 0))
        (unwind-protect
             (progn
               (woo.ev.event-loop:with-event-loop ()
                 (setf socket (woo.ev.socket:make-socket
                               :fd read-fd :tcp-read-cb 'woo.ev.tcp::tcp-read-cb)
                       (woo.ev.socket::socket-ssl-handle socket)
                       (cffi:null-pointer))
                 (setf (woo.ev.event-loop:deref-data-from-pointer read-fd) socket)
                 (lev:ev-io-start woo.ev:*evloop*
                                  (woo.ev.socket:socket-read-watcher socket))
                 (let ((woo.ev.tcp::*ssl-read-function*
                         (lambda (handle pointer length)
                           (declare (ignore handle pointer length))
                           (incf read-calls)
                           -1))
                       (woo.ev.tcp::*ssl-error-function*
                         (lambda (handle result)
                           (declare (ignore handle result))
                           (if (<= read-calls 2)
                               cl+ssl::+ssl-error-want-write+
                               cl+ssl::+ssl-error-want-read+))))
                   (funcall (symbol-function 'woo.ev.tcp::tcp-read-cb)
                            woo.ev:*evloop*
                            (woo.ev.socket:socket-read-watcher socket)
                            lev:+EV-READ+)
                   (ok (= read-calls 1))
                   (ok (woo.ev.socket::socket-read-wait-write-p socket))
                   (ok (zerop (lev:ev-is-active
                               (woo.ev.socket:socket-read-watcher socket))))
                   (ok (plusp (lev:ev-is-active
                               (woo.ev.socket:socket-write-watcher socket))))
                   ;; The write callback retries the read. If it still needs
                   ;; write readiness, async-write must not stop that watcher.
                   (ok (null (woo.ev.socket:async-write socket)))
                   (ok (woo.ev.socket::socket-read-wait-write-p socket))
                   (ok (plusp (lev:ev-is-active
                               (woo.ev.socket:socket-write-watcher socket))))
                   (ok (woo.ev.socket:async-write socket))
                   (ok (= read-calls 3))
                   (ok (not (woo.ev.socket::socket-read-wait-write-p socket)))
                   (ok (plusp (lev:ev-is-active (woo.ev.socket:socket-read-watcher socket))))
                   (ok (zerop (lev:ev-is-active (woo.ev.socket:socket-write-watcher socket))))
                   (setf (woo.ev.socket::socket-ssl-handle socket) nil)
                   (woo.ev.socket:close-socket socket)
                   (setf socket nil))
                 (lev:ev-break woo.ev:*evloop* lev:+EVBREAK-ALL+)))
          (when socket
            (setf (woo.ev.socket::socket-ssl-handle socket) nil)
            (ignore-errors (woo.ev.socket:close-socket socket)))
          (ignore-errors (sb-posix:close write-fd)))))))

#+sbcl
(deftest rejected-input-stops-full-read-buffer-loop
  (testing "a rejection does not consume a second SSL_read from one event"
    (multiple-value-bind (read-fd write-fd) (sb-posix:pipe)
      (let ((socket nil)
            (read-calls 0))
        (unwind-protect
             (progn
               (woo.ev.event-loop:with-event-loop ()
                 (setf socket (woo.ev.socket:make-socket
                               :fd read-fd :tcp-read-cb 'woo.ev.tcp::tcp-read-cb)
                       (woo.ev.socket::socket-ssl-handle socket)
                       (cffi:null-pointer)
                       (woo.ev.socket:socket-read-cb socket)
                       (lambda (socket data &key start end)
                         (declare (ignore data start end))
                         (woo.ev.socket:stop-reading-for-close socket)))
                 (setf (woo.ev.event-loop:deref-data-from-pointer read-fd) socket)
                 (let ((woo.ev.tcp::*ssl-read-function*
                         (lambda (handle pointer length)
                           (declare (ignore handle pointer))
                           (incf read-calls)
                           (when (> read-calls 1)
                             (error "rejected input attempted a second SSL_read"))
                           length)))
                   (funcall (symbol-function 'woo.ev.tcp::tcp-read-cb)
                            woo.ev:*evloop*
                            (woo.ev.socket:socket-read-watcher socket)
                            lev:+EV-READ+)
                   (ok (= read-calls 1))
                   (ok (woo.ev.socket::socket-input-rejected-p socket)))
                 (setf (woo.ev.socket::socket-ssl-handle socket) nil)
                 (woo.ev.socket:close-socket socket)
                 (setf socket nil))
               ))
          (when socket
            (setf (woo.ev.socket::socket-ssl-handle socket) nil)
            (ignore-errors (woo.ev.socket:close-socket socket)))
          (ignore-errors (sb-posix:close write-fd))))))

#+sbcl
(deftest rejected-input-services-pending-write-readiness
  (testing "a rejected socket still retries SSL_write that needs read readiness"
    (multiple-value-bind (read-fd write-fd) (sb-posix:pipe)
      (let ((socket nil)
            (read-calls 0)
            (write-calls 0))
        (unwind-protect
             (progn
               (woo.ev.event-loop:with-event-loop ()
                 (setf socket (woo.ev.socket:make-socket
                               :fd write-fd :tcp-read-cb 'woo.ev.tcp::tcp-read-cb)
                       (woo.ev.socket::socket-ssl-handle socket)
                       (cffi:null-pointer))
                 (setf (woo.ev.event-loop:deref-data-from-pointer write-fd) socket)
                 (let ((woo.ev.socket::*ssl-write-function*
                         (lambda (handle pointer length)
                           (declare (ignore handle pointer))
                           (incf write-calls)
                           (if (= write-calls 1) -1 length)))
                       (woo.ev.socket::*ssl-error-function*
                         (lambda (handle result)
                           (declare (ignore handle result))
                           cl+ssl::+ssl-error-want-read+))
                       (woo.ev.tcp::*ssl-read-function*
                         (lambda (handle pointer length)
                           (declare (ignore handle pointer length))
                           (incf read-calls)
                           (error "rejected input must not call SSL_read"))))
                   (woo.ev.socket:write-socket-data
                    socket (make-array 4 :element-type '(unsigned-byte 8)
                                       :initial-contents '(1 2 3 4)))
                   (ok (null (woo.ev.socket:flush-buffer socket)))
                   (woo.ev.socket:stop-reading-for-close socket)
                   (ok (woo.ev.socket::socket-write-wait-read-p socket))
                   (funcall (symbol-function 'woo.ev.tcp::tcp-read-cb)
                            woo.ev:*evloop*
                            (woo.ev.socket:socket-read-watcher socket)
                            lev:+EV-READ+)
                   (ok (= write-calls 2))
                   (ok (= read-calls 0))
                   (ok (null (woo.ev.socket::socket-pending-write-data socket))))
                 (setf (woo.ev.socket::socket-ssl-handle socket) nil)
                 (woo.ev.socket:close-socket socket)
                 (setf socket nil))
               ))
          (when socket
            (setf (woo.ev.socket::socket-ssl-handle socket) nil)
            (ignore-errors (woo.ev.socket:close-socket socket)))
          (ignore-errors (sb-posix:close read-fd))))))
