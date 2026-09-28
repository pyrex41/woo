(in-package :cl-user)
(defpackage woo-test.tlsretry
  (:use :cl
        :rove))
(in-package :woo-test.tlsretry)

#+sbcl
(deftest tls-retry-keeps-pending-bytes-before-new-output
  (testing "a WANT_READ retry preserves order and queue accounting"
    (multiple-value-bind (read-fd write-fd) (sb-posix:pipe)
      (let ((socket nil)
            (calls 0)
            (writes nil)
            (admitted 0)
            (released 0))
        (unwind-protect
             (progn
               (woo.ev.event-loop:with-event-loop ()
                 (setf socket (woo.ev.socket:make-socket
                               :fd write-fd :tcp-read-cb 'woo.ev.tcp::tcp-read-cb)
                       (woo.ev.socket::socket-ssl-handle socket)
                       (cffi:make-pointer 1)
                       (woo.ev.socket::socket-output-admitter socket)
                       (lambda (n) (incf admitted n) t)
                       (woo.ev.socket::socket-output-releaser socket)
                       (lambda (n) (incf released n)))
                 (let ((woo.ev.socket::*ssl-write-function*
                         (lambda (handle pointer length)
                           (declare (ignore handle))
                           (incf calls)
                           (push (loop for i below length
                                       collect (cffi:mem-aref pointer :unsigned-char i))
                                 writes)
                           (if (= calls 1) -1 length)))
                       (woo.ev.socket::*ssl-error-function*
                         (lambda (handle result)
                           (declare (ignore handle result))
                           cl+ssl::+ssl-error-want-read+)))
                   (woo.ev.socket:write-socket-data
                    socket (make-array 4 :element-type '(unsigned-byte 8)
                                       :initial-contents '(1 2 3 4)))
                   (ok (null (woo.ev.socket:flush-buffer socket)))
                   (woo.ev.socket:write-socket-data
                    socket (make-array 2 :element-type '(unsigned-byte 8)
                                       :initial-contents '(9 8)))
                   (ok (woo.ev.socket:flush-buffer socket))
                   (ok (woo.ev.socket::async-write socket))
                   (ok (= calls 3))
                   (ok (equal (mapcan #'copy-list (nreverse writes))
                              '(1 2 3 4 1 2 3 4 9 8)))
                   (ok (= admitted 6))
                   (ok (= released 6))
                   (ok (zerop (fast-io::output-buffer-len
                               (woo.ev.socket::socket-buffer socket))))
                   (setf (woo.ev.socket::socket-ssl-handle socket) nil)
                   (woo.ev.socket:close-socket socket)
                   (setf socket nil))
                 (lev:ev-break woo.ev:*evloop* lev:+EVBREAK-ALL+)))
          (when socket
            (setf (woo.ev.socket::socket-ssl-handle socket) nil)
            (ignore-errors (woo.ev.socket:close-socket socket)))
          (ignore-errors (sb-posix:close read-fd)))))))
