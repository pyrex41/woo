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
                       (cffi:null-pointer)
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
                   (ok (woo.ev.socket:async-write socket))
                   (ok (= calls 3))
                   (ok (equal (nreverse writes)
                              '((1 2 3 4) (1 2 3 4) (9 8))))
                   (ok (= admitted 6))
                   (ok (= released 6))
                   (ok (null (woo.ev.socket::socket-pending-write-data socket)))
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

#+sbcl
(deftest tls-retry-partials-want-transitions-and-reentrant-release
  (testing "partial progress and both readiness retries preserve accounting"
    (multiple-value-bind (read-fd write-fd) (sb-posix:pipe)
      (let ((socket nil)
            (calls 0)
            (writes nil)
            (errors '(:want-write :want-read))
            (admitted 0)
            (released 0)
            (release-enqueued nil))
        (unwind-protect
             (progn
               (woo.ev.event-loop:with-event-loop ()
                 (setf socket (woo.ev.socket:make-socket
                               :fd write-fd :tcp-read-cb 'woo.ev.tcp::tcp-read-cb)
                       (woo.ev.socket::socket-ssl-handle socket)
                       (cffi:null-pointer)
                       (woo.ev.socket::socket-output-admitter socket)
                       (lambda (n) (incf admitted n) t)
                       (woo.ev.socket::socket-output-releaser socket)
                       (lambda (n)
                         (incf released n)
                         (unless release-enqueued
                           (setf release-enqueued t)
                           ;; Exercise output arriving while the pending
                           ;; write's charge is being released.
                           (woo.ev.socket:write-socket-data
                            socket (make-array 1 :element-type '(unsigned-byte 8)
                                               :initial-contents '(7))))))
                 (let ((woo.ev.socket::*ssl-write-function*
                         (lambda (handle pointer length)
                           (declare (ignore handle))
                           (incf calls)
                           (push (loop for i below length
                                       collect (cffi:mem-aref pointer :unsigned-char i))
                                 writes)
                           (case calls
                             (1 2)       ; partial progress
                             (2 -1)      ; WANT_WRITE, retry same tail
                             (3 2)       ; partial progress again
                             (4 -1)      ; WANT_READ, retry same tail
                             (5 1)       ; progress must rearm WRITE before completion
                             (otherwise length))))
                       (woo.ev.socket::*ssl-error-function*
                         (lambda (handle result)
                           (declare (ignore handle result))
                           (ecase (pop errors)
                             (:want-write cl+ssl::+ssl-error-want-write+)
                             (:want-read cl+ssl::+ssl-error-want-read+)))))
                   (woo.ev.socket:write-socket-data
                    socket (make-array 6 :element-type '(unsigned-byte 8)
                                       :initial-contents '(1 2 3 4 5 6)))
                   (ok (null (woo.ev.socket:flush-buffer socket)))
                   (ok (= (woo.ev.socket::socket-pending-write-offset socket) 2))
                   (ok (null (woo.ev.socket:flush-buffer socket)))
                   (woo.ev.socket:write-socket-data
                    socket (make-array 2 :element-type '(unsigned-byte 8)
                                       :initial-contents '(9 8)))
                   (ok (null (woo.ev.socket:flush-buffer socket)))
                   (ok (= (woo.ev.socket::socket-pending-write-offset socket) 4))
                   (ok (null (woo.ev.socket:flush-buffer socket)))
                   (ok (null (woo.ev.socket:flush-buffer socket)))
                   (ok (not (woo.ev.socket::socket-write-wait-read-p socket)))
                   (ok (plusp (lev:ev-is-active (woo.ev.socket:socket-write-watcher socket))))
                   (ok (woo.ev.socket:flush-buffer socket))
                   (ok (woo.ev.socket:async-write socket))
                   (ok (= calls 7))
                   (ok (equal (nreverse writes)
                              '((1 2 3 4 5 6)
                                (3 4 5 6)
                                (3 4 5 6)
                                (5 6)
                                (5 6)
                                (6)
                                (9 8 7))))
                   (ok (= admitted 9))
                   (ok (= released 9))
                   (ok (null (woo.ev.socket::socket-pending-write-data socket)))
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

#+sbcl
(deftest tls-fatal-write-releases-once-and-closes
  (testing "a fatal TLS write closes safely and does not reuse its buffer"
    (multiple-value-bind (read-fd write-fd) (sb-posix:pipe)
      (let ((socket nil)
            (released 0))
        (unwind-protect
             (progn
               (woo.ev.event-loop:with-event-loop ()
                 (setf socket (woo.ev.socket:make-socket
                               :fd write-fd :tcp-read-cb 'woo.ev.tcp::tcp-read-cb)
                       (woo.ev.socket::socket-ssl-handle socket)
                       (cffi:null-pointer)
                       (woo.ev.socket::socket-output-admitter socket)
                       (lambda (n) (declare (ignore n)) t)
                       (woo.ev.socket::socket-output-releaser socket)
                       (lambda (n) (incf released n)))
                 (let ((woo.ev.socket::*ssl-write-function*
                         (lambda (handle pointer length)
                           (declare (ignore handle pointer length))
                           -1))
                       (woo.ev.socket::*ssl-error-function*
                         (lambda (handle result)
                           (declare (ignore handle result))
                           cl+ssl::+ssl-error-ssl+)))
                   (woo.ev.socket:write-socket-data
                    socket (make-array 4 :element-type '(unsigned-byte 8)
                                       :initial-contents '(1 2 3 4)))
                   (ok (woo.ev.socket:flush-buffer socket))
                   (ok (not (woo.ev.socket:socket-open-p socket)))
                   (ok (= released 4))
                   (ok (woo.ev.socket:async-write socket))
                   (ok (= released 4))
                   (ok (null (woo.ev.socket::socket-pending-write-data socket))))
                 (setf socket nil)
                 (lev:ev-break woo.ev:*evloop* lev:+EVBREAK-ALL+)))
          (when socket
            (setf (woo.ev.socket::socket-ssl-handle socket) nil)
            (ignore-errors (woo.ev.socket:close-socket socket)))
          (ignore-errors (sb-posix:close read-fd)))))))
