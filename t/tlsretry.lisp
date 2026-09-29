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

#+sbcl
(deftest tls-shutdown-want-write-survives-flush-completion
  (multiple-value-bind (read-fd write-fd) (sb-posix:pipe)
    (let ((socket nil) (shutdown-calls 0))
      (unwind-protect
           (woo.ev.event-loop:with-event-loop ()
             (setf socket (woo.ev.socket:make-socket
                           :fd write-fd :tcp-read-cb 'woo.ev.tcp::tcp-read-cb))
             (setf (woo.ev.socket::socket-ssl-handle socket) (cffi:null-pointer)
                   (woo.ev.socket::socket-write-cb socket)
                   (lambda (s) (woo.ev.socket:graceful-close-socket s)))
             (let ((woo.ev.socket::*ssl-shutdown-function*
                     (lambda (handle)
                       (declare (ignore handle))
                       (incf shutdown-calls)
                       (if (= shutdown-calls 1) -1 1)))
                   (woo.ev.socket::*ssl-error-function*
                     (lambda (handle result)
                       (declare (ignore handle result))
                       cl+ssl::+ssl-error-want-write+)))
               ;; Exercise the normal response completion callback, then
               ;; service the readiness requested by SSL_shutdown itself.
               (woo.ev.socket:async-write socket)
               (ok (= shutdown-calls 1))
               (ok (woo.ev.socket::socket-tls-shutdown-p socket))
               (ok (plusp (lev:ev-is-active (woo.ev.socket:socket-write-watcher socket))))
               (ok (zerop (lev:ev-is-active (woo.ev.socket:socket-read-watcher socket))))
               (woo.ev.socket:async-write socket)
               (ok (= shutdown-calls 2))
               (ok (not (woo.ev.socket:socket-open-p socket))))
             (setf socket nil)
             (lev:ev-break woo.ev:*evloop* lev:+EVBREAK-ALL+))
        (when socket
          (setf (woo.ev.socket::socket-ssl-handle socket) nil)
          (ignore-errors (woo.ev.socket:close-socket socket)))
          (ignore-errors (sb-posix:close read-fd))))))

#+sbcl
(deftest tls-shutdown-receive-phase-drains-before-second-shutdown
  (testing "peer application records are drained before the second SSL_shutdown"
    (multiple-value-bind (read-fd write-fd) (sb-posix:pipe)
      (let ((socket nil)
            (shutdown-calls 0)
            (reads '(1 -1 -1 -1 0))
            (errors '(:want-read :want-write :want-read :zero)))
        (unwind-protect
             (woo.ev.event-loop:with-event-loop ()
               (setf socket (woo.ev.socket:make-socket
                             :fd write-fd :tcp-read-cb 'woo.ev.tcp::tcp-read-cb))
               (setf (woo.ev.event-loop:deref-data-from-pointer write-fd) socket
                     (woo.ev.socket::socket-ssl-handle socket) (cffi:null-pointer)
                     (woo.ev.socket::socket-tls-shutdown-p socket) t
                     (woo.ev.socket::socket-tls-shutdown-recv-p socket) t
                     (woo.ev.socket::socket-tls-shutdown-deadline socket)
                     (+ (lev:ev-now woo.ev:*evloop*) 10.0d0))
               (let ((woo.ev.socket::*ssl-read-function*
                       (lambda (handle pointer length)
                         (declare (ignore handle pointer length))
                         (pop reads)))
                     (woo.ev.socket::*ssl-error-function*
                       (lambda (handle result)
                         (declare (ignore handle))
                         (ecase result
                           (-1 (ecase (pop errors)
                                 (:want-read cl+ssl::+ssl-error-want-read+)
                                 (:want-write cl+ssl::+ssl-error-want-write+)
                                 (:zero cl+ssl::+ssl-error-zero-return+)))
                           (0 cl+ssl::+ssl-error-zero-return+))))
                     (woo.ev.socket::*ssl-shutdown-function*
                       (lambda (handle)
                         (declare (ignore handle))
                         (incf shutdown-calls)
                         1)))
                 (funcall (symbol-function 'woo.ev.tcp::tcp-read-cb)
                          woo.ev:*evloop* (woo.ev.socket:socket-read-watcher socket)
                          lev:+EV-READ+)
                 (ok (zerop shutdown-calls))
                 (ok (woo.ev.socket::socket-tls-shutdown-recv-p socket))
                 (woo.ev.socket:tls-shutdown-read-step socket)
                 (ok (zerop shutdown-calls))
                 (ok (plusp (lev:ev-is-active
                             (woo.ev.socket:socket-write-watcher socket))))
                 (ok (zerop (lev:ev-is-active
                             (woo.ev.socket:socket-read-watcher socket))))
                 (funcall (symbol-function 'woo.ev.socket::async-write-cb)
                          woo.ev:*evloop* (woo.ev.socket:socket-write-watcher socket)
                          lev:+EV-WRITE+)
                 (ok (zerop shutdown-calls))
                 (ok (zerop (lev:ev-is-active
                             (woo.ev.socket:socket-write-watcher socket))))
                 (ok (plusp (lev:ev-is-active
                             (woo.ev.socket:socket-read-watcher socket))))
                 (funcall (symbol-function 'woo.ev.socket::tls-shutdown-cb)
                          woo.ev:*evloop* (woo.ev.socket::socket-shutdown-timer socket) 0)
                 (ok (= shutdown-calls 1))
                 (ok (not (woo.ev.socket:socket-open-p socket))))
               (setf socket nil)
               (lev:ev-break woo.ev:*evloop* lev:+EVBREAK-ALL+))
          (when socket
            (setf (woo.ev.socket::socket-ssl-handle socket) nil)
            (ignore-errors (woo.ev.socket:close-socket socket)))
          (ignore-errors (sb-posix:close read-fd)))))))

#+sbcl
(defmacro with-fake-shutdown-socket ((socket) &body body)
  `(multiple-value-bind (read-fd write-fd) (sb-posix:pipe)
     (let ((,socket nil))
       (unwind-protect
            (woo.ev.event-loop:with-event-loop ()
              (setf ,socket (woo.ev.socket:make-socket
                              :fd read-fd :tcp-read-cb 'woo.ev.tcp::tcp-read-cb)
                    (woo.ev.event-loop:deref-data-from-pointer read-fd) ,socket
                    (woo.ev.socket::socket-ssl-handle ,socket) (cffi:null-pointer))
              ,@body
              (when (woo.ev.socket:socket-open-p ,socket)
                (setf (woo.ev.socket::socket-ssl-handle ,socket) nil)
                (woo.ev.socket:close-socket ,socket :abort t))
              (lev:ev-break woo.ev:*evloop* lev:+EVBREAK-ALL+))
         (when (and ,socket (woo.ev.socket:socket-open-p ,socket))
           (setf (woo.ev.socket::socket-ssl-handle ,socket) nil)
           (ignore-errors (woo.ev.socket:close-socket ,socket :abort t)))
         (ignore-errors (sb-posix:close write-fd))))))

#+sbcl
(deftest tls-shutdown-initial-read-retry-enters-receive-phase
  (dolist (initial-result '(0 -1))
    (with-fake-shutdown-socket (socket)
      (let ((shutdown-calls 0) (error-calls 0) (read-calls 0) (parser-calls 0))
        (setf (woo.ev.socket::socket-read-cb socket)
              (lambda (&rest args) (declare (ignore args)) (incf parser-calls)))
        (let ((woo.ev.socket::*ssl-shutdown-function*
                (lambda (handle)
                  (declare (ignore handle))
                  (if (= (incf shutdown-calls) 1) initial-result 1)))
              (woo.ev.socket::*ssl-error-function*
                (lambda (handle result)
                  (declare (ignore handle result))
                  (incf error-calls)
                  (if (zerop read-calls)
                      cl+ssl::+ssl-error-want-read+
                      cl+ssl::+ssl-error-zero-return+)))
              (woo.ev.socket::*ssl-read-function*
                (lambda (handle pointer length)
                  (declare (ignore handle pointer length))
                  (incf read-calls) 0)))
          (woo.ev.socket:graceful-close-socket socket)
          (ok (= shutdown-calls 1))
          (ok (= error-calls 1) "classify the shutdown result once")
          (ok (woo.ev.socket::socket-tls-shutdown-recv-p socket))
          (ok (plusp (lev:ev-is-active (woo.ev.socket:socket-read-watcher socket))))
          (ok (zerop (lev:ev-is-active (woo.ev.socket:socket-write-watcher socket))))
          ;; A direct output retry must service the receive phase, too.
          (woo.ev.socket:async-write socket)
          (ok (= read-calls 1))
          (ok (= shutdown-calls 2))
          (ok (zerop parser-calls))
          (ok (not (woo.ev.socket:socket-open-p socket))))))))

#+sbcl
(deftest tls-shutdown-buffered-input-continuation-retains-deadline
  (with-fake-shutdown-socket (socket)
    (let ((shutdown-calls 0) (read-calls 0))
      (let ((woo.ev.socket::*ssl-shutdown-function*
              (lambda (handle) (declare (ignore handle)) (incf shutdown-calls) 0))
            (woo.ev.socket::*ssl-error-function*
              (lambda (handle result)
                (declare (ignore handle result))
                (cond ((= shutdown-calls 1) cl+ssl::+ssl-error-want-write+)
                      ((zerop read-calls) cl+ssl::+ssl-error-syscall+)
                      (t cl+ssl::+ssl-error-want-read+))))
            (woo.ev.socket::*ssl-read-function*
              (lambda (handle pointer length)
                (declare (ignore handle pointer length))
                (if (<= (incf read-calls) 5) 1 -1))))
        (woo.ev.socket:graceful-close-socket socket)
        (ok (not (woo.ev.socket::socket-tls-shutdown-recv-p socket)))
        (ok (zerop (lev:ev-is-active (woo.ev.socket:socket-read-watcher socket))))
        (ok (plusp (lev:ev-is-active (woo.ev.socket:socket-write-watcher socket))))
        (funcall (symbol-function 'woo.ev.socket::async-write-cb)
                 woo.ev:*evloop* (woo.ev.socket:socket-write-watcher socket) lev:+EV-WRITE+)
        (ok (= shutdown-calls 2))
        (ok (woo.ev.socket::socket-tls-shutdown-recv-p socket))
        (let ((deadline (woo.ev.socket::socket-tls-shutdown-deadline socket)))
          (funcall (symbol-function 'woo.ev.tcp::tcp-read-cb)
                   woo.ev:*evloop* (woo.ev.socket:socket-read-watcher socket) lev:+EV-READ+)
          (ok (= read-calls 4) "one callback has a bounded read budget")
          ;; The pipe has no input: only the scheduled timer can continue.
          (lev:ev-run woo.ev:*evloop* lev:+EVRUN-ONCE+)
          (ok (= read-calls 6) "drain OpenSSL buffers without kernel readiness")
          (ok (= shutdown-calls 2) "WANT_READ does not retry shutdown")
          (ok (plusp (lev:ev-is-active (woo.ev.socket::socket-shutdown-timer socket)))
              "a continuation that blocks restores the deadline timer")
          (ok (= deadline (woo.ev.socket::socket-tls-shutdown-deadline socket)))
          (ok (zerop (lev:ev-is-active (woo.ev.socket:socket-write-watcher socket))))
          (setf (woo.ev.socket::socket-tls-shutdown-deadline socket)
                (- (lev:ev-now woo.ev:*evloop*) 1.0d0))
          (funcall (symbol-function 'woo.ev.socket::tls-shutdown-cb)
                   woo.ev:*evloop* (woo.ev.socket::socket-shutdown-timer socket) 0)
          (ok (= read-calls 6) "expiry aborts before another SSL operation")
          (ok (= shutdown-calls 2))
          (ok (not (woo.ev.socket:socket-open-p socket))))))))

#+sbcl
(deftest tls-shutdown-fatal-receive-and-send-errors-close
  (dolist (read-result '(0 -1))
    (with-fake-shutdown-socket (socket)
      (let ((shutdown-calls 0) (reading-p nil))
        (let ((woo.ev.socket::*ssl-shutdown-function*
                (lambda (handle) (declare (ignore handle)) (incf shutdown-calls) 0))
              (woo.ev.socket::*ssl-read-function*
                (lambda (handle pointer length)
                  (declare (ignore handle pointer length))
                  (setf reading-p t) read-result))
              (woo.ev.socket::*ssl-error-function*
                (lambda (handle result)
                  (declare (ignore handle result))
                  (if reading-p cl+ssl::+ssl-error-ssl+ cl+ssl::+ssl-error-want-read+))))
          (woo.ev.socket:graceful-close-socket socket)
          (funcall (symbol-function 'woo.ev.tcp::tcp-read-cb)
                   woo.ev:*evloop* (woo.ev.socket:socket-read-watcher socket) lev:+EV-READ+)
          (ok (= shutdown-calls 1))
          (ok (not (woo.ev.socket:socket-open-p socket)))))))
  (with-fake-shutdown-socket (socket)
    (let ((error-calls 0)
          (woo.ev.socket::*ssl-shutdown-function*
            (lambda (handle) (declare (ignore handle)) -1)))
      (let ((woo.ev.socket::*ssl-error-function*
              (lambda (handle result)
                (declare (ignore handle result))
                (incf error-calls) cl+ssl::+ssl-error-ssl+)))
        (woo.ev.socket:graceful-close-socket socket)
        (ok (= error-calls 1))
        (ok (not (woo.ev.socket:socket-open-p socket)))))))
