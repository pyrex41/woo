(in-package :cl-user)
(defpackage woo-test.tls-stream
  (:use :cl :rove))
(in-package :woo-test.tls-stream)

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

