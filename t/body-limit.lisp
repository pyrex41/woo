(in-package :cl-user)

(defpackage woo-test.body-limit
  (:use :cl :rove)
  (:import-from :clack.test
                :testing-app
                :*clack-test-access-port*)
  (:import-from :clack.test.suite
                :localhost)
  (:import-from :trivial-utf-8
                :string-to-utf-8-bytes
                :utf-8-bytes-to-string))

(in-package :woo-test.body-limit)

(defparameter *memory-limit* 16384)
(defparameter *disk-limit* 65536)
(defvar *body-file* nil)

(defmacro with-body-limits (&body body)
  `(let* ((memory-limit smart-buffer:*default-memory-limit*)
          (disk-limit smart-buffer:*default-disk-limit*)
          (temporary-directory
            (uiop:ensure-directory-pathname
             (merge-pathnames
              (format nil "woo-body-limit-~36R/" (random (expt 36 8)))
              (uiop:temporary-directory)))))
     (ensure-directories-exist temporary-directory)
     (let ((smart-buffer::*temporary-directory* temporary-directory))
       (setf smart-buffer:*default-memory-limit* *memory-limit*
             smart-buffer:*default-disk-limit* *disk-limit*
             *body-file* nil)
       (unwind-protect (progn ,@body)
         (setf smart-buffer:*default-memory-limit* memory-limit
               smart-buffer:*default-disk-limit* disk-limit)
         (uiop:delete-directory-tree temporary-directory
                                      :validate t :if-does-not-exist :ignore)))))

(defun body-files ()
  (uiop:directory-files smart-buffer::*temporary-directory*))

(defun wait-until (predicate &key (timeout 5))
  (loop repeat (* timeout 20)
        until (funcall predicate)
        do (sleep 0.05))
  (funcall predicate))

(defun receive-until-eof (stream)
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0)))
    (handler-case
        (sb-ext:with-timeout 5
          (loop for byte = (read-byte stream nil nil)
                while byte
                do (vector-push-extend byte bytes)))
      (sb-ext:timeout () nil)
      (error () nil))
    (utf-8-bytes-to-string bytes)))

(defun send-request (stream headers &optional body)
  (write-sequence
   (string-to-utf-8-bytes
    (format nil "POST / HTTP/1.1~C~CHost: localhost~C~C~A~C~C"
            #\Return #\Newline #\Return #\Newline headers
            #\Return #\Newline))
   stream)
  (when body (write-sequence body stream))
  (force-output stream))

(defmacro with-connection ((stream) &body body)
  (let ((socket (gensym "SOCKET")))
    `(let* ((,socket (usocket:socket-connect "127.0.0.1" *clack-test-access-port*
                                             :element-type '(unsigned-byte 8)))
            (,stream (usocket:socket-stream ,socket)))
       (unwind-protect (progn ,@body)
         (ignore-errors (usocket:socket-close ,socket))))))

(defun body-app (env)
  (let ((raw-body (getf env :raw-body))
        (size 0))
    (when (typep raw-body 'file-stream)
      (setf *body-file* (pathname raw-body)))
    (loop while (read-byte raw-body nil nil)
          do (incf size))
    `(200 (:content-type "text/plain") (,(princ-to-string size)))))

(deftest body-limit-and-cleanup
  (let ((clack.test:*clack-test-handler* :woo))
    (with-body-limits
      (testing-app "a spilled body is released after the response"
          #'body-app
        (let ((body (make-array (* 2 *memory-limit*)
                                :element-type '(unsigned-byte 8)
                                :initial-element 97)))
          (multiple-value-bind (response status)
              (dex:post (localhost) :content body :keep-alive nil)
            (ok (= status 200))
            (ok (string= response (princ-to-string (length body))))))
        (ok *body-file* "the body crossed the memory limit")
        (ok (not (probe-file *body-file*)) "the finalized body file is deleted"))

      (testing-app "a chunked body over the limit is cleaned up"
          #'body-app
        (let ((before (body-files))
              (body (make-array (1+ *disk-limit*)
                                :element-type '(unsigned-byte 8)
                                :initial-element 97)))
          (with-connection (stream)
            (send-request stream
                          (format nil "Transfer-Encoding: chunked~C~C"
                                  #\Return #\Newline))
            (write-sequence (string-to-utf-8-bytes
                             (format nil "~X~C~C" (length body) #\Return #\Newline)) stream)
            (write-sequence body stream)
            (write-sequence (string-to-utf-8-bytes
                             (format nil "~C~C0~C~C~C~C"
                                     #\Return #\Newline #\Return #\Newline
                                     #\Return #\Newline)) stream)
            (force-output stream)
            (let ((response (receive-until-eof stream)))
              (ok (search "HTTP/1.1 413 " response))))
          (ok (wait-until (lambda ()
                            (null (set-difference (body-files) before :test #'equal))))
              "the rejected body file is deleted"))))))

(deftest declared-body-limit-before-payload
  (let ((clack.test:*clack-test-handler* :woo))
    (with-body-limits
      (testing-app "a declared oversized body is rejected without sending payload"
          #'body-app
        (with-connection (stream)
          (send-request stream
                        (format nil "Content-Length: ~D~C~CConnection: close~C~C"
                                (1+ *disk-limit*) #\Return #\Newline
                                #\Return #\Newline))
          (let ((response (receive-until-eof stream)))
            (ok (search "HTTP/1.1 413 " response))
            (ok (not (search "HTTP/1.1 " response :start2 1)))))
        (ok (null (body-files)))))))
