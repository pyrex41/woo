(in-package :cl-user)
(defpackage woo-test (:use :cl))
(in-package :woo-test)
(defun raw-exchange (port &rest packets)
  (let ((sock (usocket:socket-connect "127.0.0.1" port :element-type '(unsigned-byte 8))))
    (unwind-protect
         (let ((stream (usocket:socket-stream sock))
               (out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
           (dolist (packet packets)
             (write-sequence (trivial-utf-8:string-to-utf-8-bytes packet) stream)
             (force-output stream))
           (handler-case
               (sb-ext:with-timeout 5
                 (loop for byte = (read-byte stream nil nil) while byte do
                   (vector-push-extend byte out)))
             (sb-ext:timeout () nil)
             (error () nil))
           (map 'string #'code-char out))
      (usocket:socket-close sock))))
(defun crlf-lines (&rest lines)
  (format nil "~{~A~C~C~}" (loop for line in lines append (list line #\Return #\Newline))))
