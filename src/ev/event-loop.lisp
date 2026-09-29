(in-package :cl-user)
(defpackage woo.ev.event-loop
  (:use :cl)
  (:import-from :lev
                :ev-loop-new
                :ev-run
                :+EVFLAG-FORKCHECK+)
  (:import-from :static-vectors
                :make-static-vector
                :free-static-vector)
  (:export :with-event-loop
           :check-event-loop-running

           :*evloop*
           :*buffer-size*
           :*input-buffer*
           :*data-registry*

           :callbacks
           :remove-callbacks
           :deref-data-from-pointer
           :remove-pointer-from-registry))
(in-package :woo.ev.event-loop)

(defparameter *evloop* nil)
(defvar *buffer-size* (* 1024 64))
(defparameter *input-buffer* nil)

(defvar *callbacks* nil)
(defvar *data-registry* nil)

(defun (setf callbacks) (callbacks pointer)
  (setf (gethash pointer *callbacks*) callbacks))

(defun callbacks (pointer)
  (declare (optimize (speed 3) (safety 0)))
  (when *callbacks*
    (gethash pointer (the hash-table *callbacks*))))

(defun remove-callbacks (pointer)
  (declare (optimize (speed 3) (safety 0)))
  (when *callbacks*
    (remhash pointer (the hash-table *callbacks*))))

(defun deref-data-from-pointer (pointer)
  (declare (optimize (speed 3) (safety 0)))
  (when *data-registry*
    (gethash pointer (the hash-table *data-registry*))))

(defun (setf deref-data-from-pointer) (data pointer)
  (setf (gethash pointer *data-registry*) data))

(defun remove-pointer-from-registry (pointer)
  (declare (optimize (speed 3) (safety 0)))
  (when *data-registry*
    (remhash pointer (the hash-table *data-registry*))))

(defmacro with-event-loop ((&key enable-fork cleanup-fn) &body body)
  `(let ((*evloop* (lev:ev-loop-new (if ,enable-fork
                                        lev:+EVFLAG-FORKCHECK+
                                        0)))
         (*callbacks* (make-hash-table :test 'eql))
         (*data-registry* (make-hash-table :test 'eql))
         (*input-buffer* (make-static-vector *buffer-size*)))
     (unwind-protect (progn
                       ,@body
                       (lev:ev-run *evloop* 0))
       (unwind-protect
            (progn
              ;; CLOSE-SOCKET removes its descriptor from *DATA-REGISTRY*.
              ;; Snapshot the table before closing every accepted socket.
              (let ((close-socket-fn (intern #.(string :close-socket) (find-package #.(string :woo.ev.socket))))
                    (sockets (loop for socket being the hash-values of *data-registry*
                                   collect socket)))
                (dolist (socket sockets)
                  (handler-case (funcall close-socket-fn socket)
                    (error (e) (vom:error "Error closing socket on event loop exit: ~A" e)))))
              ,@(when cleanup-fn
                  `((funcall ,cleanup-fn))))
         (unwind-protect
              (free-static-vector *input-buffer*)
           ;; Destroy the loop even if a caller's cleanup callback fails.
           ;; libev owns backend descriptors and internal watcher memory.
           (lev:ev-loop-destroy *evloop*))))))

(defun check-event-loop-running ()
  (unless *evloop*
    (error "Event loop not running")))
