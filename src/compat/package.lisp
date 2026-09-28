(defpackage woo.compat
  (:use :cl)
  (:export :clackup :call-on-connection :server-state :shutdown-timeout
           :shutdown-timeout-workers :connection-closed :queue-limit-exceeded))
(defpackage clack.handler.woo-managed
  (:use :cl)
  (:export :run :stop))
(in-package :woo.compat)

(define-condition connection-closed (error) ())
(define-condition queue-limit-exceeded (error) ())
(define-condition shutdown-timeout (error)
  ((workers :initarg :workers :reader shutdown-timeout-workers))
  (:report (lambda (c s)
             (format s "Managed Woo shutdown timed out with ~D live workers"
                     (length (shutdown-timeout-workers c))))))

(defstruct (managed-server (:conc-name ms-))
  app options thread control startup-error
  (lock (bt2:make-lock :name "Woo managed server"))
  (wake (bt2:make-condition-variable))
  (state :starting) (jobs nil) (running 0) (workers nil)
  (requests (make-hash-table :test 'eq))
  (connections (make-hash-table :test 'eq))
  (input-bytes 0) (output-bytes 0) (dropped-completions 0)
  (application-workers 4) (max-pending-requests 64)
  (max-connection-requests 16) (max-request-body-bytes (* 64 1024 1024))
  (max-request-storage-bytes (* 256 1024 1024))
  (max-response-queue-bytes (* 8 1024 1024))
  (max-connection-queue-bytes (* 64 1024 1024))
  (max-server-queue-bytes (* 256 1024 1024))
  (startup-timeout 10) (drain-timeout 10) (cleanup-timeout 5))

(defclass connection ()
  ((server :initarg :server :reader connection-server)
   (socket :initarg :socket :reader connection-socket)
   (runner :initarg :runner :reader connection-runner)
   (owner :initarg :owner :reader connection-owner)
   (request :initform nil :accessor connection-request)
   (requests :initform nil :accessor connection-requests)
   (input-bytes :initform 0 :accessor connection-input-bytes)
   (held :initform nil :accessor connection-held)
   (held-bytes :initform 0 :accessor connection-held-bytes)
   (output-bytes :initform 0 :accessor connection-output-bytes)
   (flush-callbacks :initform nil :accessor connection-flush-callbacks)
   (upgraded :initform nil :accessor connection-upgraded)
   (h2-input :initform (make-hash-table :test 'eq) :reader connection-h2-input)
   (h2 :initform nil :accessor connection-h2)))

(defclass stream-connection (connection)
  ((parent :initarg :parent :reader stream-parent)))

(defstruct (managed-request (:conc-name mr-))
  server connection env runner responder body
  (phase :new) (cancelled nil) (producer-done nil) (wire-done nil)
  (input-bytes 0) (output-bytes 0) (dropped-completions 0) (delivered-bytes 0)
  (lock (bt2:make-lock :name "Woo response"))
  (expected-length nil) (status nil) (writer-closed nil)
  (headers-sent nil) (failure-handled nil) (close-p nil) (bodyless nil) (chunked nil)
  (callbacks nil) h2-stream)

(defun now () (/ (get-internal-real-time) internal-time-units-per-second))

(defun cancelled-p (request)
  (bt2:with-lock-held ((ms-lock (mr-server request)))
    (or (mr-cancelled request) (eq (mr-phase request) :closed))))

(defun parent-connection (connection)
  (if (typep connection 'stream-connection) (stream-parent connection) connection))

(defun server-state (handler)
  "Return a resource snapshot; HANDLER is the ordinary Clack handle."
  (let ((server (clack.handler::handler-acceptor handler)))
    (check-type server managed-server)
    (bt2:with-lock-held ((ms-lock server))
      (list :state (ms-state server) :queued (length (ms-jobs server))
            :running (ms-running server) :requests (hash-table-count (ms-requests server))
            :connections (hash-table-count (ms-connections server))
            :input-bytes (ms-input-bytes server) :output-bytes (ms-output-bytes server)
            :dropped-completions (ms-dropped-completions server)
            :live-workers (count-if #'bt2:thread-alive-p (ms-workers server))))))
