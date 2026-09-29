(in-package :cl-user)
(defpackage woo.ev.socket
  (:use :cl)
  (:import-from :woo.ev.event-loop
                :*evloop*
                :*input-buffer*
                :deref-data-from-pointer
                :remove-pointer-from-registry)
  (:import-from :woo.ev.util
                :io-fd
                :define-c-callback)
  (:import-from :woo.syscall
                #+nil :close
                #+nil :write
                :errno
                :EWOULDBLOCK
                :EINTR
                :ECONNABORTED
                :ECONNREFUSED
                :ECONNRESET)
  (:import-from :woo.ev.condition
                :socket-closed)
  (:import-from :lev
                :ev-now
                :ev-io
                :ev-io-init
                :ev-io-start
                :ev-io-stop
                :ev-timer
                :ev-timer-init
                :ev-timer-start
                :ev-timer-stop
                :+EV-READ+
                :+EV-WRITE+)
  (:import-from :fast-io
                :make-output-buffer
                :fast-write-sequence
                :fast-write-byte
                :finish-output-buffer)
  (:import-from :cffi
                :with-pointer-to-vector-data
                :incf-pointer
                :foreign-free)
  (:export :socket
           :make-socket
           :socket-read-watcher
           :socket-write-watcher
           :socket-timeout-timer
           :socket-shutdown-timer
           :socket-last-activity
           :socket-remote-addr
           :socket-remote-port
           :socket-data
           :socket-read-cb
           :socket-open-p
           :socket-ssl-handle
           :socket-tls-shutdown-p
           :socket-tls-shutdown-read-wait-write-p
           :socket-tls-shutdown-recv-p
           :socket-input-rejected-p
           :stop-reading-for-close
           :*ssl-write-function*
           :*ssl-read-function*
           :*ssl-error-function*
           :async-write
           :check-socket-open

           :write-socket-data
           :write-socket-byte
           :write-socket-stream
           :flush-buffer
           :with-async-writing
           :start-static-stream
           :send-static-file
           :graceful-close-socket
           :tls-shutdown-step
           :tls-shutdown-read-step
           :close-socket))
(in-package :woo.ev.socket)

#-woo-no-ssl
(defvar *ssl-write-function* #'cl+ssl::ssl-write
  "Indirection for the nonblocking SSL_write call; tests may inject WANT results.")
#-woo-no-ssl
(defvar *ssl-read-function* #'cl+ssl::ssl-read
  "Indirection for the nonblocking SSL_read call; tests may inject WANT results.")
#-woo-no-ssl
(defvar *ssl-shutdown-function* #'cl+ssl::ssl-shutdown
  "Indirection for deterministic nonblocking shutdown tests.")
#-woo-no-ssl
(defvar *ssl-error-function* #'cl+ssl::ssl-get-error
  "Indirection for SSL_get_error; tests may inject deterministic retry paths.")

(defstruct (socket (:constructor %make-socket))
  (watchers (make-array 3
                        :element-type 'cffi:foreign-pointer
                        :initial-contents (list (cffi:foreign-alloc '(:struct lev:ev-io))
                                                (cffi:foreign-alloc '(:struct lev:ev-io))
                                                (cffi:foreign-alloc '(:struct lev:ev-timer))))
   :type (simple-array cffi:foreign-pointer (3)))
  (shutdown-watcher (cffi:foreign-alloc '(:struct lev:ev-timer))
                    :type cffi:foreign-pointer)
  (last-activity (lev:ev-now *evloop*) :type double-float)
  (fd nil :type fixnum)
  remote-addr
  remote-port
  data
  (tcp-read-cb nil :type symbol)
  (read-cb nil :type (or null function))
  (write-cb nil :type (or null function))
  (ssl-handle nil :type (or null cffi:foreign-pointer))
  (open-p t :type boolean)
  close-hooks
  flush-hooks
  output-admitter
  output-releaser
  (charged-output 0)
  http2-initializer
  body-admitter
  body-memory-limit
  input-holder
  (input-paused-p nil)
  ;; A rejected request may still have application bytes in the kernel/TLS
  ;; input buffer.  Stop consuming them while the error response drains;
  ;; TLS shutdown itself re-enables the watcher when it needs close_notify.
  (input-rejected-p nil)
  ;; Incremented when a response starts writing headers. This lets error
  ;; handlers distinguish a pre-commit failure from a committed stream.
  (response-generation 0 :type fixnum)

  (buffer (make-output-buffer #+lispworks :output #+lispworks :static))
  (sendfile-fd nil :type (or null fixnum))
  (sendfile-size nil :type (or null integer))
  (sendfile-offset 0 :type (or null integer))
  ;; TLS cannot use sendfile. Keep the stream alive and pump bounded chunks
  ;; from the owner event loop instead of loading the whole pathname.
  (send-stream nil :type (or null stream))
  (send-stream-size nil :type (or null integer))
  (send-stream-offset 0 :type (or null integer))
  ;; Once a TLS write has started, keep its bytes separate from later
  ;; application writes. OpenSSL may retry the same call after WANT_READ or
  ;; WANT_WRITE, so appending to the buffer being retried is unsafe.
  (pending-write-data nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (pending-write-offset 0 :type fixnum)
  (pending-output-charge 0 :type integer)
  (write-wait-read-p nil :type boolean)
  (read-wait-write-p nil :type boolean)
  (send-stream-error-p nil :type boolean)
  (tls-shutdown-p nil :type boolean)
  (tls-shutdown-read-wait-write-p nil :type boolean)
  (tls-shutdown-recv-p nil :type boolean)
  (tls-close-after-drain-p nil :type boolean)
  (tls-drain-deadline nil :type (or null double-float))
  (tls-shutdown-deadline nil :type (or null double-float)))

(defun buffer-empty-p (socket)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (= (the fixnum (fast-io::output-buffer-len (socket-buffer socket))) 0))

(defun make-socket (&rest initargs &key tcp-read-cb fd &allow-other-keys)
  (let ((socket (apply #'%make-socket initargs)))
    (lev:ev-io-init (socket-read-watcher socket)
                    tcp-read-cb
                    fd
                    lev:+EV-READ+)
    (lev:ev-io-init (socket-write-watcher socket)
                    'async-write-cb
                    fd
                    lev:+EV-WRITE+)
    ;; Every allocated watcher must be initialized before close-socket can
    ;; stop it, including sockets closed before start-listening. This timer
    ;; stays inactive until TCP installs its connection-timeout callback.
    (lev:ev-timer-init (socket-timeout-timer socket)
                       'tls-shutdown-cb 0.0d0 0.0d0)
    (lev:ev-timer-init (socket-shutdown-timer socket)
                       'tls-shutdown-cb 0.0d0 0.0d0)
    socket))

(declaim (inline socket-read-watcher socket-write-watcher socket-timeout-timer))

(defun socket-read-watcher (socket)
  (svref (socket-watchers socket) 0))

(defun socket-write-watcher (socket)
  (svref (socket-watchers socket) 1))

(defun socket-timeout-timer (socket)
  (svref (socket-watchers socket) 2))

(defun socket-shutdown-timer (socket)
  (socket-shutdown-watcher socket))

(defun free-watchers (socket)
  (let ((read-watcher (socket-read-watcher socket))
        (write-watcher (socket-write-watcher socket))
        (timeout-timer (socket-timeout-timer socket)))
    (let ((shutdown-timer (socket-shutdown-timer socket)))
      (lev:ev-timer-stop *evloop* shutdown-timer)
      (cffi:foreign-free shutdown-timer))
    (lev:ev-io-stop *evloop* read-watcher)
    (lev:ev-io-stop *evloop* write-watcher)
    (lev:ev-timer-stop *evloop* timeout-timer)
    (cffi:foreign-free read-watcher)
    (cffi:foreign-free write-watcher)
    (cffi:foreign-free timeout-timer)))

(defun close-socket (socket &key (abort t))
  (declare (ignorable abort))
  (when (socket-open-p socket)
    (setf (socket-open-p socket) nil)
    (release-buffer-charge socket)
    (when (plusp (socket-pending-output-charge socket))
      (let ((count (socket-pending-output-charge socket)))
        (setf (socket-pending-output-charge socket) 0)
        (when (socket-output-releaser socket)
          (funcall (socket-output-releaser socket) count))))
    (dolist (hook (prog1 (socket-close-hooks socket)
                    (setf (socket-close-hooks socket) nil)))
      (handler-case (funcall hook)
        (error () (vom:error "Socket cleanup hook failed"))))
    (free-watchers socket)
    (let ((stream (socket-send-stream socket)))
      (when stream
        (ignore-errors (close stream :abort t))
        (setf (socket-send-stream socket) nil)))
    #-woo-no-ssl
    (let ((ssl-handle (socket-ssl-handle socket)))
      (when ssl-handle
        ;; Abort paths skip close_notify. Graceful paths perform it through
        ;; TLS-SHUTDOWN-STEP before this final teardown.
        (unless abort
          (ignore-errors (cl+ssl::ssl-shutdown ssl-handle)))
        (ignore-errors (cl+ssl::ssl-free ssl-handle))
        (setf (socket-ssl-handle socket) nil)))
    (let ((fd (socket-fd socket)))
      (wsys:close fd)
      (remove-pointer-from-registry fd))
    (setf (socket-read-cb socket) nil
          (socket-write-cb socket) nil
          (socket-buffer socket) nil
          (socket-pending-write-data socket) nil
          (socket-tls-shutdown-p socket) nil
          (socket-tls-shutdown-read-wait-write-p socket) nil
          (socket-tls-shutdown-recv-p socket) nil
          (socket-tls-close-after-drain-p socket) nil
          (socket-input-rejected-p socket) nil
          (socket-data socket) nil)
    (let ((sendfile-fd (socket-sendfile-fd socket)))
      (when sendfile-fd
        (wsys:close sendfile-fd)
        (setf (socket-sendfile-fd socket) nil))))
  t)

(defun stop-reading-for-close (socket)
  "Stop application reads while a terminal response is being drained."
  (when (socket-open-p socket)
    (setf (socket-input-rejected-p socket) t)
    ;; SSL_write may be waiting for read readiness. Keep that watcher armed
    ;; so the pending write can progress; TCP dispatch checks the rejection
    ;; flag before attempting another application SSL_read.
    (unless (socket-write-wait-read-p socket)
      (lev:ev-io-stop *evloop* (socket-read-watcher socket))))
  socket)

(defun arm-tls-shutdown-timer (socket &optional continuation-delay)
  "Arm a continuation or the original deadline without extending shutdown."
  (let ((deadline (socket-tls-shutdown-deadline socket)))
    (when (and deadline (socket-open-p socket))
      (let ((timer (socket-shutdown-timer socket))
            (remaining (max 0.0d0 (- deadline (lev:ev-now *evloop*)))))
        (lev:ev-timer-stop *evloop* timer)
        (lev:ev-timer-init timer 'tls-shutdown-cb
                           (if continuation-delay
                               (min continuation-delay remaining)
                               remaining)
                           0.0d0)
        (setf (cffi:foreign-slot-value timer '(:struct lev:ev-timer) 'lev::data)
              (socket-read-watcher socket))
        (lev:ev-timer-start *evloop* timer)))))

(defun tls-shutdown-step (socket)
  "Advance a nonblocking TLS close_notify, or abort at its deadline."
  (unless (socket-tls-shutdown-p socket)
    (return-from tls-shutdown-step t))
  (when (or (not (socket-open-p socket))
            (and (socket-tls-shutdown-deadline socket)
                 (>= (lev:ev-now *evloop*)
                     (socket-tls-shutdown-deadline socket))))
    (close-socket socket :abort t)
    (return-from tls-shutdown-step t))
  (when (socket-tls-shutdown-recv-p socket)
    (return-from tls-shutdown-step (tls-shutdown-read-step socket)))
  #+woo-no-ssl
  (progn (close-socket socket :abort t) t)
  #-woo-no-ssl
  (let ((handle (socket-ssl-handle socket)))
    (unless handle
      (close-socket socket :abort t)
      (return-from tls-shutdown-step t))
    (arm-tls-shutdown-timer socket)
    (let ((result (ignore-errors (funcall *ssl-shutdown-function* handle))))
      (cond
        ((eql result 1)
         (close-socket socket :abort t)
         t)
        ((null result)
         (close-socket socket :abort t)
         t)
        (t
         (let ((errno (ignore-errors (funcall *ssl-error-function* handle result))))
           (setf (socket-tls-shutdown-read-wait-write-p socket) nil)
           (cond
             ((eql errno cl+ssl::+ssl-error-want-write+)
              ;; The local alert has not finished writing yet.
              (setf (socket-tls-shutdown-recv-p socket) nil)
              (lev:ev-io-stop *evloop* (socket-read-watcher socket))
              (lev:ev-io-start *evloop* (socket-write-watcher socket))
              nil)
             ((or (eql errno cl+ssl::+ssl-error-want-read+) (zerop result))
              ;; Older OpenSSL versions return 0 for a sent alert without
              ;; reporting WANT_READ. Newer versions can report WANT_WRITE
              ;; with 0, handled above. Drain input before another shutdown.
              (setf (socket-tls-shutdown-recv-p socket) t)
              (lev:ev-io-stop *evloop* (socket-write-watcher socket))
              (lev:ev-io-start *evloop* (socket-read-watcher socket))
              ;; A previous SSL_read may already have buffered peer records.
              ;; Continue after the HTTP parser's current stack unwinds.
              (arm-tls-shutdown-timer socket 0.001d0)
              nil)
             (t
              (close-socket socket :abort t)
              t))))))))

(defun tls-shutdown-read-step (socket)
  "Discard bounded application input while waiting for peer close_notify."
  (unless (and (socket-open-p socket) (socket-tls-shutdown-p socket))
    (return-from tls-shutdown-read-step t))
  (unless (socket-tls-shutdown-recv-p socket)
    (return-from tls-shutdown-read-step (tls-shutdown-step socket)))
  (when (and (socket-tls-shutdown-deadline socket)
             (>= (lev:ev-now *evloop*)
                 (socket-tls-shutdown-deadline socket)))
    (close-socket socket :abort t)
    (return-from tls-shutdown-read-step t))
  #+woo-no-ssl
  (return-from tls-shutdown-read-step (close-socket socket :abort t))
  #-woo-no-ssl
  (let ((handle (socket-ssl-handle socket)))
    (unless handle
      (return-from tls-shutdown-read-step (close-socket socket :abort t)))
    ;; A continuation timer is one-shot. Restore the absolute deadline even
    ;; when this retry reaches WANT_READ/WRITE and no further I/O arrives.
    (arm-tls-shutdown-timer socket)
    (setf (socket-tls-shutdown-read-wait-write-p socket) nil)
    ;; Four reads at the current 16 KiB input-buffer size keep each callback
    ;; bounded to at most 64 KiB; the absolute shutdown deadline remains the
    ;; overall bound if more data is still in flight.
    (loop repeat 4
          while (socket-open-p socket)
          do (let ((n (funcall *ssl-read-function*
                               handle
                               (static-vectors:static-vector-pointer *input-buffer*)
                               (length *input-buffer*))))
               (declare (type fixnum n))
               (cond
                 ((plusp n)
                  (setf (socket-last-activity socket) (lev:ev-now *evloop*)))
                 (t
                  (let ((errno (funcall *ssl-error-function* handle n)))
                    (cond
                      ((= errno cl+ssl::+ssl-error-zero-return+)
                       (setf (socket-tls-shutdown-recv-p socket) nil)
                       (tls-shutdown-step socket)
                       (return-from tls-shutdown-read-step t))
                      ((= errno cl+ssl::+ssl-error-want-read+)
                       ;; Do not call SSL_shutdown again before peer EOF.
                       (lev:ev-io-stop *evloop* (socket-write-watcher socket))
                       (lev:ev-io-start *evloop* (socket-read-watcher socket))
                       (return-from tls-shutdown-read-step t))
                      ((= errno cl+ssl::+ssl-error-want-write+)
                       (setf (socket-tls-shutdown-read-wait-write-p socket) t)
                       (lev:ev-io-stop *evloop* (socket-read-watcher socket))
                       (lev:ev-io-start *evloop* (socket-write-watcher socket))
                       (return-from tls-shutdown-read-step t))
                      (t
                       (close-socket socket :abort t)
                       (return-from tls-shutdown-read-step t))))))))
    (when (socket-open-p socket)
      (lev:ev-io-stop *evloop* (socket-write-watcher socket))
      (lev:ev-io-start *evloop* (socket-read-watcher socket))
      ;; Continue bounded draining even when OpenSSL consumed records from
      ;; its internal buffer without another kernel readability edge.
      (arm-tls-shutdown-timer socket 0.001d0))
    t))

(define-c-callback tls-shutdown-cb :void
    ((evloop :pointer) (timer :pointer) (events :int))
  (declare (ignore evloop events))
  (let* ((watcher (cffi:foreign-slot-value timer '(:struct lev:ev-timer) 'lev::data))
         (socket (and watcher (deref-data-from-pointer (io-fd watcher)))))
    (when socket
      (when (and (socket-tls-close-after-drain-p socket)
                 (socket-tls-drain-deadline socket)
                 (>= (lev:ev-now *evloop*) (socket-tls-drain-deadline socket)))
        (close-socket socket :abort t)
        (return-from tls-shutdown-cb))
      (if (socket-tls-shutdown-recv-p socket)
          (tls-shutdown-read-step socket)
          (tls-shutdown-step socket)))))

(defun graceful-close-socket (socket &key deadline)
  "Drain accepted output and close_notify within one second or DEADLINE."
  (when (socket-open-p socket)
    (when (or (socket-tls-shutdown-p socket)
              (socket-tls-close-after-drain-p socket))
      (return-from graceful-close-socket socket))
    (let ((absolute-deadline
            (min (or deadline most-positive-double-float)
                 (+ (lev:ev-now *evloop*) 1.0d0))))
      (setf (socket-tls-drain-deadline socket) absolute-deadline
            (socket-tls-shutdown-deadline socket) absolute-deadline)
      (let ((timer (socket-shutdown-timer socket)))
        (lev:ev-timer-init timer 'tls-shutdown-cb
                           (max 0.0d0 (- absolute-deadline (lev:ev-now *evloop*)))
                           0.0d0)
        (setf (cffi:foreign-slot-value timer '(:struct lev:ev-timer) 'lev::data)
              (socket-read-watcher socket))
        (lev:ev-timer-start *evloop* timer))
      (when (or (socket-pending-write-data socket)
                (not (buffer-empty-p socket))
                (socket-sendfile-fd socket)
                (socket-send-stream socket))
        (setf (socket-tls-close-after-drain-p socket) t)
        (unless (socket-write-wait-read-p socket)
          (lev:ev-io-start *evloop* (socket-write-watcher socket)))
        (return-from graceful-close-socket socket))
      #+woo-no-ssl
      (return-from graceful-close-socket (close-socket socket :abort t))
      #-woo-no-ssl
      (if (null (socket-ssl-handle socket))
          (close-socket socket :abort t)
          (progn
            (setf (socket-tls-shutdown-p socket) t)
            (lev:ev-io-stop *evloop* (socket-read-watcher socket))
            (lev:ev-io-stop *evloop* (socket-write-watcher socket))
            (tls-shutdown-step socket))))))

(defun check-socket-open (socket)
  (unless (socket-open-p socket)
    (error 'socket-closed)))

(defun charge-output (socket count)
  (when (socket-output-admitter socket)
    (unless (funcall (socket-output-admitter socket) count)
      (error 'woo.ev.condition:output-limit-exceeded))
    (incf (socket-charged-output socket) count)))

(defun release-buffer-charge (socket)
  (when (plusp (socket-charged-output socket))
    (let ((count (socket-charged-output socket)))
      (setf (socket-charged-output socket) 0)
      (when (socket-output-releaser socket)
        (funcall (socket-output-releaser socket) count)))))

(defun write-socket-data (socket data &key (start 0) (end (length data))
                                        (write-cb nil write-cb-specified-p))
  (declare (optimize speed)
           (type vector data)
           (type fixnum start end))
  (when (socket-open-p socket)
    (charge-output socket (- end start))
    (when write-cb-specified-p
      (setf (socket-write-cb socket) write-cb))
    (if (typep data '(simple-array (unsigned-byte 8) (*)))
        (fast-write-sequence data
                             (socket-buffer socket)
                             start end)
        (loop for i from start upto (1- end)
              for byte of-type (unsigned-byte 8) = (aref data i)
              do (fast-write-byte byte (socket-buffer socket))))))

(defun write-socket-byte (socket byte &key (write-cb nil write-cb-specified-p))
  (declare (optimize speed)
           (type (unsigned-byte 8) byte))
  (when (socket-open-p socket)
    (charge-output socket 1)
    (when write-cb-specified-p
      (setf (socket-write-cb socket) write-cb))
    (fast-write-byte byte (socket-buffer socket))))

(defun write-socket-stream (socket stream &key (write-cb nil write-cb-specified-p))
  (declare (optimize speed)
           (type file-stream stream))
  (when (socket-open-p socket)
    (when write-cb-specified-p
      (setf (socket-write-cb socket) write-cb))
    (let ((file-size (file-length stream))
          (buffer (socket-buffer socket)))
      (unless (= (file-position stream) file-size)
        (loop
          (let* ((start (fast-io::output-buffer-fill buffer))
                 (end
                   (read-sequence (fast-io::output-buffer-vector buffer)
                                  stream
                                  :start start)))
            (setf (fast-io::output-buffer-fill buffer) end)
            (incf (fast-io::output-buffer-len buffer)
                  (- end start)))
          (cond
            ((= (file-position stream) file-size)
             (return))
            ;; Prevent from loading a too large file on memory.
            ;; TODO: Allow to set the threshold by users.
            ((< 1048576 (fast-io::output-buffer-len buffer))
             (and (flush-buffer socket)
                  (reset-buffer socket)))
            (t
            (fast-io::extend buffer))))))))

(defun start-static-stream (socket stream size)
  "Transfer ownership of STREAM to SOCKET and send it in bounded chunks.
   The stream is closed by the socket on completion, cancellation, or error."
  (check-socket-open socket)
  (when (socket-send-stream socket)
    (ignore-errors (close (socket-send-stream socket) :abort t)))
  (setf (socket-send-stream socket) stream
        (socket-send-stream-size socket) size
        (socket-send-stream-offset socket) (file-position stream))
  socket)

(defun pump-static-stream (socket)
  (let ((stream (socket-send-stream socket)))
    (when stream
      (let* ((remaining (- (or (socket-send-stream-size socket) 0)
                           (socket-send-stream-offset socket)))
             (count (min 65536 (max 0 remaining)))
             (chunk (make-array count :element-type '(unsigned-byte 8))))
        (if (zerop count)
            (progn
              (ignore-errors (close stream))
              (setf (socket-send-stream socket) nil))
            (let ((n (read-sequence chunk stream :end count)))
              (if (zerop n)
                  (progn
                    (ignore-errors (close stream))
                    (setf (socket-send-stream socket) nil
                          ;; A short read is valid, but EOF before the
                          ;; declared Content-Length is a failed response.
                          (socket-send-stream-error-p socket) t))
                  (progn
                    (incf (socket-send-stream-offset socket) n)
                    (write-socket-data socket chunk :end n)))))))))

(declaim (inline reset-buffer))
(defun reset-buffer (socket)
  (let ((buffer (socket-buffer socket)))
    (when buffer
      (setf (fast-io::output-buffer-vector buffer) (fast-io::make-octet-vector fast-io:*default-output-buffer-size*)
            (fast-io::output-buffer-fill buffer) 0
            (fast-io::output-buffer-len buffer) 0
            (fast-io::output-buffer-queue buffer) nil
            (fast-io::output-buffer-last buffer) nil))))

(defun release-pending-charge (socket)
  (let ((count (socket-pending-output-charge socket)))
    (setf (socket-pending-output-charge socket) 0)
    (when (and (plusp count) (socket-output-releaser socket))
      (funcall (socket-output-releaser socket) count))))

(defun flush-buffer (socket)
  "Flush one immutable pending write, preserving it across TLS retries."
  (declare (optimize speed))
  (check-socket-open socket)
  (unless (socket-pending-write-data socket)
    (let ((data (finish-output-buffer (socket-buffer socket))))
      (setf (socket-pending-write-data socket) data
            (socket-pending-write-offset socket) 0
            (socket-pending-output-charge socket)
            (socket-charged-output socket)
            (socket-charged-output socket) 0)
      (reset-buffer socket)))
  (let* ((data (socket-pending-write-data socket))
         (offset (socket-pending-write-offset socket))
         (len (- (length data) offset))
         (fd (socket-fd socket)))
    (when (zerop len)
      (setf (socket-pending-write-data socket) nil
            (socket-pending-write-offset socket) 0)
      (release-pending-charge socket)
      (return-from flush-buffer t))
    (cffi:with-pointer-to-vector-data (data-sap data)
      (let ((ptr (cffi:inc-pointer data-sap offset))
            (ssl-handle (socket-ssl-handle socket)))
        (let ((n
                #+woo-no-ssl
                (wsys:write fd ptr len)
                #-woo-no-ssl
                (if ssl-handle
                    (funcall *ssl-write-function* ssl-handle ptr len)
                    (wsys:write fd ptr len))))
          (declare (type fixnum n))
          (cond
            ((plusp n)
             (incf (socket-pending-write-offset socket) n)
             ;; Any progress completes the previous WANT_READ transition,
             ;; even when partial-write mode leaves another record pending.
             (setf (socket-write-wait-read-p socket) nil
                   (socket-last-activity socket) (lev:ev-now *evloop*))
             (lev:ev-io-start *evloop* (socket-write-watcher socket))
             (when (= (socket-pending-write-offset socket) (length data))
               (setf (socket-pending-write-data socket) nil
                     (socket-pending-write-offset socket) 0
                     (socket-write-wait-read-p socket) nil)
               (release-pending-charge socket)
               (lev:ev-io-start *evloop* (socket-write-watcher socket)))
             (null (socket-pending-write-data socket)))
            ((and (zerop n) (null ssl-handle)) nil)
            (t
             #+woo-no-ssl
             (let ((errno (wsys:errno)))
               (cond
                 ((or (= errno wsys:EWOULDBLOCK) (= errno wsys:EINTR)) nil)
                 (t (close-socket socket) t)))
             #-woo-no-ssl
             (if ssl-handle
                 (let ((errno (funcall *ssl-error-function* ssl-handle n)))
                   (cond
                     ((or (= errno cl+ssl::+ssl-error-want-write+))
                      (setf (socket-write-wait-read-p socket) nil)
                      (lev:ev-io-start *evloop* (socket-write-watcher socket))
                      nil)
                     ((= errno cl+ssl::+ssl-error-want-read+)
                      (setf (socket-write-wait-read-p socket) t)
                      (lev:ev-io-stop *evloop* (socket-write-watcher socket))
                      (lev:ev-io-start *evloop* (socket-read-watcher socket))
                      nil)
                     (t
                      (vom:error "Unexpected TLS write error (Code: ~D)" errno)
                      (close-socket socket)
                      t)))
                 (let ((errno (wsys:errno)))
                   (if (or (= errno wsys:EWOULDBLOCK) (= errno wsys:EINTR))
                       nil
                       (progn (close-socket socket) t)))))))))))

(defun send-file (socket)
  (declare (optimize speed))
  (let* ((infd (socket-sendfile-fd socket))
         (offset (socket-sendfile-offset socket))
         (n (wsys:sendfile infd (socket-fd socket) offset
                           (min (- (socket-sendfile-size socket) offset)
                                (* 1024 100)))))
    (declare (type fixnum n))
    (cond
      ((= n -1)
       (let ((errno (wsys:errno)))
         (declare (type fixnum errno))
         (return-from send-file
           (cond
             ((or (= errno wsys:EWOULDBLOCK)
                  (= errno wsys:EINTR))
              nil)
             ((or (= errno wsys:ECONNABORTED)
                  (= errno wsys:ECONNREFUSED)
                  (= errno wsys:ECONNRESET)
                  (= errno wsys:EPIPE)
                  (= errno wsys:ENOTCONN))
              (vom:error "Connection is already closed (Code: ~D)" errno)
              (close-socket socket)
              t)
             (t
              (vom:error "Unexpected error (Code: ~D)" errno)
              (close-socket socket)
              t)))))
      (t
       (setf (socket-last-activity socket) (lev:ev-now *evloop*))
       (let ((completedp (= (socket-sendfile-size socket)
                            (incf (socket-sendfile-offset socket) n))))
         (when completedp
           (wsys:close infd)
           (setf (socket-sendfile-fd socket) nil))
         completedp)))))

(defun async-write (socket)
  (declare (optimize speed))
  (unless (socket-open-p socket)
    (return-from async-write t))
  (when (socket-tls-shutdown-p socket)
    (if (or (socket-tls-shutdown-read-wait-write-p socket)
            (socket-tls-shutdown-recv-p socket))
        (tls-shutdown-read-step socket)
        (tls-shutdown-step socket))
    (return-from async-write t))

  ;; Complete an in-flight TLS write before touching application bytes that
  ;; arrived while it was waiting for readiness.
  (when (socket-pending-write-data socket)
    (unless (flush-buffer socket)
      (return-from async-write nil)))
  ;; A nonblocking SSL_read can require write readiness (handshake/alert).
  ;; Retry that read from the write callback once the write side is serviced.
  (when (and (socket-open-p socket)
             (socket-read-wait-write-p socket)
             (socket-tcp-read-cb socket))
    (setf (socket-read-wait-write-p socket) nil)
    (funcall (symbol-function (socket-tcp-read-cb socket))
             *evloop* (socket-read-watcher socket) lev:+EV-WRITE+)
    (unless (socket-open-p socket)
      (return-from async-write t)))
  ;; The read callback may still need write readiness after the retry.
  ;; Keep the write watcher armed instead of falling through to the
  ;; completed-output stop below.
  (when (socket-read-wait-write-p socket)
    (return-from async-write nil))
  (unless (socket-open-p socket)
    (return-from async-write t))
  ;; Send from buffer
  (unless (buffer-empty-p socket)
    (unless (flush-buffer socket)
      (return-from async-write nil)))
  ;; TLS pathname responses use a stream rather than sendfile. Pump one
  ;; bounded chunk per readiness callback to preserve backpressure.
  (when (and (socket-open-p socket)
             (null (socket-sendfile-fd socket))
             (socket-send-stream socket))
    (pump-static-stream socket)
    (when (socket-send-stream-error-p socket)
      (vom:error "TLS static response ended before its declared length")
      (close-socket socket)
      (return-from async-write t))
    (unless (buffer-empty-p socket)
      (unless (flush-buffer socket)
        (return-from async-write nil))
      ;; Leave the write watcher armed for the next bounded chunk. The
      ;; response callback belongs only to the final chunk.
      (when (socket-send-stream socket)
        (return-from async-write nil))))
  ;; Send a static file?
  (when (socket-sendfile-fd socket)
    (unless (send-file socket)
      (return-from async-write nil)))

  ;; Transfer has been completed.
  (unless (socket-open-p socket)
    (return-from async-write t))
  (let ((callback (socket-write-cb socket))
        (hooks (prog1 (socket-flush-hooks socket) (setf (socket-flush-hooks socket) nil))))
    (setf (socket-write-cb socket) nil)
    (when callback (funcall callback socket))
    (dolist (hook hooks) (funcall hook)))
  (when (and (socket-open-p socket)
             (socket-tls-close-after-drain-p socket)
             (buffer-empty-p socket)
             (null (socket-pending-write-data socket))
             (null (socket-sendfile-fd socket))
             (null (socket-send-stream socket)))
    (setf (socket-tls-close-after-drain-p socket) nil)
    (graceful-close-socket socket :deadline (socket-tls-drain-deadline socket))
    (return-from async-write t))

  ;; Completion callbacks can enqueue another write. Keep its watcher alive.
  (when (and (socket-open-p socket) (buffer-empty-p socket)
             (not (socket-tls-shutdown-p socket))
             (null (socket-pending-write-data socket))
             (null (socket-sendfile-fd socket))
             (null (socket-send-stream socket))
             (null (socket-flush-hooks socket))
             (not (socket-read-wait-write-p socket)))
    (lev:ev-io-stop *evloop* (socket-write-watcher socket)))
  t)

(define-c-callback async-write-cb :void ((evloop :pointer) (io :pointer) (events :int))
  (declare (optimize speed)
           (ignore events))
  (let* ((fd (io-fd io))
         (socket (deref-data-from-pointer fd)))
    (unless socket
      (lev:ev-io-stop evloop io)
      (cffi:foreign-free io)
      (return-from async-write-cb))

    (when (socket-tls-shutdown-p socket)
      (if (or (socket-tls-shutdown-read-wait-write-p socket)
              (socket-tls-shutdown-recv-p socket))
          (tls-shutdown-read-step socket)
          (tls-shutdown-step socket))
      (return-from async-write-cb))

    (handler-case (async-write socket)
      (woo.ev.condition:output-limit-exceeded () (close-socket socket)))))

(defmacro with-async-writing ((socket &key write-cb force-streaming) &body body)
  `(progn
     ,@body
     (setf (socket-write-cb ,socket) ,write-cb)
     ,(if force-streaming
          `(unless (async-write ,socket)
             (unless (socket-write-wait-read-p ,socket)
               (lev:ev-io-start *evloop* (socket-write-watcher ,socket))))
          `(lev:ev-io-start *evloop* (socket-write-watcher ,socket)))))

(defun send-static-file (socket fd size)
  (with-slots (sendfile-fd sendfile-size sendfile-offset) socket
    (when sendfile-fd
      (warn "Trying to send another file while sending a file.")
      (wsys:close sendfile-fd))
    (setf sendfile-fd fd
          sendfile-size size
          sendfile-offset 0)))
