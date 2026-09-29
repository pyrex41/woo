(defpackage woo.dispatch
  (:use :cl)
  (:import-from :woo.ev.socket :socket-open-p)
  (:export :*dispatch-drop-callback* :dispatch-work :make-dispatch-work :dispatch-work-p :dispatch-work-thunk :dispatch-work-on-drop :loop-dispatcher :make-loop-dispatcher :dispatcher-id :dispatcher-evloop :dispatcher-watcher :dispatcher-lock :dispatcher-thunks :dispatcher-budgets :dispatcher-stopped :dispatcher-cancel-budgets :*dispatchers* :*dispatchers-lock* :*dispatcher-counter* :find-dispatcher :drain-dispatcher :dispatcher-enqueue :stop-dispatcher :current-loop-dispatcher :http2-socket-accepts-p :dispatcher-runner :make-loop-runner))
(in-package :woo.dispatch)

;;; Running response writes on the connection's event loop.
;;;
;;; A delayed response may call the responder, and the streaming writer it
;;; returns, from any thread. Connection state, the HPACK encoder and the
;;; socket buffer belong to the event loop, so writes from another thread
;;; are queued and the loop is woken with an ev_async watcher, one per loop.
;;; When the loop stops, its dispatcher is marked stopped under its lock,
;;; so no thread wakes a loop that is being freed.

(defvar *dispatch-drop-callback* nil
  "Cleanup for the work currently being submitted if its loop drops it.")
(defstruct dispatch-work thunk on-drop)

(defstruct (loop-dispatcher (:conc-name dispatcher-))
  (id 0 :type fixnum)
  evloop
  watcher
  (lock (bt2:make-lock :name "Woo dispatcher"))
  ;; Thunks waiting to run on the loop, newest first.
  (thunks nil :type list)
  ;; Weak keys avoid retaining completed connections until the loop exits.
  (budgets #+sbcl (make-hash-table :test 'eq :weakness :key)
           #+ccl (make-hash-table :test 'eq :weak :key)
           #+lispworks (make-hash-table :test 'eq :weak-kind :key)
           #-(or sbcl ccl lispworks) (make-hash-table :test 'eq))
  ;; Set, under LOCK, before the loop and the watcher are freed.
  (stopped nil)
  cancel-budgets
  (draining nil)
  (pending-count 0))

(defvar *dispatchers* (make-hash-table)
  "Dispatcher id -> dispatcher. The id is kept in the loop's ev_userdata.")
(defvar *dispatchers-lock* (bt2:make-lock :name "Woo dispatchers"))
(defvar *dispatcher-counter* 0)

;; lev does not bind ev_unref. The async watcher must not keep ev_run alive
;; once the listener stops.
(cffi:defcfun ("ev_unref" %ev-unref) :void (evloop :pointer))

(defun find-dispatcher (evloop)
  (let ((userdata (lev:ev-userdata evloop)))
    (unless (cffi:null-pointer-p userdata)
      (bt2:with-lock-held (*dispatchers-lock*)
        (gethash (cffi:pointer-address userdata) *dispatchers*)))))

(defun drain-dispatcher (dispatcher)
  "Run one bounded batch in submission order; arrivals wake a later turn."
  (unless (dispatcher-draining dispatcher)
    (setf (dispatcher-draining dispatcher) t)
    (unwind-protect
         (let ((thunks (bt2:with-lock-held ((dispatcher-lock dispatcher))
                         (prog1 (nreverse (dispatcher-thunks dispatcher))
                           (setf (dispatcher-thunks dispatcher) nil
                                 (dispatcher-pending-count dispatcher) 0)))))
           (dolist (thunk thunks)
             (handler-case (funcall (if (dispatch-work-p thunk) (dispatch-work-thunk thunk) thunk))
               (error () (vom:error "Woo dispatch callback failed")))))
      (setf (dispatcher-draining dispatcher) nil))))

(cffi:defcallback http2-dispatch-cb :void ((evloop :pointer) (watcher :pointer) (events :int))
  (declare (ignore watcher events))
  (let ((dispatcher (find-dispatcher evloop)))
    (when dispatcher
      (drain-dispatcher dispatcher))))

(defun dispatcher-enqueue (dispatcher thunk)
  "Queue THUNK to run on DISPATCHER's loop and wake the loop. Returns T, or
   NIL, dropping THUNK, once the loop has stopped. The check and the wakeup
   hold the lock that stop-dispatcher takes before the loop is freed."
  (bt2:with-lock-held ((dispatcher-lock dispatcher))
    (unless (or (dispatcher-stopped dispatcher)
                (>= (dispatcher-pending-count dispatcher) 4096))
      (push (if *dispatch-drop-callback*
                (make-dispatch-work :thunk thunk :on-drop *dispatch-drop-callback*)
                thunk)
            (dispatcher-thunks dispatcher))
      (incf (dispatcher-pending-count dispatcher))
      (lev:ev-async-send (dispatcher-evloop dispatcher)
                         (dispatcher-watcher dispatcher))
      t)))

(defun stop-dispatcher (dispatcher)
  "Called on the loop's thread after ev_run returns, before the loop is
   freed. Later enqueues are refused. Queued thunks are dropped: they would
   write to sockets the loop is closing, and they hold connections and body
   octets. The watcher is stopped and freed, and the dispatcher forgotten."
  (let ((watcher nil)
        (evloop nil)
        (dropped nil))
    (bt2:with-lock-held ((dispatcher-lock dispatcher))
      (setf (dispatcher-stopped dispatcher) t
            dropped (dispatcher-thunks dispatcher)
            (dispatcher-thunks dispatcher) nil
            watcher (dispatcher-watcher dispatcher)
            evloop (dispatcher-evloop dispatcher)
            (dispatcher-watcher dispatcher) nil
            (dispatcher-evloop dispatcher) nil))
    (dolist (work dropped)
      (when (dispatch-work-p work)
        (ignore-errors (funcall (dispatch-work-on-drop work)))))
    (maphash (lambda (budget ignored)
               (declare (ignore ignored))
               (when (dispatcher-cancel-budgets dispatcher)
        (funcall (dispatcher-cancel-budgets dispatcher) budget)))
             (dispatcher-budgets dispatcher))
    (clrhash (dispatcher-budgets dispatcher))
    (bt2:with-lock-held (*dispatchers-lock*)
      (remhash (dispatcher-id dispatcher) *dispatchers*))
    (when evloop
      (lev:ev-set-userdata evloop (cffi:null-pointer))
      (when watcher
        ;; Undo the ev_unref done at start, as libev requires before a stop.
        (lev:ev-ref evloop)
        (lev:ev-async-stop evloop watcher)
        (cffi:foreign-free watcher)))
    nil))

(defun current-loop-dispatcher ()
  "The dispatcher of the event loop running in this thread, made on first
   use. NIL outside an event loop. A fresh loop has NULL ev_userdata, so a
   loop allocated at a freed loop's address does not find the old one. The
   dispatcher is stopped when the loop exits (woo.ev.event-loop's
   *evloop-exit-hooks*)."
  (let ((evloop woo.ev.event-loop:*evloop*))
    (when (and evloop (not (cffi:null-pointer-p evloop)))
      (or (find-dispatcher evloop)
          (let* ((id (bt2:with-lock-held (*dispatchers-lock*)
                       (incf *dispatcher-counter*)))
                 (watcher (cffi:foreign-alloc '(:struct lev:ev-async)))
                 (dispatcher (make-loop-dispatcher :id id :evloop evloop
                                                   :watcher watcher)))
            (lev:ev-async-init watcher 'http2-dispatch-cb)
            (lev:ev-async-start evloop watcher)
            (%ev-unref evloop)
            (bt2:with-lock-held (*dispatchers-lock*)
              (setf (gethash id *dispatchers*) dispatcher))
            (lev:ev-set-userdata evloop (cffi:make-pointer id))
            (push (lambda () (stop-dispatcher dispatcher))
                  woo.ev.event-loop:*evloop-exit-hooks*)
            dispatcher)))))

(defun http2-socket-accepts-p (socket)
  (or (null socket) (socket-open-p socket)))

(defun dispatcher-runner (dispatcher socket)
  "Call on DISPATCHER's loop thread. A function that runs a thunk on that
   loop and returns T, or NIL when the thunk was dropped because SOCKET is
   closed or the loop has stopped. On the loop thread the thunk runs at
   once. Outside a dispatch batch, it first drains the queued batch. Nested
   calls stay inside the current callback; callers must serialize a response's
   writes when using several application threads."
  (let ((owner (bt2:current-thread)))
    (lambda (thunk)
      (cond
        ((eq (bt2:current-thread) owner)
         (drain-dispatcher dispatcher)
         (funcall thunk)
         t)
        ;; The loop closes its sockets before it is freed, so a closed
        ;; socket means there may be no loop left to wake.
        ((http2-socket-accepts-p socket)
         (dispatcher-enqueue dispatcher thunk))))))

(defun make-loop-runner (socket)
  "Call on the connection's event-loop thread. Returns a function that runs
   a thunk on that loop (see dispatcher-runner). Without an event loop (unit
   tests) the thunk runs at once."
  (let ((dispatcher (and socket (current-loop-dispatcher))))
    (if dispatcher
        (dispatcher-runner dispatcher socket)
        (lambda (thunk)
          (funcall thunk)
          t))))
