(in-package :woo-showcase)

(defvar *app* (make-instance 'ningle:app))
(defvar *server* nil)

;; Static file path
(defvar *static-root*
  (asdf:system-relative-pathname :woo-showcase "static/"))

;; Template directory
(defvar *template-root*
  (asdf:system-relative-pathname :woo-showcase "templates/"))

;; Initialize Djula templates
(djula:add-template-directory *template-root*)

(defun render-template (name &rest args)
  (let ((template (djula:compile-template* name)))
    (apply #'djula:render-template* template nil args)))

;; Landing page
(setf (ningle:route *app* "/")
      (lambda (params)
        (declare (ignore params))
        (render-template "index.html")))

;; Benchmarks page
(setf (ningle:route *app* "/benchmarks")
      (lambda (params)
        (declare (ignore params))
        (render-template "benchmarks.html")))

;; static/js/charts.js reads these camelCase keys. jonathan:to-json would
;; upcase plist keys ("REQUESTS-PER-SEC") and encode NIL as [].
(defparameter *benchmark-json-keys*
  '((:name . :|name|)
    (:language . :|language|)
    (:requests-per-sec . :|requestsPerSec|)
    (:latency-avg-ms . :|latencyAvgMs|)
    (:latency-max-ms . :|latencyMaxMs|)
    (:highlight . :|highlight|)))

(defun benchmark-json-object (entry)
  "ENTRY of *BENCHMARK-DATA* as a plist with the JSON keys; booleans are
   true/false."
  (loop for (key . json-key) in *benchmark-json-keys*
        for value = (getf entry key)
        append (list json-key
                     (if (eq key :highlight)
                         (if value t :false)
                         value))))

(defun benchmarks-json ()
  (jonathan:to-json (mapcar #'benchmark-json-object *benchmark-data*)))

;; API endpoint for benchmark data (JSON)
(setf (ningle:route *app* "/api/benchmarks")
      (lambda (params)
        (declare (ignore params))
        (setf (getf (lack.response:response-headers ningle:*response*) :content-type)
              "application/json")
        (benchmarks-json)))

;; WebSocket demo page
(setf (ningle:route *app* "/websocket")
      (lambda (params)
        (declare (ignore params))
        (render-template "websocket.html")))

(defun request-clack-env (request)
  (woo-showcase.limits:request-clack-env request))

;; WebSocket upgrade endpoint
(setf (ningle:route *app* "/ws/echo")
      (lambda (params)
        (declare (ignore params))
        (let* ((env (request-clack-env ningle:*request*))
               (socket (getf env :clack.io)))
          (when (woo:websocket-p env)
            (let* ((headers (getf env :headers))
                   (key (gethash "sec-websocket-key" headers))
                   (accept (woo:compute-accept-key key)))
              ;; Send upgrade response
              (woo:write-websocket-upgrade-response socket accept)
              ;; Setup WebSocket handling
              ;; Setup WebSocket handling. No :on-close: the default one
              ;; answers the client's close frame and closes the socket.
              (woo:setup-websocket socket
                :on-message (lambda (opcode payload)
                              ;; Echo back the message with its own type: a
                              ;; binary payload need not be valid UTF-8.
                              (if (= opcode woo.websocket:+opcode-binary+)
                                  (woo:send-binary-frame socket payload)
                                  (woo:send-text-frame socket
                                    (trivial-utf-8:utf-8-bytes-to-string payload)))))
              ;; Woo writes no HTTP response once the socket is upgraded,
              ;; whatever the route returns.
              nil)))))

;; HTTP/2 demo page
(setf (ningle:route *app* "/http2")
      (lambda (params)
        (declare (ignore params))
        (render-template "http2.html")))

(defparameter *max-resource-delay-ms* woo-showcase.limits:*max-resource-delay-ms*)

(defun clamp-resource-delay (query-delay)
  (woo-showcase.limits:clamp-resource-delay query-delay))

;; Run THUNK after SECONDS on the current woo event loop, without blocking
;; it. Sleeping in a handler would stall every connection on that worker.
(defvar *pending-timers* (make-hash-table))
(defvar *pending-timers-lock* (bt2:make-lock :name "showcase-timers"))

(cffi:defcallback showcase-timer-cb :void ((evloop :pointer) (timer :pointer) (events :int))
  (declare (ignore events))
  (lev:ev-timer-stop evloop timer)
  (let ((thunk (bt2:with-lock-held (*pending-timers-lock*)
                 (prog1 (gethash (cffi:pointer-address timer) *pending-timers*)
                   (remhash (cffi:pointer-address timer) *pending-timers*)))))
    (cffi:foreign-free timer)
    (when thunk
      (handler-case (funcall thunk)
        (error (e) (warn "Delayed response failed: ~A" e))))))

(defun call-later (seconds thunk)
  (if (null woo.ev:*evloop*)
      ;; Not on a woo event loop: nothing else to stall.
      (progn (sleep seconds) (funcall thunk))
      (let ((timer (cffi:foreign-alloc '(:struct lev:ev-timer))))
        (bt2:with-lock-held (*pending-timers-lock*)
          (setf (gethash (cffi:pointer-address timer) *pending-timers*) thunk))
        (lev:ev-timer-init timer 'showcase-timer-cb (coerce seconds 'double-float) 0.0d0)
        (lev:ev-timer-start woo.ev:*evloop* timer))))

;; Simulated slow resource (configurable delay). A delayed Clack response:
;; the responder is called from a timer on the same event loop, so other
;; requests keep being served while this one waits.
(setf (ningle:route *app* "/api/resource/:id")
      (lambda (params)
        (let* ((id (cdr (assoc :id params)))
               (query-delay (cdr (assoc "delay" params :test #'string=)))
               (delay (clamp-resource-delay query-delay))
               (socket (getf (request-clack-env ningle:*request*) :clack.io))
               ;; Ningle finalizes the delayed result with its context
               ;; variables, bound only while this handler runs. The timer
               ;; fires later, so rebind them there.
               (context ningle:*context*)
               (request ningle:*request*)
               (response ningle:*response*)
               (session ningle:*session*))
          (setf (getf (lack.response:response-headers response) :content-type)
                "application/json")
          (lambda (responder)
            (call-later (/ delay 1000.0)
                        (lambda ()
                          ;; The client may have gone away meanwhile.
                          (when (or (not (typep socket 'woo.ev.socket:socket))
                                    (woo.ev.socket:socket-open-p socket))
                            (let ((ningle:*context* context)
                                  (ningle:*request* request)
                                  (ningle:*response* response)
                                  (ningle:*session* session))
                              (funcall responder
                                       (jonathan:to-json
                                        (list :|id| id
                                              :|timestamp| (get-universal-time)
                                              :|delay| delay)))))))))))

;; Build Lack app with middleware
(defvar *handler*
  (lack:builder
    :accesslog
    (:static :path "/static/" :root *static-root*)
    *app*))

(defun start (&key (port 5000) (worker-num 4))
  (setf *server*
        (woo:run *handler*
                 :port port
                 :worker-num worker-num
                 :debug nil))
  (format t "~%Woo Showcase running at http://127.0.0.1:~A~%" port)
  (format t "Press Ctrl+C to stop.~%"))

(defun stop ()
  (when *server*
    (woo:stop *server*)
    (setf *server* nil)
    (format t "Server stopped.~%")))
