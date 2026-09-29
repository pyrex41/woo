;; Highly optimized benchmark server for Woo
;; Compile with maximum optimizations

;; SBCL-specific optimizations
#+sbcl
(progn
  ;; Disable runtime type checking
  (declaim (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))
  ;; Disable GC during benchmark (optional, can cause issues)
  ;; (setf (sb-ext:bytes-consed-between-gcs) (* 512 1024 1024))
  )

(ql:quickload '(:woo) :silent t)

;; Pre-allocate response to avoid any allocation during request handling
(defvar *response-body* "Hello, World!")

;; Use a simple vector for headers to avoid plist overhead
(defvar *response-headers* 
  '(:content-type "text/plain" :content-length 13))

(defvar *response*
  (list 200 *response-headers* (list *response-body*)))

(declaim (inline benchmark-app))
(defun benchmark-app (env)
  (declare (ignore env)
           (optimize (speed 3) (safety 0)))
  *response*)

(defun positive-integer-or-nil (n)
  (and (integerp n) (plusp n) n))

(defun cpu-count ()
  "Online CPU count, or NIL if unknown. sysconf's _SC_NPROCESSORS_ONLN is
   84 on Linux only (on macOS it is 58 and 84 returns -1), so ask sysctl
   there."
  (positive-integer-or-nil
   (ignore-errors
     #+darwin
     (parse-integer (uiop:run-program '("sysctl" "-n" "hw.ncpu")
                                      :output :string)
                    :junk-allowed t)
     #+(and sbcl linux)
     (sb-alien:alien-funcall
      (sb-alien:extern-alien "sysconf"
                             (function sb-alien:long sb-alien:int))
      84)                               ; _SC_NPROCESSORS_ONLN
     #-(or darwin (and sbcl linux))
     nil)))

(defun get-worker-count ()
  "Get optimal worker count based on CPU cores. Always a positive integer,
   as woo:run requires."
  (or (positive-integer-or-nil
       (ignore-errors
         (parse-integer (uiop:getenv "WORKERS") :junk-allowed t)))
      (cpu-count)
      1))

(let ((workers (get-worker-count)))
  (format t "~%=== Woo Benchmark Server ===~%")
  (format t "Port: 3000~%")
  (format t "Workers: ~A~%" workers)
  (format t "SBCL version: ~A~%" (lisp-implementation-version))
  (format t "Optimizations: speed=3, safety=0~%")
  (format t "~%Starting server...~%~%")
  
  (woo:run #'benchmark-app
           :port 3000
           :address "127.0.0.1"
           :worker-num workers
           :debug nil
           :backlog 8192))  ; Increase listen backlog
