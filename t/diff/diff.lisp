(in-package :cl-user)
(defpackage woo-test.diff
  (:use :cl :rove)
  (:import-from :woo.http2.hpack
                :make-hpack-context
                :hpack-encode-headers
                :hpack-decode-headers
                :hpack-compression-error)
  (:import-from :woo.http2.frames
                :make-data-frame
                :make-headers-frame
                :make-settings-frame
                :make-settings-ack-frame
                :make-window-update-frame
                :make-ping-frame
                :make-ping-ack-frame
                :make-goaway-frame
                :make-rst-stream-frame
                :serialize-frame
                :parse-frame
                :frame-type
                :frame-flags
                :frame-stream-id
                :frame-payload)
  (:import-from :woo.http2.constants
                :+frame-data+
                :+frame-headers+
                :+frame-settings+
                :+frame-window-update+
                :+frame-ping+
                :+frame-goaway+
                :+frame-rst-stream+
                :+no-error+))
(in-package :woo-test.diff)

(defun hex-encode (bytes)
  (with-output-to-string (out)
    (loop for b across bytes
          do (format out "~2,'0X" b))))

(defun hex-decode (text)
  (let* ((n (length text))
         (out (make-array (/ n 2) :element-type '(unsigned-byte 8))))
    (loop for i from 0 below n by 2
          for j from 0
          do (setf (aref out j)
                   (parse-integer text :start i :end (+ i 2) :radix 16)))
    out))

(defun string-bytes (s)
  (trivial-utf-8:string-to-utf-8-bytes s))

;; The Go reference binary is built per test into a unique temp directory from
;; a copy of t/diff/{go.mod,go.sum,refh2.go}, so the source tree is never
;; written and concurrent runs never share a binary. Knobs:
;;   WOO_SKIP_GO=1             skip (reported as a rove skip, not a pass)
;;   WOO_GO_BUILD_TIMEOUT=<s>  build budget in seconds (default 240)
;; `go` missing from PATH, a toolchain too old for go.mod, or modules that
;; cannot be downloaded (offline) also skip, with the reason. A compile error
;; in refh2.go, or a build that times out without downloading, fails.

(defvar *woo-root*
  (let ((here (or *load-truename* *compile-file-truename*)))
    (dotimes (i 2)
      (setf here (uiop:pathname-parent-directory-pathname here)))
    here))

(defun refh2-dir ()
  (merge-pathnames "t/diff/"
                   (or (ignore-errors (asdf:system-source-directory :woo))
                       *woo-root*)))

(defun env-seconds (name default)
  (let ((env (ignore-errors (uiop:getenv name))))
    (or (and env (plusp (length env))
             (let ((n (parse-integer env :junk-allowed t)))
               (and n (plusp n) n)))
        default)))

(defun run-bounded (argv dir &key directory input (timeout 60))
  "Run ARGV with stdout+stderr captured to files in DIR, killing it after
   TIMEOUT seconds. Returns (values status exit-code output) where STATUS is
   :exited or :timeout. Signals an error if ARGV cannot be started."
  (let* ((tag (format nil "~36R" (random (expt 36 8))))
         (out (merge-pathnames (format nil "out-~A.txt" tag) dir))
         (err (merge-pathnames (format nil "err-~A.txt" tag) dir))
         (in (and input (merge-pathnames (format nil "in-~A.txt" tag) dir))))
    (when in
      (with-open-file (s in :direction :output :if-exists :supersede)
        (write-string input s)))
    (let* ((proc (uiop:launch-program argv
                                      :directory directory
                                      :input (or in nil)
                                      :output out
                                      :if-output-exists :supersede
                                      :error-output err
                                      :if-error-output-exists :supersede))
           (deadline (+ (get-internal-real-time)
                        (* timeout internal-time-units-per-second)))
           (status :exited))
      (loop while (uiop:process-alive-p proc)
            do (when (> (get-internal-real-time) deadline)
                 (setf status :timeout)
                 (ignore-errors (uiop:terminate-process proc :urgent t))
                 (return))
               (sleep 0.05))
      (let ((code (uiop:wait-process proc)))
        (values status code
                (concatenate 'string
                             (or (ignore-errors (uiop:read-file-string out)) "")
                             (or (ignore-errors (uiop:read-file-string err)) "")))))))

(defun go-skip-reason (dir)
  "NIL when `go` can be used, else a string saying why the Go differential
   tests are skipped."
  (when (equal (ignore-errors (uiop:getenv "WOO_SKIP_GO")) "1")
    (return-from go-skip-reason "WOO_SKIP_GO=1"))
  (handler-case
      (multiple-value-bind (status code)
          (run-bounded '("go" "version") dir :timeout 30)
        (unless (and (eq status :exited) (eql code 0))
          "`go version` did not succeed"))
    (error ()
      "`go` is not on PATH")))

(defun any-search (needles haystack)
  (some (lambda (n) (search n haystack)) needles))

(defparameter *offline-markers*
  '("dial tcp" "no such host" "i/o timeout" "connection refused"
    "network is unreachable" "TLS handshake" "GOPROXY=off"
    "module lookup disabled" "Temporary failure in name resolution"))

(defparameter *old-toolchain-markers*
  '("requires go >=" "GOTOOLCHAIN=local"))

(defun build-refh2 (dir)
  "Build refh2 into DIR. Returns (values :ok binary) or (values :skip reason).
   A real build failure signals an error, which fails the test."
  (dolist (name '("go.mod" "go.sum" "refh2.go"))
    (uiop:copy-file (merge-pathnames name (refh2-dir))
                    (merge-pathnames name dir)))
  (let ((bin (merge-pathnames "woo-refh2" dir))
        (timeout (env-seconds "WOO_GO_BUILD_TIMEOUT" 240)))
    (multiple-value-bind (status code output)
        (run-bounded (list "env" "GOTOOLCHAIN=local" "GOFLAGS=-mod=mod"
                           "go" "build" "-o" (uiop:native-namestring bin) ".")
                     dir :directory dir :timeout timeout)
      (cond
        ((and (eq status :exited) (eql code 0) (probe-file bin))
         (values :ok bin))
        ((and (eq status :timeout) (search "go: downloading" output))
         (values :skip (format nil "go build timed out after ~As while downloading modules (offline?)" timeout)))
        ((eq status :timeout)
         (error "go build timed out after ~As:~%~A" timeout output))
        ((any-search *offline-markers* output)
         (values :skip (format nil "Go modules could not be downloaded (offline?): ~A"
                               (string-trim '(#\Newline #\Space) output))))
        ((search "refh2.go:" output)
         (error "refh2.go failed to compile:~%~A" output))
        ((every (lambda (m) (search m output)) *old-toolchain-markers*)
         (values :skip (format nil "local Go toolchain too old for t/diff/go.mod: ~A"
                               (string-trim '(#\Newline #\Space) output))))
        (t
         (error "go build failed (exit ~A):~%~A" code output))))))

(defvar *refh2-bin* nil
  "Binary used by REFH2; bound by WITH-REFH2.")

(defvar *refh2-dir* nil)

(defmacro with-refh2 (&body body)
  "Build refh2 in a fresh temp directory and run BODY with it, or record a
   rove skip that names the reason. The directory is always removed."
  (let ((dir (gensym "DIR"))
        (reason (gensym "REASON"))
        (status (gensym "STATUS"))
        (bin (gensym "BIN")))
    `(woo-test.prop:with-unique-temp-directory (,dir "refh2")
       (let ((,reason (go-skip-reason ,dir)))
         (if ,reason
             (skip (format nil "Go differential skipped: ~A" ,reason))
             (multiple-value-bind (,status ,bin) (build-refh2 ,dir)
               (if (eq ,status :skip)
                   (skip (format nil "Go differential skipped: ~A" ,bin))
                   (let ((*refh2-bin* ,bin)
                         (*refh2-dir* ,dir))
                     ,@body))))))))

(defun refh2 (lines)
  "Send LINES to refh2 and return its answers. Signals unless the process
   exits cleanly within the timeout."
  (let ((input (with-output-to-string (out)
                 (dolist (line lines)
                   (write-line line out)))))
    (multiple-value-bind (status code output)
        (run-bounded (list (uiop:native-namestring *refh2-bin*)) *refh2-dir*
                     :input input :timeout 60)
      (unless (and (eq status :exited) (eql code 0))
        (error "refh2 ~A (exit ~A): ~A" status code output))
      (loop for line in (uiop:split-string output :separator '(#\Newline))
            unless (zerop (length line))
              collect line))))

(defun refh2-answers (lines)
  "REFH2, plus an assertion that there is exactly one answer per input, so a
   dropped answer can never be skipped by the pairing loops below."
  (let ((answers (refh2 lines)))
    (ok (= (length answers) (length lines))
        (format nil "Go answered ~A of ~A requests" (length answers) (length lines)))
    answers))

(defun hex-field (bytes)
  (if (zerop (length bytes)) "-" (hex-encode bytes)))

(defun unhex-field (text)
  (if (string= text "-")
      (make-array 0 :element-type '(unsigned-byte 8))
      (hex-decode text)))

(defun fields-text (pairs)
  (format nil "~{~A~^ ~}"
          (loop for (name . value) in pairs
                collect (hex-field (string-bytes name))
                collect (hex-field (string-bytes value)))))

(defun field-line (pairs)
  (format nil "ENC ~A" (fields-text pairs)))

(defun woo-decode (bytes)
  (handler-case
      (hpack-decode-headers (make-hpack-context) bytes)
    (hpack-compression-error () :err)))

(defun pairs-match (expected got)
  (and (listp got)
       (= (length expected) (length got))
       (loop for e in expected
             for g in got
             always (and (string= (car e) (car g))
                         (string= (cdr e) (cdr g))))))

(defun decode-field-tokens (tokens)
  (loop for rest on tokens by #'cddr
        collect (cons (map 'string #'code-char (unhex-field (first rest)))
                      (map 'string #'code-char (unhex-field (or (second rest) ""))))))

(defun go-decode-line (line)
  (let ((parts (uiop:split-string line :separator '(#\Space))))
    (cond
      ((and (string= (first parts) "DEC") (equal (second parts) "ERR"))
       :err)
      ((string= (first parts) "DEC")
       (decode-field-tokens (rest parts)))
      (t :bad))))

(defparameter *hpack-cases*
  (list '(("custom-key" . "custom-header"))
        '((":method" . "GET"))
        '(("a" . "b") ("c" . "d"))
        '(("www.example.com" . "/") ("accept" . "text/plain"))
        '(("x-empty" . "") ("accept" . "text/plain"))))

(deftest differential-hpack-against-go
  (with-refh2
    (let* ((cases *hpack-cases*)
           (enc-lines (mapcar #'field-line cases))
           (go-blocks (refh2-answers enc-lines)))
      (loop for pairs in cases
            for line in go-blocks
            do (ok (and (>= (length line) 4) (string= (subseq line 0 4) "ENC "))
                   line)
               (let* ((bytes (unhex-field (subseq line 4)))
                      (got (woo-decode bytes)))
                 (ok (pairs-match pairs got)
                     (format nil "Woo decodes Go's HPACK block as ~S, want ~S" got pairs))))
      (let* ((woo-lines
               (loop for pairs in cases
                     collect (format nil "DEC ~A"
                                     (hex-field
                                      (hpack-encode-headers (make-hpack-context) pairs)))))
             (decoded (refh2-answers woo-lines)))
        (loop for pairs in cases
              for line in decoded
              for got = (go-decode-line line)
              do (ok (pairs-match pairs got)
                     (format nil "Go decodes Woo's HPACK block as ~S, want ~S" got pairs)))))))

;;; Stateful HPACK: one encoder/decoder context shared across several header
;;; blocks, so the dynamic table left by block N decides how block N+1 reads.

(defparameter *hpack-sequence*
  (list '(("custom-key" . "custom-header") ("a" . "b"))
        '(("custom-key" . "custom-header") ("c" . "d"))
        '(("a" . "b") ("custom-key" . "custom-header") ("c" . "d"))))

(defun ub8 (&rest parts)
  (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
         (mapcar (lambda (p) (coerce p '(simple-array (unsigned-byte 8) (*)))) parts)))

(defun literal-incremental (name value)
  "RFC 7541 6.2.1: literal with incremental indexing, new name, no Huffman."
  (ub8 (woo.http2.hpack::hpack-encode-integer 0 6 #x40)
       (woo.http2.hpack::hpack-encode-string name)
       (woo.http2.hpack::hpack-encode-string value)))

(defun indexed (index)
  "RFC 7541 6.1: indexed header field."
  (ub8 (woo.http2.hpack::hpack-encode-integer index 7 #x80)))

(defun woo-stateful-blocks ()
  "Header blocks that only decode correctly with the dynamic table kept
   across blocks: block 2 and 3 name entries added by earlier blocks.
   Returns (values blocks expected-header-lists)."
  (values
   (list (ub8 (literal-incremental "custom-key" "custom-header")
              (literal-incremental "x-a" "1"))
         ;; 62 = newest (x-a), 63 = custom-key.
         (ub8 (indexed 63) (indexed 62)
              (literal-incremental "x-b" "2"))
         ;; Now 62 = x-b, 63 = x-a, 64 = custom-key.
         (ub8 (indexed 64) (indexed 62) (indexed 63) (indexed 2)))
   (list '(("custom-key" . "custom-header") ("x-a" . "1"))
         '(("custom-key" . "custom-header") ("x-a" . "1") ("x-b" . "2"))
         '(("custom-key" . "custom-header") ("x-b" . "2") ("x-a" . "1")
           (":method" . "GET")))))

(defun go-seq-blocks (line)
  "Parse DECSEQ output into a list of header lists, or :err / :bad."
  (let ((parts (uiop:split-string line :separator '(#\Space))))
    (cond
      ((not (string= (first parts) "DECSEQ")) :bad)
      ((equal (second parts) "ERR") :err)
      (t
       (let ((groups (list nil)))
         (dolist (tok (rest parts))
           (if (string= tok "/")
               (push nil groups)
               (push tok (first groups))))
         (mapcar (lambda (g) (decode-field-tokens (reverse g)))
                 (reverse groups)))))))

(deftest differential-hpack-dynamic-table-against-go
  (with-refh2
    ;; Go encodes a sequence with one encoder; Woo decodes it with one context.
    (let* ((line (format nil "ENCSEQ ~{~A~^ / ~}"
                         (mapcar #'fields-text *hpack-sequence*)))
           (answer (first (refh2-answers (list line))))
           (parts (uiop:split-string answer :separator '(#\Space)))
           (blocks (mapcar #'unhex-field (rest parts)))
           (ctx (make-hpack-context)))
      (ok (and (string= (first parts) "ENCSEQ")
               (= (length blocks) (length *hpack-sequence*)))
          answer)
      (loop for pairs in *hpack-sequence*
            for block in blocks
            for i from 1
            for got = (handler-case (hpack-decode-headers ctx block)
                        (hpack-compression-error () :err))
            do (ok (pairs-match pairs got)
                   (format nil "Woo shared context decodes Go block ~A as ~S, want ~S"
                           i got pairs)))
      ;; Go's later blocks must actually lean on the dynamic table, or this
      ;; test would not be comparing table state at all.
      (let ((fresh (unhex-field
                    (subseq (first (refh2-answers
                                    (list (field-line (second *hpack-sequence*)))))
                            4))))
        (ok (< (length (second blocks)) (length fresh))
            (format nil "Go block 2 uses the dynamic table (~A bytes vs ~A fresh)"
                    (length (second blocks)) (length fresh))))
      ;; Both sides agree on what the dynamic table holds afterwards:
      ;; newest first, as Go's encoder inserted them.
      (ok (equal (map 'list (lambda (e) (cons (car e) (cdr e)))
                      (woo.http2.hpack:hpack-context-dynamic-table ctx))
                 '(("c" . "d") ("a" . "b") ("custom-key" . "custom-header")))
          (format nil "Woo dynamic table after Go's sequence: ~S"
                  (woo.http2.hpack:hpack-context-dynamic-table ctx))))
    ;; Woo-built blocks that index earlier blocks; Go decodes with one decoder.
    (multiple-value-bind (blocks expected) (woo-stateful-blocks)
      (let* ((line (format nil "DECSEQ ~{~A~^ ~}" (mapcar #'hex-field blocks)))
             (got (go-seq-blocks (first (refh2-answers (list line)))))
             (ctx (make-hpack-context)))
        (ok (and (listp got) (= (length got) (length expected)))
            (format nil "Go decoded ~S" got))
        (when (listp got)
          (loop for want in expected
                for g in got
                for i from 1
                do (ok (pairs-match want g)
                       (format nil "Go shared decoder reads Woo block ~A as ~S, want ~S"
                               i g want))))
        (loop for want in expected
              for block in blocks
              for i from 1
              do (ok (pairs-match want (hpack-decode-headers ctx block))
                     (format nil "Woo shared context reads its own block ~A" i)))))))

(defun frame-line (frame)
  (format nil "FRM ~A" (hex-encode (serialize-frame frame))))

(defun woo-frame-desc (bytes)
  "(type flags stream-id length payload) as Woo parses BYTES."
  (multiple-value-bind (frame consumed) (parse-frame bytes)
    (declare (ignore consumed))
    (when frame
      (list (frame-type frame)
            (frame-flags frame)
            (frame-stream-id frame)
            (length (frame-payload frame))
            (coerce (frame-payload frame) 'list)))))

(defun octets (&rest bytes)
  (coerce bytes '(simple-array (unsigned-byte 8) (*))))

(deftest differential-frames-against-go
  (with-refh2
    (let* ((frames (list (make-data-frame 1 (octets 1 2 3) :end-stream t)
                         (make-data-frame 3 (string-bytes "hello, differential world"))
                         (make-headers-frame 1 (octets) :end-headers t :end-stream t)
                         (make-headers-frame 5 (hpack-encode-headers
                                                (make-hpack-context)
                                                '((":method" . "GET") (":path" . "/x")))
                                             :end-headers t)
                         (make-settings-frame nil)
                         (make-settings-frame '((1 . 4096) (3 . 100) (4 . 65535)))
                         (make-settings-ack-frame)
                         (make-window-update-frame 0 128)
                         (make-window-update-frame 1 10)
                         (make-ping-frame (octets 1 2 3 4 5 6 7 8))
                         (make-ping-ack-frame (octets 8 7 6 5 4 3 2 1))
                         (make-goaway-frame 1 +no-error+)
                         (make-goaway-frame 7 2 (string-bytes "debug"))
                         (make-rst-stream-frame 1 +no-error+)
                         (make-rst-stream-frame 9 8)))
           (lines (mapcar #'frame-line frames))
           (answers (refh2-answers lines)))
      (loop for frame in frames
            for bytes = (serialize-frame frame)
            for woo = (woo-frame-desc bytes)
            for line in answers
            do (ok woo)
               (let ((parts (uiop:split-string line :separator '(#\Space))))
                 (ok (and (string= (first parts) "FRM") (equal (second parts) "OK")
                          (= (length parts) 7))
                     line)
                 (when (and woo (equal (second parts) "OK") (= (length parts) 7))
                   (ok (equal woo
                              (list (parse-integer (third parts))
                                    (parse-integer (fourth parts))
                                    (parse-integer (fifth parts))
                                    (parse-integer (sixth parts))
                                    (coerce (unhex-field (seventh parts)) 'list)))
                       (format nil "frame fields+payload Woo ~S Go ~S" woo line))))))))
