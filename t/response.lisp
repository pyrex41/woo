(in-package :cl-user)
(defpackage woo-test.response
  (:use :cl
        :rove))
(in-package :woo-test.response)

;;; Unit tests for the response status-line table.
;;;
;;; Two things are under test and they are easy to conflate:
;;;
;;;   `status-code-to-text'  -- a pure code -> reason-phrase function.
;;;   `*status-line*'        -- a hash table of code -> encoded status-line
;;;                             bytes, precomputed at load time by looping
;;;                             over a fixed code range.
;;;
;;; A code can be present in the first and absent from the second if it falls
;;; outside that loop's bounds. That is a real bug this file guards against:
;;; 511 was unreachable because the loop stopped at 510.

(defparameter *loop-lower-bound* 100)
(defparameter *loop-upper-bound* 599
  "Upper bound of the range `*status-line*' is built over, inclusive.
Must track the loop in src/response.lisp.")

(defun status-line-string (code)
  "Decoded status line for CODE, or NIL if not registered."
  (let ((bytes (gethash code woo.response::*status-line*)))
    (when bytes
      (map 'string #'code-char bytes))))

(defun reason-phrase (code)
  (woo.response::status-code-to-text code))

;;; ---------------------------------------------------------------------------

(deftest status-code-to-text-tests
  (testing "returns the reason phrase for known codes"
    (ok (equal (reason-phrase 200) "OK"))
    (ok (equal (reason-phrase 404) "Not Found"))
    (ok (equal (reason-phrase 500) "Internal Server Error")))

  (testing "returns NIL for unassigned codes"
    ;; 306 is reserved/unused; the others are simply not assigned.
    (ok (null (reason-phrase 306)))
    (ok (null (reason-phrase 419)))
    (ok (null (reason-phrase 499)))
    (ok (null (reason-phrase 599))))

  (testing "returns NIL outside the HTTP status range"
    (ok (null (reason-phrase 99)))
    (ok (null (reason-phrase 600)))))

(deftest status-line-format-tests
  (testing "a status line is HTTP/1.1 SP code SP reason CRLF"
    (ok (equal (status-line-string 200)
               (format nil "HTTP/1.1 200 OK~C~C" #\Return #\Linefeed)))
    (ok (equal (status-line-string 404)
               (format nil "HTTP/1.1 404 Not Found~C~C" #\Return #\Linefeed))))

  (testing "every registered status line is well formed"
    (let ((malformed '()))
      (maphash (lambda (code bytes)
                 (let ((line (map 'string #'code-char bytes)))
                   (unless (equal line
                                  (format nil "HTTP/1.1 ~D ~A~C~C"
                                          code (or (reason-phrase code) "")
                                          #\Return #\Linefeed))
                     (push code malformed))))
               woo.response::*status-line*)
      (ok (null malformed)
          (format nil "malformed status lines for: ~S" malformed))))

  (testing "status lines are octet vectors, not strings"
    ;; The write path runs under (safety 0) and passes these straight to
    ;; write-socket-data; a non-octet vector would corrupt output rather
    ;; than signal.
    (let ((wrong-type '()))
      (maphash (lambda (code bytes)
                 (unless (typep bytes '(simple-array (unsigned-byte 8) (*)))
                   (push code wrong-type)))
               woo.response::*status-line*)
      (ok (null wrong-type)
          (format nil "non-octet status lines for: ~S" wrong-type)))))

(deftest status-line-table-consistency-tests
  ;; This is the structural test: it ties the two representations together.
  ;; It fails if `status-code-to-text' knows a code that the precomputed
  ;; table does not -- which is exactly how 511 was broken.
  (testing "every code with a reason phrase is registered in *status-line*"
    (let ((unregistered '()))
      (loop for code from *loop-lower-bound* to *loop-upper-bound*
            when (and (reason-phrase code)
                      (null (gethash code woo.response::*status-line*)))
              do (push code unregistered))
      (ok (null unregistered)
          (format nil "codes with a reason phrase but no status line: ~S"
                  unregistered))))

  (testing "every registered code has a reason phrase"
    (let ((phraseless '()))
      (maphash (lambda (code bytes)
                 (declare (ignore bytes))
                 (unless (or (>= code 200) (reason-phrase code))
                   (push code phraseless)))
               woo.response::*status-line*)
      (ok (null phraseless)
          (format nil "registered codes with no reason phrase: ~S" phraseless))))

  (testing "no reason phrase exists beyond the loop's upper bound"
    ;; If this fails someone added a code above the bound and it is
    ;; silently unreachable -- widen the loop in src/response.lisp.
    (let ((beyond '()))
      (loop for code from (1+ *loop-upper-bound*) to 599
            when (reason-phrase code)
              do (push code beyond))
      (ok (null beyond)
          (format nil "reason phrases outside the built range: ~S" beyond)))))

(deftest rfc-6585-and-8470-status-tests
  ;; Regression tests for the codes that were missing.
  ;; Include #127 alongside #130.
  (testing "RFC 6585 codes are present"
    (ok (equal (reason-phrase 429) "Too Many Requests"))
    (ok (equal (reason-phrase 428) "Precondition Required"))
    (ok (equal (reason-phrase 431) "Request Header Fields Too Large"))
    (ok (equal (reason-phrase 511) "Network Authentication Required")))

  (testing "RFC 8470 425 Too Early is present"
    (ok (equal (reason-phrase 425) "Too Early")))

  (testing "they render as complete status lines"
    (dolist (code '(425 428 429 431 511))
      (ok (status-line-string code)
          (format nil "~D has a status line" code))))

  (testing "511 specifically, as it sits on the loop's upper bound"
    (ok (equal (status-line-string 511)
               (format nil "HTTP/1.1 511 Network Authentication Required~C~C"
                       #\Return #\Linefeed)))))

(deftest common-status-coverage-tests
  (testing "the codes a typical app returns are all registered"
    (dolist (code '(200 201 204 206
                    301 302 303 304 307 308
                    400 401 403 404 405 409 410 413 415 422
                    500 501 502 503 504))
      (ok (status-line-string code)
          (format nil "~D is registered" code)))))

(deftest final-status-wire-range
  (let ((clack.test:*clack-test-handler* :woo) (clack.test:*enable-debug* nil))
    (clack.test:testing-app "All final HTTP statuses reach the wire"
        (lambda (env)
          (list (parse-integer (getf env :path-info) :start 1) nil nil))
      (let ((failures nil))
        (loop for code from 200 to 599
              for request = (woo-test::crlf-lines
                             (format nil "GET /~D HTTP/1.1" code) "Host: localhost"
                             "Connection: close" "")
              for response = (woo-test::raw-exchange clack.test:*clack-test-port* request)
              unless (uiop:string-prefix-p (woo.response::http/1.1 code) response)
                do (push code failures))
        (ok (null failures) (format nil "bad final statuses: ~S" failures))))))

(deftest malformed-response-is-contained
  (let ((clack.test:*clack-test-handler* :woo) (clack.test:*enable-debug* nil))
    (dolist (response '((99 nil nil) (600 nil nil) (200 (:bad) nil)
                        (200 (:content-type "bad
header") nil)
                        (200 nil ("ok" 42)) (200 nil #("bad")) (200 (:content-length "oops") nil)
                        (200 (:content-length 0 :content-length 1) nil)
                        (200 (:content-length 0 :transfer-encoding "chunked") nil)
                        (200 (:content-length 1) ("too much"))
                        (200 (:content-length 99) ("short"))
                        (200 (:content-length 1) #(1 2 3))))
      (clack.test:testing-app "Malformed response receives one 500"
          (lambda (env) (declare (ignore env)) response)
        (let ((result (woo-test::raw-exchange clack.test:*clack-test-port*
                        (woo-test::crlf-lines "GET / HTTP/1.1" "Host: localhost"
                                               "Connection: close" ""))))
          (ok (uiop:string-prefix-p "HTTP/1.1 500 " result))
          (ok (not (search "HTTP/1.1 " result :start2 1)))))))
  (ok (equal (woo::validate-response '(200 nil "hello")) '(200 nil ("hello"))))
  (ok (equal (woo::validate-response '(200 nil)) '(200 nil))))

(deftest worker-random-bindings-are-private
  (let* ((a (cdr (assoc '*random-state* (woo.specials:default-thread-bindings))))
         (b (cdr (assoc '*random-state* (woo.specials:default-thread-bindings))))
         (caller (make-random-state *random-state*)))
    (ok (typep a 'random-state)) (ok (typep b 'random-state))
    (ok (not (eq a b))) (ok (not (eq a *random-state*)))
    (let ((*random-state* a)) (dotimes (i 100) (random 1000000)))
    (ok (= (random 1000000 caller) (random 1000000 (make-random-state *random-state*))))))

(deftest empty-status-framing
  (let ((clack.test:*clack-test-handler* :woo) (clack.test:*enable-debug* nil))
    (dolist (code '(204 205 304))
      (clack.test:testing-app "Bodyless statuses ignore application payload"
          (lambda (env) (declare (ignore env)) (list code nil '("must-not-send")))
        (let ((response (woo-test::raw-exchange clack.test:*clack-test-port*
                         (woo-test::crlf-lines "GET / HTTP/1.1" "Host: localhost"
                                                "Connection: close" ""))))
          (ok (not (search "must-not-send" response)))
          (when (= code 204) (ok (not (search "Content-Length:" response))))
          (when (= code 205) (ok (search "Content-Length: 0" response))))))))

(deftest response-framing-and-header-storage
  (ok (equal (woo::validate-response '(200 (:content-length 2) ("λ")))
             '(200 (:content-length 2) ("λ"))))
  (ok (equal (woo::validate-response '(200 (:content-length 0) nil))
             '(200 (:content-length 0) nil)))
  (ok (equal (woo::validate-response '(200 (:content-length 10) nil) t)
             '(200 (:content-length 10) nil)))
  (let* ((key (format nil "x-woo-test-~A" (gensym)))
         (normalized (second (woo::validate-response (list 200 (list key "ok") nil)))))
    (ok (stringp (first normalized)))
    (ok (null (find-symbol (string-upcase key) :keyword))))
  (ok (null (getf (second (woo::validate-response '(200 (:transfer-encoding "chunked") ("ok"))))
                 :transfer-encoding))))

(deftest legacy-head-persistent-pipeline-and-utf8-length
  (let ((clack.test:*clack-test-handler* :woo)
        (clack.test:*enable-debug* nil)
        (utf8 (list "λ" "日本")))
    (clack.test:testing-app "HEAD preserves metadata and pipeline has no HEAD body"
        (lambda (env)
          (cond
            ((string= (getf env :path-info) "/meta")
             (if (eq (getf env :request-method) :head)
                 '(200 (:content-length 100) nil)
                 (list 200 '(:content-length 100) (list (make-string 100 :initial-element #\a)))))
            ((string= (getf env :path-info) "/utf8") (list 200 nil utf8))
            (t '(404 nil nil))))
      (let* ((head (woo-test::crlf-lines "HEAD /meta HTTP/1.1" "Host: localhost" ""))
             (get (woo-test::crlf-lines "GET /meta HTTP/1.1" "Host: localhost" "Connection: close" ""))
             (wire (woo-test::raw-exchange clack.test:*clack-test-port* head get))
             (first-end (search (format nil "~C~C~C~C" #\Return #\Linefeed #\Return #\Linefeed) wire))
             (second-status (and first-end (search "HTTP/1.1 200" wire :start2 (+ first-end 4)))))
        (ok first-end)
        (ok (search "content-length: 100" (subseq wire 0 first-end) :test #'char-equal))
        (ok (and second-status (= second-status (+ first-end 4)))
            "the pipelined GET starts immediately after HEAD headers")
        (ok (= 2 (loop with start = 0
                       for pos = (search "HTTP/1.1 200" wire :start2 start)
                       while pos count 1 do (setf start (+ pos 1)))))
        (let* ((utf8-wire (woo-test::raw-exchange clack.test:*clack-test-port*
                                                 (woo-test::crlf-lines "GET /utf8 HTTP/1.0"
                                                                       "Host: localhost" "Connection: close" "")))
               (get-end (search (format nil "~C~C~C~C" #\Return #\Linefeed #\Return #\Linefeed) utf8-wire))
               (expected (map 'string #'code-char (trivial-utf-8:string-to-utf-8-bytes (apply #'concatenate 'string utf8)))))
          (ok (and get-end (string= (subseq utf8-wire (+ get-end 4)) expected)))
          (let* ((head-wire (woo-test::raw-exchange clack.test:*clack-test-port*
                                                   (woo-test::crlf-lines "HEAD /utf8 HTTP/1.1" "Host: localhost" "Connection: close" "")))
                 (head-end (search (format nil "~C~C~C~C" #\Return #\Linefeed #\Return #\Linefeed) head-wire)))
            (ok (and head-end (= (length head-wire) (+ head-end 4)))))
          (ok (search (format nil "content-length: ~D"
                              (reduce #'+ utf8 :key #'trivial-utf-8:utf-8-byte-length))
                       (woo-test::raw-exchange clack.test:*clack-test-port*
                                               (woo-test::crlf-lines "HEAD /utf8 HTTP/1.1"
                                                                     "Host: localhost" "Connection: close" ""))
                       :test #'char-equal)))))))

(deftest legacy-head-pathname-plain-no-payload
  (let* ((root (merge-pathnames (format nil "woo-head-private-~D/" (random 1000000000)) (uiop:temporary-directory)))
         (path (merge-pathnames "body.txt" root)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :error)
      (write-string "pathname body" s))
    (unwind-protect
         (let ((clack.test:*clack-test-handler* :woo)
               (clack.test:*enable-debug* nil))
           (clack.test:testing-app "pathname HEAD has no payload"
               (lambda (env) (list 200 nil path))
             (let* ((head (woo-test::crlf-lines "HEAD / HTTP/1.1" "Host: localhost" ""))
                    (get (woo-test::crlf-lines "GET / HTTP/1.1" "Host: localhost" "Connection: close" ""))
                    (wire (woo-test::raw-exchange clack.test:*clack-test-port* head get))
                    (first-end (search (format nil "~C~C~C~C" #\Return #\Linefeed #\Return #\Linefeed) wire))
                    (second-start (and first-end (+ first-end 4)))
                    (second-end (and second-start (search (format nil "~C~C~C~C" #\Return #\Linefeed #\Return #\Linefeed) wire :start2 second-start))))
               (ok (search "content-length: 13" wire :test #'char-equal))
               (ok (and second-start (= second-start (or (search "HTTP/1.1 200" wire :start2 second-start) -1))))
               (ok (and second-end (string= (subseq wire (+ second-end 4)) "pathname body"))))))
      (ignore-errors (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))))

#-woo-no-ssl
(deftest legacy-head-pathname-tls-no-payload
  (let* ((path (merge-pathnames (format nil "woo-head-tls-~D.txt" (random 1000000))
                                (uiop:temporary-directory))))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :error)
      (write-string "tls pathname" s))
    (unwind-protect
         (let ((clack.test:*clack-test-handler* :woo)
               (clack.test:*enable-debug* nil)
               (clack.test:*use-https* t)
               (clack.test:*clackup-additional-args*
                 (list :ssl-cert-file (asdf:system-relative-pathname :woo-test "t/certs/localhost.crt")
                       :ssl-key-file (asdf:system-relative-pathname :woo-test "t/certs/localhost.key")))
               (dex:*not-verify-ssl* t))
           (clack.test:testing-app "TLS pathname HEAD has no payload"
               (lambda (env) (list 200 nil path))
             (multiple-value-bind (body status headers)
                 (dex:request (format nil "https://127.0.0.1:~D/" clack.test:*clack-test-port*)
                              :method :head :keep-alive nil :force-string t)
               (ok (= status 200))
               (ok (zerop (length body)))
               (ok (string= (gethash "content-length" headers) "12")))
             (ok (string= (dex:get (format nil "https://127.0.0.1:~D/" clack.test:*clack-test-port*)
                                  :keep-alive nil :force-string t)
                         "tls pathname")))
      (ignore-errors (delete-file path))))))
