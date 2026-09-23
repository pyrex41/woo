(in-package :cl-user)
(defpackage woo-test.http2-e2e
  (:use :cl :rove))
(in-package :woo-test.http2-e2e)

;;; End-to-end HTTP/2: a real woo server running a Lack-style app, driven by
;;; clients that share no code with woo: curl (nghttp2) and Node's http2.
;;; h2c with prior knowledge, and TLS with ALPN h2. A missing client is a
;;; visible rove skip, not a silent pass.

(defparameter *client-timeout-seconds* 30
  "Upper bound for any one client process.")

(defparameter *large-size* (+ (* 1024 1024) 300001)
  "A response body over 1 MB, many times the initial flow-control window.")

(defparameter *file-size* 400000)

(defun large-body ()
  (let ((bytes (make-array *large-size* :element-type '(unsigned-byte 8))))
    (dotimes (i *large-size* bytes)
      (setf (aref bytes i) (mod i 251)))))

(defun file-pattern (size)
  (let ((bytes (make-array size :element-type '(unsigned-byte 8))))
    (dotimes (i size bytes)
      (setf (aref bytes i) (mod (* i 7) 256)))))

(defun run-client (program &rest args)
  "Run PROGRAM with ARGS, killed after *client-timeout-seconds*.
   Returns (values stdout stderr exit-code)."
  (multiple-value-bind (out err code)
      (uiop:run-program (list* "perl" "-e" "alarm shift @ARGV; exec @ARGV or exit 127"
                               (princ-to-string *client-timeout-seconds*)
                               program args)
                        :output :string :error-output :string
                        :ignore-error-status t)
    (values out err code)))

(defun curl-http2-p ()
  (ignore-errors
   (multiple-value-bind (out err code) (run-client "curl" "--version")
     (declare (ignore err))
     (and (eql code 0) (search "HTTP2" out) t))))

(defun node-http2-p ()
  (ignore-errors
   (multiple-value-bind (out err code)
       (run-client "node" "-e" "require('http2'); console.log(process.versions.node)")
     (declare (ignore err))
     (and (eql code 0) (plusp (length (string-trim '(#\Newline) out)))))))

(defun octets-string (octets)
  (map 'string #'code-char octets))

(defun read-file-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((buf (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence buf in)
      buf)))

(defun temp-path (name)
  (merge-pathnames (format nil "woo-h2-e2e-~A-~D" name (random 1000000))
                   (uiop:temporary-directory)))

;;; The application: plain Lack conventions, lack.request for the body.

(defun make-e2e-app (file-path)
  (let ((large (large-body)))
    (lambda (env)
      (let ((path (getf env :path-info)))
        (cond
          ((string= path "/hello")
           `(200 (:content-type "text/plain" :x-custom "yes")
                 (,(format nil "hello ~A" (or (getf env :query-string) "")))))
          ((string= path "/form")
           (let* ((req (lack.request:make-request env))
                  (params (lack.request:request-body-parameters req)))
             `(200 (:content-type "text/plain")
                   (,(format nil "~A=~A;n=~A ~A"
                             "name"
                             (cdr (assoc "name" params :test #'string=))
                             (cdr (assoc "n" params :test #'string=))
                             (getf env :request-method))))))
          ((string= path "/large")
           `(200 (:content-type "application/octet-stream") ,large))
          ((string= path "/stream")
           (lambda (responder)
             (let ((writer (funcall responder '(200 (:content-type "text/plain")))))
               (bt2:make-thread
                (lambda ()
                  (dotimes (i 5)
                    (sleep 0.03)
                    (funcall writer (format nil "chunk~D;" i)))
                  (funcall writer nil :close t))
                :name "woo-e2e streaming writer"))))
          ((string= path "/file")
           `(200 () ,file-path))
          ((string= path "/trailers")
           (let ((req (lack.request:make-request env)))
             `(200 (:content-type "text/plain")
                   (,(format nil "trailer=~A;body=~A"
                             (cdr (assoc "x-checksum" (getf env :http2.trailers)
                                         :test #'string=))
                             (octets-string (lack.request:request-content req)))))))
          ((string= path "/hop")
           ;; Connection-specific fields are not valid in HTTP/2 and strict
           ;; clients reject the response if they are sent.
           '(200 (:connection "keep-alive" :keep-alive "timeout=5"
                  :transfer-encoding "chunked" :upgrade "h2c" :x-ok "1")
             ("hop")))
          (t '(404 (:content-type "text/plain") ("not found"))))))))

(defmacro with-e2e-server ((&key tls worker-num) &body body)
  "Run BODY against a woo server on clack.test:*clack-test-port*. FILE-PATH
   and EXPECTED-FILE are bound for the pathname response."
  `(let* ((file-path (temp-path "file"))
          (expected-file (file-pattern *file-size*))
          (clack.test:*clack-test-handler* :woo)
          (clack.test:*clackup-additional-args*
            (append (and ,tls
                         (list :ssl-cert-file (cert-path "localhost.crt")
                               :ssl-key-file (cert-path "localhost.key")))
                    (and ,worker-num (list :worker-num ,worker-num))))
          ;; The server runs in its own thread; make it advertise h2 there.
          (bt2:*default-special-bindings*
            (if ,tls
                (acons (find-symbol "*ALPN-PROTOCOLS*" :woo.ssl) ''("h2" "http/1.1")
                       bt2:*default-special-bindings*)
                bt2:*default-special-bindings*)))
     (declare (ignorable expected-file))
     (with-open-file (out file-path :direction :output :if-exists :supersede
                                    :element-type '(unsigned-byte 8))
       (write-sequence expected-file out))
     (unwind-protect
          (clack.test:testing-app ,(format nil "woo ~:[h2c~;TLS h2~]~@[ worker-num ~D~]"
                                           tls worker-num)
              (make-e2e-app file-path)
            ,@body)
       (when (probe-file file-path)
         (delete-file file-path)))))

(defun cert-path (name)
  (asdf:system-relative-pathname :woo-test (format nil "t/certs/~A" name)))

(defun base-url (tls)
  (format nil "~:[http~;https~]://127.0.0.1:~D" tls clack.test:*clack-test-port*))

;;; curl

(defun curl (tls &rest args)
  (apply #'run-client "curl" "-sS" "--max-time" "20"
         (append (if tls (list "--http2" "-k") (list "--http2-prior-knowledge"))
                 args)))

(defun check-with-curl (tls)
  (let ((base (base-url tls)))
    (multiple-value-bind (out err code)
        (curl tls "-D" "-" (concatenate 'string base "/hello?x=1"))
      (ok (eql code 0) (format nil "curl GET exit ~A ~A" code err))
      (ok (search "HTTP/2 200" out) "curl GET: HTTP/2 200")
      (ok (search "x-custom: yes" out) "curl GET: response header")
      (ok (search "hello x=1" out) "curl GET: body"))
    (multiple-value-bind (out err code)
        (curl tls "-d" "name=Ada+Lovelace&n=3" (concatenate 'string base "/form"))
      (ok (eql code 0) (format nil "curl POST exit ~A ~A" code err))
      (ok (equal out "name=Ada Lovelace;n=3 POST")
          (format nil "curl POST: form params through lack.request ~S" out)))
    (let ((dest (temp-path "large")))
      (unwind-protect
           (multiple-value-bind (out err code)
               (curl tls "-o" (namestring dest) "-w" "%{http_version}"
                     (concatenate 'string base "/large"))
             (ok (eql code 0) (format nil "curl large exit ~A ~A" code err))
             (ok (equal out "2") "curl large: HTTP/2")
             (ok (and (probe-file dest) (equalp (read-file-octets dest) (large-body)))
                 "curl large: all 1.3 MB arrive intact"))
        (when (probe-file dest) (delete-file dest))))
    (multiple-value-bind (out err code)
        (curl tls (concatenate 'string base "/stream"))
      (ok (eql code 0) (format nil "curl stream exit ~A ~A" code err))
      (ok (equal out "chunk0;chunk1;chunk2;chunk3;chunk4;")
          (format nil "curl stream: chunks written from another thread ~S" out)))
    (let ((dest (temp-path "file-dl")))
      (unwind-protect
           (multiple-value-bind (out err code)
               (curl tls "-o" (namestring dest) "-w" "%{http_code}"
                     (concatenate 'string base "/file"))
             (ok (eql code 0) (format nil "curl file exit ~A ~A" code err))
             (ok (equal out "200"))
             (ok (and (probe-file dest)
                      (equalp (read-file-octets dest) (file-pattern *file-size*)))
                 "curl file: the pathname body arrives intact"))
        (when (probe-file dest) (delete-file dest))))
    (multiple-value-bind (out err code)
        (curl tls "-D" "-" (concatenate 'string base "/hop"))
      (ok (eql code 0) (format nil "curl hop exit ~A ~A" code err))
      (ok (search "x-ok: 1" out))
      (ok (not (search "keep-alive" out)) "curl hop: no connection-specific fields"))
    (let ((urls (loop for i below 20
                      collect (format nil "~A/hello?i=~D" base i))))
      (multiple-value-bind (out err code)
          (apply #'curl tls "--parallel" "--parallel-max" "20"
                 "-w" "%{http_code} %{num_connects}\\n"
                 (loop for url in urls append (list "-o" "/dev/null" url)))
        (let ((lines (remove "" (uiop:split-string out :separator '(#\Newline))
                             :test #'string=)))
          (ok (eql code 0) (format nil "curl parallel exit ~A ~A" code err))
          (ok (= (length lines) 20))
          (ok (every (lambda (l) (eql 0 (search "200 " l))) lines) "all 200")
          (ok (= 1 (reduce #'+ lines :key (lambda (l) (parse-integer l :start 4 :junk-allowed t))))
              (format nil "curl parallel: 20 streams on one connection ~S" lines)))))))

;;; Node's http2

(defparameter *node-client* "
const http2 = require('http2');
const url = process.argv[1];
const largeSize = Number(process.argv[2]);
const fileSize = Number(process.argv[3]);
const session = http2.connect(url, {rejectUnauthorized: false});
session.on('error', e => { console.log('session.error ' + String(e).replace(/\\n/g, ' ')); process.exit(2); });
function req(headers, body, opts) {
  return new Promise(resolve => {
    const r = session.request(headers, opts || {});
    const chunks = [];
    let h = {};
    r.on('response', x => { h = x; });
    r.on('data', c => chunks.push(c));
    r.on('end', () => resolve({h, body: Buffer.concat(chunks), n: chunks.length}));
    r.on('error', e => resolve({h, error: String(e), body: Buffer.alloc(0)}));
    if (opts && opts.waitForTrailers) r.on('wantTrailers', () => r.sendTrailers({'x-checksum': 'abc123'}));
    if (body !== undefined) r.end(body); else r.end();
  });
}
function out(k, v) { console.log(k + ' ' + String(v).replace(/\\n/g, ' ')); }
(async () => {
  const hello = await req({':path': '/hello?n=1'});
  out('hello.status', hello.h[':status']);
  out('hello.custom', hello.h['x-custom']);
  out('hello.body', hello.body.toString());
  const form = await req({':method': 'POST', ':path': '/form',
                          'content-type': 'application/x-www-form-urlencoded'},
                         'name=Ada+Lovelace&n=3');
  out('form.body', form.body.toString());
  const large = await req({':path': '/large'});
  let okLarge = large.body.length === largeSize;
  for (let i = 0; okLarge && i < large.body.length; i++) if (large.body[i] !== i % 251) okLarge = false;
  out('large.ok', okLarge);
  out('large.length', large.body.length);
  const stream = await req({':path': '/stream'});
  out('stream.body', stream.body.toString());
  out('stream.chunks', stream.n);
  const file = await req({':path': '/file'});
  let okFile = file.body.length === fileSize;
  for (let i = 0; okFile && i < file.body.length; i++) if (file.body[i] !== (i * 7) % 256) okFile = false;
  out('file.ok', okFile);
  out('file.length', file.h['content-length']);
  const trailers = await req({':method': 'POST', ':path': '/trailers'}, 'abc', {waitForTrailers: true});
  out('trailers.body', trailers.body.toString());
  const hop = await req({':path': '/hop'});
  out('hop.status', hop.h[':status']);
  out('hop.error', hop.error || 'none');
  out('hop.connection', hop.h['connection'] || 'none');
  const many = await Promise.all(Array.from({length: 50}, (_, i) => req({':path': '/hello?i=' + i})));
  out('many.ok', many.every((r, i) => r.h[':status'] === 200 && r.body.toString() === 'hello i=' + i));
  out('many.count', many.length);
  session.close();
})().catch(e => { out('fatal', e); process.exit(3); });
")

(defun node-results (tls)
  "Run the Node client once. Returns an alist of (key . value) and the exit code."
  (multiple-value-bind (out err code)
      (run-client "node" "-e" *node-client* (base-url tls)
                  (princ-to-string *large-size*) (princ-to-string *file-size*))
    (values
     (loop for line in (uiop:split-string out :separator '(#\Newline))
           for space = (position #\Space line)
           when space
             collect (cons (subseq line 0 space) (subseq line (1+ space))))
     code
     err)))

(defun check-with-node (tls)
  (multiple-value-bind (results code err) (node-results tls)
    (flet ((val (key) (cdr (assoc key results :test #'string=))))
      (ok (eql code 0) (format nil "node exit ~A ~A ~A" code err (val "session.error")))
      (ok (equal (val "hello.status") "200") "node GET: 200")
      (ok (equal (val "hello.custom") "yes") "node GET: response header")
      (ok (equal (val "hello.body") "hello n=1") "node GET: body")
      (ok (equal (val "form.body") "name=Ada Lovelace;n=3 POST")
          (format nil "node POST: form params through lack.request ~S" (val "form.body")))
      (ok (equal (val "large.ok") "true")
          (format nil "node large: ~A octets intact" (val "large.length")))
      (ok (equal (val "stream.body") "chunk0;chunk1;chunk2;chunk3;chunk4;")
          (format nil "node stream: ~S in ~A chunks" (val "stream.body") (val "stream.chunks")))
      (ok (equal (val "file.ok") "true") "node file: pathname body intact")
      (ok (equal (val "file.length") (princ-to-string *file-size*)) "node file: content-length")
      (ok (equal (val "trailers.body") "trailer=abc123;body=abc")
          (format nil "node trailers reach the app ~S" (val "trailers.body")))
      (ok (and (equal (val "hop.status") "200") (equal (val "hop.error") "none"))
          (format nil "node hop: accepted, error ~A" (val "hop.error")))
      (ok (equal (val "many.ok") "true")
          (format nil "node: ~A concurrent streams on one session" (val "many.count"))))))

(deftest http2-e2e-h2c
  (with-e2e-server ()
    (if (curl-http2-p)
        (check-with-curl nil)
        (skip "curl with HTTP/2 support not found; curl h2c checks skipped"))
    (if (node-http2-p)
        (check-with-node nil)
        (skip "node with the http2 module not found; node h2c checks skipped"))))

(deftest http2-e2e-h2c-workers
  (with-e2e-server (:worker-num 2)
    (if (node-http2-p)
        (check-with-node nil)
        (skip "node with the http2 module not found; worker checks skipped"))))

#-woo-no-ssl
(deftest http2-e2e-tls
  (with-e2e-server (:tls t)
    (if (curl-http2-p)
        (check-with-curl t)
        (skip "curl with HTTP/2 support not found; curl TLS checks skipped"))
    (if (node-http2-p)
        (check-with-node t)
        (skip "node with the http2 module not found; node TLS checks skipped"))))
