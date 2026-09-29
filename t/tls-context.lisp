(in-package :cl-user)
(defpackage woo-test.tls-stream
  (:use :cl :rove))
(in-package :woo-test.tls-stream)

(deftest tls-alpn-setup-failure-releases-listener-context
  (testing "a failure after ALPN setup releases the listener context"
    (let ((create (symbol-function 'woo.ssl:create-context))
          (configure (symbol-function 'woo.ssl:configure-context-alpn))
          (free (symbol-function 'woo.ssl:free-context))
          (context nil)
          (configured nil)
          (freed 0)
          (baseline (hash-table-count woo.ssl.alpn::*alpn-ctx-args*)))
      (unwind-protect
           (progn
             (setf (symbol-function 'woo.ssl:create-context)
                   (lambda (&rest args)
                     (setf context (apply create args)))
                   (symbol-function 'woo.ssl:configure-context-alpn)
                   (lambda (&rest args)
                     (apply configure args)
                     (setf configured t)
                     (error "Injected ALPN setup failure"))
                   (symbol-function 'woo.ssl:free-context)
                   (lambda (ctx)
                     (funcall free ctx)
                     (incf freed)
                     (setf context nil)))
             (ok (signals
                   (woo:run (lambda (env)
                              (declare (ignore env))
                              '(200 nil ("ok")))
                            :debug nil
                            :worker-num nil
                            :ssl-cert-file #P"t/certs/localhost.crt"
                            :ssl-key-file #P"t/certs/localhost.key")
                   'error))
             (ok configured)
             (ok (= freed 1))
             (ok (= baseline (hash-table-count woo.ssl.alpn::*alpn-ctx-args*))))
        (setf (symbol-function 'woo.ssl:create-context) create
              (symbol-function 'woo.ssl:configure-context-alpn) configure
              (symbol-function 'woo.ssl:free-context) free)
        (when context (funcall free context))))))

