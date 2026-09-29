(in-package :cl-user)
(defpackage woo-test.worker
  (:use :cl
        :rove))
(in-package :woo-test.worker)

#+sbcl
(deftest legacy-worker-random-bindings-are-private
  (testing "actual legacy workers receive distinct random states"
    (let ((lock (bt2:make-lock))
          (done (bt2:make-semaphore :count 0))
          (states nil)
          (cluster nil))
      (unwind-protect
           (progn
             (setf cluster
                   (woo.worker:make-cluster
                    2
                    (lambda (ignored)
                      (declare (ignore ignored))
                      (bt2:with-lock-held (lock)
                        (push *random-state* states))
                      (bt2:signal-semaphore done))))
             (woo.worker:add-job-to-cluster cluster nil)
             (woo.worker:add-job-to-cluster cluster nil)
             (ok (bt2:wait-on-semaphore done :timeout 5))
             (ok (bt2:wait-on-semaphore done :timeout 5))
             (ok (= 2 (length (remove-duplicates states :test #'eq))))
             (ok (not (member *random-state* states :test #'eq))))
        (when cluster
          (woo.worker:stop-cluster cluster))))))
