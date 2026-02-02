;;; local-storage-sexp-tests.lisp - S-expression IO tests

(in-package :swimmy.tests)

(deftest test-sexp-io-roundtrip
  (let* ((tmp (merge-pathnames "sexp_io_test.sexp" #P"/tmp/"))
         (payload '((schema_version . 1) (name . "X") (value . 42))))
    (unwind-protect
        (progn
          (swimmy.core:write-sexp-atomic tmp payload)
          (let ((read (swimmy.core:read-sexp-file tmp :package :swimmy.school)))
            (assert-equal payload read)))
      (when (probe-file tmp) (delete-file tmp)))))

(deftest test-backtest-cache-sexp
  (let* ((tmp (merge-pathnames "backtest_cache.sexp" #P"/tmp/"))
         (swimmy.school::*backtest-cache-file* (namestring tmp))
         (swimmy.school::*backtest-cache* (make-hash-table :test 'equal)))
    (setf (gethash "StratA" swimmy.school::*backtest-cache*)
          (list :timestamp 123 :result '(:sharpe 1.0 :trades 10)))
    (swimmy.school::save-backtest-cache)
    (let* ((sexp (swimmy.core:read-sexp-file tmp :package :swimmy.school))
           (schema (and sexp (find-if (lambda (pair)
                                        (and (consp pair)
                                             (string-equal "SCHEMA_VERSION" (symbol-name (car pair)))))
                                      sexp))))
      (assert-true schema "Expected schema_version in S-expression file")
      (assert-equal 1 (cdr schema)))
    (clrhash swimmy.school::*backtest-cache*)
    (swimmy.school::load-backtest-cache)
    (let ((cached (gethash "StratA" swimmy.school::*backtest-cache*)))
      (assert-true cached)
      (assert-equal 1.0 (getf (getf cached :result) :sharpe)))
    (when (probe-file tmp) (delete-file tmp))))
