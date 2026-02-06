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

(deftest test-write-sexp-atomic-stable-defaults
  "write-sexp-atomic should not depend on *default-pathname-defaults* mid-call"
  (let* ((orig-rename (symbol-function 'cl:rename-file))
         (orig-default *default-pathname-defaults*)
         (root (uiop:ensure-directory-pathname (uiop:getcwd)))
         (path "data/tmp/atomic_defaults_test.sexp")
         (full (merge-pathnames path root))
         (hit nil))
    (unwind-protect
        (progn
          (sb-ext:without-package-locks
            (setf (symbol-function 'cl:rename-file)
                  (lambda (from to &rest args)
                    (declare (ignore args))
                    (setf hit t)
                    ;; Simulate a cwd/default change right before rename.
                    (setf *default-pathname-defaults* (merge-pathnames "data/" root))
                    (funcall orig-rename from to))))
          (setf *default-pathname-defaults* root)
          (swimmy.core:write-sexp-atomic path '((schema_version . 1)))
          (assert-true hit "rename-file should be called")
          (assert-true (probe-file full) "Expected target file to exist"))
      (sb-ext:without-package-locks
        (setf (symbol-function 'cl:rename-file) orig-rename))
      (setf *default-pathname-defaults* orig-default)
      (ignore-errors (when (probe-file full) (delete-file full)))
      (ignore-errors
        (let ((tmp (make-pathname :name (format nil "~a.tmp" (pathname-name full))
                                  :type (pathname-type full)
                                  :defaults full)))
          (when (probe-file tmp) (delete-file tmp)))))))

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

(deftest test-telemetry-sexp
  (let* ((tmp (merge-pathnames "system_metrics.sexp" #P"/tmp/"))
         (swimmy.school::*telemetry-file* (namestring tmp)))
    (swimmy.school::save-telemetry-sexp (list :heap 1 :strategy-count 2))
    (let* ((sexp (swimmy.core:read-sexp-file tmp :package :swimmy.school))
           (schema (and sexp (find-if (lambda (pair)
                                        (and (consp pair)
                                             (string-equal "SCHEMA_VERSION" (symbol-name (car pair)))))
                                      sexp))))
      (assert-true schema "Expected schema_version in telemetry S-expression"))
    (when (probe-file tmp) (delete-file tmp))))

(deftest test-live-status-sexp
  (let* ((tmp (merge-pathnames "live_status.sexp" #P"/tmp/"))
         (swimmy.shell::*live-status-path* (namestring tmp)))
    (swimmy.shell::save-live-status)
    (let* ((sexp (swimmy.core:read-sexp-file tmp :package :swimmy.main))
           (schema (and sexp (find-if (lambda (pair)
                                        (and (consp pair)
                                             (string-equal "SCHEMA_VERSION" (symbol-name (car pair)))))
                                      sexp)))
           (daily (and sexp (find-if (lambda (pair)
                                       (and (consp pair)
                                            (string-equal "DAILY_PNL" (symbol-name (car pair)))))
                                     sexp))))
      (assert-true schema "Expected schema_version in live status S-expression")
      (assert-true daily "Expected daily_pnl in live status S-expression"))
    (when (probe-file tmp) (delete-file tmp))))
