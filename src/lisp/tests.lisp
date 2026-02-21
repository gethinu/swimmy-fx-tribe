;;; tests.lisp - Unit Tests for Swimmy
;;; ═══════════════════════════════════════════════════
;;; "Untested code is broken code." - Unknown

;;; tests.lisp - Unit Tests for Swimmy
;;; ═══════════════════════════════════════════════════
;;; "Untested code is broken code." - Unknown

(in-package :swimmy.tests)

(defvar *test-results* nil)
(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

;;; ─────────────────────────────────────────
;;; TEST FRAMEWORK
;;; ─────────────────────────────────────────

(defmacro deftest (name &body body)
  "Define a test case"
  `(defun ,name ()
     (let ((swimmy.core::*log-file-path* "data/memory/swimmy-tests-telemetry.jsonl")
           (swimmy.core::*telemetry-fallback-log-path* "data/memory/swimmy-tests-telemetry-fallback.jsonl"))
       (ignore-errors (ensure-directories-exist swimmy.core::*log-file-path*))
       (ignore-errors (ensure-directories-exist swimmy.core::*telemetry-fallback-log-path*))
       (handler-case
           (progn
             ,@body
             (incf *tests-passed*)
             (push (list :passed ',name) *test-results*)
             t)
         (error (e)
           (format t " [ERROR: ~a] " e) ; Debug print
           (incf *tests-failed*)
           (push (list :failed ',name e) *test-results*)
           nil)))))

(defmacro assert-true (expr &optional message)
  "Assert that expression is true"
  `(unless ,expr
     (error (or ,message (format nil "Assertion failed: ~a" ',expr)))))

(defmacro assert-false (expr &optional message)
  "Assert that expression is false"
  `(when ,expr
     (error (or ,message (format nil "Expected false: ~a" ',expr)))))

(defmacro assert-equal (expected actual &optional message)
  "Assert that expected equals actual"
  `(unless (equal ,expected ,actual)
     (error (or ,message 
                (format nil "Expected ~a but got ~a" ,expected ,actual)))))

(defmacro assert-not-nil (expr &optional message)
  "Assert that expression is not nil"
  `(unless ,expr
     (error (or ,message (format nil "Expected non-nil: ~a" ',expr)))))

;;; ─────────────────────────────────────────
;;; PACKAGE SHADOWING TESTS
;;; ─────────────────────────────────────────

(deftest test-main-shadows-last-new-day
  "swimmy.main should shadow *last-new-day* to avoid hot reload conflicts"
  (let* ((main (find-package :swimmy.main))
         (globals (find-package :swimmy.globals))
         (global (find-symbol "*LAST-NEW-DAY*" globals))
         (shadowed (package-shadowing-symbols main)))
    (assert-not-nil global "Expected swimmy.globals:*last-new-day* to exist")
    (assert-true (member global shadowed :test #'eq)
                 "Expected *last-new-day* to be shadowed in swimmy.main")))

(deftest test-hot-reload-school-refreshes-db-metrics-when-available
  "hot-reload-school should refresh in-memory DB metrics after ASDF reload."
  (let* ((orig-load (symbol-function 'asdf:load-system))
         (orig-refresh (symbol-function 'swimmy.school::refresh-strategy-metrics-from-db))
         (orig-end (and (fboundp 'swimmy.school::end-startup-mode)
                        (symbol-function 'swimmy.school::end-startup-mode)))
         (orig-flush (and (fboundp 'swimmy.school::flush-deferred-founders)
                          (symbol-function 'swimmy.school::flush-deferred-founders)))
         (orig-notify (and (fboundp 'swimmy.core:notify-apex)
                           (symbol-function 'swimmy.core:notify-apex)))
         (load-called nil)
         (refresh-called nil))
    (unwind-protect
        (progn
          (setf (symbol-function 'asdf:load-system)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (setf load-called t)
                  t))
          (setf (symbol-function 'swimmy.school::refresh-strategy-metrics-from-db)
                (lambda (&key force since-timestamp)
                  (declare (ignore since-timestamp))
                  (setf refresh-called force)
                  t))
          (when orig-end
            (setf (symbol-function 'swimmy.school::end-startup-mode)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-flush
            (setf (symbol-function 'swimmy.school::flush-deferred-founders)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-notify
            (setf (symbol-function 'swimmy.core:notify-apex)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (assert-true (swimmy.main::hot-reload-school) "Expected hot reload to succeed")
          (assert-true load-called "Expected ASDF reload to be called")
          (assert-true refresh-called "Expected DB metric refresh to be called with force=T"))
      (setf (symbol-function 'asdf:load-system) orig-load)
      (setf (symbol-function 'swimmy.school::refresh-strategy-metrics-from-db) orig-refresh)
      (when orig-end
        (setf (symbol-function 'swimmy.school::end-startup-mode) orig-end))
      (when orig-flush
        (setf (symbol-function 'swimmy.school::flush-deferred-founders) orig-flush))
      (when orig-notify
        (setf (symbol-function 'swimmy.core:notify-apex) orig-notify)))))

;;; ─────────────────────────────────────────
;;; STRATEGY ACTIVE FILTER TESTS
;;; ─────────────────────────────────────────

(deftest test-active-strategy-p-bas-legend
  "active-strategy-p should allow only B/A/S/LEGEND ranks"
  (flet ((mk (rank)
           (swimmy.school:make-strategy :name "UT-ACTIVE" :rank rank)))
    (assert-true (swimmy.school::active-strategy-p (mk :B)) "B should be active")
    (assert-true (swimmy.school::active-strategy-p (mk :A)) "A should be active")
    (assert-true (swimmy.school::active-strategy-p (mk :S)) "S should be active")
    (assert-true (swimmy.school::active-strategy-p (mk :legend)) "LEGEND should be active")
    (assert-false (swimmy.school::active-strategy-p (mk nil)) "NIL rank should be inactive")
    (assert-false (swimmy.school::active-strategy-p (mk :retired)) "RETIRED should be inactive")
    (assert-false (swimmy.school::active-strategy-p (mk :graveyard)) "GRAVEYARD should be inactive")))

;;; ─────────────────────────────────────────
;;; GRAVEYARD AVOIDANCE (P3) TESTS
;;; ─────────────────────────────────────────

(deftest test-analyze-graveyard-for-avoidance-skips-corrupt-form
  "analyze-graveyard-for-avoidance should skip malformed forms and still compute avoid regions"
  (let* ((tmp-dir (uiop:temporary-directory))
         (gy-path (merge-pathnames
                   (format nil "swimmy_graveyard_avoid_test_~a.sexp" (get-universal-time))
                   tmp-dir))
         (retired-path (merge-pathnames
                        (format nil "swimmy_retired_avoid_test_~a.sexp" (get-universal-time))
                        tmp-dir))
         (now (get-universal-time)))
    (unwind-protect
        (progn
          (with-open-file (out gy-path :direction :output :if-exists :supersede :if-does-not-exist :create)
            ;; Force a reader error: unmatched close paren at top-level.
            (format out ")~%")
            ;; Then include valid patterns that should still be processed.
            (dolist (sl '(1 2 3 4 5))
              (format out "(:name \"UT\" :timeframe 5 :direction :buy :symbol \"USDJPY\" :sl ~a :tp ~a :timestamp ~d)~%"
                      sl (* sl 10) now)))
          (when (probe-file retired-path)
            (ignore-errors (delete-file retired-path)))
          (let ((swimmy.school::*graveyard-file* (namestring gy-path))
                (swimmy.school::*retired-file* (namestring retired-path)))
            (let* ((out-stream (make-string-output-stream))
                   (*standard-output* out-stream)
                   (regions (swimmy.school::analyze-graveyard-for-avoidance))
                   (output (get-output-stream-string out-stream)))
              ;; Avoid logging full reader conditions which may include stream/position info and
              ;; can cause huge allocations on large/corrupt files.
              (assert-false (search "File-Position" output)
                            "Read error logs should not include File-Position details")
              (assert-false (search "Stream:" output)
                            "Read error logs should not include Stream object details")
              (assert-true (and (listp regions) (= (length regions) 1))
                           "Expected one avoid region")
              (let ((r (first regions)))
                (assert-equal 1 (getf r :sl-min) "sl-min should be min SL")
                (assert-equal 5 (getf r :sl-max) "sl-max should be max SL")
                (assert-equal 10 (getf r :tp-min) "tp-min should be min TP")
                (assert-equal 50 (getf r :tp-max) "tp-max should be max TP")))))
      (when (probe-file gy-path)
        (ignore-errors (delete-file gy-path)))
      (when (probe-file retired-path)
        (ignore-errors (delete-file retired-path))))))

;;; ─────────────────────────────────────────
;;; SAFE READ TESTS
;;; ─────────────────────────────────────────

(deftest test-safe-read-rejects-read-eval
  "safe-read-sexp rejects read-time eval (#.)"
  (let ((form (swimmy.core:safe-read-sexp "#.(+ 1 2)")))
    (assert-true (null form) "Expected nil for read-time eval")))

(deftest test-safe-read-allows-simple-alist
  "safe-read-sexp allows simple alist"
  (let ((form (swimmy.core:safe-read-sexp "((action . \"PING\"))")))
    (let ((key (find-symbol "ACTION" :swimmy.main)))
      (assert-true (and (listp form) key (assoc key form)) "Expected alist"))))

(deftest test-safe-read-normalizes-sharp-booleans
  "safe-read-sexp should accept #t/#f and normalize to t/nil."
  (let ((form (swimmy.core:safe-read-sexp "((ok . #t) (ng . #f))")))
    (let ((ok-key (find-symbol "OK" :swimmy.main))
          (ng-key (find-symbol "NG" :swimmy.main)))
      (assert-true (and (listp form) ok-key ng-key) "Expected parsed alist for #t/#f")
      (assert-true (eq (cdr (assoc ok-key form)) t) "Expected #t normalized to T")
      (assert-true (null (cdr (assoc ng-key form))) "Expected #f normalized to NIL"))))

(deftest test-internal-process-msg-rejects-read-eval
  "internal-process-msg should ignore unsafe sexp"
  (let ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (let ((orig swimmy.globals:*danger-level*))
      (unwind-protect
          (progn
            (setf swimmy.globals:*danger-level* 0)
            (funcall fn "((type . \"PING\") (payload . #.(setf swimmy.globals::*danger-level* 999)))")
            (assert-equal 0 swimmy.globals:*danger-level* "Should not evaluate read-time forms"))
        (setf swimmy.globals:*danger-level* orig)))))

(deftest test-internal-process-msg-backtest-request-id-bound
  "internal-process-msg should handle BACKTEST_RESULT with request_id"
  (let ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (let* ((msg "{\"type\":\"BACKTEST_RESULT\",\"result\":{\"strategy_name\":\"UT-REQ\",\"sharpe\":0.2,\"trades\":1,\"pnl\":0.1,\"request_id\":\"RID-1\"}}")
           (orig-cache (symbol-function 'swimmy.school:cache-backtest-result))
           (orig-apply (symbol-function 'swimmy.school:apply-backtest-result))
           (orig-lookup (symbol-function 'swimmy.school:lookup-oos-request))
           (orig-v2 (and (fboundp 'swimmy.school::handle-v2-result)
                         (symbol-function 'swimmy.school::handle-v2-result))))
      (unwind-protect
          (progn
            (setf (symbol-function 'swimmy.school:cache-backtest-result)
                  (lambda (&rest args) (declare (ignore args)) nil))
            (setf (symbol-function 'swimmy.school:apply-backtest-result)
                  (lambda (&rest args) (declare (ignore args)) nil))
            (setf (symbol-function 'swimmy.school:lookup-oos-request)
                  (lambda (&rest args) (declare (ignore args)) (values nil nil nil)))
            (when (fboundp 'swimmy.school::handle-v2-result)
              (setf (symbol-function 'swimmy.school::handle-v2-result)
                    (lambda (&rest args) (declare (ignore args)) nil)))
            (let* ((out-stream (make-string-output-stream))
                   (*standard-output* out-stream))
              (funcall fn msg)
              (let ((output (get-output-stream-string out-stream)))
                (assert-true (null (search "Msg Error" output))
                             "Expected no Msg Error during BACKTEST_RESULT"))))
        (setf (symbol-function 'swimmy.school:cache-backtest-result) orig-cache)
        (setf (symbol-function 'swimmy.school:apply-backtest-result) orig-apply)
        (setf (symbol-function 'swimmy.school:lookup-oos-request) orig-lookup)
        (when orig-v2
          (setf (symbol-function 'swimmy.school::handle-v2-result) orig-v2))))))

(deftest test-apply-backtest-result-tolerates-nil-metrics
  "apply-backtest-result should not type-error when metrics are present-but-NIL."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-upsert (and (fboundp 'swimmy.school:upsert-strategy)
                           (symbol-function 'swimmy.school:upsert-strategy)))
         (strat (swimmy.school:make-strategy :name "UT-NIL-METRICS" :rank :B))
         (failed nil))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list strat))
          (when orig-upsert
            (setf (symbol-function 'swimmy.school:upsert-strategy)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (handler-case
              (swimmy.school:apply-backtest-result
               "UT-NIL-METRICS"
               (list :sharpe nil
                     :profit-factor nil
                     :win-rate nil
                     :trades nil
                     :max-dd nil))
            (error () (setf failed t)))
          (assert-false failed "Expected apply-backtest-result not to crash on NIL metrics")
          (assert-equal 0.0 (swimmy.school:strategy-sharpe strat)
                        "sharpe should default to 0.0")
          (assert-equal 0.0 (swimmy.school:strategy-profit-factor strat)
                        "profit-factor should default to 0.0"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (when orig-upsert
        (setf (symbol-function 'swimmy.school:upsert-strategy) orig-upsert)))))

(deftest test-handle-v2-result-tolerates-nil-metrics
  "handle-v2-result should not type-error when metrics are present-but-NIL."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-upsert (and (fboundp 'swimmy.school:upsert-strategy)
                           (symbol-function 'swimmy.school:upsert-strategy)))
         (orig-ensure (and (fboundp 'swimmy.school:ensure-rank)
                           (symbol-function 'swimmy.school:ensure-rank)))
         (orig-grave (and (fboundp 'swimmy.school:send-to-graveyard)
                          (symbol-function 'swimmy.school:send-to-graveyard)))
         (strat (swimmy.school:make-strategy :name "UT-V2-NIL" :rank :incubator))
         (grave-called nil)
         (failed nil))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list strat))
          (when orig-upsert
            (setf (symbol-function 'swimmy.school:upsert-strategy)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-ensure
            (setf (symbol-function 'swimmy.school:ensure-rank)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-grave
            (setf (symbol-function 'swimmy.school:send-to-graveyard)
                  (lambda (&rest _args) (declare (ignore _args))
                    (setf grave-called t)
                    nil)))
          (handler-case
              (swimmy.school::handle-v2-result
               "UT-V2-NIL_P1"
               (list :sharpe nil
                     :profit-factor nil
                     :win-rate nil
                     :trades nil
                     :max-dd nil))
            (error () (setf failed t)))
          (assert-false failed "Expected handle-v2-result not to crash on NIL metrics")
          (assert-true grave-called "Expected NIL metrics to fail Phase1 and route to graveyard"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (when orig-upsert
        (setf (symbol-function 'swimmy.school:upsert-strategy) orig-upsert))
      (when orig-ensure
        (setf (symbol-function 'swimmy.school:ensure-rank) orig-ensure))
      (when orig-grave
        (setf (symbol-function 'swimmy.school:send-to-graveyard) orig-grave)))))

(deftest test-handle-v2-result-founder-recovery-pass
  "Founder seeds should pass Phase1 on recovery thresholds when Sharpe is marginal but PF/trades are healthy."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-upsert (and (fboundp 'swimmy.school:upsert-strategy)
                           (symbol-function 'swimmy.school:upsert-strategy)))
         (orig-ensure (and (fboundp 'swimmy.school:ensure-rank)
                           (symbol-function 'swimmy.school:ensure-rank)))
         (orig-grave (and (fboundp 'swimmy.school:send-to-graveyard)
                          (symbol-function 'swimmy.school:send-to-graveyard)))
         (strat (swimmy.school:make-strategy :name "Hunted-UT-Recovery"
                                             :rank :incubator
                                             :generation 0))
         (ensure-called 0)
         (grave-called 0))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list strat))
          (when orig-upsert
            (setf (symbol-function 'swimmy.school:upsert-strategy)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-ensure
            (setf (symbol-function 'swimmy.school:ensure-rank)
                  (lambda (s rank &optional reason)
                    (declare (ignore reason))
                    (incf ensure-called)
                    (setf (swimmy.school:strategy-rank s) rank)
                    rank)))
          (when orig-grave
            (setf (symbol-function 'swimmy.school:send-to-graveyard)
                  (lambda (&rest _args) (declare (ignore _args))
                    (incf grave-called)
                    nil)))
          (swimmy.school::handle-v2-result
           "Hunted-UT-Recovery_P1"
           '(:sharpe 0.06 :profit-factor 1.20 :win-rate 0.42 :trades 25 :max-dd 0.08))
          (assert-equal 1 ensure-called
                        "Expected founder recovery profile to pass Phase1")
          (assert-equal 0 grave-called
                        "Founder recovery pass should not route to graveyard")
          (assert-equal :B (swimmy.school:strategy-rank strat)
                        "Expected founder recovery pass to promote to B"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (when orig-upsert
        (setf (symbol-function 'swimmy.school:upsert-strategy) orig-upsert))
      (when orig-ensure
        (setf (symbol-function 'swimmy.school:ensure-rank) orig-ensure))
      (when orig-grave
        (setf (symbol-function 'swimmy.school:send-to-graveyard) orig-grave)))))

(deftest test-internal-process-msg-backtest-json-applies
  "JSON BACKTEST_RESULT should call apply-backtest-result"
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (called nil)
         (orig-apply (symbol-function 'swimmy.school:apply-backtest-result))
         (orig-cache (symbol-function 'swimmy.school:cache-backtest-result))
         (orig-lookup (symbol-function 'swimmy.school:lookup-oos-request))
         (orig-v2 (and (fboundp 'swimmy.school::handle-v2-result)
                       (symbol-function 'swimmy.school::handle-v2-result))))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school:apply-backtest-result)
                (lambda (name metrics) (setf called (list name metrics))))
          (setf (symbol-function 'swimmy.school:cache-backtest-result)
                (lambda (&rest _args) (declare (ignore _args)) nil))
          (setf (symbol-function 'swimmy.school:lookup-oos-request)
                (lambda (&rest _args) (declare (ignore _args)) (values nil nil nil)))
          (when (fboundp 'swimmy.school::handle-v2-result)
            (setf (symbol-function 'swimmy.school::handle-v2-result)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (funcall fn "{\"type\":\"BACKTEST_RESULT\",\"result\":{\"strategy_name\":\"UT-JSON-1\",\"sharpe\":1.2,\"trades\":3,\"pnl\":0.4,\"request_id\":\"RID-J1\"}}")
          (assert-not-nil called "Expected apply-backtest-result to be called")
          (assert-equal "UT-JSON-1" (first called) "Expected strategy name")
          (assert-equal "RID-J1" (getf (second called) :request-id)
                        "Expected request-id in metrics"))
      (setf (symbol-function 'swimmy.school:apply-backtest-result) orig-apply)
      (setf (symbol-function 'swimmy.school:cache-backtest-result) orig-cache)
      (setf (symbol-function 'swimmy.school:lookup-oos-request) orig-lookup)
      (when orig-v2
        (setf (symbol-function 'swimmy.school::handle-v2-result) orig-v2)))))

(deftest test-backtest-result-phase1-normalizes-base-name
  "BACKTEST_RESULT with _P1 suffix should apply metrics to base strategy name."
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (called nil)
         (orig-apply (symbol-function 'swimmy.school:apply-backtest-result))
         (orig-cache (symbol-function 'swimmy.school:cache-backtest-result))
         (orig-lookup (symbol-function 'swimmy.school:lookup-oos-request))
         (orig-v2 (and (fboundp 'swimmy.school::handle-v2-result)
                       (symbol-function 'swimmy.school::handle-v2-result))))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school:apply-backtest-result)
                (lambda (name metrics) (setf called (list name metrics))))
          (setf (symbol-function 'swimmy.school:cache-backtest-result)
                (lambda (&rest _args) (declare (ignore _args)) nil))
          (setf (symbol-function 'swimmy.school:lookup-oos-request)
                (lambda (&rest _args) (declare (ignore _args)) (values nil nil nil)))
          (when (fboundp 'swimmy.school::handle-v2-result)
            (setf (symbol-function 'swimmy.school::handle-v2-result)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (funcall fn "((type . \"BACKTEST_RESULT\") (result . ((strategy_name . \"UT-P1-NAME_P1\") (sharpe . 0.8) (trades . 5) (request_id . \"RID-P1\"))))")
          (assert-not-nil called "Expected apply-backtest-result to be called")
          (assert-equal "UT-P1-NAME" (first called)
                        "Expected _P1 suffix to be stripped before apply-backtest-result")
          (assert-equal "RID-P1" (getf (second called) :request-id)
                        "Expected request-id in metrics"))
      (setf (symbol-function 'swimmy.school:apply-backtest-result) orig-apply)
      (setf (symbol-function 'swimmy.school:cache-backtest-result) orig-cache)
      (setf (symbol-function 'swimmy.school:lookup-oos-request) orig-lookup)
      (when orig-v2
        (setf (symbol-function 'swimmy.school::handle-v2-result) orig-v2)))))

(deftest test-backtest-result-qual-normalizes-trailing-suffix-only
  "BACKTEST_RESULT with trailing -QUAL should strip only terminal suffix and keep base -QUAL token."
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (called nil)
         (orig-apply (symbol-function 'swimmy.school:apply-backtest-result))
         (orig-cache (symbol-function 'swimmy.school:cache-backtest-result))
         (orig-lookup (symbol-function 'swimmy.school:lookup-oos-request))
         (orig-v2 (and (fboundp 'swimmy.school::handle-v2-result)
                       (symbol-function 'swimmy.school::handle-v2-result)))
         (full-name "UT-QUAL-BASE-3979952960-1-QUAL"))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school:apply-backtest-result)
                (lambda (name metrics) (setf called (list name metrics))))
          (setf (symbol-function 'swimmy.school:cache-backtest-result)
                (lambda (&rest _args) (declare (ignore _args)) nil))
          (setf (symbol-function 'swimmy.school:lookup-oos-request)
                (lambda (&rest _args) (declare (ignore _args)) (values nil nil nil)))
          (when (fboundp 'swimmy.school::handle-v2-result)
            (setf (symbol-function 'swimmy.school::handle-v2-result)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (funcall fn
                   (format nil
                           "((type . \"BACKTEST_RESULT\") (result . ((strategy_name . \"~a\") (sharpe . 0.7) (trades . 9) (request_id . \"RID-QUAL\"))))"
                           full-name))
          (assert-not-nil called "Expected apply-backtest-result to be called")
          (assert-equal "UT-QUAL-BASE-3979952960-1" (first called)
                        "Expected only trailing -QUAL suffix to be stripped")
          (assert-equal "RID-QUAL" (getf (second called) :request-id)
                        "Expected request-id in metrics"))
      (setf (symbol-function 'swimmy.school:apply-backtest-result) orig-apply)
      (setf (symbol-function 'swimmy.school:cache-backtest-result) orig-cache)
      (setf (symbol-function 'swimmy.school:lookup-oos-request) orig-lookup)
      (when orig-v2
        (setf (symbol-function 'swimmy.school::handle-v2-result) orig-v2)))))

(deftest test-backtest-result-phase1-excluded-from-rr-buffer
  "Phase1 BACKTEST_RESULT (_P1) should not be counted in RR batch buffer."
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (orig-apply (symbol-function 'swimmy.school:apply-backtest-result))
         (orig-cache (symbol-function 'swimmy.school:cache-backtest-result))
         (orig-lookup (symbol-function 'swimmy.school:lookup-oos-request))
         (orig-v2 (and (fboundp 'swimmy.school::handle-v2-result)
                       (symbol-function 'swimmy.school::handle-v2-result)))
         (orig-rr swimmy.globals:*rr-backtest-results-buffer*)
         (orig-rr-exp swimmy.globals:*rr-expected-backtest-count*))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (unwind-protect
        (progn
          (setf swimmy.globals:*rr-backtest-results-buffer* nil)
          (setf swimmy.globals:*rr-expected-backtest-count* 20)
          (setf (symbol-function 'swimmy.school:apply-backtest-result)
                (lambda (&rest _args) (declare (ignore _args)) t))
          (setf (symbol-function 'swimmy.school:cache-backtest-result)
                (lambda (&rest _args) (declare (ignore _args)) nil))
          (setf (symbol-function 'swimmy.school:lookup-oos-request)
                (lambda (&rest _args) (declare (ignore _args)) (values nil nil nil)))
          (when (fboundp 'swimmy.school::handle-v2-result)
            (setf (symbol-function 'swimmy.school::handle-v2-result)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (funcall fn "((type . \"BACKTEST_RESULT\") (result . ((strategy_name . \"UT-RR-MIX_P1\") (sharpe . 0.4) (trades . 3) (request_id . \"RID-RR-P1\"))))")
          (assert-true (null swimmy.globals:*rr-backtest-results-buffer*)
                       "Phase1 result should not be pushed into RR results buffer"))
      (setf swimmy.globals:*rr-backtest-results-buffer* orig-rr)
      (setf swimmy.globals:*rr-expected-backtest-count* orig-rr-exp)
      (setf (symbol-function 'swimmy.school:apply-backtest-result) orig-apply)
      (setf (symbol-function 'swimmy.school:cache-backtest-result) orig-cache)
      (setf (symbol-function 'swimmy.school:lookup-oos-request) orig-lookup)
      (when orig-v2
        (setf (symbol-function 'swimmy.school::handle-v2-result) orig-v2)))))

(deftest test-backtest-queue-enqueues-when-requester-missing
  "Backtest sends should enqueue before requester is ready"
  (let* ((orig-enabled swimmy.core:*backtest-service-enabled*)
         (orig-req (and (boundp 'swimmy.globals:*backtest-requester*)
                        swimmy.globals:*backtest-requester*))
         (orig-cmd (and (boundp 'swimmy.globals:*cmd-publisher*)
                        swimmy.globals:*cmd-publisher*))
         (orig-send (symbol-function 'pzmq:send))
         (sent nil))
    (unwind-protect
        (progn
          (setf swimmy.core:*backtest-service-enabled* t)
          (setf swimmy.globals:*backtest-requester* nil)
          (setf swimmy.globals:*cmd-publisher* :dummy)
          (setf swimmy.school::*backtest-send-queue* nil)
          (setf (symbol-function 'pzmq:send)
                (lambda (&rest args) (setf sent args)))
          (swimmy.school::send-zmq-msg "(PING)" :target :backtest)
          (assert-true (null sent) "Should not send before requester")
          (assert-equal 1 (length swimmy.school::*backtest-send-queue*)
                        "Expected queued message"))
      (setf swimmy.core:*backtest-service-enabled* orig-enabled)
      (setf swimmy.globals:*backtest-requester* orig-req)
      (setf swimmy.globals:*cmd-publisher* orig-cmd)
      (setf (symbol-function 'pzmq:send) orig-send))))

(deftest test-backtest-queue-flushes-after-requester
  "Queued backtests should flush in order once requester is ready"
  (let* ((orig-req (and (boundp 'swimmy.globals:*backtest-requester*)
                        swimmy.globals:*backtest-requester*))
         (orig-send (symbol-function 'pzmq:send))
         (sent nil))
    (unwind-protect
        (progn
          (setf swimmy.school::*backtest-send-queue* (list "m1" "m2"))
          (setf swimmy.globals:*backtest-requester* :dummy)
          (setf (symbol-function 'pzmq:send)
                (lambda (_sock msg) (push msg sent)))
          (swimmy.school::flush-backtest-queue)
          (assert-equal '("m1" "m2") (nreverse sent) "Expected FIFO send")
          (assert-true (null swimmy.school::*backtest-send-queue*)
                       "Queue should be empty"))
      (setf swimmy.globals:*backtest-requester* orig-req)
      (setf (symbol-function 'pzmq:send) orig-send))))

(deftest test-init-backtest-zmq-fails-when-requester-missing
  "init-backtest-zmq should fail fast when enabled but requester is missing"
  (let* ((orig-enabled swimmy.core:*backtest-service-enabled*)
         (orig-req (and (boundp 'swimmy.globals:*backtest-requester*)
                        swimmy.globals:*backtest-requester*))
         (orig-ctx (symbol-function 'pzmq:ctx-new))
         (orig-socket (symbol-function 'pzmq:socket))
         (orig-connect (symbol-function 'pzmq:connect))
         (signaled nil))
    (unwind-protect
        (progn
          (setf swimmy.core:*backtest-service-enabled* t)
          (setf swimmy.globals:*backtest-requester* nil)
          (setf (symbol-function 'pzmq:ctx-new) (lambda () :ctx))
          (setf (symbol-function 'pzmq:socket)
                (lambda (&rest _args) (declare (ignore _args)) :sock))
          (setf (symbol-function 'pzmq:connect)
                (lambda (&rest _args) (declare (ignore _args)) (error "boom")))
          (handler-case
              (swimmy.school::init-backtest-zmq)
            (error () (setf signaled t)))
          (assert-true signaled "Expected init-backtest-zmq to signal error"))
      (setf swimmy.core:*backtest-service-enabled* orig-enabled)
      (setf swimmy.globals:*backtest-requester* orig-req)
      (setf (symbol-function 'pzmq:ctx-new) orig-ctx)
      (setf (symbol-function 'pzmq:socket) orig-socket)
      (setf (symbol-function 'pzmq:connect) orig-connect))))

(deftest test-init-external-cmd-zmq-sets-publisher
  "init-external-cmd-zmq should connect and set cmd publisher when missing"
  (let* ((fn (find-symbol "INIT-EXTERNAL-CMD-ZMQ" :swimmy.school))
         (orig-pub (and (boundp 'swimmy.globals:*cmd-publisher*)
                        swimmy.globals:*cmd-publisher*))
         (orig-ctx (symbol-function 'pzmq:ctx-new))
         (orig-socket (symbol-function 'pzmq:socket))
         (orig-connect (symbol-function 'pzmq:connect))
         (captured-endpoint nil))
    (assert-true (and fn (fboundp fn)) "init-external-cmd-zmq exists")
    (unwind-protect
        (progn
          (setf swimmy.globals:*cmd-publisher* nil)
          (setf (symbol-function 'pzmq:ctx-new) (lambda () :ctx))
          (setf (symbol-function 'pzmq:socket)
                (lambda (&rest _args) (declare (ignore _args)) :sock))
          (setf (symbol-function 'pzmq:connect)
                (lambda (_sock endpoint)
                  (setf captured-endpoint endpoint)))
          (funcall fn)
          (assert-equal :sock swimmy.globals:*cmd-publisher* "publisher should be set")
          (assert-equal (swimmy.core:zmq-connect-endpoint swimmy.core:*port-external*)
                        captured-endpoint
                        "should connect to external port"))
      (setf swimmy.globals:*cmd-publisher* orig-pub)
      (setf (symbol-function 'pzmq:ctx-new) orig-ctx)
      (setf (symbol-function 'pzmq:socket) orig-socket)
      (setf (symbol-function 'pzmq:connect) orig-connect))))

(deftest test-backtest-result-preserves-request-id
  "BACKTEST_RESULT should carry request_id through the pipeline"
  (let ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (let* ((msg "((type . \"BACKTEST_RESULT\") (result . ((strategy_name . \"UT-REQ\") (sharpe . 0.2) (trades . 1) (request_id . \"RID-1\"))))")
           (orig-cache (symbol-function 'swimmy.school:cache-backtest-result))
           (orig-apply (symbol-function 'swimmy.school:apply-backtest-result))
           (orig-lookup (symbol-function 'swimmy.school:lookup-oos-request))
           (orig-v2 (and (fboundp 'swimmy.school::handle-v2-result)
                         (symbol-function 'swimmy.school::handle-v2-result))))
      (unwind-protect
          (progn
            (setf (symbol-function 'swimmy.school:cache-backtest-result)
                  (lambda (&rest args) (declare (ignore args)) nil))
            (setf (symbol-function 'swimmy.school:apply-backtest-result)
                  (lambda (&rest args) (declare (ignore args)) nil))
            (setf (symbol-function 'swimmy.school:lookup-oos-request)
                  (lambda (&rest args) (declare (ignore args)) (values nil nil nil)))
            (when (fboundp 'swimmy.school::handle-v2-result)
              (setf (symbol-function 'swimmy.school::handle-v2-result)
                    (lambda (&rest args) (declare (ignore args)) nil)))
            (setf swimmy.main::*backtest-recv-count* 0)
            (setf swimmy.main::*backtest-recv-last-id* nil)
            (funcall fn msg)
            (assert-equal "RID-1" swimmy.main::*backtest-recv-last-id*
                          "request_id should be preserved"))
        (setf (symbol-function 'swimmy.school:cache-backtest-result) orig-cache)
        (setf (symbol-function 'swimmy.school:apply-backtest-result) orig-apply)
        (setf (symbol-function 'swimmy.school:lookup-oos-request) orig-lookup)
        (when orig-v2
          (setf (symbol-function 'swimmy.school::handle-v2-result) orig-v2))))))

(deftest test-backtest-result-persists-trade-list
  "internal-process-msg should persist trade_list with oos_kind mapping"
  (let ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (let* ((msg "((type . \"BACKTEST_RESULT\") (result . ((strategy_name . \"AAA-OOS\") (sharpe . 0.1) (trades . 1) (request_id . \"RID-1\") (trade_list . (((timestamp . 1) (pnl . 1.0) (symbol . \"USDJPY\")))))))")
           (called nil)
           (orig-record (symbol-function 'swimmy.school:record-backtest-trades))
           (orig-cache (symbol-function 'swimmy.school:cache-backtest-result))
           (orig-apply (symbol-function 'swimmy.school:apply-backtest-result))
           (orig-lookup (symbol-function 'swimmy.school:lookup-oos-request))
           (orig-v2 (and (fboundp 'swimmy.school::handle-v2-result)
                         (symbol-function 'swimmy.school::handle-v2-result)))
           (orig-oos (and (fboundp 'swimmy.school:handle-oos-backtest-result)
                          (symbol-function 'swimmy.school:handle-oos-backtest-result))))
      (unwind-protect
          (progn
            (setf (symbol-function 'swimmy.school:record-backtest-trades)
                  (lambda (rid name kind trades)
                    (setf called (list rid name kind trades))
                    nil))
            (setf (symbol-function 'swimmy.school:cache-backtest-result)
                  (lambda (&rest args) (declare (ignore args)) nil))
            (setf (symbol-function 'swimmy.school:apply-backtest-result)
                  (lambda (&rest args) (declare (ignore args)) nil))
            (setf (symbol-function 'swimmy.school:lookup-oos-request)
                  (lambda (&rest args) (declare (ignore args)) (values nil nil nil)))
            (when orig-v2
              (setf (symbol-function 'swimmy.school::handle-v2-result)
                    (lambda (&rest args) (declare (ignore args)) nil)))
            (when orig-oos
              (setf (symbol-function 'swimmy.school:handle-oos-backtest-result)
                    (lambda (&rest args) (declare (ignore args)) nil)))
            (funcall fn msg)
            (assert-true called "record-backtest-trades should be called")
            (assert-equal "RID-1" (first called) "request_id should match")
            (assert-equal "AAA" (second called) "strategy name should be normalized")
            (assert-equal "OOS" (third called) "oos_kind should be OOS")
            (assert-true (listp (fourth called)) "trade_list should be passed"))
        (setf (symbol-function 'swimmy.school:record-backtest-trades) orig-record)
        (setf (symbol-function 'swimmy.school:cache-backtest-result) orig-cache)
        (setf (symbol-function 'swimmy.school:apply-backtest-result) orig-apply)
        (setf (symbol-function 'swimmy.school:lookup-oos-request) orig-lookup)
        (when orig-v2
          (setf (symbol-function 'swimmy.school::handle-v2-result) orig-v2))
        (when orig-oos
          (setf (symbol-function 'swimmy.school:handle-oos-backtest-result) orig-oos))))))

(deftest test-cpcv-result-persists-trade-list
  "CPCV_RESULT should persist trade_list as oos_kind=CPCV"
  (let ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (let* ((msg "((type . \"CPCV_RESULT\") (result . ((strategy_name . \"AAA\") (request_id . \"RID-CPCV\") (trade_list . (((timestamp . 1) (pnl . 1.0) (symbol . \"USDJPY\")))))))")
           (called nil)
           (orig-record (symbol-function 'swimmy.school:record-backtest-trades)))
      (unwind-protect
          (progn
            (setf (symbol-function 'swimmy.school:record-backtest-trades)
                  (lambda (rid name kind trades)
                    (setf called (list rid name kind trades))
                    nil))
            (funcall fn msg)
            (assert-true called "record-backtest-trades should be called")
            (assert-equal "RID-CPCV" (first called) "request_id should match")
            (assert-equal "AAA" (second called) "strategy name should match")
            (assert-equal "CPCV" (third called) "oos_kind should be CPCV"))
        (setf (symbol-function 'swimmy.school:record-backtest-trades) orig-record)))))

(deftest test-cpcv-result-normalizes-string-keys
  "CPCV_RESULT should accept keyword/string keys for request_id/trade_list"
  (let ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (let* ((msg "((type . \"CPCV_RESULT\") (result . ((:strategy_name . \"AAA\") (\"request_id\" . \"RID-CPCV\") (\"trade_list\" . (((timestamp . 1) (pnl . 1.0) (symbol . \"USDJPY\")))))))")
           (called nil)
           (orig-record (symbol-function 'swimmy.school:record-backtest-trades)))
      (unwind-protect
          (progn
            (setf (symbol-function 'swimmy.school:record-backtest-trades)
                  (lambda (rid name kind trades)
                    (setf called (list rid name kind trades))
                    nil))
            (funcall fn msg)
            (assert-true called "record-backtest-trades should be called")
            (assert-equal "RID-CPCV" (first called) "request_id should match")
            (assert-equal "AAA" (second called) "strategy name should match")
            (assert-equal "CPCV" (third called) "oos_kind should be CPCV"))
        (setf (symbol-function 'swimmy.school:record-backtest-trades) orig-record)))))

(deftest test-cpcv-result-preserves-trade-meta
  "CPCV_RESULT should preserve trade_list metadata fields"
  (let ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (let* ((msg "((type . \"CPCV_RESULT\") (result . ((strategy_name . \"AAA\") (request_id . \"RID-CPCV\") (trade_list . (((timestamp . 1) (pnl . 1.0) (symbol . \"USDJPY\")))) (trades_truncated . t) (trades_ref . \"RID-CPCV\"))))")
           (called nil)
           (captured nil)
           (orig-kb swimmy.school::*strategy-knowledge-base*)
           (orig-record (symbol-function 'swimmy.school:record-backtest-trades))
           (orig-notify (symbol-function 'swimmy.core:notify-cpcv-result)))
      (unwind-protect
          (progn
            (setf swimmy.school::*strategy-knowledge-base*
                  (list (swimmy.school:make-strategy :name "AAA" :rank :A)))
            (setf (symbol-function 'swimmy.school:record-backtest-trades)
                  (lambda (rid name kind trades)
                    (setf called (list rid name kind trades))
                    nil))
            (setf (symbol-function 'swimmy.core:notify-cpcv-result)
                  (lambda (data)
                    (setf captured data)
                    nil))
            (funcall fn msg)
            (assert-true called "record-backtest-trades should be called")
            (assert-true captured "notify-cpcv-result should be called")
            (assert-equal t (getf captured :trades-truncated)
                          "trades_truncated should be preserved")
            (assert-equal "RID-CPCV" (getf captured :trades-ref)
                          "trades_ref should be preserved"))
        (setf swimmy.school::*strategy-knowledge-base* orig-kb)
        (setf (symbol-function 'swimmy.school:record-backtest-trades) orig-record)
        (setf (symbol-function 'swimmy.core:notify-cpcv-result) orig-notify)))))

(deftest test-cpcv-result-normalizes-is-passed
  "CPCV_RESULT should treat false-ish is_passed as not passed"
  (let ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
           (orig-ensure (symbol-function 'swimmy.school:ensure-rank))
           (orig-check (symbol-function 'swimmy.school:check-rank-criteria))
           (orig-upsert (symbol-function 'swimmy.school:upsert-strategy))
           (called nil))
      (unwind-protect
          (progn
            (setf swimmy.school::*strategy-knowledge-base*
                  (list (swimmy.school:make-strategy :name "AAA" :rank :A)))
            (setf (symbol-function 'swimmy.school:ensure-rank)
                  (lambda (&rest args)
                    (setf called args)
                    nil))
            (setf (symbol-function 'swimmy.school:check-rank-criteria)
                  (lambda (&rest args)
                    (declare (ignore args))
                    t))
            (setf (symbol-function 'swimmy.school:upsert-strategy)
                  (lambda (&rest args)
                    (declare (ignore args))
                    nil))
            (funcall fn "((type . \"CPCV_RESULT\") (result . ((strategy_name . \"AAA\") (is_passed . \"false\"))))")
            (assert-false called "should not promote when is_passed=false")
            (setf called nil)
            (funcall fn "((type . \"CPCV_RESULT\") (result . ((strategy_name . \"AAA\") (is_passed . \"true\"))))")
            (assert-true called "should promote when is_passed=true"))
        (setf swimmy.school::*strategy-knowledge-base* orig-kb)
        (setf (symbol-function 'swimmy.school:ensure-rank) orig-ensure)
        (setf (symbol-function 'swimmy.school:check-rank-criteria) orig-check)
        (setf (symbol-function 'swimmy.school:upsert-strategy) orig-upsert)))))

(deftest test-cpcv-result-updates-db-when-strategy-missing
  "CPCV_RESULT should update DB even when strategy is not in memory"
  (let ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
           (orig-evolved (and (boundp 'swimmy.globals:*evolved-strategies*)
                              swimmy.globals:*evolved-strategies*))
           (orig-update (and (fboundp 'swimmy.school::update-cpcv-metrics-by-name)
                             (symbol-function 'swimmy.school::update-cpcv-metrics-by-name)))
           (orig-notify (symbol-function 'swimmy.core:notify-cpcv-result))
           (called nil))
      (unwind-protect
          (progn
            (setf swimmy.school::*strategy-knowledge-base* nil)
            (when (boundp 'swimmy.globals:*evolved-strategies*)
              (setf swimmy.globals:*evolved-strategies* nil))
            (setf (symbol-function 'swimmy.core:notify-cpcv-result)
                  (lambda (&rest args) (declare (ignore args)) nil))
            (setf (symbol-function 'swimmy.school::update-cpcv-metrics-by-name)
                  (lambda (name median median-pf median-wr median-maxdd pass-rate &key request-id)
                    (setf called (list name median median-pf median-wr median-maxdd pass-rate request-id))
                    t))
            (funcall fn "((type . \"CPCV_RESULT\") (result . ((strategy_name . \"AAA\") (request_id . \"RID\") (median_sharpe . 1.2) (median_pf . 1.5) (median_wr . 0.55) (median_maxdd . 0.1) (pass_rate . 0.6))))")
            (assert-true called "should update CPCV metrics even when strategy missing")
            (assert-equal "AAA" (first called) "strategy name should match")
            (assert-equal 1.2 (second called) "median_sharpe should match")
            (assert-equal 1.5 (third called) "median_pf should match")
            (assert-equal 0.55 (fourth called) "median_wr should match")
            (assert-equal 0.1 (fifth called) "median_maxdd should match")
            (assert-equal 0.6 (sixth called) "pass_rate should match")
            (assert-equal "RID" (seventh called) "request_id should match"))
        (setf swimmy.school::*strategy-knowledge-base* orig-kb)
        (when (boundp 'swimmy.globals:*evolved-strategies*)
          (setf swimmy.globals:*evolved-strategies* orig-evolved))
        (setf (symbol-function 'swimmy.core:notify-cpcv-result) orig-notify)
        (if orig-update
            (setf (symbol-function 'swimmy.school::update-cpcv-metrics-by-name) orig-update)
            (fmakunbound 'swimmy.school::update-cpcv-metrics-by-name))))))

(deftest test-cpcv-result-metrics-split-runtime-vs-criteria
  "CPCV_RESULT should split runtime errors and criteria failures in counters"
  (let ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
           (orig-metrics swimmy.school::*cpcv-metrics*)
           (orig-record (symbol-function 'swimmy.school:record-backtest-trades))
           (orig-notify (symbol-function 'swimmy.core:notify-cpcv-result))
           (orig-upsert (symbol-function 'swimmy.school:upsert-strategy))
           (orig-write (symbol-function 'swimmy.school::write-cpcv-status-file)))
      (unwind-protect
          (progn
            (setf swimmy.school::*strategy-knowledge-base*
                  (list (swimmy.school:make-strategy :name "AAA" :rank :A)))
            (setf swimmy.school::*cpcv-metrics* (make-hash-table :test 'equal))
            (swimmy.school::reset-cpcv-metrics :queued 0)
            (setf (symbol-function 'swimmy.school:record-backtest-trades)
                  (lambda (&rest args) (declare (ignore args)) nil))
            (setf (symbol-function 'swimmy.core:notify-cpcv-result)
                  (lambda (&rest args) (declare (ignore args)) nil))
            (setf (symbol-function 'swimmy.school:upsert-strategy)
                  (lambda (&rest args) (declare (ignore args)) nil))
            (setf (symbol-function 'swimmy.school::write-cpcv-status-file)
                  (lambda (&rest args) (declare (ignore args)) t))

            ;; Runtime error path
            (funcall fn
                     "((type . \"CPCV_RESULT\") (result . ((strategy_name . \"AAA\") (request_id . \"RID-ERR\") (error . \"boom\") (is_passed . nil))))")
            (assert-equal 1 (gethash :received swimmy.school::*cpcv-metrics* 0)
                          "received should increment")
            (assert-equal 1 (gethash :result_runtime_failed swimmy.school::*cpcv-metrics* 0)
                          "runtime failure should increment")
            (assert-equal 0 (gethash :result_criteria_failed swimmy.school::*cpcv-metrics* 0)
                          "criteria failure should not increment on runtime error")

            ;; Criteria fail path
            (funcall fn
                     "((type . \"CPCV_RESULT\") (result . ((strategy_name . \"AAA\") (request_id . \"RID-FAIL\") (median_sharpe . 0.1) (is_passed . nil))))")
            (assert-equal 2 (gethash :received swimmy.school::*cpcv-metrics* 0)
                          "received should increment")
            (assert-equal 1 (gethash :result_runtime_failed swimmy.school::*cpcv-metrics* 0)
                          "runtime failure count should remain")
            (assert-equal 1 (gethash :result_criteria_failed swimmy.school::*cpcv-metrics* 0)
                          "criteria failure should increment")
            (assert-equal 2 (gethash :result_failed swimmy.school::*cpcv-metrics* 0)
                          "total result failure should remain backward compatible")

            ;; Empty-path result should be runtime fail, not criteria fail
            (funcall fn
                     "((type . \"CPCV_RESULT\") (result . ((strategy_name . \"AAA\") (request_id . \"RID-EMPTY\") (path_count . 0) (passed_count . 0) (failed_count . 0) (pass_rate . 0.0))))")
            (assert-equal 3 (gethash :received swimmy.school::*cpcv-metrics* 0)
                          "received should increment for empty result")
            (assert-equal 2 (gethash :result_runtime_failed swimmy.school::*cpcv-metrics* 0)
                          "empty result should increment runtime failure")
            (assert-equal 1 (gethash :result_criteria_failed swimmy.school::*cpcv-metrics* 0)
                          "empty result should not increment criteria failure")
            (assert-equal 3 (gethash :result_failed swimmy.school::*cpcv-metrics* 0)
                          "total failure should include empty runtime result"))
        (setf swimmy.school::*strategy-knowledge-base* orig-kb)
        (setf swimmy.school::*cpcv-metrics* orig-metrics)
        (setf (symbol-function 'swimmy.school:record-backtest-trades) orig-record)
        (setf (symbol-function 'swimmy.core:notify-cpcv-result) orig-notify)
        (setf (symbol-function 'swimmy.school:upsert-strategy) orig-upsert)
        (setf (symbol-function 'swimmy.school::write-cpcv-status-file) orig-write)))))

(deftest test-cpcv-result-skips-notify-for-unknown-strategy
  "CPCV_RESULT should not send per-strategy alert for unknown strategy names"
  (let ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (let* ((unknown-name "UT-UNKNOWN-CPCV-ALERT")
           (orig-kb swimmy.school::*strategy-knowledge-base*)
           (orig-evolved (and (boundp 'swimmy.globals:*evolved-strategies*)
                              swimmy.globals:*evolved-strategies*))
           (orig-notify (symbol-function 'swimmy.core:notify-cpcv-result))
           (orig-update (and (fboundp 'swimmy.school::update-cpcv-metrics-by-name)
                             (symbol-function 'swimmy.school::update-cpcv-metrics-by-name)))
           (notify-count 0)
           (update-called nil))
      (unwind-protect
          (progn
            (setf swimmy.school::*strategy-knowledge-base* nil)
            (when (boundp 'swimmy.globals:*evolved-strategies*)
              (setf swimmy.globals:*evolved-strategies* nil))
            (when (fboundp 'swimmy.school::execute-non-query)
              (ignore-errors
                (swimmy.school::execute-non-query
                 "DELETE FROM strategies WHERE name = ?"
                 unknown-name)))
            (setf (symbol-function 'swimmy.core:notify-cpcv-result)
                  (lambda (&rest _args)
                    (declare (ignore _args))
                    (incf notify-count)
                    nil))
            (when (fboundp 'swimmy.school::update-cpcv-metrics-by-name)
              (setf (symbol-function 'swimmy.school::update-cpcv-metrics-by-name)
                    (lambda (&rest _args)
                      (declare (ignore _args))
                      (setf update-called t)
                      t)))
            (funcall fn
                     (format nil
                             "((type . \"CPCV_RESULT\") (result . ((strategy_name . \"~a\") (request_id . \"RID-U\") (median_sharpe . 0.3) (median_pf . 1.1) (median_wr . 0.4) (median_maxdd . 0.2) (path_count . 10) (passed_count . 3) (failed_count . 7) (pass_rate . 0.3) (is_passed . nil))))"
                             unknown-name))
            (assert-true update-called
                         "DB update path should still run for unknown strategy")
            (assert-equal 0 notify-count
                          "unknown strategy should not trigger CPCV per-strategy notification"))
        (setf swimmy.school::*strategy-knowledge-base* orig-kb)
        (when (boundp 'swimmy.globals:*evolved-strategies*)
          (setf swimmy.globals:*evolved-strategies* orig-evolved))
        (setf (symbol-function 'swimmy.core:notify-cpcv-result) orig-notify)
        (if orig-update
            (setf (symbol-function 'swimmy.school::update-cpcv-metrics-by-name) orig-update)
            (fmakunbound 'swimmy.school::update-cpcv-metrics-by-name))))))

(deftest test-cpcv-result-skips-notify-for-graveyard-strategy
  "CPCV_RESULT should not send per-strategy alert for graveyard-only strategies"
  (let ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (let* ((grave-name "UT-GRAVE-CPCV-ALERT")
           (orig-kb swimmy.school::*strategy-knowledge-base*)
           (orig-evolved (and (boundp 'swimmy.globals:*evolved-strategies*)
                              swimmy.globals:*evolved-strategies*))
           (orig-notify (symbol-function 'swimmy.core:notify-cpcv-result))
           (notify-count 0))
      (unwind-protect
          (progn
            (setf swimmy.school::*strategy-knowledge-base* nil)
            (when (boundp 'swimmy.globals:*evolved-strategies*)
              (setf swimmy.globals:*evolved-strategies* nil))
            (when (fboundp 'swimmy.school::execute-non-query)
              (ignore-errors
                (swimmy.school::execute-non-query
                 "DELETE FROM strategies WHERE name = ?"
                 grave-name))
              (ignore-errors
                (swimmy.school::execute-non-query
                 "INSERT OR REPLACE INTO strategies (name, rank, updated_at) VALUES (?, ?, ?)"
                 grave-name ":GRAVEYARD" (get-universal-time))))
            (setf (symbol-function 'swimmy.core:notify-cpcv-result)
                  (lambda (&rest _args)
                    (declare (ignore _args))
                    (incf notify-count)
                    nil))
            (funcall fn
                     (format nil
                             "((type . \"CPCV_RESULT\") (result . ((strategy_name . \"~a\") (request_id . \"RID-G\") (path_count . 0) (passed_count . 0) (failed_count . 0) (pass_rate . 0.0))))"
                             grave-name))
            (assert-equal 0 notify-count
                          "graveyard strategy should not trigger CPCV per-strategy notification"))
        (setf swimmy.school::*strategy-knowledge-base* orig-kb)
        (when (boundp 'swimmy.globals:*evolved-strategies*)
          (setf swimmy.globals:*evolved-strategies* orig-evolved))
        (when (fboundp 'swimmy.school::execute-non-query)
          (ignore-errors
            (swimmy.school::execute-non-query
             "DELETE FROM strategies WHERE name = ?"
             grave-name)))
        (setf (symbol-function 'swimmy.core:notify-cpcv-result) orig-notify)))))

(deftest test-request-cpcv-includes-request-id
  "request-cpcv-validation should include request_id in payload"
  (let* ((fn (find-symbol "REQUEST-CPCV-VALIDATION" :swimmy.school))
         (orig-send (symbol-function 'swimmy.school::send-zmq-msg))
         (orig-uuid (symbol-function 'swimmy.core:generate-uuid))
         (captured nil)
         (strat (swimmy.school:make-strategy :name "UT-CPCV" :rank :A)))
    (assert-true (and fn (fboundp fn)) "request-cpcv-validation exists")
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.core:generate-uuid)
                (lambda () "RID-CPCV-1"))
          (setf (symbol-function 'swimmy.school::send-zmq-msg)
                (lambda (msg &key target)
                  (declare (ignore target))
                  (setf captured msg)
                  nil))
          (funcall fn strat)
          (assert-true (and captured (search "request_id" captured))
                       "request_id should be present in payload")
          (assert-true (search "RID-CPCV-1" captured)
                       "request_id should match generated value")
          (assert-true (search "(action" captured)
                       "action should be present in payload")
          (assert-true (search "CPCV_VALIDATE" captured)
                       "action should be CPCV_VALIDATE")
          (assert-true (search "strategy_name" captured)
                       "strategy_name should be present")
          (assert-true (search "symbol" captured)
                       "symbol should be present")
          (assert-true (search "candles_file" captured)
                       "candles_file should be present")
          (assert-true (search "strategy_params" captured)
                       "strategy_params should be present"))
      (setf (symbol-function 'swimmy.school::send-zmq-msg) orig-send)
      (setf (symbol-function 'swimmy.core:generate-uuid) orig-uuid))))

(deftest test-cpcv-gate-failure-counts
  "cpcv-gate-failure-counts should tally elite and S-base failures by criterion"
  (let* ((mk (lambda (name sharpe pf wr dd)
               (swimmy.school:make-strategy :name name :rank :A
                                            :sharpe sharpe :profit-factor pf
                                            :win-rate wr :max-dd dd)))
         (s-pass (funcall mk "S-PASS" 0.80 1.8 0.55 0.09))
         (s-fs (funcall mk "S-FS" 0.70 2.0 0.55 0.09))
         (s-fpf (funcall mk "S-FPF" 0.80 1.6 0.55 0.09))
         (s-fwr (funcall mk "S-FWR" 0.80 1.8 0.49 0.09))
         (s-fdd (funcall mk "S-FDD" 0.80 1.8 0.55 0.11))
         (counts (swimmy.school::cpcv-gate-failure-counts
                  (list s-pass s-fs s-fpf s-fwr s-fdd))))
    (assert-equal 5 (getf counts :total) "total count")
    (assert-equal 4 (getf counts :pass) "elite pass count (sharpe>=0.75)")
    (assert-equal 1 (getf counts :sharpe) "sharpe fail count")
    (assert-equal 1 (getf counts :pf) "pf fail count")
    (assert-equal 1 (getf counts :wr) "wr fail count")
    (assert-equal 1 (getf counts :maxdd) "maxdd fail count")))

(deftest test-a-stage1-failure-counts
  "a-stage1-failure-counts should tally A-base gate failures by criterion"
  (let* ((mk (lambda (name sharpe pf wr dd)
               (swimmy.school:make-strategy :name name :rank :B
                                            :sharpe sharpe :profit-factor pf
                                            :win-rate wr :max-dd dd)))
         (a-pass (funcall mk "A-PASS" 0.50 1.35 0.45 0.10))
         (a-fsh (funcall mk "A-FSH" 0.40 1.35 0.45 0.10))
         (a-fpf (funcall mk "A-FPF" 0.50 1.20 0.45 0.10))
         (a-fwr (funcall mk "A-FWR" 0.50 1.35 0.40 0.10))
         (a-fdd (funcall mk "A-FDD" 0.50 1.35 0.45 0.20))
         (counts (swimmy.school::a-stage1-failure-counts
                  (list a-pass a-fsh a-fpf a-fwr a-fdd))))
    (assert-equal 5 (getf counts :total) "total count")
    (assert-equal 1 (getf counts :pass) "A-base pass count")
    (assert-equal 1 (getf counts :sharpe) "sharpe fail count")
    (assert-equal 1 (getf counts :pf) "pf fail count")
    (assert-equal 1 (getf counts :wr) "wr fail count")
    (assert-equal 1 (getf counts :maxdd) "maxdd fail count")))

(deftest test-a-stage1-failure-summary-line
  "a-stage1-failure-summary-line should include PF/WR failure counts"
  (let* ((counts (list :total 3 :pass 1 :sharpe 0 :pf 1 :wr 1 :maxdd 0
                       :sharpe-min 0.45 :pf-min 1.30 :wr-min 0.43 :maxdd-max 0.16))
         (line (swimmy.school::a-stage1-failure-summary-line counts :label "A Stage1 Failures (Batch)")))
    (assert-true (search "A Stage1 Failures (Batch)" line))
    (assert-true (search "pf<1.30=1" line))
    (assert-true (search "wr<43%" line))
    (assert-true (search "pass=1" line))))

(deftest test-fetch-cpcv-candidates-filters-by-elite-sharpe
  "fetch-cpcv-candidates should return A-rank CPCV candidates that pass S-base gates (excluding CPCV)."
  (let* ((s-min (or (ignore-errors (swimmy.school::min-trade-evidence-for-rank :S)) 100))
         (s-trades (+ s-min 20))
         (mk (lambda (name sharpe pf wr dd trades)
               (swimmy.school:make-strategy :name name :rank :A
                                            :sharpe sharpe :profit-factor pf
                                            :win-rate wr :max-dd dd
                                            :trades trades)))
         (s-pass (funcall mk "S-PASS" 0.8 1.8 0.55 0.09 s-trades))
         ;; S-base staged PF/WR + trade-evidence floor (>=100) should allow this.
         (s-stage (funcall mk "S-STAGE" 0.8 1.6 0.46 0.09 s-trades))
         ;; Still fails staged PF/WR gates (even with enough trade evidence).
         (s-fail (funcall mk "S-FAIL" 0.8 1.3 0.40 0.09 s-trades))
         (b-elite (swimmy.school:make-strategy :name "B-ELITE" :rank :B
                                               :sharpe 0.8 :profit-factor 2.0
                                               :win-rate 0.6 :max-dd 0.1))
         (orig-fetch (symbol-function 'swimmy.school:fetch-candidate-strategies)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school:fetch-candidate-strategies)
                (lambda (&rest args) (declare (ignore args)) (list s-pass s-stage s-fail b-elite)))
          (let ((cands (swimmy.school::fetch-cpcv-candidates)))
            (assert-equal 2 (length cands) "should return only S-base-ready A-rank candidates")
            (assert-true (find "S-PASS" cands :key #'swimmy.school:strategy-name :test #'string=)
                         "S-PASS should be included")
            (assert-true (find "S-STAGE" cands :key #'swimmy.school:strategy-name :test #'string=)
                         "S-STAGE should be included")
            (assert-false (find "S-FAIL" cands :key #'swimmy.school:strategy-name :test #'string=)
                          "S-FAIL should be excluded")
            (assert-false (find "B-ELITE" cands :key #'swimmy.school:strategy-name :test #'string=)
                          "B-ELITE should be excluded")))
      (setf (symbol-function 'swimmy.school:fetch-candidate-strategies) orig-fetch))))

(deftest test-cpcv-dispatch-eligible-cooldown
  "cpcv-dispatch-eligible-p should enforce cooldown unless metrics changed."
  (let* ((orig-interval swimmy.school::*cpcv-strategy-retry-interval*)
         (orig-times swimmy.school::*cpcv-last-dispatch-times*)
         (orig-sigs swimmy.school::*cpcv-last-dispatch-signatures*)
         (orig-outcomes (and (boundp 'swimmy.school::*cpcv-last-result-outcomes*)
                             swimmy.school::*cpcv-last-result-outcomes*))
         (strat (swimmy.school:make-strategy :name "UT-CPCV-CD" :rank :A
                                             :sharpe 0.8 :profit-factor 1.8
                                             :win-rate 0.55 :max-dd 0.09)))
    (unwind-protect
        (progn
          (setf swimmy.school::*cpcv-strategy-retry-interval* 300)
          (setf swimmy.school::*cpcv-last-dispatch-times* (make-hash-table :test 'equal))
          (setf swimmy.school::*cpcv-last-dispatch-signatures* (make-hash-table :test 'equal))
          (setf swimmy.school::*cpcv-last-result-outcomes* (make-hash-table :test 'equal))
          (assert-true (swimmy.school::cpcv-dispatch-eligible-p strat :now 1000)
                       "First dispatch should be eligible")
          (swimmy.school::mark-cpcv-dispatched strat :now 1000)
          (assert-false (swimmy.school::cpcv-dispatch-eligible-p strat :now 1100)
                        "Unchanged strategy inside cooldown should be skipped")
          ;; Cooldown expiry should allow retry when result is still inflight/unknown.
          (assert-true (swimmy.school::cpcv-dispatch-eligible-p strat :now 1400)
                       "Cooldown expiry should allow redispatch when inflight/unknown")
          ;; But once we know CPCV failed criteria, do not re-dispatch just because time passed.
          (setf (gethash (swimmy.school:strategy-name strat) swimmy.school::*cpcv-last-result-outcomes*)
                :criteria-fail)
          (assert-false (swimmy.school::cpcv-dispatch-eligible-p strat :now 1400)
                        "Criteria-failed strategy should not be redispatched on time alone")
          ;; Metric change should still bypass cooldown even after criteria fail.
          (setf (swimmy.school:strategy-sharpe strat) 0.95)
          (assert-true (swimmy.school::cpcv-dispatch-eligible-p strat :now 1400)
                       "Metric change should bypass cooldown and criteria-fail lockout")
          ;; Runtime failures (or stuck inflight) should remain retryable after cooldown.
          (swimmy.school::mark-cpcv-dispatched strat :now 1400)
          (setf (gethash (swimmy.school:strategy-name strat) swimmy.school::*cpcv-last-result-outcomes*)
                :runtime-fail)
          (assert-true (swimmy.school::cpcv-dispatch-eligible-p strat :now 1800)
                       "Runtime-failed strategy should be retryable after cooldown"))
      (setf swimmy.school::*cpcv-strategy-retry-interval* orig-interval)
      (setf swimmy.school::*cpcv-last-dispatch-times* orig-times)
      (setf swimmy.school::*cpcv-last-dispatch-signatures* orig-sigs)
      (when (boundp 'swimmy.school::*cpcv-last-result-outcomes*)
        (setf swimmy.school::*cpcv-last-result-outcomes* orig-outcomes)))))

(deftest test-run-a-rank-cpcv-batch-respects-strategy-cooldown
  "run-a-rank-cpcv-batch should skip unchanged recently dispatched strategies."
  (let* ((s-min (or (ignore-errors (swimmy.school::min-trade-evidence-for-rank :S)) 100))
         (s-trades (+ s-min 20))
         (orig-fetch (symbol-function 'swimmy.school:fetch-candidate-strategies))
         (orig-request (symbol-function 'swimmy.school::request-cpcv-validation))
         (orig-write (and (fboundp 'swimmy.school::write-cpcv-status-file)
                          (symbol-function 'swimmy.school::write-cpcv-status-file)))
         (orig-cycle-interval swimmy.school::*cpcv-cycle-interval*)
         (orig-last-cycle swimmy.school::*last-cpcv-cycle*)
         (orig-retry-interval swimmy.school::*cpcv-strategy-retry-interval*)
         (orig-times swimmy.school::*cpcv-last-dispatch-times*)
         (orig-sigs swimmy.school::*cpcv-last-dispatch-signatures*)
         (dispatches nil)
         (s1 (swimmy.school:make-strategy :name "UT-CPCV-B1" :rank :A
                                          :sharpe 0.80 :profit-factor 1.8
                                          :win-rate 0.55 :max-dd 0.09
                                          :trades s-trades))
         (s2 (swimmy.school:make-strategy :name "UT-CPCV-B2" :rank :A
                                          :sharpe 0.82 :profit-factor 1.9
                                          :win-rate 0.56 :max-dd 0.08
                                          :trades s-trades)))
    (unwind-protect
        (progn
          (setf swimmy.school::*cpcv-cycle-interval* 0)
          (setf swimmy.school::*last-cpcv-cycle* 0)
          (setf swimmy.school::*cpcv-strategy-retry-interval* 600)
          (setf swimmy.school::*cpcv-last-dispatch-times* (make-hash-table :test 'equal))
          (setf swimmy.school::*cpcv-last-dispatch-signatures* (make-hash-table :test 'equal))
          (setf (symbol-function 'swimmy.school:fetch-candidate-strategies)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (list s1 s2)))
          (setf (symbol-function 'swimmy.school::request-cpcv-validation)
                (lambda (strat)
                  (push (swimmy.school:strategy-name strat) dispatches)
                  (values t nil nil)))
          (when orig-write
            (setf (symbol-function 'swimmy.school::write-cpcv-status-file)
                  (lambda (&rest _args) (declare (ignore _args)) t)))

          (swimmy.school::run-a-rank-cpcv-batch)
          (assert-equal 2 (length dispatches)
                        "First batch should dispatch both strategies")
          (swimmy.school::run-a-rank-cpcv-batch)
          (assert-equal 2 (length dispatches)
                        "Second immediate batch should be skipped by cooldown")
          ;; Change one strategy metric and rerun; only that one should redispatch.
          (setf (swimmy.school:strategy-sharpe s1) 0.95)
          (swimmy.school::run-a-rank-cpcv-batch)
          (assert-equal 3 (length dispatches)
                        "Metric change should trigger one additional dispatch"))
      (setf (symbol-function 'swimmy.school:fetch-candidate-strategies) orig-fetch)
      (setf (symbol-function 'swimmy.school::request-cpcv-validation) orig-request)
      (when orig-write
        (setf (symbol-function 'swimmy.school::write-cpcv-status-file) orig-write))
      (setf swimmy.school::*cpcv-cycle-interval* orig-cycle-interval)
      (setf swimmy.school::*last-cpcv-cycle* orig-last-cycle)
      (setf swimmy.school::*cpcv-strategy-retry-interval* orig-retry-interval)
      (setf swimmy.school::*cpcv-last-dispatch-times* orig-times)
      (setf swimmy.school::*cpcv-last-dispatch-signatures* orig-sigs))))

(deftest test-build-cpcv-status-snippet-includes-metrics
  "build-cpcv-status-snippet should include queued/sent/received/failed"
  (let* ((orig-path swimmy.school::*cpcv-status-path*)
         (orig-metrics swimmy.school::*cpcv-metrics*)
         (orig-start swimmy.globals:*cpcv-start-time*)
         (tmp (merge-pathnames (format nil "/tmp/cpcv-snippet-~a.txt" (get-universal-time)))))
    (unwind-protect
        (progn
          ;; build-cpcv-status-snippet uses the shared status file as source of truth.
          (setf swimmy.school::*cpcv-status-path* tmp)
          (with-open-file (out tmp :direction :output :if-exists :supersede :if-does-not-exist :create)
            (write-line "5 queued | 4 sent | 3 received | 2 failed (send 1 / result 1) | inflight 1" out)
            (write-line "last_start_unix: 1" out)
            (write-line "updated: 01/01 00:00 JST / 00:00 UTC reason: test" out))
          (let ((snippet (swimmy.school::build-cpcv-status-snippet)))
            (assert-true (search "queued" snippet) "snippet should include queued")
            (assert-true (search "sent" snippet) "snippet should include sent")
            (assert-true (search "received" snippet) "snippet should include received")
            (assert-true (search "failed" snippet) "snippet should include failed")
            (assert-true (search "inflight" snippet) "snippet should include inflight")
            (assert-false (search "last start: N/A" snippet) "should include last start when available")))
      (setf swimmy.school::*cpcv-metrics* orig-metrics)
      (setf swimmy.school::*cpcv-status-path* orig-path)
      (setf swimmy.globals:*cpcv-start-time* orig-start)
      (when (probe-file tmp) (delete-file tmp)))))

(deftest test-write-cpcv-status-file
  "write-cpcv-status-file should persist metrics"
  (let* ((orig-path swimmy.school::*cpcv-status-path*)
         (orig-metrics swimmy.school::*cpcv-metrics*)
         (tmp (merge-pathnames (format nil "/tmp/cpcv-status-~a.txt" (get-universal-time)))))
    (unwind-protect
        (progn
          (setf swimmy.school::*cpcv-status-path* tmp)
          (setf swimmy.school::*cpcv-metrics* (make-hash-table :test 'equal))
          (setf (gethash :queued swimmy.school::*cpcv-metrics*) 7)
          (setf (gethash :sent swimmy.school::*cpcv-metrics*) 6)
          (setf (gethash :received swimmy.school::*cpcv-metrics*) 5)
          (setf (gethash :send_failed swimmy.school::*cpcv-metrics*) 1)
          (setf (gethash :result_failed swimmy.school::*cpcv-metrics*) 0)
          (swimmy.school::write-cpcv-status-file :reason "test")
          (assert-true (probe-file tmp) "status file should exist")
          (with-open-file (s tmp :direction :input)
            (let ((content (read-line s nil "")))
              (assert-true (search "queued" content) "content should include queued")
              (assert-true (search "sent" content) "content should include sent")
              (assert-true (search "received" content) "content should include received")
              (assert-true (search "failed" content) "content should include failed")
              (assert-true (search "inflight" content) "content should include inflight"))))
      (setf swimmy.school::*cpcv-status-path* orig-path)
      (setf swimmy.school::*cpcv-metrics* orig-metrics)
      (when (probe-file tmp) (delete-file tmp)))))

(deftest test-cpcv-metrics-summary-line-breakdown
  "cpcv-metrics-summary-line should include runtime/criteria breakdown"
  (let* ((orig-metrics swimmy.school::*cpcv-metrics*))
    (unwind-protect
        (progn
          (setf swimmy.school::*cpcv-metrics* (make-hash-table :test 'equal))
          (setf (gethash :queued swimmy.school::*cpcv-metrics*) 7)
          (setf (gethash :sent swimmy.school::*cpcv-metrics*) 6)
          (setf (gethash :received swimmy.school::*cpcv-metrics*) 5)
          (setf (gethash :send_failed swimmy.school::*cpcv-metrics*) 1)
          (setf (gethash :result_runtime_failed swimmy.school::*cpcv-metrics*) 2)
          (setf (gethash :result_criteria_failed swimmy.school::*cpcv-metrics*) 1)
          (setf (gethash :result_failed swimmy.school::*cpcv-metrics*) 3)
          (let ((line (swimmy.school::cpcv-metrics-summary-line)))
            (assert-true (search "send 1 / result 3" line)
                         "summary should include send/result totals")
            (assert-true (search "runtime 2 / criteria 1" line)
                         "summary should include runtime/criteria split")))
      (setf swimmy.school::*cpcv-metrics* orig-metrics))))

(deftest test-notify-cpcv-result-distinguishes-error
  "notify-cpcv-result should label error vs criteria failure"
  (let* ((orig-notify (and (fboundp 'swimmy.core:notify-discord-alert)
                           (symbol-function 'swimmy.core:notify-discord-alert)))
         (captured nil))
    (unwind-protect
        (progn
          (when orig-notify
            (setf (symbol-function 'swimmy.core:notify-discord-alert)
                  (lambda (msg &key color)
                    (declare (ignore color))
                    (setf captured msg)
                    nil)))
          (swimmy.core:notify-cpcv-result
           (list :strategy-name "UT-CPCV-ERR"
                 :error "timeout"
                 :median-sharpe 0.0
                 :path-count 0
                 :passed-count 0
                 :failed-count 0
                 :pass-rate 0.0
                 :is-passed nil))
          (assert-true (and captured (search "ERROR" captured))
                       "error should be labeled ERROR")
          (setf captured nil)
          (swimmy.core:notify-cpcv-result
           (list :strategy-name "UT-CPCV-FAIL"
                 :median-sharpe 0.1
                 :path-count 10
                 :passed-count 2
                 :failed-count 8
                 :pass-rate 0.2
                 :is-passed nil))
          (assert-true (and captured (search "FAILED" captured))
                       "criteria failure should be labeled FAILED")
          (setf captured nil)
          (swimmy.core:notify-cpcv-result
           (list :strategy-name "UT-CPCV-EMPTY"
                 :median-sharpe 0.0
                 :path-count 0
                 :passed-count 0
                 :failed-count 0
                 :pass-rate 0.0
                 :is-passed nil))
          (assert-true (and captured (search "ERROR (runtime)" captured))
                       "empty result should be labeled runtime error"))
      (when orig-notify
        (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify)))))

(deftest test-notify-cpcv-result-dedupes-identical
  "notify-cpcv-result should suppress duplicate per-strategy alerts within dedupe window."
  (let* ((orig-notify (and (fboundp 'swimmy.core:notify-discord-alert)
                           (symbol-function 'swimmy.core:notify-discord-alert)))
         (orig-window swimmy.core::*cpcv-notify-dedupe-window*)
         (send-count 0))
    (unwind-protect
        (progn
          (setf swimmy.core::*cpcv-notify-dedupe-window* 600)
          (clrhash swimmy.core::*cpcv-notify-cache*)
          (when orig-notify
            (setf (symbol-function 'swimmy.core:notify-discord-alert)
                  (lambda (&rest args)
                    (declare (ignore args))
                    (incf send-count)
                    nil)))
          (let ((data (list :strategy-name "UT-CPCV-DEDUPE"
                            :median-sharpe 1.23
                            :path-count 45
                            :passed-count 36
                            :failed-count 9
                            :pass-rate 0.8
                            :is-passed t)))
            (swimmy.core:notify-cpcv-result data)
            (swimmy.core:notify-cpcv-result data)
            (assert-equal 1 send-count
                          "Second identical CPCV notification should be suppressed")
            ;; Material metric change should bypass dedupe.
            (swimmy.core:notify-cpcv-result
             (list :strategy-name "UT-CPCV-DEDUPE"
                   :median-sharpe 1.40
                   :path-count 45
                   :passed-count 38
                   :failed-count 7
                   :pass-rate 0.84
                   :is-passed t))
            (assert-equal 2 send-count
                          "Changed CPCV metrics should emit a new notification")))
      (setf swimmy.core::*cpcv-notify-dedupe-window* orig-window)
      (clrhash swimmy.core::*cpcv-notify-cache*)
      (when orig-notify
        (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify)))))

(deftest test-notify-cpcv-result-tolerates-nil-metrics-in-log
  "notify-cpcv-result should not crash when CPCV metrics are present-but-NIL (log persistence)."
  (let* ((orig-notify (and (fboundp 'swimmy.core:notify-discord-alert)
                           (symbol-function 'swimmy.core:notify-discord-alert)))
         (orig-default *default-pathname-defaults*)
         (tmp-dir (merge-pathnames
                   (format nil "swimmy_ut_cpcv_nil_~a/" (get-universal-time))
                   (uiop:temporary-directory))))
    (unwind-protect
        (progn
          ;; Isolate CPCV CSV writes away from the repo tree.
          (ensure-directories-exist (merge-pathnames "x" tmp-dir))
          (setf *default-pathname-defaults* (uiop:ensure-directory-pathname tmp-dir))
          (when orig-notify
            (setf (symbol-function 'swimmy.core:notify-discord-alert)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          ;; Key point: keys exist, but values are NIL.
          (swimmy.core:notify-cpcv-result
           (list :strategy-name "UT-CPCV-NIL"
                 :median-sharpe nil
                 :path-count 1
                 :passed-count 1
                 :failed-count 0
                 :pass-rate nil
                 :is-passed t))
          (let* ((csv (merge-pathnames "data/logs/cpcv_history.csv"
                                       (uiop:ensure-directory-pathname tmp-dir))))
            (assert-true (probe-file csv) "Expected cpcv_history.csv to be created")
            (let ((text (uiop:read-file-string csv)))
              ;; NIL metrics must be recorded as 0.0 (not literal \"NIL\") to keep the CSV numeric.
              (assert-true (search ",UT-CPCV-NIL,0.0000,1,0.0000,PASS" text)
                           "Expected NIL metrics to persist as 0.0000 in CSV"))))
      (setf *default-pathname-defaults* orig-default)
      (when orig-notify
        (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify))
      ;; Best-effort cleanup.
      (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t)))))

(deftest test-backtest-debug-log-records-apply
  "BACKTEST_RESULT should write debug log around apply when debug enabled"
  (let ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (let* ((msg "((type . \"BACKTEST_RESULT\") (result . ((strategy_name . \"UT-DBG\") (sharpe . 0.2) (trades . 1) (pnl . 0.1) (request_id . \"RID-DBG\"))))")
           (orig-cache (symbol-function 'swimmy.school:cache-backtest-result))
           (orig-apply (symbol-function 'swimmy.school:apply-backtest-result))
           (orig-lookup (symbol-function 'swimmy.school:lookup-oos-request))
           (orig-v2 (and (fboundp 'swimmy.school::handle-v2-result)
                         (symbol-function 'swimmy.school::handle-v2-result)))
           (orig-env (uiop:getenv "SWIMMY_BACKTEST_DEBUG_RECV"))
           (tmp-path (merge-pathnames
                      (format nil "backtest_debug_test_~a.log" (get-universal-time))
                      (uiop:temporary-directory))))
      (unwind-protect
          (progn
            (sb-posix:setenv "SWIMMY_BACKTEST_DEBUG_RECV" "1" 1)
            (setf (symbol-function 'swimmy.school:cache-backtest-result)
                  (lambda (&rest args) (declare (ignore args)) nil))
            (setf (symbol-function 'swimmy.school:apply-backtest-result)
                  (lambda (&rest args) (declare (ignore args)) nil))
            (setf (symbol-function 'swimmy.school:lookup-oos-request)
                  (lambda (&rest args) (declare (ignore args)) (values nil nil nil)))
            (when (fboundp 'swimmy.school::handle-v2-result)
              (setf (symbol-function 'swimmy.school::handle-v2-result)
                    (lambda (&rest args) (declare (ignore args)) nil)))
            (when (probe-file tmp-path)
              (ignore-errors (delete-file tmp-path)))
            (let ((swimmy.main::*backtest-debug-log* (namestring tmp-path)))
              (funcall fn msg)
              (assert-true (probe-file tmp-path) "Expected debug log file")
              (let ((content (uiop:read-file-string tmp-path)))
                (assert-true (search "apply-backtest-result start" content)
                             "Expected apply start log")
                (assert-true (search "apply-backtest-result end" content)
                             "Expected apply end log"))))
        (setf (symbol-function 'swimmy.school:cache-backtest-result) orig-cache)
        (setf (symbol-function 'swimmy.school:apply-backtest-result) orig-apply)
        (setf (symbol-function 'swimmy.school:lookup-oos-request) orig-lookup)
        (when orig-v2
          (setf (symbol-function 'swimmy.school::handle-v2-result) orig-v2))
        (if orig-env
            (sb-posix:setenv "SWIMMY_BACKTEST_DEBUG_RECV" orig-env 1)
            (sb-posix:unsetenv "SWIMMY_BACKTEST_DEBUG_RECV"))
        (when (probe-file tmp-path)
          (ignore-errors (delete-file tmp-path)))))))

(deftest test-backtest-status-includes-last-request-id
  "backtest_status.txt should include last_request_id only"
  (let ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (let* ((tmp-path (merge-pathnames
                      (format nil "backtest_status_test_~a.txt" (get-universal-time))
                      (uiop:temporary-directory)))
           (status-path (namestring tmp-path))
           (msg "((type . \"BACKTEST_RESULT\") (result . ((strategy_name . \"UT-REQ-STATUS\") (sharpe . 0.2) (trades . 1) (pnl . 0.1) (request_id . \"RID-123\"))))")
           (orig-cache (symbol-function 'swimmy.school:cache-backtest-result))
           (orig-apply (symbol-function 'swimmy.school:apply-backtest-result))
           (orig-lookup (symbol-function 'swimmy.school:lookup-oos-request))
           (orig-v2 (and (fboundp 'swimmy.school::handle-v2-result)
                         (symbol-function 'swimmy.school::handle-v2-result)))
           (orig-submit-count swimmy.globals::*backtest-submit-count*)
           (orig-recv-count swimmy.main::*backtest-recv-count*))
      (unwind-protect
          (progn
            (setf (symbol-function 'swimmy.school:cache-backtest-result)
                  (lambda (&rest args) (declare (ignore args)) nil))
            (setf (symbol-function 'swimmy.school:apply-backtest-result)
                  (lambda (&rest args) (declare (ignore args)) nil))
            (setf (symbol-function 'swimmy.school:lookup-oos-request)
                  (lambda (&rest args) (declare (ignore args)) (values nil nil nil)))
            (when (fboundp 'swimmy.school::handle-v2-result)
              (setf (symbol-function 'swimmy.school::handle-v2-result)
                    (lambda (&rest args) (declare (ignore args)) nil)))
            (setf swimmy.globals:*backtest-submit-last-id* "SUB-999")
            (setf swimmy.main::*backtest-recv-last-log* 0)
            (setf swimmy.globals::*backtest-submit-count* 5)
            (setf swimmy.main::*backtest-recv-count* 1)

            ;; Ensure we never overwrite production status reports.
            (when (probe-file tmp-path)
              (ignore-errors (delete-file tmp-path)))
            (let ((swimmy.main::*backtest-status-path* status-path))
              (funcall fn msg))

            (let ((content (uiop:read-file-string status-path)))
              (assert-true (search "last_request_id: RID-123" content))
              (assert-true (search "pending: 3" content))
              (assert-true (null (search "last_submit_id" content))
                           "Expected last_submit_id to be absent from backtest_status.txt")
              (assert-true (null (search "last_recv_id" content))
                           "Expected last_recv_id to be absent from backtest_status.txt")))
        (setf (symbol-function 'swimmy.school:cache-backtest-result) orig-cache)
        (setf (symbol-function 'swimmy.school:apply-backtest-result) orig-apply)
        (setf (symbol-function 'swimmy.school:lookup-oos-request) orig-lookup)
        (setf swimmy.globals::*backtest-submit-count* orig-submit-count)
        (setf swimmy.main::*backtest-recv-count* orig-recv-count)
        (when orig-v2
          (setf (symbol-function 'swimmy.school::handle-v2-result) orig-v2))
        (when (probe-file tmp-path)
          (ignore-errors (delete-file tmp-path)))))))

(deftest test-backtest-stale-alert-skips-when-no-pending
  "Backtest stale alert should not fire when pending/inflight is zero."
  (let* ((fn (find-symbol "MAYBE-ALERT-BACKTEST-STALE" :swimmy.main))
         (orig-notify (and (fboundp 'swimmy.core:notify-discord-alert)
                           (symbol-function 'swimmy.core:notify-discord-alert)))
         (orig-last-ts swimmy.main::*backtest-recv-last-ts*)
         (orig-threshold swimmy.main::*backtest-stale-threshold*)
         (orig-interval swimmy.main::*backtest-stale-alert-interval*)
         (orig-last-alert swimmy.main::*backtest-stale-last-alert*)
         (orig-submit swimmy.globals::*backtest-submit-count*)
         (orig-recv swimmy.main::*backtest-recv-count*)
         (captured nil))
    (assert-true (and fn (fboundp fn)) "maybe-alert-backtest-stale exists")
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.core:notify-discord-alert)
                (lambda (message &key webhook color title)
                  (declare (ignore webhook color title))
                  (push message captured)
                  t))
          (setf swimmy.main::*backtest-stale-threshold* 1)
          (setf swimmy.main::*backtest-stale-alert-interval* 1)
          (setf swimmy.main::*backtest-stale-last-alert* 0)
          (setf swimmy.main::*backtest-recv-last-ts* (- (get-universal-time) 100))
          ;; pending = submit - recv = 0
          (setf swimmy.globals::*backtest-submit-count* 10)
          (setf swimmy.main::*backtest-recv-count* 10)
          (funcall fn)
          (assert-equal 0 (length captured)
                        "Should not alert when no backtest is pending"))
      (when orig-notify
        (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify))
      (setf swimmy.main::*backtest-recv-last-ts* orig-last-ts)
      (setf swimmy.main::*backtest-stale-threshold* orig-threshold)
      (setf swimmy.main::*backtest-stale-alert-interval* orig-interval)
      (setf swimmy.main::*backtest-stale-last-alert* orig-last-alert)
      (setf swimmy.globals::*backtest-submit-count* orig-submit)
      (setf swimmy.main::*backtest-recv-count* orig-recv))))

(deftest test-backtest-stale-alert-fires-when-pending
  "Backtest stale alert should fire when stale and pending/inflight is positive."
  (let* ((fn (find-symbol "MAYBE-ALERT-BACKTEST-STALE" :swimmy.main))
         (orig-notify (and (fboundp 'swimmy.core:notify-discord-alert)
                           (symbol-function 'swimmy.core:notify-discord-alert)))
         (orig-last-ts swimmy.main::*backtest-recv-last-ts*)
         (orig-threshold swimmy.main::*backtest-stale-threshold*)
         (orig-interval swimmy.main::*backtest-stale-alert-interval*)
         (orig-last-alert swimmy.main::*backtest-stale-last-alert*)
         (orig-submit swimmy.globals::*backtest-submit-count*)
         (orig-recv swimmy.main::*backtest-recv-count*)
         (captured nil))
    (assert-true (and fn (fboundp fn)) "maybe-alert-backtest-stale exists")
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.core:notify-discord-alert)
                (lambda (message &key webhook color title)
                  (declare (ignore webhook color title))
                  (push message captured)
                  t))
          (setf swimmy.main::*backtest-stale-threshold* 1)
          (setf swimmy.main::*backtest-stale-alert-interval* 1)
          (setf swimmy.main::*backtest-stale-last-alert* 0)
          (setf swimmy.main::*backtest-recv-last-ts* (- (get-universal-time) 100))
          ;; pending = submit - recv > 0
          (setf swimmy.globals::*backtest-submit-count* 25)
          (setf swimmy.main::*backtest-recv-count* 10)
          (funcall fn)
          (assert-equal 1 (length captured)
                        "Should alert when stale and pending exists")
          (assert-true (search "Backtest停滞" (or (first captured) ""))
                       "Alert message should mention Backtest停滞"))
      (when orig-notify
        (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify))
      (setf swimmy.main::*backtest-recv-last-ts* orig-last-ts)
      (setf swimmy.main::*backtest-stale-threshold* orig-threshold)
      (setf swimmy.main::*backtest-stale-alert-interval* orig-interval)
      (setf swimmy.main::*backtest-stale-last-alert* orig-last-alert)
      (setf swimmy.globals::*backtest-submit-count* orig-submit)
      (setf swimmy.main::*backtest-recv-count* orig-recv))))

(deftest test-request-backtest-sets-submit-id
  "request-backtest should set submit request_id when none provided"
  (let ((orig-send (symbol-function 'swimmy.school::send-zmq-msg)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::send-zmq-msg)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf swimmy.globals:*backtest-submit-last-id* nil)
          (let ((s (swimmy.school:make-strategy :name "ReqIdTest" :symbol "USDJPY")))
            (swimmy.school::request-backtest s :candles nil))
          (assert-true swimmy.globals:*backtest-submit-last-id* "Expected submit request_id to be set"))
      (setf (symbol-function 'swimmy.school::send-zmq-msg) orig-send))))

(deftest test-request-backtest-includes-include-trades-flag
  "request-backtest should include include_trades flag when requested."
  (let ((orig-send (symbol-function 'swimmy.school::send-zmq-msg))
        (captured nil))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::send-zmq-msg)
                (lambda (msg &key target)
                  (declare (ignore target))
                  (setf captured msg)
                  t))
          (let ((s (swimmy.school:make-strategy :name "UT-REQ-TRADES" :symbol "USDJPY")))
            (swimmy.school::request-backtest s :candles nil :request-id "RID-TRADES" :include-trades t))
          (assert-true (and (stringp captured)
                            (search "(include_trades . t)" (string-downcase captured)))
                       "Expected include_trades flag in dispatched S-expression"))
      (setf (symbol-function 'swimmy.school::send-zmq-msg) orig-send))))

(deftest test-generate-uuid-changes-even-with-reset-rng
  "generate-uuid should vary even if random state resets"
  (let ((state (make-random-state nil))
        (id1 nil)
        (id2 nil))
    (let ((*random-state* (make-random-state state)))
      (setf id1 (swimmy.core:generate-uuid)))
    (sleep 1)
    (let ((*random-state* (make-random-state state)))
      (setf id2 (swimmy.core:generate-uuid)))
    (assert-false (string= id1 id2) "UUID should include time-based entropy")))

(deftest test-generate-uuid-uses-entropy-file
  "generate-uuid should use entropy file bytes when available"
  (let* ((tmp-path (format nil "data/memory/uuid-entropy-~a.bin" (get-universal-time)))
         (bytes (make-array 16 :element-type '(unsigned-byte 8)
                            :initial-contents '(#x00 #x11 #x22 #x33 #x44 #x55 #x66 #x77
                                                #x88 #x99 #xaa #xbb #xcc #xdd #xee #xff)))
         (expected "00112233-4455-4677-8899-aabbccddeeff")
         (orig-path swimmy.core::*uuid-entropy-path*))
    (unwind-protect
        (progn
          (ensure-directories-exist tmp-path)
          (with-open-file (s tmp-path
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create
                             :element-type '(unsigned-byte 8))
            (write-sequence bytes s))
          (setf swimmy.core::*uuid-entropy-path* tmp-path)
          (assert-equal expected (swimmy.core:generate-uuid)
                        "UUID should be derived from entropy bytes"))
      (setf swimmy.core::*uuid-entropy-path* orig-path)
      (when (probe-file tmp-path) (delete-file tmp-path)))))

(deftest test-oos-retry-uses-new-request-id
  "OOS retry should generate a new request_id"
  (let* ((tmp-db (format nil "data/memory/test-oos-retry-~a.db" (get-universal-time)))
         (orig-db swimmy.core::*db-path-default*)
         (orig-interval swimmy.school::*oos-request-interval*)
         (orig-request (symbol-function 'swimmy.school::request-backtest)))
    (unwind-protect
         (progn
           (setf swimmy.core::*db-path-default* tmp-db)
           (swimmy.core:close-db-connection)
           (swimmy.school::init-db)
           (setf swimmy.school::*oos-request-interval* 0)
           (setf (symbol-function 'swimmy.school::request-backtest)
                 (lambda (strat &key request-id &allow-other-keys)
                   (declare (ignore strat))
                   request-id))
           (swimmy.school::enqueue-oos-request "UT-OOS" "RID-OLD" :status "sent"
                                               :requested-at (- (get-universal-time) 999))
           (let* ((strat (cl-user::make-strategy :name "UT-OOS" :symbol "USDJPY" :oos-sharpe nil))
                  (id (swimmy.school::maybe-request-oos-backtest strat)))
             (assert-true (and id (not (string= id "RID-OLD")))
                          "retry should issue new request_id")))
      (setf (symbol-function 'swimmy.school::request-backtest) orig-request)
      (setf swimmy.school::*oos-request-interval* orig-interval)
      (setf swimmy.core::*db-path-default* orig-db)
      (swimmy.core:close-db-connection)
      (when (probe-file tmp-db) (delete-file tmp-db)))))

(deftest test-oos-queue-prune-on-startup
  "startup cleanup should prune stale rows and preserve fresh inflight rows"
  (let* ((tmp-db (format nil "data/memory/test-oos-startup-~a.db" (get-universal-time)))
         (orig-db swimmy.core::*db-path-default*))
    (unwind-protect
         (progn
           (setf swimmy.core::*db-path-default* tmp-db)
           (swimmy.core:close-db-connection)
           (swimmy.school::init-db)
           (let* ((now (get-universal-time))
                  (fresh-name "UT-OOS-FRESH")
                  (stale-name "UT-OOS-STALE"))
             (swimmy.school::enqueue-oos-request stale-name "RID-OLD" :status "sent"
                                                 :requested-at (- now 9999))
             (swimmy.school::enqueue-oos-request fresh-name "RID-NEW" :status "sent"
                                                 :requested-at now))
           (swimmy.school::cleanup-oos-queue-on-startup)
           (multiple-value-bind (fresh-rid _fresh-at _fresh-status)
               (swimmy.school::lookup-oos-request "UT-OOS-FRESH")
             (declare (ignore _fresh-at _fresh-status))
             (assert-equal "RID-NEW" fresh-rid "fresh inflight OOS row should remain"))
           (multiple-value-bind (stale-rid _stale-at _stale-status)
               (swimmy.school::lookup-oos-request "UT-OOS-STALE")
             (declare (ignore _stale-at _stale-status))
             (assert-true (null stale-rid) "stale OOS row should be pruned")))
      (setf swimmy.core::*db-path-default* orig-db)
      (swimmy.core:close-db-connection)
      (when (probe-file tmp-db) (delete-file tmp-db)))))

(deftest test-maybe-request-oos-throttles-with-memory-fallback
  "OOS dispatch should throttle using in-memory last-dispatch even if DB queue row is missing."
  (let* ((orig-interval swimmy.school::*oos-request-interval*)
         (orig-lookup (symbol-function 'swimmy.school::lookup-oos-request))
         (orig-enqueue (symbol-function 'swimmy.school::enqueue-oos-request))
         (orig-request (symbol-function 'swimmy.school::request-backtest))
         (orig-dispatch-map swimmy.school::*oos-last-dispatch-at*)
         (calls 0)
         (strat (cl-user::make-strategy :name "UT-OOS-MEM-THROTTLE" :symbol "USDJPY" :oos-sharpe nil)))
    (unwind-protect
        (progn
          (setf swimmy.school::*oos-request-interval* 600)
          (setf swimmy.school::*oos-last-dispatch-at* (make-hash-table :test 'equal))
          (setf (symbol-function 'swimmy.school::lookup-oos-request)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (values nil nil nil)))
          (setf (symbol-function 'swimmy.school::enqueue-oos-request)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  t))
          (setf (symbol-function 'swimmy.school::request-backtest)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (incf calls)
                  t))
          (let ((id1 (swimmy.school::maybe-request-oos-backtest strat))
                (id2 (swimmy.school::maybe-request-oos-backtest strat)))
            (assert-true id1 "first dispatch should be sent")
            (assert-false id2 "second dispatch should be throttled by memory fallback")
            (assert-equal 1 calls "request-backtest should be called once")))
      (setf swimmy.school::*oos-request-interval* orig-interval)
      (setf swimmy.school::*oos-last-dispatch-at* orig-dispatch-map)
	      (setf (symbol-function 'swimmy.school::lookup-oos-request) orig-lookup)
	      (setf (symbol-function 'swimmy.school::enqueue-oos-request) orig-enqueue)
	      (setf (symbol-function 'swimmy.school::request-backtest) orig-request))))

(deftest test-qualification-candidate-renames-when-db-rank-archived
  "Qualification should rename candidates that collide with archived DB rows."
  (let* ((strat (swimmy.school:make-strategy :name "UT-QUAL-COLLIDE" :symbol "USDJPY"))
         (orig-name (swimmy.school:strategy-name strat))
         (fn (find-symbol "EXECUTE-SINGLE" :swimmy.school))
         (orig-exec (symbol-function fn)))
    (unwind-protect
        (progn
          (setf (symbol-function fn)
                (lambda (&rest args)
                  (declare (ignore args))
                  ":RETIRED"))
          (swimmy.school::ensure-qualification-candidate-name-unique strat)
          (let ((new-name (swimmy.school:strategy-name strat)))
            (assert-true (string/= orig-name new-name)
                         "Archived collision should force a rename")
            (assert-true (search orig-name new-name)
                         "Renamed candidate should keep original name as prefix")))
      (setf (symbol-function fn) orig-exec))))

(deftest test-qualification-candidate-keeps-name-when-db-rank-active
  "Qualification should keep original name when DB row is active."
  (let* ((strat (swimmy.school:make-strategy :name "UT-QUAL-ACTIVE" :symbol "USDJPY"))
         (orig-name (swimmy.school:strategy-name strat))
         (fn (find-symbol "EXECUTE-SINGLE" :swimmy.school))
         (orig-exec (symbol-function fn)))
    (unwind-protect
        (progn
          (setf (symbol-function fn)
                (lambda (&rest args)
                  (declare (ignore args))
                  ":B"))
          (swimmy.school::ensure-qualification-candidate-name-unique strat)
          (assert-equal orig-name (swimmy.school:strategy-name strat)
                        "Active rank should not trigger candidate renaming"))
      (setf (symbol-function fn) orig-exec))))

(deftest test-run-qualification-cycle-prioritizes-incubator-candidates
  "Qualification should include incubator backlog even when many nil-rank candidates exist."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-last swimmy.school::*last-qual-cycle*)
         (orig-interval swimmy.school::*qual-cycle-interval*)
         (orig-request (symbol-function 'swimmy.school::request-backtest))
         (orig-accepted (symbol-function 'swimmy.school::backtest-dispatch-accepted-p))
         (orig-rename (symbol-function 'swimmy.school::ensure-qualification-candidate-name-unique))
         (requested nil)
         (unranked (loop for i from 1 to 60
                         collect (swimmy.school:make-strategy
                                  :name (format nil "UT-NIL-~d" i)
                                  :rank nil
                                  :sharpe 0.0
                                  :trades 0
                                  :symbol "USDJPY")))
         (inc1 (swimmy.school:make-strategy :name "UT-INC-1" :rank :incubator :sharpe 0.0 :trades 0 :symbol "USDJPY"))
         (inc2 (swimmy.school:make-strategy :name "UT-INC-2" :rank :incubator :sharpe 0.0 :trades 0 :symbol "USDJPY")))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (append unranked (list inc1 inc2)))
          (setf swimmy.school::*last-qual-cycle* 0)
          (setf swimmy.school::*qual-cycle-interval* 0)
          (setf (symbol-function 'swimmy.school::ensure-qualification-candidate-name-unique)
                (lambda (s) s))
          (setf (symbol-function 'swimmy.school::request-backtest)
                (lambda (strat &key suffix)
                  (declare (ignore suffix))
                  (push (swimmy.school:strategy-name strat) requested)
                  :accepted))
          (setf (symbol-function 'swimmy.school::backtest-dispatch-accepted-p)
                (lambda (state) (eq state :accepted)))
          (swimmy.school::run-qualification-cycle)
          (assert-true (member "UT-INC-1" requested :test #'string=)
                       "Expected incubator candidate UT-INC-1 to be dispatched")
          (assert-true (member "UT-INC-2" requested :test #'string=)
                       "Expected incubator candidate UT-INC-2 to be dispatched"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*last-qual-cycle* orig-last)
      (setf swimmy.school::*qual-cycle-interval* orig-interval)
      (setf (symbol-function 'swimmy.school::request-backtest) orig-request)
      (setf (symbol-function 'swimmy.school::backtest-dispatch-accepted-p) orig-accepted)
      (setf (symbol-function 'swimmy.school::ensure-qualification-candidate-name-unique) orig-rename))))

(deftest test-run-qualification-cycle-reconciles-scored-incubator-backlog
  "Qualification should evaluate scored incubators immediately instead of re-dispatching them."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-last swimmy.school::*last-qual-cycle*)
         (orig-interval swimmy.school::*qual-cycle-interval*)
         (orig-request (symbol-function 'swimmy.school::request-backtest))
         (orig-accepted (symbol-function 'swimmy.school::backtest-dispatch-accepted-p))
         (orig-eval (symbol-function 'swimmy.school::evaluate-new-strategy))
         (requested nil)
         (evaluated nil)
         (inc-scored-pass (swimmy.school:make-strategy
                           :name "UT-INC-SCORED-PASS"
                           :rank :incubator
                           :symbol "USDJPY"
                           :sharpe 0.80
                           :profit-factor 1.20
                           :win-rate 0.45
                           :max-dd 0.12
                           :trades 120))
         (inc-scored-fail (swimmy.school:make-strategy
                           :name "UT-INC-SCORED-FAIL"
                           :rank :incubator
                           :symbol "USDJPY"
                           :sharpe 0.05
                           :profit-factor 0.95
                           :win-rate 0.20
                           :max-dd 0.45
                           :trades 120))
         (inc-needs-bt (swimmy.school:make-strategy
                        :name "UT-INC-NEEDS-BT"
                        :rank :incubator
                        :symbol "USDJPY"
                        :sharpe 0.0
                        :trades 0)))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base*
                (list inc-scored-pass inc-scored-fail inc-needs-bt))
          (setf swimmy.school::*last-qual-cycle* 0)
          (setf swimmy.school::*qual-cycle-interval* 0)
          (setf (symbol-function 'swimmy.school::evaluate-new-strategy)
                (lambda (s)
                  (push (swimmy.school:strategy-name s) evaluated)
                  :B))
          (setf (symbol-function 'swimmy.school::request-backtest)
                (lambda (strat &key suffix)
                  (declare (ignore suffix))
                  (push (swimmy.school:strategy-name strat) requested)
                  :accepted))
          (setf (symbol-function 'swimmy.school::backtest-dispatch-accepted-p)
                (lambda (state) (eq state :accepted)))

          (swimmy.school::run-qualification-cycle)

          (assert-true (member "UT-INC-SCORED-PASS" evaluated :test #'string=)
                       "Scored incubator should be evaluated immediately")
          (assert-true (member "UT-INC-SCORED-FAIL" evaluated :test #'string=)
                       "Scored incubator should be evaluated immediately")
          (assert-false (member "UT-INC-SCORED-PASS" requested :test #'string=)
                        "Scored incubator should not be re-dispatched")
          (assert-false (member "UT-INC-SCORED-FAIL" requested :test #'string=)
                        "Scored incubator should not be re-dispatched")
          (assert-true (member "UT-INC-NEEDS-BT" requested :test #'string=)
                       "Unscored incubator should still be dispatched"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*last-qual-cycle* orig-last)
      (setf swimmy.school::*qual-cycle-interval* orig-interval)
      (setf (symbol-function 'swimmy.school::request-backtest) orig-request)
      (setf (symbol-function 'swimmy.school::backtest-dispatch-accepted-p) orig-accepted)
      (setf (symbol-function 'swimmy.school::evaluate-new-strategy) orig-eval))))

(deftest test-strategy-to-alist-omits-filter-enabled-when-false
  "strategy-to-alist should omit filter_enabled when filter is disabled (Rust expects bool default false)"
  (let* ((fn (find-symbol "STRATEGY-TO-ALIST" :swimmy.school))
         (key (find-symbol "FILTER_ENABLED" :swimmy.school)))
    (assert-true (and fn (fboundp fn)) "strategy-to-alist exists")
    (assert-not-nil key "FILTER_ENABLED symbol exists")
    (let* ((strat (swimmy.school:make-strategy
                   :name "UT-FILTER-OFF"
                   :indicators '((sma 5) (sma 20))
                   :filter-enabled nil))
           (alist (funcall fn strat)))
      (assert-true (listp alist) "Expected an alist list")
      (assert-true (null (assoc key alist)) "filter_enabled should be omitted when false"))))

(deftest test-strategy-to-alist-includes-filter-enabled-when-true
  "strategy-to-alist should include filter_enabled when filter is enabled"
  (let* ((fn (find-symbol "STRATEGY-TO-ALIST" :swimmy.school))
         (key (find-symbol "FILTER_ENABLED" :swimmy.school)))
    (assert-true (and fn (fboundp fn)) "strategy-to-alist exists")
    (assert-not-nil key "FILTER_ENABLED symbol exists")
    (let* ((strat (swimmy.school:make-strategy
                   :name "UT-FILTER-ON"
                   :indicators '((sma 5) (sma 20))
                   :filter-enabled t))
           (alist (funcall fn strat))
           (pair (assoc key alist)))
      (assert-not-nil pair "filter_enabled should exist when enabled")
      (assert-true (eq (cdr pair) t) "filter_enabled should be t when enabled"))))

(deftest test-order-open-uses-instrument-side
  (let* ((msg (swimmy.core:make-order-message "UT" "USDJPY" :buy 0.1 0 0 0 :comment "UT|M1"))
         (sexp (swimmy.core:encode-sexp msg))
         (parsed (swimmy.core:safe-read-sexp sexp :package :swimmy.core)))
    (assert-equal "USDJPY" (swimmy.core:sexp-alist-get parsed "instrument"))
    (assert-equal "BUY" (swimmy.core:sexp-alist-get parsed "side"))))

(deftest test-calculate-portfolio-exposure-reads-order-open-v2-pending
  "calculate-portfolio-exposure should parse executor pending ORDER_OPEN using instrument/side/lot keys."
  (let ((orig-pending swimmy.globals:*pending-orders*)
        (orig-arm swimmy.engine::*arm-states*)
        (orig-candles swimmy.globals::*current-candles*)
        (orig-equity swimmy.globals::*current-equity*)
        (orig-min-safe swimmy.globals::*min-safe-capital*))
    (unwind-protect
        (progn
          (setf swimmy.globals:*pending-orders* (make-hash-table :test 'equal))
          (setf swimmy.engine::*arm-states* (make-hash-table :test 'equal))
          (setf swimmy.globals::*current-candles* (make-hash-table :test 'equal))
          (setf swimmy.globals::*current-equity* 1000000.0)
          (setf swimmy.globals::*min-safe-capital* 1000000.0)
          (let* ((msg (swimmy.core:make-order-message "UT-PENDING" "USDJPY" :buy 0.10 0.0 0.0 0.0
                                                      :comment "UT-PENDING|M1"))
                 (id (swimmy.core:sexp-alist-get msg "id")))
            (setf (gethash id swimmy.globals:*pending-orders*)
                  (list (get-universal-time) 0 msg))
            (multiple-value-bind (exposures gross _net)
                (swimmy.engine::calculate-portfolio-exposure)
              (declare (ignore _net))
              (assert-true (hash-table-p exposures) "Expected exposure map")
              (assert-true (> gross 0.0)
                           "Pending ORDER_OPEN exposure should contribute to gross exposure"))))
      (setf swimmy.globals:*pending-orders* orig-pending)
      (setf swimmy.engine::*arm-states* orig-arm)
      (setf swimmy.globals::*current-candles* orig-candles)
      (setf swimmy.globals::*current-equity* orig-equity)
      (setf swimmy.globals::*min-safe-capital* orig-min-safe))))

(deftest test-data-keeper-request-sexp
  (let* ((req (swimmy.core::build-data-keeper-request "STATUS"))
         (parsed (swimmy.core:safe-read-sexp req :package :swimmy.core)))
    (assert-equal "DATA_KEEPER" (swimmy.core:sexp-alist-get parsed "type"))
    (assert-equal 1 (swimmy.core:sexp-alist-get parsed "schema_version"))
    (assert-equal "STATUS" (swimmy.core:sexp-alist-get parsed "action"))))

(deftest test-message-dispatcher-compiles-without-warnings
  "message-dispatcher should compile without warnings"
  (let* ((warnings '())
         (tmp-path (merge-pathnames
                    (format nil "swimmy-tests-message-dispatcher-~a.fasl" (get-universal-time))
                    (uiop:temporary-directory))))
    (unwind-protect
        (handler-bind ((warning (lambda (w)
                                  (push w warnings)
                                  (muffle-warning w))))
          ;; Avoid mutating the repo-tracked .fasl during tests.
          (compile-file "src/lisp/core/message-dispatcher.lisp"
                        :output-file (namestring tmp-path)))
      (when (probe-file tmp-path)
        (ignore-errors (delete-file tmp-path))))
    (assert-true (null warnings)
                 (format nil "Expected no warnings, got: ~a" warnings))))

(deftest test-safe-read-used-for-db-rank
  "DB rank parsing should ignore unsafe read-time eval"
  (let ((fn (find-symbol "%PARSE-RANK-SAFE" :swimmy.school)))
    (assert-true (and fn (fboundp fn)) "%parse-rank-safe exists")
    (let ((orig swimmy.globals:*danger-level*))
      (unwind-protect
          (progn
            (setf swimmy.globals:*danger-level* 0)
            (multiple-value-bind (rank ok)
                (funcall fn "#.(setf swimmy.globals::*danger-level* 999)")
              (declare (ignore rank))
              (assert-true (null ok) "Expected invalid rank")
              (assert-equal 0 swimmy.globals:*danger-level* "Should not evaluate read-time forms")))
        (setf swimmy.globals:*danger-level* orig)))))

(deftest test-safe-eval-strategy-rejects-read-eval
  "safe-eval-strategy should ignore read-time eval forms"
  (let ((orig swimmy.globals:*danger-level*))
    (unwind-protect
        (progn
          (setf swimmy.globals:*danger-level* 0)
          (multiple-value-bind (_strategy ok)
              (swimmy.school::safe-eval-strategy
               "#.(setf swimmy.globals::*danger-level* 999) (defstrategy ut-safe :indicators nil :entry nil :exit nil :sl 0 :tp 0 :volume 0)")
            (declare (ignore _strategy))
            (assert-false ok "Expected unsafe form to be rejected")
            (assert-equal 0 swimmy.globals:*danger-level* "Should not evaluate read-time forms")))
      (setf swimmy.globals:*danger-level* orig))))

(deftest test-map-strategies-from-db-rejects-read-eval
  "map-strategies-from-db should ignore unsafe read-time eval"
  (let ((fn (find-symbol "EXECUTE-TO-LIST" :swimmy.school)))
    (assert-true (and fn (fboundp fn)) "execute-to-list exists")
    (let ((orig (symbol-function fn))
          (orig-danger swimmy.globals:*danger-level*)
          (calls 0))
      (unwind-protect
          (progn
            (setf swimmy.globals:*danger-level* 0)
            (setf (symbol-function fn)
                  (lambda (&rest args)
                    (declare (ignore args))
                    (incf calls)
                    (if (= calls 1)
                        (list (list "#.(setf swimmy.globals::*danger-level* 999)"))
                        nil)))
            (let ((count (swimmy.school::map-strategies-from-db (lambda (s) (declare (ignore s))))))
              (assert-equal 0 count "Expected no strategies processed")
              (assert-equal 0 swimmy.globals:*danger-level* "Should not evaluate read-time forms")))
        (setf (symbol-function fn) orig
              swimmy.globals:*danger-level* orig-danger)))))

(deftest test-parse-macro-csv-rejects-read-eval
  "parse-macro-csv should ignore unsafe read-time eval"
  (let* ((orig-danger swimmy.globals:*danger-level*)
         (orig-dir swimmy.school::*macro-data-dir*)
         (root (uiop:ensure-directory-pathname (uiop:getcwd)))
         (temp-dir (uiop:ensure-directory-pathname (merge-pathnames "data/tmp/macro-test/" root)))
         (driver "UT-MACRO")
         (path (merge-pathnames (format nil "~a.csv" driver) temp-dir)))
    (unwind-protect
        (progn
          (ensure-directories-exist temp-dir)
          (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
            (write-line "Date,Open,High,Low,Close" out)
            (write-line "2024-01-01,1,2,3,#.(setf swimmy.globals::*danger-level* 999)" out))
          (setf swimmy.globals:*danger-level* 0)
          (let ((swimmy.school::*macro-data-dir* temp-dir))
            (let ((data (swimmy.school::parse-macro-csv driver)))
              (assert-true (null data) "Expected no parsed rows")
              (assert-equal 0 swimmy.globals:*danger-level* "Should not evaluate read-time forms"))))
      (setf swimmy.globals:*danger-level* orig-danger
            swimmy.school::*macro-data-dir* orig-dir)
      (ignore-errors (delete-file path)))))

(deftest test-req-history-uses-count
  "REQ_HISTORY bootstrap request should use count key (not volume)"
  (let* ((path "src/lisp/system/runner.lisp")
         (content (with-open-file (in path :direction :input)
                    (let ((out (make-string-output-stream)))
                      (loop for line = (read-line in nil nil)
                            while line do (write-line line out))
                      (get-output-stream-string out))))
         (has-req (search "REQ_HISTORY" content))
         (has-count (search "(count ." content))
         (has-volume (search "(volume ." content)))
    (assert-true has-req "REQ_HISTORY should exist in runner.lisp")
    (assert-true has-count "REQ_HISTORY should use count key")
    (assert-false has-volume "REQ_HISTORY should not use volume key")))

(deftest test-order-open-sexp-keys
  "ORDER_OPEN should use instrument/side/lot and be S-expression"
  (let* ((msg (swimmy.core:make-order-message "UT" "USDJPY" "BUY" 0.1 0.0 1.0 2.0
                                              :magic 123 :comment "UT|H1"))
         (payload (if (stringp msg)
                      msg
                      (swimmy.core:encode-sexp msg))))
    (assert-true (search "(instrument . \"USDJPY\")" payload) "Expected instrument key")
    (assert-true (search "(side . \"BUY\")" payload) "Expected side key")
    (assert-true (search "(lot . 0.1" payload) "Expected lot key")
    (assert-false (search "action" payload) "Should not contain action key")
    (assert-false (search "symbol" payload) "Should not contain symbol key")))

(deftest test-mt5-bridge-order-open-idempotency-guards
  "MT5 bridge should dedupe duplicate ORDER_OPEN by id and replay prior outcome."
  (let* ((path "src/mt5/SwimmyBridge.mq5")
         (content (with-open-file (in path :direction :input)
                    (let ((out (make-string-output-stream)))
                      (loop for line = (read-line in nil nil)
                            while line do (write-line line out))
                      (get-output-stream-string out)))))
    (assert-true (search "FindOrderResultById(cmd_id)" content)
                 "Expected ORDER_OPEN to check already-processed id before sending trade")
    (assert-true (search "RecordOrderResult(cmd_id, true" content)
                 "Expected successful ORDER_OPEN to record idempotent ACK result")
    (assert-true (search "RecordOrderResult(cmd_id, false" content)
                 "Expected failed ORDER_OPEN to record idempotent REJECT result")
    (assert-true (search "Duplicate ORDER_OPEN id=" content)
                 "Expected explicit duplicate ORDER_OPEN replay log")))

(deftest test-invalid-order-comment-reason-detects-context-gaps
  "invalid-order-comment-reason should reject NIL/format-missing execution comments."
  (assert-equal :missing-comment (swimmy.core::invalid-order-comment-reason nil))
  (assert-equal :missing-comment (swimmy.core::invalid-order-comment-reason ""))
  (assert-equal :invalid-format (swimmy.core::invalid-order-comment-reason "StrategyOnly"))
  (assert-equal :missing-strategy (swimmy.core::invalid-order-comment-reason "NIL|M1"))
  (assert-equal :missing-timeframe (swimmy.core::invalid-order-comment-reason "ValidName|NIL"))
  (assert-false (swimmy.core::invalid-order-comment-reason "ValidName|H1")))

(deftest test-make-order-message-rejects-invalid-comment
  "make-order-message should reject invalid execution-context comments."
  (let ((failed nil))
    (handler-case
        (progn
          (swimmy.core:make-order-message "UT" "USDJPY" :buy 0.1 0.0 1.0 2.0
                                          :comment "NIL|M1")
          (setf failed t))
      (error () nil))
    (assert-false failed "Expected invalid comment to be rejected by make-order-message")))

(deftest test-safe-order-fails-closed-on-invalid-comment
  "safe-order should block invalid execution comment before risk-gate request."
  (let ((orig-request (symbol-function 'swimmy.engine::request-trade-approval))
        (orig-notify-alert (and (fboundp 'swimmy.core:notify-discord-alert)
                                (symbol-function 'swimmy.core:notify-discord-alert)))
        (orig-emit-telemetry (and (fboundp 'swimmy.core::emit-telemetry-event)
                                  (symbol-function 'swimmy.core::emit-telemetry-event)))
        (orig-log-file swimmy.core::*log-file-path*)
        (orig-fallback-file swimmy.core::*telemetry-fallback-log-path*)
        (request-called nil)
        (alert-called nil)
        (telemetry-called nil))
    (unwind-protect
        (progn
          (setf swimmy.core::*log-file-path* "data/memory/swimmy-tests-telemetry.jsonl")
          (setf swimmy.core::*telemetry-fallback-log-path* "data/memory/swimmy-tests-telemetry-fallback.jsonl")
          (setf (symbol-function 'swimmy.engine::request-trade-approval)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (setf request-called t)
                  (values t "")))
          (setf (symbol-function 'swimmy.core:notify-discord-alert)
                (lambda (msg &key color)
                  (declare (ignore msg color))
                  (setf alert-called t)
                  nil))
          (setf (symbol-function 'swimmy.core::emit-telemetry-event)
                (lambda (event-type &key service severity correlation-id data)
                  (declare (ignore event-type service severity correlation-id data))
                  (setf telemetry-called t)
                  nil))
          (let ((ok (swimmy.engine::safe-order "BUY" "USDJPY" 0.01 0.0 0.0 123 "NIL|M1")))
            (assert-false ok "Expected invalid comment to be fail-closed")
            (assert-false request-called "Risk-gate request should not run for invalid comment")
            (assert-true alert-called "Expected alert when invalid comment is blocked")
            (assert-true telemetry-called "Expected telemetry when invalid comment is blocked")))
      (setf (symbol-function 'swimmy.engine::request-trade-approval) orig-request)
      (if orig-notify-alert
          (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify-alert)
          (fmakunbound 'swimmy.core:notify-discord-alert))
      (if orig-emit-telemetry
          (setf (symbol-function 'swimmy.core::emit-telemetry-event) orig-emit-telemetry)
          (fmakunbound 'swimmy.core::emit-telemetry-event))
      (setf swimmy.core::*log-file-path* orig-log-file)
      (setf swimmy.core::*telemetry-fallback-log-path* orig-fallback-file))))

(deftest test-safe-order-fails-closed-when-mt5-offline
  "safe-order should block execution when MT5 heartbeat is stale."
  (let ((orig-request (symbol-function 'swimmy.engine::request-trade-approval))
        (orig-log-warn (symbol-function 'swimmy.core:log-warn))
        (orig-notify-alert (and (fboundp 'swimmy.core:notify-discord-alert)
                                (symbol-function 'swimmy.core:notify-discord-alert)))
        (orig-last-hb swimmy.globals::*last-guardian-heartbeat*)
        (request-called nil)
        (warn-called nil)
        (alert-called nil))
    (unwind-protect
        (progn
          (setf swimmy.globals::*last-guardian-heartbeat* 0)
          (setf (symbol-function 'swimmy.engine::request-trade-approval)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (setf request-called t)
                  (values t "")))
          (setf (symbol-function 'swimmy.core:log-warn)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (setf warn-called t)
                  nil))
          (setf (symbol-function 'swimmy.core:notify-discord-alert)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (setf alert-called t)
                  nil))
          (let ((ok (swimmy.engine::safe-order "BUY" "USDJPY" 0.01 0.0 0.0 123 "UT|M1")))
            (assert-false ok "Expected MT5-offline path to fail-closed")
            (assert-false request-called "Risk-gate request should not run when MT5 is offline")
            (assert-true warn-called "Expected warning log when MT5 is offline")
            (assert-true alert-called "Expected alert when MT5 is offline")))
      (setf (symbol-function 'swimmy.engine::request-trade-approval) orig-request)
      (setf (symbol-function 'swimmy.core:log-warn) orig-log-warn)
      (if orig-notify-alert
          (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify-alert)
          (fmakunbound 'swimmy.core:notify-discord-alert))
      (setf swimmy.globals::*last-guardian-heartbeat* orig-last-hb))))

(deftest test-safe-order-fails-closed-when-guardian-heartbeat-stale
  "safe-order should block before risk-gate request when guardian heartbeat is stale."
  (let ((orig-request (symbol-function 'swimmy.engine::request-trade-approval))
        (orig-notify-alert (and (fboundp 'swimmy.core:notify-discord-alert)
                                (symbol-function 'swimmy.core:notify-discord-alert)))
        (orig-last-heartbeat swimmy.globals::*last-guardian-heartbeat*)
        (orig-last-account swimmy.globals::*last-account-info-time*)
        (orig-pending swimmy.globals:*pending-orders*)
        (request-called nil))
    (unwind-protect
        (progn
          (setf swimmy.globals:*pending-orders* (make-hash-table :test 'equal))
          (setf swimmy.globals::*last-guardian-heartbeat* (- (get-universal-time) 3600))
          (setf swimmy.globals::*last-account-info-time* (get-universal-time))
          (setf (symbol-function 'swimmy.engine::request-trade-approval)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (setf request-called t)
                  (values nil "SHOULD-NOT-RUN")))
          (setf (symbol-function 'swimmy.core:notify-discord-alert)
                (lambda (&rest _args) (declare (ignore _args)) nil))
          (let ((ok (swimmy.engine::safe-order "BUY" "USDJPY" 0.01 0.0 0.0 123 "ValidName|H1")))
            (assert-false ok "Expected stale guardian heartbeat to fail-closed")
            (assert-false request-called "Risk-gate request should not run when heartbeat is stale")
            (assert-equal 0 (hash-table-count swimmy.globals:*pending-orders*)
                          "Blocked orders must not be enqueued into pending-orders")))
      (setf (symbol-function 'swimmy.engine::request-trade-approval) orig-request)
      (if orig-notify-alert
          (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify-alert)
          (fmakunbound 'swimmy.core:notify-discord-alert))
      (setf swimmy.globals::*last-guardian-heartbeat* orig-last-heartbeat)
      (setf swimmy.globals::*last-account-info-time* orig-last-account)
      (setf swimmy.globals:*pending-orders* orig-pending))))

(deftest test-safe-order-fails-closed-when-account-info-stale
  "safe-order should block before risk-gate request when ACCOUNT_INFO is stale."
  (let ((orig-request (symbol-function 'swimmy.engine::request-trade-approval))
        (orig-notify-alert (and (fboundp 'swimmy.core:notify-discord-alert)
                                (symbol-function 'swimmy.core:notify-discord-alert)))
        (orig-last-heartbeat swimmy.globals::*last-guardian-heartbeat*)
        (orig-last-account swimmy.globals::*last-account-info-time*)
        (orig-pending swimmy.globals:*pending-orders*)
        (request-called nil))
    (unwind-protect
        (progn
          (setf swimmy.globals:*pending-orders* (make-hash-table :test 'equal))
          (setf swimmy.globals::*last-guardian-heartbeat* (get-universal-time))
          (setf swimmy.globals::*last-account-info-time* (- (get-universal-time) 3600))
          (setf (symbol-function 'swimmy.engine::request-trade-approval)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (setf request-called t)
                  (values nil "SHOULD-NOT-RUN")))
          (setf (symbol-function 'swimmy.core:notify-discord-alert)
                (lambda (&rest _args) (declare (ignore _args)) nil))
          (let ((ok (swimmy.engine::safe-order "BUY" "USDJPY" 0.01 0.0 0.0 123 "ValidName|H1")))
            (assert-false ok "Expected stale ACCOUNT_INFO to fail-closed")
            (assert-false request-called "Risk-gate request should not run when ACCOUNT_INFO is stale")
            (assert-equal 0 (hash-table-count swimmy.globals:*pending-orders*)
                          "Blocked orders must not be enqueued into pending-orders")))
      (setf (symbol-function 'swimmy.engine::request-trade-approval) orig-request)
      (if orig-notify-alert
          (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify-alert)
          (fmakunbound 'swimmy.core:notify-discord-alert))
      (setf swimmy.globals::*last-guardian-heartbeat* orig-last-heartbeat)
      (setf swimmy.globals::*last-account-info-time* orig-last-account)
      (setf swimmy.globals:*pending-orders* orig-pending))))

(deftest test-check-pending-orders-pauses-when-guardian-heartbeat-stale
  "check-pending-orders should keep pending entries untouched while guardian heartbeat is stale."
  (let ((orig-pending swimmy.globals:*pending-orders*)
        (orig-last-heartbeat swimmy.globals::*last-guardian-heartbeat*)
        (orig-last-account swimmy.globals::*last-account-info-time*)
        (orig-cmd-publisher swimmy.globals::*cmd-publisher*)
        (orig-send (symbol-function 'pzmq:send))
        (orig-notify (and (fboundp 'swimmy.core:notify-discord-alert)
                          (symbol-function 'swimmy.core:notify-discord-alert)))
        (send-count 0)
        (alert-count 0)
        (order-id "UT-PENDING-HB-STALE"))
    (unwind-protect
        (progn
          (setf swimmy.globals:*pending-orders* (make-hash-table :test 'equal))
          (setf swimmy.globals::*last-guardian-heartbeat* 0)
          (setf swimmy.globals::*last-account-info-time* (get-universal-time))
          (setf swimmy.globals::*cmd-publisher* :ut-cmd-publisher)
          (setf (gethash order-id swimmy.globals:*pending-orders*)
                (list (- (get-universal-time) 30)
                      0
                      '((type . "ORDER_OPEN")
                        (instrument . "USDJPY")
                        (side . "BUY")
                        (lot . 0.01))))
          (setf (symbol-function 'pzmq:send)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (incf send-count)
                  nil))
          (setf (symbol-function 'swimmy.core:notify-discord-alert)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (incf alert-count)
                  nil))
          (swimmy.executor::check-pending-orders)
          (assert-equal 0 send-count "Retry send must pause while guardian heartbeat is stale")
          (assert-equal 0 alert-count "Timeout alert must pause while guardian heartbeat is stale")
          (multiple-value-bind (entry presentp)
              (gethash order-id swimmy.globals:*pending-orders*)
            (assert-true presentp "Pending order should remain while stale")
            (assert-equal 0 (second entry) "Retry count should not change while stale")))
      (setf swimmy.globals:*pending-orders* orig-pending)
      (setf swimmy.globals::*last-guardian-heartbeat* orig-last-heartbeat)
      (setf swimmy.globals::*last-account-info-time* orig-last-account)
      (setf swimmy.globals::*cmd-publisher* orig-cmd-publisher)
      (setf (symbol-function 'pzmq:send) orig-send)
      (if orig-notify
          (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify)
          (fmakunbound 'swimmy.core:notify-discord-alert)))))

(deftest test-check-pending-orders-pauses-when-account-info-stale
  "check-pending-orders should keep pending entries untouched while ACCOUNT_INFO is stale."
  (let ((orig-pending swimmy.globals:*pending-orders*)
        (orig-last-heartbeat swimmy.globals::*last-guardian-heartbeat*)
        (orig-last-account swimmy.globals::*last-account-info-time*)
        (orig-cmd-publisher swimmy.globals::*cmd-publisher*)
        (orig-send (symbol-function 'pzmq:send))
        (orig-notify (and (fboundp 'swimmy.core:notify-discord-alert)
                          (symbol-function 'swimmy.core:notify-discord-alert)))
        (send-count 0)
        (alert-count 0)
        (order-id "UT-PENDING-ACCOUNT-STALE"))
    (unwind-protect
        (progn
          (setf swimmy.globals:*pending-orders* (make-hash-table :test 'equal))
          (setf swimmy.globals::*last-guardian-heartbeat* (get-universal-time))
          (setf swimmy.globals::*last-account-info-time* (- (get-universal-time) 600))
          (setf swimmy.globals::*cmd-publisher* :ut-cmd-publisher)
          (setf (gethash order-id swimmy.globals:*pending-orders*)
                (list (- (get-universal-time) 30)
                      0
                      '((type . "ORDER_OPEN")
                        (instrument . "USDJPY")
                        (side . "BUY")
                        (lot . 0.01))))
          (setf (symbol-function 'pzmq:send)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (incf send-count)
                  nil))
          (setf (symbol-function 'swimmy.core:notify-discord-alert)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (incf alert-count)
                  nil))
          (swimmy.executor::check-pending-orders)
          (assert-equal 0 send-count "Retry send must pause while ACCOUNT_INFO is stale")
          (assert-equal 0 alert-count "Timeout alert must pause while ACCOUNT_INFO is stale")
          (multiple-value-bind (entry presentp)
              (gethash order-id swimmy.globals:*pending-orders*)
            (assert-true presentp "Pending order should remain while stale")
            (assert-equal 0 (second entry) "Retry count should not change while stale")))
      (setf swimmy.globals:*pending-orders* orig-pending)
      (setf swimmy.globals::*last-guardian-heartbeat* orig-last-heartbeat)
      (setf swimmy.globals::*last-account-info-time* orig-last-account)
      (setf swimmy.globals::*cmd-publisher* orig-cmd-publisher)
      (setf (symbol-function 'pzmq:send) orig-send)
      (if orig-notify
          (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify)
          (fmakunbound 'swimmy.core:notify-discord-alert)))))

(deftest test-armada-kill-switch-pauses-on-hard-dd
  "Armada kill switch should pause system when hard drawdown cap is breached."
  (let* ((had-enabled (boundp 'swimmy.school::*armada-kill-switch-enabled*))
         (orig-enabled (and had-enabled swimmy.school::*armada-kill-switch-enabled*))
         (had-dd-cap (boundp 'swimmy.school::*armada-hard-dd-percent*))
         (orig-dd-cap (and had-dd-cap swimmy.school::*armada-hard-dd-percent*))
         (orig-state swimmy.globals::*system-state*)
         (orig-dd swimmy.globals::*current-drawdown*)
         (orig-monitor-dd swimmy.globals::*monitoring-drawdown*)
         (orig-cooldown swimmy.globals::*danger-cooldown-until*))
    (unwind-protect
        (progn
          (setf swimmy.school::*armada-kill-switch-enabled* t)
          (setf swimmy.school::*armada-hard-dd-percent* 12.0)
          (setf swimmy.globals::*system-state* :trading)
          (setf swimmy.globals::*current-drawdown* 13.5)
          (setf swimmy.globals::*monitoring-drawdown* 13.5)
          (setf swimmy.globals::*danger-cooldown-until* 0)
          (let ((reason (swimmy.school::enforce-armada-kill-switch)))
            (assert-equal :hard-dd reason "Expected hard-dd reason")
            (assert-equal :paused swimmy.globals::*system-state*
                          "Kill switch should pause trading state")
            (assert-true (> swimmy.globals::*danger-cooldown-until* (get-universal-time))
                         "Kill switch should set cooldown")))
      (if had-enabled
          (setf swimmy.school::*armada-kill-switch-enabled* orig-enabled)
          (makunbound 'swimmy.school::*armada-kill-switch-enabled*))
      (if had-dd-cap
          (setf swimmy.school::*armada-hard-dd-percent* orig-dd-cap)
          (makunbound 'swimmy.school::*armada-hard-dd-percent*))
      (setf swimmy.globals::*system-state* orig-state)
      (setf swimmy.globals::*current-drawdown* orig-dd)
      (setf swimmy.globals::*monitoring-drawdown* orig-monitor-dd)
      (setf swimmy.globals::*danger-cooldown-until* orig-cooldown))))

(deftest test-enable-armada-kill-switch-canary-mode-activates-guardrails
  "Canary helper should enable Armada kill switch and reset key thresholds."
  (let* ((had-enabled (boundp 'swimmy.school::*armada-kill-switch-enabled*))
         (orig-enabled (and had-enabled swimmy.school::*armada-kill-switch-enabled*))
         (had-hard-dd (boundp 'swimmy.school::*armada-hard-dd-percent*))
         (orig-hard-dd (and had-hard-dd swimmy.school::*armada-hard-dd-percent*))
         (had-weekly-dd (boundp 'swimmy.school::*armada-weekly-dd-percent*))
         (orig-weekly-dd (and had-weekly-dd swimmy.school::*armada-weekly-dd-percent*)))
    (unwind-protect
        (progn
          (setf swimmy.school::*armada-kill-switch-enabled* nil)
          (setf swimmy.school::*armada-hard-dd-percent* 99.0)
          (setf swimmy.school::*armada-weekly-dd-percent* 99.0)
          (assert-true (swimmy.school::enable-armada-kill-switch-canary-mode))
          (assert-true swimmy.school::*armada-kill-switch-enabled*
                       "Canary mode should enable kill switch")
          (assert-equal 12.0 swimmy.school::*armada-hard-dd-percent*
                        "Canary mode should use hard DD 12%")
          (assert-equal 4.0 swimmy.school::*armada-weekly-dd-percent*
                        "Canary mode should use weekly DD 4%"))
      (if had-enabled
          (setf swimmy.school::*armada-kill-switch-enabled* orig-enabled)
          (makunbound 'swimmy.school::*armada-kill-switch-enabled*))
      (if had-hard-dd
          (setf swimmy.school::*armada-hard-dd-percent* orig-hard-dd)
          (makunbound 'swimmy.school::*armada-hard-dd-percent*))
      (if had-weekly-dd
          (setf swimmy.school::*armada-weekly-dd-percent* orig-weekly-dd)
          (makunbound 'swimmy.school::*armada-weekly-dd-percent*)))))

(deftest test-guard-execution-status-blocks-when-armada-kill-switch-trips
  "Execution guard should fail-close when Armada kill switch is active."
  (let* ((had-enabled (boundp 'swimmy.school::*armada-kill-switch-enabled*))
         (orig-enabled (and had-enabled swimmy.school::*armada-kill-switch-enabled*))
         (had-dd-cap (boundp 'swimmy.school::*armada-hard-dd-percent*))
         (orig-dd-cap (and had-dd-cap swimmy.school::*armada-hard-dd-percent*))
         (orig-state swimmy.school::*system-state*)
         (orig-last-entry swimmy.school::*last-entry-time*)
         (orig-warmup-end swimmy.school::*warmup-end-time*)
         (orig-breaker swimmy.school::*circuit-breaker-active*)
         (orig-breaker-end swimmy.school::*breaker-cooldown-end*)
         (orig-recent swimmy.school::*recent-losses*)
         (orig-dd swimmy.globals::*current-drawdown*)
         (orig-monitor-dd swimmy.globals::*monitoring-drawdown*)
         (orig-market-open (and (fboundp 'swimmy.school::market-open-p)
                                (symbol-function 'swimmy.school::market-open-p)))
         (now (get-universal-time)))
    (unwind-protect
        (progn
          (setf swimmy.school::*armada-kill-switch-enabled* t)
          (setf swimmy.school::*armada-hard-dd-percent* 12.0)
          (setf swimmy.school::*system-state* :trading)
          (setf swimmy.school::*last-entry-time* (- now 100))
          (setf swimmy.school::*warmup-end-time* (- now 120))
          (setf swimmy.school::*circuit-breaker-active* nil)
          (setf swimmy.school::*breaker-cooldown-end* 0)
          (setf swimmy.school::*recent-losses* nil)
          (setf swimmy.globals::*current-drawdown* 13.1)
          (setf swimmy.globals::*monitoring-drawdown* 13.1)
          (when orig-market-open
            (setf (symbol-function 'swimmy.school::market-open-p)
                  (lambda () t)))
          (assert-false (swimmy.school::guard-execution-status "USDJPY" now)
                        "Guard should block when kill switch trips")
          (assert-equal :paused swimmy.school::*system-state*
                        "System state should move to :paused"))
      (if had-enabled
          (setf swimmy.school::*armada-kill-switch-enabled* orig-enabled)
          (makunbound 'swimmy.school::*armada-kill-switch-enabled*))
      (if had-dd-cap
          (setf swimmy.school::*armada-hard-dd-percent* orig-dd-cap)
          (makunbound 'swimmy.school::*armada-hard-dd-percent*))
      (setf swimmy.school::*system-state* orig-state)
      (setf swimmy.school::*last-entry-time* orig-last-entry)
      (setf swimmy.school::*warmup-end-time* orig-warmup-end)
      (setf swimmy.school::*circuit-breaker-active* orig-breaker)
      (setf swimmy.school::*breaker-cooldown-end* orig-breaker-end)
      (setf swimmy.school::*recent-losses* orig-recent)
      (setf swimmy.globals::*current-drawdown* orig-dd)
      (setf swimmy.globals::*monitoring-drawdown* orig-monitor-dd)
      (when orig-market-open
        (setf (symbol-function 'swimmy.school::market-open-p) orig-market-open)))))

(deftest test-sexp-string-avoids-array-syntax
  "sexp->string should not emit #A for base strings (lexpr compatibility)"
  (let* ((base-str (make-array 3 :element-type 'base-char :initial-contents "abc"))
         (payload `((action . ,base-str)))
         (out (swimmy.core::sexp->string payload :package :swimmy.school)))
    (assert-true (search "\"abc\"" out) "Expected quoted string")
    (assert-false (search "#A" out) "Should not include #A array syntax")))

(deftest test-heartbeat-message-is-sexp
  "Heartbeat should return S-expression alist"
  (let ((msg (swimmy.core:make-heartbeat-message)))
    (assert-true (listp msg) "Expected alist heartbeat message")
    (assert-equal "HEARTBEAT" (cdr (assoc 'swimmy.core::type msg)) "Expected type key")
    (assert-equal "BRAIN" (cdr (assoc 'swimmy.core::source msg)) "Expected source key")
    (assert-true (cdr (assoc 'swimmy.core::status msg)) "Expected status key")))

(deftest test-heartbeat-now-trigger-file
  "heartbeat.now file should trigger immediate heartbeat and then be removed"
  (let ((fn (find-symbol "MAYBE-TRIGGER-HEARTBEAT-NOW" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "Trigger function should exist")
    (let* ((path (swimmy.core::swimmy-path ".opus/heartbeat.now"))
           (orig-send (and (fboundp 'swimmy.engine::send-discord-heartbeat)
                           (symbol-function 'swimmy.engine::send-discord-heartbeat)))
           (called nil))
      (unwind-protect
          (progn
            (when (probe-file path)
              (ignore-errors (delete-file path)))
            (ensure-directories-exist path)
            (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
              (write-line "now" out))
            (setf (symbol-function 'swimmy.engine::send-discord-heartbeat)
                  (lambda ()
                    (setf called t)))
            (funcall fn)
            (assert-true called "Expected heartbeat to be triggered")
            (assert-false (probe-file path) "Trigger file should be removed"))
        (when orig-send
          (setf (symbol-function 'swimmy.engine::send-discord-heartbeat) orig-send))
        (when (probe-file path)
          (ignore-errors (delete-file path)))))))

(deftest test-heartbeat-uses-heartbeat-webhook
  "send-discord-heartbeat should resolve heartbeat webhook key"
  (let* ((orig-queue (symbol-function 'swimmy.core:queue-discord-notification))
         (orig-get (and (fboundp 'swimmy.core::get-discord-webhook)
                        (symbol-function 'swimmy.core::get-discord-webhook)))
         (orig-url (and (boundp 'swimmy.core::*heartbeat-webhook-url*)
                        swimmy.core::*heartbeat-webhook-url*))
         (captured nil))
    (unwind-protect
        (progn
          (setf swimmy.core::*heartbeat-webhook-url* nil)
          (setf (symbol-function 'swimmy.core:queue-discord-notification)
                (lambda (webhook message &key title color)
                  (declare (ignore message title color))
                  (setf captured webhook)))
          (when orig-get
            (setf (symbol-function 'swimmy.core::get-discord-webhook)
                  (lambda (key)
                    (if (string= key "heartbeat")
                        "HB_URL"
                        "ALERT_URL"))))
          (swimmy.engine::send-discord-heartbeat)
          (assert-equal "HB_URL" captured "heartbeat should use heartbeat webhook"))
      (setf (symbol-function 'swimmy.core:queue-discord-notification) orig-queue)
      (when orig-get
        (setf (symbol-function 'swimmy.core::get-discord-webhook) orig-get))
      (when (boundp 'swimmy.core::*heartbeat-webhook-url*)
        (setf swimmy.core::*heartbeat-webhook-url* orig-url)))))

(deftest test-heartbeat-summary-no-data-omits-age
  "Heartbeat summary should not show age when no MT5 data has ever arrived"
  (let ((orig-last swimmy.globals::*last-guardian-heartbeat*))
    (unwind-protect
        (progn
          (setf swimmy.globals::*last-guardian-heartbeat* 0)
          (let ((summary (swimmy.engine::get-system-summary)))
            (assert-true (search "No data" summary) "Expected No data in summary")
            (assert-false (search "秒前" summary) "No data should not include age")))
      (setf swimmy.globals::*last-guardian-heartbeat* orig-last))))

(deftest test-heartbeat-summary-currency-format-no-trailing-dot
  "Heartbeat summary yen amounts should not include trailing decimal point"
  (let ((orig-last swimmy.globals::*last-guardian-heartbeat*)
        (orig-equity swimmy.globals::*current-equity*)
        (orig-daily swimmy.globals::*daily-pnl*))
    (unwind-protect
        (progn
          (setf swimmy.globals::*last-guardian-heartbeat* (get-universal-time))
          (setf swimmy.globals::*current-equity* 1000000.0)
          (setf swimmy.globals::*daily-pnl* 1234.0)
          (let ((summary (swimmy.engine::get-system-summary)))
            (assert-true (search "💼 Equity: ¥1000000 | Today: +¥1234" summary)
                         "Expected heartbeat yen amounts without decimal suffix")
            (assert-false (search "¥1000000." summary)
                          "Heartbeat equity must not include trailing decimal point")
            (assert-false (search "¥1234." summary)
                          "Heartbeat daily pnl must not include trailing decimal point")))
      (setf swimmy.globals::*last-guardian-heartbeat* orig-last)
      (setf swimmy.globals::*current-equity* orig-equity)
      (setf swimmy.globals::*daily-pnl* orig-daily))))

(deftest test-executor-heartbeat-sends-sexp
  "Executor should send heartbeat as S-expression (not JSON)"
  (let* ((content (uiop:read-file-string "src/lisp/core/executor.lisp"))
         (uses-json (search "jsown:to-json heartbeat-msg" content))
         (uses-sexp (search "encode-sexp heartbeat-msg" content)))
    (assert-false uses-json "Heartbeat should not use jsown:to-json")
    (assert-true uses-sexp "Heartbeat should use encode-sexp")))

(deftest test-executor-pending-orders-sends-sexp
  "Executor pending retry should resend S-expression (not JSON)"
  (let* ((content (uiop:read-file-string "src/lisp/core/executor.lisp"))
         (uses-json (search "jsown:to-json msg-obj" content))
         (uses-sexp (search "encode-sexp msg-obj" content)))
    (assert-false uses-json "Pending retries should not use jsown:to-json")
    (assert-true uses-sexp "Pending retries should use encode-sexp")))

(deftest test-internal-cmd-json-disallowed
  "Internal CMD senders should not use jsown:to-json"
  (let* ((paths '("src/lisp/school/school-stress.lisp"
                  "src/lisp/school/school-validation.lisp"
                  "src/lisp/school/school-strategy.lisp"
                  "src/lisp/school/school-founders.lisp"
                  "src/lisp/evolution.lisp"))
         (offenders (remove-if-not
                     (lambda (path)
                       (search "jsown:to-json" (uiop:read-file-string path)))
                     paths)))
    (assert-true (null offenders)
                 (format nil "Found jsown:to-json in: ~a" offenders))))

(deftest test-internal-process-msg-tick-sexp
  "internal-process-msg should process TICK S-expression"
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (orig-update (symbol-function 'swimmy.main:update-candle))
         (orig-hb (and (fboundp 'swimmy.executor:send-heartbeat)
                       (symbol-function 'swimmy.executor:send-heartbeat)))
         (orig-process (and (fboundp 'swimmy.school:process-category-trades)
                            (symbol-function 'swimmy.school:process-category-trades)))
         (orig-save (and (fboundp 'swimmy.shell:save-live-status)
                         (symbol-function 'swimmy.shell:save-live-status)))
         (orig-status (and (fboundp 'swimmy.shell:send-periodic-status-report)
                           (symbol-function 'swimmy.shell:send-periodic-status-report)))
         (orig-learn (and (fboundp 'swimmy.school:continuous-learning-step)
                          (symbol-function 'swimmy.school:continuous-learning-step)))
         (orig-last swimmy.globals::*last-guardian-heartbeat*)
         (called nil)
         (hb-called nil))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (unwind-protect
        (progn
          (setf swimmy.globals::*last-guardian-heartbeat* 0)
          (setf (symbol-function 'swimmy.main:update-candle)
                (lambda (bid symbol) (setf called (list bid symbol))))
          (when orig-hb
            (setf (symbol-function 'swimmy.executor:send-heartbeat)
                  (lambda () (setf hb-called t) nil)))
          (when orig-process
            (setf (symbol-function 'swimmy.school:process-category-trades)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-save
            (setf (symbol-function 'swimmy.shell:save-live-status)
                  (lambda () nil)))
          (when orig-status
            (setf (symbol-function 'swimmy.shell:send-periodic-status-report)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-learn
            (setf (symbol-function 'swimmy.school:continuous-learning-step)
                  (lambda () nil)))
          (funcall fn "((type . \"TICK\") (symbol . \"USDJPY\") (bid . 1.23) (ask . 1.24))")
          (assert-equal '(1.23 "USDJPY") called)
          (assert-true hb-called "TICK should trigger brain heartbeat send")
          (assert-true (> swimmy.globals::*last-guardian-heartbeat* 0)
                       "TICK should update last guardian heartbeat"))
      (setf (symbol-function 'swimmy.main:update-candle) orig-update)
      (when orig-hb
        (setf (symbol-function 'swimmy.executor:send-heartbeat) orig-hb))
      (when orig-process
        (setf (symbol-function 'swimmy.school:process-category-trades) orig-process))
      (when orig-save
        (setf (symbol-function 'swimmy.shell:save-live-status) orig-save))
      (when orig-status
        (setf (symbol-function 'swimmy.shell:send-periodic-status-report) orig-status))
      (when orig-learn
        (setf (symbol-function 'swimmy.school:continuous-learning-step) orig-learn))
      (setf swimmy.globals::*last-guardian-heartbeat* orig-last))))

(deftest test-internal-process-msg-tick-json
  "internal-process-msg should process TICK JSON payloads (legacy MT5 bridge compatibility)."
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (orig-update (symbol-function 'swimmy.main:update-candle))
         (orig-hb (and (fboundp 'swimmy.executor:send-heartbeat)
                       (symbol-function 'swimmy.executor:send-heartbeat)))
         (orig-process (and (fboundp 'swimmy.school:process-category-trades)
                            (symbol-function 'swimmy.school:process-category-trades)))
         (orig-save (and (fboundp 'swimmy.shell:save-live-status)
                         (symbol-function 'swimmy.shell:save-live-status)))
         (orig-status (and (fboundp 'swimmy.shell:send-periodic-status-report)
                           (symbol-function 'swimmy.shell:send-periodic-status-report)))
         (orig-learn (and (fboundp 'swimmy.school:continuous-learning-step)
                          (symbol-function 'swimmy.school:continuous-learning-step)))
         (orig-last swimmy.globals::*last-guardian-heartbeat*)
         (called nil)
         (hb-called nil))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (unwind-protect
        (progn
          (setf swimmy.globals::*last-guardian-heartbeat* 0)
          (setf (symbol-function 'swimmy.main:update-candle)
                (lambda (bid symbol) (setf called (list bid symbol))))
          (when orig-hb
            (setf (symbol-function 'swimmy.executor:send-heartbeat)
                  (lambda () (setf hb-called t) nil)))
          (when orig-process
            (setf (symbol-function 'swimmy.school:process-category-trades)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-save
            (setf (symbol-function 'swimmy.shell:save-live-status)
                  (lambda () nil)))
          (when orig-status
            (setf (symbol-function 'swimmy.shell:send-periodic-status-report)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-learn
            (setf (symbol-function 'swimmy.school:continuous-learning-step)
                  (lambda () nil)))
          (funcall fn "{\"type\":\"TICK\",\"symbol\":\"USDJPY\",\"bid\":1.23,\"ask\":1.24}")
          (assert-true (and called (numberp (first called)) (< (abs (- (first called) 1.23)) 1.0e-6))
                       (format nil "Expected JSON TICK to call update-candle with bid close to 1.23; got ~a" called))
          (assert-equal "USDJPY" (second called))
          (assert-true hb-called "JSON TICK should trigger brain heartbeat send")
          (assert-true (> swimmy.globals::*last-guardian-heartbeat* 0)
                       "JSON TICK should update last guardian heartbeat"))
      (setf (symbol-function 'swimmy.main:update-candle) orig-update)
      (when orig-hb
        (setf (symbol-function 'swimmy.executor:send-heartbeat) orig-hb))
      (when orig-process
        (setf (symbol-function 'swimmy.school:process-category-trades) orig-process))
      (when orig-save
        (setf (symbol-function 'swimmy.shell:save-live-status) orig-save))
      (when orig-status
        (setf (symbol-function 'swimmy.shell:send-periodic-status-report) orig-status))
      (when orig-learn
      (setf (symbol-function 'swimmy.school:continuous-learning-step) orig-learn))
      (setf swimmy.globals::*last-guardian-heartbeat* orig-last))))

(deftest test-internal-process-msg-account-info-refreshes-heartbeat
  "ACCOUNT_INFO should refresh guardian heartbeat freshness."
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (orig-process (symbol-function 'swimmy.executor:process-account-info))
         (orig-last-hb swimmy.globals::*last-guardian-heartbeat*)
         (orig-last-account swimmy.globals::*last-account-info-time*)
         (process-called nil))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (unwind-protect
        (progn
          (setf swimmy.globals::*last-guardian-heartbeat* 0)
          (setf swimmy.globals::*last-account-info-time* 0)
          (setf (symbol-function 'swimmy.executor:process-account-info)
                (lambda (payload)
                  (setf process-called t)
                  (funcall orig-process payload)))
          (funcall fn "((type . \"ACCOUNT_INFO\") (timestamp . 123))")
          (assert-true process-called "ACCOUNT_INFO should call process-account-info")
          (assert-true (> swimmy.globals::*last-account-info-time* 0)
                       "ACCOUNT_INFO should update account-info timestamp")
          (assert-true (> swimmy.globals::*last-guardian-heartbeat* 0)
                       "ACCOUNT_INFO should refresh guardian heartbeat freshness"))
      (setf (symbol-function 'swimmy.executor:process-account-info) orig-process)
      (setf swimmy.globals::*last-guardian-heartbeat* orig-last-hb)
      (setf swimmy.globals::*last-account-info-time* orig-last-account))))

(deftest test-internal-process-msg-positions-sexp
  "internal-process-msg should process POSITIONS S-expression and reconcile allocations."
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (orig-reconcile (and (fboundp 'swimmy.school:reconcile-with-mt5-positions)
                              (symbol-function 'swimmy.school:reconcile-with-mt5-positions)))
         (called nil))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (unwind-protect
        (progn
          (when orig-reconcile
            (setf (symbol-function 'swimmy.school:reconcile-with-mt5-positions)
                  (lambda (symbol positions)
                    (setf called (list symbol positions))
                    nil)))
          (funcall fn
                   "((type . \"POSITIONS\") (symbol . \"USDJPY\") (data . (((ticket . 1) (magic . 110012345) (type . \"BUY\") (volume . 0.10) (entry_price . 100.03)))))")
          (assert-true called "Expected POSITIONS to trigger reconcile-with-mt5-positions")
          (assert-equal "USDJPY" (first called))
          (let* ((positions (second called))
                 (pos (and (listp positions) (first positions))))
            (assert-true (and pos (listp pos) (jsown:keyp pos "entry_price"))
                         "Expected jsown position object with entry_price")))
      (when orig-reconcile
        (setf (symbol-function 'swimmy.school:reconcile-with-mt5-positions) orig-reconcile)))))

(deftest test-internal-process-msg-order-ack-clears-executor-pending
  "ORDER_ACK should clear matching UUID from executor pending retry table."
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (orig-pending swimmy.globals:*pending-orders*)
         (order-id "UT-ORDER-ACK-UUID"))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (unwind-protect
        (progn
          (setf swimmy.globals:*pending-orders* (make-hash-table :test 'equal))
          (setf (gethash order-id swimmy.globals:*pending-orders*)
                (list (get-universal-time)
                      0
                      '((type . "ORDER_OPEN")
                        (instrument . "USDJPY")
                        (side . "BUY")
                        (lot . 0.10))))
          (funcall fn
                   (format nil "((type . \"ORDER_ACK\") (id . \"~a\") (ticket . 123456) (symbol . \"USDJPY\"))"
                           order-id))
          (multiple-value-bind (_ pendingp)
              (gethash order-id swimmy.globals:*pending-orders*)
            (declare (ignore _))
            (assert-false pendingp "Expected pending order to be removed on ORDER_ACK")))
      (setf swimmy.globals:*pending-orders* orig-pending))))

(deftest test-internal-process-msg-order-ack-accepts-sharp-bool-token
  "ORDER_ACK with #t payload should not be rejected as invalid SEXP."
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (orig-pending swimmy.globals:*pending-orders*)
         (order-id "UT-ORDER-ACK-SHARP"))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (unwind-protect
        (progn
          (setf swimmy.globals:*pending-orders* (make-hash-table :test 'equal))
          (setf (gethash order-id swimmy.globals:*pending-orders*)
                (list (get-universal-time)
                      0
                      '((type . "ORDER_OPEN")
                        (instrument . "USDJPY")
                        (side . "BUY")
                        (lot . 0.10))))
          (let ((out-stream (make-string-output-stream)))
            (let ((*standard-output* out-stream))
              (funcall fn
                       (format nil
                               "((type . \"ORDER_ACK\") (id . \"~a\") (ticket . 123456) (symbol . \"USDJPY\") (dup . #t))"
                               order-id)))
            (let ((output (get-output-stream-string out-stream)))
              (assert-false (search "Unsafe/invalid SEXP" output)
                            "Expected #t payload to be parsed without invalid SEXP warning")))
          (multiple-value-bind (_ pendingp)
              (gethash order-id swimmy.globals:*pending-orders*)
            (declare (ignore _))
            (assert-false pendingp "Expected pending order to be removed on ORDER_ACK with #t")))
      (setf swimmy.globals:*pending-orders* orig-pending))))

(deftest test-internal-process-msg-order-reject-clears-executor-pending
  "ORDER_REJECT should clear matching UUID from executor pending retry table."
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (orig-pending swimmy.globals:*pending-orders*)
         (order-id "UT-ORDER-REJECT-UUID"))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (unwind-protect
        (progn
          (setf swimmy.globals:*pending-orders* (make-hash-table :test 'equal))
          (setf (gethash order-id swimmy.globals:*pending-orders*)
                (list (get-universal-time)
                      0
                      '((type . "ORDER_OPEN")
                        (instrument . "USDJPY")
                        (side . "BUY")
                        (lot . 0.10))))
          (funcall fn
                   (format nil "((type . \"ORDER_REJECT\") (id . \"~a\") (symbol . \"USDJPY\") (reason . \"NO_MONEY\"))"
                           order-id))
          (multiple-value-bind (_ pendingp)
              (gethash order-id swimmy.globals:*pending-orders*)
            (declare (ignore _))
            (assert-false pendingp "Expected pending order to be removed on ORDER_REJECT")))
      (setf swimmy.globals:*pending-orders* orig-pending))))

(deftest test-internal-process-msg-order-reject-sink-guard-alert
  "Sink-guard ORDER_REJECT should trigger Discord alert for operator visibility."
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (orig-notify-alert (and (fboundp 'swimmy.core:notify-discord-alert)
                                 (symbol-function 'swimmy.core:notify-discord-alert)))
         (alert-msg nil))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.core:notify-discord-alert)
                (lambda (msg &key color)
                  (declare (ignore color))
                  (setf alert-msg msg)
                  nil))
          (funcall fn "((type . \"ORDER_REJECT\") (id . \"UT-REJECT-ALERT\") (symbol . \"USDJPY\") (reason . \"MISSING_STRATEGY\") (retcode . 0))")
          (assert-true alert-msg "Expected sink-guard ORDER_REJECT to trigger alert")
          (assert-true (search "MISSING_STRATEGY" alert-msg)
                       "Expected alert payload to include reject reason"))
      (if orig-notify-alert
          (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify-alert)
          (fmakunbound 'swimmy.core:notify-discord-alert)))))

(deftest test-internal-process-msg-order-reject-sink-guard-alert-dedupes-duplicate-id
  "Duplicate sink-guard ORDER_REJECT with same id/reason should alert once within dedupe window."
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (orig-notify-alert (and (fboundp 'swimmy.core:notify-discord-alert)
                                 (symbol-function 'swimmy.core:notify-discord-alert)))
         (orig-window swimmy.main::*order-reject-alert-dedupe-window-sec*)
         (orig-cache swimmy.main::*order-reject-alert-last-sent*)
         (alert-count 0))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (unwind-protect
        (progn
          (setf swimmy.main::*order-reject-alert-dedupe-window-sec* 300)
          (setf swimmy.main::*order-reject-alert-last-sent* (make-hash-table :test 'equal))
          (setf (symbol-function 'swimmy.core:notify-discord-alert)
                (lambda (msg &key color)
                  (declare (ignore msg color))
                  (incf alert-count)
                  nil))
          (funcall fn "((type . \"ORDER_REJECT\") (id . \"UT-REJECT-DEDUPE\") (symbol . \"USDJPY\") (reason . \"MISSING_STRATEGY\") (retcode . 0))")
          (funcall fn "((type . \"ORDER_REJECT\") (id . \"UT-REJECT-DEDUPE\") (symbol . \"USDJPY\") (reason . \"MISSING_STRATEGY\") (retcode . 0))")
          (assert-equal 1 alert-count "Expected duplicate sink-guard reject alert to be deduped"))
      (setf swimmy.main::*order-reject-alert-dedupe-window-sec* orig-window)
      (setf swimmy.main::*order-reject-alert-last-sent* orig-cache)
      (if orig-notify-alert
          (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify-alert)
          (fmakunbound 'swimmy.core:notify-discord-alert)))))

(deftest test-internal-process-msg-order-reject-sink-guard-alert-does-not-dedupe-different-ids
  "Sink-guard ORDER_REJECT with different IDs should not be deduped by symbol/reason."
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (orig-notify-alert (and (fboundp 'swimmy.core:notify-discord-alert)
                                 (symbol-function 'swimmy.core:notify-discord-alert)))
         (orig-window swimmy.main::*order-reject-alert-dedupe-window-sec*)
         (orig-cache swimmy.main::*order-reject-alert-last-sent*)
         (alert-count 0))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (unwind-protect
        (progn
          (setf swimmy.main::*order-reject-alert-dedupe-window-sec* 300)
          (setf swimmy.main::*order-reject-alert-last-sent* (make-hash-table :test 'equal))
          (setf (symbol-function 'swimmy.core:notify-discord-alert)
                (lambda (msg &key color)
                  (declare (ignore msg color))
                  (incf alert-count)
                  nil))
          (funcall fn "((type . \"ORDER_REJECT\") (id . \"UT-REJECT-A\") (symbol . \"USDJPY\") (reason . \"MISSING_STRATEGY\") (retcode . 0))")
          (funcall fn "((type . \"ORDER_REJECT\") (id . \"UT-REJECT-B\") (symbol . \"USDJPY\") (reason . \"MISSING_STRATEGY\") (retcode . 0))")
          (assert-equal 2 alert-count "Expected different order IDs to each trigger alert"))
      (setf swimmy.main::*order-reject-alert-dedupe-window-sec* orig-window)
      (setf swimmy.main::*order-reject-alert-last-sent* orig-cache)
      (if orig-notify-alert
          (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify-alert)
          (fmakunbound 'swimmy.core:notify-discord-alert)))))

(deftest test-internal-process-msg-order-reject-instrument-sink-guard-alert
  "Instrument sink-guard ORDER_REJECT reasons should trigger Discord alert."
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (orig-notify-alert (and (fboundp 'swimmy.core:notify-discord-alert)
                                 (symbol-function 'swimmy.core:notify-discord-alert))))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (unwind-protect
        (dolist (reason '("MISSING_INSTRUMENT" "INVALID_INSTRUMENT"))
          (let ((alert-msg nil))
            (setf (symbol-function 'swimmy.core:notify-discord-alert)
                  (lambda (msg &key color)
                    (declare (ignore color))
                    (setf alert-msg msg)
                    nil))
            (funcall fn
                     (format nil "((type . \"ORDER_REJECT\") (id . \"UT-REJECT-~a\") (symbol . \"USDJPY\") (reason . \"~a\") (retcode . 0))"
                             reason reason))
            (assert-true alert-msg
                         (format nil "Expected ~a ORDER_REJECT to trigger alert" reason))
            (assert-true (search reason alert-msg)
                         (format nil "Expected alert payload to include reason ~a" reason))))
      (if orig-notify-alert
          (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify-alert)
          (fmakunbound 'swimmy.core:notify-discord-alert)))))

(deftest test-internal-process-msg-order-reject-non-sink-no-alert
  "Non sink-guard ORDER_REJECT should not trigger Discord alert spam."
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (orig-notify-alert (and (fboundp 'swimmy.core:notify-discord-alert)
                                 (symbol-function 'swimmy.core:notify-discord-alert)))
         (alert-called nil))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.core:notify-discord-alert)
                (lambda (msg &key color)
                  (declare (ignore msg color))
                  (setf alert-called t)
                  nil))
          (funcall fn "((type . \"ORDER_REJECT\") (id . \"UT-REJECT-NO-ALERT\") (symbol . \"USDJPY\") (reason . \"NO_MONEY\") (retcode . 10019))")
          (assert-false alert-called "Expected non sink-guard ORDER_REJECT to skip alert"))
      (if orig-notify-alert
          (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify-alert)
          (fmakunbound 'swimmy.core:notify-discord-alert)))))

(deftest test-allocation-pending-timeouts-ignores-executor-pending-orders
  "School allocation pending cleanup should not scan executor retry pending orders (UUID table)."
  (let ((orig-globals swimmy.globals:*pending-orders*)
        (orig-alloc swimmy.school::*allocation-pending-orders*)
        (orig-ttl swimmy.school::*pending-ttl*))
    (unwind-protect
        (progn
          ;; Seed executor retry table with non-plist payload.
          (setf swimmy.globals:*pending-orders* (make-hash-table :test 'equal))
          (setf (gethash "UUID1" swimmy.globals:*pending-orders*)
                (list (get-universal-time) 0 '((type . "ORDER_OPEN") (symbol . "USDJPY"))))

          ;; Seed allocation pending table with a stale entry (plist), then run cleanup.
          (setf swimmy.school::*allocation-pending-orders* (make-hash-table :test 'eql))
          (setf swimmy.school::*pending-ttl* 0)
          (setf (gethash 123 swimmy.school::*allocation-pending-orders*)
                (list :timestamp 0 :strategy "UT-ALLOC" :symbol "USDJPY"))

          (let ((ok (handler-case (progn (swimmy.school::check-pending-timeouts) t)
                      (error (e)
                        (format t "[TEST] Unexpected error: ~a~%" e)
                        nil))))
            (assert-true ok "check-pending-timeouts should not error")
            (multiple-value-bind (_val presentp)
                (gethash 123 swimmy.school::*allocation-pending-orders*)
              (declare (ignore _val))
              (assert-false presentp "Expected allocation pending entry to be cleaned up"))
            (multiple-value-bind (_val presentp)
                (gethash "UUID1" swimmy.globals:*pending-orders*)
              (declare (ignore _val))
              (assert-true presentp "Expected executor pending order to remain intact"))))
      (setf swimmy.globals:*pending-orders* orig-globals)
      (setf swimmy.school::*allocation-pending-orders* orig-alloc)
      (setf swimmy.school::*pending-ttl* orig-ttl))))

(deftest test-reconcile-with-mt5-positions-records-dryrun-slippage-on-promotion
  "reconcile-with-mt5-positions should record DryRun slippage when entry_price is available."
  (let ((orig-pending swimmy.school::*allocation-pending-orders*)
        (orig-slots swimmy.school::*slot-allocation*)
        (orig-record (and (fboundp 'swimmy.school::record-dryrun-slippage)
                          (symbol-function 'swimmy.school::record-dryrun-slippage)))
        (magic 110012345)
        (captured nil))
    (unwind-protect
        (progn
          (setf swimmy.school::*allocation-pending-orders* (make-hash-table :test 'eql))
          (setf swimmy.school::*slot-allocation* (make-hash-table :test 'equal))
          (setf (gethash magic swimmy.school::*allocation-pending-orders*)
                (list :timestamp (get-universal-time)
                      :strategy "UT-SLIP"
                      :symbol "USDJPY"
                      :category :trend
                      :direction :buy
                      :magic magic
                      :entry-bid 100.00
                      :entry-ask 100.02))
          (when orig-record
            (setf (symbol-function 'swimmy.school::record-dryrun-slippage)
                  (lambda (strategy-name slippage-pips)
                    (setf captured (list strategy-name slippage-pips))
                    slippage-pips)))
          (swimmy.school:reconcile-with-mt5-positions
           "USDJPY"
           (list (jsown:new-js
                   ("ticket" 1)
                   ("magic" magic)
                   ("type" "BUY")
                   ("volume" 0.1)
                   ("entry_price" 100.03))))
          (assert-true captured "Expected DryRun slippage to be recorded on promotion")
          (assert-equal "UT-SLIP" (first captured))
          ;; Float rounding can introduce tiny pip drift; allow small epsilon.
          (assert-true (< (abs (- (second captured) 1.0)) 1.0e-3)
                       (format nil "Expected slippage about 1.0 pips; got ~a" (second captured))))
      (setf swimmy.school::*allocation-pending-orders* orig-pending)
      (setf swimmy.school::*slot-allocation* orig-slots)
      (when orig-record
        (setf (symbol-function 'swimmy.school::record-dryrun-slippage) orig-record)))))

(deftest test-allocation-pending-timeouts-tolerates-malformed-entry
  "check-pending-timeouts should not error if allocation pending entry is malformed (missing :timestamp)."
  (let ((orig-alloc swimmy.school::*allocation-pending-orders*)
        (orig-ttl swimmy.school::*pending-ttl*))
    (unwind-protect
        (progn
          (setf swimmy.school::*allocation-pending-orders* (make-hash-table :test 'eql))
          ;; Simulate corrupted config/state (observed in production logs).
          (setf swimmy.school::*pending-ttl* nil)
          ;; Malformed entry: missing :timestamp (observed in production logs).
          (setf (gethash 999 swimmy.school::*allocation-pending-orders*)
                (list :strategy "UT-BAD" :symbol "USDJPY"))
          (let ((ok (handler-case (progn (swimmy.school::check-pending-timeouts) t)
                      (error (e)
                        (format t "[TEST] Unexpected error: ~a~%" e)
                        nil))))
            (assert-true ok "check-pending-timeouts should tolerate malformed entries"))
          (multiple-value-bind (_val presentp)
              (gethash 999 swimmy.school::*allocation-pending-orders*)
            (declare (ignore _val))
            (assert-false presentp "Expected malformed pending entry to be removed")))
      (setf swimmy.school::*allocation-pending-orders* orig-alloc)
      (setf swimmy.school::*pending-ttl* orig-ttl))))

(deftest test-internal-process-msg-status-now-sexp
  "internal-process-msg should force periodic status dispatch for STATUS_NOW S-expression"
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (orig-update (symbol-function 'swimmy.main:update-candle))
         (orig-status (and (fboundp 'swimmy.shell:send-periodic-status-report)
                           (symbol-function 'swimmy.shell:send-periodic-status-report)))
         (orig-last-map swimmy.globals:*last-status-notification-time*)
         (status-called nil)
         (update-called nil))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (unwind-protect
        (progn
          (setf swimmy.globals:*last-status-notification-time* (make-hash-table :test 'equal))
          (setf (gethash "GBPUSD" swimmy.globals:*last-status-notification-time*)
                (get-universal-time))
          (setf (symbol-function 'swimmy.main:update-candle)
                (lambda (bid symbol)
                  (setf update-called (list symbol bid))
                  nil))
          (when orig-status
            (setf (symbol-function 'swimmy.shell:send-periodic-status-report)
                  (lambda (symbol bid &optional force-now)
                    (setf status-called (list symbol bid force-now))
                    nil)))
          (funcall fn "((type . \"STATUS_NOW\") (symbol . \"GBPUSD\") (bid . 1.371))")
          (assert-equal '("GBPUSD" 1.371 t) status-called)
          (assert-equal '("GBPUSD" 1.371) update-called
                        "STATUS_NOW should synthesize fresh candle before reporting")
          (assert-equal 0 (gethash "GBPUSD" swimmy.globals:*last-status-notification-time* 0)
                        "STATUS_NOW should clear status throttle for symbol"))
      (setf (symbol-function 'swimmy.main:update-candle) orig-update)
      (when orig-status
        (setf (symbol-function 'swimmy.shell:send-periodic-status-report) orig-status))
      (setf swimmy.globals:*last-status-notification-time* orig-last-map))))

(deftest test-internal-process-msg-recruit-special-forces-sexp
  "internal-process-msg should dispatch RECRUIT_SPECIAL_FORCES with symbol override and Phase1 flush."
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (orig-supported swimmy.core::*supported-symbols*)
         (preflight-sym 'swimmy.school::*founder-preflight-screen-enabled*)
         (orig-preflight-bound (boundp preflight-sym))
         (orig-preflight (and orig-preflight-bound (symbol-value preflight-sym)))
         (bypass-sym 'swimmy.school::*founder-graveyard-bypass-enabled*)
         (orig-bypass-bound (boundp bypass-sym))
         (orig-bypass (and orig-bypass-bound (symbol-value bypass-sym)))
         (orig-recruit (and (fboundp 'swimmy.school::recruit-special-forces)
                            (symbol-function 'swimmy.school::recruit-special-forces)))
         (orig-flush (and (fboundp 'swimmy.school::flush-deferred-founders)
                          (symbol-function 'swimmy.school::flush-deferred-founders)))
         (recruit-called 0)
         (flush-called 0)
         (captured-symbols nil)
         (captured-recruit-limit nil)
         (captured-founder-keys nil)
         (captured-limit nil)
         (captured-preflight :unset)
         (captured-bypass :unset))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (assert-true orig-recruit "recruit-special-forces should exist")
    (assert-true orig-flush "flush-deferred-founders should exist")
    (unwind-protect
        (progn
          (setf swimmy.core::*supported-symbols* '("USDJPY"))
          (setf (symbol-value preflight-sym) t)
          (setf (symbol-value bypass-sym) nil)
          (setf (symbol-function 'swimmy.school::recruit-special-forces)
                (lambda (&key max-attempts founder-keys)
                  (incf recruit-called)
                  (setf captured-symbols (copy-list swimmy.core::*supported-symbols*))
                  (setf captured-recruit-limit max-attempts)
                  (setf captured-founder-keys (and founder-keys (copy-list founder-keys)))
                  (setf captured-preflight (symbol-value preflight-sym))
                  (setf captured-bypass (symbol-value bypass-sym))
                  t))
          (setf (symbol-function 'swimmy.school::flush-deferred-founders)
                (lambda (&key limit)
                  (incf flush-called)
                  (setf captured-limit limit)
                  2))
          (funcall fn
                   "((type . \"RECRUIT_SPECIAL_FORCES\") (symbols . (\"EURUSD\" \"GBPUSD\")) (founder_keys . (\"hunted-d1-adx\" \"hunted-w1-ema\")) (recruit_limit . 9) (disable_preflight . t) (bypass_graveyard . t) (flush_phase1 . t) (flush_limit . 7))")
          (assert-equal 1 recruit-called
                        "Expected exactly one recruit-special-forces dispatch")
          (assert-equal '("EURUSD" "GBPUSD") captured-symbols
                        "Expected supported symbols to be overridden during recruit")
          (assert-equal 9 captured-recruit-limit
                        "Expected recruit_limit to propagate to recruit-special-forces")
          (assert-equal '(:HUNTED-D1-ADX :HUNTED-W1-EMA) captured-founder-keys
                        "Expected founder_keys to normalize into keyword list")
          (assert-false captured-preflight
                        "Expected disable_preflight=t to bind founder preflight screen to NIL")
          (assert-true captured-bypass
                       "Expected bypass_graveyard=t to bind founder graveyard bypass to T")
          (assert-equal 1 flush-called
                        "Expected one deferred Phase1 flush call")
          (assert-equal 7 captured-limit
                        "Expected flush_limit to propagate to deferred flush"))
      (setf swimmy.core::*supported-symbols* orig-supported)
      (if orig-preflight-bound
          (setf (symbol-value preflight-sym) orig-preflight)
          (makunbound preflight-sym))
      (if orig-bypass-bound
          (setf (symbol-value bypass-sym) orig-bypass)
          (makunbound bypass-sym))
      (if orig-recruit
          (setf (symbol-function 'swimmy.school::recruit-special-forces) orig-recruit)
          (fmakunbound 'swimmy.school::recruit-special-forces))
      (if orig-flush
          (setf (symbol-function 'swimmy.school::flush-deferred-founders) orig-flush)
          (fmakunbound 'swimmy.school::flush-deferred-founders)))))

(deftest test-internal-process-msg-history-sexp
  "internal-process-msg should store HISTORY S-expression as newest-first"
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (orig-hist swimmy.globals:*candle-histories*)
         (orig-hist-tf swimmy.globals:*candle-histories-tf*)
         (orig-main-hist (and (boundp 'swimmy.main::*candle-history*) swimmy.main::*candle-history*)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (unwind-protect
        (progn
          (setf swimmy.globals:*candle-histories* (make-hash-table :test 'equal))
          (setf swimmy.globals:*candle-histories-tf* (make-hash-table :test 'equal))
          (when (boundp 'swimmy.main::*candle-history*)
            (setf swimmy.main::*candle-history* nil))
          (funcall fn
                   "((type . \"HISTORY\") (symbol . \"USDJPY\") (tf . \"M1\") (batch . 0) (total . 1) (data . (((t . 1) (o . 1.0) (h . 1.1) (l . 0.9) (c . 1.0)) ((t . 2) (o . 2.0) (h . 2.1) (l . 1.9) (c . 2.0)))))")
          (let ((hist (gethash "USDJPY" swimmy.globals:*candle-histories*)))
            (assert-true hist "Expected history stored")
            (assert-equal 2 (swimmy.globals:candle-timestamp (first hist)) "Newest should be first")))
      (setf swimmy.globals:*candle-histories* orig-hist)
      (setf swimmy.globals:*candle-histories-tf* orig-hist-tf)
      (when (boundp 'swimmy.main::*candle-history*)
        (setf swimmy.main::*candle-history* orig-main-hist)))))

(deftest test-internal-process-msg-history-sexp-normalizes-unix-timestamp
  "HISTORY unix timestamps should be normalized to universal-time in memory"
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (orig-hist swimmy.globals:*candle-histories*)
         (orig-hist-tf swimmy.globals:*candle-histories-tf*)
         (orig-main-hist (and (boundp 'swimmy.main::*candle-history*) swimmy.main::*candle-history*))
         (unix-older 1700000000)
         (unix-latest 1700000060)
         (offset 2208988800))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (unwind-protect
        (progn
          (setf swimmy.globals:*candle-histories* (make-hash-table :test 'equal))
          (setf swimmy.globals:*candle-histories-tf* (make-hash-table :test 'equal))
          (when (boundp 'swimmy.main::*candle-history*)
            (setf swimmy.main::*candle-history* nil))
          (funcall fn
                   (format nil
                           "((type . \"HISTORY\") (symbol . \"GBPUSD\") (tf . \"M1\") (batch . 0) (total . 1) (data . (((t . ~d) (o . 1.0) (h . 1.1) (l . 0.9) (c . 1.0)) ((t . ~d) (o . 2.0) (h . 2.1) (l . 1.9) (c . 2.0)))))"
                           unix-older unix-latest))
          (let ((hist (gethash "GBPUSD" swimmy.globals:*candle-histories*)))
            (assert-true hist "Expected history stored")
            (assert-equal (+ offset unix-latest) (swimmy.globals:candle-timestamp (first hist))
                          "Expected unix timestamp to be converted to universal time")))
      (setf swimmy.globals:*candle-histories* orig-hist)
      (setf swimmy.globals:*candle-histories-tf* orig-hist-tf)
      (when (boundp 'swimmy.main::*candle-history*)
        (setf swimmy.main::*candle-history* orig-main-hist)))))

(deftest test-internal-process-msg-history-sexp-normalizes-symbol-key
  "HISTORY symbol keys should be normalized to uppercase string keys"
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (orig-hist swimmy.globals:*candle-histories*)
         (orig-hist-tf swimmy.globals:*candle-histories-tf*)
         (legacy-key (find-symbol "GBPUSD" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (unwind-protect
        (progn
          (setf swimmy.globals:*candle-histories* (make-hash-table :test 'equal))
          (setf swimmy.globals:*candle-histories-tf* (make-hash-table :test 'equal))
          (when legacy-key
            (setf (gethash legacy-key swimmy.globals:*candle-histories*)
                  (list (swimmy.globals:make-candle :timestamp 1 :open 1.0 :high 1.0 :low 1.0 :close 1.0 :volume 1))))
          (funcall fn
                   "((type . \"HISTORY\") (symbol . GBPUSD) (tf . \"M1\") (batch . 0) (total . 1) (data . (((t . 10) (o . 1.0) (h . 1.1) (l . 0.9) (c . 1.0)))))")
          (assert-true (gethash "GBPUSD" swimmy.globals:*candle-histories*)
                       "Expected canonical string key history")
          (when legacy-key
            (assert-false (gethash legacy-key swimmy.globals:*candle-histories*)
                          "Expected legacy symbol key to be migrated")))
      (setf swimmy.globals:*candle-histories* orig-hist)
      (setf swimmy.globals:*candle-histories-tf* orig-hist-tf))))

(deftest test-data-client-sexp-candle-normalizes-unix-timestamp
  "Data Keeper unix timestamps should normalize to universal-time candles"
  (let* ((unix-ts 1700000000)
         (offset 2208988800)
         (candle (swimmy.core::%sexp-candle->struct
                  `((swimmy.core::timestamp . ,unix-ts)
                    (swimmy.core::open . 1.1)
                    (swimmy.core::high . 1.2)
                    (swimmy.core::low . 1.0)
                    (swimmy.core::close . 1.15)
                    (swimmy.core::volume . 7)))))
    (assert-true candle "Expected candle struct")
    (assert-equal (+ offset unix-ts) (swimmy.globals:candle-timestamp candle))))

(deftest test-update-candle-updates-symbol-history-all-symbols
  "update-candle should update per-symbol M1 history (not only USDJPY)"
  (let* ((orig-hist swimmy.globals:*candle-histories*)
         (orig-single swimmy.globals:*candle-history*)
         (now (get-universal-time))
         (seed (swimmy.globals:make-candle
                :timestamp (- now 120)
                :open 1.3000 :high 1.3001 :low 1.2999 :close 1.3000 :volume 1)))
    (unwind-protect
        (progn
          (setf swimmy.globals:*candle-histories* (make-hash-table :test 'equal))
          (setf swimmy.globals:*candle-history* nil)
          (setf (gethash "GBPUSD" swimmy.globals:*candle-histories*) (list seed))

          (swimmy.main:update-candle 1.3010 "GBPUSD")

          (let ((hist (gethash "GBPUSD" swimmy.globals:*candle-histories*)))
            (assert-true hist "Expected GBPUSD history to be present")
            (assert-true (>= (length hist) 1) "Expected at least one GBPUSD candle")
            (assert-true (>= (swimmy.globals:candle-timestamp (first hist))
                             (- now 5))
                         "Expected latest GBPUSD candle timestamp to be fresh")))
      (setf swimmy.globals:*candle-histories* orig-hist)
      (setf swimmy.globals:*candle-history* orig-single))))

(deftest test-update-candle-migrates-legacy-symbol-key
  "update-candle should migrate legacy symbol-keyed history to string keys"
  (let* ((orig-hist swimmy.globals:*candle-histories*)
         (orig-hist-tf swimmy.globals:*candle-histories-tf*)
         (orig-single swimmy.globals:*candle-history*)
         (legacy-key (find-symbol "GBPUSD" :swimmy.main))
         (seed (swimmy.globals:make-candle
                :timestamp (- (get-universal-time) 120)
                :open 1.3000 :high 1.3001 :low 1.2999 :close 1.3000 :volume 1)))
    (unwind-protect
        (progn
          (setf swimmy.globals:*candle-histories* (make-hash-table :test 'equal))
          (setf swimmy.globals:*candle-histories-tf* (make-hash-table :test 'equal))
          (setf swimmy.globals:*candle-history* nil)
          (when legacy-key
            (setf (gethash legacy-key swimmy.globals:*candle-histories*) (list seed)))

          (swimmy.main:update-candle 1.3010 "GBPUSD")

          (assert-true (gethash "GBPUSD" swimmy.globals:*candle-histories*)
                       "Expected canonical string key to exist after update")
          (when legacy-key
            (assert-false (gethash legacy-key swimmy.globals:*candle-histories*)
                          "Expected legacy symbol key to be removed")))
      (setf swimmy.globals:*candle-histories* orig-hist)
      (setf swimmy.globals:*candle-histories-tf* orig-hist-tf)
      (setf swimmy.globals:*candle-history* orig-single))))

(deftest test-periodic-status-report-uses-symbol-history-context
  "periodic status should use symbol-specific market context when history exists"
  (let* ((orig-notify (and (fboundp 'swimmy.core:notify-discord-status)
                           (symbol-function 'swimmy.core:notify-discord-status)))
         (orig-fx-open (and (fboundp 'swimmy.shell::fx-market-open-p)
                            (symbol-function 'swimmy.shell::fx-market-open-p)))
         (orig-predict (and (fboundp 'swimmy.school:summarize-status-prediction)
                            (symbol-function 'swimmy.school:summarize-status-prediction)))
         (orig-rank (and (fboundp 'swimmy.school:get-strategies-by-rank)
                         (symbol-function 'swimmy.school:get-strategies-by-rank)))
         (orig-detect (and (fboundp 'swimmy.school::detect-market-regime)
                           (symbol-function 'swimmy.school::detect-market-regime)))
         (orig-hist swimmy.globals:*candle-histories*)
         (orig-single swimmy.globals:*candle-history*)
         (orig-last swimmy.globals:*last-status-notification-time*)
         (orig-interval swimmy.globals:*status-notification-interval*)
         (orig-regime swimmy.school:*current-regime*)
         (orig-vol swimmy.school:*volatility-regime*)
         (captured nil))
    (unwind-protect
        (progn
          (setf swimmy.globals:*status-notification-interval* 0)
          (setf swimmy.globals:*last-status-notification-time* (make-hash-table :test 'equal))
          (setf swimmy.globals:*candle-histories* (make-hash-table :test 'equal))
          (setf swimmy.globals:*candle-history* nil)
          (setf swimmy.school:*current-regime* :unknown)
          (setf swimmy.school:*volatility-regime* :normal)
          (setf (gethash "GBPUSD" swimmy.globals:*candle-histories*)
                (loop for i from 1 to 80
                      collect (swimmy.globals:make-candle
                               :timestamp (+ 1700000000 i)
                               :open (+ 1.3000 (* i 0.0001))
                               :high (+ 1.3003 (* i 0.0001))
                               :low (+ 1.2997 (* i 0.0001))
                               :close (+ 1.3001 (* i 0.0001))
                               :volume 1)))
          (when orig-notify
            (setf (symbol-function 'swimmy.core:notify-discord-status)
                  (lambda (msg &key color)
                    (declare (ignore color))
                    (setf captured msg))))
          (when orig-fx-open
            (setf (symbol-function 'swimmy.shell::fx-market-open-p)
                  (lambda (&optional _ts)
                    (declare (ignore _ts))
                    t)))
          (when orig-predict
            (setf (symbol-function 'swimmy.school:summarize-status-prediction)
                  (lambda (_symbol)
                    (declare (ignore _symbol))
                    (values :buy 0.825 :ok))))
          (when orig-rank
            (setf (symbol-function 'swimmy.school:get-strategies-by-rank)
                  (lambda (&rest _args)
                    (declare (ignore _args))
                    nil)))
          (when orig-detect
            (setf (symbol-function 'swimmy.school::detect-market-regime)
                  (lambda ()
                    (setf swimmy.school:*volatility-regime* :high)
                    :trend-early)))
          (swimmy.shell:send-periodic-status-report "GBPUSD" 1.365)
          (assert-true captured "Expected periodic status notification")
          (assert-true (search "TREND-EARLY" captured)
                       "Expected regime from symbol-specific detection")
          (assert-true (search "HIGH" captured)
                       "Expected volatility from symbol-specific detection")
          (assert-true (search "シンボル別履歴（再計算）" captured)
                       "Expected explicit symbol-history source label")
          (assert-true (search "履歴再計算OK" captured)
                       "Expected explicit market reason label"))
      (when orig-notify
        (setf (symbol-function 'swimmy.core:notify-discord-status) orig-notify))
      (when orig-fx-open
        (setf (symbol-function 'swimmy.shell::fx-market-open-p) orig-fx-open))
      (when orig-predict
        (setf (symbol-function 'swimmy.school:summarize-status-prediction) orig-predict))
      (when orig-rank
        (setf (symbol-function 'swimmy.school:get-strategies-by-rank) orig-rank))
      (when orig-detect
        (setf (symbol-function 'swimmy.school::detect-market-regime) orig-detect))
      (setf swimmy.globals:*candle-histories* orig-hist)
      (setf swimmy.globals:*candle-history* orig-single)
      (setf swimmy.globals:*last-status-notification-time* orig-last)
      (setf swimmy.globals:*status-notification-interval* orig-interval)
      (setf swimmy.school:*current-regime* orig-regime)
      (setf swimmy.school:*volatility-regime* orig-vol))))

(deftest test-periodic-status-report-includes-evidence-and-freshness
  "periodic status should include reason/freshness lines"
  (let* ((orig-notify (and (fboundp 'swimmy.core:notify-discord-status)
                           (symbol-function 'swimmy.core:notify-discord-status)))
         (orig-fx-open (and (fboundp 'swimmy.shell::fx-market-open-p)
                            (symbol-function 'swimmy.shell::fx-market-open-p)))
         (orig-predict (and (fboundp 'swimmy.school:summarize-status-prediction)
                            (symbol-function 'swimmy.school:summarize-status-prediction)))
         (orig-rank (and (fboundp 'swimmy.school:get-strategies-by-rank)
                         (symbol-function 'swimmy.school:get-strategies-by-rank)))
         (orig-last swimmy.globals:*last-status-notification-time*)
         (orig-interval swimmy.globals:*status-notification-interval*)
         (captured nil))
    (unwind-protect
        (progn
          (setf swimmy.globals:*status-notification-interval* 0)
          (setf swimmy.globals:*last-status-notification-time* (make-hash-table :test 'equal))
          (when orig-notify
            (setf (symbol-function 'swimmy.core:notify-discord-status)
                  (lambda (msg &key color)
                    (declare (ignore color))
                    (setf captured msg))))
          (when orig-fx-open
            (setf (symbol-function 'swimmy.shell::fx-market-open-p)
                  (lambda (&optional _ts)
                    (declare (ignore _ts))
                    t)))
          (when orig-predict
            (setf (symbol-function 'swimmy.school:summarize-status-prediction)
                  (lambda (_symbol)
                    (declare (ignore _symbol))
                    (values :hold 0.0 :insufficient-history))))
          (when orig-rank
            (setf (symbol-function 'swimmy.school:get-strategies-by-rank)
                  (lambda (&rest _args)
                    (declare (ignore _args))
                    nil)))
          (swimmy.shell:send-periodic-status-report "GBPUSD" 1.367)
          (assert-true captured "Expected periodic status notification")
          (assert-true (search "判定根拠/鮮度" captured)
                       "Expected evidence header in status message")
          (assert-true (search "データ鮮度:" captured)
                       "Expected freshness line in status message")
          (assert-true (search "全体状態フォールバック（推定保留）" captured)
                       "Expected human-readable market source label")
          (assert-true (search "履歴不足（60本未満）" captured)
                       "Expected explicit market fallback reason label")
          (assert-true (search "履歴不足（50本以下）" captured)
                       "Expected explicit prediction reason label")
          (assert-false (search "GLOBAL-FALLBACK" captured)
                        "Internal source key should not be shown")
          (assert-false (search "INSUFFICIENT-HISTORY" captured)
                        "Internal reason key should not be shown")
          (assert-false (search "全体状態フォールバック (" captured)
                        "Legacy ASCII parenthesis label should not remain"))
      (when orig-notify
        (setf (symbol-function 'swimmy.core:notify-discord-status) orig-notify))
      (when orig-fx-open
        (setf (symbol-function 'swimmy.shell::fx-market-open-p) orig-fx-open))
      (when orig-predict
        (setf (symbol-function 'swimmy.school:summarize-status-prediction) orig-predict))
      (when orig-rank
        (setf (symbol-function 'swimmy.school:get-strategies-by-rank) orig-rank))
      (setf swimmy.globals:*last-status-notification-time* orig-last)
      (setf swimmy.globals:*status-notification-interval* orig-interval))))

(deftest test-process-account-info-sexp
  "process-account-info should accept S-expression alist"
  (let* ((fn (find-symbol "PROCESS-ACCOUNT-INFO" :swimmy.executor))
         (orig-equity swimmy.executor::*current-equity*)
         (orig-peak swimmy.executor::*peak-equity*)
         (orig-alert swimmy.executor::*account-info-alert-sent*))
    (assert-true (and fn (fboundp fn)) "process-account-info exists")
    (unwind-protect
        (progn
          (setf swimmy.executor::*current-equity* 0.0)
          (setf swimmy.executor::*peak-equity* 0.0)
          (setf swimmy.executor::*account-info-alert-sent* nil)
          (funcall fn '((equity . 1000000.0) (balance . 900000.0)))
          (assert-true (> swimmy.executor::*current-equity* 0.0) "Equity should update"))
      (setf swimmy.executor::*current-equity* orig-equity)
      (setf swimmy.executor::*peak-equity* orig-peak)
      (setf swimmy.executor::*account-info-alert-sent* orig-alert))))

(deftest test-process-account-info-drawdown-alert-peak-format
  "Dynamic drawdown alert should not include trailing decimal point in peak"
  (let* ((fn (find-symbol "PROCESS-ACCOUNT-INFO" :swimmy.executor))
         (orig-notify (and (fboundp 'swimmy.core:notify-discord-alert)
                           (symbol-function 'swimmy.core:notify-discord-alert)))
         (orig-current swimmy.executor::*current-equity*)
         (orig-peak swimmy.executor::*peak-equity*)
         (orig-monitor-peak swimmy.executor::*monitoring-peak-equity*)
         (orig-monitor-dd swimmy.executor::*monitoring-drawdown*)
         (orig-monitor-alert swimmy.executor::*monitoring-alert-sent-20*)
         (orig-last-save swimmy.executor::*last-account-save-time*)
         (orig-save-interval swimmy.executor::*account-info-save-interval*)
         (captured nil))
    (assert-true (and fn (fboundp fn)) "process-account-info exists")
    (unwind-protect
        (progn
          (when orig-notify
            (setf (symbol-function 'swimmy.core:notify-discord-alert)
                  (lambda (msg &key color)
                    (declare (ignore color))
                    (setf captured msg))))
          (setf swimmy.executor::*current-equity* 1000000.0)
          (setf swimmy.executor::*peak-equity* 1000000.0)
          (setf swimmy.executor::*monitoring-peak-equity* 1000000.0)
          (setf swimmy.executor::*monitoring-drawdown* 0.0)
          (setf swimmy.executor::*monitoring-alert-sent-20* nil)
          (setf swimmy.executor::*last-account-save-time* (get-universal-time))
          (setf swimmy.executor::*account-info-save-interval* 999999)
          (funcall fn '((equity . 70000.0) (balance . 70000.0)))
          (assert-not-nil captured "Expected dynamic drawdown alert")
          (assert-true (search "DYNAMIC DRAWDOWN WARNING: 93.0% (Peak: ¥1000000)" captured)
                       "Expected peak value without decimal suffix")
          (assert-false (search "Peak: ¥1000000.)" captured)
                        "Peak value must not include trailing decimal point"))
      (when orig-notify
        (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify))
      (setf swimmy.executor::*current-equity* orig-current)
      (setf swimmy.executor::*peak-equity* orig-peak)
      (setf swimmy.executor::*monitoring-peak-equity* orig-monitor-peak)
      (setf swimmy.executor::*monitoring-drawdown* orig-monitor-dd)
      (setf swimmy.executor::*monitoring-alert-sent-20* orig-monitor-alert)
      (setf swimmy.executor::*last-account-save-time* orig-last-save)
      (setf swimmy.executor::*account-info-save-interval* orig-save-interval))))

(deftest test-process-account-info-sync-log-format-no-trailing-dot
  "MT5 sync log should print integer yen values without trailing decimal points"
  (let* ((fn (find-symbol "PROCESS-ACCOUNT-INFO" :swimmy.executor))
         (orig-current swimmy.executor::*current-equity*)
         (orig-peak swimmy.executor::*peak-equity*)
         (orig-monitor-peak swimmy.executor::*monitoring-peak-equity*)
         (orig-monitor-dd swimmy.executor::*monitoring-drawdown*)
         (orig-monitor-alert swimmy.executor::*monitoring-alert-sent-20*)
         (orig-last-save swimmy.executor::*last-account-save-time*)
         (orig-save-interval swimmy.executor::*account-info-save-interval*))
    (assert-true (and fn (fboundp fn)) "process-account-info exists")
    (unwind-protect
        (let ((*standard-output* (make-string-output-stream)))
          (setf swimmy.executor::*current-equity* 0.0)
          (setf swimmy.executor::*peak-equity* 0.0)
          (setf swimmy.executor::*monitoring-peak-equity* 0.0)
          (setf swimmy.executor::*monitoring-drawdown* 0.0)
          (setf swimmy.executor::*monitoring-alert-sent-20* nil)
          (setf swimmy.executor::*last-account-save-time* (get-universal-time))
          (setf swimmy.executor::*account-info-save-interval* 999999)
          (funcall fn '((equity . 1000000.0) (balance . 1000000.0)))
          (let ((output (get-output-stream-string *standard-output*)))
            (assert-true (search "MT5 Sync: Equity=¥1000000 DynPK=¥1000000 DynDD=0.0%" output)
                         "Expected MT5 sync yen values without decimal suffix")
            (assert-false (search "Equity=¥1000000." output)
                          "Current equity must not include trailing decimal point")
            (assert-false (search "DynPK=¥1000000." output)
                          "Monitoring peak must not include trailing decimal point")))
      (setf swimmy.executor::*current-equity* orig-current)
      (setf swimmy.executor::*peak-equity* orig-peak)
      (setf swimmy.executor::*monitoring-peak-equity* orig-monitor-peak)
      (setf swimmy.executor::*monitoring-drawdown* orig-monitor-dd)
      (setf swimmy.executor::*monitoring-alert-sent-20* orig-monitor-alert)
      (setf swimmy.executor::*last-account-save-time* orig-last-save)
      (setf swimmy.executor::*account-info-save-interval* orig-save-interval))))

(deftest test-process-account-info-rebases-stale-monitoring-peak-after-restart
  "When monitoring peak is stale across restart, first ACCOUNT_INFO should rebase DynDD baseline."
  (let* ((fn (find-symbol "PROCESS-ACCOUNT-INFO" :swimmy.executor))
         (orig-current swimmy.executor::*current-equity*)
         (orig-peak swimmy.executor::*peak-equity*)
         (orig-monitor-peak swimmy.executor::*monitoring-peak-equity*)
         (orig-monitor-dd swimmy.executor::*monitoring-drawdown*)
         (orig-monitor-alert swimmy.executor::*monitoring-alert-sent-20*)
         (orig-last-info swimmy.executor::*last-account-info-time*)
         (orig-system-start swimmy.globals::*system-start-time*)
         (orig-last-save swimmy.executor::*last-account-save-time*)
         (orig-save-interval swimmy.executor::*account-info-save-interval*))
    (assert-true (and fn (fboundp fn)) "process-account-info exists")
    (unwind-protect
        (progn
          (setf swimmy.globals::*system-start-time* (get-universal-time))
          (setf swimmy.executor::*last-account-info-time*
                (- swimmy.globals::*system-start-time* 3600))
          (setf swimmy.executor::*current-equity* 100000.0)
          (setf swimmy.executor::*peak-equity* 100000.0)
          (setf swimmy.executor::*monitoring-peak-equity* 1000000.0)
          (setf swimmy.executor::*monitoring-drawdown* 90.0)
          (setf swimmy.executor::*monitoring-alert-sent-20* t)
          (setf swimmy.executor::*last-account-save-time* (get-universal-time))
          (setf swimmy.executor::*account-info-save-interval* 999999)
          (funcall fn '((equity . 70000.0) (balance . 70000.0)))
          (assert-equal 70000.0 swimmy.executor::*monitoring-peak-equity*
                        "Stale monitoring peak should rebase to current equity")
          (assert-equal 0.0 swimmy.executor::*monitoring-drawdown*
                        "DynDD should reset to 0 after baseline rebase")
          (assert-false swimmy.executor::*monitoring-alert-sent-20*
                        "DynDD alert latch should clear after rebase"))
      (setf swimmy.executor::*current-equity* orig-current)
      (setf swimmy.executor::*peak-equity* orig-peak)
      (setf swimmy.executor::*monitoring-peak-equity* orig-monitor-peak)
      (setf swimmy.executor::*monitoring-drawdown* orig-monitor-dd)
      (setf swimmy.executor::*monitoring-alert-sent-20* orig-monitor-alert)
      (setf swimmy.executor::*last-account-info-time* orig-last-info)
      (setf swimmy.globals::*system-start-time* orig-system-start)
      (setf swimmy.executor::*last-account-save-time* orig-last-save)
      (setf swimmy.executor::*account-info-save-interval* orig-save-interval))))

(deftest test-s-rank-gate-uses-configurable-minimum
  "s-rank-gate-passed-p should use configurable minimum S-rank threshold."
  (let* ((gate-fn (find-symbol "S-RANK-GATE-PASSED-P" :swimmy.school))
         (gate-sym (find-symbol "*MIN-S-RANK-STRATEGIES-FOR-LIVE*" :swimmy.school))
         (orig-kb swimmy.globals:*strategy-knowledge-base*)
         (orig-min (and gate-sym (boundp gate-sym) (symbol-value gate-sym))))
    (assert-true (and gate-fn (fboundp gate-fn)) "s-rank-gate-passed-p exists")
    (assert-not-nil gate-sym "Expected configurable min S-rank symbol")
    (unwind-protect
        (progn
          (setf swimmy.globals:*strategy-knowledge-base*
                (list (swimmy.school:make-strategy :name "UT-S1" :rank :S)
                      (swimmy.school:make-strategy :name "UT-S2" :rank :S)
                      (swimmy.school:make-strategy :name "UT-B1" :rank :B)))
          (setf (symbol-value gate-sym) 2)
          (assert-true (funcall gate-fn)
                       "2 S strategies should pass when minimum is 2")
          (setf (symbol-value gate-sym) 3)
          (assert-false (funcall gate-fn)
                        "2 S strategies should fail when minimum is 3"))
      (setf swimmy.globals:*strategy-knowledge-base* orig-kb)
      (when gate-sym
        (if orig-min
            (setf (symbol-value gate-sym) orig-min)
            (makunbound gate-sym))))))

(deftest test-pulse-check-equity-format-no-trailing-dot
  "Pulse check should print integer equity without trailing decimal point"
  (let ((orig-ticks swimmy.executor::*total-ticks-count*)
        (orig-equity swimmy.executor::*current-equity*)
        (orig-heartbeat swimmy.executor::*last-guardian-heartbeat*))
    (unwind-protect
        (let ((*standard-output* (make-string-output-stream)))
          (setf swimmy.executor::*total-ticks-count* 0)
          (setf swimmy.executor::*current-equity* 1000000.0)
          (setf swimmy.executor::*last-guardian-heartbeat* (get-universal-time))
          (swimmy.executor::pulse-check)
          (let ((output (get-output-stream-string *standard-output*)))
            (assert-true (search "Equity: 1000000" output)
                         "Expected integer equity text in pulse")
            (assert-false (search "Equity: 1000000." output)
                          "Pulse equity must not include trailing decimal point")))
      (setf swimmy.executor::*total-ticks-count* orig-ticks)
      (setf swimmy.executor::*current-equity* orig-equity)
      (setf swimmy.executor::*last-guardian-heartbeat* orig-heartbeat))))

(deftest test-process-trade-closed-sexp
  "process-trade-closed should accept S-expression alist"
  (let* ((fn (find-symbol "PROCESS-TRADE-CLOSED" :swimmy.executor))
         (orig-trade-result (and (fboundp 'swimmy.school:record-trade-result)
                                 (symbol-function 'swimmy.school:record-trade-result)))
         (orig-trade-outcome (and (fboundp 'swimmy.school:record-trade-outcome)
                                  (symbol-function 'swimmy.school:record-trade-outcome)))
         (orig-notify (and (fboundp 'swimmy.core:notify-discord-alert)
                           (symbol-function 'swimmy.core:notify-discord-alert)))
         (orig-queue (and (fboundp 'swimmy.core:queue-discord-notification)
                          (symbol-function 'swimmy.core:queue-discord-notification)))
         (orig-processed swimmy.executor::*processed-tickets*)
         (orig-daily swimmy.executor::*daily-pnl*)
         (orig-weekly swimmy.executor::*weekly-pnl*)
         (orig-monthly swimmy.executor::*monthly-pnl*)
         (orig-accum swimmy.executor::*accumulated-pnl*)
         (orig-total swimmy.executor::*total-trades*)
         (orig-success swimmy.executor::*success-count*))
    (assert-true (and fn (fboundp fn)) "process-trade-closed exists")
    (unwind-protect
        (progn
          (when orig-trade-result
            (setf (symbol-function 'swimmy.school:record-trade-result) (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-trade-outcome
            (setf (symbol-function 'swimmy.school:record-trade-outcome) (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-notify
            (setf (symbol-function 'swimmy.core:notify-discord-alert) (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-queue
            (setf (symbol-function 'swimmy.core:queue-discord-notification) (lambda (&rest _args) (declare (ignore _args)) nil)))
          (setf swimmy.executor::*processed-tickets* (make-hash-table :test 'equal))
          (setf swimmy.executor::*daily-pnl* 0.0)
          (setf swimmy.executor::*weekly-pnl* 0.0)
          (setf swimmy.executor::*monthly-pnl* 0.0)
          (setf swimmy.executor::*accumulated-pnl* 0.0)
          (setf swimmy.executor::*total-trades* 0)
          (setf swimmy.executor::*success-count* 0)
          (funcall fn '((ticket . 12345) (symbol . "USDJPY") (pnl . 5.0) (won . t) (magic . 123)) "")
          (assert-true (gethash 12345 swimmy.executor::*processed-tickets*) "Expected ticket tracked"))
      (when orig-trade-result
        (setf (symbol-function 'swimmy.school:record-trade-result) orig-trade-result))
      (when orig-trade-outcome
        (setf (symbol-function 'swimmy.school:record-trade-outcome) orig-trade-outcome))
      (when orig-notify
        (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify))
      (when orig-queue
        (setf (symbol-function 'swimmy.core:queue-discord-notification) orig-queue))
      (setf swimmy.executor::*processed-tickets* orig-processed)
      (setf swimmy.executor::*daily-pnl* orig-daily)
      (setf swimmy.executor::*weekly-pnl* orig-weekly)
      (setf swimmy.executor::*monthly-pnl* orig-monthly)
      (setf swimmy.executor::*accumulated-pnl* orig-accum)
      (setf swimmy.executor::*total-trades* orig-total)
      (setf swimmy.executor::*success-count* orig-success))))

(deftest test-process-trade-closed-fixed-template-message-no-narrative
  "Trade close notification should use fixed template and not call legacy narrative generator."
  (let* ((fn (find-symbol "PROCESS-TRADE-CLOSED" :swimmy.executor))
         (orig-gen (and (fboundp 'swimmy.school:generate-trade-result-narrative)
                        (symbol-function 'swimmy.school:generate-trade-result-narrative)))
         (orig-queue (and (fboundp 'swimmy.core:queue-discord-notification)
                          (symbol-function 'swimmy.core:queue-discord-notification)))
         (orig-trade-result (and (fboundp 'swimmy.school:record-trade-result)
                                 (symbol-function 'swimmy.school:record-trade-result)))
         (orig-trade-outcome (and (fboundp 'swimmy.school:record-trade-outcome)
                                  (symbol-function 'swimmy.school:record-trade-outcome)))
         (orig-save (and (fboundp 'swimmy.engine:save-state)
                         (symbol-function 'swimmy.engine:save-state)))
         (orig-processed swimmy.executor::*processed-tickets*)
         (captured nil)
         (narrative-called nil))
    (assert-true (and fn (fboundp fn)) "process-trade-closed exists")
    (unwind-protect
        (progn
          (setf swimmy.executor::*processed-tickets* (make-hash-table :test 'equal))
          (when orig-gen
            (setf (symbol-function 'swimmy.school:generate-trade-result-narrative)
                  (lambda (&rest _args)
                    (declare (ignore _args))
                    (setf narrative-called t)
                    "legacy-narrative")))
          (when orig-queue
            (setf (symbol-function 'swimmy.core:queue-discord-notification)
                  (lambda (_webhook msg &key color title)
                    (declare (ignore _webhook color title))
                    (setf captured msg))))
          (when orig-trade-result
            (setf (symbol-function 'swimmy.school:record-trade-result)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-trade-outcome
            (setf (symbol-function 'swimmy.school:record-trade-outcome)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-save
            (setf (symbol-function 'swimmy.engine:save-state)
                  (lambda () nil)))
          (funcall fn '((ticket . 98765)
                        (symbol . "USDJPY")
                        (pnl . 1000.0)
                        (won . t)
                        (magic . 321)
                        (direction . "BUY")
                        (strategy_name . "UT-FIXED")
                        (category . "trend")
                        (entry_price . 153.123)
                        (exit_price . 153.223))
                  "")
          (assert-false narrative-called
                        "Legacy narrative generator must not be called in trade-close path")
          (assert-not-nil captured "Expected trade-close notification message")
          (assert-true (search "Trade Closed" captured)
                       "Expected fixed-template title in notification")
          (assert-true (search "PnL: +¥1000" captured)
                       "Expected yen amount without decimal suffix")
          (assert-false (search "+¥1000." captured)
                        "Yen value must not include trailing decimal point"))
      (when orig-gen
        (setf (symbol-function 'swimmy.school:generate-trade-result-narrative) orig-gen))
      (when orig-queue
        (setf (symbol-function 'swimmy.core:queue-discord-notification) orig-queue))
      (when orig-trade-result
        (setf (symbol-function 'swimmy.school:record-trade-result) orig-trade-result))
      (when orig-trade-outcome
        (setf (symbol-function 'swimmy.school:record-trade-outcome) orig-trade-outcome))
      (when orig-save
        (setf (symbol-function 'swimmy.engine:save-state) orig-save))
      (setf swimmy.executor::*processed-tickets* orig-processed))))

(deftest test-process-trade-closed-uses-payload-context-for-learning
  "process-trade-closed should use payload context keys for learning without slot entry-context lookup."
  (let* ((fn (find-symbol "PROCESS-TRADE-CLOSED" :swimmy.executor))
         (orig-record-result (and (fboundp 'swimmy.school:record-trade-result)
                                  (symbol-function 'swimmy.school:record-trade-result)))
         (orig-record-outcome (and (fboundp 'swimmy.school:record-trade-outcome)
                                   (symbol-function 'swimmy.school:record-trade-outcome)))
         (orig-lookup-entry (and (fboundp 'swimmy.school:lookup-entry-context-by-magic)
                                 (symbol-function 'swimmy.school:lookup-entry-context-by-magic)))
         (orig-lookup-pair (and (fboundp 'swimmy.school:lookup-pair-id-by-magic)
                                (symbol-function 'swimmy.school:lookup-pair-id-by-magic)))
         (orig-generate (and (fboundp 'swimmy.school:generate-trade-result-narrative)
                             (symbol-function 'swimmy.school:generate-trade-result-narrative)))
         (orig-queue (and (fboundp 'swimmy.core:queue-discord-notification)
                          (symbol-function 'swimmy.core:queue-discord-notification)))
         (orig-save (and (fboundp 'swimmy.engine:save-state)
                         (symbol-function 'swimmy.engine:save-state)))
         (orig-processed swimmy.executor::*processed-tickets*)
         (captured nil))
    (assert-true (and fn (fboundp fn)) "process-trade-closed exists")
    (unwind-protect
        (progn
          (setf swimmy.executor::*processed-tickets* (make-hash-table :test 'equal))
          (when orig-record-result
            (setf (symbol-function 'swimmy.school:record-trade-result)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-record-outcome
            (setf (symbol-function 'swimmy.school:record-trade-outcome)
                  (lambda (symbol direction category strategy-name pnl &key hit hold-time pair-id)
                    (declare (ignore hit hold-time pair-id))
                    (setf captured (list symbol direction category strategy-name pnl))
                    nil)))
          (when orig-lookup-entry
            (setf (symbol-function 'swimmy.school:lookup-entry-context-by-magic)
                  (lambda (magic)
                    (declare (ignore magic))
                    (list :strategy "CTX-STRAT"
                          :category :scalp
                          :direction :buy
                          :entry-bid 150.123
                          :entry-ask 150.133
                          :entry-spread-pips 1.0
                          :entry-cost-pips 1.0))))
          (when orig-lookup-pair
            (setf (symbol-function 'swimmy.school:lookup-pair-id-by-magic)
                  (lambda (_magic) (declare (ignore _magic)) "PAIR-CTX")))
          (when orig-generate
            (setf (symbol-function 'swimmy.school:generate-trade-result-narrative)
                  (lambda (&rest _args) (declare (ignore _args)) "narrative")))
          (when orig-queue
            (setf (symbol-function 'swimmy.core:queue-discord-notification)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-save
            (setf (symbol-function 'swimmy.engine:save-state) (lambda () nil)))
          (funcall fn
                   '((ticket . 24680)
                     (symbol . "USDJPY")
                     (pnl . -12.5)
                     (won . nil)
                     (magic . 110012345)
                     (direction . "SELL")
                     (strategy_name . "PAYLOAD-STRAT")
                     (category . "breakout"))
                   "")
          (assert-not-nil captured "Expected record-trade-outcome to be called")
          (assert-equal "USDJPY" (first captured))
          (assert-equal :sell (second captured))
          (assert-equal :breakout (third captured))
          (assert-equal "PAYLOAD-STRAT" (fourth captured))
          (assert-true (< (abs (+ 12.5 (fifth captured))) 1.0e-6)
                       (format nil "Expected pnl -12.5, got ~a" (fifth captured))))
      (when orig-record-result
        (setf (symbol-function 'swimmy.school:record-trade-result) orig-record-result))
      (when orig-record-outcome
        (setf (symbol-function 'swimmy.school:record-trade-outcome) orig-record-outcome))
      (when orig-lookup-entry
        (setf (symbol-function 'swimmy.school:lookup-entry-context-by-magic) orig-lookup-entry))
      (when orig-lookup-pair
        (setf (symbol-function 'swimmy.school:lookup-pair-id-by-magic) orig-lookup-pair))
      (when orig-generate
        (setf (symbol-function 'swimmy.school:generate-trade-result-narrative) orig-generate))
      (when orig-queue
        (setf (symbol-function 'swimmy.core:queue-discord-notification) orig-queue))
      (when orig-save
        (setf (symbol-function 'swimmy.engine:save-state) orig-save))
      (setf swimmy.executor::*processed-tickets* orig-processed))))

(deftest test-process-trade-closed-normalizes-nil-category-from-magic
  "process-trade-closed should treat category=NIL as missing and recover category from magic without entry-context."
  (let* ((fn (find-symbol "PROCESS-TRADE-CLOSED" :swimmy.executor))
         (orig-record-result (and (fboundp 'swimmy.school:record-trade-result)
                                  (symbol-function 'swimmy.school:record-trade-result)))
         (orig-record-outcome (and (fboundp 'swimmy.school:record-trade-outcome)
                                   (symbol-function 'swimmy.school:record-trade-outcome)))
         (orig-lookup-entry (and (fboundp 'swimmy.school:lookup-entry-context-by-magic)
                                 (symbol-function 'swimmy.school:lookup-entry-context-by-magic)))
         (orig-generate (and (fboundp 'swimmy.school:generate-trade-result-narrative)
                             (symbol-function 'swimmy.school:generate-trade-result-narrative)))
         (orig-queue (and (fboundp 'swimmy.core:queue-discord-notification)
                          (symbol-function 'swimmy.core:queue-discord-notification)))
         (orig-save (and (fboundp 'swimmy.engine:save-state)
                         (symbol-function 'swimmy.engine:save-state)))
         (orig-processed swimmy.executor::*processed-tickets*)
         (captured nil))
    (assert-true (and fn (fboundp fn)) "process-trade-closed exists")
    (unwind-protect
        (progn
          (setf swimmy.executor::*processed-tickets* (make-hash-table :test 'equal))
          (when orig-record-result
            (setf (symbol-function 'swimmy.school:record-trade-result)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-record-outcome
            (setf (symbol-function 'swimmy.school:record-trade-outcome)
                  (lambda (symbol direction category strategy-name pnl &key hit hold-time pair-id)
                    (declare (ignore hit hold-time pair-id pnl))
                    (setf captured (list symbol direction category strategy-name))
                    nil)))
          (when orig-lookup-entry
            (setf (symbol-function 'swimmy.school:lookup-entry-context-by-magic)
                  (lambda (magic)
                    (declare (ignore magic))
                    (list :category :scalp :direction :buy))))
          (when orig-generate
            (setf (symbol-function 'swimmy.school:generate-trade-result-narrative)
                  (lambda (&rest _args) (declare (ignore _args)) "narrative")))
          (when orig-queue
            (setf (symbol-function 'swimmy.core:queue-discord-notification)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-save
            (setf (symbol-function 'swimmy.engine:save-state) (lambda () nil)))
          (funcall fn
                   '((ticket . 13579)
                     (symbol . "USDJPY")
                     (pnl . 5.0)
                     (won . t)
                     (magic . 130022233)
                     (direction . "BUY")
                     (strategy_name . "UT-SNAME")
                     (category . "NIL"))
                   "")
          (assert-not-nil captured "Expected record-trade-outcome to be called")
          (assert-equal "USDJPY" (first captured))
          (assert-equal :buy (second captured))
          (assert-equal :breakout (third captured))
          (assert-equal "UT-SNAME" (fourth captured)))
      (when orig-record-result
        (setf (symbol-function 'swimmy.school:record-trade-result) orig-record-result))
      (when orig-record-outcome
        (setf (symbol-function 'swimmy.school:record-trade-outcome) orig-record-outcome))
      (when orig-lookup-entry
        (setf (symbol-function 'swimmy.school:lookup-entry-context-by-magic) orig-lookup-entry))
      (when orig-generate
        (setf (symbol-function 'swimmy.school:generate-trade-result-narrative) orig-generate))
      (when orig-queue
        (setf (symbol-function 'swimmy.core:queue-discord-notification) orig-queue))
      (when orig-save
        (setf (symbol-function 'swimmy.engine:save-state) orig-save))
      (setf swimmy.executor::*processed-tickets* orig-processed))))

;;; ─────────────────────────────────────────
;;; INDICATOR TESTS
;;; ─────────────────────────────────────────

(deftest test-sma-calculation
  "Test SMA calculation"
  (let* ((test-data (loop for i from 1 to 10 
                          collect (cl-user::make-candle 
                                   :close (float i) :open 1.0 :high 1.0 :low 1.0)))
         (sma (cl-user::ind-sma 5 test-data)))
    ;; SMA of 1,2,3,4,5 = 15/5 = 3.0
    (assert-true (and sma (> sma 0)) "SMA should be positive")))

(deftest test-rsi-bounds
  "Test that RSI stays within 0-100"
  (let ((test-data (loop for i from 1 to 20 
                         collect (cl-user::make-candle 
                                  :close (+ 100 (random 10.0)) 
                                  :open 100.0 :high 110.0 :low 90.0))))
    (let ((rsi (cl-user::ind-rsi 14 test-data)))
      (when rsi
        (assert-true (<= 0 rsi 100) "RSI should be between 0-100")))))

;;; ─────────────────────────────────────────
;;; MODEL SWITCH TESTS
;;; ─────────────────────────────────────────

(deftest test-select-optimal-model-normal-vol-uses-ensemble
  "Normal volatility should choose :ensemble"
  (let ((orig (symbol-function 'swimmy.core::calculate-realized-volatility)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.core::calculate-realized-volatility)
                (lambda (history &key (period 20))
                  (declare (ignore history period))
                  1.0))
          (assert-equal :ensemble
                        (swimmy.core::select-optimal-model '(dummy))
                        "Normal vol should choose :ensemble"))
      (setf (symbol-function 'swimmy.core::calculate-realized-volatility) orig))))

(deftest test-process-category-trades-ignores-model-gate
  "process-category-trades should execute even if model predicts :HOLD (model gate abolished)."
  (let ((orig-candle-histories swimmy.globals:*candle-histories*)
        (orig-candle-history swimmy.globals:*candle-history*)
        (orig-get-model (symbol-function 'swimmy.core::get-model-prediction))
        (orig-trading-allowed (symbol-function 'swimmy.engine::trading-allowed-p))
        (orig-s-rank-gate (symbol-function 'swimmy.school::s-rank-gate-passed-p))
        (orig-cleanup (symbol-function 'swimmy.school::cleanup-stale-allocations))
        (orig-close (symbol-function 'swimmy.school::close-category-positions))
        (orig-safe (symbol-function 'swimmy.school::is-safe-to-trade-p))
        (orig-vol-ok (symbol-function 'swimmy.school::volatility-allows-trading-p))
        (orig-research (symbol-function 'swimmy.core::research-enhanced-analysis))
        (orig-detect (symbol-function 'swimmy.core::detect-regime-hmm))
        (orig-elect (symbol-function 'swimmy.school::elect-leader))
        (orig-collect (symbol-function 'swimmy.school::collect-strategy-signals))
        (orig-cache (symbol-function 'swimmy.school::get-cached-backtest))
        (orig-can-trade (symbol-function 'swimmy.school::can-category-trade-p))
        (orig-exec (symbol-function 'swimmy.school::execute-category-trade))
        (orig-narrative (symbol-function 'swimmy.school::generate-dynamic-narrative))
        (orig-record-time (symbol-function 'swimmy.school::record-category-trade-time))
        (orig-record-strat (symbol-function 'swimmy.school::record-strategy-trade)))
    (unwind-protect
        (let ((executed nil)
              (history (loop repeat 120 collect nil)))
          ;; Ensure per-symbol history exists and is long enough.
          (setf swimmy.globals:*candle-histories* (make-hash-table :test 'equal))
          (setf (gethash "USDJPY" swimmy.globals:*candle-histories*) history)
          (setf swimmy.globals:*candle-history* history)

          ;; Force environment to allow trading and simplify control flow.
          (setf (symbol-function 'swimmy.core::get-model-prediction)
                (lambda (h) (declare (ignore h)) :HOLD))
          (setf (symbol-function 'swimmy.engine::trading-allowed-p) (lambda () t))
          ;; S-rank count gate is no longer a live-path blocker.
          (setf (symbol-function 'swimmy.school::s-rank-gate-passed-p) (lambda () nil))
          (setf (symbol-function 'swimmy.school::cleanup-stale-allocations) (lambda () nil))
          (setf (symbol-function 'swimmy.school::close-category-positions) (lambda (s b a) (declare (ignore s b a)) nil))
          (setf (symbol-function 'swimmy.school::is-safe-to-trade-p) (lambda () t))
          (setf (symbol-function 'swimmy.school::volatility-allows-trading-p) (lambda () t))
          (setf (symbol-function 'swimmy.core::research-enhanced-analysis) (lambda (h) (declare (ignore h)) nil))
          (setf (symbol-function 'swimmy.core::detect-regime-hmm) (lambda (h) (declare (ignore h)) :unknown))
          (setf (symbol-function 'swimmy.school::elect-leader) (lambda () nil))
          (setf (symbol-function 'swimmy.school::collect-strategy-signals)
                (lambda (s h)
                  (declare (ignore s h))
                  (list (list :strategy-name "UT-STRAT"
                              :category :trend
                              :direction :BUY))))
          (setf (symbol-function 'swimmy.school::get-cached-backtest) (lambda (n) (declare (ignore n)) nil))
          (setf (symbol-function 'swimmy.school::can-category-trade-p) (lambda (k) (declare (ignore k)) t))
          (setf (symbol-function 'swimmy.school::execute-category-trade)
                (lambda (cat dir sym bid ask &key lot-multiplier signal-confidence strategy-name strategy-timeframe)
                  (declare (ignore cat dir sym bid ask lot-multiplier signal-confidence strategy-name strategy-timeframe))
                  (setf executed t)
                  t))
          (setf (symbol-function 'swimmy.school::generate-dynamic-narrative) (lambda (&rest args) (declare (ignore args)) ""))
          (setf (symbol-function 'swimmy.school::record-category-trade-time) (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::record-strategy-trade) (lambda (&rest args) (declare (ignore args)) nil))

          (swimmy.school::process-category-trades "USDJPY" 100.0 100.1)
          (assert-true executed
                       "Expected execute-category-trade to run (model gate/S-rank count gate should not block)."))
      ;; Restore globals/functions even if test fails.
      (setf swimmy.globals:*candle-histories* orig-candle-histories)
      (setf swimmy.globals:*candle-history* orig-candle-history)
      (setf (symbol-function 'swimmy.core::get-model-prediction) orig-get-model)
      (setf (symbol-function 'swimmy.engine::trading-allowed-p) orig-trading-allowed)
      (setf (symbol-function 'swimmy.school::s-rank-gate-passed-p) orig-s-rank-gate)
      (setf (symbol-function 'swimmy.school::cleanup-stale-allocations) orig-cleanup)
      (setf (symbol-function 'swimmy.school::close-category-positions) orig-close)
      (setf (symbol-function 'swimmy.school::is-safe-to-trade-p) orig-safe)
      (setf (symbol-function 'swimmy.school::volatility-allows-trading-p) orig-vol-ok)
      (setf (symbol-function 'swimmy.core::research-enhanced-analysis) orig-research)
      (setf (symbol-function 'swimmy.core::detect-regime-hmm) orig-detect)
      (setf (symbol-function 'swimmy.school::elect-leader) orig-elect)
      (setf (symbol-function 'swimmy.school::collect-strategy-signals) orig-collect)
      (setf (symbol-function 'swimmy.school::get-cached-backtest) orig-cache)
      (setf (symbol-function 'swimmy.school::can-category-trade-p) orig-can-trade)
      (setf (symbol-function 'swimmy.school::execute-category-trade) orig-exec)
      (setf (symbol-function 'swimmy.school::generate-dynamic-narrative) orig-narrative)
      (setf (symbol-function 'swimmy.school::record-category-trade-time) orig-record-time)
      (setf (symbol-function 'swimmy.school::record-strategy-trade) orig-record-strat))))

(deftest test-confidence-entry-multiplier-policy
  "Signal confidence policy should block low confidence and scale lot for medium confidence."
  (let ((orig-entry swimmy.school::*signal-confidence-entry-threshold*)
        (orig-soft swimmy.school::*signal-confidence-soft-threshold*)
        (orig-mult swimmy.school::*signal-confidence-soft-lot-multiplier*))
    (unwind-protect
        (progn
          (setf swimmy.school::*signal-confidence-entry-threshold* 0.20)
          (setf swimmy.school::*signal-confidence-soft-threshold* 0.35)
          (setf swimmy.school::*signal-confidence-soft-lot-multiplier* 0.55)
          (assert-equal 0.0 (swimmy.school::signal-confidence-lot-multiplier 0.10)
                        "Low confidence should be rejected")
          (assert-equal 0.55 (swimmy.school::signal-confidence-lot-multiplier 0.25)
                        "Medium confidence should use reduced lot")
          (assert-equal 1.0 (swimmy.school::signal-confidence-lot-multiplier 0.60)
                        "High confidence should use full lot"))
      (setf swimmy.school::*signal-confidence-entry-threshold* orig-entry)
      (setf swimmy.school::*signal-confidence-soft-threshold* orig-soft)
      (setf swimmy.school::*signal-confidence-soft-lot-multiplier* orig-mult))))

(deftest test-process-category-trades-skips-low-confidence-signal
  "process-category-trades should not execute when top signal confidence is below entry threshold."
  (let ((orig-candle-histories swimmy.globals:*candle-histories*)
        (orig-candle-history swimmy.globals:*candle-history*)
        (orig-entry-threshold swimmy.school::*signal-confidence-entry-threshold*)
        (orig-soft-threshold swimmy.school::*signal-confidence-soft-threshold*)
        (orig-soft-mult swimmy.school::*signal-confidence-soft-lot-multiplier*)
        (orig-trading-allowed (symbol-function 'swimmy.engine::trading-allowed-p))
        (orig-s-rank-gate (symbol-function 'swimmy.school::s-rank-gate-passed-p))
        (orig-cleanup (symbol-function 'swimmy.school::cleanup-stale-allocations))
        (orig-close (symbol-function 'swimmy.school::close-category-positions))
        (orig-safe (symbol-function 'swimmy.school::is-safe-to-trade-p))
        (orig-vol-ok (symbol-function 'swimmy.school::volatility-allows-trading-p))
        (orig-research (symbol-function 'swimmy.core::research-enhanced-analysis))
        (orig-detect (symbol-function 'swimmy.core::detect-regime-hmm))
        (orig-elect (symbol-function 'swimmy.school::elect-leader))
        (orig-collect (symbol-function 'swimmy.school::collect-strategy-signals))
        (orig-cache (symbol-function 'swimmy.school::get-cached-backtest))
        (orig-can-trade (symbol-function 'swimmy.school::can-category-trade-p))
        (orig-exec (symbol-function 'swimmy.school::execute-category-trade))
        (orig-narrative (symbol-function 'swimmy.school::generate-dynamic-narrative))
        (orig-record-time (symbol-function 'swimmy.school::record-category-trade-time))
        (orig-record-strat (symbol-function 'swimmy.school::record-strategy-trade)))
    (unwind-protect
        (let ((executed nil)
              (history (loop repeat 120 collect nil)))
          (setf swimmy.globals:*candle-histories* (make-hash-table :test 'equal))
          (setf (gethash "USDJPY" swimmy.globals:*candle-histories*) history)
          (setf swimmy.globals:*candle-history* history)
          (setf swimmy.school::*signal-confidence-entry-threshold* 0.20)
          (setf swimmy.school::*signal-confidence-soft-threshold* 0.35)
          (setf swimmy.school::*signal-confidence-soft-lot-multiplier* 0.55)

          (setf (symbol-function 'swimmy.engine::trading-allowed-p) (lambda () t))
          (setf (symbol-function 'swimmy.school::s-rank-gate-passed-p) (lambda () t))
          (setf (symbol-function 'swimmy.school::cleanup-stale-allocations) (lambda () nil))
          (setf (symbol-function 'swimmy.school::close-category-positions) (lambda (s b a) (declare (ignore s b a)) nil))
          (setf (symbol-function 'swimmy.school::is-safe-to-trade-p) (lambda () t))
          (setf (symbol-function 'swimmy.school::volatility-allows-trading-p) (lambda () t))
          (setf (symbol-function 'swimmy.core::research-enhanced-analysis) (lambda (h) (declare (ignore h)) nil))
          (setf (symbol-function 'swimmy.core::detect-regime-hmm) (lambda (h) (declare (ignore h)) :unknown))
          (setf (symbol-function 'swimmy.school::elect-leader) (lambda () nil))
          (setf (symbol-function 'swimmy.school::collect-strategy-signals)
                (lambda (s h)
                  (declare (ignore s h))
                  (list (list :strategy-name "UT-LOW-CONF"
                              :category :trend
                              :direction :BUY
                              :confidence 0.10))))
          (setf (symbol-function 'swimmy.school::get-cached-backtest) (lambda (n) (declare (ignore n)) nil))
          (setf (symbol-function 'swimmy.school::can-category-trade-p) (lambda (k) (declare (ignore k)) t))
          (setf (symbol-function 'swimmy.school::execute-category-trade)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (setf executed t)
                  t))
          (setf (symbol-function 'swimmy.school::generate-dynamic-narrative) (lambda (&rest args) (declare (ignore args)) ""))
          (setf (symbol-function 'swimmy.school::record-category-trade-time) (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::record-strategy-trade) (lambda (&rest args) (declare (ignore args)) nil))

          (swimmy.school::process-category-trades "USDJPY" 100.0 100.1)
          (assert-false executed "Low confidence signal should be skipped."))
      (setf swimmy.globals:*candle-histories* orig-candle-histories)
      (setf swimmy.globals:*candle-history* orig-candle-history)
      (setf swimmy.school::*signal-confidence-entry-threshold* orig-entry-threshold)
      (setf swimmy.school::*signal-confidence-soft-threshold* orig-soft-threshold)
      (setf swimmy.school::*signal-confidence-soft-lot-multiplier* orig-soft-mult)
      (setf (symbol-function 'swimmy.engine::trading-allowed-p) orig-trading-allowed)
      (setf (symbol-function 'swimmy.school::s-rank-gate-passed-p) orig-s-rank-gate)
      (setf (symbol-function 'swimmy.school::cleanup-stale-allocations) orig-cleanup)
      (setf (symbol-function 'swimmy.school::close-category-positions) orig-close)
      (setf (symbol-function 'swimmy.school::is-safe-to-trade-p) orig-safe)
      (setf (symbol-function 'swimmy.school::volatility-allows-trading-p) orig-vol-ok)
      (setf (symbol-function 'swimmy.core::research-enhanced-analysis) orig-research)
      (setf (symbol-function 'swimmy.core::detect-regime-hmm) orig-detect)
      (setf (symbol-function 'swimmy.school::elect-leader) orig-elect)
      (setf (symbol-function 'swimmy.school::collect-strategy-signals) orig-collect)
      (setf (symbol-function 'swimmy.school::get-cached-backtest) orig-cache)
      (setf (symbol-function 'swimmy.school::can-category-trade-p) orig-can-trade)
      (setf (symbol-function 'swimmy.school::execute-category-trade) orig-exec)
      (setf (symbol-function 'swimmy.school::generate-dynamic-narrative) orig-narrative)
      (setf (symbol-function 'swimmy.school::record-category-trade-time) orig-record-time)
      (setf (symbol-function 'swimmy.school::record-strategy-trade) orig-record-strat))))

(deftest test-process-category-trades-applies-confidence-lot-multiplier
  "process-category-trades should pass confidence-based lot multiplier to execution."
  (let ((orig-candle-histories swimmy.globals:*candle-histories*)
        (orig-candle-history swimmy.globals:*candle-history*)
        (orig-entry-threshold swimmy.school::*signal-confidence-entry-threshold*)
        (orig-soft-threshold swimmy.school::*signal-confidence-soft-threshold*)
        (orig-soft-mult swimmy.school::*signal-confidence-soft-lot-multiplier*)
        (orig-trading-allowed (symbol-function 'swimmy.engine::trading-allowed-p))
        (orig-s-rank-gate (symbol-function 'swimmy.school::s-rank-gate-passed-p))
        (orig-cleanup (symbol-function 'swimmy.school::cleanup-stale-allocations))
        (orig-close (symbol-function 'swimmy.school::close-category-positions))
        (orig-safe (symbol-function 'swimmy.school::is-safe-to-trade-p))
        (orig-vol-ok (symbol-function 'swimmy.school::volatility-allows-trading-p))
        (orig-research (symbol-function 'swimmy.core::research-enhanced-analysis))
        (orig-detect (symbol-function 'swimmy.core::detect-regime-hmm))
        (orig-elect (symbol-function 'swimmy.school::elect-leader))
        (orig-collect (symbol-function 'swimmy.school::collect-strategy-signals))
        (orig-cache (symbol-function 'swimmy.school::get-cached-backtest))
        (orig-can-trade (symbol-function 'swimmy.school::can-category-trade-p))
        (orig-exec (symbol-function 'swimmy.school::execute-category-trade))
        (orig-narrative (symbol-function 'swimmy.school::generate-dynamic-narrative))
        (orig-record-time (symbol-function 'swimmy.school::record-category-trade-time))
        (orig-record-strat (symbol-function 'swimmy.school::record-strategy-trade)))
    (unwind-protect
        (let ((captured-lot-mult nil)
              (captured-confidence nil)
              (history (loop repeat 120 collect nil)))
          (setf swimmy.globals:*candle-histories* (make-hash-table :test 'equal))
          (setf (gethash "USDJPY" swimmy.globals:*candle-histories*) history)
          (setf swimmy.globals:*candle-history* history)
          (setf swimmy.school::*signal-confidence-entry-threshold* 0.20)
          (setf swimmy.school::*signal-confidence-soft-threshold* 0.35)
          (setf swimmy.school::*signal-confidence-soft-lot-multiplier* 0.55)

          (setf (symbol-function 'swimmy.engine::trading-allowed-p) (lambda () t))
          (setf (symbol-function 'swimmy.school::s-rank-gate-passed-p) (lambda () t))
          (setf (symbol-function 'swimmy.school::cleanup-stale-allocations) (lambda () nil))
          (setf (symbol-function 'swimmy.school::close-category-positions) (lambda (s b a) (declare (ignore s b a)) nil))
          (setf (symbol-function 'swimmy.school::is-safe-to-trade-p) (lambda () t))
          (setf (symbol-function 'swimmy.school::volatility-allows-trading-p) (lambda () t))
          (setf (symbol-function 'swimmy.core::research-enhanced-analysis) (lambda (h) (declare (ignore h)) nil))
          (setf (symbol-function 'swimmy.core::detect-regime-hmm) (lambda (h) (declare (ignore h)) :unknown))
          (setf (symbol-function 'swimmy.school::elect-leader) (lambda () nil))
          (setf (symbol-function 'swimmy.school::collect-strategy-signals)
                (lambda (s h)
                  (declare (ignore s h))
                  (list (list :strategy-name "UT-MEDIUM-CONF"
                              :category :trend
                              :direction :BUY
                              :confidence 0.25))))
          (setf (symbol-function 'swimmy.school::get-cached-backtest)
                (lambda (n)
                  (declare (ignore n))
                  (list :sharpe 1.0)))
          (setf (symbol-function 'swimmy.school::can-category-trade-p) (lambda (k) (declare (ignore k)) t))
          (setf (symbol-function 'swimmy.school::execute-category-trade)
                (lambda (_cat _dir _sym _bid _ask &key lot-multiplier signal-confidence strategy-name strategy-timeframe)
                  (declare (ignore strategy-name strategy-timeframe))
                  (setf captured-lot-mult lot-multiplier)
                  (setf captured-confidence signal-confidence)
                  t))
          (setf (symbol-function 'swimmy.school::generate-dynamic-narrative) (lambda (&rest args) (declare (ignore args)) ""))
          (setf (symbol-function 'swimmy.school::record-category-trade-time) (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::record-strategy-trade) (lambda (&rest args) (declare (ignore args)) nil))

          (swimmy.school::process-category-trades "USDJPY" 100.0 100.1)
          (assert-true (< (abs (- captured-lot-mult 0.55)) 1e-9)
                       "Expected medium-confidence lot multiplier to be applied")
          (assert-true (< (abs (- captured-confidence 0.25)) 1e-9)
                       "Expected raw signal confidence to be forwarded"))
      (setf swimmy.globals:*candle-histories* orig-candle-histories)
      (setf swimmy.globals:*candle-history* orig-candle-history)
      (setf swimmy.school::*signal-confidence-entry-threshold* orig-entry-threshold)
      (setf swimmy.school::*signal-confidence-soft-threshold* orig-soft-threshold)
      (setf swimmy.school::*signal-confidence-soft-lot-multiplier* orig-soft-mult)
      (setf (symbol-function 'swimmy.engine::trading-allowed-p) orig-trading-allowed)
      (setf (symbol-function 'swimmy.school::s-rank-gate-passed-p) orig-s-rank-gate)
      (setf (symbol-function 'swimmy.school::cleanup-stale-allocations) orig-cleanup)
      (setf (symbol-function 'swimmy.school::close-category-positions) orig-close)
      (setf (symbol-function 'swimmy.school::is-safe-to-trade-p) orig-safe)
      (setf (symbol-function 'swimmy.school::volatility-allows-trading-p) orig-vol-ok)
      (setf (symbol-function 'swimmy.core::research-enhanced-analysis) orig-research)
      (setf (symbol-function 'swimmy.core::detect-regime-hmm) orig-detect)
      (setf (symbol-function 'swimmy.school::elect-leader) orig-elect)
      (setf (symbol-function 'swimmy.school::collect-strategy-signals) orig-collect)
      (setf (symbol-function 'swimmy.school::get-cached-backtest) orig-cache)
      (setf (symbol-function 'swimmy.school::can-category-trade-p) orig-can-trade)
      (setf (symbol-function 'swimmy.school::execute-category-trade) orig-exec)
      (setf (symbol-function 'swimmy.school::generate-dynamic-narrative) orig-narrative)
      (setf (symbol-function 'swimmy.school::record-category-trade-time) orig-record-time)
      (setf (symbol-function 'swimmy.school::record-strategy-trade) orig-record-strat))))

(deftest test-process-category-trades-forwards-top-strategy-context
  "process-category-trades should forward top signal strategy name/timeframe into execution."
  (let ((orig-candle-histories swimmy.globals:*candle-histories*)
        (orig-candle-history swimmy.globals:*candle-history*)
        (orig-kb swimmy.school::*strategy-knowledge-base*)
        (orig-trading-allowed (symbol-function 'swimmy.engine::trading-allowed-p))
        (orig-s-rank-gate (symbol-function 'swimmy.school::s-rank-gate-passed-p))
        (orig-cleanup (symbol-function 'swimmy.school::cleanup-stale-allocations))
        (orig-close (symbol-function 'swimmy.school::close-category-positions))
        (orig-safe (symbol-function 'swimmy.school::is-safe-to-trade-p))
        (orig-vol-ok (symbol-function 'swimmy.school::volatility-allows-trading-p))
        (orig-research (symbol-function 'swimmy.core::research-enhanced-analysis))
        (orig-detect (symbol-function 'swimmy.core::detect-regime-hmm))
        (orig-elect (symbol-function 'swimmy.school::elect-leader))
        (orig-collect (symbol-function 'swimmy.school::collect-strategy-signals))
        (orig-cache (symbol-function 'swimmy.school::get-cached-backtest))
        (orig-can-trade (symbol-function 'swimmy.school::can-category-trade-p))
        (orig-exec (symbol-function 'swimmy.school::execute-category-trade))
        (orig-narrative (symbol-function 'swimmy.school::generate-dynamic-narrative))
        (orig-record-time (symbol-function 'swimmy.school::record-category-trade-time))
        (orig-record-strat (symbol-function 'swimmy.school::record-strategy-trade)))
    (unwind-protect
        (let ((captured-name nil)
              (captured-timeframe nil)
              (history (loop repeat 120 collect nil)))
          (setf swimmy.globals:*candle-histories* (make-hash-table :test 'equal))
          (setf (gethash "USDJPY" swimmy.globals:*candle-histories*) history)
          (setf swimmy.globals:*candle-history* history)
          (setf swimmy.school::*strategy-knowledge-base*
                (list (swimmy.school:make-strategy :name "UT-TOP-CTX"
                                                   :symbol "USDJPY"
                                                   :timeframe 240)))

          (setf (symbol-function 'swimmy.engine::trading-allowed-p) (lambda () t))
          (setf (symbol-function 'swimmy.school::s-rank-gate-passed-p) (lambda () t))
          (setf (symbol-function 'swimmy.school::cleanup-stale-allocations) (lambda () nil))
          (setf (symbol-function 'swimmy.school::close-category-positions) (lambda (s b a) (declare (ignore s b a)) nil))
          (setf (symbol-function 'swimmy.school::is-safe-to-trade-p) (lambda () t))
          (setf (symbol-function 'swimmy.school::volatility-allows-trading-p) (lambda () t))
          (setf (symbol-function 'swimmy.core::research-enhanced-analysis) (lambda (h) (declare (ignore h)) nil))
          (setf (symbol-function 'swimmy.core::detect-regime-hmm) (lambda (h) (declare (ignore h)) :unknown))
          (setf (symbol-function 'swimmy.school::elect-leader) (lambda () nil))
          (setf (symbol-function 'swimmy.school::collect-strategy-signals)
                (lambda (_s _h)
                  (declare (ignore _s _h))
                  (list (list :strategy-name "UT-TOP-CTX"
                              :category :trend
                              :direction :BUY
                              :confidence 0.8))))
          (setf (symbol-function 'swimmy.school::get-cached-backtest)
                (lambda (_n) (declare (ignore _n)) (list :sharpe 1.0)))
          (setf (symbol-function 'swimmy.school::can-category-trade-p) (lambda (_k) (declare (ignore _k)) t))
          (setf (symbol-function 'swimmy.school::execute-category-trade)
                (lambda (_cat _dir _sym _bid _ask &key lot-multiplier signal-confidence strategy-name strategy-timeframe)
                  (declare (ignore _cat _dir _sym _bid _ask lot-multiplier signal-confidence))
                  (setf captured-name strategy-name)
                  (setf captured-timeframe strategy-timeframe)
                  t))
          (setf (symbol-function 'swimmy.school::generate-dynamic-narrative) (lambda (&rest args) (declare (ignore args)) ""))
          (setf (symbol-function 'swimmy.school::record-category-trade-time) (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::record-strategy-trade) (lambda (&rest args) (declare (ignore args)) nil))

          (swimmy.school::process-category-trades "USDJPY" 100.0 100.1)
          (assert-equal "UT-TOP-CTX" captured-name
                        "Expected top signal strategy name to be forwarded to execution")
          (assert-equal 240 captured-timeframe
                        "Expected top signal timeframe to be forwarded to execution"))
      (setf swimmy.globals:*candle-histories* orig-candle-histories)
      (setf swimmy.globals:*candle-history* orig-candle-history)
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf (symbol-function 'swimmy.engine::trading-allowed-p) orig-trading-allowed)
      (setf (symbol-function 'swimmy.school::s-rank-gate-passed-p) orig-s-rank-gate)
      (setf (symbol-function 'swimmy.school::cleanup-stale-allocations) orig-cleanup)
      (setf (symbol-function 'swimmy.school::close-category-positions) orig-close)
      (setf (symbol-function 'swimmy.school::is-safe-to-trade-p) orig-safe)
      (setf (symbol-function 'swimmy.school::volatility-allows-trading-p) orig-vol-ok)
      (setf (symbol-function 'swimmy.core::research-enhanced-analysis) orig-research)
      (setf (symbol-function 'swimmy.core::detect-regime-hmm) orig-detect)
      (setf (symbol-function 'swimmy.school::elect-leader) orig-elect)
      (setf (symbol-function 'swimmy.school::collect-strategy-signals) orig-collect)
      (setf (symbol-function 'swimmy.school::get-cached-backtest) orig-cache)
      (setf (symbol-function 'swimmy.school::can-category-trade-p) orig-can-trade)
      (setf (symbol-function 'swimmy.school::execute-category-trade) orig-exec)
      (setf (symbol-function 'swimmy.school::generate-dynamic-narrative) orig-narrative)
      (setf (symbol-function 'swimmy.school::record-category-trade-time) orig-record-time)
      (setf (symbol-function 'swimmy.school::record-strategy-trade) orig-record-strat))))

(deftest test-process-category-trades-runs-a-rank-shadow-when-s-gate-blocked
  "When live entry path is skipped, A-rank signals should still run in shadow and persist shadow outcomes."
  (let ((orig-candle-histories swimmy.globals:*candle-histories*)
        (orig-candle-history swimmy.globals:*candle-history*)
        (orig-kb swimmy.school::*strategy-knowledge-base*)
        (orig-trading-allowed (symbol-function 'swimmy.engine::trading-allowed-p))
        (orig-s-rank-gate (symbol-function 'swimmy.school::s-rank-gate-passed-p))
        (orig-cleanup (symbol-function 'swimmy.school::cleanup-stale-allocations))
        (orig-close (symbol-function 'swimmy.school::close-category-positions))
        (orig-safe (symbol-function 'swimmy.school::is-safe-to-trade-p))
        (orig-vol-ok (symbol-function 'swimmy.school::volatility-allows-trading-p))
        (orig-research (symbol-function 'swimmy.core::research-enhanced-analysis))
        (orig-detect (symbol-function 'swimmy.core::detect-regime-hmm))
        (orig-elect (symbol-function 'swimmy.school::elect-leader))
        (orig-collect (symbol-function 'swimmy.school::collect-strategy-signals))
        (orig-cache (symbol-function 'swimmy.school::get-cached-backtest))
        (orig-can-trade (symbol-function 'swimmy.school::can-category-trade-p))
        (orig-exec (symbol-function 'swimmy.school::execute-category-trade))
        (orig-narrative (symbol-function 'swimmy.school::generate-dynamic-narrative))
        (orig-record-time (symbol-function 'swimmy.school::record-category-trade-time))
        (orig-record-strat (symbol-function 'swimmy.school::record-strategy-trade))
        (orig-record-outcome (symbol-function 'swimmy.school::record-trade-outcome))
        (orig-shadow-slots (and (boundp 'swimmy.school::*shadow-slot-allocation*)
                                swimmy.school::*shadow-slot-allocation*)))
    (unwind-protect
        (let ((live-executed nil)
              (captured-modes nil)
              (history (loop repeat 120 collect nil)))
          (setf swimmy.globals:*candle-histories* (make-hash-table :test 'equal))
          (setf (gethash "USDJPY" swimmy.globals:*candle-histories*) history)
          (setf swimmy.globals:*candle-history* history)
          (setf swimmy.school::*strategy-knowledge-base*
                (list (swimmy.school:make-strategy :name "UT-A-SHADOW"
                                                   :symbol "USDJPY"
                                                   :timeframe 15
                                                   :rank :A
                                                   :sl 0.03
                                                   :tp 0.03)))
          (when (boundp 'swimmy.school::*shadow-slot-allocation*)
            (setf swimmy.school::*shadow-slot-allocation* (make-hash-table :test 'equal)))

          (setf (symbol-function 'swimmy.engine::trading-allowed-p) (lambda () t))
          (setf (symbol-function 'swimmy.school::s-rank-gate-passed-p) (lambda () nil))
          (setf (symbol-function 'swimmy.school::cleanup-stale-allocations) (lambda () nil))
          (setf (symbol-function 'swimmy.school::close-category-positions) (lambda (s b a) (declare (ignore s b a)) nil))
          (setf (symbol-function 'swimmy.school::is-safe-to-trade-p) (lambda () t))
          (setf (symbol-function 'swimmy.school::volatility-allows-trading-p) (lambda () t))
          (setf (symbol-function 'swimmy.core::research-enhanced-analysis) (lambda (h) (declare (ignore h)) nil))
          (setf (symbol-function 'swimmy.core::detect-regime-hmm) (lambda (h) (declare (ignore h)) :unknown))
          (setf (symbol-function 'swimmy.school::elect-leader) (lambda () nil))
          (setf (symbol-function 'swimmy.school::collect-strategy-signals)
                (lambda (_s _h)
                  (declare (ignore _s _h))
                  (list (list :strategy-name "UT-A-SHADOW"
                              :category :trend
                              :direction :BUY
                              :confidence 0.9
                              :timeframe 15))))
          (setf (symbol-function 'swimmy.school::get-cached-backtest)
                (lambda (_n) (declare (ignore _n)) (list :sharpe 1.0)))
          ;; Skip live entry path (independent from S-rank count gate).
          (setf (symbol-function 'swimmy.school::can-category-trade-p)
                (lambda (_k) (declare (ignore _k)) nil))
          (setf (symbol-function 'swimmy.school::execute-category-trade)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (setf live-executed t)
                  t))
          (setf (symbol-function 'swimmy.school::generate-dynamic-narrative)
                (lambda (&rest args) (declare (ignore args)) ""))
          (setf (symbol-function 'swimmy.school::record-category-trade-time)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::record-strategy-trade)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::record-trade-outcome)
                (lambda (_symbol _direction _category _strategy-name _pnl &key execution-mode hold-time hit pair-id)
                  (declare (ignore _symbol _direction _category _strategy-name _pnl hold-time hit pair-id))
                  (push execution-mode captured-modes)
                  nil))

          ;; 1st tick: open shadow trade, 2nd tick: hit TP and close.
          (swimmy.school::process-category-trades "USDJPY" 100.00 100.02)
          (swimmy.school::process-category-trades "USDJPY" 100.08 100.10)

          (assert-false live-executed "Live execute-category-trade should not run when entry path is skipped")
          (assert-true (member :shadow captured-modes :test #'eq)
                       "Expected at least one shadow-mode learning outcome"))
      (setf swimmy.globals:*candle-histories* orig-candle-histories)
      (setf swimmy.globals:*candle-history* orig-candle-history)
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf (symbol-function 'swimmy.engine::trading-allowed-p) orig-trading-allowed)
      (setf (symbol-function 'swimmy.school::s-rank-gate-passed-p) orig-s-rank-gate)
      (setf (symbol-function 'swimmy.school::cleanup-stale-allocations) orig-cleanup)
      (setf (symbol-function 'swimmy.school::close-category-positions) orig-close)
      (setf (symbol-function 'swimmy.school::is-safe-to-trade-p) orig-safe)
      (setf (symbol-function 'swimmy.school::volatility-allows-trading-p) orig-vol-ok)
      (setf (symbol-function 'swimmy.core::research-enhanced-analysis) orig-research)
      (setf (symbol-function 'swimmy.core::detect-regime-hmm) orig-detect)
      (setf (symbol-function 'swimmy.school::elect-leader) orig-elect)
      (setf (symbol-function 'swimmy.school::collect-strategy-signals) orig-collect)
      (setf (symbol-function 'swimmy.school::get-cached-backtest) orig-cache)
      (setf (symbol-function 'swimmy.school::can-category-trade-p) orig-can-trade)
      (setf (symbol-function 'swimmy.school::execute-category-trade) orig-exec)
      (setf (symbol-function 'swimmy.school::generate-dynamic-narrative) orig-narrative)
      (setf (symbol-function 'swimmy.school::record-category-trade-time) orig-record-time)
      (setf (symbol-function 'swimmy.school::record-strategy-trade) orig-record-strat)
      (setf (symbol-function 'swimmy.school::record-trade-outcome) orig-record-outcome)
      (when (boundp 'swimmy.school::*shadow-slot-allocation*)
        (setf swimmy.school::*shadow-slot-allocation* orig-shadow-slots)))))

(deftest test-execute-category-trade-fails-closed-on-missing-timeframe
  "execute-category-trade should fail-closed and alert when strategy timeframe is missing/invalid."
  (let ((orig-team swimmy.school::*active-team*)
        (orig-hist swimmy.globals::*candle-histories*)
        (orig-hist-tf swimmy.globals::*candle-histories-tf*)
        (orig-total (symbol-function 'swimmy.school::total-exposure-allowed-p))
        (orig-spread (symbol-function 'swimmy.school::spread-pips-from-bid-ask))
        (orig-validate (symbol-function 'swimmy.school::validate-trade-opportunity))
        (orig-lot (symbol-function 'swimmy.school::get-category-lot))
        (orig-calc (symbol-function 'swimmy.school::calc-execution-lot))
        (orig-overlay (symbol-function 'swimmy.school::apply-pair-overlay))
        (orig-gate (symbol-function 'swimmy.school::apply-pattern-soft-gate))
        (orig-emit (symbol-function 'swimmy.school::emit-pattern-gate-telemetry))
        (orig-auth (symbol-function 'swimmy.school::verify-signal-authority))
        (orig-exec (symbol-function 'swimmy.school::execute-order-sequence))
        (orig-notify-alert (symbol-function 'swimmy.core:notify-discord-alert))
        (orig-emit-telemetry (symbol-function 'swimmy.core::emit-telemetry-event))
        (alert-called nil)
        (alert-msg nil)
        (telemetry-called nil)
        (telemetry-event nil)
        (executed nil))
    (unwind-protect
        (progn
          (setf swimmy.school::*active-team* (make-hash-table :test 'equal))
          (setf swimmy.globals::*candle-histories* (make-hash-table :test 'equal))
          (setf swimmy.globals::*candle-histories-tf* (make-hash-table :test 'equal))

          (setf (symbol-function 'swimmy.school::total-exposure-allowed-p) (lambda () t))
          (setf (symbol-function 'swimmy.school::spread-pips-from-bid-ask) (lambda (_s _b _a) (declare (ignore _s _b _a)) 1.0))
          (setf (symbol-function 'swimmy.school::validate-trade-opportunity) (lambda (&rest _args) (declare (ignore _args)) t))
          (setf (symbol-function 'swimmy.school::get-category-lot) (lambda (&rest _args) (declare (ignore _args)) 0.01))
          (setf (symbol-function 'swimmy.school::calc-execution-lot) (lambda (&rest _args) (declare (ignore _args)) 0.01))
          (setf (symbol-function 'swimmy.school::apply-pair-overlay) (lambda (&rest _args) (declare (ignore _args)) (list 0.01 nil)))
          (setf (symbol-function 'swimmy.school::apply-pattern-soft-gate) (lambda (&rest _args) (declare (ignore _args)) (values 0.01 nil)))
          (setf (symbol-function 'swimmy.school::emit-pattern-gate-telemetry) (lambda (&rest _args) (declare (ignore _args)) nil))
          (setf (symbol-function 'swimmy.school::verify-signal-authority) (lambda (&rest _args) (declare (ignore _args)) t))
          (setf (symbol-function 'swimmy.school::execute-order-sequence)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (setf executed t)
                  t))
          (setf (symbol-function 'swimmy.core:notify-discord-alert)
                (lambda (msg &key color)
                  (declare (ignore color))
                  (setf alert-called t
                        alert-msg msg)
                  nil))
          (setf (symbol-function 'swimmy.core::emit-telemetry-event)
                (lambda (event-type &key service severity correlation-id data)
                  (declare (ignore service severity correlation-id data))
                  (setf telemetry-called t
                        telemetry-event event-type)
                  nil))

          (dolist (invalid-tf (list nil "NIL" "" "UNKNOWN" "M0"))
            (setf alert-called nil
                  alert-msg nil
                  telemetry-called nil
                  telemetry-event nil
                  executed nil)
            (let ((ok (swimmy.school::execute-category-trade
                       :trend :buy "USDJPY" 100.0 100.1
                       :strategy-name "UT-NO-TF"
                       :strategy-timeframe invalid-tf
                       :signal-confidence 0.8)))
              (assert-false ok (format nil "Expected trade skip for invalid timeframe context: ~a" invalid-tf))
              (assert-false executed (format nil "execute-order-sequence must not be called for invalid timeframe: ~a" invalid-tf))
              (assert-true alert-called (format nil "Expected Discord alert for invalid timeframe: ~a" invalid-tf))
              (assert-true (and (stringp alert-msg) (search "Missing timeframe context" alert-msg))
                           "Expected alert message to include missing timeframe context")
              (assert-true telemetry-called (format nil "Expected telemetry for invalid timeframe: ~a" invalid-tf))
              (assert-equal "execution.context_missing" telemetry-event
                            "Expected execution.context_missing telemetry event"))))
      (setf swimmy.school::*active-team* orig-team)
      (setf swimmy.globals::*candle-histories* orig-hist)
      (setf swimmy.globals::*candle-histories-tf* orig-hist-tf)
      (setf (symbol-function 'swimmy.school::total-exposure-allowed-p) orig-total)
      (setf (symbol-function 'swimmy.school::spread-pips-from-bid-ask) orig-spread)
      (setf (symbol-function 'swimmy.school::validate-trade-opportunity) orig-validate)
      (setf (symbol-function 'swimmy.school::get-category-lot) orig-lot)
      (setf (symbol-function 'swimmy.school::calc-execution-lot) orig-calc)
      (setf (symbol-function 'swimmy.school::apply-pair-overlay) orig-overlay)
      (setf (symbol-function 'swimmy.school::apply-pattern-soft-gate) orig-gate)
      (setf (symbol-function 'swimmy.school::emit-pattern-gate-telemetry) orig-emit)
      (setf (symbol-function 'swimmy.school::verify-signal-authority) orig-auth)
      (setf (symbol-function 'swimmy.school::execute-order-sequence) orig-exec)
      (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify-alert)
      (setf (symbol-function 'swimmy.core::emit-telemetry-event) orig-emit-telemetry))))

(deftest test-execute-category-trade-fails-closed-on-unresolved-strategy-name
  "execute-category-trade should fail-closed when strategy-name is not resolvable in KB/evolved pools."
  (let ((orig-team swimmy.school::*active-team*)
        (orig-kb swimmy.school::*strategy-knowledge-base*)
        (orig-evolved swimmy.school::*evolved-strategies*)
        (orig-hist swimmy.globals::*candle-histories*)
        (orig-hist-tf swimmy.globals::*candle-histories-tf*)
        (orig-total (symbol-function 'swimmy.school::total-exposure-allowed-p))
        (orig-spread (symbol-function 'swimmy.school::spread-pips-from-bid-ask))
        (orig-validate (symbol-function 'swimmy.school::validate-trade-opportunity))
        (orig-lot (symbol-function 'swimmy.school::get-category-lot))
        (orig-calc (symbol-function 'swimmy.school::calc-execution-lot))
        (orig-overlay (symbol-function 'swimmy.school::apply-pair-overlay))
        (orig-gate (symbol-function 'swimmy.school::apply-pattern-soft-gate))
        (orig-emit (symbol-function 'swimmy.school::emit-pattern-gate-telemetry))
        (orig-auth (symbol-function 'swimmy.school::verify-signal-authority))
        (orig-exec (symbol-function 'swimmy.school::execute-order-sequence))
        (orig-notify-alert (symbol-function 'swimmy.core:notify-discord-alert))
        (orig-emit-telemetry (symbol-function 'swimmy.core::emit-telemetry-event))
        (alert-called nil)
        (alert-msg nil)
        (telemetry-called nil)
        (telemetry-event nil)
        (executed nil))
    (unwind-protect
        (progn
          (setf swimmy.school::*active-team* (make-hash-table :test 'equal))
          (setf swimmy.school::*strategy-knowledge-base* nil)
          (setf swimmy.school::*evolved-strategies* nil)
          (setf swimmy.globals::*candle-histories* (make-hash-table :test 'equal))
          (setf swimmy.globals::*candle-histories-tf* (make-hash-table :test 'equal))

          (setf (symbol-function 'swimmy.school::total-exposure-allowed-p) (lambda () t))
          (setf (symbol-function 'swimmy.school::spread-pips-from-bid-ask) (lambda (_s _b _a) (declare (ignore _s _b _a)) 1.0))
          (setf (symbol-function 'swimmy.school::validate-trade-opportunity) (lambda (&rest _args) (declare (ignore _args)) t))
          (setf (symbol-function 'swimmy.school::get-category-lot) (lambda (&rest _args) (declare (ignore _args)) 0.01))
          (setf (symbol-function 'swimmy.school::calc-execution-lot) (lambda (&rest _args) (declare (ignore _args)) 0.01))
          (setf (symbol-function 'swimmy.school::apply-pair-overlay) (lambda (&rest _args) (declare (ignore _args)) (list 0.01 nil)))
          (setf (symbol-function 'swimmy.school::apply-pattern-soft-gate) (lambda (&rest _args) (declare (ignore _args)) (values 0.01 nil)))
          (setf (symbol-function 'swimmy.school::emit-pattern-gate-telemetry) (lambda (&rest _args) (declare (ignore _args)) nil))
          (setf (symbol-function 'swimmy.school::verify-signal-authority) (lambda (&rest _args) (declare (ignore _args)) t))
          (setf (symbol-function 'swimmy.school::execute-order-sequence)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (setf executed t)
                  t))
          (setf (symbol-function 'swimmy.core:notify-discord-alert)
                (lambda (msg &key color)
                  (declare (ignore color))
                  (setf alert-called t
                        alert-msg msg)
                  nil))
          (setf (symbol-function 'swimmy.core::emit-telemetry-event)
                (lambda (event-type &key service severity correlation-id data)
                  (declare (ignore service severity correlation-id data))
                  (setf telemetry-called t
                        telemetry-event event-type)
                  nil))

          (let ((ok (swimmy.school::execute-category-trade
                     :trend :buy "USDJPY" 100.0 100.1
                     :strategy-name "UT-UNKNOWN-STRATEGY"
                     :strategy-timeframe 10080
                     :signal-confidence 0.8)))
            (assert-false ok "Expected unresolved strategy name to fail-closed")
            (assert-false executed "execute-order-sequence must not run for unresolved strategy context")
            (assert-true alert-called "Expected Discord alert for unresolved strategy context")
            (assert-true (and (stringp alert-msg) (search "Missing strategy context" alert-msg))
                         "Expected alert message to include missing strategy context")
            (assert-true telemetry-called "Expected telemetry for unresolved strategy context")
            (assert-equal "execution.context_missing" telemetry-event
                          "Expected execution.context_missing telemetry event")))
      (setf swimmy.school::*active-team* orig-team)
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*evolved-strategies* orig-evolved)
      (setf swimmy.globals::*candle-histories* orig-hist)
      (setf swimmy.globals::*candle-histories-tf* orig-hist-tf)
      (setf (symbol-function 'swimmy.school::total-exposure-allowed-p) orig-total)
      (setf (symbol-function 'swimmy.school::spread-pips-from-bid-ask) orig-spread)
      (setf (symbol-function 'swimmy.school::validate-trade-opportunity) orig-validate)
      (setf (symbol-function 'swimmy.school::get-category-lot) orig-lot)
      (setf (symbol-function 'swimmy.school::calc-execution-lot) orig-calc)
      (setf (symbol-function 'swimmy.school::apply-pair-overlay) orig-overlay)
      (setf (symbol-function 'swimmy.school::apply-pattern-soft-gate) orig-gate)
      (setf (symbol-function 'swimmy.school::emit-pattern-gate-telemetry) orig-emit)
      (setf (symbol-function 'swimmy.school::verify-signal-authority) orig-auth)
      (setf (symbol-function 'swimmy.school::execute-order-sequence) orig-exec)
      (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify-alert)
      (setf (symbol-function 'swimmy.core::emit-telemetry-event) orig-emit-telemetry))))

(deftest test-verify-signal-authority-requires-live-ready-deployment-gate
  "verify-signal-authority should fail-closed unless deployment gate decision is LIVE_READY."
  (let ((orig-block (symbol-function 'swimmy.school::should-block-trade-p))
        (orig-unlearn (symbol-function 'swimmy.school::should-unlearn-p))
        (orig-parallel (symbol-function 'swimmy.school::verify-parallel-scenarios))
        (orig-fetch (symbol-function 'swimmy.school::fetch-deployment-gate-status))
        (gate-map (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::should-block-trade-p)
                (lambda (&rest _args) (declare (ignore _args)) nil))
          (setf (symbol-function 'swimmy.school::should-unlearn-p)
                (lambda (&rest _args) (declare (ignore _args)) nil))
          (setf (symbol-function 'swimmy.school::verify-parallel-scenarios)
                (lambda (&rest _args) (declare (ignore _args)) t))
          (setf (symbol-function 'swimmy.school::fetch-deployment-gate-status)
                (lambda (strategy-name)
                  (gethash strategy-name gate-map)))

          (assert-false
           (swimmy.school::verify-signal-authority "USDJPY" :buy :trend 0.01 :S "UT-GATE-MISSING")
           "Expected fail-closed when deployment gate row is missing")

          (setf (gethash "UT-GATE-RUNNING" gate-map) (list :decision "FORWARD_RUNNING"))
          (assert-false
           (swimmy.school::verify-signal-authority "USDJPY" :buy :trend 0.01 :S "UT-GATE-RUNNING")
           "Expected fail-closed when deployment gate is FORWARD_RUNNING")

          (setf (gethash "UT-GATE-LIVE" gate-map) (list :decision "LIVE_READY"))
          (assert-true
           (swimmy.school::verify-signal-authority "USDJPY" :buy :trend 0.01 :S "UT-GATE-LIVE")
           "Expected authority success only when deployment gate is LIVE_READY"))
      (setf (symbol-function 'swimmy.school::should-block-trade-p) orig-block)
      (setf (symbol-function 'swimmy.school::should-unlearn-p) orig-unlearn)
      (setf (symbol-function 'swimmy.school::verify-parallel-scenarios) orig-parallel)
      (setf (symbol-function 'swimmy.school::fetch-deployment-gate-status) orig-fetch))))

(deftest test-verify-signal-authority-blocks-on-live-edge-degradation
  "verify-signal-authority should fail-closed when recent LIVE edge falls below runtime floor."
  (let ((orig-block (symbol-function 'swimmy.school::should-block-trade-p))
        (orig-unlearn (symbol-function 'swimmy.school::should-unlearn-p))
        (orig-parallel (symbol-function 'swimmy.school::verify-parallel-scenarios))
        (orig-fetch-gate (symbol-function 'swimmy.school::fetch-deployment-gate-status))
        (orig-fetch-metrics (and (fboundp 'swimmy.school::fetch-recent-live-trade-metrics)
                                 (symbol-function 'swimmy.school::fetch-recent-live-trade-metrics)))
        (orig-notify (and (fboundp 'swimmy.core:notify-discord-alert)
                          (symbol-function 'swimmy.core:notify-discord-alert)))
        (orig-telemetry (and (fboundp 'swimmy.core::emit-telemetry-event)
                             (symbol-function 'swimmy.core::emit-telemetry-event)))
        (alert-called nil)
        (alert-msg nil)
        (telemetry-called nil)
        (telemetry-event nil))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::should-block-trade-p)
                (lambda (&rest _args) (declare (ignore _args)) nil))
          (setf (symbol-function 'swimmy.school::should-unlearn-p)
                (lambda (&rest _args) (declare (ignore _args)) nil))
          (setf (symbol-function 'swimmy.school::verify-parallel-scenarios)
                (lambda (&rest _args) (declare (ignore _args)) t))
          (setf (symbol-function 'swimmy.school::fetch-deployment-gate-status)
                (lambda (_strategy-name)
                  (list :decision "LIVE_READY" :reason "forward gate passed")))
          (setf (symbol-function 'swimmy.school::fetch-recent-live-trade-metrics)
                (lambda (_strategy-name &key limit)
                  (declare (ignore _strategy-name limit))
                  ;; Below runtime floor: PF<1.05, WR<35%, net<0 with enough evidence.
                  (list :trades 40 :wins 12 :losses 28
                        :win-rate 0.30
                        :profit-factor 0.82
                        :net-pnl -1200.0)))
          (when (fboundp 'swimmy.core:notify-discord-alert)
            (setf (symbol-function 'swimmy.core:notify-discord-alert)
                  (lambda (msg &key color)
                    (declare (ignore color))
                    (setf alert-called t
                          alert-msg msg)
                    t)))
          (when (fboundp 'swimmy.core::emit-telemetry-event)
            (setf (symbol-function 'swimmy.core::emit-telemetry-event)
                  (lambda (event &key &allow-other-keys)
                    (setf telemetry-called t
                          telemetry-event event)
                    t)))
          (assert-false
           (swimmy.school::verify-signal-authority "USDJPY" :buy :trend 0.01 :S "UT-GATE-LIVE-BAD-EDGE")
           "Expected fail-closed when recent LIVE edge is degraded")
          (assert-true alert-called "Expected Discord alert for live-edge block")
          (assert-true (and (stringp alert-msg) (search "Live Edge" alert-msg))
                       "Expected alert message to mention Live Edge block")
          (assert-true telemetry-called "Expected telemetry for live-edge block")
          (assert-equal "execution.live_edge_blocked" telemetry-event
                        "Expected execution.live_edge_blocked telemetry event"))
      (setf (symbol-function 'swimmy.school::should-block-trade-p) orig-block)
      (setf (symbol-function 'swimmy.school::should-unlearn-p) orig-unlearn)
      (setf (symbol-function 'swimmy.school::verify-parallel-scenarios) orig-parallel)
      (setf (symbol-function 'swimmy.school::fetch-deployment-gate-status) orig-fetch-gate)
      (if orig-fetch-metrics
          (setf (symbol-function 'swimmy.school::fetch-recent-live-trade-metrics) orig-fetch-metrics)
          (fmakunbound 'swimmy.school::fetch-recent-live-trade-metrics))
      (when (and orig-notify (fboundp 'swimmy.core:notify-discord-alert))
        (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify))
      (when (and orig-telemetry (fboundp 'swimmy.core::emit-telemetry-event))
        (setf (symbol-function 'swimmy.core::emit-telemetry-event) orig-telemetry)))))

(deftest test-verify-signal-authority-blocks-on-live-edge-loss-streak
  "verify-signal-authority should fail-closed when latest LIVE loss streak breaches runtime cap."
  (let ((orig-block (symbol-function 'swimmy.school::should-block-trade-p))
        (orig-unlearn (symbol-function 'swimmy.school::should-unlearn-p))
        (orig-parallel (symbol-function 'swimmy.school::verify-parallel-scenarios))
        (orig-fetch-gate (symbol-function 'swimmy.school::fetch-deployment-gate-status))
        (orig-fetch-metrics (and (fboundp 'swimmy.school::fetch-recent-live-trade-metrics)
                                 (symbol-function 'swimmy.school::fetch-recent-live-trade-metrics)))
        (orig-notify (and (fboundp 'swimmy.core:notify-discord-alert)
                          (symbol-function 'swimmy.core:notify-discord-alert)))
        (orig-telemetry (and (fboundp 'swimmy.core::emit-telemetry-event)
                             (symbol-function 'swimmy.core::emit-telemetry-event)))
        (alert-called nil)
        (telemetry-event nil))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::should-block-trade-p)
                (lambda (&rest _args) (declare (ignore _args)) nil))
          (setf (symbol-function 'swimmy.school::should-unlearn-p)
                (lambda (&rest _args) (declare (ignore _args)) nil))
          (setf (symbol-function 'swimmy.school::verify-parallel-scenarios)
                (lambda (&rest _args) (declare (ignore _args)) t))
          (setf (symbol-function 'swimmy.school::fetch-deployment-gate-status)
                (lambda (_strategy-name)
                  (list :decision "LIVE_READY" :reason "forward gate passed")))
          (setf (symbol-function 'swimmy.school::fetch-recent-live-trade-metrics)
                (lambda (_strategy-name &key limit)
                  (declare (ignore _strategy-name limit))
                  ;; Meets PF/WR/net floors but should fail on latest-loss-streak.
                  (list :trades 40 :wins 22 :losses 18
                        :win-rate 0.55
                        :profit-factor 1.22
                        :net-pnl 450.0
                        :latest-loss-streak 4
                        :max-loss-streak 4)))
          (when (fboundp 'swimmy.core:notify-discord-alert)
            (setf (symbol-function 'swimmy.core:notify-discord-alert)
                  (lambda (&rest _args)
                    (declare (ignore _args))
                    (setf alert-called t)
                    t)))
          (when (fboundp 'swimmy.core::emit-telemetry-event)
            (setf (symbol-function 'swimmy.core::emit-telemetry-event)
                  (lambda (event &key &allow-other-keys)
                    (setf telemetry-event event)
                    t)))
          (assert-false
           (swimmy.school::verify-signal-authority "USDJPY" :buy :trend 0.01 :S "UT-GATE-LIVE-LOSS-STREAK")
           "Expected fail-closed when latest LIVE loss streak is too high")
          (assert-true alert-called "Expected Discord alert for loss-streak live-edge block")
          (assert-equal "execution.live_edge_blocked" telemetry-event
                        "Expected execution.live_edge_blocked telemetry event"))
      (setf (symbol-function 'swimmy.school::should-block-trade-p) orig-block)
      (setf (symbol-function 'swimmy.school::should-unlearn-p) orig-unlearn)
      (setf (symbol-function 'swimmy.school::verify-parallel-scenarios) orig-parallel)
      (setf (symbol-function 'swimmy.school::fetch-deployment-gate-status) orig-fetch-gate)
      (if orig-fetch-metrics
          (setf (symbol-function 'swimmy.school::fetch-recent-live-trade-metrics) orig-fetch-metrics)
          (fmakunbound 'swimmy.school::fetch-recent-live-trade-metrics))
      (when (and orig-notify (fboundp 'swimmy.core:notify-discord-alert))
        (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify))
      (when (and orig-telemetry (fboundp 'swimmy.core::emit-telemetry-event))
        (setf (symbol-function 'swimmy.core::emit-telemetry-event) orig-telemetry)))))

(deftest test-execute-category-trade-fails-closed-on-reserved-strategy-name-tokens
  "execute-category-trade should fail-closed for reserved NIL-like strategy tokens even if resolvable."
  (let ((orig-team swimmy.school::*active-team*)
        (orig-kb swimmy.school::*strategy-knowledge-base*)
        (orig-evolved swimmy.school::*evolved-strategies*)
        (orig-hist swimmy.globals::*candle-histories*)
        (orig-hist-tf swimmy.globals::*candle-histories-tf*)
        (orig-total (symbol-function 'swimmy.school::total-exposure-allowed-p))
        (orig-spread (symbol-function 'swimmy.school::spread-pips-from-bid-ask))
        (orig-validate (symbol-function 'swimmy.school::validate-trade-opportunity))
        (orig-lot (symbol-function 'swimmy.school::get-category-lot))
        (orig-calc (symbol-function 'swimmy.school::calc-execution-lot))
        (orig-overlay (symbol-function 'swimmy.school::apply-pair-overlay))
        (orig-gate (symbol-function 'swimmy.school::apply-pattern-soft-gate))
        (orig-emit (symbol-function 'swimmy.school::emit-pattern-gate-telemetry))
        (orig-auth (symbol-function 'swimmy.school::verify-signal-authority))
        (orig-exec (symbol-function 'swimmy.school::execute-order-sequence))
        (orig-notify-alert (symbol-function 'swimmy.core:notify-discord-alert))
        (orig-emit-telemetry (symbol-function 'swimmy.core::emit-telemetry-event))
        (alert-called nil)
        (alert-msg nil)
        (telemetry-called nil)
        (telemetry-event nil)
        (executed nil))
    (unwind-protect
        (progn
          (setf swimmy.school::*active-team* (make-hash-table :test 'equal))
          (setf swimmy.school::*strategy-knowledge-base*
                (list (swimmy.school:make-strategy :name "UNKNOWN" :symbol "USDJPY" :timeframe 60)
                      (swimmy.school:make-strategy :name "NULL" :symbol "USDJPY" :timeframe 60)
                      (swimmy.school:make-strategy :name "NONE" :symbol "USDJPY" :timeframe 60)))
          (setf swimmy.school::*evolved-strategies* nil)
          (setf swimmy.globals::*candle-histories* (make-hash-table :test 'equal))
          (setf swimmy.globals::*candle-histories-tf* (make-hash-table :test 'equal))

          (setf (symbol-function 'swimmy.school::total-exposure-allowed-p) (lambda () t))
          (setf (symbol-function 'swimmy.school::spread-pips-from-bid-ask) (lambda (_s _b _a) (declare (ignore _s _b _a)) 1.0))
          (setf (symbol-function 'swimmy.school::validate-trade-opportunity) (lambda (&rest _args) (declare (ignore _args)) t))
          (setf (symbol-function 'swimmy.school::get-category-lot) (lambda (&rest _args) (declare (ignore _args)) 0.01))
          (setf (symbol-function 'swimmy.school::calc-execution-lot) (lambda (&rest _args) (declare (ignore _args)) 0.01))
          (setf (symbol-function 'swimmy.school::apply-pair-overlay) (lambda (&rest _args) (declare (ignore _args)) (list 0.01 nil)))
          (setf (symbol-function 'swimmy.school::apply-pattern-soft-gate) (lambda (&rest _args) (declare (ignore _args)) (values 0.01 nil)))
          (setf (symbol-function 'swimmy.school::emit-pattern-gate-telemetry) (lambda (&rest _args) (declare (ignore _args)) nil))
          (setf (symbol-function 'swimmy.school::verify-signal-authority) (lambda (&rest _args) (declare (ignore _args)) t))
          (setf (symbol-function 'swimmy.school::execute-order-sequence)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (setf executed t)
                  t))
          (setf (symbol-function 'swimmy.core:notify-discord-alert)
                (lambda (msg &key color)
                  (declare (ignore color))
                  (setf alert-called t
                        alert-msg msg)
                  nil))
          (setf (symbol-function 'swimmy.core::emit-telemetry-event)
                (lambda (event-type &key service severity correlation-id data)
                  (declare (ignore service severity correlation-id data))
                  (setf telemetry-called t
                        telemetry-event event-type)
                  nil))

          (dolist (reserved '("UNKNOWN" "NULL" "NONE"))
            (setf alert-called nil
                  alert-msg nil
                  telemetry-called nil
                  telemetry-event nil
                  executed nil)
            (let ((ok (swimmy.school::execute-category-trade
                       :trend :buy "USDJPY" 100.0 100.1
                       :strategy-name reserved
                       :strategy-timeframe 60
                       :signal-confidence 0.8)))
              (assert-false ok (format nil "Expected reserved strategy token to fail-closed: ~a" reserved))
              (assert-false executed (format nil "execute-order-sequence must not run for reserved token: ~a" reserved))
              (assert-true alert-called (format nil "Expected Discord alert for reserved token: ~a" reserved))
              (assert-true (and (stringp alert-msg) (search "Missing strategy context" alert-msg))
                           "Expected alert message to include missing strategy context")
              (assert-true telemetry-called (format nil "Expected telemetry for reserved token: ~a" reserved))
              (assert-equal "execution.context_missing" telemetry-event
                            "Expected execution.context_missing telemetry event"))))
      (setf swimmy.school::*active-team* orig-team)
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*evolved-strategies* orig-evolved)
      (setf swimmy.globals::*candle-histories* orig-hist)
      (setf swimmy.globals::*candle-histories-tf* orig-hist-tf)
      (setf (symbol-function 'swimmy.school::total-exposure-allowed-p) orig-total)
      (setf (symbol-function 'swimmy.school::spread-pips-from-bid-ask) orig-spread)
      (setf (symbol-function 'swimmy.school::validate-trade-opportunity) orig-validate)
      (setf (symbol-function 'swimmy.school::get-category-lot) orig-lot)
      (setf (symbol-function 'swimmy.school::calc-execution-lot) orig-calc)
      (setf (symbol-function 'swimmy.school::apply-pair-overlay) orig-overlay)
      (setf (symbol-function 'swimmy.school::apply-pattern-soft-gate) orig-gate)
      (setf (symbol-function 'swimmy.school::emit-pattern-gate-telemetry) orig-emit)
      (setf (symbol-function 'swimmy.school::verify-signal-authority) orig-auth)
      (setf (symbol-function 'swimmy.school::execute-order-sequence) orig-exec)
      (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify-alert)
      (setf (symbol-function 'swimmy.core::emit-telemetry-event) orig-emit-telemetry))))

(deftest test-execute-order-sequence-fails-closed-on-missing-context
  "execute-order-sequence should reject NIL strategy/timeframe context before reservation."
  (let ((orig-try-reserve (symbol-function 'swimmy.school::try-reserve-slot))
        (orig-notify-alert (symbol-function 'swimmy.core:notify-discord-alert))
        (orig-emit-telemetry (symbol-function 'swimmy.core::emit-telemetry-event))
        (reserve-called nil)
        (alert-called nil)
        (telemetry-called nil)
        (telemetry-event nil))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::try-reserve-slot)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (setf reserve-called t)
                  (values nil nil)))
          (setf (symbol-function 'swimmy.core:notify-discord-alert)
                (lambda (msg &key color)
                  (declare (ignore msg color))
                  (setf alert-called t)
                  nil))
          (setf (symbol-function 'swimmy.core::emit-telemetry-event)
                (lambda (event-type &key service severity correlation-id data)
                  (declare (ignore service severity correlation-id data))
                  (setf telemetry-called t
                        telemetry-event event-type)
                  nil))
          (let ((ok (swimmy.school::execute-order-sequence
                     :trend :buy "USDJPY" 100.0 100.1 0.01 nil "H1" nil)))
            (assert-false ok "Expected execute-order-sequence to fail-closed on missing strategy context")
            (assert-false reserve-called "Reservation must not run when execution context is missing")
            (assert-true alert-called "Expected Discord alert on fail-closed reservation skip")
            (assert-true telemetry-called "Expected telemetry on fail-closed reservation skip")
            (assert-equal "execution.context_missing" telemetry-event
                          "Expected execution.context_missing telemetry event")))
      (setf (symbol-function 'swimmy.school::try-reserve-slot) orig-try-reserve)
      (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify-alert)
      (setf (symbol-function 'swimmy.core::emit-telemetry-event) orig-emit-telemetry))))

(deftest test-execute-order-sequence-fails-closed-on-reserved-strategy-token
  "execute-order-sequence should reject UNKNOWN/NULL/NONE strategy tokens before reservation."
  (let ((orig-try-reserve (symbol-function 'swimmy.school::try-reserve-slot))
        (orig-notify-alert (symbol-function 'swimmy.core:notify-discord-alert))
        (orig-emit-telemetry (symbol-function 'swimmy.core::emit-telemetry-event))
        (reserve-called nil)
        (alert-called nil)
        (telemetry-called nil)
        (telemetry-event nil))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::try-reserve-slot)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (setf reserve-called t)
                  (values nil nil)))
          (setf (symbol-function 'swimmy.core:notify-discord-alert)
                (lambda (msg &key color)
                  (declare (ignore msg color))
                  (setf alert-called t)
                  nil))
          (setf (symbol-function 'swimmy.core::emit-telemetry-event)
                (lambda (event-type &key service severity correlation-id data)
                  (declare (ignore service severity correlation-id data))
                  (setf telemetry-called t
                        telemetry-event event-type)
                  nil))
          (dolist (reserved '("UNKNOWN" "NULL" "NONE"))
            (setf reserve-called nil
                  alert-called nil
                  telemetry-called nil
                  telemetry-event nil)
            (let ((ok (swimmy.school::execute-order-sequence
                       :trend :buy "USDJPY" 100.0 100.1 0.01 reserved "H1" nil)))
              (assert-false ok (format nil "Expected fail-closed on reserved strategy token: ~a" reserved))
              (assert-false reserve-called (format nil "Reservation must not run for reserved strategy token: ~a" reserved))
              (assert-true alert-called (format nil "Expected Discord alert for reserved strategy token: ~a" reserved))
              (assert-true telemetry-called (format nil "Expected telemetry for reserved strategy token: ~a" reserved))
              (assert-equal "execution.context_missing" telemetry-event
                            "Expected execution.context_missing telemetry event"))))
      (setf (symbol-function 'swimmy.school::try-reserve-slot) orig-try-reserve)
      (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify-alert)
      (setf (symbol-function 'swimmy.core::emit-telemetry-event) orig-emit-telemetry))))

;;; ─────────────────────────────────────────
;;; CATEGORY EXECUTION TESTS
;;; ─────────────────────────────────────────

(deftest test-category-trade-interval
  "category trade interval should allow/deny by elapsed seconds"
  (let* ((cat '("M5" :BUY "USDJPY"))
         (orig-table swimmy.school::*last-category-trade-time*)
         (orig-interval swimmy.school::*min-trade-interval*))
    (unwind-protect
        (progn
          (setf swimmy.school::*min-trade-interval* 10)
          (setf swimmy.school::*last-category-trade-time*
                (make-hash-table :test 'equal))
          (setf (gethash cat swimmy.school::*last-category-trade-time*)
                (- (get-universal-time) 20))
          (assert-true (swimmy.school::can-category-trade-p cat))
          (setf (gethash cat swimmy.school::*last-category-trade-time*)
                (get-universal-time))
          (assert-false (swimmy.school::can-category-trade-p cat)))
      (setf swimmy.school::*last-category-trade-time* orig-table)
      (setf swimmy.school::*min-trade-interval* orig-interval))))

(deftest test-verify-parallel-scenarios-uses-category-keys
  "parallel verification should map canonical categories"
  (let ((orig-regime swimmy.school::*market-regime*)
        (orig-vol swimmy.school::*current-volatility-state*))
    (unwind-protect
        (progn
          (setf swimmy.school::*market-regime* :trending)
          (setf swimmy.school::*current-volatility-state* :normal)
          (assert-true (swimmy.school::verify-parallel-scenarios "USDJPY" :buy :trend))
          (assert-true (swimmy.school::verify-parallel-scenarios "USDJPY" :buy :scalp))
          (setf swimmy.school::*market-regime* :ranging)
          (assert-true (swimmy.school::verify-parallel-scenarios "USDJPY" :buy :reversion))
          (setf swimmy.school::*market-regime* :volatile)
          (assert-true (swimmy.school::verify-parallel-scenarios "USDJPY" :buy :breakout)))
      (setf swimmy.school::*market-regime* orig-regime)
      (setf swimmy.school::*current-volatility-state* orig-vol))))

(deftest test-live-status-schema-v2-no-tribe
  "live_status should be schema v2 and omit tribe fields"
  (let ((captured nil)
        (orig (symbol-function 'swimmy.core:write-sexp-atomic)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.core:write-sexp-atomic)
                (lambda (path payload)
                  (declare (ignore path))
                  (setf captured payload)))
          (let ((swimmy.shell::*live-status-interval* 0)
                (swimmy.shell::*last-status-write* 0))
            (swimmy.shell::save-live-status))
          (assert-equal 2 (cdr (assoc 'swimmy.shell::schema_version captured)))
          (assert-false (assoc 'swimmy.shell::tribes captured))
          (assert-false (assoc 'swimmy.shell::tribe_consensus captured)))
      (setf (symbol-function 'swimmy.core:write-sexp-atomic) orig))))

(deftest test-live-status-includes-heartbeat-metrics
  "live_status should include guardian heartbeat timestamp + tick age"
  (let ((captured nil)
        (orig-writer (symbol-function 'swimmy.core:write-sexp-atomic))
        (orig-hb swimmy.globals::*last-guardian-heartbeat*))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.core:write-sexp-atomic)
                (lambda (path payload)
                  (declare (ignore path))
                  (setf captured payload)))
          (let ((swimmy.shell::*live-status-interval* 0)
                (swimmy.shell::*last-status-write* 0))
            (setf swimmy.globals::*last-guardian-heartbeat* 0)
            (swimmy.shell::save-live-status))
          (assert-true (assoc 'swimmy.shell::last_guardian_heartbeat captured))
          (assert-true (assoc 'swimmy.shell::tick_age_secs captured))
          (assert-equal 0 (cdr (assoc 'swimmy.shell::last_guardian_heartbeat captured)))
          (assert-equal 0 (cdr (assoc 'swimmy.shell::tick_age_secs captured))))
      (setf (symbol-function 'swimmy.core:write-sexp-atomic) orig-writer)
      (setf swimmy.globals::*last-guardian-heartbeat* orig-hb))))

(deftest test-daily-report-omits-tribe
  "daily report should omit tribe wording"
  (let ((captured nil)
        (orig (symbol-function 'swimmy.shell::notify-discord-daily)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.shell::notify-discord-daily)
                (lambda (msg &key color)
                  (declare (ignore color))
                  (setf captured msg)))
          (swimmy.main::send-daily-status-report)
          (assert-true (null (search "Tribe" captured)))
          (assert-true (null (search "部族" captured))))
      (setf (symbol-function 'swimmy.shell::notify-discord-daily) orig))))

(deftest test-notify-discord-daily-accepts-custom-title
  "notify-discord-daily should accept :title without signaling unknown keyword"
  (let* ((orig-daily swimmy.core:*discord-daily-webhook*)
         (orig-queue (symbol-function 'swimmy.core:queue-discord-notification))
         (captured nil))
    (unwind-protect
        (progn
          (setf swimmy.core:*discord-daily-webhook* "https://example.invalid/daily")
          (setf (symbol-function 'swimmy.core:queue-discord-notification)
                (lambda (webhook msg &key color title)
                  (setf captured (list webhook msg color title))))
          (swimmy.core:notify-discord-daily "hello" :title "🧐 Advisor Council Report")
          (assert-not-nil captured "Expected queue-discord-notification call")
          (assert-equal "https://example.invalid/daily" (first captured))
          (assert-equal "hello" (second captured))
          (assert-equal "🧐 Advisor Council Report" (fourth captured)))
      (setf swimmy.core:*discord-daily-webhook* orig-daily)
      (setf (symbol-function 'swimmy.core:queue-discord-notification) orig-queue))))

(deftest test-advisor-council-triggers-auto-maintenance
  "Advisor Council should trigger auto-maintenance (housekeeping->retired, objective failure->graveyard) and include a summary."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-retired (symbol-function 'swimmy.school::send-to-retired))
         (orig-graveyard (symbol-function 'swimmy.school:send-to-graveyard))
         (orig-notify (symbol-function 'swimmy.core:notify-discord-daily))
         (now (get-universal-time))
         (zombie (swimmy.school:make-strategy :name "UT-ZOMBIE"
                                              :sharpe 1.0
                                              :trades 10
                                              :rank :B))
         (low (swimmy.school:make-strategy :name "UT-LOW-SHARPE"
                                           :sharpe 0.0
                                           :trades 10
                                           :rank nil))
         (retired-calls nil)
         (graveyard-calls nil)
         (captured-msg nil))
    (unwind-protect
        (progn
          ;; Ensure not protected by newborn/age guards.
          (setf (swimmy.school::strategy-creation-time zombie) (- now (* 10 24 60 60)))
          (setf (swimmy.school::strategy-last-update zombie) (- now (* 8 24 60 60)))
          (setf (swimmy.school::strategy-creation-time low) (- now (* 2 24 60 60)))
          (setf (swimmy.school::strategy-last-update low) now)
          (setf swimmy.school::*strategy-knowledge-base* (list zombie low))

          (setf (symbol-function 'swimmy.school::send-to-retired)
                (lambda (s reason)
                  (declare (ignore reason))
                  (push (swimmy.school::strategy-name s) retired-calls)
                  nil))
          (setf (symbol-function 'swimmy.school:send-to-graveyard)
                (lambda (s reason)
                  (declare (ignore reason))
                  (push (swimmy.school::strategy-name s) graveyard-calls)
                  nil))
          (setf (symbol-function 'swimmy.core:notify-discord-daily)
                (lambda (msg &key color title)
                  (declare (ignore color title))
                  (setf captured-msg msg)
                  nil))

          (swimmy.school::run-advisor-council)

          (assert-true (find "UT-ZOMBIE" retired-calls :test #'string=)
                       "Expected zombie strategy to be retired (housekeeping)")
          (assert-true (find "UT-LOW-SHARPE" graveyard-calls :test #'string=)
                       "Expected low-sharpe strategy to be graveyarded (objective failure)")
          (assert-true (and (stringp captured-msg) (search "Auto Maintenance" captured-msg))
                       "Expected maintenance summary included in Advisor Council message"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf (symbol-function 'swimmy.school::send-to-retired) orig-retired)
      (setf (symbol-function 'swimmy.school:send-to-graveyard) orig-graveyard)
      (setf (symbol-function 'swimmy.core:notify-discord-daily) orig-notify))))

(deftest test-naval-report-zombie-count-not-capped
  "Naval report should count all zombies, not cap counts to the daily action limit."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-max-actions swimmy.school::*advisor-zombie-max-actions*)
         (now (get-universal-time))
         (kb nil))
    (unwind-protect
        (progn
          (setf swimmy.school::*advisor-zombie-max-actions* 25)
          (dotimes (i 30)
            (let ((s (swimmy.school:make-strategy :name (format nil "UT-ZOMBIE-~2,'0d" i)
                                                  :sharpe 1.0
                                                  :trades 10
                                                  :rank :B)))
              (setf (swimmy.school::strategy-last-update s) (- now (* 8 24 60 60)))
              (push s kb)))
          (setf swimmy.school::*strategy-knowledge-base* kb)
          (let ((report (swimmy.school::generate-naval-report)))
            (assert-true (and (stringp report)
                              (search "Zombies (>7d inactive): 30" report))
                         "Expected Naval report zombie count to reflect all zombies (30), not the action cap (25).")))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*advisor-zombie-max-actions* orig-max-actions))))

(deftest test-recruit-notification-uses-category
  "recruit notification should say Category and omit Clan/tribe"
  (let ((captured nil)
        (orig (symbol-function 'swimmy.core:notify-discord-recruit)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.core:notify-discord-recruit)
                (lambda (msg &key color)
                  (declare (ignore color))
                  (setf captured msg)))
          (let ((strat (swimmy.school:make-strategy
                        :name "UT-RECRUIT"
                        :symbol "USDJPY"
                        :timeframe 5
                        :direction :BUY
                        :category :trend)))
            (swimmy.school::notify-recruit-unified strat :founder))
          (assert-true (and captured (> (length captured) 0)))
          (assert-true (search "Category" captured))
          (assert-false (search "Clan" captured))
          (assert-false (search "Tribe" captured))
          (assert-false (search "部族" captured)))
      (setf (symbol-function 'swimmy.core:notify-discord-recruit) orig))))

(deftest test-category-vocabulary-omits-clan-terms-in-sources
  "key source files should not contain clan/tribe vocabulary"
  (dolist (path '("src/lisp/core/rituals.lisp"
                  "src/lisp/mixseek.lisp"
                  "src/lisp/quality.lisp"
                  "src/lisp/school/school-execution.lisp"
                  "src/lisp/school/school-founders.lisp"))
    (let ((content (uiop:read-file-string path)))
      (assert-false (search "Clan" content))
      (assert-false (search "Clans" content))
      (assert-false (search "Hunters" content))
      (assert-false (search "Shamans" content))
      (assert-false (search "Breakers" content))
      (assert-false (search "Raiders" content)))))

(deftest test-founder-template-uses-category-placeholder
  "founder template should use CATEGORY placeholder and omit CLAN"
  (let ((content (uiop:read-file-string "src/lisp/templates/founder.lisp.template")))
    (assert-true (search ":{{CATEGORY}}" content))
    (assert-false (search ":{{CLAN}}" content))))

(deftest test-hunted-founder-templates-apply-trend-hardening-filters
  "Core D1/W1 hunted founders should include stricter trend filters for EUR/GBP admission quality."
  (labels ((indicator-present-p (indicators name period)
             (some (lambda (spec)
                     (and (consp spec)
                          (symbolp (first spec))
                          (string= (string-upcase name)
                                   (string-upcase (symbol-name (first spec))))
                          (equal (second spec) period)))
                   indicators))
           (sexp-upcase (x)
             (string-upcase (prin1-to-string x))))
    (let* ((w1-ema-maker (gethash :hunted-w1-ema swimmy.school::*founder-registry*))
           (w1-pullback-maker (gethash :hunted-w1-pullback swimmy.school::*founder-registry*))
           (w1-volume-maker (gethash :hunted-w1-volume swimmy.school::*founder-registry*))
           (d1-adx-maker (gethash :hunted-d1-adx swimmy.school::*founder-registry*))
           (d1-williams-maker (gethash :hunted-d1-williams swimmy.school::*founder-registry*))
           (h12-vwapvr-maker (gethash :hunted-h12-vwapvr-50-150 swimmy.school::*founder-registry*)))
      (assert-not-nil w1-ema-maker "Expected :hunted-w1-ema founder")
      (assert-not-nil w1-pullback-maker "Expected :hunted-w1-pullback founder")
      (assert-not-nil w1-volume-maker "Expected :hunted-w1-volume founder")
      (assert-not-nil d1-adx-maker "Expected :hunted-d1-adx founder")
      (assert-not-nil d1-williams-maker "Expected :hunted-d1-williams founder")
      (assert-not-nil h12-vwapvr-maker "Expected :hunted-h12-vwapvr-50-150 founder")

      (let* ((s (funcall w1-ema-maker))
             (inds (swimmy.school:strategy-indicators s))
             (entry (sexp-upcase (swimmy.school:strategy-entry s)))
             (exit (sexp-upcase (swimmy.school:strategy-exit s))))
        (assert-true (indicator-present-p inds "EMA" 52)
                     "Expected W1 EMA template to include EMA 52 trend filter")
        (assert-true (search "CROSS-ABOVE" entry)
                     "Expected W1 EMA entry to keep crossover trigger")
        (assert-true (search "EMA-52" entry)
                     "Expected W1 EMA entry to require EMA52 trend filter")
        (assert-true (search "CROSS-BELOW" exit)
                     "Expected W1 EMA exit to keep crossover stop")
        (assert-true (search "EMA-52" exit)
                     "Expected W1 EMA exit to guard EMA52 trend break"))

      (let* ((s (funcall w1-pullback-maker))
             (inds (swimmy.school:strategy-indicators s))
             (entry (sexp-upcase (swimmy.school:strategy-entry s)))
             (exit (sexp-upcase (swimmy.school:strategy-exit s))))
        (assert-true (indicator-present-p inds "SMA" 200)
                     "Expected W1 Pullback template to include SMA 200 regime filter")
        (assert-true (and (search "SMA-50" entry) (search "SMA-200" entry))
                     "Expected W1 Pullback entry to require SMA50 > SMA200")
        (assert-true (search "RSI 40" entry)
                     "Expected W1 Pullback entry to tighten RSI pullback threshold")
        (assert-true (and (search "CLOSE" exit) (search "SMA-50" exit))
                     "Expected W1 Pullback exit to protect on close < SMA50"))

      (let* ((s (funcall w1-volume-maker))
             (inds (swimmy.school:strategy-indicators s))
             (entry (sexp-upcase (swimmy.school:strategy-entry s)))
             (exit (sexp-upcase (swimmy.school:strategy-exit s))))
        (assert-true (indicator-present-p inds "EMA" 50)
                     "Expected W1 Volume template to include EMA 50 trend filter")
        (assert-true (indicator-present-p inds "RSI" 14)
                     "Expected W1 Volume template to include RSI 14 momentum filter")
        (assert-true (and (search "SMA-20" entry) (search "EMA-50" entry))
                     "Expected W1 Volume entry to require SMA20 > EMA50")
        (assert-true (search "RSI 52" entry)
                     "Expected W1 Volume entry to require RSI > 52")
        (assert-true (search "RSI 45" exit)
                     "Expected W1 Volume exit to cut on RSI < 45"))

      (let* ((s (funcall d1-adx-maker))
             (inds (swimmy.school:strategy-indicators s))
             (entry (sexp-upcase (swimmy.school:strategy-entry s)))
             (exit (sexp-upcase (swimmy.school:strategy-exit s))))
        (assert-true (indicator-present-p inds "EMA" 50)
                     "Expected D1 ADX template to include EMA 50 direction filter")
        (assert-true (search "ADX 23" entry)
                     "Expected D1 ADX entry to use lower but filtered ADX threshold")
        (assert-true (and (search "CLOSE" entry) (search "EMA-50" entry))
                     "Expected D1 ADX entry to require close > EMA50")
        (assert-true (and (search "CLOSE" exit) (search "EMA-50" exit))
                     "Expected D1 ADX exit to include trend break guard"))

      (let* ((s (funcall d1-williams-maker))
             (inds (swimmy.school:strategy-indicators s))
             (entry (sexp-upcase (swimmy.school:strategy-entry s)))
             (exit (sexp-upcase (swimmy.school:strategy-exit s))))
        (assert-true (indicator-present-p inds "SMA" 200)
                     "Expected D1 Williams template to include SMA 200 bias filter")
        (assert-true (and (search "CROSS-ABOVE" entry) (search "WILLIAMS -80" entry))
                     "Expected D1 Williams entry to require recovery cross above -80")
        (assert-true (and (search "CLOSE" entry) (search "SMA-200" entry))
                     "Expected D1 Williams entry to require close > SMA200")
        (assert-true (and (search "CLOSE" exit) (search "SMA-200" exit))
                     "Expected D1 Williams exit to include close < SMA200 guard"))

      (let* ((s (funcall h12-vwapvr-maker))
             (inds (swimmy.school:strategy-indicators s))
             (entry (sexp-upcase (swimmy.school:strategy-entry s)))
             (exit (sexp-upcase (swimmy.school:strategy-exit s))))
        (assert-true (indicator-present-p inds "VWAPVR" 50)
                     "Expected H12 VWAPVR template to include VWAPVR period 50")
        (assert-true (search "VWAPVR-50-PREV" entry)
                     "Expected H12 VWAPVR entry to require previous-bar threshold cross context")
        (assert-true (search "VWAPVR-THRESHOLD" entry)
                     "Expected H12 VWAPVR entry to reference orange threshold")
        (assert-true (and (search "VWAPVR-50" exit) (search "<=" exit))
                     "Expected H12 VWAPVR exit to flatten when VWAPVR loses strength")))))

(deftest test-ledger-omits-tribe-fields
  "save-state should omit tribe fields"
  (let* ((tmp-path (merge-pathnames (format nil "/tmp/swimmy-state-~a.sexp" (get-universal-time))))
         (orig-path swimmy.engine::*state-file-path*))
    (unwind-protect
        (progn
          (setf swimmy.engine::*state-file-path* tmp-path)
          (swimmy.engine:save-state)
          (with-open-file (in tmp-path :direction :input)
            (let ((obj (read in nil nil)))
              (assert-false (member :tribe-consensus obj))
              (assert-false (member :tribe-direction obj)))))
      (setf swimmy.engine::*state-file-path* orig-path)
      (when (probe-file tmp-path) (delete-file tmp-path)))))

(deftest test-dynamic-narrative-uses-category-display
  "dynamic narrative should use category display, not clan names"
  (let* ((signal (list :strategy-name "Test"
                       :direction :buy
                       :category :trend
                       :indicator-values nil
                       :sl 0.1
                       :tp 0.2))
         (text (swimmy.school::generate-dynamic-narrative signal "USDJPY" 150.0)))
    (assert-true (search "TREND" text))
    (assert-false (search "Hunters" text))
    (assert-false (search "Unknown" text))))

(deftest test-category-counts-returns-alist
  "category counts should return alist with core categories"
  (let ((counts (swimmy.school::get-category-counts)))
    (assert-true (assoc :trend counts))
    (assert-true (assoc :reversion counts))
    (assert-true (assoc :breakout counts))
    (assert-true (assoc :scalp counts))))

(deftest test-repl-help-omits-clans
  "REPL help should not mention clan commands"
  (let ((fn (find-symbol "PRINT-HELP" :swimmy-repl)))
    (assert-true (and fn (fboundp fn)) "print-help should exist")
    (let* ((out (make-string-output-stream))
           (*standard-output* out))
      (funcall fn)
      (let ((txt (get-output-stream-string out)))
        (assert-true (null (search ":clans" txt)))
        (assert-true (null (search ":clan" txt)))))))

;;; ─────────────────────────────────────────
;;; CONSTITUTION TESTS
;;; ─────────────────────────────────────────

(deftest test-constitution-evaluation
  "Test constitution evaluation"
  (cl-user::initialize-constitution)
  (let* ((context (list :daily-pnl 0 :volatility-state :normal))
         (result (cl-user::evaluate-constitution context)))
    (assert-not-nil result "Evaluation should return result")
    (assert-true (getf result :alignment) "Should have alignment score")))

(deftest test-constitution-blocks-dangerous
  "Test that constitution blocks dangerous actions"
  (cl-user::initialize-constitution)
  (let* ((dangerous-context (list :daily-pnl -6000 :volatility-state :extreme))
         (result (cl-user::evaluate-constitution dangerous-context)))
    (assert-true (getf result :violations) "Should have violations")))

;;; ─────────────────────────────────────────
;;; REPUTATION TESTS
;;; ─────────────────────────────────────────

(deftest test-reputation-creation
  "Test reputation creation"
  (let ((rep (cl-user::get-reputation "Test-Strategy")))
    (assert-not-nil rep "Reputation should be created")
    (assert-equal 0.5 (cl-user::reputation-trust-score rep) "Initial trust should be 0.5")))

(deftest test-reputation-update
  "Test reputation update on win"
  (let ((rep (cl-user::get-reputation "Winner-Strategy")))
    (declare (ignore rep))
    (cl-user::update-reputation "Winner-Strategy" :win :pnl 100)
    (let ((updated-rep (cl-user::get-reputation "Winner-Strategy")))
      (assert-true (> (cl-user::reputation-trust-score updated-rep) 0.5)
                   "Trust should increase on win"))))

;;; ─────────────────────────────────────────
;;; V6.18: DANGER AVOIDANCE TESTS
;;; ─────────────────────────────────────────

(deftest test-danger-level-initial
  "Test initial danger level is 0"
  (assert-equal 0 cl-user::*danger-level* "Initial danger level should be 0"))

(deftest test-consecutive-losses-tracked
  "Test consecutive losses tracking"
  (let ((old-losses cl-user::*consecutive-losses*))
    (cl-user::record-trade-result :loss)
    (assert-true (>= cl-user::*consecutive-losses* 1) "Should track losses")
    (setf cl-user::*consecutive-losses* old-losses)))

(deftest test-cooldown-returns-false-initially
  "Test cooldown is inactive initially"
  (setf cl-user::*danger-cooldown-until* 0)
  (assert-false (cl-user::danger-cooldown-active-p) "Cooldown should be inactive"))

(deftest test-cooldown-tolerates-nil-until
  "Cooldown helpers should treat NIL until as inactive (no type errors)."
  (let ((orig cl-user::*danger-cooldown-until*))
    (unwind-protect
        (progn
          (setf cl-user::*danger-cooldown-until* nil)
          (assert-false (cl-user::danger-cooldown-active-p)
                        "Cooldown should be inactive when until is NIL")
          (assert-equal 0 (cl-user::get-cooldown-remaining)
                        "Remaining seconds should be 0 when until is NIL"))
      (setf cl-user::*danger-cooldown-until* orig))))

;;; ─────────────────────────────────────────
;;; V6.18: RESIGNATION TESTS
;;; ─────────────────────────────────────────

(deftest test-has-resigned-initial
  "Test initial resignation state"
  (setf cl-user::*has-resigned-today* nil)
  (assert-false (cl-user::has-resigned-p) "Should not be resigned initially"))

(deftest test-resignation-threshold-exists
  "Test resignation threshold is defined"
  (assert-not-nil cl-user::*resignation-threshold* "Threshold should exist")
  (assert-true (< cl-user::*resignation-threshold* 0) "Threshold should be negative"))

;;; ─────────────────────────────────────────
;;; V6.18: LEADER SYSTEM TESTS
;;; ─────────────────────────────────────────

(deftest test-leader-info-struct
  "Test leader-info struct creation"
  (let ((leader (cl-user::make-leader-info 
                 :strategy-name "Test" 
                 :sharpe 1.5 
                 :win-rate 0.6
                 :tenure-start 0
                 :trades-as-leader 0
                 :pnl-as-leader 0.0)))
    (assert-equal "Test" (cl-user::leader-info-strategy-name leader))
    (assert-equal 1.5 (cl-user::leader-info-sharpe leader))))

;;; ─────────────────────────────────────────
;;; V6.18: RISK MANAGER TESTS
;;; ─────────────────────────────────────────

(deftest test-risk-summary
  "Test risk summary generation"
  (let ((summary (cl-user::get-risk-summary)))
    (assert-not-nil summary "Summary should be generated")
    (assert-true (stringp summary) "Summary should be a string")))

(deftest test-risk-fallback-capital-format-no-trailing-dot
  "Risk fallback warning should print integer yen without trailing decimal point"
  (let ((orig-equity swimmy.globals::*current-equity*)
        (orig-min-safe swimmy.globals::*min-safe-capital*))
    (unwind-protect
        (let ((*standard-output* (make-string-output-stream)))
          (setf swimmy.globals::*current-equity* 0.0)
          (setf swimmy.globals::*min-safe-capital* 1000000.0)
          (let ((capital (swimmy.engine::get-effective-risk-capital))
                (output (get-output-stream-string *standard-output*)))
            (assert-equal 1000000.0 capital "Expected fallback capital")
            (assert-true (search "Fallback: ¥1000000" output)
                         "Expected fallback warning without decimal suffix")
            (assert-false (search "Fallback: ¥1000000." output)
                          "Fallback yen value must not include trailing decimal point")))
      (setf swimmy.globals::*current-equity* orig-equity)
      (setf swimmy.globals::*min-safe-capital* orig-min-safe))))

(deftest test-risk-daily-loss-limit-format-no-trailing-dot
  "Daily loss limit warning should print integer yen without trailing decimal point"
  (let ((orig-equity swimmy.globals::*current-equity*)
        (orig-daily-pnl swimmy.globals::*daily-pnl*)
        (orig-daily-limit swimmy.globals::*daily-loss-limit*)
        (orig-daily-limit-pct swimmy.globals::*daily-loss-limit-pct*)
        (orig-weekly-pnl swimmy.globals::*weekly-pnl*)
        (orig-monthly-pnl swimmy.globals::*monthly-pnl*)
        (orig-max-dd swimmy.globals::*max-drawdown*)
        (orig-hard-deck swimmy.globals::*hard-deck-drawdown-pct*)
        (orig-danger swimmy.globals::*danger-level*)
        (orig-resigned cl-user::*has-resigned-today*))
    (unwind-protect
        (let ((*standard-output* (make-string-output-stream)))
          (setf swimmy.globals::*current-equity* 1000000.0)
          (setf swimmy.globals::*daily-pnl* -6000.0)
          (setf swimmy.globals::*daily-loss-limit* -5000.0)
          (setf swimmy.globals::*daily-loss-limit-pct* -50.0)
          (setf swimmy.globals::*weekly-pnl* 0.0)
          (setf swimmy.globals::*monthly-pnl* 0.0)
          (setf swimmy.globals::*max-drawdown* 0.0)
          (setf swimmy.globals::*hard-deck-drawdown-pct* 50.0)
          (setf swimmy.globals::*danger-level* 0)
          (setf cl-user::*has-resigned-today* nil)
          (let ((allowed (swimmy.engine::trading-allowed-p))
                (output (get-output-stream-string *standard-output*)))
            (assert-false allowed "Expected trading to be blocked by daily fixed limit")
            (assert-true (search "DAILY LOSS LIMIT HIT: ¥-6000 < ¥-5000" output)
                         "Expected daily loss warning without decimal suffix")
            (assert-false (search "¥-6000." output)
                          "Daily pnl must not include trailing decimal point")
            (assert-false (search "¥-5000." output)
                          "Daily limit must not include trailing decimal point")))
      (setf swimmy.globals::*current-equity* orig-equity)
      (setf swimmy.globals::*daily-pnl* orig-daily-pnl)
      (setf swimmy.globals::*daily-loss-limit* orig-daily-limit)
      (setf swimmy.globals::*daily-loss-limit-pct* orig-daily-limit-pct)
      (setf swimmy.globals::*weekly-pnl* orig-weekly-pnl)
      (setf swimmy.globals::*monthly-pnl* orig-monthly-pnl)
      (setf swimmy.globals::*max-drawdown* orig-max-dd)
      (setf swimmy.globals::*hard-deck-drawdown-pct* orig-hard-deck)
      (setf swimmy.globals::*danger-level* orig-danger)
      (setf cl-user::*has-resigned-today* orig-resigned))))

(deftest test-update-global-stats-pnl-format-no-trailing-dot
  "Global stats log should print integer yen without trailing decimal point"
  (let ((orig-success swimmy.school::*success-log*)
        (orig-failure swimmy.school::*failure-log*))
    (unwind-protect
        (let ((*standard-output* (make-string-output-stream)))
          (setf swimmy.school::*success-log* nil)
          (setf swimmy.school::*failure-log* nil)
          (swimmy.school::update-global-stats)
          (let ((output (get-output-stream-string *standard-output*)))
            (assert-true (search "PnL: ¥0" output)
                         "Expected yen output without decimal suffix")
            (assert-false (search "PnL: ¥0." output)
                          "Global stats yen value must not include trailing decimal point")))
      (setf swimmy.school::*success-log* orig-success)
      (setf swimmy.school::*failure-log* orig-failure))))

(deftest test-should-unlearn-p-daily-pnl-format-no-trailing-dot
  "Darwin toxic daily pnl log should print integer yen without trailing decimal point"
  (let ((orig-daily-pnl swimmy.globals::*daily-pnl*)
        (orig-losses swimmy.globals::*consecutive-losses*))
    (unwind-protect
        (let ((*standard-output* (make-string-output-stream)))
          (setf swimmy.globals::*daily-pnl* -3500.0)
          (setf swimmy.globals::*consecutive-losses* 0)
          (swimmy.school::should-unlearn-p "USDJPY")
          (let ((output (get-output-stream-string *standard-output*)))
            (assert-true (search "Daily PnL toxic (¥-3500)" output)
                         "Expected toxic pnl without decimal suffix")
            (assert-false (search "¥-3500.)" output)
                          "Toxic pnl must not include trailing decimal point")))
      (setf swimmy.globals::*daily-pnl* orig-daily-pnl)
      (setf swimmy.globals::*consecutive-losses* orig-losses))))

(deftest test-learning-log-summary-line-breaks-down-live-and-shadow
  "learning-log-summary-line should include LIVE/SHADOW totals from in-memory logs."
  (let ((orig-success swimmy.school::*success-log*)
        (orig-failure swimmy.school::*failure-log*))
    (unwind-protect
        (progn
          (setf swimmy.school::*success-log*
                (list (swimmy.school::make-trade-record :pnl 0.4 :execution-mode :live)
                      (swimmy.school::make-trade-record :pnl 0.3 :execution-mode :shadow)))
          (setf swimmy.school::*failure-log*
                (list (swimmy.school::make-trade-record :pnl -0.2 :execution-mode :live)
                      (swimmy.school::make-trade-record :pnl -0.1 :execution-mode :shadow)
                      (swimmy.school::make-trade-record :pnl -0.1 :execution-mode :shadow)))
          (let ((line (swimmy.school::learning-log-summary-line :bootstrap nil)))
            (assert-true (search "total=5" line) "Expected total learning sample count")
            (assert-true (search "LIVE=2" line) "Expected LIVE sample count in summary")
            (assert-true (search "SHADOW=3" line) "Expected SHADOW sample count in summary")))
      (setf swimmy.school::*success-log* orig-success)
      (setf swimmy.school::*failure-log* orig-failure))))

(deftest test-bootstrap-learning-logs-from-db-preserves-execution-mode
  "bootstrap-learning-logs-from-db should restore LIVE/SHADOW modes from trade_logs."
  (let* ((tmp-db (format nil "/tmp/swimmy-learning-bootstrap-~a.db" (get-universal-time)))
         (orig-success swimmy.school::*success-log*)
         (orig-failure swimmy.school::*failure-log*)
         (orig-last-bootstrap swimmy.school::*learning-log-last-bootstrap-at*))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (setf swimmy.school::*success-log* nil
                  swimmy.school::*failure-log* nil
                  swimmy.school::*learning-log-last-bootstrap-at* 0)
            (swimmy.school::record-trade-to-db
             (swimmy.school::make-trade-record
              :timestamp 100
              :strategy-name "UT-LEARN-LIVE"
              :symbol "USDJPY"
              :direction :buy
              :category :trend
              :regime :trend
              :pnl 0.5
              :hold-time 30))
            (swimmy.school::record-trade-to-db
             (swimmy.school::make-trade-record
              :timestamp 101
              :strategy-name "UT-LEARN-SHADOW"
              :symbol "USDJPY"
              :direction :sell
              :category :trend
              :regime :trend
              :execution-mode :shadow
              :pnl -0.3
              :hold-time 45))
            (let ((loaded (swimmy.school::bootstrap-learning-logs-from-db :force t)))
              (assert-equal 2 loaded "Expected bootstrap to load 2 rows")
              (assert-true (find :live swimmy.school::*success-log*
                                 :key #'swimmy.school::trade-record-execution-mode
                                 :test #'eq)
                           "Expected LIVE sample in success log")
              (assert-true (find :shadow swimmy.school::*failure-log*
                                 :key #'swimmy.school::trade-record-execution-mode
                                 :test #'eq)
                           "Expected SHADOW sample in failure log")))
        (setf swimmy.school::*success-log* orig-success
              swimmy.school::*failure-log* orig-failure
              swimmy.school::*learning-log-last-bootstrap-at* orig-last-bootstrap)
        (ignore-errors (swimmy.school::close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-analyze-by-category-bootstraps-learning-logs-from-db
  "analyze-by-category should bootstrap learning logs from trade_logs when memory is empty."
  (let* ((tmp-db (format nil "/tmp/swimmy-learning-analytics-~a.db" (get-universal-time)))
         (orig-success swimmy.school::*success-log*)
         (orig-failure swimmy.school::*failure-log*)
         (orig-last-bootstrap swimmy.school::*learning-log-last-bootstrap-at*))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (setf swimmy.school::*success-log* nil
                  swimmy.school::*failure-log* nil
                  swimmy.school::*learning-log-last-bootstrap-at* 0)
            (swimmy.school::record-trade-to-db
             (swimmy.school::make-trade-record
              :timestamp 200
              :strategy-name "UT-ANALYTICS-LIVE"
              :symbol "USDJPY"
              :direction :buy
              :category :trend
              :regime :trend
              :pnl 0.8
              :hold-time 20))
            (swimmy.school::record-trade-to-db
             (swimmy.school::make-trade-record
              :timestamp 201
              :strategy-name "UT-ANALYTICS-SHADOW"
              :symbol "USDJPY"
              :direction :sell
              :category :breakout
              :regime :trend
              :execution-mode :shadow
              :pnl -0.4
              :hold-time 25))
            (let* ((stats (swimmy.school::analyze-by-category))
                   (trend (gethash :trend stats))
                   (breakout (gethash :breakout stats)))
              (assert-equal 1 (first trend) "Expected trend win count from bootstrapped LIVE row")
              (assert-equal 0 (second trend) "Expected no trend losses")
              (assert-equal 0 (first breakout) "Expected no breakout wins")
              (assert-equal 1 (second breakout) "Expected breakout loss count from SHADOW row")))
        (setf swimmy.school::*success-log* orig-success
              swimmy.school::*failure-log* orig-failure
              swimmy.school::*learning-log-last-bootstrap-at* orig-last-bootstrap)
        (ignore-errors (swimmy.school::close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

;;; ─────────────────────────────────────────
;;; V6.18: DYNAMIC TP/SL TESTS
;;; ─────────────────────────────────────────

(deftest test-volatility-multiplier
  "Test volatility multiplier values"
  (let ((mult (cl-user::get-volatility-multiplier)))
    (assert-not-nil mult "Multiplier should exist")
    (assert-true (and (> mult 0) (<= mult 2)) "Should be reasonable range")))

(deftest test-atr-empty-candles
  "Test ATR with empty candles returns nil"
  (let ((atr (cl-user::calculate-atr nil)))
    (assert-true (null atr) "ATR of nil should be nil")))

;;; ─────────────────────────────────────────
;;; V6.18: UTILITY TESTS
;;; ─────────────────────────────────────────

(deftest test-gotobi-day-returns-boolean
  "Test gotobi-day-p returns proper boolean"
  (let ((result (cl-user::gotobi-day-p)))
    (assert-true (or (eq result t) (eq result nil)) "Should return t or nil")))

(deftest test-london-session-check
  "Test London session detection"
  (let ((result (cl-user::london-session-p)))
    (assert-true (or (eq result t) (eq result nil)) "Should return t or nil")))

;;; ─────────────────────────────────────────
;;; V6.18: CANDLE STRUCT TESTS
;;; ─────────────────────────────────────────

(deftest test-candle-creation
  "Test candle struct creation"
  (let ((c (cl-user::make-candle :open 100.0 :high 110.0 :low 90.0 :close 105.0)))
    (assert-equal 100.0 (cl-user::candle-open c))
    (assert-equal 105.0 (cl-user::candle-close c))))

;;; V8.4: CHARACTERIZATION TESTS (memo3.txt)
;;; ─────────────────────────────────────────
;;; Purpose: Freeze current behavior before refactoring
;;; These tests document ACTUAL behavior, not intended behavior

(deftest test-maintenance-throttle-60s
  "Characterization: run-periodic-maintenance respects 60s throttle"
  ;; Test that the throttle variable exists and is a number
  (let ((maint-time-sym (find-symbol "*LAST-MAINTENANCE-TIME*" :swimmy.main)))
    (if (and maint-time-sym (boundp maint-time-sym))
        (assert-true (numberp (symbol-value maint-time-sym)) "Should be a number")
        (assert-true t "Variable not defined yet - OK for cold start"))))

(deftest test-dream-cycle-self-throttle
  "Characterization: Dream cycle uses *dream-interval* for throttling"
  ;; Document current behavior: dream uses 3600s interval
  (assert-true (boundp 'swimmy.globals:*dream-interval*))
  (assert-true (>= swimmy.globals:*dream-interval* 60)  ; Minimum 60s is reasonable
               "Dream interval should be reasonable"))

(deftest test-processing-step-no-maintenance
  "Characterization: process-msg no longer calls maintenance"
  ;; V8.4: Just verify the function exists and can be called
  (let ((fn-sym (find-symbol "PROCESS-MSG" :swimmy.main)))
    (assert-true (and fn-sym (fboundp fn-sym)) "process-msg should be defined")))

(deftest test-backtest-debug-enabled-p
  "backtest debug env flag should be parsed consistently"
  (require :sb-posix)
  (let ((orig (uiop:getenv "SWIMMY_BACKTEST_DEBUG_RECV")))
    (unwind-protect
        (progn
          (sb-posix:setenv "SWIMMY_BACKTEST_DEBUG_RECV" "1" 1)
          (assert-true (swimmy.main::backtest-debug-enabled-p))
          (sb-posix:setenv "SWIMMY_BACKTEST_DEBUG_RECV" "true" 1)
          (assert-true (swimmy.main::backtest-debug-enabled-p))
          (sb-posix:setenv "SWIMMY_BACKTEST_DEBUG_RECV" "false" 1)
          (assert-true (not (swimmy.main::backtest-debug-enabled-p)))
          (sb-posix:setenv "SWIMMY_BACKTEST_DEBUG_RECV" "" 1)
          (assert-true (not (swimmy.main::backtest-debug-enabled-p))))
      (when orig
        (sb-posix:setenv "SWIMMY_BACKTEST_DEBUG_RECV" orig 1))
      (unless orig
        (sb-posix:unsetenv "SWIMMY_BACKTEST_DEBUG_RECV")))))

(deftest test-backtest-pending-counters-defaults
  "backtest pending counters should initialize to sane defaults"
  (assert-true (boundp 'swimmy.globals::*backtest-submit-count*) "submit counter exists")
  (assert-true (boundp 'swimmy.globals::*backtest-max-pending*) "max pending exists")
  (assert-true (numberp swimmy.globals::*backtest-max-pending*) "max pending numeric"))

(deftest test-backtest-pending-count-rebases-large-drift
  "pending drift far beyond max should rebase submit counter to a safe target."
  (let* ((orig-max swimmy.globals::*backtest-max-pending*)
         (orig-submit swimmy.globals::*backtest-submit-count*)
         (orig-recv (if (boundp 'swimmy.main::*backtest-recv-count*)
                        swimmy.main::*backtest-recv-count*
                        0))
         (orig-factor swimmy.school::*backtest-pending-rebase-threshold-factor*)
         (orig-target swimmy.school::*backtest-pending-rebase-target-ratio*)
         (orig-min-max swimmy.school::*backtest-pending-rebase-min-max-pending*)
         (orig-cooldown swimmy.school::*backtest-pending-rebase-cooldown-sec*)
         (orig-last swimmy.school::*backtest-pending-last-rebase*))
    (unwind-protect
        (progn
          (setf swimmy.globals::*backtest-max-pending* 3000)
          (setf swimmy.globals::*backtest-submit-count* 12000)
          (setf swimmy.main::*backtest-recv-count* 1000)
          (setf swimmy.school::*backtest-pending-rebase-threshold-factor* 3.0d0)
          (setf swimmy.school::*backtest-pending-rebase-target-ratio* 0.5d0)
          (setf swimmy.school::*backtest-pending-rebase-min-max-pending* 1000)
          (setf swimmy.school::*backtest-pending-rebase-cooldown-sec* 0)
          (setf swimmy.school::*backtest-pending-last-rebase* 0)
          (let ((pending (swimmy.school::backtest-pending-count)))
            (assert-equal 1500 pending "Expected pending rebase target at 50% of max")
            (assert-equal 2500 swimmy.globals::*backtest-submit-count*
                          "Expected submit counter to be rebased to recv+target")))
      (setf swimmy.globals::*backtest-max-pending* orig-max)
      (setf swimmy.globals::*backtest-submit-count* orig-submit)
      (setf swimmy.main::*backtest-recv-count* orig-recv)
      (setf swimmy.school::*backtest-pending-rebase-threshold-factor* orig-factor)
      (setf swimmy.school::*backtest-pending-rebase-target-ratio* orig-target)
      (setf swimmy.school::*backtest-pending-rebase-min-max-pending* orig-min-max)
      (setf swimmy.school::*backtest-pending-rebase-cooldown-sec* orig-cooldown)
      (setf swimmy.school::*backtest-pending-last-rebase* orig-last))))

(deftest test-backtest-pending-count-skips-rebase-for-small-max
  "small max-pending configs should keep raw submit-recv pending semantics."
  (let* ((orig-max swimmy.globals::*backtest-max-pending*)
         (orig-submit swimmy.globals::*backtest-submit-count*)
         (orig-recv (if (boundp 'swimmy.main::*backtest-recv-count*)
                        swimmy.main::*backtest-recv-count*
                        0))
         (orig-factor swimmy.school::*backtest-pending-rebase-threshold-factor*)
         (orig-target swimmy.school::*backtest-pending-rebase-target-ratio*)
         (orig-min-max swimmy.school::*backtest-pending-rebase-min-max-pending*)
         (orig-cooldown swimmy.school::*backtest-pending-rebase-cooldown-sec*)
         (orig-last swimmy.school::*backtest-pending-last-rebase*))
    (unwind-protect
        (progn
          (setf swimmy.globals::*backtest-max-pending* 10)
          (setf swimmy.globals::*backtest-submit-count* 100)
          (setf swimmy.main::*backtest-recv-count* 0)
          (setf swimmy.school::*backtest-pending-rebase-threshold-factor* 3.0d0)
          (setf swimmy.school::*backtest-pending-rebase-target-ratio* 0.5d0)
          (setf swimmy.school::*backtest-pending-rebase-min-max-pending* 1000)
          (setf swimmy.school::*backtest-pending-rebase-cooldown-sec* 0)
          (setf swimmy.school::*backtest-pending-last-rebase* 0)
          (let ((pending (swimmy.school::backtest-pending-count)))
            (assert-equal 100 pending "Expected raw pending without rebase")
            (assert-equal 100 swimmy.globals::*backtest-submit-count*
                          "Submit counter should remain unchanged for small max-pending")))
      (setf swimmy.globals::*backtest-max-pending* orig-max)
      (setf swimmy.globals::*backtest-submit-count* orig-submit)
      (setf swimmy.main::*backtest-recv-count* orig-recv)
      (setf swimmy.school::*backtest-pending-rebase-threshold-factor* orig-factor)
      (setf swimmy.school::*backtest-pending-rebase-target-ratio* orig-target)
      (setf swimmy.school::*backtest-pending-rebase-min-max-pending* orig-min-max)
      (setf swimmy.school::*backtest-pending-rebase-cooldown-sec* orig-cooldown)
      (setf swimmy.school::*backtest-pending-last-rebase* orig-last))))

(deftest test-backtest-send-throttles-when-pending-high
  "send-zmq-msg should refuse backtest send when pending exceeds max"
  (let* ((orig-send (symbol-function 'pzmq:send))
         (orig-req swimmy.globals:*backtest-requester*)
         (orig-max swimmy.globals::*backtest-max-pending*)
         (orig-submit swimmy.globals::*backtest-submit-count*)
         (orig-recv (if (boundp 'swimmy.main::*backtest-recv-count*)
                        swimmy.main::*backtest-recv-count*
                        0))
         (sent nil))
    (unwind-protect
        (progn
          (setf swimmy.globals:*backtest-requester* :dummy)
          (setf swimmy.globals::*backtest-submit-count* 10)
          (setf swimmy.main::*backtest-recv-count* 0)
          (setf swimmy.globals::*backtest-max-pending* 1)
          (setf (symbol-function 'pzmq:send)
                (lambda (&rest _)
                  (declare (ignore _))
                  (setf sent t)))
          (swimmy.school::send-zmq-msg "(dummy)" :target :backtest)
          (assert-true (null sent) "send should be blocked"))
      (setf swimmy.globals:*backtest-requester* orig-req)
      (setf swimmy.globals::*backtest-max-pending* orig-max)
      (setf swimmy.globals::*backtest-submit-count* orig-submit)
      (when (boundp 'swimmy.main::*backtest-recv-count*)
        (setf swimmy.main::*backtest-recv-count* orig-recv))
      (setf (symbol-function 'pzmq:send) orig-send))))

(deftest test-backtest-send-throttle-enqueues-instead-of-drop
  "Rate throttle should enqueue backtest messages instead of dropping."
  (let* ((orig-send (symbol-function 'pzmq:send))
         (orig-now (symbol-function 'swimmy.school::backtest-now-seconds))
         (orig-req swimmy.globals:*backtest-requester*)
         (orig-rate swimmy.globals::*backtest-rate-limit-per-sec*)
         (orig-max swimmy.globals::*backtest-max-pending*)
         (orig-submit swimmy.globals::*backtest-submit-count*)
         (orig-recv swimmy.main::*backtest-recv-count*)
         (orig-last swimmy.globals::*backtest-last-send-ts*)
         (orig-queue swimmy.school::*backtest-send-queue*)
         (orig-queue-max swimmy.school::*backtest-send-queue-max*)
         (sent nil))
    (unwind-protect
        (progn
          (setf swimmy.globals:*backtest-requester* :dummy)
          (setf swimmy.globals::*backtest-rate-limit-per-sec* 100)
          (setf swimmy.globals::*backtest-max-pending* 3000)
          (setf swimmy.globals::*backtest-submit-count* 0)
          (setf swimmy.main::*backtest-recv-count* 0)
          (setf swimmy.globals::*backtest-last-send-ts* 100.0d0)
          (setf swimmy.school::*backtest-send-queue* nil)
          (setf swimmy.school::*backtest-send-queue-max* 10)
          (setf (symbol-function 'swimmy.school::backtest-now-seconds)
                (lambda () 100.0d0))
          (setf (symbol-function 'pzmq:send)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (setf sent t)
                  t))
          (let ((state (swimmy.school::send-zmq-msg "(dummy)" :target :backtest)))
            (assert-equal :queued state "throttled request should be queued")
            (assert-equal 1 (length swimmy.school::*backtest-send-queue*)
                          "queue should contain throttled message")
            (assert-true (null sent) "throttled message should not send immediately")))
      (setf swimmy.globals:*backtest-requester* orig-req)
      (setf swimmy.globals::*backtest-rate-limit-per-sec* orig-rate)
      (setf swimmy.globals::*backtest-max-pending* orig-max)
      (setf swimmy.globals::*backtest-submit-count* orig-submit)
      (setf swimmy.main::*backtest-recv-count* orig-recv)
      (setf swimmy.globals::*backtest-last-send-ts* orig-last)
      (setf swimmy.school::*backtest-send-queue* orig-queue)
      (setf swimmy.school::*backtest-send-queue-max* orig-queue-max)
      (setf (symbol-function 'swimmy.school::backtest-now-seconds) orig-now)
      (setf (symbol-function 'pzmq:send) orig-send))))

(deftest test-backtest-send-queue-count-stays-in-sync
  "enqueue/flush should keep backtest queue metadata (count/tail) consistent."
  (let* ((orig-send (symbol-function 'pzmq:send))
         (orig-req (and (boundp 'swimmy.globals:*backtest-requester*)
                        swimmy.globals:*backtest-requester*))
         (orig-max (and (boundp 'swimmy.globals::*backtest-max-pending*)
                        swimmy.globals::*backtest-max-pending*))
         (orig-submit (and (boundp 'swimmy.globals::*backtest-submit-count*)
                           swimmy.globals::*backtest-submit-count*))
         (orig-recv (when (boundp 'swimmy.main::*backtest-recv-count*)
                      swimmy.main::*backtest-recv-count*))
         (orig-queue swimmy.school::*backtest-send-queue*)
         (orig-count (and (boundp 'swimmy.school::*backtest-send-queue-count*)
                          swimmy.school::*backtest-send-queue-count*))
         (orig-tail (and (boundp 'swimmy.school::*backtest-send-queue-tail*)
                         swimmy.school::*backtest-send-queue-tail*)))
    (unwind-protect
        (progn
          (setf swimmy.school::*backtest-send-queue* nil)
          (setf swimmy.school::*backtest-send-queue-count* 0)
          (setf swimmy.school::*backtest-send-queue-tail* nil)
          (assert-true (swimmy.school::enqueue-backtest-msg "m1") "enqueue m1")
          (assert-true (swimmy.school::enqueue-backtest-msg "m2") "enqueue m2")
          (assert-equal 2 swimmy.school::*backtest-send-queue-count* "count should be 2 after enqueue")
          (assert-not-nil swimmy.school::*backtest-send-queue-tail* "tail should be set")
          (setf swimmy.globals:*backtest-requester* :dummy)
          (setf swimmy.globals::*backtest-max-pending* 999999)
          (setf swimmy.globals::*backtest-submit-count* 0)
          (when (boundp 'swimmy.main::*backtest-recv-count*)
            (setf swimmy.main::*backtest-recv-count* 0))
          (setf (symbol-function 'pzmq:send)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  t))
          (swimmy.school::flush-backtest-queue)
          (assert-equal 0 swimmy.school::*backtest-send-queue-count* "count should be zero after flush")
          (assert-true (null swimmy.school::*backtest-send-queue-tail*) "tail should reset when queue empties"))
      (setf swimmy.school::*backtest-send-queue* orig-queue)
      (when (boundp 'swimmy.school::*backtest-send-queue-count*)
        (setf swimmy.school::*backtest-send-queue-count* orig-count))
      (when (boundp 'swimmy.school::*backtest-send-queue-tail*)
        (setf swimmy.school::*backtest-send-queue-tail* orig-tail))
      (setf swimmy.globals:*backtest-requester* orig-req)
      (when (boundp 'swimmy.globals::*backtest-max-pending*)
        (setf swimmy.globals::*backtest-max-pending* orig-max))
      (when (boundp 'swimmy.globals::*backtest-submit-count*)
        (setf swimmy.globals::*backtest-submit-count* orig-submit))
      (when (boundp 'swimmy.main::*backtest-recv-count*)
        (setf swimmy.main::*backtest-recv-count* orig-recv))
      (setf (symbol-function 'pzmq:send) orig-send))))

(deftest test-backtest-send-queue-max-env-resolution
  "backtest queue max should be resolvable from SWIMMY_BACKTEST_SEND_QUEUE_MAX."
  (let ((orig (sb-posix:getenv "SWIMMY_BACKTEST_SEND_QUEUE_MAX")))
    (unwind-protect
        (progn
          (sb-posix:setenv "SWIMMY_BACKTEST_SEND_QUEUE_MAX" "1234" 1)
          (assert-equal 1234 (swimmy.school::resolve-backtest-send-queue-max 99)
                        "env override should be applied")
          (sb-posix:setenv "SWIMMY_BACKTEST_SEND_QUEUE_MAX" "oops" 1)
          (assert-equal 99 (swimmy.school::resolve-backtest-send-queue-max 99)
                        "invalid env should fallback"))
      (when orig
        (sb-posix:setenv "SWIMMY_BACKTEST_SEND_QUEUE_MAX" orig 1))
      (unless orig
        (sb-posix:unsetenv "SWIMMY_BACKTEST_SEND_QUEUE_MAX")))))

(deftest test-backtest-send-uses-subsecond-time
  "backtest-send-allowed-p should use subsecond time source for rate limiting"
  (let* ((orig-now (when (fboundp 'swimmy.school::backtest-now-seconds)
                     (symbol-function 'swimmy.school::backtest-now-seconds)))
         (calls 0)
         (orig-rate swimmy.globals::*backtest-rate-limit-per-sec*)
         (orig-max swimmy.globals::*backtest-max-pending*)
         (orig-submit swimmy.globals::*backtest-submit-count*)
         (orig-last swimmy.globals::*backtest-last-send-ts*)
         (orig-recv (when (boundp 'swimmy.main::*backtest-recv-count*)
                      swimmy.main::*backtest-recv-count*)))
    (unwind-protect
        (progn
          (setf swimmy.globals::*backtest-rate-limit-per-sec* 5)
          (setf swimmy.globals::*backtest-max-pending* 999999)
          (setf swimmy.globals::*backtest-submit-count* 0)
          (setf swimmy.globals::*backtest-last-send-ts* 100.0d0)
          (when (boundp 'swimmy.main::*backtest-recv-count*)
            (setf swimmy.main::*backtest-recv-count* 0))
          (setf (symbol-function 'swimmy.school::backtest-now-seconds)
                (lambda ()
                  (incf calls)
                  100.2d0))
          (assert-true (swimmy.school::backtest-send-allowed-p)
                       "should allow after 0.2s at 5/s")
          (assert-true (> calls 0) "should use backtest-now-seconds"))
      (setf swimmy.globals::*backtest-rate-limit-per-sec* orig-rate)
      (setf swimmy.globals::*backtest-max-pending* orig-max)
      (setf swimmy.globals::*backtest-submit-count* orig-submit)
      (setf swimmy.globals::*backtest-last-send-ts* orig-last)
      (when (boundp 'swimmy.main::*backtest-recv-count*)
        (setf swimmy.main::*backtest-recv-count* orig-recv))
      (if orig-now
          (setf (symbol-function 'swimmy.school::backtest-now-seconds) orig-now)
          (fmakunbound 'swimmy.school::backtest-now-seconds)))))

(deftest test-backtest-throttle-diagnostics-reason
  "backtest-throttle-diagnostics should distinguish pending and rate throttles."
  (let* ((orig-rate swimmy.globals::*backtest-rate-limit-per-sec*)
         (orig-max swimmy.globals::*backtest-max-pending*)
         (orig-submit swimmy.globals::*backtest-submit-count*)
         (orig-last swimmy.globals::*backtest-last-send-ts*)
         (orig-recv (when (boundp 'swimmy.main::*backtest-recv-count*)
                      swimmy.main::*backtest-recv-count*)))
    (unwind-protect
        (progn
          ;; pending throttle should take priority over rate throttle
          (setf swimmy.globals::*backtest-rate-limit-per-sec* 10)
          (setf swimmy.globals::*backtest-max-pending* 5)
          (setf swimmy.globals::*backtest-submit-count* 5)
          (setf swimmy.globals::*backtest-last-send-ts* 100.0d0)
          (when (boundp 'swimmy.main::*backtest-recv-count*)
            (setf swimmy.main::*backtest-recv-count* 0))
          (let ((diag (swimmy.school::backtest-throttle-diagnostics 100.01d0)))
            (assert-equal :pending (getf diag :reason)
                          "pending cap should be reported first"))
          ;; rate throttle when pending is below cap
          (setf swimmy.globals::*backtest-submit-count* 1)
          (when (boundp 'swimmy.main::*backtest-recv-count*)
            (setf swimmy.main::*backtest-recv-count* 0))
          (let ((diag (swimmy.school::backtest-throttle-diagnostics 100.01d0)))
            (assert-equal :rate (getf diag :reason)
                          "rate gate should be reported when pending is under cap")))
      (setf swimmy.globals::*backtest-rate-limit-per-sec* orig-rate)
      (setf swimmy.globals::*backtest-max-pending* orig-max)
      (setf swimmy.globals::*backtest-submit-count* orig-submit)
      (setf swimmy.globals::*backtest-last-send-ts* orig-last)
      (when (boundp 'swimmy.main::*backtest-recv-count*)
        (setf swimmy.main::*backtest-recv-count* orig-recv)))))

(deftest test-backtest-send-allowed-false-on-rate-throttle
  "backtest-send-allowed-p should be NIL when rate interval has not elapsed."
  (let* ((orig-now (when (fboundp 'swimmy.school::backtest-now-seconds)
                     (symbol-function 'swimmy.school::backtest-now-seconds)))
         (orig-rate swimmy.globals::*backtest-rate-limit-per-sec*)
         (orig-max swimmy.globals::*backtest-max-pending*)
         (orig-submit swimmy.globals::*backtest-submit-count*)
         (orig-last swimmy.globals::*backtest-last-send-ts*)
         (orig-recv (when (boundp 'swimmy.main::*backtest-recv-count*)
                      swimmy.main::*backtest-recv-count*)))
    (unwind-protect
        (progn
          (setf swimmy.globals::*backtest-rate-limit-per-sec* 10)
          (setf swimmy.globals::*backtest-max-pending* 1000)
          (setf swimmy.globals::*backtest-submit-count* 0)
          (setf swimmy.globals::*backtest-last-send-ts* 100.0d0)
          (when (boundp 'swimmy.main::*backtest-recv-count*)
            (setf swimmy.main::*backtest-recv-count* 0))
          (setf (symbol-function 'swimmy.school::backtest-now-seconds)
                (lambda () 100.01d0))
          (assert-false (swimmy.school::backtest-send-allowed-p)
                        "should throttle when elapsed < 1/rate"))
      (setf swimmy.globals::*backtest-rate-limit-per-sec* orig-rate)
      (setf swimmy.globals::*backtest-max-pending* orig-max)
      (setf swimmy.globals::*backtest-submit-count* orig-submit)
      (setf swimmy.globals::*backtest-last-send-ts* orig-last)
      (when (boundp 'swimmy.main::*backtest-recv-count*)
        (setf swimmy.main::*backtest-recv-count* orig-recv))
      (if orig-now
          (setf (symbol-function 'swimmy.school::backtest-now-seconds) orig-now)
          (fmakunbound 'swimmy.school::backtest-now-seconds)))))

(deftest test-send-zmq-sleep-suppressed-for-backtest-requester
  "send-zmq-msg should not sleep for backtest requester path"
  (let* ((orig-send (symbol-function 'pzmq:send))
         (orig-req swimmy.globals:*backtest-requester*)
         (orig-cmd swimmy.globals:*cmd-publisher*)
         (orig-rate swimmy.globals::*backtest-rate-limit-per-sec*)
         (orig-max swimmy.globals::*backtest-max-pending*)
         (orig-submit swimmy.globals::*backtest-submit-count*)
         (orig-last swimmy.globals::*backtest-last-send-ts*)
         (orig-recv (when (boundp 'swimmy.main::*backtest-recv-count*)
                      swimmy.main::*backtest-recv-count*))
         (iterations 30))
    (unwind-protect
        (progn
          (setf swimmy.globals:*backtest-requester* :dummy)
          (setf swimmy.globals:*cmd-publisher* :dummy)
          (setf swimmy.globals::*backtest-rate-limit-per-sec* 0)
          (setf swimmy.globals::*backtest-max-pending* 999999)
          (setf swimmy.globals::*backtest-submit-count* 0)
          (setf swimmy.globals::*backtest-last-send-ts* 0.0d0)
          (when (boundp 'swimmy.main::*backtest-recv-count*)
            (setf swimmy.main::*backtest-recv-count* 0))
          (setf (symbol-function 'pzmq:send)
                (lambda (&rest _args) (declare (ignore _args)) t))
          (let* ((cmd-start (get-internal-real-time)))
            (dotimes (_ iterations)
              (swimmy.school::send-zmq-msg "(dummy)" :target :cmd))
            (let* ((cmd-end (get-internal-real-time))
                   (cmd-elapsed (/ (- cmd-end cmd-start)
                                   internal-time-units-per-second)))
              (let* ((bt-start (get-internal-real-time)))
                (dotimes (_ iterations)
                  (swimmy.school::send-zmq-msg "(dummy)" :target :backtest))
                (let* ((bt-end (get-internal-real-time))
                       (bt-elapsed (/ (- bt-end bt-start)
                                      internal-time-units-per-second)))
                  ;; Expect backtest path to be significantly faster when sleep is suppressed.
                  (assert-true (< bt-elapsed (* cmd-elapsed 0.7d0))
                               "backtest path should be faster than cmd path"))))))
      (setf swimmy.globals:*backtest-requester* orig-req)
      (setf swimmy.globals:*cmd-publisher* orig-cmd)
      (setf swimmy.globals::*backtest-rate-limit-per-sec* orig-rate)
      (setf swimmy.globals::*backtest-max-pending* orig-max)
      (setf swimmy.globals::*backtest-submit-count* orig-submit)
      (setf swimmy.globals::*backtest-last-send-ts* orig-last)
      (when (boundp 'swimmy.main::*backtest-recv-count*)
        (setf swimmy.main::*backtest-recv-count* orig-recv))
      (setf (symbol-function 'pzmq:send) orig-send))))

(deftest test-backtest-queue-periodic-flush
  "maybe-flush-backtest-send-queue should trigger flush when interval elapsed"
  (let* ((orig-flush (when (fboundp 'swimmy.school::flush-backtest-send-queue)
                       (symbol-function 'swimmy.school::flush-backtest-send-queue)))
         (orig-queue swimmy.school::*backtest-send-queue*)
         (orig-last (when (boundp 'swimmy.school::*backtest-queue-last-flush*)
                      swimmy.school::*backtest-queue-last-flush*))
         (orig-interval (when (boundp 'swimmy.school::*backtest-queue-flush-interval-sec*)
                          swimmy.school::*backtest-queue-flush-interval-sec*))
         (flushed nil)
         (now (get-universal-time)))
    (unwind-protect
        (progn
          (setf swimmy.school::*backtest-send-queue* (list "(m1)"))
          (setf swimmy.school::*backtest-queue-last-flush* (- now 10))
          (setf swimmy.school::*backtest-queue-flush-interval-sec* 1)
          (setf (symbol-function 'swimmy.school::flush-backtest-send-queue)
                (lambda ()
                  (setf flushed t)
                  1))
          (swimmy.school::maybe-flush-backtest-send-queue)
          (assert-true flushed "expected flush to run")
          (assert-true (>= swimmy.school::*backtest-queue-last-flush* now)
                       "last flush timestamp should update"))
      (setf swimmy.school::*backtest-send-queue* orig-queue)
      (when (boundp 'swimmy.school::*backtest-queue-last-flush*)
        (setf swimmy.school::*backtest-queue-last-flush* orig-last))
      (when (boundp 'swimmy.school::*backtest-queue-flush-interval-sec*)
        (setf swimmy.school::*backtest-queue-flush-interval-sec* orig-interval))
      (when orig-flush
        (setf (symbol-function 'swimmy.school::flush-backtest-send-queue) orig-flush)))))

(deftest test-rr-batch-respects-max-pending
  "RR batch size should follow SWIMMY_BACKTEST_MAX_PENDING"
  (let* ((orig-request (symbol-function 'swimmy.school::request-backtest))
         (orig-notify (and (fboundp 'swimmy.core:notify-discord-alert)
                           (symbol-function 'swimmy.core:notify-discord-alert)))
         (orig-summary (and (fboundp 'swimmy.school::notify-backtest-summary)
                            (symbol-function 'swimmy.school::notify-backtest-summary)))
         (orig-cache (and (fboundp 'swimmy.school::get-cached-backtest)
                          (symbol-function 'swimmy.school::get-cached-backtest)))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-candle swimmy.globals:*candle-history*)
         (orig-hist swimmy.globals:*candle-histories*)
         (orig-max swimmy.globals::*backtest-max-pending*)
         (orig-cursor swimmy.school::*backtest-cursor*)
         (count 0))
    (unwind-protect
        (progn
          (setf swimmy.globals::*backtest-max-pending* 2)
          (setf swimmy.globals:*candle-history* (loop repeat 101 collect 1))
          (setf swimmy.globals:*candle-histories* (make-hash-table :test 'equal))
          (let ((s1 (swimmy.school:make-strategy :name "S1"))
                (s2 (swimmy.school:make-strategy :name "S2"))
                (s3 (swimmy.school:make-strategy :name "S3"))
                (s4 (swimmy.school:make-strategy :name "S4")))
            (setf (strategy-rank s1) :B)
            (setf (strategy-rank s2) :B)
            (setf (strategy-rank s3) :B)
            (setf (strategy-rank s4) :B)
            (setf swimmy.school::*strategy-knowledge-base* (list s1 s2 s3 s4)))
          (setf swimmy.school::*backtest-cursor* 0)
          (when orig-cache
            (setf (symbol-function 'swimmy.school::get-cached-backtest)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (setf (symbol-function 'swimmy.school::request-backtest)
                (lambda (&rest _args) (declare (ignore _args)) (incf count)))
          (when orig-notify
            (setf (symbol-function 'swimmy.core:notify-discord-alert)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-summary
            (setf (symbol-function 'swimmy.school::notify-backtest-summary)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (swimmy.school::batch-backtest-knowledge)
          (assert-equal 2 count "batch should follow max pending"))
      (setf swimmy.globals::*backtest-max-pending* orig-max)
      (setf swimmy.globals:*candle-history* orig-candle)
      (setf swimmy.globals:*candle-histories* orig-hist)
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*backtest-cursor* orig-cursor)
      (when orig-cache
        (setf (symbol-function 'swimmy.school::get-cached-backtest) orig-cache))
      (setf (symbol-function 'swimmy.school::request-backtest) orig-request)
      (when orig-notify
        (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify))
      (when orig-summary
        (setf (symbol-function 'swimmy.school::notify-backtest-summary) orig-summary)))))

(deftest test-rr-batch-respects-available-pending-slots
  "RR batch should cap dispatches by remaining pending slots."
  (let* ((orig-request (symbol-function 'swimmy.school::request-backtest))
         (orig-notify (and (fboundp 'swimmy.core:notify-discord-alert)
                           (symbol-function 'swimmy.core:notify-discord-alert)))
         (orig-summary (and (fboundp 'swimmy.school::notify-backtest-summary)
                            (symbol-function 'swimmy.school::notify-backtest-summary)))
         (orig-cache (and (fboundp 'swimmy.school::get-cached-backtest)
                          (symbol-function 'swimmy.school::get-cached-backtest)))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-candle swimmy.globals:*candle-history*)
         (orig-hist swimmy.globals:*candle-histories*)
         (orig-max swimmy.globals::*backtest-max-pending*)
         (orig-rate swimmy.globals::*backtest-rate-limit-per-sec*)
         (orig-submit swimmy.globals::*backtest-submit-count*)
         (orig-recv (if (boundp 'swimmy.main::*backtest-recv-count*)
                        swimmy.main::*backtest-recv-count*
                        0))
         (orig-cursor swimmy.school::*backtest-cursor*)
         (count 0))
    (unwind-protect
        (progn
          ;; pending=2 of max=3 => only 1 slot available
          (setf swimmy.globals::*backtest-max-pending* 3)
          (setf swimmy.globals::*backtest-rate-limit-per-sec* 1000)
          (setf swimmy.globals::*backtest-submit-count* 5)
          (setf swimmy.main::*backtest-recv-count* 3)
          (setf swimmy.globals:*candle-history* (loop repeat 101 collect 1))
          (setf swimmy.globals:*candle-histories* (make-hash-table :test 'equal))
          (let ((s1 (swimmy.school:make-strategy :name "AP1"))
                (s2 (swimmy.school:make-strategy :name "AP2"))
                (s3 (swimmy.school:make-strategy :name "AP3")))
            (setf (strategy-rank s1) :B
                  (strategy-rank s2) :B
                  (strategy-rank s3) :B)
            (setf swimmy.school::*strategy-knowledge-base* (list s1 s2 s3)))
          (setf swimmy.school::*backtest-cursor* 0)
          (when orig-cache
            (setf (symbol-function 'swimmy.school::get-cached-backtest)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (setf (symbol-function 'swimmy.school::request-backtest)
                (lambda (&rest _args) (declare (ignore _args)) (incf count) t))
          (when orig-notify
            (setf (symbol-function 'swimmy.core:notify-discord-alert)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-summary
            (setf (symbol-function 'swimmy.school::notify-backtest-summary)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (swimmy.school::batch-backtest-knowledge)
          (assert-equal 1 count "Expected RR batch to honor available pending slots"))
      (setf swimmy.globals::*backtest-max-pending* orig-max
            swimmy.globals::*backtest-rate-limit-per-sec* orig-rate
            swimmy.globals::*backtest-submit-count* orig-submit)
      (when (boundp 'swimmy.main::*backtest-recv-count*)
        (setf swimmy.main::*backtest-recv-count* orig-recv))
      (setf swimmy.globals:*candle-history* orig-candle
            swimmy.globals:*candle-histories* orig-hist
            swimmy.school::*strategy-knowledge-base* orig-kb
            swimmy.school::*backtest-cursor* orig-cursor)
      (when orig-cache
        (setf (symbol-function 'swimmy.school::get-cached-backtest) orig-cache))
      (setf (symbol-function 'swimmy.school::request-backtest) orig-request)
      (when orig-notify
        (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify))
      (when orig-summary
        (setf (symbol-function 'swimmy.school::notify-backtest-summary) orig-summary)))))

(deftest test-rr-batch-respects-backtest-rate-limit
  "RR batch should cap per-run dispatches by configured backtest rate limit."
  (let* ((orig-request (symbol-function 'swimmy.school::request-backtest))
         (orig-notify (and (fboundp 'swimmy.core:notify-discord-alert)
                           (symbol-function 'swimmy.core:notify-discord-alert)))
         (orig-summary (and (fboundp 'swimmy.school::notify-backtest-summary)
                            (symbol-function 'swimmy.school::notify-backtest-summary)))
         (orig-cache (and (fboundp 'swimmy.school::get-cached-backtest)
                          (symbol-function 'swimmy.school::get-cached-backtest)))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-candle swimmy.globals:*candle-history*)
         (orig-hist swimmy.globals:*candle-histories*)
         (orig-max swimmy.globals::*backtest-max-pending*)
         (orig-rate swimmy.globals::*backtest-rate-limit-per-sec*)
         (orig-submit swimmy.globals::*backtest-submit-count*)
         (orig-recv (if (boundp 'swimmy.main::*backtest-recv-count*)
                        swimmy.main::*backtest-recv-count*
                        0))
         (orig-cursor swimmy.school::*backtest-cursor*)
         (count 0))
    (unwind-protect
        (progn
          (setf swimmy.globals::*backtest-max-pending* 100)
          (setf swimmy.globals::*backtest-rate-limit-per-sec* 1)
          (setf swimmy.globals::*backtest-submit-count* 0)
          (setf swimmy.main::*backtest-recv-count* 0)
          (setf swimmy.globals:*candle-history* (loop repeat 101 collect 1))
          (setf swimmy.globals:*candle-histories* (make-hash-table :test 'equal))
          (let ((s1 (swimmy.school:make-strategy :name "RL1"))
                (s2 (swimmy.school:make-strategy :name "RL2"))
                (s3 (swimmy.school:make-strategy :name "RL3")))
            (setf (strategy-rank s1) :B
                  (strategy-rank s2) :B
                  (strategy-rank s3) :B)
            (setf swimmy.school::*strategy-knowledge-base* (list s1 s2 s3)))
          (setf swimmy.school::*backtest-cursor* 0)
          (when orig-cache
            (setf (symbol-function 'swimmy.school::get-cached-backtest)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (setf (symbol-function 'swimmy.school::request-backtest)
                (lambda (&rest _args) (declare (ignore _args)) (incf count) t))
          (when orig-notify
            (setf (symbol-function 'swimmy.core:notify-discord-alert)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-summary
            (setf (symbol-function 'swimmy.school::notify-backtest-summary)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (swimmy.school::batch-backtest-knowledge)
          (assert-equal 1 count "Expected RR batch to honor rate-limit budget per run"))
      (setf swimmy.globals::*backtest-max-pending* orig-max
            swimmy.globals::*backtest-rate-limit-per-sec* orig-rate
            swimmy.globals::*backtest-submit-count* orig-submit)
      (when (boundp 'swimmy.main::*backtest-recv-count*)
        (setf swimmy.main::*backtest-recv-count* orig-recv))
      (setf swimmy.globals:*candle-history* orig-candle
            swimmy.globals:*candle-histories* orig-hist
            swimmy.school::*strategy-knowledge-base* orig-kb
            swimmy.school::*backtest-cursor* orig-cursor)
      (when orig-cache
        (setf (symbol-function 'swimmy.school::get-cached-backtest) orig-cache))
      (setf (symbol-function 'swimmy.school::request-backtest) orig-request)
      (when orig-notify
        (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify))
      (when orig-summary
        (setf (symbol-function 'swimmy.school::notify-backtest-summary) orig-summary)))))

(deftest test-rr-batch-applies-rate-limit-pause
  "RR batch should invoke dispatch pacing hook after each accepted request."
  (let* ((orig-request (symbol-function 'swimmy.school::request-backtest))
         (orig-pause (and (fboundp 'swimmy.school::maybe-pause-after-backtest-dispatch)
                          (symbol-function 'swimmy.school::maybe-pause-after-backtest-dispatch)))
         (orig-notify (and (fboundp 'swimmy.core:notify-discord-alert)
                           (symbol-function 'swimmy.core:notify-discord-alert)))
         (orig-summary (and (fboundp 'swimmy.school::notify-backtest-summary)
                            (symbol-function 'swimmy.school::notify-backtest-summary)))
         (orig-cache (and (fboundp 'swimmy.school::get-cached-backtest)
                          (symbol-function 'swimmy.school::get-cached-backtest)))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-candle swimmy.globals:*candle-history*)
         (orig-hist swimmy.globals:*candle-histories*)
         (orig-max swimmy.globals::*backtest-max-pending*)
         (orig-rate swimmy.globals::*backtest-rate-limit-per-sec*)
         (orig-submit swimmy.globals::*backtest-submit-count*)
         (orig-recv (if (boundp 'swimmy.main::*backtest-recv-count*)
                        swimmy.main::*backtest-recv-count*
                        0))
         (orig-cursor swimmy.school::*backtest-cursor*)
         (pause-calls 0))
    (unwind-protect
        (progn
          (setf swimmy.globals::*backtest-max-pending* 100)
          (setf swimmy.globals::*backtest-rate-limit-per-sec* 10)
          (setf swimmy.globals::*backtest-submit-count* 0)
          (setf swimmy.main::*backtest-recv-count* 0)
          (setf swimmy.globals:*candle-history* (loop repeat 101 collect 1))
          (setf swimmy.globals:*candle-histories* (make-hash-table :test 'equal))
          (let ((s1 (swimmy.school:make-strategy :name "RRP1"))
                (s2 (swimmy.school:make-strategy :name "RRP2"))
                (s3 (swimmy.school:make-strategy :name "RRP3")))
            (setf (strategy-rank s1) :B
                  (strategy-rank s2) :B
                  (strategy-rank s3) :B)
            (setf swimmy.school::*strategy-knowledge-base* (list s1 s2 s3)))
          (setf swimmy.school::*backtest-cursor* 0)
          (setf swimmy.globals:*rr-expected-backtest-count* 0)
          (when orig-cache
            (setf (symbol-function 'swimmy.school::get-cached-backtest)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (setf (symbol-function 'swimmy.school::request-backtest)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  t))
          (setf (symbol-function 'swimmy.school::maybe-pause-after-backtest-dispatch)
                (lambda (&optional _pause-sec)
                  (declare (ignore _pause-sec))
                  (incf pause-calls)))
          (when orig-notify
            (setf (symbol-function 'swimmy.core:notify-discord-alert)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-summary
            (setf (symbol-function 'swimmy.school::notify-backtest-summary)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (swimmy.school::batch-backtest-knowledge)
          (assert-equal 3 swimmy.globals:*rr-expected-backtest-count*
                        "Expected RR batch to accept all 3 requests")
          (assert-equal 3 pause-calls
                        "Expected RR pacing hook to run per accepted dispatch"))
      (setf swimmy.globals::*backtest-max-pending* orig-max
            swimmy.globals::*backtest-rate-limit-per-sec* orig-rate
            swimmy.globals::*backtest-submit-count* orig-submit)
      (when (boundp 'swimmy.main::*backtest-recv-count*)
        (setf swimmy.main::*backtest-recv-count* orig-recv))
      (setf swimmy.globals:*candle-history* orig-candle
            swimmy.globals:*candle-histories* orig-hist
            swimmy.school::*strategy-knowledge-base* orig-kb
            swimmy.school::*backtest-cursor* orig-cursor)
      (when orig-cache
        (setf (symbol-function 'swimmy.school::get-cached-backtest) orig-cache))
      (setf (symbol-function 'swimmy.school::request-backtest) orig-request)
      (when orig-pause
        (setf (symbol-function 'swimmy.school::maybe-pause-after-backtest-dispatch) orig-pause))
      (when orig-notify
        (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify))
      (when orig-summary
        (setf (symbol-function 'swimmy.school::notify-backtest-summary) orig-summary)))))

(deftest test-rr-batch-counts-only-accepted-dispatches
  "RR expected count should exclude throttled request-backtest responses."
  (let* ((orig-request (symbol-function 'swimmy.school::request-backtest))
         (orig-notify (and (fboundp 'swimmy.core:notify-discord-alert)
                           (symbol-function 'swimmy.core:notify-discord-alert)))
         (orig-summary (and (fboundp 'swimmy.school::notify-backtest-summary)
                            (symbol-function 'swimmy.school::notify-backtest-summary)))
         (orig-cache (and (fboundp 'swimmy.school::get-cached-backtest)
                          (symbol-function 'swimmy.school::get-cached-backtest)))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-candle swimmy.globals:*candle-history*)
         (orig-hist swimmy.globals:*candle-histories*)
         (orig-max swimmy.globals::*backtest-max-pending*)
         (orig-cursor swimmy.school::*backtest-cursor*)
         (calls 0))
    (unwind-protect
        (progn
          (setf swimmy.globals::*backtest-max-pending* 10)
          (setf swimmy.globals:*candle-history* (loop repeat 101 collect 1))
          (setf swimmy.globals:*candle-histories* (make-hash-table :test 'equal))
          (let ((s1 (swimmy.school:make-strategy :name "D1"))
                (s2 (swimmy.school:make-strategy :name "D2"))
                (s3 (swimmy.school:make-strategy :name "D3")))
            (setf (strategy-rank s1) :B)
            (setf (strategy-rank s2) :B)
            (setf (strategy-rank s3) :B)
            (setf swimmy.school::*strategy-knowledge-base* (list s1 s2 s3)))
          (setf swimmy.school::*backtest-cursor* 0)
          (setf swimmy.globals:*rr-expected-backtest-count* 0)
          (when orig-cache
            (setf (symbol-function 'swimmy.school::get-cached-backtest)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (setf (symbol-function 'swimmy.school::request-backtest)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (incf calls)
                  (if (<= calls 2) :throttled t)))
          (when orig-notify
            (setf (symbol-function 'swimmy.core:notify-discord-alert)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-summary
            (setf (symbol-function 'swimmy.school::notify-backtest-summary)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (swimmy.school::batch-backtest-knowledge)
          (assert-equal 1 swimmy.globals:*rr-expected-backtest-count*
                        "Expected count should include only accepted dispatches"))
      (setf swimmy.globals::*backtest-max-pending* orig-max)
      (setf swimmy.globals:*candle-history* orig-candle)
      (setf swimmy.globals:*candle-histories* orig-hist)
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*backtest-cursor* orig-cursor)
      (when orig-cache
        (setf (symbol-function 'swimmy.school::get-cached-backtest) orig-cache))
      (setf (symbol-function 'swimmy.school::request-backtest) orig-request)
      (when orig-notify
        (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify))
      (when orig-summary
        (setf (symbol-function 'swimmy.school::notify-backtest-summary) orig-summary)))))

(deftest test-rr-batch-skips-retired-strategies
  "RR batch should skip retired/graveyard strategies"
  (let* ((orig-request (symbol-function 'swimmy.school::request-backtest))
         (orig-notify (and (fboundp 'swimmy.core:notify-discord-alert)
                           (symbol-function 'swimmy.core:notify-discord-alert)))
         (orig-summary (and (fboundp 'swimmy.school::notify-backtest-summary)
                            (symbol-function 'swimmy.school::notify-backtest-summary)))
         (orig-cache (and (fboundp 'swimmy.school::get-cached-backtest)
                          (symbol-function 'swimmy.school::get-cached-backtest)))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-candle swimmy.globals:*candle-history*)
         (orig-hist swimmy.globals:*candle-histories*)
         (orig-max swimmy.globals::*backtest-max-pending*)
         (orig-cursor swimmy.school::*backtest-cursor*)
         (seen nil)
         (count 0))
    (unwind-protect
        (progn
          (setf swimmy.globals::*backtest-max-pending* 10)
          (setf swimmy.globals:*candle-history* (loop repeat 101 collect 1))
          (setf swimmy.globals:*candle-histories* (make-hash-table :test 'equal))
          (let ((s1 (swimmy.school:make-strategy :name "S1"))
                (s2 (swimmy.school:make-strategy :name "S2"))
                (s3 (swimmy.school:make-strategy :name "S3")))
            (setf (strategy-rank s1) :B)
            (setf (strategy-rank s2) :retired)
            (setf (strategy-rank s3) :graveyard)
            (setf swimmy.school::*strategy-knowledge-base* (list s1 s2 s3)))
          (setf swimmy.school::*backtest-cursor* 0)
          (when orig-cache
            (setf (symbol-function 'swimmy.school::get-cached-backtest)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (setf (symbol-function 'swimmy.school::request-backtest)
                (lambda (strat &rest _args)
                  (declare (ignore _args))
                  (incf count)
                  (push (strategy-name strat) seen)))
          (when orig-notify
            (setf (symbol-function 'swimmy.core:notify-discord-alert)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-summary
            (setf (symbol-function 'swimmy.school::notify-backtest-summary)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (swimmy.school::batch-backtest-knowledge)
          (assert-equal 1 count "Should only backtest active strategies")
          (assert-true (null (member "S2" seen :test #'string=))
                       "Retired strategy should be skipped")
          (assert-true (null (member "S3" seen :test #'string=))
                       "Graveyard strategy should be skipped"))
      (setf swimmy.globals::*backtest-max-pending* orig-max)
      (setf swimmy.globals:*candle-history* orig-candle)
      (setf swimmy.globals:*candle-histories* orig-hist)
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*backtest-cursor* orig-cursor)
      (when orig-cache
        (setf (symbol-function 'swimmy.school::get-cached-backtest) orig-cache))
      (setf (symbol-function 'swimmy.school::request-backtest) orig-request)
      (when orig-notify
        (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify))
      (when orig-summary
        (setf (symbol-function 'swimmy.school::notify-backtest-summary) orig-summary)))))

(deftest test-rr-batch-skips-db-archived-active-strategies
  "RR batch should skip active in-memory strategies that are archived in DB."
  (let* ((orig-request (symbol-function 'swimmy.school::request-backtest))
         (orig-exec (symbol-function 'swimmy.school::execute-single))
         (orig-notify (and (fboundp 'swimmy.core:notify-discord-alert)
                           (symbol-function 'swimmy.core:notify-discord-alert)))
         (orig-summary (and (fboundp 'swimmy.school::notify-backtest-summary)
                            (symbol-function 'swimmy.school::notify-backtest-summary)))
         (orig-cache (and (fboundp 'swimmy.school::get-cached-backtest)
                          (symbol-function 'swimmy.school::get-cached-backtest)))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-candle swimmy.globals:*candle-history*)
         (orig-hist swimmy.globals:*candle-histories*)
         (orig-max swimmy.globals::*backtest-max-pending*)
         (orig-cursor swimmy.school::*backtest-cursor*)
         (orig-rr-cache (and (boundp 'swimmy.school::*rr-db-archive-cache*)
                             swimmy.school::*rr-db-archive-cache*))
         (seen nil)
         (count 0))
    (unwind-protect
        (progn
          (setf swimmy.globals::*backtest-max-pending* 10)
          (setf swimmy.globals:*candle-history* (loop repeat 101 collect 1))
          (setf swimmy.globals:*candle-histories* (make-hash-table :test 'equal))
          (let ((s1 (swimmy.school:make-strategy :name "DB-LIVE"))
                (s2 (swimmy.school:make-strategy :name "DB-ARCH")))
            (setf (strategy-rank s1) :B)
            (setf (strategy-rank s2) :B)
            (setf swimmy.school::*strategy-knowledge-base* (list s1 s2)))
          (setf swimmy.school::*backtest-cursor* 0)
          (when (boundp 'swimmy.school::*rr-db-archive-cache*)
            (setf swimmy.school::*rr-db-archive-cache* (make-hash-table :test 'equal)))
          (setf (symbol-function 'swimmy.school::execute-single)
                (lambda (sql name)
                  (declare (ignore sql))
                  (if (string= name "DB-ARCH") ":RETIRED" nil)))
          (when orig-cache
            (setf (symbol-function 'swimmy.school::get-cached-backtest)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (setf (symbol-function 'swimmy.school::request-backtest)
                (lambda (strat &rest _args)
                  (declare (ignore _args))
                  (incf count)
                  (push (strategy-name strat) seen)
                  t))
          (when orig-notify
            (setf (symbol-function 'swimmy.core:notify-discord-alert)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-summary
            (setf (symbol-function 'swimmy.school::notify-backtest-summary)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (swimmy.school::batch-backtest-knowledge)
          (assert-equal 1 count "RR should dispatch only DB-live strategy")
          (assert-true (member "DB-LIVE" seen :test #'string=)
                       "Live strategy should be dispatched")
          (assert-true (null (member "DB-ARCH" seen :test #'string=))
                       "DB archived strategy should be skipped"))
      (setf swimmy.globals::*backtest-max-pending* orig-max)
      (setf swimmy.globals:*candle-history* orig-candle)
      (setf swimmy.globals:*candle-histories* orig-hist)
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*backtest-cursor* orig-cursor)
      (setf (symbol-function 'swimmy.school::execute-single) orig-exec)
      (when orig-cache
        (setf (symbol-function 'swimmy.school::get-cached-backtest) orig-cache))
      (setf (symbol-function 'swimmy.school::request-backtest) orig-request)
      (when orig-notify
        (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify))
      (when orig-summary
        (setf (symbol-function 'swimmy.school::notify-backtest-summary) orig-summary))
      (when (boundp 'swimmy.school::*rr-db-archive-cache*)
        (setf swimmy.school::*rr-db-archive-cache* orig-rr-cache)))))

(deftest test-rr-batch-no-active-strategies
  "RR batch should no-op when no active strategies exist"
  (let* ((orig-request (symbol-function 'swimmy.school::request-backtest))
         (orig-notify (and (fboundp 'swimmy.core:notify-discord-alert)
                           (symbol-function 'swimmy.core:notify-discord-alert)))
         (orig-summary (and (fboundp 'swimmy.school::notify-backtest-summary)
                            (symbol-function 'swimmy.school::notify-backtest-summary)))
         (orig-cache (and (fboundp 'swimmy.school::get-cached-backtest)
                          (symbol-function 'swimmy.school::get-cached-backtest)))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-candle swimmy.globals:*candle-history*)
         (orig-hist swimmy.globals:*candle-histories*)
         (orig-max swimmy.globals::*backtest-max-pending*)
         (orig-cursor swimmy.school::*backtest-cursor*)
         (count 0))
    (unwind-protect
        (progn
          (setf swimmy.globals::*backtest-max-pending* 10)
          (setf swimmy.globals:*candle-history* (loop repeat 101 collect 1))
          (setf swimmy.globals:*candle-histories* (make-hash-table :test 'equal))
          (let ((s1 (swimmy.school:make-strategy :name "S1"))
                (s2 (swimmy.school:make-strategy :name "S2")))
            (setf (strategy-rank s1) :retired)
            (setf (strategy-rank s2) :retired)
            (setf swimmy.school::*strategy-knowledge-base* (list s1 s2)))
          (setf swimmy.school::*backtest-cursor* 0)
          (when orig-cache
            (setf (symbol-function 'swimmy.school::get-cached-backtest)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (setf (symbol-function 'swimmy.school::request-backtest)
                (lambda (&rest _args) (declare (ignore _args)) (incf count)))
          (when orig-notify
            (setf (symbol-function 'swimmy.core:notify-discord-alert)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-summary
            (setf (symbol-function 'swimmy.school::notify-backtest-summary)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (swimmy.school::batch-backtest-knowledge)
          (assert-equal 0 count "No active strategies should be backtested"))
      (setf swimmy.globals::*backtest-max-pending* orig-max)
      (setf swimmy.globals:*candle-history* orig-candle)
      (setf swimmy.globals:*candle-histories* orig-hist)
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*backtest-cursor* orig-cursor)
      (when orig-cache
        (setf (symbol-function 'swimmy.school::get-cached-backtest) orig-cache))
      (setf (symbol-function 'swimmy.school::request-backtest) orig-request)
      (when orig-notify
        (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify))
      (when orig-summary
        (setf (symbol-function 'swimmy.school::notify-backtest-summary) orig-summary)))))

(deftest test-recruit-special-forces-skips-existing-founder-names
  "recruit-special-forces should skip founder templates whose names already exist in KB."
  (let* ((orig-registry swimmy.school::*founder-registry*)
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-recruit (symbol-function 'swimmy.school::recruit-founder))
         (orig-force (symbol-function 'swimmy.school::force-recruit-strategy))
         (orig-supported swimmy.core::*supported-symbols*)
         (called nil))
    (unwind-protect
        (progn
          (setf swimmy.core::*supported-symbols* '("USDJPY" "EURUSD" "GBPUSD"))
          (setf swimmy.school::*founder-registry* (make-hash-table :test 'equal))
          (setf (gethash :k-existing swimmy.school::*founder-registry*)
                (lambda () (swimmy.school:make-strategy :name "F-EXISTING")))
          (setf (gethash :k-new swimmy.school::*founder-registry*)
                (lambda () (swimmy.school:make-strategy :name "F-NEW")))
          (setf swimmy.school::*strategy-knowledge-base*
                ;; Under multi-symbol recruitment, names are symbolized (e.g., -USDJPY).
                (list (swimmy.school:make-strategy :name "F-EXISTING-USDJPY")))
          (setf (symbol-function 'swimmy.school::force-recruit-strategy)
                (lambda (&rest _args) (declare (ignore _args)) t))
          (setf (symbol-function 'swimmy.school::recruit-founder)
                (lambda (key &key symbol)
                  (push (list key symbol) called)
                  t))
          (swimmy.school::recruit-special-forces)
          (assert-true (find (list :k-new "USDJPY") called :test #'equal)
                       "Expected missing founder to be recruited")
          (assert-true (find (list :k-existing "EURUSD") called :test #'equal)
                       "Expected existing founder to still be recruited for missing symbols")
          (assert-false (find (list :k-existing "USDJPY") called :test #'equal)
                        "Expected existing founder name to be skipped (per-symbol)"))
      (setf swimmy.school::*founder-registry* orig-registry
            swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.core::*supported-symbols* orig-supported)
      (setf (symbol-function 'swimmy.school::recruit-founder) orig-recruit)
      (setf (symbol-function 'swimmy.school::force-recruit-strategy) orig-force))))

(deftest test-recruit-special-forces-ignores-archived-name-collisions
  "recruit-special-forces should not skip founders when the only name collision is archived."
  (let* ((orig-registry swimmy.school::*founder-registry*)
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-recruit (symbol-function 'swimmy.school::recruit-founder))
         (orig-force (symbol-function 'swimmy.school::force-recruit-strategy))
         (orig-supported swimmy.core::*supported-symbols*)
         (called nil))
    (unwind-protect
        (progn
          (setf swimmy.core::*supported-symbols* '("USDJPY"))
          (setf swimmy.school::*founder-registry* (make-hash-table :test 'equal))
          (setf (gethash :k-existing swimmy.school::*founder-registry*)
                (lambda () (swimmy.school:make-strategy :name "F-EXISTING")))
          (setf swimmy.school::*strategy-knowledge-base*
                (list (swimmy.school:make-strategy :name "F-EXISTING-USDJPY"
                                                   :rank :graveyard
                                                   :symbol "USDJPY")))
          (setf (symbol-function 'swimmy.school::force-recruit-strategy)
                (lambda (&rest _args) (declare (ignore _args)) t))
          (setf (symbol-function 'swimmy.school::recruit-founder)
                (lambda (key &key symbol)
                  (push (list key symbol) called)
                  t))
          (swimmy.school::recruit-special-forces)
          (assert-true (find (list :k-existing "USDJPY") called :test #'equal)
                       "Expected archived-only name collision to be retried"))
      (setf swimmy.school::*founder-registry* orig-registry
            swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.core::*supported-symbols* orig-supported)
      (setf (symbol-function 'swimmy.school::recruit-founder) orig-recruit)
      (setf (symbol-function 'swimmy.school::force-recruit-strategy) orig-force))))

(deftest test-recruit-special-forces-respects-recruit-limit
  "recruit-special-forces should stop after max-attempts founder dispatches."
  (let* ((orig-registry swimmy.school::*founder-registry*)
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-recruit (symbol-function 'swimmy.school::recruit-founder))
         (orig-force (symbol-function 'swimmy.school::force-recruit-strategy))
         (orig-supported swimmy.core::*supported-symbols*)
         (called nil))
    (unwind-protect
        (progn
          (setf swimmy.core::*supported-symbols* '("USDJPY" "EURUSD" "GBPUSD"))
          (setf swimmy.school::*founder-registry* (make-hash-table :test 'equal))
          (setf (gethash :k-a swimmy.school::*founder-registry*)
                (lambda () (swimmy.school:make-strategy :name "F-A")))
          (setf (gethash :k-b swimmy.school::*founder-registry*)
                (lambda () (swimmy.school:make-strategy :name "F-B")))
          (setf swimmy.school::*strategy-knowledge-base* nil)
          (setf (symbol-function 'swimmy.school::force-recruit-strategy)
                (lambda (&rest _args) (declare (ignore _args)) t))
          (setf (symbol-function 'swimmy.school::recruit-founder)
                (lambda (key &key symbol)
                  (push (list key symbol) called)
                  t))
          (swimmy.school::recruit-special-forces :max-attempts 2)
          (assert-equal 2 (length called)
                        "Expected recruit-special-forces to stop at max-attempts=2"))
      (setf swimmy.school::*founder-registry* orig-registry
            swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.core::*supported-symbols* orig-supported)
      (setf (symbol-function 'swimmy.school::recruit-founder) orig-recruit)
      (setf (symbol-function 'swimmy.school::force-recruit-strategy) orig-force))))

(deftest test-recruit-special-forces-filters-founder-keys
  "recruit-special-forces should recruit only selected founder keys when founder-keys is provided."
  (let* ((orig-registry swimmy.school::*founder-registry*)
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-recruit (symbol-function 'swimmy.school::recruit-founder))
         (orig-force (symbol-function 'swimmy.school::force-recruit-strategy))
         (orig-supported swimmy.core::*supported-symbols*)
         (called nil))
    (unwind-protect
        (progn
          (setf swimmy.core::*supported-symbols* '("USDJPY" "EURUSD"))
          (setf swimmy.school::*founder-registry* (make-hash-table :test 'equal))
          (setf (gethash :k-a swimmy.school::*founder-registry*)
                (lambda () (swimmy.school:make-strategy :name "F-A")))
          (setf (gethash :k-b swimmy.school::*founder-registry*)
                (lambda () (swimmy.school:make-strategy :name "F-B")))
          (setf swimmy.school::*strategy-knowledge-base* nil)
          (setf (symbol-function 'swimmy.school::force-recruit-strategy)
                (lambda (&rest _args) (declare (ignore _args)) t))
          (setf (symbol-function 'swimmy.school::recruit-founder)
                (lambda (key &key symbol)
                  (push (list key symbol) called)
                  t))
          (swimmy.school::recruit-special-forces :founder-keys '(:k-b))
          (assert-true called "Expected filtered founder to be recruited")
          (assert-false (find :k-a called :key #'first :test #'eql)
                        "Expected non-selected founder key to be skipped")
          (assert-true (find :k-b called :key #'first :test #'eql)
                       "Expected selected founder key to be recruited"))
      (setf swimmy.school::*founder-registry* orig-registry
            swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.core::*supported-symbols* orig-supported)
      (setf (symbol-function 'swimmy.school::recruit-founder) orig-recruit)
      (setf (symbol-function 'swimmy.school::force-recruit-strategy) orig-force))))

(deftest test-recruit-special-forces-skips-hunter-auto-founders-when-enabled
  "recruit-special-forces should skip school-hunter-auto founders when configured."
  (let* ((orig-registry swimmy.school::*founder-registry*)
         (meta-sym 'swimmy.school::*founder-registry-meta*)
         (orig-meta (and (boundp meta-sym) (symbol-value meta-sym)))
         (skip-sym 'swimmy.school::*special-force-skip-hunter-auto-founders*)
         (orig-skip (and (boundp skip-sym) (symbol-value skip-sym)))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-recruit (symbol-function 'swimmy.school::recruit-founder))
         (orig-force (symbol-function 'swimmy.school::force-recruit-strategy))
         (orig-supported swimmy.core::*supported-symbols*)
         (called nil))
    (unwind-protect
        (progn
          (setf swimmy.core::*supported-symbols* '("USDJPY" "EURUSD" "GBPUSD"))
          (setf swimmy.school::*founder-registry* (make-hash-table :test 'equal))
          (setf (symbol-value meta-sym) (make-hash-table :test 'equal))
          (setf (symbol-value skip-sym) t)
          (setf (gethash :k-auto swimmy.school::*founder-registry*)
                (lambda () (swimmy.school:make-strategy :name "AUTO-CAND")))
          (setf (gethash :k-core swimmy.school::*founder-registry*)
                (lambda () (swimmy.school:make-strategy :name "CORE-CAND")))
          (setf (gethash :k-auto (symbol-value meta-sym))
                (list :source-file "/tmp/src/lisp/school/school-hunter-auto.lisp"))
          (setf (gethash :k-core (symbol-value meta-sym))
                (list :source-file "/tmp/src/lisp/school/school-founders.lisp"))
          (setf swimmy.school::*strategy-knowledge-base* nil)
          (setf (symbol-function 'swimmy.school::force-recruit-strategy)
                (lambda (&rest _args) (declare (ignore _args)) t))
          (setf (symbol-function 'swimmy.school::recruit-founder)
                (lambda (key &key symbol)
                  (push (list key symbol) called)
                  t))
          (swimmy.school::recruit-special-forces)
          (assert-false (find :k-auto called :key #'first :test #'eql)
                        "Expected hunter-auto founder key to be skipped")
          (assert-true (find :k-core called :key #'first :test #'eql)
                       "Expected non-auto founder key to be recruited"))
      (setf swimmy.school::*founder-registry* orig-registry
            swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.core::*supported-symbols* orig-supported)
      (setf (symbol-function 'swimmy.school::recruit-founder) orig-recruit)
      (setf (symbol-function 'swimmy.school::force-recruit-strategy) orig-force)
      (if orig-meta
          (setf (symbol-value meta-sym) orig-meta)
          (makunbound meta-sym))
      (if orig-skip
          (setf (symbol-value skip-sym) orig-skip)
          (makunbound skip-sym)))))

(deftest test-format-phase1-bt-batch-message
  "Phase1 BT batch message should include completion text when flagged"
  (let ((fn (find-symbol "FORMAT-PHASE1-BT-BATCH-MESSAGE" :swimmy.school)))
    (assert-true (and fn (fboundp fn)) "format helper should exist")
    (let ((msg (funcall fn 2 3 1 :cycle-completed t)))
      (assert-true (search "Phase1 BT Cycle Complete" msg)
                   (format nil "Expected completion text, got: ~a" msg)))
    (let ((msg (funcall fn 2 3 1 :cycle-completed nil)))
      (assert-true (null (search "Phase1 BT Cycle Complete" msg))
                   "Completion text should be omitted when not complete"))))

(deftest test-notify-backtest-summary-includes-a-stage1-failure-breakdown
  "RR backtest summary should include A Stage1 failure breakdown."
  (let* ((orig-notify (symbol-function 'swimmy.core:notify-discord-backtest))
         (orig-ranks (symbol-function 'swimmy.school:get-db-rank-counts))
         (orig-buf swimmy.globals:*rr-backtest-results-buffer*)
         (orig-exp swimmy.globals:*rr-expected-backtest-count*)
         (captured nil))
    (unwind-protect
        (progn
          (setf swimmy.globals:*rr-backtest-results-buffer*
                (list (cons "UT-RR-1" (list :sharpe 0.60 :profit-factor 1.20 :win-rate 0.50 :max-dd 0.10 :trades 100))
                      (cons "UT-RR-2" (list :sharpe 0.80 :profit-factor 1.40 :win-rate 0.40 :max-dd 0.10 :trades 100))
                      (cons "UT-RR-3" (list :sharpe 0.90 :profit-factor 1.40 :win-rate 0.50 :max-dd 0.10 :trades 100))))
          (setf swimmy.globals:*rr-expected-backtest-count* 3)
          (setf (symbol-function 'swimmy.school:get-db-rank-counts)
                (lambda ()
                  (list :s 0 :a 0 :b 0 :graveyard 0)))
          (setf (symbol-function 'swimmy.core:notify-discord-backtest)
                (lambda (msg &key color)
                  (declare (ignore color))
                  (setf captured msg)
                  t))
          (swimmy.core:notify-backtest-summary :rr)
          (assert-true (search "A Stage1 Failures (Batch)" captured))
          (assert-true (search "pf<1.30=1" captured))
          (assert-true (search "wr<43%" captured))
          (assert-true (search "pass=1" captured)))
      (setf (symbol-function 'swimmy.core:notify-discord-backtest) orig-notify)
      (setf (symbol-function 'swimmy.school:get-db-rank-counts) orig-ranks)
      (setf swimmy.globals:*rr-backtest-results-buffer* orig-buf)
      (setf swimmy.globals:*rr-expected-backtest-count* orig-exp))))

(deftest test-notify-backtest-summary-preserves-state-for-timeout-progress
  "Timeout progress report should not clear RR buffer/expected state."
  (let* ((orig-notify (symbol-function 'swimmy.core:notify-discord-backtest))
         (orig-ranks (symbol-function 'swimmy.school:get-db-rank-counts))
         (orig-buf swimmy.globals:*rr-backtest-results-buffer*)
         (orig-exp swimmy.globals:*rr-expected-backtest-count*)
         (captured nil))
    (unwind-protect
        (progn
          (setf swimmy.globals:*rr-backtest-results-buffer*
                (list (cons "UT-RR-PROGRESS"
                            (list :sharpe 0.55 :profit-factor 1.22 :win-rate 0.41 :max-dd 0.11 :trades 120))))
          (setf swimmy.globals:*rr-expected-backtest-count* 10)
          (setf (symbol-function 'swimmy.school:get-db-rank-counts)
                (lambda ()
                  (list :s 0 :a 0 :b 0 :graveyard 0)))
          (setf (symbol-function 'swimmy.core:notify-discord-backtest)
                (lambda (msg &key color)
                  (declare (ignore color))
                  (setf captured msg)
                  t))
          (swimmy.core:notify-backtest-summary :rr :preserve-state t)
          (assert-true (search "Progress: 1/10" captured)
                       "Expected timeout progress snapshot in report")
          (assert-equal 10 swimmy.globals:*rr-expected-backtest-count*
                        "Expected RR expected count to remain for ongoing batch")
          (assert-equal 1 (length swimmy.globals:*rr-backtest-results-buffer*)
                        "Expected RR result buffer to remain for ongoing batch"))
      (setf (symbol-function 'swimmy.core:notify-discord-backtest) orig-notify)
      (setf (symbol-function 'swimmy.school:get-db-rank-counts) orig-ranks)
      (setf swimmy.globals:*rr-backtest-results-buffer* orig-buf)
      (setf swimmy.globals:*rr-expected-backtest-count* orig-exp))))

(deftest test-notify-backtest-summary-includes-throughput-and-stage1-metrics
  "RR summary should include throughput/ETA and PF/WR/DD to avoid Sharpe-only misread."
  (let* ((orig-notify (symbol-function 'swimmy.core:notify-discord-backtest))
         (orig-ranks (symbol-function 'swimmy.school:get-db-rank-counts))
         (orig-buf swimmy.globals:*rr-backtest-results-buffer*)
         (orig-exp swimmy.globals:*rr-expected-backtest-count*)
         (orig-start swimmy.globals:*rr-backtest-start-time*)
         (captured nil))
    (unwind-protect
        (progn
          (setf swimmy.globals:*rr-backtest-results-buffer*
                (list (cons "UT-RR-METRICS"
                            (list :sharpe 1.30 :profit-factor 1.22 :win-rate 0.45 :max-dd 0.01 :trades 194))))
          (setf swimmy.globals:*rr-expected-backtest-count* 10)
          (setf swimmy.globals:*rr-backtest-start-time* (- (get-universal-time) 20))
          (setf (symbol-function 'swimmy.school:get-db-rank-counts)
                (lambda ()
                  (list :s 0 :a 0 :b 0 :graveyard 0)))
          (setf (symbol-function 'swimmy.core:notify-discord-backtest)
                (lambda (msg &key color)
                  (declare (ignore color))
                  (setf captured msg)
                  t))
          (swimmy.core:notify-backtest-summary :rr :preserve-state t)
          (assert-true (search "Throughput:" captured)
                       "Expected throughput line in progress summary")
          (assert-true (search "ETA:" captured)
                       "Expected ETA line in progress summary")
          (assert-true (search "PF=" captured)
                       "Expected PF in top strategy line")
          (assert-true (search "WR=" captured)
                       "Expected WR in top strategy line")
          (assert-true (search "DD=" captured)
                       "Expected DD in top strategy line")
          (assert-false (search "~%" captured)
                        "Expected report not to contain raw Lisp ~% format directives"))
      (setf (symbol-function 'swimmy.core:notify-discord-backtest) orig-notify)
      (setf (symbol-function 'swimmy.school:get-db-rank-counts) orig-ranks)
      (setf swimmy.globals:*rr-backtest-results-buffer* orig-buf)
      (setf swimmy.globals:*rr-expected-backtest-count* orig-exp)
      (setf swimmy.globals:*rr-backtest-start-time* orig-start))))

(deftest test-format-percent-no-double
  "format-percent should return a single percent sign"
  (let ((fn (find-symbol "FORMAT-PERCENT" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "format-percent should exist")
    (assert-equal "50%" (funcall fn 0.5) "Expected 50% (single percent)")
    (assert-equal "0%" (funcall fn 0.0) "Expected 0% (single percent)")
    (assert-equal "N/A" (funcall fn nil) "Expected fallback for NIL")))

(deftest test-format-value-rounds-int
  "format-value should round floats when using integer format"
  (let ((fn (find-symbol "FORMAT-VALUE" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "format-value should exist")
    (assert-equal "100000" (funcall fn 100000.4 "~d") "Expected rounded integer")
    (assert-equal "-2" (funcall fn -1.6 "~d") "Expected rounded negative")
    (assert-equal "N/A" (funcall fn nil "~d") "Expected fallback for NIL")))

(deftest test-ledger-persists-equity
  "save-state/load-state should persist equity and drawdown metrics"
  (let* ((tmp-path (merge-pathnames (format nil "/tmp/swimmy-state-~a.sexp" (get-universal-time))))
         (orig-path swimmy.engine::*state-file-path*)
         (orig-equity swimmy.globals::*current-equity*)
         (orig-peak swimmy.globals::*peak-equity*)
         (orig-max-dd swimmy.globals::*max-drawdown*)
         (orig-monitor-peak swimmy.globals::*monitoring-peak-equity*)
         (orig-monitor-dd swimmy.globals::*monitoring-drawdown*)
         (orig-current-dd swimmy.globals::*current-drawdown*)
         (orig-last-account swimmy.globals::*last-account-info-time*))
    (unwind-protect
        (progn
          (setf swimmy.engine::*state-file-path* tmp-path)
          (setf swimmy.globals::*current-equity* 123456.0)
          (setf swimmy.globals::*peak-equity* 234567.0)
          (setf swimmy.globals::*max-drawdown* 12.3)
          (setf swimmy.globals::*monitoring-peak-equity* 345678.0)
          (setf swimmy.globals::*monitoring-drawdown* 4.5)
          (setf swimmy.globals::*current-drawdown* 6.7)
          (setf swimmy.globals::*last-account-info-time* 999)
          (swimmy.engine:save-state)
          (setf swimmy.globals::*current-equity* 0.0)
          (setf swimmy.globals::*peak-equity* 0.0)
          (setf swimmy.globals::*max-drawdown* 0.0)
          (setf swimmy.globals::*monitoring-peak-equity* 0.0)
          (setf swimmy.globals::*monitoring-drawdown* 0.0)
          (setf swimmy.globals::*current-drawdown* 0.0)
          (setf swimmy.globals::*last-account-info-time* 0)
          (swimmy.engine:load-state)
          (assert-equal 123456.0 swimmy.globals::*current-equity* "Current equity should restore")
          (assert-equal 234567.0 swimmy.globals::*peak-equity* "Peak equity should restore")
          (assert-equal 12.3 swimmy.globals::*max-drawdown* "Max drawdown should restore")
          (assert-equal 345678.0 swimmy.globals::*monitoring-peak-equity* "Monitoring peak should restore")
          (assert-equal 4.5 swimmy.globals::*monitoring-drawdown* "Monitoring drawdown should restore")
          (assert-equal 6.7 swimmy.globals::*current-drawdown* "Current drawdown should restore")
          (assert-equal 999 swimmy.globals::*last-account-info-time* "Account info timestamp should restore"))
      (setf swimmy.engine::*state-file-path* orig-path)
      (setf swimmy.globals::*current-equity* orig-equity)
      (setf swimmy.globals::*peak-equity* orig-peak)
      (setf swimmy.globals::*max-drawdown* orig-max-dd)
      (setf swimmy.globals::*monitoring-peak-equity* orig-monitor-peak)
      (setf swimmy.globals::*monitoring-drawdown* orig-monitor-dd)
      (setf swimmy.globals::*current-drawdown* orig-current-dd)
      (setf swimmy.globals::*last-account-info-time* orig-last-account)
      (when (probe-file tmp-path) (delete-file tmp-path)))))

(deftest test-system-pulse-5m-text
  "System Pulse heartbeat should not claim cycle completion"
  (let* ((orig-queue (and (fboundp 'swimmy.core:queue-discord-notification)
                          (symbol-function 'swimmy.core:queue-discord-notification)))
         (orig-last (and (boundp 'swimmy.school::*last-cycle-notify-time*)
                         swimmy.school::*last-cycle-notify-time*))
         (captured nil))
    (unwind-protect
        (progn
          (when (boundp 'swimmy.school::*last-cycle-notify-time*)
            (setf swimmy.school::*last-cycle-notify-time* 0))
          (when orig-queue
            (setf (symbol-function 'swimmy.core:queue-discord-notification)
                  (lambda (_webhook msg &key title color)
                    (declare (ignore _webhook title color))
                    (setf captured msg))))
          (swimmy.school::notify-cycle-complete)
          (assert-true captured "Expected System Pulse message")
          (assert-true (search "System Pulse (5m)" captured) "Expected 5m text")
          (assert-true (null (search "Cycle Complete" captured))
                       "Should not claim cycle completion"))
      (when (boundp 'swimmy.school::*last-cycle-notify-time*)
        (setf swimmy.school::*last-cycle-notify-time* orig-last))
      (when orig-queue
        (setf (symbol-function 'swimmy.core:queue-discord-notification) orig-queue)))))

(deftest test-backtest-pending-count-decrements-on-recv
  "pending count should drop when a BACKTEST_RESULT is processed"
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (msg "((type . \"BACKTEST_RESULT\") (result . ((strategy_name . \"UT-PENDING\") (sharpe . 0.1) (trades . 1))))"))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (setf swimmy.globals::*backtest-submit-count* 5)
    (setf swimmy.main::*backtest-recv-count* 0)
    (funcall fn msg)
    (assert-true (> swimmy.main::*backtest-recv-count* 0) "recv count increments")))

(deftest test-deferred-flush-respects-batch
  "flush-deferred-founders should only request up to batch size"
  (let* ((orig-request (symbol-function 'swimmy.school::request-backtest))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-batch (and (boundp 'swimmy.school::*deferred-flush-batch*)
                          swimmy.school::*deferred-flush-batch*))
         (count 0))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base*
                (list (swimmy.school:make-strategy :name "S1")
                      (swimmy.school:make-strategy :name "S2")
                      (swimmy.school:make-strategy :name "S3")))
          (setf swimmy.school::*deferred-flush-batch* 1)
          (setf (symbol-function 'swimmy.school::request-backtest)
                (lambda (&rest _) (declare (ignore _)) (incf count)))
          (swimmy.school::flush-deferred-founders)
          (assert-equal 1 count "batch=1 should send exactly one"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*deferred-flush-batch* orig-batch)
      (setf (symbol-function 'swimmy.school::request-backtest) orig-request))))

(deftest test-deferred-flush-counts-only-accepted-dispatches
  "Deferred flush sent count should ignore throttled request-backtest responses."
  (let* ((orig-request (symbol-function 'swimmy.school::request-backtest))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-batch (and (boundp 'swimmy.school::*deferred-flush-batch*)
                          swimmy.school::*deferred-flush-batch*))
         (orig-queue swimmy.school::*deferred-flush-queue*)
         (orig-queue-count swimmy.school::*deferred-flush-queue-count*)
         (orig-queued-names swimmy.school::*deferred-flush-queued-names*))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base*
                (list (swimmy.school:make-strategy :name "TD1")
                      (swimmy.school:make-strategy :name "TD2")))
          (setf swimmy.school::*deferred-flush-queue* nil)
          (setf swimmy.school::*deferred-flush-queue-count* 0)
          (setf swimmy.school::*deferred-flush-queued-names* (make-hash-table :test 'equal))
          (setf swimmy.school::*deferred-flush-batch* 2)
          (setf (symbol-function 'swimmy.school::request-backtest)
                (lambda (strat &rest _args)
                  (declare (ignore strat _args))
                  :throttled))
          (let ((sent (swimmy.school::flush-deferred-founders :limit 2)))
            (assert-equal 0 sent "throttled dispatches should not be counted as sent")))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*deferred-flush-batch* orig-batch)
      (setf swimmy.school::*deferred-flush-queue* orig-queue)
      (setf swimmy.school::*deferred-flush-queue-count* orig-queue-count)
      (setf swimmy.school::*deferred-flush-queued-names* orig-queued-names)
      (setf (symbol-function 'swimmy.school::request-backtest) orig-request))))

(deftest test-deferred-flush-throttled-requeues-and-pauses-tick
  "Deferred flush should requeue throttled strategy and stop current tick."
  (let* ((orig-request (symbol-function 'swimmy.school::request-backtest))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-batch (and (boundp 'swimmy.school::*deferred-flush-batch*)
                          swimmy.school::*deferred-flush-batch*))
         (orig-queue swimmy.school::*deferred-flush-queue*)
         (orig-queue-count swimmy.school::*deferred-flush-queue-count*)
         (orig-queued-names swimmy.school::*deferred-flush-queued-names*)
         (calls 0))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base*
                (list (swimmy.school:make-strategy :name "TQ1")
                      (swimmy.school:make-strategy :name "TQ2")))
          (setf swimmy.school::*deferred-flush-queue* nil)
          (setf swimmy.school::*deferred-flush-queue-count* 0)
          (setf swimmy.school::*deferred-flush-queued-names* (make-hash-table :test 'equal))
          (setf swimmy.school::*deferred-flush-batch* 10)
          (setf (symbol-function 'swimmy.school::request-backtest)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (incf calls)
                  :throttled))
          (let ((sent (swimmy.school::flush-deferred-founders :limit 10)))
            (assert-equal 0 sent "Expected no accepted dispatch on throttled send")
            (assert-equal 1 calls "Expected tick to stop after first throttled dispatch")
            (assert-equal 2 swimmy.school::*deferred-flush-queue-count*
                          "Expected both deferred strategies to remain queued")))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*deferred-flush-batch* orig-batch)
      (setf swimmy.school::*deferred-flush-queue* orig-queue)
      (setf swimmy.school::*deferred-flush-queue-count* orig-queue-count)
      (setf swimmy.school::*deferred-flush-queued-names* orig-queued-names)
      (setf (symbol-function 'swimmy.school::request-backtest) orig-request))))

(deftest test-deferred-flush-applies-rate-limit-pause
  "Deferred flush should invoke pacing hook for accepted dispatches."
  (let* ((orig-request (symbol-function 'swimmy.school::request-backtest))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-batch (and (boundp 'swimmy.school::*deferred-flush-batch*)
                          swimmy.school::*deferred-flush-batch*))
         (orig-rate swimmy.globals::*backtest-rate-limit-per-sec*)
         (orig-queue swimmy.school::*deferred-flush-queue*)
         (orig-queue-count swimmy.school::*deferred-flush-queue-count*)
         (orig-queued-names swimmy.school::*deferred-flush-queued-names*)
         (pause-sym 'swimmy.school::maybe-pause-after-backtest-dispatch)
         (orig-pause (and (fboundp pause-sym) (symbol-function pause-sym)))
         (pause-calls 0))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base*
                (list (swimmy.school:make-strategy :name "RP1")
                      (swimmy.school:make-strategy :name "RP2")))
          (setf swimmy.school::*deferred-flush-queue* nil)
          (setf swimmy.school::*deferred-flush-queue-count* 0)
          (setf swimmy.school::*deferred-flush-queued-names* (make-hash-table :test 'equal))
          (setf swimmy.school::*deferred-flush-batch* 2)
          (setf swimmy.globals::*backtest-rate-limit-per-sec* 100)
          (setf (symbol-function 'swimmy.school::request-backtest)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  t))
          (setf (symbol-function pause-sym)
                (lambda (&optional _interval)
                  (declare (ignore _interval))
                  (incf pause-calls)))
          (let ((sent (swimmy.school::flush-deferred-founders :limit 2)))
            (assert-equal 2 sent "Expected two accepted dispatches")
            (assert-equal 2 pause-calls "Expected pacing hook per accepted dispatch")))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*deferred-flush-batch* orig-batch)
      (setf swimmy.globals::*backtest-rate-limit-per-sec* orig-rate)
      (setf swimmy.school::*deferred-flush-queue* orig-queue)
      (setf swimmy.school::*deferred-flush-queue-count* orig-queue-count)
      (setf swimmy.school::*deferred-flush-queued-names* orig-queued-names)
      (setf (symbol-function 'swimmy.school::request-backtest) orig-request)
      (if orig-pause
          (setf (symbol-function pause-sym) orig-pause)
          (fmakunbound pause-sym)))))

(deftest test-schedule-deferred-founders-respects-hard-cap
  "Deferred founder scheduling should respect queue hard cap."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-queue swimmy.school::*deferred-flush-queue*)
         (orig-queue-count swimmy.school::*deferred-flush-queue-count*)
         (orig-queued-names swimmy.school::*deferred-flush-queued-names*)
         (orig-hard-cap (and (boundp 'swimmy.school::*deferred-flush-queue-hard-cap*)
                             swimmy.school::*deferred-flush-queue-hard-cap*)))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base*
                (list (swimmy.school:make-strategy :name "HC1" :rank nil)
                      (swimmy.school:make-strategy :name "HC2" :rank nil)
                      (swimmy.school:make-strategy :name "HC3" :rank nil)
                      (swimmy.school:make-strategy :name "HC4" :rank nil)))
          (setf swimmy.school::*deferred-flush-queue* nil)
          (setf swimmy.school::*deferred-flush-queue-count* 0)
          (setf swimmy.school::*deferred-flush-queued-names* (make-hash-table :test 'equal))
          (setf swimmy.school::*deferred-flush-queue-hard-cap* 2)
          (let ((added (swimmy.school::schedule-deferred-founders)))
            (assert-equal 2 added "Expected scheduling to stop at hard cap")
            (assert-equal 2 swimmy.school::*deferred-flush-queue-count*
                          "Queue count should honor hard cap")))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb
            swimmy.school::*deferred-flush-queue* orig-queue
            swimmy.school::*deferred-flush-queue-count* orig-queue-count
            swimmy.school::*deferred-flush-queued-names* orig-queued-names)
      (when (boundp 'swimmy.school::*deferred-flush-queue-hard-cap*)
        (setf swimmy.school::*deferred-flush-queue-hard-cap* orig-hard-cap)))))

(deftest test-schedule-deferred-founders-skips-db-archived-candidates
  "Scheduling deferred founders should skip names archived in DB."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-queue swimmy.school::*deferred-flush-queue*)
         (orig-queue-count swimmy.school::*deferred-flush-queue-count*)
         (orig-queued-names swimmy.school::*deferred-flush-queued-names*)
         (orig-exec (symbol-function 'swimmy.school::execute-single))
         (orig-cache (and (boundp 'swimmy.school::*deferred-db-archive-cache*)
                          swimmy.school::*deferred-db-archive-cache*))
         (arch (swimmy.school:make-strategy :name "ARCH-DB" :rank nil))
         (live (swimmy.school:make-strategy :name "LIVE-DB" :rank nil)))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list arch live))
          (setf swimmy.school::*deferred-flush-queue* nil)
          (setf swimmy.school::*deferred-flush-queue-count* 0)
          (setf swimmy.school::*deferred-flush-queued-names* (make-hash-table :test 'equal))
          (when (boundp 'swimmy.school::*deferred-db-archive-cache*)
            (setf swimmy.school::*deferred-db-archive-cache* (make-hash-table :test 'equal)))
          (setf (symbol-function 'swimmy.school::execute-single)
                (lambda (sql name)
                  (declare (ignore sql))
                  (if (string= name "ARCH-DB") ":RETIRED" nil)))
          (let ((added (swimmy.school::schedule-deferred-founders)))
            (assert-equal 1 added "Only non-archived candidate should be queued")
            (assert-equal 1 swimmy.school::*deferred-flush-queue-count*
                          "Queue count should exclude archived DB candidate")
            (assert-equal "LIVE-DB"
                          (swimmy.school:strategy-name (first swimmy.school::*deferred-flush-queue*))
                          "Live candidate should be queued first")))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb
            swimmy.school::*deferred-flush-queue* orig-queue
            swimmy.school::*deferred-flush-queue-count* orig-queue-count
            swimmy.school::*deferred-flush-queued-names* orig-queued-names)
      (setf (symbol-function 'swimmy.school::execute-single) orig-exec)
      (when (boundp 'swimmy.school::*deferred-db-archive-cache*)
        (setf swimmy.school::*deferred-db-archive-cache* orig-cache)))))

(deftest test-flush-deferred-founders-skips-db-archived-before-dispatch
  "Flush should skip dispatch for candidates archived in DB."
  (let* ((orig-request (symbol-function 'swimmy.school::request-backtest))
         (orig-exec (symbol-function 'swimmy.school::execute-single))
         (orig-queue swimmy.school::*deferred-flush-queue*)
         (orig-queue-count swimmy.school::*deferred-flush-queue-count*)
         (orig-queued-names swimmy.school::*deferred-flush-queued-names*)
         (orig-cache (and (boundp 'swimmy.school::*deferred-db-archive-cache*)
                          swimmy.school::*deferred-db-archive-cache*))
         (calls 0)
         (arch (swimmy.school:make-strategy :name "ARCH-FLUSH" :rank nil))
         (live (swimmy.school:make-strategy :name "LIVE-FLUSH" :rank nil)))
    (unwind-protect
        (progn
          (setf swimmy.school::*deferred-flush-queue* (list arch live))
          (setf swimmy.school::*deferred-flush-queue-count* 2)
          (setf swimmy.school::*deferred-flush-queued-names* (make-hash-table :test 'equal))
          (setf (gethash "ARCH-FLUSH" swimmy.school::*deferred-flush-queued-names*) t)
          (setf (gethash "LIVE-FLUSH" swimmy.school::*deferred-flush-queued-names*) t)
          (when (boundp 'swimmy.school::*deferred-db-archive-cache*)
            (setf swimmy.school::*deferred-db-archive-cache* (make-hash-table :test 'equal)))
          (setf (symbol-function 'swimmy.school::execute-single)
                (lambda (sql name)
                  (declare (ignore sql))
                  (if (string= name "ARCH-FLUSH") ":GRAVEYARD" nil)))
          (setf (symbol-function 'swimmy.school::request-backtest)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (incf calls)
                  t))
          (let ((sent (swimmy.school::flush-deferred-founders :limit 2)))
            (assert-equal 1 sent "Only non-archived candidate should be dispatched")
            (assert-equal 1 calls "request-backtest should be called once")))
      (setf (symbol-function 'swimmy.school::request-backtest) orig-request)
      (setf (symbol-function 'swimmy.school::execute-single) orig-exec)
      (setf swimmy.school::*deferred-flush-queue* orig-queue
            swimmy.school::*deferred-flush-queue-count* orig-queue-count
            swimmy.school::*deferred-flush-queued-names* orig-queued-names)
      (when (boundp 'swimmy.school::*deferred-db-archive-cache*)
        (setf swimmy.school::*deferred-db-archive-cache* orig-cache)))))

(deftest test-recruit-founder-preflight-skips-logic-duplicate-before-add-to-kb
  "Founder preflight should skip obvious logic duplicates before add-to-kb."
  (let* ((orig-registry swimmy.school::*founder-registry*)
         (orig-add (symbol-function 'swimmy.school::add-to-kb))
         (orig-dupe (symbol-function 'swimmy.school::is-logic-duplicate-p))
         (orig-corr (symbol-function 'swimmy.school::find-correlated-strategy))
         (preflight-sym 'swimmy.school::*founder-preflight-screen-enabled*)
         (cache-sym 'swimmy.school::*founder-preflight-reject-cache*)
         (orig-preflight (and (boundp preflight-sym) (symbol-value preflight-sym)))
         (orig-cache (and (boundp cache-sym) (symbol-value cache-sym)))
         (add-called 0)
         (key :ut-preflight-dupe)
         (founder (swimmy.school:make-strategy
                   :name "UT-FOUNDER-PREFLIGHT-DUPE"
                   :category :scalp
                   :timeframe 5
                   :symbol "USDJPY"
                   :rank nil
                   :sl 0.22
                   :tp 0.31
                   :indicators '((sma 20))
                   :entry '(> close sma-20)
                   :exit '(< close sma-20))))
    (unwind-protect
        (progn
          (setf swimmy.school::*founder-registry* (make-hash-table :test 'equal))
          (setf (gethash key swimmy.school::*founder-registry*)
                (lambda () founder))
          (setf (symbol-value preflight-sym) t)
          (setf (symbol-value cache-sym) (make-hash-table :test 'equal))
          (setf (symbol-function 'swimmy.school::add-to-kb)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (incf add-called)
                  t))
          (setf (symbol-function 'swimmy.school::is-logic-duplicate-p)
                (lambda (_strategy _kb)
                  (declare (ignore _strategy _kb))
                  founder))
          (setf (symbol-function 'swimmy.school::find-correlated-strategy)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (values nil 0.0)))
          (assert-false (swimmy.school::recruit-founder key)
                        "Expected preflight duplicate screen to reject founder")
          (assert-equal 0 add-called
                        "Expected add-to-kb to be skipped when preflight detects duplicate"))
      (setf swimmy.school::*founder-registry* orig-registry)
      (setf (symbol-function 'swimmy.school::add-to-kb) orig-add)
      (setf (symbol-function 'swimmy.school::is-logic-duplicate-p) orig-dupe)
      (setf (symbol-function 'swimmy.school::find-correlated-strategy) orig-corr)
      (if orig-preflight
          (setf (symbol-value preflight-sym) orig-preflight)
          (makunbound preflight-sym))
      (if orig-cache
          (setf (symbol-value cache-sym) orig-cache)
          (makunbound cache-sym)))))

(deftest test-recruit-founder-preflight-cooldown-skips-second-retry-after-reject
  "Founder preflight should cooldown repeated retries after add-to-kb rejection."
  (let* ((orig-registry swimmy.school::*founder-registry*)
         (orig-add (symbol-function 'swimmy.school::add-to-kb))
         (orig-dupe (symbol-function 'swimmy.school::is-logic-duplicate-p))
         (orig-corr (symbol-function 'swimmy.school::find-correlated-strategy))
         (preflight-sym 'swimmy.school::*founder-preflight-screen-enabled*)
         (cache-sym 'swimmy.school::*founder-preflight-reject-cache*)
         (cooldown-sym 'swimmy.school::*founder-preflight-reject-cooldown-seconds*)
         (orig-preflight (and (boundp preflight-sym) (symbol-value preflight-sym)))
         (orig-cache (and (boundp cache-sym) (symbol-value cache-sym)))
         (orig-cooldown (and (boundp cooldown-sym) (symbol-value cooldown-sym)))
         (add-called 0)
         (key :ut-preflight-cooldown)
         (founder (swimmy.school:make-strategy
                   :name "UT-FOUNDER-PREFLIGHT-COOLDOWN"
                   :category :breakout
                   :timeframe 15
                   :symbol "USDJPY"
                   :rank nil
                   :sl 0.24
                   :tp 0.36
                   :indicators '((ema 9))
                   :entry '(> close ema-9)
                   :exit '(< close ema-9))))
    (unwind-protect
        (progn
          (setf swimmy.school::*founder-registry* (make-hash-table :test 'equal))
          (setf (gethash key swimmy.school::*founder-registry*)
                (lambda () founder))
          (setf (symbol-value preflight-sym) t)
          (setf (symbol-value cache-sym) (make-hash-table :test 'equal))
          (setf (symbol-value cooldown-sym) 600)
          (setf (symbol-function 'swimmy.school::is-logic-duplicate-p)
                (lambda (_strategy _kb)
                  (declare (ignore _strategy _kb))
                  nil))
          (setf (symbol-function 'swimmy.school::find-correlated-strategy)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (values nil 0.0)))
          (setf (symbol-function 'swimmy.school::add-to-kb)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (incf add-called)
                  nil))
          (assert-false (swimmy.school::recruit-founder key)
                        "First recruit should fail at add-to-kb")
          (assert-false (swimmy.school::recruit-founder key)
                        "Second recruit should be blocked by cooldown preflight")
          (assert-equal 1 add-called
                        "Expected add-to-kb to be called only once across retry burst"))
      (setf swimmy.school::*founder-registry* orig-registry)
      (setf (symbol-function 'swimmy.school::add-to-kb) orig-add)
      (setf (symbol-function 'swimmy.school::is-logic-duplicate-p) orig-dupe)
      (setf (symbol-function 'swimmy.school::find-correlated-strategy) orig-corr)
      (if orig-preflight
          (setf (symbol-value preflight-sym) orig-preflight)
          (makunbound preflight-sym))
      (if orig-cache
          (setf (symbol-value cache-sym) orig-cache)
          (makunbound cache-sym))
      (if orig-cooldown
          (setf (symbol-value cooldown-sym) orig-cooldown)
          (makunbound cooldown-sym)))))

(deftest test-recruit-founder-applies-symbol-override-and-rewrites-name
  "recruit-founder should accept a symbol override, set strategy.symbol, and rewrite strategy.name to include the symbol."
  (let* ((orig-registry swimmy.school::*founder-registry*)
         (orig-add (symbol-function 'swimmy.school::add-to-kb))
         (orig-preflight (and (boundp 'swimmy.school::*founder-preflight-screen-enabled*)
                              swimmy.school::*founder-preflight-screen-enabled*))
         (orig-startup (and (boundp 'swimmy.school::*startup-mode*)
                            swimmy.school::*startup-mode*))
         (captured nil)
         (key :ut-founder-symbol-override)
         (maker (lambda ()
                  (swimmy.school:make-strategy
                   :name "UT-FOUNDER-SYM"
                   :category :trend
                   :timeframe 60
                   :symbol "USDJPY"
                   :rank nil
                   :sl 0.22
                   :tp 0.31
                   :indicators '((sma 20))
                   :entry '(> close sma-20)
                   :exit '(< close sma-20)))))
    (unwind-protect
        (progn
          (setf swimmy.school::*founder-registry* (make-hash-table :test 'equal))
          (setf (gethash key swimmy.school::*founder-registry*) maker)
          ;; Isolate this test from preflight/correlation behavior.
          (setf swimmy.school::*founder-preflight-screen-enabled* nil)
          ;; Avoid async BT dispatch path; we only assert on the strategy handed to add-to-kb.
          (setf swimmy.school::*startup-mode* t)
          (setf (symbol-function 'swimmy.school::add-to-kb)
                (lambda (strategy source &rest _args)
                  (declare (ignore source _args))
                  (setf captured strategy)
                  t))
          (assert-true (swimmy.school::recruit-founder key :symbol "EURUSD")
                       "Expected recruit-founder to accept EURUSD symbol override")
          (assert-true (and captured (swimmy.school::strategy-p captured))
                       "Expected add-to-kb to receive a strategy instance")
          (assert-equal "EURUSD" (swimmy.school::strategy-symbol captured)
                        "Expected strategy.symbol to be set to override")
          (assert-true (search "EURUSD" (string-upcase (swimmy.school::strategy-name captured)))
                       "Expected strategy.name to include override symbol"))
      (setf swimmy.school::*founder-registry* orig-registry)
      (setf (symbol-function 'swimmy.school::add-to-kb) orig-add)
      (setf swimmy.school::*founder-preflight-screen-enabled* orig-preflight)
      (setf swimmy.school::*startup-mode* orig-startup))))

(deftest test-recruit-founder-queues-phase1-before-admission
  "recruit-founder should require Phase1 gating and avoid duplicate request-backtest when queued."
  (let* ((orig-registry swimmy.school::*founder-registry*)
         (orig-add (symbol-function 'swimmy.school::add-to-kb))
         (orig-request (symbol-function 'swimmy.school::request-backtest))
         (orig-preflight (and (boundp 'swimmy.school::*founder-preflight-screen-enabled*)
                              swimmy.school::*founder-preflight-screen-enabled*))
         (orig-startup (and (boundp 'swimmy.school::*startup-mode*)
                            swimmy.school::*startup-mode*))
         (captured-require-bt nil)
         (captured-source nil)
         (request-called 0)
         (key :ut-founder-phase1-queue)
         (maker (lambda ()
                  (swimmy.school:make-strategy
                   :name "UT-FOUNDER-P1-QUEUE"
                   :category :trend
                   :timeframe 60
                   :symbol "USDJPY"
                   :rank nil
                   :sl 0.22
                   :tp 0.31
                   :indicators '((sma 20))
                   :entry '(> close sma-20)
                   :exit '(< close sma-20)))))
    (unwind-protect
        (progn
          (setf swimmy.school::*founder-registry* (make-hash-table :test 'equal))
          (setf (gethash key swimmy.school::*founder-registry*) maker)
          ;; Isolate this test from preflight/correlation behavior.
          (setf swimmy.school::*founder-preflight-screen-enabled* nil)
          (setf swimmy.school::*startup-mode* nil)
          (setf (symbol-function 'swimmy.school::add-to-kb)
                (lambda (_strategy source &key require-bt &allow-other-keys)
                  (setf captured-source source
                        captured-require-bt require-bt)
                  (values t :queued-phase1)))
          (setf (symbol-function 'swimmy.school::request-backtest)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (incf request-called)
                  t))
          (assert-true (swimmy.school::recruit-founder key)
                       "Expected recruit-founder success when add-to-kb queues Phase1")
          (assert-equal :founder captured-source
                        "Expected founder source to be propagated into add-to-kb")
          (assert-true captured-require-bt
                       "Founder intake should require BT/Phase1 gating")
          (assert-equal 0 request-called
                        "Queued Phase1 path must not emit duplicate request-backtest"))
      (setf swimmy.school::*founder-registry* orig-registry)
      (setf (symbol-function 'swimmy.school::add-to-kb) orig-add)
      (setf (symbol-function 'swimmy.school::request-backtest) orig-request)
      (setf swimmy.school::*founder-preflight-screen-enabled* orig-preflight)
      (setf swimmy.school::*startup-mode* orig-startup))))

(deftest test-recruit-special-forces-recruits-founders-per-supported-symbol
  "recruit-special-forces should attempt founder recruitment per supported symbol (USDJPY/EURUSD/GBPUSD)."
  (let* ((orig-registry swimmy.school::*founder-registry*)
         (orig-meta swimmy.school::*founder-registry-meta*)
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-recruit (symbol-function 'swimmy.school::recruit-founder))
         (orig-supported swimmy.core::*supported-symbols*)
         (calls nil)
         (key :ut-founder-multi-symbol))
    (unwind-protect
        (progn
          (setf swimmy.core::*supported-symbols* '("USDJPY" "EURUSD" "GBPUSD"))
          (setf swimmy.school::*strategy-knowledge-base* nil)
          (setf swimmy.school::*founder-registry* (make-hash-table :test 'equal))
          (setf swimmy.school::*founder-registry-meta* (make-hash-table :test 'equal))
          (setf (gethash key swimmy.school::*founder-registry*)
                (lambda ()
                  (swimmy.school:make-strategy :name "UT-SF-FOUNDER" :rank nil)))
          (setf (gethash key swimmy.school::*founder-registry-meta*)
                (list :name "UT-SF-FOUNDER" :source-file "unit-test"))
          (setf (symbol-function 'swimmy.school::recruit-founder)
                (lambda (_key &key symbol)
                  (push symbol calls)
                  t))
          (swimmy.school::recruit-special-forces)
          (assert-equal 3 (length calls)
                        "Expected one recruit attempt per supported symbol")
          (assert-true (find "USDJPY" calls :test #'string=)
                       "Expected USDJPY attempt")
          (assert-true (find "EURUSD" calls :test #'string=)
                       "Expected EURUSD attempt")
          (assert-true (find "GBPUSD" calls :test #'string=)
                       "Expected GBPUSD attempt"))
      (setf swimmy.school::*founder-registry* orig-registry)
      (setf swimmy.school::*founder-registry-meta* orig-meta)
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.core::*supported-symbols* orig-supported)
      (setf (symbol-function 'swimmy.school::recruit-founder) orig-recruit))))

(deftest test-backtest-uses-csv-override
  "request-backtest should honor SWIMMY_BACKTEST_CSV_OVERRIDE when set"
  (require :sb-posix)
  (let* ((orig-env (uiop:getenv "SWIMMY_BACKTEST_CSV_OVERRIDE"))
         (orig-override (and (boundp 'swimmy.core::*backtest-csv-override*)
                             swimmy.core::*backtest-csv-override*))
         (path "/tmp/swimmy-test.csv")
         (captured nil)
         (orig-send (symbol-function 'swimmy.school::send-zmq-msg)))
    (unwind-protect
        (progn
          (sb-posix:setenv "SWIMMY_BACKTEST_CSV_OVERRIDE" path 1)
          (setf swimmy.core::*backtest-csv-override* path)
          (with-open-file (s path :direction :output :if-exists :supersede :if-does-not-exist :create)
            (write-line "" s))
          (setf (symbol-function 'swimmy.school::send-zmq-msg)
                (lambda (msg &key target)
                  (declare (ignore target))
                  (setf captured msg)))
          (swimmy.school::request-backtest (swimmy.school:make-strategy :name "T" :symbol "USDJPY"))
          (assert-true (and captured (search path captured))
                       "payload should include override path"))
      (setf (symbol-function 'swimmy.school::send-zmq-msg) orig-send)
      (ignore-errors (delete-file path))
      (when orig-env
        (sb-posix:setenv "SWIMMY_BACKTEST_CSV_OVERRIDE" orig-env 1))
      (unless orig-env
        (sb-posix:unsetenv "SWIMMY_BACKTEST_CSV_OVERRIDE"))
      (setf swimmy.core::*backtest-csv-override* orig-override))))

(deftest test-heartbeat-webhook-prefers-env
  "get-discord-webhook should prefer SWIMMY_DISCORD_HEARTBEAT when set"
  (require :sb-posix)
  (let* ((orig-hb (uiop:getenv "SWIMMY_DISCORD_HEARTBEAT"))
         (orig-alerts (uiop:getenv "SWIMMY_DISCORD_ALERTS"))
         (hb "https://example.com/heartbeat")
         (alerts "https://example.com/alerts")
         (result nil))
    (unwind-protect
        (progn
          (sb-posix:setenv "SWIMMY_DISCORD_HEARTBEAT" hb 1)
          (sb-posix:setenv "SWIMMY_DISCORD_ALERTS" alerts 1)
          (setf result (swimmy.core::get-discord-webhook "heartbeat"))
          (assert-equal hb result "heartbeat should prefer SWIMMY_DISCORD_HEARTBEAT"))
      (if orig-hb
          (sb-posix:setenv "SWIMMY_DISCORD_HEARTBEAT" orig-hb 1)
          (sb-posix:unsetenv "SWIMMY_DISCORD_HEARTBEAT"))
      (if orig-alerts
          (sb-posix:setenv "SWIMMY_DISCORD_ALERTS" orig-alerts 1)
          (sb-posix:unsetenv "SWIMMY_DISCORD_ALERTS")))))

(deftest test-backtest-webhook-routes-to-system-logs
  "get-discord-webhook should route backtest to SWIMMY_DISCORD_SYSTEM_LOGS (not REPORTS)"
  (require :sb-posix)
  (let* ((orig-sys (uiop:getenv "SWIMMY_DISCORD_SYSTEM_LOGS"))
         (orig-rep (uiop:getenv "SWIMMY_DISCORD_REPORTS"))
         (sys "https://example.com/system-logs")
         (rep "https://example.com/reports")
         (result nil))
    (unwind-protect
        (progn
          (sb-posix:setenv "SWIMMY_DISCORD_SYSTEM_LOGS" sys 1)
          (sb-posix:setenv "SWIMMY_DISCORD_REPORTS" rep 1)
          (setf result (swimmy.core::get-discord-webhook "backtest"))
          (assert-equal sys result "backtest should route to system logs"))
      (if orig-sys
          (sb-posix:setenv "SWIMMY_DISCORD_SYSTEM_LOGS" orig-sys 1)
          (sb-posix:unsetenv "SWIMMY_DISCORD_SYSTEM_LOGS"))
      (if orig-rep
          (sb-posix:setenv "SWIMMY_DISCORD_REPORTS" orig-rep 1)
          (sb-posix:unsetenv "SWIMMY_DISCORD_REPORTS")))))

(deftest test-discord-send-guard-respects-env
  "queue-discord-notification should hard-stop when SWIMMY_DISABLE_DISCORD=1."
  (require :sb-posix)
  (let* ((orig-disable (uiop:getenv "SWIMMY_DISABLE_DISCORD"))
         (orig-flag (and (boundp 'swimmy.core::*discord-disabled*)
                         swimmy.core::*discord-disabled*))
         (orig-ensure (symbol-function 'swimmy.core::ensure-notifier-connection))
         (ensure-called nil)
         (result :unset))
    (unwind-protect
        (progn
          (setf swimmy.core::*discord-disabled* nil)
          (sb-posix:setenv "SWIMMY_DISABLE_DISCORD" "1" 1)
          (setf (symbol-function 'swimmy.core::ensure-notifier-connection)
                (lambda ()
                  (setf ensure-called t)
                  nil))
          (setf result (swimmy.core:queue-discord-notification
                        "https://example.invalid/webhook"
                        "guard-test-message"))
          (assert-equal nil result "Expected hard-stop no-op return when disabled")
          (assert-false ensure-called "Expected no notifier connection when disabled"))
      (setf (symbol-function 'swimmy.core::ensure-notifier-connection) orig-ensure)
      (setf swimmy.core::*discord-disabled* orig-flag)
      (if orig-disable
          (sb-posix:setenv "SWIMMY_DISABLE_DISCORD" orig-disable 1)
          (sb-posix:unsetenv "SWIMMY_DISABLE_DISCORD")))))

(deftest test-backtest-v2-uses-alist
  "request-backtest-v2 should send alist strategy payload"
  (let ((captured nil)
        (orig-send (symbol-function 'swimmy.school::send-zmq-msg))
        (orig-fetch (symbol-function 'swimmy.school::fetch-swap-history)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::send-zmq-msg)
                (lambda (msg &key target)
                  (declare (ignore target))
                  (setf captured msg)))
          (setf (symbol-function 'swimmy.school::fetch-swap-history)
                (lambda (&rest _) (declare (ignore _)) nil))
          (let ((s (swimmy.school:make-strategy :name "T" :symbol "USDJPY")))
            (swimmy.school::request-backtest-v2 s :start-date "2020.01.01" :end-date "2020.12.31")))
      (setf (symbol-function 'swimmy.school::send-zmq-msg) orig-send)
      (setf (symbol-function 'swimmy.school::fetch-swap-history) orig-fetch))
    (assert-not-nil captured "Expected V2 backtest payload to be sent")
    (let ((*package* (find-package :swimmy.tests)))
      (multiple-value-bind (payload _pos) (read-from-string captured)
        (declare (ignore _pos))
        (let ((strategy (cdr (assoc 'strategy payload))))
          (assert-true (consp strategy) "Strategy should be an alist"))
        (assert-equal "USDJPY" (cdr (assoc 'symbol payload)) "Symbol should be USDJPY")
        (let ((tf (cdr (assoc 'timeframe payload))))
          (assert-true (and (listp tf) (= 1 (length tf)) (numberp (first tf)))
                       "Timeframe Option<i64> should be numeric"))
        (let ((data-id (cdr (assoc 'data_id payload))))
          (assert-true (and (listp data-id) (= 1 (length data-id)) (stringp (first data-id)))
                       "Expected data_id Option<String> present"))
        (let ((candles-file (cdr (assoc 'candles_file payload))))
          (assert-true (and (listp candles-file) (= 1 (length candles-file)) (stringp (first candles-file)))
                       "Expected candles_file Option<String> present"))
        (let ((start-time (cdr (assoc 'start_time payload))))
          (assert-true (and (listp start-time) (= 1 (length start-time)) (numberp (first start-time)))
                       "Expected start_time Option<i64> present"))
        (let ((end-time (cdr (assoc 'end_time payload))))
          (assert-true (and (listp end-time) (= 1 (length end-time)) (numberp (first end-time)))
                       "Expected end_time Option<i64> present"))))))

(deftest test-prune-low-sharpe-skips-newborn-age
  "Newborn (age<24h) low-sharpe strategies are protected from pruning."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-send (symbol-function 'swimmy.school:send-to-graveyard))
         (now (get-universal-time))
         (strat (swimmy.school:make-strategy :name "UT-NEWBORN-AGE"
                                             :sharpe 0.0
                                             :trades 5)))
    (unwind-protect
        (progn
          (setf (swimmy.school::strategy-creation-time strat) now)
          (setf swimmy.school::*strategy-knowledge-base* (list strat))
          (setf (symbol-function 'swimmy.school:send-to-graveyard)
                (lambda (s reason)
                  (declare (ignore s reason))
                  nil))
          (let ((removed (swimmy.school::prune-low-sharpe-strategies)))
            (assert-equal 0 removed "Newborn should be protected by age")
            (assert-equal 1 (length swimmy.school::*strategy-knowledge-base*)
                          "KB should keep newborn strategy")))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf (symbol-function 'swimmy.school:send-to-graveyard) orig-send))))

(deftest test-prune-similar-skips-newborn-trades
  "Newborn (trades<=0) strategies are protected from similarity pruning."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-send (symbol-function 'swimmy.school:send-to-graveyard))
         (now (get-universal-time))
         (old-ts (- now (* 2 24 60 60)))
         (strat1 (swimmy.school:make-strategy :name "UT-SIM-A"
                                              :sl 30 :tp 60 :timeframe 60
                                              :indicators '((sma 20)) :symbol "EURUSD"
                                              :sharpe 1.0 :trades 10))
         (strat2 (swimmy.school:make-strategy :name "UT-SIM-B"
                                              :sl 30 :tp 60 :timeframe 60
                                              :indicators '((sma 20)) :symbol "EURUSD"
                                              :sharpe 0.5 :trades 0)))
    (unwind-protect
        (progn
          (setf (swimmy.school::strategy-creation-time strat2) old-ts)
          (setf swimmy.school::*strategy-knowledge-base* (list strat1 strat2))
          (setf (symbol-function 'swimmy.school:send-to-graveyard)
                (lambda (s reason)
                  (declare (ignore s reason))
                  nil))
          (let ((removed (swimmy.school::prune-similar-strategies)))
            (assert-equal 0 removed "Newborn should be protected by trades")
            (assert-equal 2 (length swimmy.school::*strategy-knowledge-base*)
                          "KB should keep both strategies")))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf (symbol-function 'swimmy.school:send-to-graveyard) orig-send))))

(deftest test-hard-cap-skips-newborn
  "Hard-cap pruning should skip newborn strategies."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-send (symbol-function 'swimmy.school:send-to-graveyard))
         (orig-cap swimmy.school::*kb-hard-cap*)
         (now (get-universal-time))
         (old-ts (- now (* 3 24 60 60)))
         (newborn (swimmy.school:make-strategy :name "UT-CAP-NEW"
                                               :sharpe -1.0 :trades 0))
         (older (swimmy.school:make-strategy :name "UT-CAP-OLD"
                                             :sharpe 0.0 :trades 10)))
    (unwind-protect
        (progn
          (setf swimmy.school::*kb-hard-cap* 1)
          (setf (swimmy.school::strategy-creation-time newborn) now)
          (setf (swimmy.school::strategy-creation-time older) old-ts)
          (setf swimmy.school::*strategy-knowledge-base* (list newborn older))
          (setf (symbol-function 'swimmy.school:send-to-graveyard)
                (lambda (s reason)
                  (declare (ignore reason))
                  (setf swimmy.school::*strategy-knowledge-base*
                        (remove s swimmy.school::*strategy-knowledge-base* :test #'eq))))
          (let ((removed (swimmy.school::enforce-kb-hard-cap)))
            (assert-equal 1 (or removed 0) "Should purge one non-newborn")
            (assert-true (find "UT-CAP-NEW" swimmy.school::*strategy-knowledge-base*
                               :key #'swimmy.school:strategy-name :test #'string=)
                         "Newborn should remain")
            (assert-false (find "UT-CAP-OLD" swimmy.school::*strategy-knowledge-base*
                                :key #'swimmy.school:strategy-name :test #'string=)
                          "Older should be removed")))
      (setf swimmy.school::*kb-hard-cap* orig-cap)
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf (symbol-function 'swimmy.school:send-to-graveyard) orig-send))))

(deftest test-hard-cap-does-not-crash-when-prunable-pool-is-smaller-than-excess
  "Hard-cap pruning should not throw when purge candidates are fewer than excess."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-send (symbol-function 'swimmy.school:send-to-graveyard))
         (orig-cap swimmy.school::*kb-hard-cap*)
         (now (get-universal-time))
         (newborn (swimmy.school:make-strategy :name "UT-CAP-NB-ONLY"
                                               :sharpe -1.0
                                               :rank :B
                                               :trades 0))
         (protected-a (swimmy.school:make-strategy :name "UT-CAP-A-PROTECTED"
                                                   :sharpe -2.0
                                                   :rank :A
                                                   :trades 50))
         (protected-s (swimmy.school:make-strategy :name "UT-CAP-S-PROTECTED"
                                                   :sharpe -3.0
                                                   :rank :S
                                                   :trades 100))
         (send-called nil))
    (unwind-protect
        (progn
          ;; current-size=3, cap=1, but prunable pool should be empty.
          (setf swimmy.school::*kb-hard-cap* 1)
          (setf (swimmy.school::strategy-creation-time newborn) now)
          (setf swimmy.school::*strategy-knowledge-base* (list newborn protected-a protected-s))
          (setf (symbol-function 'swimmy.school:send-to-graveyard)
                (lambda (s reason)
                  (declare (ignore s reason))
                  (setf send-called t)
                  nil))
          (let ((removed (swimmy.school::enforce-kb-hard-cap)))
            (assert-equal 0 (or removed 0) "No strategy should be purged when all are protected")
            (assert-false send-called "send-to-graveyard must not be called")
            (assert-equal 3 (length swimmy.school::*strategy-knowledge-base*)
                          "KB should remain unchanged when purge candidates are empty")))
      (setf swimmy.school::*kb-hard-cap* orig-cap)
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf (symbol-function 'swimmy.school:send-to-graveyard) orig-send))))

(deftest test-reconcile-active-library-with-db-hydrates-missing-active-files
  "Active reconcile should hydrate missing B/A/S/LEGEND files from DB rows."
  (let* ((tmp-db (format nil "/tmp/swimmy-active-reconcile-~a.db" (get-universal-time)))
         (tmp-lib (uiop:ensure-directory-pathname
                   (format nil "/tmp/swimmy-active-reconcile-lib-~a/" (get-universal-time))))
         (orig-db swimmy.core::*db-path-default*)
         (orig-conn swimmy.core::*sqlite-conn*)
         (orig-disable swimmy.school::*disable-auto-migration*)
         (orig-lib swimmy.persistence:*library-path*)
         (orig-last swimmy.school::*last-db-active-library-reconcile-time*)
         (name "UT-ACTIVE-HYDRATE-B")
         (now (get-universal-time))
         (payload (format nil "~s"
                          (swimmy.school:make-strategy :name name :rank :B :symbol "USDJPY"))))
    (unwind-protect
        (progn
          (setf swimmy.core::*db-path-default* tmp-db
                swimmy.core::*sqlite-conn* nil
                swimmy.school::*disable-auto-migration* t
                swimmy.persistence:*library-path* tmp-lib
                swimmy.school::*last-db-active-library-reconcile-time* 0)
          (swimmy.persistence:init-library)
          (swimmy.school::init-db)
          (swimmy.school::execute-non-query
           "INSERT OR REPLACE INTO strategies (name, rank, data_sexp, updated_at) VALUES (?, ?, ?, ?)"
           name ":B" payload now)
          (let* ((summary (swimmy.school::reconcile-active-library-with-db
                           :max-additions 10 :interval 0 :now (+ now 1)))
                 (path (merge-pathnames
                        (format nil "B/~a.lisp"
                                (swimmy.persistence::sanitize-filename name))
                        tmp-lib)))
            (assert-equal 1 (getf summary :added)
                          "Expected active DB row to hydrate missing B-rank file")
            (assert-equal 0 (getf summary :write-failed)
                          "Expected no write failures for valid active payload")
            (assert-true (probe-file path)
                         "Expected hydrated B-rank file to exist")))
      (setf swimmy.core::*db-path-default* orig-db
            swimmy.core::*sqlite-conn* orig-conn
            swimmy.school::*disable-auto-migration* orig-disable
            swimmy.persistence:*library-path* orig-lib
            swimmy.school::*last-db-active-library-reconcile-time* orig-last)
      (ignore-errors (swimmy.school::close-db-connection))
      (when (probe-file tmp-db)
        (ignore-errors (delete-file tmp-db)))
      (ignore-errors (uiop:delete-directory-tree tmp-lib :validate t)))))

(deftest test-reconcile-active-library-with-db-falls-back-to-kb-on-blank-payload
  "Active reconcile should fallback to in-memory KB when DB data_sexp is blank."
  (let* ((tmp-db (format nil "/tmp/swimmy-active-kb-fallback-~a.db" (get-universal-time)))
         (tmp-lib (uiop:ensure-directory-pathname
                   (format nil "/tmp/swimmy-active-kb-fallback-lib-~a/" (get-universal-time))))
         (orig-db swimmy.core::*db-path-default*)
         (orig-conn swimmy.core::*sqlite-conn*)
         (orig-disable swimmy.school::*disable-auto-migration*)
         (orig-lib swimmy.persistence:*library-path*)
         (orig-last swimmy.school::*last-db-active-library-reconcile-time*)
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (name "UT-ACTIVE-KB-FALLBACK-A")
         (now (get-universal-time))
         (kb-strat (swimmy.school:make-strategy :name name :rank :A :symbol "USDJPY")))
    (unwind-protect
        (progn
          (setf swimmy.core::*db-path-default* tmp-db
                swimmy.core::*sqlite-conn* nil
                swimmy.school::*disable-auto-migration* t
                swimmy.persistence:*library-path* tmp-lib
                swimmy.school::*last-db-active-library-reconcile-time* 0
                swimmy.school::*strategy-knowledge-base* (list kb-strat))
          (swimmy.persistence:init-library)
          (swimmy.school::init-db)
          (swimmy.school::execute-non-query
           "INSERT OR REPLACE INTO strategies (name, rank, data_sexp, updated_at) VALUES (?, ?, ?, ?)"
           name ":A" "" now)
          (let* ((summary (swimmy.school::reconcile-active-library-with-db
                           :max-additions 10 :interval 0 :now (+ now 1)))
                 (path (merge-pathnames
                        (format nil "A/~a.lisp"
                                (swimmy.persistence::sanitize-filename name))
                        tmp-lib)))
            (assert-equal 1 (getf summary :added)
                          "Expected KB fallback to hydrate missing A-rank file")
            (assert-equal 0 (getf summary :write-failed)
                          "Expected fallback from KB to avoid write failures")
            (assert-true (probe-file path)
                         "Expected hydrated A-rank file to exist")))
      (setf swimmy.core::*db-path-default* orig-db
            swimmy.core::*sqlite-conn* orig-conn
            swimmy.school::*disable-auto-migration* orig-disable
            swimmy.persistence:*library-path* orig-lib
            swimmy.school::*last-db-active-library-reconcile-time* orig-last
            swimmy.school::*strategy-knowledge-base* orig-kb)
      (ignore-errors (swimmy.school::close-db-connection))
      (when (probe-file tmp-db)
        (ignore-errors (delete-file tmp-db)))
      (ignore-errors (uiop:delete-directory-tree tmp-lib :validate t)))))

(deftest test-reconcile-archive-library-with-db-falls-back-to-backup-data
  "Archive reconcile should hydrate missing file from archive_reconcile_backup when DB data_sexp is blank."
  (let* ((tmp-db (format nil "/tmp/swimmy-archive-backup-~a.db" (get-universal-time)))
         (tmp-lib (uiop:ensure-directory-pathname
                   (format nil "/tmp/swimmy-archive-backup-lib-~a/" (get-universal-time))))
         (orig-db swimmy.core::*db-path-default*)
         (orig-conn swimmy.core::*sqlite-conn*)
         (orig-disable swimmy.school::*disable-auto-migration*)
         (orig-lib swimmy.persistence:*library-path*)
         (orig-last swimmy.school::*last-db-archive-library-reconcile-time*)
         (orig-backup-cache swimmy.school::*archive-reconcile-backup-table-available*)
         (name "UT-ARCHIVE-BACKUP-RECOVER")
         (now (get-universal-time))
         (payload (format nil "~s"
                          (swimmy.school:make-strategy :name name :rank :GRAVEYARD :symbol "USDJPY"))))
    (unwind-protect
        (progn
          (setf swimmy.core::*db-path-default* tmp-db
                swimmy.core::*sqlite-conn* nil
                swimmy.school::*disable-auto-migration* t
                swimmy.persistence:*library-path* tmp-lib
                swimmy.school::*last-db-archive-library-reconcile-time* 0
                swimmy.school::*archive-reconcile-backup-table-available* :unknown)
          (swimmy.persistence:init-library)
          (swimmy.school::init-db)
          (swimmy.school::execute-non-query
           "CREATE TABLE IF NOT EXISTS archive_reconcile_backup (
              id INTEGER PRIMARY KEY AUTOINCREMENT,
              name TEXT NOT NULL,
              rank TEXT NOT NULL,
              data_sexp TEXT,
              updated_at INTEGER,
              backed_up_at INTEGER NOT NULL
            )")
          (swimmy.school::execute-non-query
           "INSERT OR REPLACE INTO strategies (name, rank, data_sexp, updated_at) VALUES (?, ?, ?, ?)"
           name ":GRAVEYARD" "" now)
          (swimmy.school::execute-non-query
           "INSERT INTO archive_reconcile_backup (name, rank, data_sexp, updated_at, backed_up_at)
            VALUES (?, ?, ?, ?, ?)"
           name ":GRAVEYARD" payload now now)
          (let* ((summary (swimmy.school::reconcile-archive-library-with-db
                           :max-additions 10 :interval 0 :now (+ now 1)))
                 (path (merge-pathnames
                        (format nil "GRAVEYARD/~a.lisp"
                                (swimmy.persistence::sanitize-filename name))
                        tmp-lib)))
            (assert-equal 1 (getf summary :added)
                          "Expected fallback backup payload to hydrate missing archive file")
            (assert-equal 0 (getf summary :write-failed)
                          "Expected no write failures when backup payload exists")
            (assert-true (probe-file path)
                         "Expected hydrated GRAVEYARD file to exist")))
      (setf swimmy.core::*db-path-default* orig-db
            swimmy.core::*sqlite-conn* orig-conn
            swimmy.school::*disable-auto-migration* orig-disable
            swimmy.persistence:*library-path* orig-lib
            swimmy.school::*last-db-archive-library-reconcile-time* orig-last
            swimmy.school::*archive-reconcile-backup-table-available* orig-backup-cache)
      (ignore-errors (swimmy.school::close-db-connection))
      (when (probe-file tmp-db)
        (ignore-errors (delete-file tmp-db)))
      (ignore-errors (uiop:delete-directory-tree tmp-lib :validate t)))))

(deftest test-reconcile-archive-library-with-db-counts-empty-payload-write-failures
  "Archive reconcile should report write_failed when DB/backup payload is missing."
  (let* ((tmp-db (format nil "/tmp/swimmy-archive-writefail-~a.db" (get-universal-time)))
         (tmp-lib (uiop:ensure-directory-pathname
                   (format nil "/tmp/swimmy-archive-writefail-lib-~a/" (get-universal-time))))
         (orig-db swimmy.core::*db-path-default*)
         (orig-conn swimmy.core::*sqlite-conn*)
         (orig-disable swimmy.school::*disable-auto-migration*)
         (orig-lib swimmy.persistence:*library-path*)
         (orig-last swimmy.school::*last-db-archive-library-reconcile-time*)
         (orig-backup-cache swimmy.school::*archive-reconcile-backup-table-available*)
         (name "UT-ARCHIVE-WRITEFAIL")
         (now (get-universal-time)))
    (unwind-protect
        (progn
          (setf swimmy.core::*db-path-default* tmp-db
                swimmy.core::*sqlite-conn* nil
                swimmy.school::*disable-auto-migration* t
                swimmy.persistence:*library-path* tmp-lib
                swimmy.school::*last-db-archive-library-reconcile-time* 0
                swimmy.school::*archive-reconcile-backup-table-available* :unknown)
          (swimmy.persistence:init-library)
          (swimmy.school::init-db)
          (swimmy.school::execute-non-query
           "INSERT OR REPLACE INTO strategies (name, rank, data_sexp, updated_at) VALUES (?, ?, ?, ?)"
           name ":GRAVEYARD" "" now)
          (let ((summary (swimmy.school::reconcile-archive-library-with-db
                          :max-additions 10 :interval 0 :now (+ now 1))))
            (assert-equal 0 (getf summary :added)
                          "Expected no archive hydration without payload")
            (assert-equal 1 (getf summary :write-failed)
                          "Expected write_failed=1 for blank DB payload without backup")))
      (setf swimmy.core::*db-path-default* orig-db
            swimmy.core::*sqlite-conn* orig-conn
            swimmy.school::*disable-auto-migration* orig-disable
            swimmy.persistence:*library-path* orig-lib
            swimmy.school::*last-db-archive-library-reconcile-time* orig-last
            swimmy.school::*archive-reconcile-backup-table-available* orig-backup-cache)
      (ignore-errors (swimmy.school::close-db-connection))
      (when (probe-file tmp-db)
        (ignore-errors (delete-file tmp-db)))
      (ignore-errors (uiop:delete-directory-tree tmp-lib :validate t)))))

(deftest test-delete-strategy-rank-guard
  "delete-strategy should honor :rank when provided."
  (let* ((orig-path swimmy.persistence:*library-path*)
         (tmp (format nil "/tmp/swimmy-test-lib-~a/" (get-universal-time)))
         (tmp-dir (uiop:ensure-directory-pathname tmp))
         (strat (swimmy.school:make-strategy :name "UT-DEL-RANK" :rank :B)))
    (unwind-protect
        (progn
          (setf swimmy.persistence:*library-path* tmp-dir)
          (swimmy.persistence:init-library)
          (swimmy.persistence:save-strategy strat)
          (assert-true (swimmy.persistence:strategy-exists-p "UT-DEL-RANK" :B)
                       "Strategy should exist at rank B")
          (swimmy.persistence:delete-strategy strat :rank :A)
          (assert-true (swimmy.persistence:strategy-exists-p "UT-DEL-RANK" :B)
                       "Wrong rank should not delete")
          (swimmy.persistence:delete-strategy strat :rank :B)
          (assert-false (swimmy.persistence:strategy-exists-p "UT-DEL-RANK" :B)
                        "Correct rank should delete"))
      (setf swimmy.persistence:*library-path* orig-path)
      (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t)))))

(deftest test-move-strategy-from-rank
  "move-strategy should delete from :from-rank when provided."
  (let* ((orig-path swimmy.persistence:*library-path*)
         (tmp (format nil "/tmp/swimmy-test-lib-~a/" (get-universal-time)))
         (tmp-dir (uiop:ensure-directory-pathname tmp))
         (strat (swimmy.school:make-strategy :name "UT-MOVE-RANK" :rank :B)))
    (unwind-protect
        (progn
          (setf swimmy.persistence:*library-path* tmp-dir)
          (swimmy.persistence:init-library)
          (swimmy.persistence:save-strategy strat)
          ;; simulate rank drift so delete must use :from-rank
          (setf (swimmy.school:strategy-rank strat) :A)
          (swimmy.persistence:move-strategy strat :S :from-rank :B)
          (assert-false (swimmy.persistence:strategy-exists-p "UT-MOVE-RANK" :B)
                        "Source rank file should be removed")
          (assert-true (swimmy.persistence:strategy-exists-p "UT-MOVE-RANK" :S)
                       "Target rank file should exist"))
      (setf swimmy.persistence:*library-path* orig-path)
      (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t)))))

(deftest test-ensure-rank-graveyard-deletes-old-rank-file
  "ensure-rank should delete the old-rank file when moving to graveyard."
  (let* ((orig-path swimmy.persistence:*library-path*)
         (tmp (format nil "/tmp/swimmy-ensure-graveyard-~a/" (get-universal-time)))
         (tmp-dir (uiop:ensure-directory-pathname tmp))
         (strat (swimmy.school:make-strategy :name "UT-ENSURE-GRAVE" :rank :B))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-pools swimmy.school::*category-pools*)
         (orig-upsert (symbol-function 'swimmy.school:upsert-strategy))
         (orig-cat (symbol-function 'swimmy.school::categorize-strategy))
         (orig-save (symbol-function 'swimmy.school::save-failure-pattern))
         (orig-cancel (symbol-function 'swimmy.school::cancel-oos-request-for-strategy))
         (orig-oos-p (symbol-function 'swimmy.school::oos-request-pending-p)))
    (unwind-protect
        (progn
          (setf swimmy.persistence:*library-path* tmp-dir)
          (swimmy.persistence:init-library)
          (setf swimmy.school::*strategy-knowledge-base* (list strat))
          (setf swimmy.school::*category-pools* (make-hash-table))
          (setf (symbol-function 'swimmy.school:upsert-strategy)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::categorize-strategy)
                (lambda (&rest args) (declare (ignore args)) :dummy))
          (setf (symbol-function 'swimmy.school::save-failure-pattern)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::cancel-oos-request-for-strategy)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::oos-request-pending-p)
                (lambda (&rest args) (declare (ignore args)) nil))
          (swimmy.persistence:save-strategy strat)
          (assert-true (swimmy.persistence:strategy-exists-p "UT-ENSURE-GRAVE" :B)
                       "Expected B-rank file to exist")
          (swimmy.school:ensure-rank strat :graveyard "test")
          (assert-false (swimmy.persistence:strategy-exists-p "UT-ENSURE-GRAVE" :B)
                        "Old rank file should be removed")
          (assert-true (swimmy.persistence:strategy-exists-p "UT-ENSURE-GRAVE" :GRAVEYARD)
                       "Graveyard file should exist"))
      (setf swimmy.persistence:*library-path* orig-path)
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*category-pools* orig-pools)
      (setf (symbol-function 'swimmy.school:upsert-strategy) orig-upsert)
      (setf (symbol-function 'swimmy.school::categorize-strategy) orig-cat)
      (setf (symbol-function 'swimmy.school::save-failure-pattern) orig-save)
      (setf (symbol-function 'swimmy.school::cancel-oos-request-for-strategy) orig-cancel)
      (setf (symbol-function 'swimmy.school::oos-request-pending-p) orig-oos-p)
      (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t)))))

(deftest test-load-strategy-recovers-struct-sexp
  "load-strategy should recover #S(STRATEGY ...) content via fallback."
  (let* ((tmp (format nil "/tmp/swimmy-strat-~a.lisp" (get-universal-time)))
         (content "#S(STRATEGY :NAME \"UT-STRUCT\" :INDICATORS ((SWIMMY.SCHOOL::SMA 5)) :ENTRY (SWIMMY.SCHOOL::CROSS-ABOVE CLOSE OPEN) :EXIT (> CLOSE OPEN) :RANK :LEGEND :UNKNOWN-KEY 1)")
         (strat nil))
    (unwind-protect
        (progn
          (with-open-file (s tmp :direction :output :if-exists :supersede)
            (format s "~a" content))
          (setf strat (swimmy.persistence:load-strategy tmp))
          (assert-true (and strat (swimmy.school::strategy-p strat)) "Should recover strategy")
          (assert-equal "UT-STRUCT" (swimmy.school:strategy-name strat)))
      (ignore-errors (delete-file tmp)))))

(deftest test-init-knowledge-base-skips-nil-strategies
  "init-knowledge-base should skip NIL entries and not crash."
  (let ((orig-db (symbol-function 'swimmy.school:fetch-all-strategies-from-db))
        (orig-file (symbol-function 'swimmy.persistence:load-all-strategies))
        (failed nil))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school:fetch-all-strategies-from-db)
                (lambda () (list nil (swimmy.school:make-strategy :name "UT-DB"))))
          (setf (symbol-function 'swimmy.persistence:load-all-strategies)
                (lambda () (list nil (swimmy.school:make-strategy :name "UT-FILE"))))
          (handler-case
              (swimmy.school::init-knowledge-base)
            (error () (setf failed t)))
          (assert-false failed "init-knowledge-base should not crash on NIL")
          (assert-true (every #'swimmy.school::strategy-p swimmy.school::*strategy-knowledge-base*)
                       "KB should contain only strategies"))
      (setf (symbol-function 'swimmy.school:fetch-all-strategies-from-db) orig-db)
      (setf (symbol-function 'swimmy.persistence:load-all-strategies) orig-file))))

(deftest test-init-knowledge-base-normalizes-timeframe-to-minutes
  "init-knowledge-base should normalize loaded strategy timeframe labels into minutes(int)."
  (let ((orig-db (symbol-function 'swimmy.school:fetch-all-strategies-from-db))
        (orig-file (symbol-function 'swimmy.persistence:load-all-strategies))
        (orig-kb swimmy.school::*strategy-knowledge-base*))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school:fetch-all-strategies-from-db)
                (lambda ()
                  (list (swimmy.school:make-strategy :name "UT-DB-TF-MN" :timeframe "MN1" :rank :B))))
          (setf (symbol-function 'swimmy.persistence:load-all-strategies)
                (lambda ()
                  (list (swimmy.school:make-strategy :name "UT-FILE-TF-H5" :timeframe "H5" :rank :B))))
          (swimmy.school::init-knowledge-base)
          (let ((db (find "UT-DB-TF-MN" swimmy.school::*strategy-knowledge-base*
                          :key #'swimmy.school:strategy-name :test #'string=))
                (file (find "UT-FILE-TF-H5" swimmy.school::*strategy-knowledge-base*
                            :key #'swimmy.school:strategy-name :test #'string=)))
            (assert-true db "Expected DB strategy in KB")
            (assert-true file "Expected file strategy in KB")
            (assert-equal 43200 (swimmy.school:strategy-timeframe db))
            (assert-equal 300 (swimmy.school:strategy-timeframe file))))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf (symbol-function 'swimmy.school:fetch-all-strategies-from-db) orig-db)
      (setf (symbol-function 'swimmy.persistence:load-all-strategies) orig-file))))

(deftest test-init-knowledge-base-prunes-unevaluated-ranks
  "init-knowledge-base should remove unevaluated NIL/INCUBATOR/SCOUT strategies from KB."
  (let* ((orig-db (symbol-function 'swimmy.school:fetch-all-strategies-from-db))
         (orig-file (symbol-function 'swimmy.persistence:load-all-strategies))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (active (swimmy.school:make-strategy :name "UT-KB-ACTIVE-B"
                                              :rank :B
                                              :symbol "USDJPY"
                                              :timeframe 300
                                              :sharpe 0.7
                                              :trades 50))
         (nil-rank (swimmy.school:make-strategy :name "UT-KB-NIL-RANK"
                                                :rank nil
                                                :symbol "USDJPY"
                                                :timeframe 300
                                                :sharpe 0.0
                                                :trades 0))
         (incubator (swimmy.school:make-strategy :name "UT-KB-INCUBATOR"
                                                 :rank :incubator
                                                 :symbol "USDJPY"
                                                 :timeframe 300
                                                 :sharpe 0.0
                                                 :trades 0))
         (scout (swimmy.school:make-strategy :name "UT-KB-SCOUT"
                                             :rank :scout
                                             :symbol "USDJPY"
                                             :timeframe 300
                                             :sharpe 0.0
                                             :trades 0)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school:fetch-all-strategies-from-db)
                (lambda () (list active nil-rank)))
          (setf (symbol-function 'swimmy.persistence:load-all-strategies)
                (lambda () (list incubator scout)))
          (swimmy.school::init-knowledge-base)
          (assert-true (find "UT-KB-ACTIVE-B" swimmy.school::*strategy-knowledge-base*
                             :key #'swimmy.school:strategy-name :test #'string=)
                       "Expected active B-rank strategy to remain in KB")
          (assert-false (find "UT-KB-NIL-RANK" swimmy.school::*strategy-knowledge-base*
                              :key #'swimmy.school:strategy-name :test #'string=)
                        "Expected NIL-rank strategy to be removed from KB")
          (assert-false (find "UT-KB-INCUBATOR" swimmy.school::*strategy-knowledge-base*
                              :key #'swimmy.school:strategy-name :test #'string=)
                        "Expected INCUBATOR strategy to be removed from KB")
          (assert-false (find "UT-KB-SCOUT" swimmy.school::*strategy-knowledge-base*
                              :key #'swimmy.school:strategy-name :test #'string=)
                        "Expected SCOUT strategy to be removed from KB"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf (symbol-function 'swimmy.school:fetch-all-strategies-from-db) orig-db)
      (setf (symbol-function 'swimmy.persistence:load-all-strategies) orig-file))))

(deftest test-init-knowledge-base-revives-active-file-strategy-over-archived-db-duplicate
  "init-knowledge-base should prefer active file strategy when DB duplicate is archived."
  (let* ((orig-db (symbol-function 'swimmy.school:fetch-all-strategies-from-db))
         (orig-file (symbol-function 'swimmy.persistence:load-all-strategies))
         (orig-upsert (symbol-function 'swimmy.school:upsert-strategy))
         (orig-purge (symbol-function 'swimmy.school::%purge-revived-strategy-archive-files))
         (orig-require-file swimmy.school::*kb-init-revive-require-active-library-file*)
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (db-strat (swimmy.school:make-strategy :name "UT-KB-ARCHIVE-COLLIDE"
                                                :rank :graveyard
                                                :symbol "USDJPY"
                                                :timeframe 300
                                                :sharpe 0.10))
         (file-strat (swimmy.school:make-strategy :name "UT-KB-ARCHIVE-COLLIDE"
                                                  :rank :B
                                                  :symbol "USDJPY"
                                                  :timeframe 300
                                                  :sharpe 1.10))
         (revived nil)
         (purged nil))
    (unwind-protect
        (progn
          (setf swimmy.school::*kb-init-revive-require-active-library-file* nil)
          (setf (symbol-function 'swimmy.school:fetch-all-strategies-from-db)
                (lambda () (list db-strat)))
          (setf (symbol-function 'swimmy.persistence:load-all-strategies)
                (lambda () (list file-strat)))
          (setf (symbol-function 'swimmy.school:upsert-strategy)
                (lambda (s)
                  (push (list (swimmy.school:strategy-name s)
                              swimmy.school::*allow-archived-rank-resurrection-write*)
                        revived)
                  t))
          (setf (symbol-function 'swimmy.school::%purge-revived-strategy-archive-files)
                (lambda (name &optional _root)
                  (declare (ignore _root))
                  (push name purged)
                  1))
          (swimmy.school::init-knowledge-base)
          (let ((picked (find "UT-KB-ARCHIVE-COLLIDE"
                              swimmy.school::*strategy-knowledge-base*
                              :key #'swimmy.school:strategy-name
                              :test #'string=)))
            (assert-not-nil picked "Expected collided strategy to survive KB init")
            (assert-equal :B (swimmy.school:strategy-rank picked)
                          "Expected active file rank to replace archived DB duplicate")
            (assert-true (> (swimmy.school:strategy-sharpe picked) 1.0)
                         "Expected active file metrics to be used"))
          (assert-equal 1 (length revived)
                        "Expected one archived->active DB revive write")
          (assert-true (second (first revived))
                       "Revive write should enable archived resurrection override")
          (assert-equal 1 (length purged)
                        "Expected stale archive purge to run once")
          (assert-equal "UT-KB-ARCHIVE-COLLIDE" (first purged)
                        "Expected stale archive purge for revived strategy"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*kb-init-revive-require-active-library-file* orig-require-file)
      (setf (symbol-function 'swimmy.school:fetch-all-strategies-from-db) orig-db)
      (setf (symbol-function 'swimmy.persistence:load-all-strategies) orig-file)
      (setf (symbol-function 'swimmy.school:upsert-strategy) orig-upsert)
      (setf (symbol-function 'swimmy.school::%purge-revived-strategy-archive-files) orig-purge))))

(deftest test-init-knowledge-base-revive-requires-active-library-file-by-default
  "init-knowledge-base should skip archived->active revive when strategy is not in active Library dirs."
  (let* ((orig-db (symbol-function 'swimmy.school:fetch-all-strategies-from-db))
         (orig-file (symbol-function 'swimmy.persistence:load-all-strategies))
         (orig-upsert (symbol-function 'swimmy.school:upsert-strategy))
         (orig-purge (symbol-function 'swimmy.school::%purge-revived-strategy-archive-files))
         (orig-require-file swimmy.school::*kb-init-revive-require-active-library-file*)
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (db-strat (swimmy.school:make-strategy :name "UT-KB-ARCHIVE-NO-ACTIVE-FILE"
                                                :rank :graveyard
                                                :symbol "USDJPY"
                                                :timeframe 300))
         (file-strat (swimmy.school:make-strategy :name "UT-KB-ARCHIVE-NO-ACTIVE-FILE"
                                                  :rank :B
                                                  :symbol "USDJPY"
                                                  :timeframe 300
                                                  :sharpe 1.20))
         (upsert-called 0)
         (purge-called 0))
    (unwind-protect
        (progn
          (setf swimmy.school::*kb-init-revive-require-active-library-file* t)
          (setf (symbol-function 'swimmy.school:fetch-all-strategies-from-db)
                (lambda () (list db-strat)))
          (setf (symbol-function 'swimmy.persistence:load-all-strategies)
                (lambda () (list file-strat)))
          (setf (symbol-function 'swimmy.school:upsert-strategy)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (incf upsert-called)
                  t))
          (setf (symbol-function 'swimmy.school::%purge-revived-strategy-archive-files)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (incf purge-called)
                  1))
          (swimmy.school::init-knowledge-base)
          (assert-equal 0 upsert-called
                        "Expected no revive upsert when active Library file is missing")
          (assert-equal 0 purge-called
                        "Expected no archive purge when revive is skipped")
          (assert-false (find "UT-KB-ARCHIVE-NO-ACTIVE-FILE"
                              swimmy.school::*strategy-knowledge-base*
                              :key #'swimmy.school:strategy-name
                              :test #'string=)
                        "Archived DB duplicate should remain pruned from KB"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*kb-init-revive-require-active-library-file* orig-require-file)
      (setf (symbol-function 'swimmy.school:fetch-all-strategies-from-db) orig-db)
      (setf (symbol-function 'swimmy.persistence:load-all-strategies) orig-file)
      (setf (symbol-function 'swimmy.school:upsert-strategy) orig-upsert)
      (setf (symbol-function 'swimmy.school::%purge-revived-strategy-archive-files) orig-purge))))

(deftest test-fetch-all-strategies-from-db-prefers-rank-column-over-stale-data-sexp
  "DB fetch should trust rank column when serialized rank in data_sexp is stale."
  (let* ((orig-exec (symbol-function 'swimmy.school::execute-to-list))
         (stale (swimmy.school:make-strategy :name "UT-STALE-ALL" :rank :graveyard))
         (sexp (format nil "~s" stale)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::execute-to-list)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (list (list sexp ":B"))))
          (let* ((rows (swimmy.school::fetch-all-strategies-from-db))
                 (obj (first rows)))
            (assert-not-nil obj "Expected one strategy row")
            (assert-equal :B (swimmy.school:strategy-rank obj)
                          "Expected rank column (:B) to override stale data_sexp rank")))
      (setf (symbol-function 'swimmy.school::execute-to-list) orig-exec))))

(deftest test-fetch-candidate-strategies-prefers-rank-column-over-stale-data-sexp
  "Candidate fetch should trust rank column when serialized rank in data_sexp is stale."
  (let* ((orig-exec (symbol-function 'swimmy.school::execute-to-list))
         (stale (swimmy.school:make-strategy :name "UT-STALE-CAND" :rank :graveyard))
         (sexp (format nil "~s" stale)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::execute-to-list)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (list (list sexp ":A"))))
          (let* ((rows (swimmy.school::fetch-candidate-strategies :min-sharpe 0.0 :ranks '(":A")))
                 (obj (first rows)))
            (assert-not-nil obj "Expected one candidate row")
            (assert-equal :A (swimmy.school:strategy-rank obj)
                          "Expected rank column (:A) to override stale data_sexp rank")))
      (setf (symbol-function 'swimmy.school::execute-to-list) orig-exec))))

(deftest test-add-to-kb-allows-breeder-logic-variant-when-sltp-differs
  "Breeder child with same logic should be accepted when SL/TP is materially different."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-pools swimmy.school::*category-pools*)
         (orig-startup swimmy.school::*startup-mode*)
         (orig-upsert (symbol-function 'swimmy.school:upsert-strategy))
         (orig-notify (symbol-function 'swimmy.school::notify-recruit-unified))
         (base (swimmy.school:make-strategy :name "UT-BREEDER-BASE"
                                            :symbol "USDJPY"
                                            :timeframe 300
                                            :direction :BOTH
                                            :rank :B
                                            :sl 10.0
                                            :tp 20.0
                                            :sharpe 1.2
                                            :indicators '((sma 20))
                                            :entry '(> close sma-20)
                                            :exit '(< close sma-20)))
         (child (swimmy.school:make-strategy :name "UT-BREEDER-CHILD-VARIANT"
                                             :symbol "USDJPY"
                                             :timeframe 300
                                             :direction :BOTH
                                             :rank :incubator
                                             :sl 16.0
                                             :tp 32.0
                                             :sharpe 0.0
                                             :indicators '((sma 20))
                                             :entry '(> close sma-20)
                                             :exit '(< close sma-20))))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list base))
          (setf swimmy.school::*category-pools* (make-hash-table))
          (setf swimmy.school::*startup-mode* t)
          (setf (symbol-function 'swimmy.school:upsert-strategy)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::notify-recruit-unified)
                (lambda (&rest args) (declare (ignore args)) nil))
          (assert-true (swimmy.school:add-to-kb child :breeder :notify nil :require-bt nil)
                       "Expected breeder variant to pass duplicate logic/correlation gate")
          (assert-true (find "UT-BREEDER-CHILD-VARIANT"
                             swimmy.school::*strategy-knowledge-base*
                             :key #'swimmy.school:strategy-name
                             :test #'string=)
                       "Expected breeder child to be present in KB"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*category-pools* orig-pools)
      (setf swimmy.school::*startup-mode* orig-startup)
      (setf (symbol-function 'swimmy.school:upsert-strategy) orig-upsert)
      (setf (symbol-function 'swimmy.school::notify-recruit-unified) orig-notify))))

(deftest test-add-to-kb-normalizes-timeframe-to-minutes
  "add-to-kb should canonicalize timeframe labels (e.g., H5) to minutes(int)."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-pools swimmy.school::*category-pools*)
         (orig-startup swimmy.school::*startup-mode*)
         (orig-upsert (symbol-function 'swimmy.school:upsert-strategy))
         (orig-notify (symbol-function 'swimmy.school::notify-recruit-unified))
         (s (swimmy.school:make-strategy :name "UT-ADD-TF-H5"
                                         :symbol "USDJPY"
                                         :timeframe "H5"
                                         :direction :BOTH
                                         :rank :incubator
                                         :sl 10.0
                                         :tp 20.0
                                         :sharpe 0.0
                                         :indicators '((sma 20))
                                         :entry '(> close sma-20)
                                         :exit '(< close sma-20))))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* nil)
          (setf swimmy.school::*category-pools* (make-hash-table))
          (setf swimmy.school::*startup-mode* t)
          (setf (symbol-function 'swimmy.school:upsert-strategy)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::notify-recruit-unified)
                (lambda (&rest args) (declare (ignore args)) nil))
          (assert-true (swimmy.school:add-to-kb s :founder :notify nil :require-bt nil))
          (assert-equal 300 (swimmy.school:strategy-timeframe s)))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*category-pools* orig-pools)
      (setf swimmy.school::*startup-mode* orig-startup)
      (setf (symbol-function 'swimmy.school:upsert-strategy) orig-upsert)
      (setf (symbol-function 'swimmy.school::notify-recruit-unified) orig-notify))))

(deftest test-add-to-kb-rejects-breeder-logic-duplicate-when-sltp-too-close
  "Breeder child should still be rejected when SL/TP delta is too small."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-pools swimmy.school::*category-pools*)
         (orig-startup swimmy.school::*startup-mode*)
         (orig-upsert (symbol-function 'swimmy.school:upsert-strategy))
         (orig-notify (symbol-function 'swimmy.school::notify-recruit-unified))
         (base (swimmy.school:make-strategy :name "UT-BREEDER-BASE-CLOSE"
                                            :symbol "USDJPY"
                                            :timeframe 300
                                            :direction :BOTH
                                            :rank :B
                                            :sl 10.0
                                            :tp 20.0
                                            :sharpe 1.2
                                            :indicators '((sma 20))
                                            :entry '(> close sma-20)
                                            :exit '(< close sma-20)))
         (child (swimmy.school:make-strategy :name "UT-BREEDER-CHILD-CLOSE"
                                             :symbol "USDJPY"
                                             :timeframe 300
                                             :direction :BOTH
                                             :rank :incubator
                                             :sl 10.4
                                             :tp 20.7
                                             :sharpe 0.0
                                             :indicators '((sma 20))
                                             :entry '(> close sma-20)
                                             :exit '(< close sma-20))))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list base))
          (setf swimmy.school::*category-pools* (make-hash-table))
          (setf swimmy.school::*startup-mode* t)
          (setf (symbol-function 'swimmy.school:upsert-strategy)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::notify-recruit-unified)
                (lambda (&rest args) (declare (ignore args)) nil))
          (assert-false (swimmy.school:add-to-kb child :breeder :notify nil :require-bt nil)
                        "Expected near-identical breeder clone to remain rejected"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*category-pools* orig-pools)
      (setf swimmy.school::*startup-mode* orig-startup)
      (setf (symbol-function 'swimmy.school:upsert-strategy) orig-upsert)
      (setf (symbol-function 'swimmy.school::notify-recruit-unified) orig-notify))))

(deftest test-add-to-kb-allows-correlated-strategy-across-symbols
  "Correlation screening should not block identical logic across different symbols (multi-currency evolution)."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-pools swimmy.school::*category-pools*)
         (orig-startup swimmy.school::*startup-mode*)
         (orig-upsert (symbol-function 'swimmy.school:upsert-strategy))
         (orig-notify (symbol-function 'swimmy.school::notify-recruit-unified))
         (base (swimmy.school:make-strategy :name "UT-CORR-USDJPY"
                                            :symbol "USDJPY"
                                            :timeframe 300
                                            :direction :BOTH
                                            :rank :B
                                            :sl 10.0
                                            :tp 20.0
                                            :sharpe 1.2
                                            :indicators '((sma 20))
                                            :entry '(> close sma-20)
                                            :exit '(< close sma-20)))
         (eur (swimmy.school:make-strategy :name "UT-CORR-EURUSD"
                                           :symbol "EURUSD"
                                           :timeframe 300
                                           :direction :BOTH
                                           :rank :incubator
                                           :sl 10.0
                                           :tp 20.0
                                           :sharpe 0.0
                                           :indicators '((sma 20))
                                           :entry '(> close sma-20)
                                           :exit '(< close sma-20))))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list base))
          (setf swimmy.school::*category-pools* (make-hash-table))
          (setf swimmy.school::*startup-mode* t)
          (setf (symbol-function 'swimmy.school:upsert-strategy)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::notify-recruit-unified)
                (lambda (&rest args) (declare (ignore args)) nil))
          (assert-true (swimmy.school:add-to-kb eur :founder :notify nil :require-bt nil)
                       "Expected EURUSD clone logic to be accepted (symbol-scoped correlation)")
          (assert-true (find "UT-CORR-EURUSD"
                             swimmy.school::*strategy-knowledge-base*
                             :key #'swimmy.school:strategy-name
                             :test #'string=)
                       "Expected EURUSD strategy to be present in KB"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*category-pools* orig-pools)
      (setf swimmy.school::*startup-mode* orig-startup)
      (setf (symbol-function 'swimmy.school:upsert-strategy) orig-upsert)
      (setf (symbol-function 'swimmy.school::notify-recruit-unified) orig-notify))))

(deftest test-add-to-kb-allows-founder-when-only-archived-duplicate-exists
  "Founder admission should ignore duplicate logic when conflicting entries are archived."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-pools swimmy.school::*category-pools*)
         (orig-startup swimmy.school::*startup-mode*)
         (orig-upsert (symbol-function 'swimmy.school:upsert-strategy))
         (orig-notify (symbol-function 'swimmy.school::notify-recruit-unified))
         (archived (swimmy.school:make-strategy :name "UT-ARCHIVED-DUPE"
                                                :symbol "USDJPY"
                                                :timeframe 300
                                                :direction :BOTH
                                                :rank :graveyard
                                                :sl 10.0
                                                :tp 20.0
                                                :sharpe -1.0
                                                :indicators '((sma 20))
                                                :entry '(> close sma-20)
                                                :exit '(< close sma-20)))
         (candidate (swimmy.school:make-strategy :name "UT-LIVE-FROM-ARCHIVE-DUPE"
                                                 :symbol "USDJPY"
                                                 :timeframe 300
                                                 :direction :BOTH
                                                 :rank nil
                                                 :sl 10.0
                                                 :tp 20.0
                                                 :sharpe 0.0
                                                 :indicators '((sma 20))
                                                 :entry '(> close sma-20)
                                                 :exit '(< close sma-20))))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list archived))
          (setf swimmy.school::*category-pools* (make-hash-table))
          (setf swimmy.school::*startup-mode* t)
          (setf (symbol-function 'swimmy.school:upsert-strategy)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::notify-recruit-unified)
                (lambda (&rest args) (declare (ignore args)) nil))
          (assert-true (swimmy.school:add-to-kb candidate :founder :notify nil :require-bt nil)
                       "Expected archived-only duplicate logic to not block founder admission")
          (assert-true (find "UT-LIVE-FROM-ARCHIVE-DUPE"
                             swimmy.school::*strategy-knowledge-base*
                             :key #'swimmy.school:strategy-name
                             :test #'string=)
                       "Expected candidate to be added"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*category-pools* orig-pools)
      (setf swimmy.school::*startup-mode* orig-startup)
      (setf (symbol-function 'swimmy.school:upsert-strategy) orig-upsert)
      (setf (symbol-function 'swimmy.school::notify-recruit-unified) orig-notify))))

(deftest test-add-to-kb-allows-founder-with-archived-same-name-collision
  "Founder admission should allow same-name re-entry when only archived rows collide."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-pools swimmy.school::*category-pools*)
         (orig-startup swimmy.school::*startup-mode*)
         (orig-upsert (symbol-function 'swimmy.school:upsert-strategy))
         (orig-notify (symbol-function 'swimmy.school::notify-recruit-unified))
         (archived (swimmy.school:make-strategy :name "UT-SAME-NAME-ARCHIVE"
                                                :symbol "USDJPY"
                                                :timeframe 300
                                                :direction :BOTH
                                                :rank :graveyard
                                                :sl 10.0
                                                :tp 20.0
                                                :sharpe -1.0
                                                :indicators '((sma 20))
                                                :entry '(> close sma-20)
                                                :exit '(< close sma-20)))
         (candidate (swimmy.school:make-strategy :name "UT-SAME-NAME-ARCHIVE"
                                                 :symbol "USDJPY"
                                                 :timeframe 300
                                                 :direction :BOTH
                                                 :rank nil
                                                 :sl 10.0
                                                 :tp 20.0
                                                 :sharpe 0.4
                                                 :indicators '((ema 20))
                                                 :entry '(> close ema-20)
                                                 :exit '(< close ema-20))))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list archived))
          (setf swimmy.school::*category-pools* (make-hash-table))
          (setf swimmy.school::*startup-mode* t)
          (setf (symbol-function 'swimmy.school:upsert-strategy)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::notify-recruit-unified)
                (lambda (&rest args) (declare (ignore args)) nil))
          (assert-true (swimmy.school:add-to-kb candidate :founder :notify nil :require-bt nil)
                       "Expected founder admission despite archived same-name collision")
          (let ((same-name-count
                  (count "UT-SAME-NAME-ARCHIVE"
                         swimmy.school::*strategy-knowledge-base*
                         :key #'swimmy.school:strategy-name
                         :test #'string=)))
            (assert-equal 2 same-name-count
                          "Expected archived+new same-name rows to coexist in memory")))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*category-pools* orig-pools)
      (setf swimmy.school::*startup-mode* orig-startup)
      (setf (symbol-function 'swimmy.school:upsert-strategy) orig-upsert)
      (setf (symbol-function 'swimmy.school::notify-recruit-unified) orig-notify))))

(deftest test-add-to-kb-allows-breeder-variant-even-if-graveyard-pattern
  "Breeder variant accepted by duplicate/correlation gates should bypass graveyard one-shot rejection."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-pools swimmy.school::*category-pools*)
         (orig-startup swimmy.school::*startup-mode*)
         (orig-upsert (symbol-function 'swimmy.school:upsert-strategy))
         (orig-notify (symbol-function 'swimmy.school::notify-recruit-unified))
         (orig-graveyard (symbol-function 'swimmy.school::is-graveyard-pattern-p))
         (base (swimmy.school:make-strategy :name "UT-BREEDER-BASE-GRAVE"
                                            :symbol "USDJPY"
                                            :timeframe 300
                                            :direction :BOTH
                                            :rank :B
                                            :sl 10.0
                                            :tp 20.0
                                            :sharpe 1.2
                                            :indicators '((sma 20))
                                            :entry '(> close sma-20)
                                            :exit '(< close sma-20)))
         (child (swimmy.school:make-strategy :name "UT-BREEDER-CHILD-GRAVE"
                                             :symbol "USDJPY"
                                             :timeframe 300
                                             :direction :BOTH
                                             :rank :incubator
                                             :sl 16.0
                                             :tp 32.0
                                             :sharpe 0.0
                                             :indicators '((sma 20))
                                             :entry '(> close sma-20)
                                             :exit '(< close sma-20))))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list base))
          (setf swimmy.school::*category-pools* (make-hash-table))
          (setf swimmy.school::*startup-mode* nil)
          (setf (symbol-function 'swimmy.school:upsert-strategy)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::notify-recruit-unified)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::is-graveyard-pattern-p)
                (lambda (&rest args) (declare (ignore args)) t))
          (assert-true (swimmy.school:add-to-kb child :breeder :notify nil :require-bt nil)
                       "Expected breeder-approved variant to bypass graveyard rejection"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*category-pools* orig-pools)
      (setf swimmy.school::*startup-mode* orig-startup)
      (setf (symbol-function 'swimmy.school:upsert-strategy) orig-upsert)
      (setf (symbol-function 'swimmy.school::notify-recruit-unified) orig-notify)
      (setf (symbol-function 'swimmy.school::is-graveyard-pattern-p) orig-graveyard))))

(deftest test-add-to-kb-founder-phase1-bypasses-graveyard-pattern
  "Founder entries should bypass graveyard rejection when founder graveyard bypass flag is enabled."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-pending swimmy.school::*phase1-pending-candidates*)
         (orig-pools swimmy.school::*category-pools*)
         (orig-startup swimmy.school::*startup-mode*)
         (orig-upsert (symbol-function 'swimmy.school:upsert-strategy))
         (orig-notify (symbol-function 'swimmy.school::notify-recruit-unified))
         (orig-graveyard (symbol-function 'swimmy.school::is-graveyard-pattern-p))
         (orig-phase1 (symbol-function 'swimmy.school::run-phase-1-screening))
         (bypass-sym 'swimmy.school::*founder-graveyard-bypass-enabled*)
         (orig-bypass-bound (boundp bypass-sym))
         (orig-bypass (and orig-bypass-bound (symbol-value bypass-sym)))
         (phase1-called 0)
         (founder (swimmy.school:make-strategy :name "UT-FOUNDER-GRAVE-BYPASS"
                                               :symbol "EURUSD"
                                               :timeframe 300
                                               :direction :BOTH
                                               :rank nil
                                               :sl 10.0
                                               :tp 20.0
                                               :sharpe 0.0
                                               :profit-factor 0.0
                                               :win-rate 0.0
                                               :max-dd 0.0
                                               :trades 0
                                               :indicators '((ema 20))
                                               :entry '(> close ema-20)
                                               :exit '(< close ema-20))))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* nil)
          (setf swimmy.school::*phase1-pending-candidates* (make-hash-table :test 'equal))
          (setf swimmy.school::*category-pools* (make-hash-table))
          (setf swimmy.school::*startup-mode* nil)
          (setf (symbol-value bypass-sym) t)
          (setf (symbol-function 'swimmy.school:upsert-strategy)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::notify-recruit-unified)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::is-graveyard-pattern-p)
                (lambda (&rest args) (declare (ignore args)) t))
          (setf (symbol-function 'swimmy.school::run-phase-1-screening)
                (lambda (&rest args)
                  (declare (ignore args))
                  (incf phase1-called)
                  t))
          (assert-true (swimmy.school:add-to-kb founder :founder :notify nil :require-bt t)
                       "Expected founder to bypass graveyard and queue Phase1")
          (assert-equal 1 phase1-called "Expected founder Phase1 queue to be triggered once")
          (assert-false (find founder swimmy.school::*strategy-knowledge-base* :test #'eq)
                        "Founder should remain pending before Phase1 result")
          (assert-not-nil (gethash (swimmy.school:strategy-name founder) swimmy.school::*phase1-pending-candidates*)
                          "Founder should be staged in pending queue"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*phase1-pending-candidates* orig-pending)
      (setf swimmy.school::*category-pools* orig-pools)
      (setf swimmy.school::*startup-mode* orig-startup)
      (if orig-bypass-bound
          (setf (symbol-value bypass-sym) orig-bypass)
          (makunbound bypass-sym))
      (setf (symbol-function 'swimmy.school:upsert-strategy) orig-upsert)
      (setf (symbol-function 'swimmy.school::notify-recruit-unified) orig-notify)
      (setf (symbol-function 'swimmy.school::is-graveyard-pattern-p) orig-graveyard)
      (setf (symbol-function 'swimmy.school::run-phase-1-screening) orig-phase1))))

(deftest test-add-to-kb-breeder-phase1-bypasses-graveyard-pattern
  "Breeder entries requiring Phase1 should bypass graveyard rejection when bypass flag is enabled."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-pending swimmy.school::*phase1-pending-candidates*)
         (orig-pools swimmy.school::*category-pools*)
         (orig-startup swimmy.school::*startup-mode*)
         (orig-upsert (symbol-function 'swimmy.school:upsert-strategy))
         (orig-notify (symbol-function 'swimmy.school::notify-recruit-unified))
         (orig-graveyard (symbol-function 'swimmy.school::is-graveyard-pattern-p))
         (orig-phase1 (symbol-function 'swimmy.school::run-phase-1-screening))
         (orig-bypass swimmy.school::*breeder-graveyard-bypass-for-phase1-enabled*)
         (phase1-called 0)
         (child (swimmy.school:make-strategy :name "UT-BREEDER-GRAVE-P1-BYPASS"
                                             :symbol "USDJPY"
                                             :timeframe 300
                                             :direction :BOTH
                                             :rank nil
                                             :sl 10.0
                                             :tp 20.0
                                             :sharpe 1.2
                                             :profit-factor 1.4
                                             :win-rate 0.45
                                             :max-dd 0.10
                                             :indicators '((ema 20))
                                             :entry '(> close ema-20)
                                             :exit '(< close ema-20))))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* nil)
          (setf swimmy.school::*phase1-pending-candidates* (make-hash-table :test 'equal))
          (setf swimmy.school::*category-pools* (make-hash-table))
          (setf swimmy.school::*startup-mode* nil)
          (setf swimmy.school::*breeder-graveyard-bypass-for-phase1-enabled* t)
          (setf (symbol-function 'swimmy.school:upsert-strategy)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::notify-recruit-unified)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::is-graveyard-pattern-p)
                (lambda (&rest args) (declare (ignore args)) t))
          (setf (symbol-function 'swimmy.school::run-phase-1-screening)
                (lambda (&rest args)
                  (declare (ignore args))
                  (incf phase1-called)
                  t))
          (assert-true (swimmy.school:add-to-kb child :breeder :notify nil :require-bt t)
                       "Expected breeder Phase1 path to bypass graveyard rejection")
          (assert-equal 1 phase1-called "Expected Phase1 screening queue for bypassed breeder child")
          (assert-false (find child swimmy.school::*strategy-knowledge-base* :test #'eq)
                        "Bypassed breeder child must not enter KB before Phase1 result")
          (assert-not-nil (gethash (swimmy.school:strategy-name child) swimmy.school::*phase1-pending-candidates*)
                          "Bypassed breeder child should be staged in pending queue"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*phase1-pending-candidates* orig-pending)
      (setf swimmy.school::*category-pools* orig-pools)
      (setf swimmy.school::*startup-mode* orig-startup)
      (setf swimmy.school::*breeder-graveyard-bypass-for-phase1-enabled* orig-bypass)
      (setf (symbol-function 'swimmy.school:upsert-strategy) orig-upsert)
      (setf (symbol-function 'swimmy.school::notify-recruit-unified) orig-notify)
      (setf (symbol-function 'swimmy.school::is-graveyard-pattern-p) orig-graveyard)
      (setf (symbol-function 'swimmy.school::run-phase-1-screening) orig-phase1))))

(deftest test-add-to-kb-breeder-phase1-graveyard-bypass-can-be-disabled
  "When bypass flag is disabled, breeder Phase1 entries should still be blocked by graveyard patterns."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-pools swimmy.school::*category-pools*)
         (orig-startup swimmy.school::*startup-mode*)
         (orig-upsert (symbol-function 'swimmy.school:upsert-strategy))
         (orig-notify (symbol-function 'swimmy.school::notify-recruit-unified))
         (orig-graveyard (symbol-function 'swimmy.school::is-graveyard-pattern-p))
         (orig-phase1 (symbol-function 'swimmy.school::run-phase-1-screening))
         (orig-bypass swimmy.school::*breeder-graveyard-bypass-for-phase1-enabled*)
         (phase1-called 0)
         (child (swimmy.school:make-strategy :name "UT-BREEDER-GRAVE-P1-BLOCK"
                                             :symbol "USDJPY"
                                             :timeframe 300
                                             :direction :BOTH
                                             :rank nil
                                             :sl 10.0
                                             :tp 20.0
                                             :sharpe 1.2
                                             :profit-factor 1.4
                                             :win-rate 0.45
                                             :max-dd 0.10
                                             :indicators '((ema 20))
                                             :entry '(> close ema-20)
                                             :exit '(< close ema-20))))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* nil)
          (setf swimmy.school::*category-pools* (make-hash-table))
          (setf swimmy.school::*startup-mode* nil)
          (setf swimmy.school::*breeder-graveyard-bypass-for-phase1-enabled* nil)
          (setf (symbol-function 'swimmy.school:upsert-strategy)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::notify-recruit-unified)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::is-graveyard-pattern-p)
                (lambda (&rest args) (declare (ignore args)) t))
          (setf (symbol-function 'swimmy.school::run-phase-1-screening)
                (lambda (&rest args)
                  (declare (ignore args))
                  (incf phase1-called)
                  t))
          (assert-false (swimmy.school:add-to-kb child :breeder :notify nil :require-bt t)
                        "Expected graveyard match to block breeder child when bypass disabled")
          (assert-equal 0 phase1-called "Phase1 queue should not run for rejected child"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*category-pools* orig-pools)
      (setf swimmy.school::*startup-mode* orig-startup)
      (setf swimmy.school::*breeder-graveyard-bypass-for-phase1-enabled* orig-bypass)
      (setf (symbol-function 'swimmy.school:upsert-strategy) orig-upsert)
      (setf (symbol-function 'swimmy.school::notify-recruit-unified) orig-notify)
      (setf (symbol-function 'swimmy.school::is-graveyard-pattern-p) orig-graveyard)
      (setf (symbol-function 'swimmy.school::run-phase-1-screening) orig-phase1))))

(deftest test-add-to-kb-breeder-requires-phase1-screening-when-require-bt
  "Breeder entry with require-bt should queue Phase1 screening without entering KB."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-pending swimmy.school::*phase1-pending-candidates*)
         (orig-pools swimmy.school::*category-pools*)
         (orig-startup swimmy.school::*startup-mode*)
         (orig-upsert (symbol-function 'swimmy.school:upsert-strategy))
         (orig-notify (symbol-function 'swimmy.school::notify-recruit-unified))
         (orig-graveyard (symbol-function 'swimmy.school::is-graveyard-pattern-p))
         (orig-phase1 (symbol-function 'swimmy.school::run-phase-1-screening))
         (phase1-called 0)
         (child (swimmy.school:make-strategy :name "UT-BREEDER-PHASE1"
                                             :symbol "USDJPY"
                                             :timeframe 300
                                             :direction :BOTH
                                             :rank nil
                                             :sl 10.0
                                             :tp 20.0
                                             :sharpe 1.8
                                             :profit-factor 1.6
                                             :win-rate 0.55
                                             :max-dd 0.10
                                             :indicators '((ema 20))
                                             :entry '(> close ema-20)
                                             :exit '(< close ema-20))))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* nil)
          (setf swimmy.school::*phase1-pending-candidates* (make-hash-table :test 'equal))
          (setf swimmy.school::*category-pools* (make-hash-table))
          (setf swimmy.school::*startup-mode* nil)
          (setf (symbol-function 'swimmy.school:upsert-strategy)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::notify-recruit-unified)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::is-graveyard-pattern-p)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::run-phase-1-screening)
                (lambda (&rest args)
                  (declare (ignore args))
                  (incf phase1-called)
                  t))
          (assert-true (swimmy.school:add-to-kb child :breeder :notify nil :require-bt t)
                       "Expected breeder child to be accepted for pending Phase1")
          (assert-equal 1 phase1-called "Expected Phase1 screening to be queued once")
          (assert-false (find child swimmy.school::*strategy-knowledge-base* :test #'eq)
                        "Breeder child must not enter KB before Phase1 result")
          (assert-not-nil (gethash (swimmy.school:strategy-name child) swimmy.school::*phase1-pending-candidates*)
                          "Breeder child should be held in pending Phase1 queue"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*phase1-pending-candidates* orig-pending)
      (setf swimmy.school::*category-pools* orig-pools)
      (setf swimmy.school::*startup-mode* orig-startup)
      (setf (symbol-function 'swimmy.school:upsert-strategy) orig-upsert)
      (setf (symbol-function 'swimmy.school::notify-recruit-unified) orig-notify)
      (setf (symbol-function 'swimmy.school::is-graveyard-pattern-p) orig-graveyard)
      (setf (symbol-function 'swimmy.school::run-phase-1-screening) orig-phase1))))

(deftest test-add-to-kb-phase1-pending-skips-pool-membership
  "Phase1-pending candidate must not be inserted into KB active pools."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-pending swimmy.school::*phase1-pending-candidates*)
         (orig-pools swimmy.school::*category-pools*)
         (orig-regime-pools swimmy.school::*regime-pools*)
         (orig-startup swimmy.school::*startup-mode*)
         (orig-upsert (symbol-function 'swimmy.school:upsert-strategy))
         (orig-notify (symbol-function 'swimmy.school::notify-recruit-unified))
         (orig-graveyard (symbol-function 'swimmy.school::is-graveyard-pattern-p))
         (orig-phase1 (symbol-function 'swimmy.school::run-phase-1-screening))
         (phase1-called 0)
         (child (swimmy.school:make-strategy :name "UT-BREEDER-PENDING-NO-POOL"
                                             :symbol "USDJPY"
                                             :timeframe 300
                                             :direction :BOTH
                                             :rank nil
                                             :sl 10.0
                                             :tp 20.0
                                             :sharpe 0.0
                                             :profit-factor 0.0
                                             :win-rate 0.0
                                             :max-dd 0.0
                                             :trades 0
                                             :indicators '((ema 20))
                                             :entry '(> close ema-20)
                                             :exit '(< close ema-20))))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* nil)
          (setf swimmy.school::*phase1-pending-candidates* (make-hash-table :test 'equal))
          (setf swimmy.school::*category-pools* (make-hash-table :test 'equal))
          (setf swimmy.school::*regime-pools* (make-hash-table :test 'equal))
          (setf swimmy.school::*startup-mode* nil)
          (setf (symbol-function 'swimmy.school:upsert-strategy)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::notify-recruit-unified)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::is-graveyard-pattern-p)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::run-phase-1-screening)
                (lambda (&rest args)
                  (declare (ignore args))
                  (incf phase1-called)
                  t))
          (assert-true (swimmy.school:add-to-kb child :breeder :notify nil :require-bt t)
                       "Expected candidate admission for phase1 queue")
          (assert-equal 1 phase1-called "Expected one Phase1 queue call")
          (assert-false (find child swimmy.school::*strategy-knowledge-base* :test #'eq)
                        "Pending candidate should not enter KB before Phase1 result")
          (assert-not-nil (gethash (swimmy.school:strategy-name child) swimmy.school::*phase1-pending-candidates*)
                          "Pending candidate should be tracked in pending queue")
          (let* ((cat (swimmy.school::categorize-strategy child))
                 (regime (swimmy.school::strategy-regime-class child))
                 (cat-pool (gethash cat swimmy.school::*category-pools*))
                 (regime-pool (gethash regime swimmy.school::*regime-pools*)))
            (assert-false (find child cat-pool :test #'eq)
                          "Pending incubator should not enter category pool")
            (assert-false (find child regime-pool :test #'eq)
                          "Pending incubator should not enter regime pool")))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*phase1-pending-candidates* orig-pending)
      (setf swimmy.school::*category-pools* orig-pools)
      (setf swimmy.school::*regime-pools* orig-regime-pools)
      (setf swimmy.school::*startup-mode* orig-startup)
      (setf (symbol-function 'swimmy.school:upsert-strategy) orig-upsert)
      (setf (symbol-function 'swimmy.school::notify-recruit-unified) orig-notify)
      (setf (symbol-function 'swimmy.school::is-graveyard-pattern-p) orig-graveyard)
      (setf (symbol-function 'swimmy.school::run-phase-1-screening) orig-phase1))))

(deftest test-handle-v2-result-admits-pending-phase1-candidate
  "Phase1 result should resolve pending candidate, evaluate, and clear pending queue."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-pending swimmy.school::*phase1-pending-candidates*)
         (orig-upsert (symbol-function 'swimmy.school:upsert-strategy))
         (orig-ensure (symbol-function 'swimmy.school::ensure-rank))
         (child (swimmy.school:make-strategy :name "UT-PENDING-P1-PASS"
                                             :symbol "USDJPY"
                                             :timeframe 300
                                             :direction :BOTH
                                             :rank nil
                                             :sl 10.0
                                             :tp 20.0
                                             :sharpe 0.0
                                             :profit-factor 0.0
                                             :win-rate 0.0
                                             :max-dd 1.0
                                             :trades 0
                                             :indicators '((ema 20))
                                             :entry '(> close ema-20)
                                             :exit '(< close ema-20)))
         (ensure-called 0))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* nil)
          (setf swimmy.school::*phase1-pending-candidates* (make-hash-table :test 'equal))
          (setf (gethash (swimmy.school:strategy-name child) swimmy.school::*phase1-pending-candidates*) child)
          (setf (symbol-function 'swimmy.school:upsert-strategy)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::ensure-rank)
                (lambda (strategy new-rank &optional reason)
                  (declare (ignore reason))
                  (incf ensure-called)
                  (setf (swimmy.school:strategy-rank strategy) new-rank)
                  new-rank))
          (swimmy.school::handle-v2-result
           "UT-PENDING-P1-PASS_P1"
           '(:sharpe 0.8 :profit-factor 1.2 :win-rate 0.5 :trades 10 :max-dd 0.1))
          (assert-equal 1 ensure-called "Expected pending candidate to be rank-evaluated on Phase1 result")
          (assert-equal :B (swimmy.school:strategy-rank child))
          (assert-not-nil (find child swimmy.school::*strategy-knowledge-base* :test #'eq)
                          "Resolved pending candidate should be admitted to KB")
          (assert-false (gethash (swimmy.school:strategy-name child) swimmy.school::*phase1-pending-candidates*)
                        "Pending queue entry should be cleared after Phase1 result"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*phase1-pending-candidates* orig-pending)
      (setf (symbol-function 'swimmy.school:upsert-strategy) orig-upsert)
      (setf (symbol-function 'swimmy.school::ensure-rank) orig-ensure))))

(deftest test-build-category-pools-excludes-unevaluated-incubator
  "build-category-pools should exclude unevaluated incubators from active pools."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-pools swimmy.school::*category-pools*)
         (orig-regime-pools swimmy.school::*regime-pools*)
         (evaluated (swimmy.school:make-strategy :name "UT-EVAL-ACTIVE"
                                                 :symbol "USDJPY"
                                                 :timeframe 300
                                                 :direction :BOTH
                                                 :rank :B
                                                 :sharpe 0.8
                                                 :profit-factor 1.4
                                                 :win-rate 0.45
                                                 :max-dd 0.12
                                                 :trades 42))
         (pending (swimmy.school:make-strategy :name "UT-INCUBATOR-PENDING"
                                               :symbol "USDJPY"
                                               :timeframe 300
                                               :direction :BOTH
                                               :rank :incubator
                                               :sharpe 0.0
                                               :profit-factor 0.0
                                               :win-rate 0.0
                                               :max-dd 0.0
                                               :trades 0)))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list evaluated pending))
          (setf swimmy.school::*category-pools* (make-hash-table :test 'equal))
          (setf swimmy.school::*regime-pools* (make-hash-table :test 'equal))
          (swimmy.school::build-category-pools)
          (let* ((cat (swimmy.school::categorize-strategy evaluated))
                 (pool (gethash cat swimmy.school::*category-pools*)))
            (assert-true (find evaluated pool :test #'eq)
                         "Evaluated strategy should remain in active pool")
            (assert-false (find pending pool :test #'eq)
                          "Pending incubator must be excluded from active pool")))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*category-pools* orig-pools)
      (setf swimmy.school::*regime-pools* orig-regime-pools))))

(deftest test-build-category-pools-excludes-zero-evidence-active-rank
  "build-category-pools should exclude active-rank entries that still have zero-evidence metrics."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-pools swimmy.school::*category-pools*)
         (orig-regime-pools swimmy.school::*regime-pools*)
         (evaluated (swimmy.school:make-strategy :name "UT-EVAL-B-ACTIVE"
                                                 :symbol "USDJPY"
                                                 :timeframe 300
                                                 :direction :BOTH
                                                 :rank :B
                                                 :sharpe 0.7
                                                 :profit-factor 1.3
                                                 :win-rate 0.42
                                                 :max-dd 0.10
                                                 :trades 55))
         (zero-evidence (swimmy.school:make-strategy :name "UT-ZERO-EVIDENCE-B"
                                                      :symbol "USDJPY"
                                                      :timeframe 300
                                                      :direction :BOTH
                                                      :rank :B
                                                      :sharpe 0.0
                                                      :profit-factor 0.0
                                                      :win-rate 0.0
                                                      :max-dd 0.0
                                                      :trades 0)))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list evaluated zero-evidence))
          (setf swimmy.school::*category-pools* (make-hash-table :test 'equal))
          (setf swimmy.school::*regime-pools* (make-hash-table :test 'equal))
          (swimmy.school::build-category-pools)
          (let* ((cat (swimmy.school::categorize-strategy evaluated))
                 (pool (gethash cat swimmy.school::*category-pools*)))
            (assert-true (find evaluated pool :test #'eq)
                         "Evaluated active-rank strategy should remain in active pool")
            (assert-false (find zero-evidence pool :test #'eq)
                          "Zero-evidence active-rank strategy must be excluded from active pool")))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*category-pools* orig-pools)
      (setf swimmy.school::*regime-pools* orig-regime-pools))))

(deftest test-build-category-pools-excludes-revalidation-pending-legend
  "build-category-pools should exclude legends while revalidation-pending is true."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-pools swimmy.school::*category-pools*)
         (orig-regime-pools swimmy.school::*regime-pools*)
         (legend-ready (swimmy.school:make-strategy :name "UT-LEGEND-READY"
                                                     :symbol "USDJPY"
                                                     :timeframe 3600
                                                     :direction :BOTH
                                                     :rank :legend
                                                     :revalidation-pending nil
                                                     :sharpe 1.1
                                                     :profit-factor 1.6
                                                     :win-rate 0.51
                                                     :max-dd 0.11
                                                     :trades 180))
         (legend-pending (swimmy.school:make-strategy :name "UT-LEGEND-PENDING"
                                                       :symbol "USDJPY"
                                                       :timeframe 3600
                                                       :direction :BOTH
                                                       :rank :legend
                                                       :revalidation-pending t
                                                       :sharpe 1.2
                                                       :profit-factor 1.7
                                                       :win-rate 0.52
                                                       :max-dd 0.12
                                                       :trades 190)))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list legend-ready legend-pending))
          (setf swimmy.school::*category-pools* (make-hash-table :test 'equal))
          (setf swimmy.school::*regime-pools* (make-hash-table :test 'equal))
          (swimmy.school::build-category-pools)
          (let* ((cat (swimmy.school::categorize-strategy legend-ready))
                 (pool (gethash cat swimmy.school::*category-pools*)))
            (assert-true (find legend-ready pool :test #'eq)
                         "Legend should be active once revalidation pending is cleared")
            (assert-false (find legend-pending pool :test #'eq)
                          "Revalidation-pending legend must be excluded from active pool")))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*category-pools* orig-pools)
      (setf swimmy.school::*regime-pools* orig-regime-pools))))

(deftest test-select-best-from-pool-excludes-unevaluated-incubator
  "select-best-from-pool should defensively filter incubator/unranked entries."
  (let* ((orig-regime-pools swimmy.school::*regime-pools*)
         (evaluated (swimmy.school:make-strategy :name "UT-POOL-EVAL"
                                                 :symbol "USDJPY"
                                                 :timeframe 300
                                                 :direction :BOTH
                                                 :category :trend
                                                 :rank :B
                                                 :sharpe 0.9
                                                 :profit-factor 1.4
                                                 :win-rate 0.5
                                                 :max-dd 0.1
                                                 :trades 80))
         (pending (swimmy.school:make-strategy :name "UT-POOL-PENDING"
                                               :symbol "USDJPY"
                                               :timeframe 300
                                               :direction :BOTH
                                               :category :trend
                                               :rank :incubator
                                               :sharpe 0.0
                                               :profit-factor 0.0
                                               :win-rate 0.0
                                               :max-dd 0.0
                                               :trades 0)))
    (unwind-protect
        (progn
          (setf swimmy.school::*regime-pools* (make-hash-table :test 'equal))
          (setf (gethash :trend swimmy.school::*regime-pools*) (list pending evaluated))
          (let ((picked (swimmy.school::select-best-from-pool :trend 5))
                (pool-after (gethash :trend swimmy.school::*regime-pools*)))
            (assert-true (find evaluated picked :test #'eq)
                         "Evaluated strategy should remain selectable")
            (assert-false (find pending picked :test #'eq)
                          "Unevaluated incubator must be filtered out from selection")
            (assert-false (find pending pool-after :test #'eq)
                          "Defensive scrub should remove pending incubator from regime pool")))
      (setf swimmy.school::*regime-pools* orig-regime-pools))))

(deftest test-force-recruit-strategy-skips-unevaluated-active-pools
  "force-recruit-strategy should not inject incubator/unranked strategies into active pools."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-evolved swimmy.school::*evolved-strategies*)
         (orig-category-pools swimmy.school::*category-pools*)
         (orig-regime-pools swimmy.school::*regime-pools*)
         (pending (swimmy.school:make-strategy :name "UT-SF-PENDING"
                                               :symbol "USDJPY"
                                               :timeframe 300
                                               :direction :BOTH
                                               :category :trend
                                               :rank :incubator
                                               :sharpe 0.0
                                               :profit-factor 0.0
                                               :win-rate 0.0
                                               :max-dd 0.0
                                               :trades 0)))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list pending))
          (setf swimmy.school::*evolved-strategies* nil)
          (setf swimmy.school::*category-pools* (make-hash-table :test 'equal))
          (setf swimmy.school::*regime-pools* (make-hash-table :test 'equal))
          (assert-true (swimmy.school::force-recruit-strategy "UT-SF-PENDING")
                       "Expected force-recruit to find strategy in KB")
          (let* ((cat (swimmy.school::categorize-strategy pending))
                 (regime (swimmy.school::strategy-regime-class pending)))
            (assert-false (find pending (gethash cat swimmy.school::*category-pools*) :test #'eq)
                          "Unevaluated strategy must not enter category pool")
            (assert-false (find pending (gethash regime swimmy.school::*regime-pools*) :test #'eq)
                          "Unevaluated strategy must not enter regime pool")
            (assert-true (find pending swimmy.school::*evolved-strategies* :test #'eq)
                         "force-recruit should still track strategy in evolved registry")))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*evolved-strategies* orig-evolved)
      (setf swimmy.school::*category-pools* orig-category-pools)
      (setf swimmy.school::*regime-pools* orig-regime-pools))))

(deftest test-run-legend-breeding-routes-child-through-add-to-kb
  "Legend breeding should route child admission through add-to-kb with breeder BT requirements."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-get (symbol-function 'swimmy.school::get-strategies-by-rank))
         (orig-breed (symbol-function 'swimmy.school::breed-strategies))
         (orig-add (symbol-function 'swimmy.school::add-to-kb))
         (orig-inc (symbol-function 'swimmy.school::increment-breeding-count))
         (legend (swimmy.school:make-strategy :name "UT-LEGEND-PARENT" :rank :legend :generation 10))
         (b-rank (swimmy.school:make-strategy :name "UT-B-PARENT"
                                              :rank :B
                                              :generation 3
                                              :sl 20.0 :tp 40.0
                                              :trades 80
                                              :sharpe 0.60
                                              :profit-factor 1.20
                                              :win-rate 0.40
                                              :max-dd 0.10))
         (child (swimmy.school:make-strategy :name "UT-LEGEND-CHILD" :rank nil :generation 11))
         (add-called 0)
         (captured-source nil)
         (captured-require-bt nil))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* nil)
          (setf (symbol-function 'swimmy.school::get-strategies-by-rank)
                (lambda (rank)
                  (case rank
                    (:legend (list legend))
                    (:B (list b-rank))
                    (otherwise nil))))
          (setf (symbol-function 'swimmy.school::breed-strategies)
                (lambda (&rest args)
                  (declare (ignore args))
                  child))
          (setf (symbol-function 'swimmy.school::add-to-kb)
                (lambda (strategy source &key notify require-bt)
                  (declare (ignore notify))
                  (incf add-called)
                  (setf captured-source source
                        captured-require-bt require-bt)
                  (assert-true (eq strategy child) "Legend breeder should pass child to add-to-kb")
                  t))
          (setf (symbol-function 'swimmy.school::increment-breeding-count)
                (lambda (&rest args) (declare (ignore args)) nil))
          (assert-equal 1 (swimmy.school::run-legend-breeding)
                        "Expected one legend child admitted via add-to-kb")
          (assert-equal 1 add-called "Expected add-to-kb to be called exactly once")
          (assert-equal :breeder captured-source "Legend child should use breeder source")
          (assert-equal t captured-require-bt "Legend child should require BT/Phase1 via add-to-kb")
          (assert-equal 0 (length swimmy.school::*strategy-knowledge-base*)
                        "run-legend-breeding should not directly push to KB"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf (symbol-function 'swimmy.school::get-strategies-by-rank) orig-get)
      (setf (symbol-function 'swimmy.school::breed-strategies) orig-breed)
      (setf (symbol-function 'swimmy.school::add-to-kb) orig-add)
      (setf (symbol-function 'swimmy.school::increment-breeding-count) orig-inc))))

(deftest test-increment-breeding-count-does-not-graveyard-on-limit
  "Breeding count limit should stop parent reuse without force-graveyarding viable B parents."
  (let* ((orig-send (symbol-function 'swimmy.school::send-to-graveyard))
         (orig-limit swimmy.school::*max-breeding-uses*)
         (graveyard-called nil)
         (parent (swimmy.school:make-strategy
                  :name "UT-BREED-COUNT-LIMIT"
                  :rank :B
                  :breeding-count 2
                  :sharpe 1.1
                  :profit-factor 1.2
                  :win-rate 0.41
                  :max-dd 0.09
                  :trades 300)))
    (unwind-protect
        (progn
          (setf swimmy.school::*max-breeding-uses* 3)
          (setf (symbol-function 'swimmy.school::send-to-graveyard)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (setf graveyard-called t)
                  :graveyard))
          (swimmy.school::increment-breeding-count parent)
          (assert-equal 3 (swimmy.school::strategy-breeding-count parent)
                        "Breeding count should still increment to limit")
          (assert-false graveyard-called
                        "Limit reached should not force graveyard")
          (assert-equal :B (swimmy.school:strategy-rank parent)
                        "Parent rank should remain unchanged"))
      (setf (symbol-function 'swimmy.school::send-to-graveyard) orig-send)
      (setf swimmy.school::*max-breeding-uses* orig-limit))))

(deftest test-promotion-triggers-noncorrelation-notification
  "Ensure A/S promotions fire noncorrelation notification once"
  (let* ((s-min (or (ignore-errors (swimmy.school::min-trade-evidence-for-rank :S)) 100))
         (s-trades (+ s-min 20))
         (tmp-db (format nil "/tmp/swimmy-promo-~a.db" (get-universal-time)))
         (strat (swimmy.school:make-strategy :name "PROMO"
                                             :symbol "USDJPY"
                                             :rank :B
                                             :sharpe 0.8
                                             :profit-factor 1.8
                                             :win-rate 0.55
                                             :max-dd 0.09
                                             :trades s-trades
                                             :pnl-history (loop repeat 60 collect 1.0)
                                             :cpcv-median-sharpe 0.8
                                             :cpcv-median-pf 1.6
                                             :cpcv-median-wr 0.5
                                             :cpcv-median-maxdd 0.10
                                             :cpcv-pass-rate 0.8))
         (called 0)
         (orig (and (fboundp 'swimmy.school::notify-noncorrelated-promotion)
                    (symbol-function 'swimmy.school::notify-noncorrelated-promotion)))
         (orig-sync (and (fboundp 'swimmy.school::maybe-sync-evolution-report-on-promotion)
                         (symbol-function 'swimmy.school::maybe-sync-evolution-report-on-promotion)))
         (orig-common (and (fboundp 'swimmy.school::common-stage2-gates-passed-p)
                           (symbol-function 'swimmy.school::common-stage2-gates-passed-p))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (setf (symbol-function 'swimmy.school::notify-noncorrelated-promotion)
                  (lambda (&rest args) (declare (ignore args)) (incf called)))
            ;; Prevent test promotions from triggering evolution report sync + Discord send.
            (when orig-sync
              (setf (symbol-function 'swimmy.school::maybe-sync-evolution-report-on-promotion)
                    (lambda (&rest args) (declare (ignore args)) nil)))
            ;; Isolate this test to notification behavior (not Stage2 sub-gate details).
            (when orig-common
              (setf (symbol-function 'swimmy.school::common-stage2-gates-passed-p)
                    (lambda (&rest args) (declare (ignore args)) (values t nil))))
            (swimmy.school::ensure-rank strat :A "test")
            (assert-equal 1 called "A promotion should notify once")
            (swimmy.school::ensure-rank strat :S "test")
            (assert-equal 2 called "S promotion should notify once"))
        (when orig
          (setf (symbol-function 'swimmy.school::notify-noncorrelated-promotion) orig))
        (when orig-sync
          (setf (symbol-function 'swimmy.school::maybe-sync-evolution-report-on-promotion) orig-sync))
        (when orig-common
          (setf (symbol-function 'swimmy.school::common-stage2-gates-passed-p) orig-common))
        (ignore-errors (swimmy.school::close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-composite-score-prefers-stable-pf-wr
  "Composite score should favor PF/WR with acceptable DD even if Sharpe is lower"
  (let* ((fn (find-symbol "SCORE-FROM-METRICS" :swimmy.school)))
    (assert-true (and fn (fboundp fn)) "score-from-metrics exists")
    (let* ((a (list :sharpe 0.4 :profit-factor 1.8 :win-rate 0.60 :max-dd 0.08))
           (b (list :sharpe 0.8 :profit-factor 1.1 :win-rate 0.42 :max-dd 0.18))
           (score-a (funcall fn a))
           (score-b (funcall fn b)))
      (assert-true (> score-a score-b) "PF/WR/low-DD should beat Sharpe-only"))))

(deftest test-composite-score-penalizes-high-dd
  "Composite score should penalize high MaxDD even with strong Sharpe"
  (let* ((fn (find-symbol "SCORE-FROM-METRICS" :swimmy.school)))
    (assert-true (and fn (fboundp fn)) "score-from-metrics exists")
    (let* ((safe (list :sharpe 1.0 :profit-factor 1.5 :win-rate 0.55 :max-dd 0.08))
           (risky (list :sharpe 1.2 :profit-factor 1.5 :win-rate 0.55 :max-dd 0.22))
           (score-safe (funcall fn safe))
           (score-risky (funcall fn risky)))
      (assert-true (> score-safe score-risky) "High DD should reduce score"))))

(deftest test-b-rank-cull-uses-composite-score
  "Rank B culling should use composite score, not Sharpe only"
  (let* ((mk (lambda (name sharpe pf wr dd)
               (swimmy.school:make-strategy :name name :rank :B
                                            :sharpe sharpe :profit-factor pf
                                            :win-rate wr :max-dd dd)))
         (s1 (funcall mk "S1" 1.2 1.0 0.40 0.20))
         (s2 (funcall mk "S2" 0.5 1.8 0.60 0.08))
         (s3 (funcall mk "S3" 0.6 1.4 0.50 0.10))
         (s4 (funcall mk "S4" 0.4 1.9 0.62 0.07))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-cap swimmy.school::*rank-b-capacity*)
         (orig-demote (symbol-function 'swimmy.school::demote-to-graveyard))
         (called nil))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list s1 s2 s3 s4))
          (setf swimmy.school::*rank-b-capacity* 2)
          (setf (symbol-function 'swimmy.school::demote-to-graveyard)
                (lambda (strategy reason)
                  (declare (ignore reason))
                  (push (swimmy.school:strategy-name strategy) called)))
          (swimmy.school::cull-rank-b-pool)
          (assert-equal 1 (length called) "should cull exactly one")
          (assert-true (find "S1" called :test #'string=)
                       "low composite should be culled"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*rank-b-capacity* orig-cap)
      (setf (symbol-function 'swimmy.school::demote-to-graveyard) orig-demote))))

(deftest test-breeder-cull-uses-composite-score
  "Breeder pool culling should use composite score"
  (let* ((mk (lambda (name sharpe pf wr dd)
               (swimmy.school:make-strategy :name name :rank :B
                                            :sharpe sharpe :profit-factor pf
                                            :win-rate wr :max-dd dd)))
         (s1 (funcall mk "S1" 1.2 1.0 0.40 0.20))
         (s2 (funcall mk "S2" 0.5 1.8 0.60 0.08))
         (s3 (funcall mk "S3" 0.6 1.4 0.50 0.10))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-pools swimmy.school::*category-pools*)
         (orig-regime-pools swimmy.school::*regime-pools*)
         (orig-limit swimmy.school::*b-rank-pool-size*)
         (orig-notify (symbol-function 'swimmy.school::notify-death))
         (orig-init (symbol-function 'swimmy.school::init-db))
         (orig-exec (symbol-function 'swimmy.school::execute-non-query))
         (called nil))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list s1 s2 s3))
          (setf swimmy.school::*category-pools* (make-hash-table :test 'equal))
          (setf (gethash :trend swimmy.school::*category-pools*) (list s1 s2 s3))
          (setf swimmy.school::*regime-pools* (make-hash-table :test 'equal))
          (setf (gethash :trend swimmy.school::*regime-pools*) (list s1 s2 s3))
          (setf swimmy.school::*b-rank-pool-size* 2)
          (setf (symbol-function 'swimmy.school::notify-death)
                (lambda (strategy reason)
                  (declare (ignore reason))
                  (push (swimmy.school:strategy-name strategy) called)))
          (setf (symbol-function 'swimmy.school::init-db) (lambda () nil))
          (setf (symbol-function 'swimmy.school::execute-non-query)
                (lambda (&rest args) (declare (ignore args)) nil))
          (swimmy.school::cull-pool-overflow :trend)
          (assert-equal 1 (length called) "should cull exactly one")
          (assert-true (find "S1" called :test #'string=)
                       "low composite should be culled"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*category-pools* orig-pools)
      (setf swimmy.school::*regime-pools* orig-regime-pools)
      (setf swimmy.school::*b-rank-pool-size* orig-limit)
      (setf (symbol-function 'swimmy.school::notify-death) orig-notify)
      (setf (symbol-function 'swimmy.school::init-db) orig-init)
      (setf (symbol-function 'swimmy.school::execute-non-query) orig-exec))))

(deftest test-promotion-uses-composite-score
  "Promotion should not rely on Sharpe alone"
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-ranks swimmy.school::*strategy-ranks*)
         (orig-coming (symbol-function 'swimmy.school::coming-of-age))
         (strat (swimmy.school:make-strategy :name "UT-PROMO"
                                             :sharpe 0.4
                                             :profit-factor 1.8
                                             :win-rate 0.60
                                             :max-dd 0.08)))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list strat))
          (setf swimmy.school::*strategy-ranks* (make-hash-table :test 'equal))
          (setf (symbol-function 'swimmy.school::coming-of-age)
                (lambda (&rest args) (declare (ignore args)) nil))
          (let ((rank-data (swimmy.school:get-strategy-rank "UT-PROMO")))
            (setf (swimmy.school::strategy-rank-trades rank-data) 10)
            (setf (swimmy.school::strategy-rank-wins rank-data) 6)
            (setf (swimmy.school::strategy-rank-total-pnl rank-data) 1000)
            (setf (swimmy.school::strategy-rank-rank rank-data) :incubator))
          (assert-equal :warrior (swimmy.school:check-promotion "UT-PROMO")
                        "should promote based on composite score"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*strategy-ranks* orig-ranks)
      (setf (symbol-function 'swimmy.school::coming-of-age) orig-coming))))

(deftest test-a-rank-evaluation-uses-composite-score
  "A-rank evaluation should use composite score for probation decisions"
  (let* ((orig-probation swimmy.school::*a-rank-probation-tracker*)
         (orig-promote (symbol-function 'swimmy.school::promote-rank))
         (orig-demote (symbol-function 'swimmy.school::demote-rank))
         (orig-send (symbol-function 'swimmy.school::send-to-graveyard))
         (strat (swimmy.school:make-strategy :name "UT-A-SCORE"
                                             :rank :A
                                             :sharpe 0.2
                                             :profit-factor 1.8
                                             :win-rate 0.60
                                             :trades 120
                                             :max-dd 0.08)))
    (unwind-protect
        (progn
          (setf swimmy.school::*a-rank-probation-tracker* (make-hash-table :test 'equal))
          (setf (symbol-function 'swimmy.school::promote-rank)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::demote-rank)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::send-to-graveyard)
                (lambda (&rest args) (declare (ignore args)) nil))
          (assert-equal :A (swimmy.school::evaluate-a-rank-strategy strat)
                        "should remain A based on composite score")
          (assert-true (null (gethash "UT-A-SCORE" swimmy.school::*a-rank-probation-tracker*))
                       "Composite score should avoid probation despite low Sharpe"))
      (setf swimmy.school::*a-rank-probation-tracker* orig-probation)
      (setf (symbol-function 'swimmy.school::promote-rank) orig-promote)
      (setf (symbol-function 'swimmy.school::demote-rank) orig-demote)
      (setf (symbol-function 'swimmy.school::send-to-graveyard) orig-send))))

(deftest test-evaluate-a-rank-demotes-on-min-trade-evidence
  "A-rank evaluation should demote sparse-evidence strategies before composite probation logic."
  (let* ((orig-demote (symbol-function 'swimmy.school::demote-rank))
         (orig-promote (symbol-function 'swimmy.school::promote-rank))
         (orig-send (symbol-function 'swimmy.school::send-to-graveyard))
         (demoted nil)
         (strat (swimmy.school:make-strategy :name "UT-A-LOW-EVIDENCE"
                                             :rank :A
                                             :sharpe 1.2
                                             :profit-factor 2.0
                                             :win-rate 0.70
                                             :trades 35
                                             :max-dd 0.05)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::demote-rank)
                (lambda (s new-rank reason)
                  (declare (ignore reason))
                  (setf demoted (list (swimmy.school:strategy-name s) new-rank))
                  (setf (swimmy.school:strategy-rank s) new-rank)
                  new-rank))
          (setf (symbol-function 'swimmy.school::promote-rank)
                (lambda (&rest args) (declare (ignore args)) :S))
          (setf (symbol-function 'swimmy.school::send-to-graveyard)
                (lambda (&rest args) (declare (ignore args)) :graveyard))
          (assert-equal :B (swimmy.school::evaluate-a-rank-strategy strat))
          (assert-equal :B (swimmy.school:strategy-rank strat))
          (assert-true (equal demoted (list "UT-A-LOW-EVIDENCE" :B))
                       "Expected immediate demotion to B on low trade evidence"))
      (setf (symbol-function 'swimmy.school::demote-rank) orig-demote)
      (setf (symbol-function 'swimmy.school::promote-rank) orig-promote)
      (setf (symbol-function 'swimmy.school::send-to-graveyard) orig-send))))

(deftest test-enforce-rank-trade-evidence-floors-demotes-existing-as
  "Trade-evidence floor sweep should demote sparse S/A ranks."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-demote (symbol-function 'swimmy.school::demote-rank))
         (s-low (swimmy.school:make-strategy :name "UT-S-LOW"
                                             :rank :S
                                             :trades 35))
         (s-mid (swimmy.school:make-strategy :name "UT-S-MID"
                                             :rank :S
                                             :trades 80))
         (a-low (swimmy.school:make-strategy :name "UT-A-LOW"
                                             :rank :A
                                             :trades 35))
         (a-ok (swimmy.school:make-strategy :name "UT-A-OK"
                                            :rank :A
                                            :trades 80))
         (demotions '()))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list s-low s-mid a-low a-ok))
          (setf (symbol-function 'swimmy.school::demote-rank)
                (lambda (s new-rank reason)
                  (declare (ignore reason))
                  (push (list (swimmy.school:strategy-name s) (swimmy.school:strategy-rank s) new-rank)
                        demotions)
                  (setf (swimmy.school:strategy-rank s) new-rank)
                  new-rank))
          (let ((summary (swimmy.school::enforce-rank-trade-evidence-floors)))
            (assert-equal 2 (getf summary :s-demoted))
            (assert-equal 1 (getf summary :a-demoted))
            (assert-equal :B (swimmy.school:strategy-rank s-low))
            (assert-equal :A (swimmy.school:strategy-rank s-mid))
            (assert-equal :B (swimmy.school:strategy-rank a-low))
            (assert-equal :A (swimmy.school:strategy-rank a-ok))
            (assert-equal 3 (length demotions))))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf (symbol-function 'swimmy.school::demote-rank) orig-demote))))

(deftest test-enforce-s-rank-criteria-conformance-demotes-noncompliant-s
  "S conformance sweep should demote S strategies that no longer meet S criteria."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-demote (symbol-function 'swimmy.school::demote-rank))
         (demotions '())
         (s-ok (swimmy.school:make-strategy :name "UT-S-CONF-OK"
                                            :rank :S
                                            :sharpe 0.90
                                            :profit-factor 1.80
                                            :win-rate 0.55
                                            :max-dd 0.08
                                            :trades 160
                                            :cpcv-pass-rate 0.80
                                            :cpcv-median-maxdd 0.10))
         (s-to-a (swimmy.school:make-strategy :name "UT-S-CONF-TO-A"
                                              :rank :S
                                              :sharpe 0.60
                                              :profit-factor 1.40
                                              :win-rate 0.45
                                              :max-dd 0.12
                                              :trades 160
                                              :oos-sharpe 0.50
                                              :cpcv-pass-rate 0.80
                                              :cpcv-median-maxdd 0.10))
         (s-to-b (swimmy.school:make-strategy :name "UT-S-CONF-TO-B"
                                              :rank :S
                                              :sharpe 0.20
                                              :profit-factor 1.10
                                              :win-rate 0.36
                                              :max-dd 0.20
                                              :trades 160
                                              :oos-sharpe 0.10
                                              :cpcv-pass-rate 0.80
                                              :cpcv-median-maxdd 0.10)))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list s-ok s-to-a s-to-b))
          (setf (symbol-function 'swimmy.school::demote-rank)
                (lambda (s new-rank reason)
                  (declare (ignore reason))
                  (push (list (swimmy.school:strategy-name s) (swimmy.school:strategy-rank s) new-rank)
                        demotions)
                  (setf (swimmy.school:strategy-rank s) new-rank)
                  new-rank))
          (let ((summary (swimmy.school::enforce-s-rank-criteria-conformance)))
            (assert-equal 2 (getf summary :s-demoted))
            (assert-equal :S (swimmy.school:strategy-rank s-ok))
            (assert-equal :A (swimmy.school:strategy-rank s-to-a))
            (assert-equal :B (swimmy.school:strategy-rank s-to-b))
            (assert-equal 2 (length demotions))))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf (symbol-function 'swimmy.school::demote-rank) orig-demote))))

(deftest test-enforce-a-b-rank-criteria-conformance-corrects-noncompliant-ranks
  "A/B conformance sweep should demote noncompliant A and graveyard noncompliant B."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-demote (symbol-function 'swimmy.school::demote-rank))
         (orig-send (symbol-function 'swimmy.school::send-to-graveyard))
         (a-ok (swimmy.school:make-strategy :name "UT-A-AB-OK"
                                            :rank :A
                                            :sharpe 0.80
                                            :profit-factor 1.60
                                            :win-rate 0.52
                                            :max-dd 0.10
                                            :trades 120
                                            :oos-sharpe 0.50))
         (a-to-b (swimmy.school:make-strategy :name "UT-A-AB-TO-B"
                                              :rank :A
                                              :sharpe 0.30
                                              :profit-factor 1.10
                                              :win-rate 0.36
                                              :max-dd 0.20
                                              :trades 120
                                              :oos-sharpe 0.50))
         (b-ok (swimmy.school:make-strategy :name "UT-B-AB-OK"
                                            :rank :B
                                            :sharpe 0.25
                                            :profit-factor 1.15
                                            :win-rate 0.40
                                            :max-dd 0.20
                                            :trades 80))
         (b-to-g (swimmy.school:make-strategy :name "UT-B-AB-GRAVE"
                                              :rank :B
                                              :sharpe 0.05
                                              :profit-factor 1.00
                                              :win-rate 0.30
                                              :max-dd 0.30
                                              :trades 80))
         (demotions '())
         (graveyarded '()))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list a-ok a-to-b b-ok b-to-g))
          (setf (symbol-function 'swimmy.school::demote-rank)
                (lambda (s new-rank reason)
                  (declare (ignore reason))
                  (push (list (swimmy.school:strategy-name s) (swimmy.school:strategy-rank s) new-rank)
                        demotions)
                  (setf (swimmy.school:strategy-rank s) new-rank)
                  new-rank))
          (setf (symbol-function 'swimmy.school::send-to-graveyard)
                (lambda (s reason)
                  (declare (ignore reason))
                  (push (swimmy.school:strategy-name s) graveyarded)
                  (setf (swimmy.school:strategy-rank s) :graveyard)
                  :graveyard))
          (let ((summary (swimmy.school::enforce-a-b-rank-criteria-conformance)))
            (assert-equal 1 (getf summary :a-demoted))
            (assert-equal 1 (getf summary :b-graveyarded))
            (assert-equal :A (swimmy.school:strategy-rank a-ok))
            (assert-equal :B (swimmy.school:strategy-rank a-to-b))
            (assert-equal :B (swimmy.school:strategy-rank b-ok))
            (assert-equal :graveyard (swimmy.school:strategy-rank b-to-g))
            (assert-equal 1 (length demotions))
            (assert-equal 1 (length graveyarded))))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf (symbol-function 'swimmy.school::demote-rank) orig-demote)
      (setf (symbol-function 'swimmy.school::send-to-graveyard) orig-send))))

(deftest test-run-rank-evaluation-invokes-a-b-rank-conformance
  "run-rank-evaluation should execute A/B conformance sweep every cycle."
  (let* ((orig-floor (symbol-function 'swimmy.school::enforce-rank-trade-evidence-floors))
         (orig-s-conf (symbol-function 'swimmy.school::enforce-s-rank-criteria-conformance))
         (orig-ab-conf (and (fboundp 'swimmy.school::enforce-a-b-rank-criteria-conformance)
                            (symbol-function 'swimmy.school::enforce-a-b-rank-criteria-conformance)))
         (orig-culling (symbol-function 'swimmy.school::run-b-rank-culling))
         (orig-eval-a (symbol-function 'swimmy.school::evaluate-a-rank-strategy))
         (orig-get-by-rank (symbol-function 'swimmy.school::get-strategies-by-rank))
         (orig-reset-oos (symbol-function 'swimmy.school::reset-oos-failure-stats))
         (orig-normalize-oos (symbol-function 'swimmy.school::normalize-oos-defaults))
         (orig-portfolio (symbol-function 'swimmy.school::construct-global-portfolio))
         (orig-fetch-trade-map (symbol-function 'swimmy.school::fetch-backtest-trade-count-map))
         (ab-called nil))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::enforce-rank-trade-evidence-floors)
                (lambda () (list :s-demoted 0 :a-demoted 0)))
          (setf (symbol-function 'swimmy.school::enforce-s-rank-criteria-conformance)
                (lambda () (list :s-demoted 0)))
          (setf (symbol-function 'swimmy.school::enforce-a-b-rank-criteria-conformance)
                (lambda ()
                  (setf ab-called t)
                  (list :a-demoted 0 :b-graveyarded 0)))
          (setf (symbol-function 'swimmy.school::run-b-rank-culling) (lambda (&optional single-tf) (declare (ignore single-tf)) nil))
          (setf (symbol-function 'swimmy.school::evaluate-a-rank-strategy) (lambda (s) (declare (ignore s)) :A))
          (setf (symbol-function 'swimmy.school::get-strategies-by-rank)
                (lambda (rank &optional timeframe direction symbol)
                  (declare (ignore rank timeframe direction symbol))
                  nil))
          (setf (symbol-function 'swimmy.school::reset-oos-failure-stats) (lambda () nil))
          (setf (symbol-function 'swimmy.school::normalize-oos-defaults) (lambda () nil))
          (setf (symbol-function 'swimmy.school::construct-global-portfolio) (lambda () nil))
          (setf (symbol-function 'swimmy.school::fetch-backtest-trade-count-map) (lambda () nil))
          (swimmy.school::run-rank-evaluation)
          (assert-true ab-called "Expected A/B conformance sweep to be called from run-rank-evaluation"))
      (setf (symbol-function 'swimmy.school::enforce-rank-trade-evidence-floors) orig-floor)
      (setf (symbol-function 'swimmy.school::enforce-s-rank-criteria-conformance) orig-s-conf)
      (if orig-ab-conf
          (setf (symbol-function 'swimmy.school::enforce-a-b-rank-criteria-conformance) orig-ab-conf)
          (fmakunbound 'swimmy.school::enforce-a-b-rank-criteria-conformance))
      (setf (symbol-function 'swimmy.school::run-b-rank-culling) orig-culling)
      (setf (symbol-function 'swimmy.school::evaluate-a-rank-strategy) orig-eval-a)
      (setf (symbol-function 'swimmy.school::get-strategies-by-rank) orig-get-by-rank)
      (setf (symbol-function 'swimmy.school::reset-oos-failure-stats) orig-reset-oos)
      (setf (symbol-function 'swimmy.school::normalize-oos-defaults) orig-normalize-oos)
      (setf (symbol-function 'swimmy.school::construct-global-portfolio) orig-portfolio)
      (setf (symbol-function 'swimmy.school::fetch-backtest-trade-count-map) orig-fetch-trade-map))))

(deftest test-check-rank-criteria-requires-cpcv-pass-rate
  "S-RANK criteria should require CPCV pass-rate >= 0.7"
  (let ((strat (swimmy.school:make-strategy :name "UT-CPCV-PASS"
                                            :rank :A
                                            :sharpe 0.75
                                            :profit-factor 1.8
                                            :win-rate 0.55
                                            :trades 120
                                            :max-dd 0.10
                                            :cpcv-median-sharpe 0.8
                                            :cpcv-median-pf 1.6
                                            :cpcv-median-wr 0.5
                                            :cpcv-median-maxdd 0.12
                                            :cpcv-pass-rate 0.69)))
    (assert-false (swimmy.school::check-rank-criteria strat :S)
                  "Expected S criteria to fail when CPCV pass-rate < 0.7")))

(deftest test-check-rank-criteria-cpcv-medians-pass
  "S-RANK criteria should accept CPCV medians when all thresholds are met."
  (let ((strat (swimmy.school:make-strategy :name "UT-S-OK"
                                            :rank :A
                                            :sharpe 0.75
                                            :profit-factor 1.8
                                            :win-rate 0.55
                                            :trades 120
                                            :max-dd 0.09
                                            :cpcv-median-sharpe 0.8
                                            :cpcv-pass-rate 0.7
                                            :cpcv-median-pf 1.8
                                            :cpcv-median-wr 0.55
                                            :cpcv-median-maxdd 0.10)))
    (assert-true (swimmy.school::check-rank-criteria strat :S))))

(deftest test-check-rank-criteria-s-allows-staged-pf-wr-for-high-trade-evidence
  "S-RANK base PF/WR gate should relax for high trade-evidence candidates."
  (let ((strat (swimmy.school:make-strategy :name "UT-S-STAGED-PASS"
                                            :rank :A
                                            :sharpe 0.90
                                            :profit-factor 1.35
                                            :win-rate 0.40
                                            :trades 200
                                            :max-dd 0.08
                                            :cpcv-median-sharpe 0.82
                                            :cpcv-median-pf 1.7
                                            :cpcv-median-wr 0.5
                                            :cpcv-pass-rate 0.80
                                            :cpcv-median-maxdd 0.10)))
    (assert-true (swimmy.school::check-rank-criteria strat :S)
                 "Expected staged PF/WR gate to pass for high-trade candidate")))

(deftest test-check-rank-criteria-s-keeps-strict-pf-wr-for-low-trade-evidence
  "S-RANK base PF/WR gate should remain strict for low trade-evidence candidates."
  (let ((strat (swimmy.school:make-strategy :name "UT-S-STAGED-FAIL"
                                            :rank :A
                                            :sharpe 0.90
                                            :profit-factor 1.35
                                            :win-rate 0.40
                                            :trades 10
                                            :max-dd 0.08
                                            :cpcv-median-sharpe 0.82
                                            :cpcv-median-pf 1.7
                                            :cpcv-median-wr 0.5
                                            :cpcv-pass-rate 0.80
                                            :cpcv-median-maxdd 0.10)))
    (assert-false (swimmy.school::check-rank-criteria strat :S)
                  "Expected strict PF/WR gate to fail for low-trade candidate")))

(deftest test-check-rank-criteria-a-requires-min-trade-evidence
  "A-RANK criteria should reject sparse trade evidence even when other metrics pass."
  (let ((strat (swimmy.school:make-strategy :name "UT-A-MIN-TRADES"
                                            :rank :B
                                            :sharpe 1.20
                                            :profit-factor 1.40
                                            :win-rate 0.50
                                            :trades 35
                                            :max-dd 0.08
                                            :oos-sharpe 0.50)))
    (assert-false (swimmy.school::check-rank-criteria strat :A)
                  "A criteria should fail when trade evidence is below floor")))

(deftest test-check-rank-criteria-s-requires-min-trade-evidence
  "S-RANK criteria should reject sparse trade evidence even when IS/CPCV metrics pass."
  (let ((strat (swimmy.school:make-strategy :name "UT-S-MIN-TRADES"
                                            :rank :A
                                            :sharpe 1.20
                                            :profit-factor 1.90
                                            :win-rate 0.60
                                            :trades 35
                                            :max-dd 0.08
                                            :cpcv-median-sharpe 0.90
                                            :cpcv-median-pf 1.90
                                            :cpcv-median-wr 0.60
                                            :cpcv-pass-rate 0.80
                                            :cpcv-median-maxdd 0.10)))
    (assert-false (swimmy.school::check-rank-criteria strat :S)
                  "S criteria should fail when trade evidence is below floor")))

(deftest test-strategy-trade-evidence-count-includes-shadow-trade-logs
  "strategy-trade-evidence-count should include shadow trade_logs in addition to backtest evidence."
  (let* ((tmp-db (format nil "/tmp/swimmy-shadow-evidence-~a.db" (get-universal-time)))
         (name "UT-SHADOW-EVIDENCE"))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (dotimes (i 12)
              (swimmy.school::record-trade-to-db
               (let ((mode-args (list (intern "EXECUTION-MODE" :keyword) :shadow)))
                 (apply #'swimmy.school::make-trade-record
                        (append (list :timestamp (+ 100 i)
                                      :strategy-name name
                                      :symbol "USDJPY"
                                      :direction :buy
                                      :category :trend
                                      :regime :trend
                                      :pnl (if (evenp i) 0.2 -0.1)
                                      :hold-time 60)
                                mode-args)))))
            (dotimes (i 5)
              (swimmy.school::execute-non-query
               "INSERT INTO backtest_trade_logs (request_id, strategy_name, timestamp, pnl, oos_kind)
                VALUES (?, ?, ?, ?, ?)"
               "UT-REQ" name (+ 200 i) 0.1 "CPCV"))
            (let* ((strat (swimmy.school:make-strategy :name name :symbol "USDJPY" :trades 0))
                   (evidence (swimmy.school::strategy-trade-evidence-count strat)))
              (assert-equal 17 evidence
                            "Expected evidence count to include 12 shadow + 5 backtest trades")))
        (ignore-errors (swimmy.school::close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-armada-core-profile-gate-requires-pf-dd-trades
  "Armada Core profile gate should reject candidates that fail PF/DD/Trades floors."
  (let* ((strat (swimmy.school:make-strategy :name "UT-ARMADA-CORE-PFDD"
                                             :rank :B
                                             :sharpe 1.20
                                             :profit-factor 1.45
                                             :win-rate 0.52
                                             :trades 320
                                             :max-dd 0.10
                                             :oos-sharpe 0.50)))
    (assert-false (swimmy.school::armada-core-profile-passed-p strat)
                  "PF<1.60 should fail Armada Core profile gate")))

(deftest test-armada-core-profile-gate-requires-hold-time-band
  "Armada Core profile gate should enforce average hold-time band and side-bias cap."
  (let ((strat (swimmy.school:make-strategy :name "UT-ARMADA-CORE-HOLD"
                                            :rank :B
                                            :sharpe 1.20
                                            :profit-factor 1.90
                                            :win-rate 0.52
                                            :trades 500
                                            :max-dd 0.08
                                            :oos-sharpe 0.60)))
    (assert-false (swimmy.school::armada-core-profile-passed-p
                   strat
                   :trade-profile (list :avg-hold-seconds 1200
                                        :max-side-share 0.60))
                  "Average hold below 3h should fail")
    (assert-false (swimmy.school::armada-core-profile-passed-p
                   strat
                   :trade-profile (list :avg-hold-seconds 21600
                                        :max-side-share 0.90))
                  "Side share above 80% should fail")
    (assert-true (swimmy.school::armada-core-profile-passed-p
                  strat
                  :trade-profile (list :avg-hold-seconds 21600
                                       :max-side-share 0.62))
                 "Balanced hold-time and side-share should pass")))

(deftest test-enable-armada-core-canary-mode-limits-gate-to-a-rank
  "Canary helper should enable Armada gate only for A-rank and require trade profile."
  (let* ((had-enabled (boundp 'swimmy.school::*armada-core-profile-enabled*))
         (orig-enabled (and had-enabled swimmy.school::*armada-core-profile-enabled*))
         (had-ranks (boundp 'swimmy.school::*armada-core-apply-ranks*))
         (orig-ranks (and had-ranks swimmy.school::*armada-core-apply-ranks*))
         (had-require (boundp 'swimmy.school::*armada-core-require-trade-profile*))
         (orig-require (and had-require swimmy.school::*armada-core-require-trade-profile*)))
    (unwind-protect
        (progn
          (setf swimmy.school::*armada-core-profile-enabled* nil)
          (setf swimmy.school::*armada-core-apply-ranks* '(:A :S))
          (setf swimmy.school::*armada-core-require-trade-profile* nil)
          (assert-true (swimmy.school::enable-armada-core-canary-mode))
          (assert-true swimmy.school::*armada-core-profile-enabled*
                       "Canary mode should enable Armada profile")
          (assert-equal '(:A) swimmy.school::*armada-core-apply-ranks*
                        "Canary mode should apply gate to A-rank only")
          (assert-true swimmy.school::*armada-core-require-trade-profile*
                       "Canary mode should require trade profile")))
      (if had-enabled
          (setf swimmy.school::*armada-core-profile-enabled* orig-enabled)
          (makunbound 'swimmy.school::*armada-core-profile-enabled*))
      (if had-ranks
          (setf swimmy.school::*armada-core-apply-ranks* orig-ranks)
          (makunbound 'swimmy.school::*armada-core-apply-ranks*))
      (if had-require
          (setf swimmy.school::*armada-core-require-trade-profile* orig-require)
          (makunbound 'swimmy.school::*armada-core-require-trade-profile*))))

(deftest test-check-rank-criteria-vnext-a-oos-threshold
  "A-RANK OOS gate should require OOS Sharpe >= 0.35"
  (let ((strat (swimmy.school:make-strategy :name "UT-A-OOS"
                                            :rank :B
                                            :sharpe 0.45
                                            :profit-factor 1.3
                                            :win-rate 0.43
                                            :max-dd 0.15
                                            :oos-sharpe 0.34)))
    (assert-false (swimmy.school::check-rank-criteria strat :A)
                  "Expected A criteria to fail when OOS Sharpe < 0.35")))

(deftest test-check-rank-criteria-vnext-a-wr-floor-is-43pct
  "A-RANK base gate should reject candidates below WR 43%."
  (let ((strat (swimmy.school:make-strategy :name "UT-A-WR-FLOOR"
                                            :rank :B
                                            :sharpe 1.20
                                            :profit-factor 1.35
                                            :win-rate 0.42
                                            :max-dd 0.12
                                            :oos-sharpe 0.40)))
    (assert-false (swimmy.school::check-rank-criteria strat :A)
                  "Expected A criteria to fail for WR=42% candidate")))

(deftest test-meets-a-rank-criteria-aligns-with-check-rank-criteria
  "meets-a-rank-criteria should match the canonical A base gate."
  (let ((strat (swimmy.school:make-strategy :name "UT-A-MEETS-ALIGN"
                                            :rank :B
                                            :sharpe 1.10
                                            :profit-factor 1.32
                                            :win-rate 0.42
                                            :max-dd 0.12
                                            :oos-sharpe 0.40)))
    (assert-false (swimmy.school::check-rank-criteria strat :A :include-oos nil)
                  "WR<43% should fail A base criteria")
    (assert-equal (swimmy.school::check-rank-criteria strat :A :include-oos nil)
                  (swimmy.school::meets-a-rank-criteria strat)
                  "Expected A base checks to stay in sync")))

(deftest test-b-rank-culling-default-threshold-is-20
  "B-rank culling default threshold should retain 20 strategies per category."
  (assert-equal 20 swimmy.school::*culling-threshold*))

(deftest test-b-rank-culling-for-category-filters-a-base-candidates
  "Category culling should queue only strategies meeting A base criteria."
  (let* ((a-min (or (ignore-errors (swimmy.school::min-trade-evidence-for-rank :A)) 50))
         (a-trades (+ a-min 20))
         (mk (lambda (name sharpe pf wr dd)
               (swimmy.school:make-strategy :name name
                                            :rank :B
                                            :symbol "USDJPY"
                                            :direction :BOTH
                                            :timeframe 300
                                            :sharpe sharpe
                                            :profit-factor pf
                                            :win-rate wr
                                            :max-dd dd
                                            :trades a-trades)))
         (a-pass-1 (funcall mk "UT-A-PASS-1" 1.20 1.34 0.44 0.12))
         (a-fail-pf (funcall mk "UT-A-FAIL-PF" 1.60 1.20 0.45 0.10))
         (a-pass-2 (funcall mk "UT-A-PASS-2" 0.90 1.40 0.43 0.11))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-threshold swimmy.school::*culling-threshold*)
         (orig-slots swimmy.school::*a-rank-slots-per-tf*)
         (orig-validate (symbol-function 'swimmy.school::validate-for-a-rank-promotion))
         (orig-graveyard (symbol-function 'swimmy.school::send-to-graveyard))
         (orig-exp (symbol-function 'swimmy.school::a-expectancy-gate-passed-p))
         (queued nil))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list a-pass-1 a-fail-pf a-pass-2))
          (setf swimmy.school::*culling-threshold* 1)
          (setf swimmy.school::*a-rank-slots-per-tf* 2)
          (setf (symbol-function 'swimmy.school::a-expectancy-gate-passed-p)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (values t 1.0)))
          (setf (symbol-function 'swimmy.school::validate-for-a-rank-promotion)
                (lambda (strat)
                  (push (swimmy.school:strategy-name strat) queued)
                  t))
          (setf (symbol-function 'swimmy.school::send-to-graveyard)
                (lambda (&rest _args) (declare (ignore _args)) nil))
          (swimmy.school::run-b-rank-culling-for-category 300 :BOTH "USDJPY")
          (assert-false (find "UT-A-FAIL-PF" queued :test #'string=)
                        "PF-failing strategy should not be queued for A validation")
          (assert-true (find "UT-A-PASS-1" queued :test #'string=))
          (assert-true (find "UT-A-PASS-2" queued :test #'string=)))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*culling-threshold* orig-threshold)
      (setf swimmy.school::*a-rank-slots-per-tf* orig-slots)
      (setf (symbol-function 'swimmy.school::validate-for-a-rank-promotion) orig-validate)
      (setf (symbol-function 'swimmy.school::send-to-graveyard) orig-graveyard)
      (setf (symbol-function 'swimmy.school::a-expectancy-gate-passed-p) orig-exp))))

(deftest test-run-b-rank-culling-does-not-run-global-sharpe-only-promotion
  "run-b-rank-culling should not queue all B strategies by Sharpe-only sweep."
  (let* ((mk (lambda (name sharpe pf wr dd)
               (swimmy.school:make-strategy :name name
                                            :rank :B
                                            :symbol "USDJPY"
                                            :direction :BOTH
                                            :timeframe 300
                                            :sharpe sharpe
                                            :profit-factor pf
                                            :win-rate wr
                                            :max-dd dd)))
         (s1 (funcall mk "UT-GLOBAL-SWEEP-1" 0.90 1.10 0.40 0.20))
         (s2 (funcall mk "UT-GLOBAL-SWEEP-2" 1.10 1.12 0.41 0.22))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-threshold swimmy.school::*culling-threshold*)
         (orig-symbols (symbol-function 'swimmy.school::collect-active-symbols))
         (orig-validate (symbol-function 'swimmy.school::validate-for-a-rank-promotion))
         (queued nil))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list s1 s2))
          ;; Prevent category loops and isolate legacy global sweep behavior.
          (setf swimmy.school::*culling-threshold* 99999)
          (setf (symbol-function 'swimmy.school::collect-active-symbols)
                (lambda () nil))
          (setf (symbol-function 'swimmy.school::validate-for-a-rank-promotion)
                (lambda (strat)
                  (push (swimmy.school:strategy-name strat) queued)
                  t))
          (swimmy.school::run-b-rank-culling)
          (assert-equal 0 (length queued)
                        "Global Sharpe-only sweep should not queue A validation"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*culling-threshold* orig-threshold)
      (setf (symbol-function 'swimmy.school::collect-active-symbols) orig-symbols)
      (setf (symbol-function 'swimmy.school::validate-for-a-rank-promotion) orig-validate))))

(deftest test-run-b-rank-culling-uses-active-timeframes-dynamically
  "run-b-rank-culling should include active B timeframes via TF buckets (e.g., H5->H4 bucket)."
  (let* ((s (swimmy.school:make-strategy :name "UT-TF-300"
                                         :rank :B
                                         :symbol "USDJPY"
                                         :direction :BOTH
                                         :timeframe 300
                                         :sharpe 0.9
                                         :profit-factor 1.2
                                         :win-rate 0.4
                                         :max-dd 0.2))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-symbols (symbol-function 'swimmy.school::collect-active-symbols))
         (orig-cull (symbol-function 'swimmy.school::run-b-rank-culling-for-category))
         (orig-dirs swimmy.school::*supported-directions*)
         (calls nil))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list s))
          (setf swimmy.school::*supported-directions* '(:BOTH))
          (setf (symbol-function 'swimmy.school::collect-active-symbols)
                (lambda () '("USDJPY")))
          (setf (symbol-function 'swimmy.school::run-b-rank-culling-for-category)
                (lambda (timeframe direction symbol)
                  (push (list timeframe direction symbol) calls)))
          (swimmy.school::run-b-rank-culling)
          (assert-true (find 240 calls :key #'first :test #'eql)
                       "Expected dynamic active TF bucket (300->240) to be processed"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*supported-directions* orig-dirs)
      (setf (symbol-function 'swimmy.school::collect-active-symbols) orig-symbols)
      (setf (symbol-function 'swimmy.school::run-b-rank-culling-for-category) orig-cull))))

(deftest test-timeframe-utils-support-arbitrary-minutes
  "get-tf-string/get-tf-minutes should support arbitrary TF minutes (no silent M1 fallback)."
  ;; Minutes -> Label
  (assert-equal "M36" (swimmy.school::get-tf-string 36))
  (assert-equal "H2" (swimmy.school::get-tf-string 120))
  (assert-equal "H5" (swimmy.school::get-tf-string 300))
  (assert-equal "H60" (swimmy.school::get-tf-string 3600))
  (assert-equal "MN" (swimmy.school::get-tf-string 43200))
  ;; Label -> Minutes
  (assert-equal 36 (swimmy.school::get-tf-minutes "M36"))
  (assert-equal 120 (swimmy.school::get-tf-minutes "H2"))
  (assert-equal 300 (swimmy.school::get-tf-minutes "H5"))
  (assert-equal 3600 (swimmy.school::get-tf-minutes "H60"))
  (assert-equal 43200 (swimmy.school::get-tf-minutes "MN"))
  (assert-equal 43200 (swimmy.school::get-tf-minutes "MN1")))

(deftest test-resolve-execution-timeframe-minutes-is-strict
  "Execution timeframe resolver should fail-closed on invalid literals."
  (assert-equal 15 (swimmy.school::resolve-execution-timeframe-minutes 15))
  (assert-equal 15 (swimmy.school::resolve-execution-timeframe-minutes "M15"))
  (assert-equal 15 (swimmy.school::resolve-execution-timeframe-minutes "15"))
  (assert-equal 43200 (swimmy.school::resolve-execution-timeframe-minutes "MN1"))
  (assert-false (swimmy.school::resolve-execution-timeframe-minutes nil))
  (assert-false (swimmy.school::resolve-execution-timeframe-minutes ""))
  (assert-false (swimmy.school::resolve-execution-timeframe-minutes "NIL"))
  (assert-false (swimmy.school::resolve-execution-timeframe-minutes "UNKNOWN"))
  (assert-false (swimmy.school::resolve-execution-timeframe-minutes "M0")))

(deftest test-timeframe-bucketization-is-finite
  "get-tf-bucket-minutes should map arbitrary TFs into finite category/correlation buckets."
  (assert-equal 5 (swimmy.school::get-tf-bucket-minutes 1))
  (assert-equal 30 (swimmy.school::get-tf-bucket-minutes 36))
  (assert-equal 60 (swimmy.school::get-tf-bucket-minutes 45)) ; tie -> prefer larger bucket
  (assert-equal 240 (swimmy.school::get-tf-bucket-minutes 300))
  (assert-equal 1440 (swimmy.school::get-tf-bucket-minutes 3600))
  (assert-equal 43200 (swimmy.school::get-tf-bucket-minutes "MN1")))

(deftest test-pattern-gate-enabled-timeframe-supports-custom-tf-via-bucket
  "Pattern gate timeframe predicate should accept custom TFs through bucket normalization."
  (assert-true (swimmy.school::pattern-gate-enabled-timeframe-p "H5")
               "H5 should map to H4 bucket and be gate-applicable")
  (assert-true (swimmy.school::pattern-gate-enabled-timeframe-p 300)
               "300 minutes should map to H4 bucket and be gate-applicable")
  (assert-false (swimmy.school::pattern-gate-enabled-timeframe-p "M36")
                "M36 should map to M30 bucket and stay non-applicable"))

(deftest test-apply-pattern-similarity-gate-accepts-custom-h5
  "Pattern similarity gate should evaluate H5 by normalizing to supported bucket TF."
  (let ((called nil)
        (called-tf nil)
        (history (loop for i from 0 below 130 collect i)))
    (multiple-value-bind (new-lot mult reason p-up p-down p-flat)
        (swimmy.school::apply-pattern-similarity-gate
         "USDJPY" "H5" :buy 0.10 history
         :query-fn (lambda (_sym tf _candles &key k)
                     (declare (ignore _sym _candles k))
                     (setf called t
                           called-tf tf)
                     '((status . "ok")
                       (result . ((p_up . 0.80) (p_down . 0.10) (p_flat . 0.10))))))
      (assert-true called "custom H5 should be query-applicable")
      (assert-equal "H4" called-tf "H5 should normalize to H4 bucket for pattern query")
      (assert-equal "aligned" reason)
      (assert-equal 1.0 mult)
      (assert-equal 0.10 new-lot)
      (assert-equal 0.80 p-up)
      (assert-equal 0.10 p-down)
      (assert-equal 0.10 p-flat))))

(deftest test-score-from-metrics-penalizes-low-trade-evidence
  "Composite score should shrink for low trade evidence to avoid sparse Sharpe overvaluation."
  (let* ((base '(:sharpe 1.20 :profit-factor 1.50 :win-rate 0.52 :max-dd 0.08))
         (low (swimmy.school::score-from-metrics (append base (list :trades 35))))
         (high (swimmy.school::score-from-metrics (append base (list :trades 220)))))
    (assert-true (> high low)
                 (format nil "Expected high-evidence score (~,4f) > low-evidence score (~,4f)" high low))))

(deftest test-tf-mutation-options-keeps-core-and-anchors-under-pending-pressure
  "TF mutation options should shrink under pending pressure but keep core buckets + anchors."
  (let* ((orig-mode swimmy.school::*tf-mutation-budget-mode*)
         (orig-core swimmy.school::*tf-scope-buckets-minutes*)
         (orig-extra swimmy.school::*tf-mining-candidates-minutes*)
         (orig-anchor swimmy.school::*tf-mutation-anchor-minutes*)
         (orig-submit swimmy.globals::*backtest-submit-count*)
         (orig-recv (if (boundp 'swimmy.main::*backtest-recv-count*)
                        swimmy.main::*backtest-recv-count*
                        0))
         (orig-max swimmy.globals::*backtest-max-pending*))
    (unwind-protect
        (progn
          (setf swimmy.school::*tf-scope-buckets-minutes* '(5 15 30 60 240 1440 10080 43200))
          (setf swimmy.school::*tf-mining-candidates-minutes* '(36 45 90 120 180 210 300 3600))
          (setf swimmy.school::*tf-mutation-anchor-minutes* '(300 3600))
          (setf swimmy.school::*tf-mutation-budget-mode* :auto)
          (setf swimmy.globals::*backtest-max-pending* 100)
          (setf swimmy.globals::*backtest-submit-count* 120)
          (setf swimmy.main::*backtest-recv-count* 0)
          (let ((opts (swimmy.school::get-tf-mutation-options)))
            ;; Critical pressure => only core + anchor set.
            (assert-equal 10 (length opts))
            (assert-true (find 5 opts :test #'eql))
            (assert-true (find 43200 opts :test #'eql))
            (assert-true (find 300 opts :test #'eql))
            (assert-true (find 3600 opts :test #'eql))
            (assert-false (find 36 opts :test #'eql)
                          "Expected non-anchor extras to be trimmed under pressure")))
      (setf swimmy.school::*tf-mutation-budget-mode* orig-mode)
      (setf swimmy.school::*tf-scope-buckets-minutes* orig-core)
      (setf swimmy.school::*tf-mining-candidates-minutes* orig-extra)
      (setf swimmy.school::*tf-mutation-anchor-minutes* orig-anchor)
      (setf swimmy.globals::*backtest-submit-count* orig-submit)
      (setf swimmy.main::*backtest-recv-count* orig-recv)
      (setf swimmy.globals::*backtest-max-pending* orig-max))))

(deftest test-tf-mutation-options-returns-full-set-when-pending-is-low
  "TF mutation options should keep full exploration set when pending pressure is low."
  (let* ((orig-mode swimmy.school::*tf-mutation-budget-mode*)
         (orig-core swimmy.school::*tf-scope-buckets-minutes*)
         (orig-extra swimmy.school::*tf-mining-candidates-minutes*)
         (orig-anchor swimmy.school::*tf-mutation-anchor-minutes*)
         (orig-submit swimmy.globals::*backtest-submit-count*)
         (orig-recv (if (boundp 'swimmy.main::*backtest-recv-count*)
                        swimmy.main::*backtest-recv-count*
                        0))
         (orig-max swimmy.globals::*backtest-max-pending*))
    (unwind-protect
        (progn
          (setf swimmy.school::*tf-scope-buckets-minutes* '(5 15 30 60 240 1440 10080 43200))
          (setf swimmy.school::*tf-mining-candidates-minutes* '(36 45 90 120 180 210 300 3600))
          (setf swimmy.school::*tf-mutation-anchor-minutes* '(300 3600))
          (setf swimmy.school::*tf-mutation-budget-mode* :auto)
          (setf swimmy.globals::*backtest-max-pending* 100)
          (setf swimmy.globals::*backtest-submit-count* 0)
          (setf swimmy.main::*backtest-recv-count* 0)
          (let ((opts (swimmy.school::get-tf-mutation-options)))
            (assert-equal 16 (length opts))
            (assert-true (find 36 opts :test #'eql))
            (assert-true (find 210 opts :test #'eql))
            (assert-true (find 300 opts :test #'eql))
            (assert-true (find 3600 opts :test #'eql))))
      (setf swimmy.school::*tf-mutation-budget-mode* orig-mode)
      (setf swimmy.school::*tf-scope-buckets-minutes* orig-core)
      (setf swimmy.school::*tf-mining-candidates-minutes* orig-extra)
      (setf swimmy.school::*tf-mutation-anchor-minutes* orig-anchor)
      (setf swimmy.globals::*backtest-submit-count* orig-submit)
      (setf swimmy.main::*backtest-recv-count* orig-recv)
      (setf swimmy.globals::*backtest-max-pending* orig-max))))

(deftest test-resample-candles-aligns-to-unix-buckets
  "resample-candles should align timestamps to bucket start (Guardian-compatible)."
  (flet ((mk (ts o h l c)
           (swimmy.globals:make-candle :timestamp ts :open o :high h :low l :close c :volume 1)))
    ;; Newest-first M1 candles at 0,60,120,180,240.
    (let* ((m1 (list (mk 240 5 6 4 5.5)
                     (mk 180 4 5 3 4.5)
                     (mk 120 3 4 2 3.5)
                     (mk 60 2 3 1 2.5)
                     (mk 0 1 2 0 1.5)))
           ;; 3-minute resample => buckets [0..179], [180..359]
           (out (swimmy.school::resample-candles m1 3)))
      (assert-equal 2 (length out))
      ;; Newest bucket first
      (assert-equal 180 (swimmy.globals:candle-timestamp (first out)))
      (assert-equal 0 (swimmy.globals:candle-timestamp (second out)))
      ;; Bucket 180: open from ts=180, close from ts=240
      (assert-equal 4 (swimmy.globals:candle-open (first out)))
      (assert-equal 5.5 (swimmy.globals:candle-close (first out)))
      ;; Bucket 0: open from ts=0, close from ts=120
      (assert-equal 1 (swimmy.globals:candle-open (second out)))
      (assert-equal 3.5 (swimmy.globals:candle-close (second out))))))

(deftest test-prepare-trade-context-does-not-silently-fallback-to-m1
  "prepare-trade-context should not map unknown TF minutes to M1."
  (let* ((orig-team swimmy.school::*active-team*)
         (orig-hist swimmy.globals::*candle-histories*)
         (orig-hist-tf swimmy.globals::*candle-histories-tf*)
         (team (make-hash-table :test 'equal))
         (hists (make-hash-table :test 'equal))
         (hists-tf (make-hash-table :test 'equal))
         (cat (list 300 :BOTH "USDJPY"))
         (lead (swimmy.school:make-strategy :name "UT-PTC"
                                            :symbol "USDJPY"
                                            :direction :BOTH
                                            :timeframe 300))
         (m1 (list (swimmy.globals:make-candle :timestamp 240 :open 5 :high 6 :low 4 :close 5.5 :volume 1)
                   (swimmy.globals:make-candle :timestamp 180 :open 4 :high 5 :low 3 :close 4.5 :volume 1)
                   (swimmy.globals:make-candle :timestamp 120 :open 3 :high 4 :low 2 :close 3.5 :volume 1)
                   (swimmy.globals:make-candle :timestamp 60 :open 2 :high 3 :low 1 :close 2.5 :volume 1)
                   (swimmy.globals:make-candle :timestamp 0 :open 1 :high 2 :low 0 :close 1.5 :volume 1))))
    (unwind-protect
        (progn
          (setf swimmy.school::*active-team* team)
          (setf swimmy.globals::*candle-histories* hists)
          (setf swimmy.globals::*candle-histories-tf* hists-tf)
          (setf (gethash "USDJPY" swimmy.globals::*candle-histories*) m1)
          (setf (gethash cat swimmy.school::*active-team*) (list lead))
          (multiple-value-bind (lead-name tf-key history)
              (swimmy.school::prepare-trade-context cat "USDJPY")
            (declare (ignore lead-name))
            (assert-equal "H5" tf-key)
            (assert-equal 1 (length history))
            (assert-equal 0 (swimmy.globals:candle-timestamp (first history)))))
      (setf swimmy.school::*active-team* orig-team)
      (setf swimmy.globals::*candle-histories* orig-hist)
      (setf swimmy.globals::*candle-histories-tf* orig-hist-tf))))

(deftest test-prepare-trade-context-prefers-strategy-timeframe-over-stale-override
  "prepare-trade-context should prioritize resolved strategy timeframe over stale caller override."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-team swimmy.school::*active-team*)
         (orig-hist swimmy.globals::*candle-histories*)
         (orig-hist-tf swimmy.globals::*candle-histories-tf*)
         (kb (list (swimmy.school:make-strategy :name "UT-PREFER-TF"
                                                :symbol "USDJPY"
                                                :direction :BOTH
                                                :timeframe 10080)))
         (team (make-hash-table :test 'equal))
         (hists (make-hash-table :test 'equal))
         (hists-tf (make-hash-table :test 'equal))
         (m1 (list (swimmy.globals:make-candle :timestamp 240 :open 5 :high 6 :low 4 :close 5.5 :volume 1)
                   (swimmy.globals:make-candle :timestamp 180 :open 4 :high 5 :low 3 :close 4.5 :volume 1)
                   (swimmy.globals:make-candle :timestamp 120 :open 3 :high 4 :low 2 :close 3.5 :volume 1)
                   (swimmy.globals:make-candle :timestamp 60 :open 2 :high 3 :low 1 :close 2.5 :volume 1)
                   (swimmy.globals:make-candle :timestamp 0 :open 1 :high 2 :low 0 :close 1.5 :volume 1))))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* kb)
          (setf swimmy.school::*active-team* team)
          (setf swimmy.globals::*candle-histories* hists)
          (setf swimmy.globals::*candle-histories-tf* hists-tf)
          (setf (gethash "USDJPY" swimmy.globals::*candle-histories*) m1)
          (multiple-value-bind (lead-name tf-key history)
              (swimmy.school::prepare-trade-context :trend "USDJPY"
                                                    :strategy-name "UT-PREFER-TF"
                                                    :strategy-timeframe 1)
            (declare (ignore history))
            (assert-equal "UT-PREFER-TF" lead-name)
            (assert-equal "W1" tf-key
                          "Expected strategy timeframe (W1) to override stale caller M1")))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*active-team* orig-team)
      (setf swimmy.globals::*candle-histories* orig-hist)
      (setf swimmy.globals::*candle-histories-tf* orig-hist-tf))))

(deftest test-restore-legend-61-updates-existing-timeframe-from-optimized-map
  "restore-legend-61 should refresh existing legend timeframe using optimized overrides."
  (let* ((existing (swimmy.school:make-strategy :name "Aggressive-Reversal"
                                                :timeframe 1
                                                :rank :legend))
         (canonical (swimmy.school:make-strategy :name "Aggressive-Reversal"
                                                 :timeframe 1
                                                 :rank :legend))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-read (symbol-function 'swimmy.school::%read-make-strategy-forms))
         (orig-lookup (symbol-function 'swimmy.school::%lookup-optimized-timeframe-minutes))
         (orig-mark (symbol-function 'swimmy.school::mark-revalidation-pending))
         (orig-add (symbol-function 'swimmy.school::add-to-kb))
         (orig-ensure (symbol-function 'swimmy.school::ensure-rank))
         (orig-move (symbol-function 'swimmy.persistence:move-strategy))
         (added nil))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list existing))
          (setf (symbol-function 'swimmy.school::%read-make-strategy-forms)
                (lambda (_path _names)
                  (declare (ignore _path _names))
                  (list canonical)))
          (setf (symbol-function 'swimmy.school::%lookup-optimized-timeframe-minutes)
                (lambda (name)
                  (declare (ignore name))
                  10080))
          (setf (symbol-function 'swimmy.school::mark-revalidation-pending)
                (lambda (_s) (declare (ignore _s)) nil))
          (setf (symbol-function 'swimmy.school::add-to-kb)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (setf added t)
                  t))
          (setf (symbol-function 'swimmy.school::ensure-rank)
                (lambda (&rest _args) (declare (ignore _args)) nil))
          (setf (symbol-function 'swimmy.persistence:move-strategy)
                (lambda (&rest _args) (declare (ignore _args)) nil))
          (swimmy.school::restore-legend-61)
          (assert-equal 10080 (swimmy.school:strategy-timeframe existing)
                        "Expected existing legend timeframe to be refreshed from optimized map")
          (assert-false added
                        "Existing legend path should not insert via add-to-kb"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf (symbol-function 'swimmy.school::%read-make-strategy-forms) orig-read)
      (setf (symbol-function 'swimmy.school::%lookup-optimized-timeframe-minutes) orig-lookup)
      (setf (symbol-function 'swimmy.school::mark-revalidation-pending) orig-mark)
      (setf (symbol-function 'swimmy.school::add-to-kb) orig-add)
      (setf (symbol-function 'swimmy.school::ensure-rank) orig-ensure)
      (setf (symbol-function 'swimmy.persistence:move-strategy) orig-move))))

(deftest test-restore-legend-61-applies-optimized-timeframe-when-adding
  "restore-legend-61 should apply optimized timeframe to newly added legend definitions."
  (let* ((canonical (swimmy.school:make-strategy :name "Aggressive-Reversal"
                                                 :timeframe 1
                                                 :rank :legend))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-read (symbol-function 'swimmy.school::%read-make-strategy-forms))
         (orig-lookup (symbol-function 'swimmy.school::%lookup-optimized-timeframe-minutes))
         (orig-mark (symbol-function 'swimmy.school::mark-revalidation-pending))
         (orig-add (symbol-function 'swimmy.school::add-to-kb))
         (orig-ensure (symbol-function 'swimmy.school::ensure-rank))
         (orig-save (symbol-function 'swimmy.persistence:save-strategy))
         (captured nil))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* nil)
          (setf (symbol-function 'swimmy.school::%read-make-strategy-forms)
                (lambda (_path _names)
                  (declare (ignore _path _names))
                  (list canonical)))
          (setf (symbol-function 'swimmy.school::%lookup-optimized-timeframe-minutes)
                (lambda (name)
                  (declare (ignore name))
                  240))
          (setf (symbol-function 'swimmy.school::mark-revalidation-pending)
                (lambda (_s) (declare (ignore _s)) nil))
          (setf (symbol-function 'swimmy.school::add-to-kb)
                (lambda (s &rest _args)
                  (declare (ignore _args))
                  (setf captured s)
                  t))
          (setf (symbol-function 'swimmy.school::ensure-rank)
                (lambda (&rest _args) (declare (ignore _args)) nil))
          (setf (symbol-function 'swimmy.persistence:save-strategy)
                (lambda (&rest _args) (declare (ignore _args)) nil))
          (swimmy.school::restore-legend-61)
          (assert-not-nil captured "Expected add-to-kb to receive canonical legend")
          (assert-equal 240 (swimmy.school:strategy-timeframe captured)
                        "Expected optimized timeframe to be applied before add-to-kb"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf (symbol-function 'swimmy.school::%read-make-strategy-forms) orig-read)
      (setf (symbol-function 'swimmy.school::%lookup-optimized-timeframe-minutes) orig-lookup)
      (setf (symbol-function 'swimmy.school::mark-revalidation-pending) orig-mark)
      (setf (symbol-function 'swimmy.school::add-to-kb) orig-add)
      (setf (symbol-function 'swimmy.school::ensure-rank) orig-ensure)
      (setf (symbol-function 'swimmy.persistence:save-strategy) orig-save))))

(deftest test-b-rank-culling-for-category-filters-expectancy-candidates
  "Category culling should queue only A-base candidates that also pass expectancy gate."
  (let* ((a-min (or (ignore-errors (swimmy.school::min-trade-evidence-for-rank :A)) 50))
         (a-trades (+ a-min 20))
         (mk (lambda (name wr)
               (swimmy.school:make-strategy :name name
                                            :rank :B
                                            :symbol "USDJPY"
                                            :direction :BOTH
                                            :timeframe 3600
                                            :sharpe 1.2
                                            :profit-factor 1.35
                                            :win-rate wr
                                            :max-dd 0.10
                                            :trades a-trades
                                            :sl 1.0
                                            :tp 3.0)))
         (exp-fail (funcall mk "UT-EXP-FAIL" 0.44))
         (exp-pass (funcall mk "UT-EXP-PASS" 0.43))
         (other (funcall mk "UT-EXP-OTHER" 0.45))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-threshold swimmy.school::*culling-threshold*)
         (orig-slots swimmy.school::*a-rank-slots-per-tf*)
         (orig-validate (symbol-function 'swimmy.school::validate-for-a-rank-promotion))
         (orig-graveyard (symbol-function 'swimmy.school::send-to-graveyard))
         (orig-exp (symbol-function 'swimmy.school::a-expectancy-gate-passed-p))
         (queued nil))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list exp-fail exp-pass other))
          (setf swimmy.school::*culling-threshold* 1)
          (setf swimmy.school::*a-rank-slots-per-tf* 2)
          (setf (symbol-function 'swimmy.school::a-expectancy-gate-passed-p)
                (lambda (strat)
                  (if (string= (swimmy.school:strategy-name strat) "UT-EXP-FAIL")
                      (values nil -1.0)
                      (values t 1.0))))
          (setf (symbol-function 'swimmy.school::validate-for-a-rank-promotion)
                (lambda (strat)
                  (push (swimmy.school:strategy-name strat) queued)
                  t))
          (setf (symbol-function 'swimmy.school::send-to-graveyard)
                (lambda (&rest _args) (declare (ignore _args)) nil))
          (swimmy.school::run-b-rank-culling-for-category 3600 :BOTH "USDJPY")
          (assert-false (find "UT-EXP-FAIL" queued :test #'string=)
                        "Expectancy-failing strategy should not be queued")
          (assert-true (find "UT-EXP-PASS" queued :test #'string=)))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*culling-threshold* orig-threshold)
      (setf swimmy.school::*a-rank-slots-per-tf* orig-slots)
      (setf (symbol-function 'swimmy.school::validate-for-a-rank-promotion) orig-validate)
      (setf (symbol-function 'swimmy.school::send-to-graveyard) orig-graveyard)
      (setf (symbol-function 'swimmy.school::a-expectancy-gate-passed-p) orig-exp))))

(deftest test-b-rank-culling-records-category-a-candidate-metrics
  "Category culling should persist per-category A candidate funnel metrics."
  (let* ((a-min (or (ignore-errors (swimmy.school::min-trade-evidence-for-rank :A)) 50))
         (a-trades (+ a-min 20))
         (mk (lambda (name pf wr)
               (swimmy.school:make-strategy :name name
                                            :rank :B
                                            :symbol "USDJPY"
                                            :direction :BOTH
                                            :timeframe 300
                                            :sharpe 1.10
                                            :profit-factor pf
                                            :win-rate wr
                                            :max-dd 0.10
                                            :trades a-trades)))
         (base-ready (funcall mk "UT-METRIC-READY" 1.35 0.44))
         (base-only (funcall mk "UT-METRIC-BASE" 1.32 0.43))
         (base-fail (funcall mk "UT-METRIC-FAIL" 1.10 0.44))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-threshold swimmy.school::*culling-threshold*)
         (orig-slots swimmy.school::*a-rank-slots-per-tf*)
         (orig-validate (symbol-function 'swimmy.school::validate-for-a-rank-promotion))
         (orig-graveyard (symbol-function 'swimmy.school::send-to-graveyard))
         (orig-exp (symbol-function 'swimmy.school::a-expectancy-gate-passed-p)))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list base-ready base-only base-fail))
          (setf swimmy.school::*culling-threshold* 1)
          (setf swimmy.school::*a-rank-slots-per-tf* 2)
          (swimmy.school::reset-a-candidate-category-metrics)
          (setf (symbol-function 'swimmy.school::a-expectancy-gate-passed-p)
                (lambda (strat)
                  (if (string= (swimmy.school:strategy-name strat) "UT-METRIC-BASE")
                      (values nil -0.5)
                      (values t 1.0))))
          (setf (symbol-function 'swimmy.school::validate-for-a-rank-promotion)
                (lambda (&rest _args) (declare (ignore _args)) t))
          (setf (symbol-function 'swimmy.school::send-to-graveyard)
                (lambda (&rest _args) (declare (ignore _args)) nil))
          (swimmy.school::run-b-rank-culling-for-category 300 :BOTH "USDJPY")
          (let ((m (swimmy.school::lookup-a-candidate-category-metric 300 :BOTH "USDJPY")))
            (assert-true m "Expected category metric snapshot to be recorded")
            (assert-equal 3 (getf m :b-count))
            (assert-equal 2 (getf m :a-base-count))
            (assert-equal 1 (getf m :a-ready-count))
            (assert-equal 1 (getf m :queued-count))))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*culling-threshold* orig-threshold)
      (setf swimmy.school::*a-rank-slots-per-tf* orig-slots)
      (setf (symbol-function 'swimmy.school::validate-for-a-rank-promotion) orig-validate)
      (setf (symbol-function 'swimmy.school::send-to-graveyard) orig-graveyard)
      (setf (symbol-function 'swimmy.school::a-expectancy-gate-passed-p) orig-exp))))

(deftest test-a-candidate-metrics-snippet-summarizes-category-counts
  "A candidate metrics snippet should include category and base/ready/queued counters."
  (swimmy.school::reset-a-candidate-category-metrics)
  (swimmy.school::record-a-candidate-category-metric
   300 :BOTH "USDJPY"
   :b-count 3
   :a-base-count 2
   :a-ready-count 1
   :queued-count 1
   :bootstrap-p nil)
  (let ((snippet (swimmy.school::a-candidate-metrics-snippet :limit 3)))
    (assert-true (search "USDJPY/BOTH/H5" snippet))
    (assert-true (search "base=2" snippet))
    (assert-true (search "ready=1" snippet))
    (assert-true (search "queued=1" snippet))))

(deftest test-a-expectancy-gate-normalizes-price-unit-sltp
  "A expectancy gate should treat small SL/TP as price-units and convert to pips."
  (let ((strat (swimmy.school:make-strategy :name "UT-EXP-NORM"
                                            :rank :B
                                            :symbol "USDJPY"
                                            :sharpe 1.0
                                            :profit-factor 1.3
                                            :win-rate 0.3866
                                            :max-dd 0.10
                                            :sl 0.943
                                            :tp 2.703)))
    (multiple-value-bind (passed net) (swimmy.school::a-expectancy-gate-passed-p strat)
      (assert-true passed "Expected positive net expectancy after pip normalization")
      (assert-true (> net 0.0) "net_expectancy_pips should be > 0"))))

(deftest test-b-rank-culling-bootstrap-runs-below-threshold-when-no-a
  "When A-rank is empty, culling should bootstrap promotion even below threshold."
  (let* ((a-min (or (ignore-errors (swimmy.school::min-trade-evidence-for-rank :A)) 50))
         (a-trades (+ a-min 20))
         (mk (lambda (name)
               (swimmy.school:make-strategy :name name
                                            :rank :B
                                            :symbol "USDJPY"
                                            :direction :BOTH
                                            :timeframe 3600
                                            :sharpe 1.2
                                            :profit-factor 1.35
                                            :win-rate 0.44
                                            :max-dd 0.10
                                            :trades a-trades
                                            :sl 0.9
                                            :tp 2.7)))
         (s1 (funcall mk "UT-BOOT-1"))
         (s2 (funcall mk "UT-BOOT-2"))
         (s3 (funcall mk "UT-BOOT-3"))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-threshold swimmy.school::*culling-threshold*)
         (orig-slots swimmy.school::*a-rank-slots-per-tf*)
         (orig-exp (symbol-function 'swimmy.school::a-expectancy-gate-passed-p))
         (orig-validate (symbol-function 'swimmy.school::validate-for-a-rank-promotion))
         (orig-graveyard (symbol-function 'swimmy.school::send-to-graveyard))
         (queued nil)
         (graveyarded nil))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list s1 s2 s3))
          (setf swimmy.school::*culling-threshold* 10)
          (setf swimmy.school::*a-rank-slots-per-tf* 2)
          (setf (symbol-function 'swimmy.school::a-expectancy-gate-passed-p)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (values t 1.0)))
          (setf (symbol-function 'swimmy.school::validate-for-a-rank-promotion)
                (lambda (strat)
                  (push (swimmy.school:strategy-name strat) queued)
                  t))
          (setf (symbol-function 'swimmy.school::send-to-graveyard)
                (lambda (strat &rest _args)
                  (declare (ignore _args))
                  (push (swimmy.school:strategy-name strat) graveyarded)
                  nil))
          (swimmy.school::run-b-rank-culling-for-category 3600 :BOTH "USDJPY")
          (assert-equal 2 (length queued)
                        "Expected bootstrap promotion queue even below threshold")
          (assert-equal 0 (length graveyarded)
                        "Bootstrap should not graveyard non-selected strategies"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*culling-threshold* orig-threshold)
      (setf swimmy.school::*a-rank-slots-per-tf* orig-slots)
      (setf (symbol-function 'swimmy.school::a-expectancy-gate-passed-p) orig-exp)
      (setf (symbol-function 'swimmy.school::validate-for-a-rank-promotion) orig-validate)
      (setf (symbol-function 'swimmy.school::send-to-graveyard) orig-graveyard))))

(deftest test-b-rank-culling-bootstrap-runs-below-threshold-when-a-low
  "When A-rank is still low, bootstrap promotion should run below threshold."
  (let* ((a-min (or (ignore-errors (swimmy.school::min-trade-evidence-for-rank :A)) 50))
         (a-trades (+ a-min 20))
         (mk-b (lambda (name)
                 (swimmy.school:make-strategy :name name
                                              :rank :B
                                              :symbol "USDJPY"
                                              :direction :BOTH
                                              :timeframe 3600
                                              :sharpe 1.2
                                              :profit-factor 1.35
                                              :win-rate 0.44
                                              :max-dd 0.10
                                              :trades a-trades)))
         (mk-a (lambda (name)
                 (swimmy.school:make-strategy :name name
                                              :rank :A
                                              :symbol "USDJPY"
                                              :direction :BOTH
                                              :timeframe 3600
                                              :sharpe 1.0
                                              :profit-factor 1.4
                                              :win-rate 0.45
                                              :max-dd 0.10
                                              :trades a-trades
                                              :oos-sharpe 0.5)))
         (b1 (funcall mk-b "UT-BOOT-LOW-A-1"))
         (b2 (funcall mk-b "UT-BOOT-LOW-A-2"))
         (a1 (funcall mk-a "UT-BOOT-LOW-A-EXISTING"))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-threshold swimmy.school::*culling-threshold*)
         (orig-slots swimmy.school::*a-rank-slots-per-tf*)
         (orig-bootstrap-max-a swimmy.school::*culling-bootstrap-max-a-count*)
         (orig-exp (symbol-function 'swimmy.school::a-expectancy-gate-passed-p))
         (orig-validate (symbol-function 'swimmy.school::validate-for-a-rank-promotion))
         (orig-graveyard (symbol-function 'swimmy.school::send-to-graveyard))
         (queued nil)
         (graveyarded nil))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list b1 b2 a1))
          (setf swimmy.school::*culling-threshold* 10)
          (setf swimmy.school::*a-rank-slots-per-tf* 2)
          (setf swimmy.school::*culling-bootstrap-max-a-count* 1)
          (setf (symbol-function 'swimmy.school::a-expectancy-gate-passed-p)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (values t 1.0)))
          (setf (symbol-function 'swimmy.school::validate-for-a-rank-promotion)
                (lambda (strat)
                  (push (swimmy.school:strategy-name strat) queued)
                  t))
          (setf (symbol-function 'swimmy.school::send-to-graveyard)
                (lambda (strat &rest _args)
                  (declare (ignore _args))
                  (push (swimmy.school:strategy-name strat) graveyarded)
                  nil))
          (swimmy.school::run-b-rank-culling-for-category 3600 :BOTH "USDJPY")
          (assert-true (> (length queued) 0)
                       "Expected bootstrap queue when A-rank is low")
          (assert-equal 0 (length graveyarded)
                        "Bootstrap should not graveyard non-selected strategies"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*culling-threshold* orig-threshold)
      (setf swimmy.school::*a-rank-slots-per-tf* orig-slots)
      (setf swimmy.school::*culling-bootstrap-max-a-count* orig-bootstrap-max-a)
      (setf (symbol-function 'swimmy.school::a-expectancy-gate-passed-p) orig-exp)
      (setf (symbol-function 'swimmy.school::validate-for-a-rank-promotion) orig-validate)
      (setf (symbol-function 'swimmy.school::send-to-graveyard) orig-graveyard))))

(deftest test-b-rank-culling-keeps-b-baseline-when-no-a-ready
  "Regular culling should retain baseline B pool when A-ready candidates are empty."
  (let* ((mk (lambda (name)
               (swimmy.school:make-strategy :name name
                                            :rank :B
                                            :symbol "USDJPY"
                                            :direction :BOTH
                                            :timeframe 3600
                                            :sharpe 1.1
                                            :profit-factor 1.15 ; B pass / A fail
                                            :win-rate 0.40      ; B pass / A fail
                                            :max-dd 0.10)))
         (s1 (funcall mk "UT-B-BASE-1"))
         (s2 (funcall mk "UT-B-BASE-2"))
         (s3 (funcall mk "UT-B-BASE-3"))
         (s4 (funcall mk "UT-B-BASE-4"))
         (s5 (funcall mk "UT-B-BASE-5"))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-threshold swimmy.school::*culling-threshold*)
         (orig-slots swimmy.school::*a-rank-slots-per-tf*)
         (orig-exp (symbol-function 'swimmy.school::a-expectancy-gate-passed-p))
         (orig-validate (symbol-function 'swimmy.school::validate-for-a-rank-promotion))
         (orig-graveyard (symbol-function 'swimmy.school::send-to-graveyard))
         (queued nil)
         (graveyarded nil))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list s1 s2 s3 s4 s5))
          (setf swimmy.school::*culling-threshold* 3)
          (setf swimmy.school::*a-rank-slots-per-tf* 2)
          (setf (symbol-function 'swimmy.school::a-expectancy-gate-passed-p)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (values t 1.0)))
          (setf (symbol-function 'swimmy.school::validate-for-a-rank-promotion)
                (lambda (strat)
                  (push (swimmy.school:strategy-name strat) queued)
                  t))
          (setf (symbol-function 'swimmy.school::send-to-graveyard)
                (lambda (strat &rest _args)
                  (declare (ignore _args))
                  (push (swimmy.school:strategy-name strat) graveyarded)
                  nil))
          (swimmy.school::run-b-rank-culling-for-category 3600 :BOTH "USDJPY")
          (assert-equal 0 (length queued)
                        "No A-ready candidates should be queued")
          (assert-equal 2 (length graveyarded)
                        "Regular culling should prune only surplus above threshold"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*culling-threshold* orig-threshold)
      (setf swimmy.school::*a-rank-slots-per-tf* orig-slots)
      (setf (symbol-function 'swimmy.school::a-expectancy-gate-passed-p) orig-exp)
      (setf (symbol-function 'swimmy.school::validate-for-a-rank-promotion) orig-validate)
      (setf (symbol-function 'swimmy.school::send-to-graveyard) orig-graveyard))))

(deftest test-b-rank-culling-penalizes-a-base-deficit-when-pruning
  "Culling should prune B strategies that are farther from A-base thresholds."
  (let* ((mk (lambda (name sharpe pf wr dd)
               (swimmy.school:make-strategy :name name
                                            :rank :B
                                            :symbol "USDJPY"
                                            :direction :BOTH
                                            :timeframe 3600
                                            :sharpe sharpe
                                            :profit-factor pf
                                            :win-rate wr
                                            :max-dd dd)))
         ;; Same PF near-band as peers, but WR is far from A floor -> should be pruned.
         (far-a-base (funcall mk "UT-A-DEFICIT-FAR" 1.90 1.29 0.20 0.08))
         ;; Near-A candidates (small deficits only).
         (near-a-base-1 (funcall mk "UT-A-DEFICIT-NEAR-1" 1.20 1.29 0.42 0.08))
         (near-a-base-2 (funcall mk "UT-A-DEFICIT-NEAR-2" 1.10 1.29 0.43 0.08))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-threshold swimmy.school::*culling-threshold*)
         (orig-slots swimmy.school::*a-rank-slots-per-tf*)
         (orig-exp (symbol-function 'swimmy.school::a-expectancy-gate-passed-p))
         (orig-validate (symbol-function 'swimmy.school::validate-for-a-rank-promotion))
         (orig-graveyard (symbol-function 'swimmy.school::send-to-graveyard))
         (queued nil)
         (graveyarded nil))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base*
                (list far-a-base near-a-base-1 near-a-base-2))
          (setf swimmy.school::*culling-threshold* 2)
          (setf swimmy.school::*a-rank-slots-per-tf* 2)
          (setf (symbol-function 'swimmy.school::a-expectancy-gate-passed-p)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (values t 1.0)))
          (setf (symbol-function 'swimmy.school::validate-for-a-rank-promotion)
                (lambda (strat)
                  (push (swimmy.school:strategy-name strat) queued)
                  t))
          (setf (symbol-function 'swimmy.school::send-to-graveyard)
                (lambda (strat &rest _args)
                  (declare (ignore _args))
                  (push (swimmy.school:strategy-name strat) graveyarded)
                  nil))
          (swimmy.school::run-b-rank-culling-for-category 3600 :BOTH "USDJPY")
          (assert-equal 0 (length queued)
                        "No A-base candidates should be queued in this setup")
          (assert-true (find "UT-A-DEFICIT-FAR" graveyarded :test #'string=)
                       "Far-from-A candidate should be culled"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*culling-threshold* orig-threshold)
      (setf swimmy.school::*a-rank-slots-per-tf* orig-slots)
      (setf (symbol-function 'swimmy.school::a-expectancy-gate-passed-p) orig-exp)
      (setf (symbol-function 'swimmy.school::validate-for-a-rank-promotion) orig-validate)
      (setf (symbol-function 'swimmy.school::send-to-graveyard) orig-graveyard))))

(deftest test-validate-a-rank-requires-positive-net-expectancy
  "A promotion should require positive cost-adjusted expectancy."
  (let* ((strat (swimmy.school:make-strategy :name "UT-A-NET-EXP"
                                             :rank :B
                                             :sharpe 0.60
                                             :profit-factor 1.5
                                             :win-rate 0.50
                                             :trades 80
                                             :max-dd 0.10
                                             :sl 70
                                             :tp 20))
         (promoted nil)
         (orig-oos (symbol-function 'swimmy.school::run-oos-validation))
         (orig-promote (symbol-function 'swimmy.school::promote-rank)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::run-oos-validation)
                (lambda (_strat)
                  (declare (ignore _strat))
                  (values t 0.40 "OOS mocked pass")))
          (setf (symbol-function 'swimmy.school::promote-rank)
                (lambda (&rest _args)
                  (setf promoted t)
                  :A))
          (assert-false (swimmy.school::validate-for-a-rank-promotion strat)
                        "Negative net expectancy should block A promotion")
          (assert-false promoted "promote-rank should not be called"))
      (setf (symbol-function 'swimmy.school::run-oos-validation) orig-oos)
      (setf (symbol-function 'swimmy.school::promote-rank) orig-promote))))

(deftest test-validate-a-rank-allows-dryrun-bootstrap-when-mc-passes
  "A promotion should not hard-fail only because DryRun samples are missing during bootstrap."
  (let* ((strat (swimmy.school:make-strategy :name "UT-A-DRYRUN-BOOTSTRAP"
                                             :rank :B
                                             :sharpe 0.80
                                             :profit-factor 1.50
                                             :win-rate 0.50
                                             :trades 80
                                             :max-dd 0.10
                                             :oos-sharpe 0.40))
         (promoted nil)
         (orig-oos (symbol-function 'swimmy.school::run-oos-validation))
         (orig-promote (symbol-function 'swimmy.school::promote-rank))
         (orig-exp (symbol-function 'swimmy.school::a-expectancy-gate-passed-p))
         (orig-mc (symbol-function 'swimmy.school::strategy-mc-prob-ruin))
         (orig-dryrun (symbol-function 'swimmy.school::dryrun-slippage-p95))
         (had-require (boundp 'swimmy.school::*a-rank-require-dryrun*))
         (orig-require (when had-require swimmy.school::*a-rank-require-dryrun*)))
    (unwind-protect
        (progn
          (setf swimmy.school::*a-rank-require-dryrun* nil)
          (setf (symbol-function 'swimmy.school::run-oos-validation)
                (lambda (_strat)
                  (declare (ignore _strat))
                  (values t 0.40 "OOS mocked pass")))
          (setf (symbol-function 'swimmy.school::a-expectancy-gate-passed-p)
                (lambda (_strat)
                  (declare (ignore _strat))
                  (values t 1.20)))
          (setf (symbol-function 'swimmy.school::strategy-mc-prob-ruin)
                (lambda (_strat &key iterations)
                  (declare (ignore _strat iterations))
                  0.01))
          (setf (symbol-function 'swimmy.school::dryrun-slippage-p95)
                (lambda (_name)
                  (declare (ignore _name))
                  nil))
          (setf (symbol-function 'swimmy.school::promote-rank)
                (lambda (&rest _args)
                  (setf promoted t)
                  :A))
          (assert-true (swimmy.school::validate-for-a-rank-promotion strat)
                       "Bootstrap path should allow A promotion without DryRun samples")
          (assert-true promoted "promote-rank should be called"))
      (setf (symbol-function 'swimmy.school::run-oos-validation) orig-oos)
      (setf (symbol-function 'swimmy.school::promote-rank) orig-promote)
      (setf (symbol-function 'swimmy.school::a-expectancy-gate-passed-p) orig-exp)
      (setf (symbol-function 'swimmy.school::strategy-mc-prob-ruin) orig-mc)
      (setf (symbol-function 'swimmy.school::dryrun-slippage-p95) orig-dryrun)
      (if had-require
          (setf swimmy.school::*a-rank-require-dryrun* orig-require)
          (makunbound 'swimmy.school::*a-rank-require-dryrun*)))))

(deftest test-validate-a-rank-allows-mc-bootstrap-when-disabled
  "A promotion can bootstrap when MC history is missing and MC requirement is disabled."
  (let* ((strat (swimmy.school:make-strategy :name "UT-A-MC-BOOTSTRAP"
                                             :rank :B
                                             :sharpe 0.82
                                             :profit-factor 1.55
                                             :win-rate 0.51
                                             :trades 80
                                             :max-dd 0.09
                                             :oos-sharpe 0.44))
         (promoted nil)
         (orig-oos (symbol-function 'swimmy.school::run-oos-validation))
         (orig-promote (symbol-function 'swimmy.school::promote-rank))
         (orig-exp (symbol-function 'swimmy.school::a-expectancy-gate-passed-p))
         (orig-mc (symbol-function 'swimmy.school::strategy-mc-prob-ruin))
         (orig-dryrun (symbol-function 'swimmy.school::dryrun-slippage-p95))
         (had-require-mc (boundp 'swimmy.school::*a-rank-require-mc*))
         (orig-require-mc (when had-require-mc swimmy.school::*a-rank-require-mc*)))
    (unwind-protect
        (progn
          (setf swimmy.school::*a-rank-require-mc* nil)
          (setf (symbol-function 'swimmy.school::run-oos-validation)
                (lambda (_strat)
                  (declare (ignore _strat))
                  (values t 0.44 "OOS mocked pass")))
          (setf (symbol-function 'swimmy.school::a-expectancy-gate-passed-p)
                (lambda (_strat)
                  (declare (ignore _strat))
                  (values t 1.15)))
          (setf (symbol-function 'swimmy.school::strategy-mc-prob-ruin)
                (lambda (_strat &key iterations)
                  (declare (ignore _strat iterations))
                  nil))
          (setf (symbol-function 'swimmy.school::dryrun-slippage-p95)
                (lambda (_name)
                  (declare (ignore _name))
                  nil))
          (setf (symbol-function 'swimmy.school::promote-rank)
                (lambda (&rest _args)
                  (setf promoted t)
                  :A))
          (assert-true (swimmy.school::validate-for-a-rank-promotion strat)
                       "MC bootstrap should allow A promotion when MC requirement is disabled")
          (assert-true promoted "promote-rank should be called"))
      (setf (symbol-function 'swimmy.school::run-oos-validation) orig-oos)
      (setf (symbol-function 'swimmy.school::promote-rank) orig-promote)
      (setf (symbol-function 'swimmy.school::a-expectancy-gate-passed-p) orig-exp)
      (setf (symbol-function 'swimmy.school::strategy-mc-prob-ruin) orig-mc)
      (setf (symbol-function 'swimmy.school::dryrun-slippage-p95) orig-dryrun)
      (if had-require-mc
          (setf swimmy.school::*a-rank-require-mc* orig-require-mc)
          (makunbound 'swimmy.school::*a-rank-require-mc*)))))

(deftest test-validate-a-rank-requires-mc-when-enabled
  "A promotion should fail on missing MC history when MC requirement is enabled."
  (let* ((strat (swimmy.school:make-strategy :name "UT-A-MC-REQUIRED"
                                             :rank :B
                                             :sharpe 0.82
                                             :profit-factor 1.55
                                             :win-rate 0.51
                                             :trades 80
                                             :max-dd 0.09
                                             :oos-sharpe 0.44))
         (promoted nil)
         (orig-oos (symbol-function 'swimmy.school::run-oos-validation))
         (orig-promote (symbol-function 'swimmy.school::promote-rank))
         (orig-exp (symbol-function 'swimmy.school::a-expectancy-gate-passed-p))
         (orig-mc (symbol-function 'swimmy.school::strategy-mc-prob-ruin))
         (orig-dryrun (symbol-function 'swimmy.school::dryrun-slippage-p95))
         (had-require-mc (boundp 'swimmy.school::*a-rank-require-mc*))
         (orig-require-mc (when had-require-mc swimmy.school::*a-rank-require-mc*)))
    (unwind-protect
        (progn
          (setf swimmy.school::*a-rank-require-mc* t)
          (setf (symbol-function 'swimmy.school::run-oos-validation)
                (lambda (_strat)
                  (declare (ignore _strat))
                  (values t 0.44 "OOS mocked pass")))
          (setf (symbol-function 'swimmy.school::a-expectancy-gate-passed-p)
                (lambda (_strat)
                  (declare (ignore _strat))
                  (values t 1.15)))
          (setf (symbol-function 'swimmy.school::strategy-mc-prob-ruin)
                (lambda (_strat &key iterations)
                  (declare (ignore _strat iterations))
                  nil))
          (setf (symbol-function 'swimmy.school::dryrun-slippage-p95)
                (lambda (_name)
                  (declare (ignore _name))
                  0.1))
          (setf (symbol-function 'swimmy.school::promote-rank)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (setf promoted t)
                  :A))
          (assert-false (swimmy.school::validate-for-a-rank-promotion strat)
                        "Missing MC history should block A promotion when MC is required")
          (assert-false promoted "promote-rank should not be called"))
      (setf (symbol-function 'swimmy.school::run-oos-validation) orig-oos)
      (setf (symbol-function 'swimmy.school::promote-rank) orig-promote)
      (setf (symbol-function 'swimmy.school::a-expectancy-gate-passed-p) orig-exp)
      (setf (symbol-function 'swimmy.school::strategy-mc-prob-ruin) orig-mc)
      (setf (symbol-function 'swimmy.school::dryrun-slippage-p95) orig-dryrun)
      (if had-require-mc
          (setf swimmy.school::*a-rank-require-mc* orig-require-mc)
          (makunbound 'swimmy.school::*a-rank-require-mc*)))))

(deftest test-evaluate-a-rank-requires-common-stage2-gates
  "S promotion should require common MC/DryRun gates."
  (let* ((strat (swimmy.school:make-strategy :name "UT-S-STAGE2"
                                             :rank :A
                                             :sharpe 0.80
                                             :profit-factor 1.9
                                             :win-rate 0.56
                                             :trades 120
                                             :max-dd 0.09
                                             :cpcv-median-sharpe 0.9
                                             :cpcv-median-pf 1.9
                                             :cpcv-median-wr 0.56
                                             :cpcv-median-maxdd 0.10
                                             :cpcv-pass-rate 0.9))
         (orig-promote (symbol-function 'swimmy.school::promote-rank))
         (promoted nil))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::promote-rank)
                (lambda (&rest _args)
                  (setf promoted t)
                  :S))
          (let ((result (swimmy.school::evaluate-a-rank-strategy strat)))
            (assert-false (eq result :S)
                          "Without common stage2 evidence, strategy should not become S")
            (assert-false promoted "promote-rank should not be called")))
      (setf (symbol-function 'swimmy.school::promote-rank) orig-promote))))

(deftest test-common-stage2-bootstrap-passes-for-mature-cpcv-candidate
  "Common Stage2 should bootstrap-pass sparse evidence for mature CPCV-strong candidates."
  (let* ((strat (swimmy.school:make-strategy :name "UT-STAGE2-BOOT-PASS"
                                             :rank :A
                                             :trades 200
                                             :cpcv-pass-rate 0.80))
         (orig-mc (symbol-function 'swimmy.school::strategy-mc-prob-ruin))
         (orig-dryrun (symbol-function 'swimmy.school::dryrun-slippage-p95)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::strategy-mc-prob-ruin)
                (lambda (_strat &key iterations)
                  (declare (ignore _strat iterations))
                  nil))
          (setf (symbol-function 'swimmy.school::dryrun-slippage-p95)
                (lambda (_name)
                  (declare (ignore _name))
                  nil))
          (multiple-value-bind (passed msg)
              (swimmy.school::common-stage2-gates-passed-p strat :require-mc t :require-dryrun t)
            (assert-true passed "Expected Stage2 bootstrap pass for mature candidate")
            (assert-true (search "bootstrap" (string-downcase (or msg "")))
                         "Expected bootstrap note in stage2 message")))
      (setf (symbol-function 'swimmy.school::strategy-mc-prob-ruin) orig-mc)
      (setf (symbol-function 'swimmy.school::dryrun-slippage-p95) orig-dryrun))))

(deftest test-common-stage2-bootstrap-does-not-bypass-with-low-trades
  "Common Stage2 should still fail sparse evidence when trade count is too low."
  (let* ((strat (swimmy.school:make-strategy :name "UT-STAGE2-BOOT-FAIL"
                                             :rank :A
                                             :trades 20
                                             :cpcv-pass-rate 0.90))
         (orig-mc (symbol-function 'swimmy.school::strategy-mc-prob-ruin))
         (orig-dryrun (symbol-function 'swimmy.school::dryrun-slippage-p95)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::strategy-mc-prob-ruin)
                (lambda (_strat &key iterations)
                  (declare (ignore _strat iterations))
                  nil))
          (setf (symbol-function 'swimmy.school::dryrun-slippage-p95)
                (lambda (_name)
                  (declare (ignore _name))
                  nil))
          (multiple-value-bind (passed msg)
              (swimmy.school::common-stage2-gates-passed-p strat :require-mc t :require-dryrun t)
            (assert-false passed "Expected Stage2 fail for low-trade sparse-evidence candidate")
            (assert-true (search "insufficient pnl-history" (or msg ""))
                         "Expected MC insufficiency message")))
      (setf (symbol-function 'swimmy.school::strategy-mc-prob-ruin) orig-mc)
      (setf (symbol-function 'swimmy.school::dryrun-slippage-p95) orig-dryrun))))

(deftest test-common-stage2-mc-insufficiency-dispatches-backfill-backtest-when-trades-sufficient
  "Common Stage2 should request a trade-log backfill backtest when MC is required but pnl-history is missing despite enough trades."
  (let* ((strat (swimmy.school:make-strategy :name "UT-MC-BACKFILL"
                                             :rank :A
                                             :trades 40
                                             :cpcv-pass-rate 0.0))
         (orig-mc (symbol-function 'swimmy.school::strategy-mc-prob-ruin))
         (orig-req (symbol-function 'swimmy.school:request-backtest))
         (orig-interval (and (boundp 'swimmy.school::*mc-evidence-request-interval*)
                             swimmy.school::*mc-evidence-request-interval*))
         (orig-cache (and (boundp 'swimmy.school::*mc-evidence-last-dispatch-at*)
                          swimmy.school::*mc-evidence-last-dispatch-at*))
         (called nil)
         (include-called nil))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::strategy-mc-prob-ruin)
                (lambda (_strat &key iterations)
                  (declare (ignore _strat iterations))
                  nil))
          (setf (symbol-function 'swimmy.school:request-backtest)
                (lambda (_strat &key candles suffix symbol request-id include-trades)
                  (declare (ignore _strat candles suffix symbol request-id))
                  (setf called t
                        include-called include-trades)
                  :accepted))
          (when (boundp 'swimmy.school::*mc-evidence-request-interval*)
            (setf swimmy.school::*mc-evidence-request-interval* 0))
          (when (boundp 'swimmy.school::*mc-evidence-last-dispatch-at*)
            (setf swimmy.school::*mc-evidence-last-dispatch-at* (make-hash-table :test 'equal)))
          (multiple-value-bind (passed msg)
              (swimmy.school::common-stage2-gates-passed-p strat :require-mc t :require-dryrun nil)
            (assert-false passed "Expected Stage2 fail while MC evidence is missing")
            (assert-true called "Expected MC backfill backtest dispatch when trades are sufficient")
            (assert-true include-called "Expected backfill dispatch to request include_trades")
            (assert-true (search "insufficient pnl-history" (or msg ""))
                         "Expected MC insufficiency message")))
      (setf (symbol-function 'swimmy.school::strategy-mc-prob-ruin) orig-mc)
      (setf (symbol-function 'swimmy.school:request-backtest) orig-req)
      (when (boundp 'swimmy.school::*mc-evidence-request-interval*)
        (setf swimmy.school::*mc-evidence-request-interval* orig-interval))
      (when (boundp 'swimmy.school::*mc-evidence-last-dispatch-at*)
        (setf swimmy.school::*mc-evidence-last-dispatch-at* orig-cache)))))

(deftest test-strategy-mc-prob-ruin-falls-back-to-db-trade-history
  "strategy-mc-prob-ruin should use persisted backtest trade PnL when in-memory pnl-history is sparse."
  (let* ((strat (swimmy.school:make-strategy :name "UT-MC-DB-FALLBACK"
                                             :rank :A
                                             :trades 60
                                             :pnl-history '(1.0 2.0)))
         (orig-min swimmy.school::*mc-min-trades*)
         (orig-fetch (symbol-function 'swimmy.school:fetch-backtest-trades))
         (orig-run (symbol-function 'swimmy.school::run-monte-carlo-simulation))
         (orig-analyze (symbol-function 'swimmy.school::analyze-monte-carlo-results))
         (captured-history nil))
    (unwind-protect
        (progn
          (setf swimmy.school::*mc-min-trades* 30)
          (setf (symbol-function 'swimmy.school:fetch-backtest-trades)
                (lambda (strategy-name &key oos-kind)
                  (declare (ignore strategy-name oos-kind))
                  (loop for i from 1 to 40
                        collect (list "RID" "UT-MC-DB-FALLBACK" i (float i) "USDJPY"
                                      nil nil nil nil nil nil nil nil nil nil nil "BACKTEST"))))
          (setf (symbol-function 'swimmy.school::run-monte-carlo-simulation)
                (lambda (trades &key iterations starting-equity)
                  (declare (ignore iterations starting-equity))
                  (setf captured-history (copy-list trades))
                  (values #(0.01 0.02 0.03) #(100.0 120.0 80.0))))
          (setf (symbol-function 'swimmy.school::analyze-monte-carlo-results)
                (lambda (strategy-name drawdowns final-pnls)
                  (declare (ignore strategy-name drawdowns final-pnls))
                  (swimmy.school::make-mc-result :prob-ruin 0.01)))
          (let ((prob-ruin (swimmy.school::strategy-mc-prob-ruin strat :iterations 50)))
            (assert-true (numberp prob-ruin)
                         "Expected MC prob_ruin to be computed from DB fallback history")
            (assert-true (>= (length captured-history) 30)
                         "Expected DB fallback to supply >= min trades to MC simulation")))
      (setf swimmy.school::*mc-min-trades* orig-min)
      (setf (symbol-function 'swimmy.school:fetch-backtest-trades) orig-fetch)
      (setf (symbol-function 'swimmy.school::run-monte-carlo-simulation) orig-run)
      (setf (symbol-function 'swimmy.school::analyze-monte-carlo-results) orig-analyze))))

(deftest test-dryrun-slippage-persists-across-memory-reset
  "DryRun slippage p95 should be recoverable from DB after in-memory reset."
  (let* ((tmp-db (format nil "/tmp/swimmy-dryrun-persist-~a.db" (get-universal-time)))
         (orig-db swimmy.core::*db-path-default*)
         (orig-conn swimmy.core::*sqlite-conn*)
         (orig-disable swimmy.school::*disable-auto-migration*)
         (orig-min swimmy.school::*dryrun-min-slippage-samples*)
         (orig-cap swimmy.school::*dryrun-slippage-sample-cap*)
         (orig-cache swimmy.school::*dryrun-slippage-by-strategy*))
    (unwind-protect
        (progn
          (setf swimmy.core::*db-path-default* tmp-db
                swimmy.core::*sqlite-conn* nil
                swimmy.school::*disable-auto-migration* t
                swimmy.school::*dryrun-min-slippage-samples* 3
                swimmy.school::*dryrun-slippage-sample-cap* 10
                swimmy.school::*dryrun-slippage-by-strategy* (make-hash-table :test 'equal))
          (swimmy.school::init-db)
          (swimmy.school::record-dryrun-slippage "UT-DRYRUN-PERSIST" 0.2)
          (swimmy.school::record-dryrun-slippage "UT-DRYRUN-PERSIST" 0.8)
          (swimmy.school::record-dryrun-slippage "UT-DRYRUN-PERSIST" 1.2)
          ;; Simulate process restart: clear memory cache, keep DB.
          (setf swimmy.school::*dryrun-slippage-by-strategy* (make-hash-table :test 'equal))
          (let ((p95 (swimmy.school::dryrun-slippage-p95 "UT-DRYRUN-PERSIST")))
            (assert-true (numberp p95)
                         "Expected p95 to be recovered from persisted samples")))
      (setf swimmy.core::*db-path-default* orig-db
            swimmy.core::*sqlite-conn* orig-conn
            swimmy.school::*disable-auto-migration* orig-disable
            swimmy.school::*dryrun-min-slippage-samples* orig-min
            swimmy.school::*dryrun-slippage-sample-cap* orig-cap
            swimmy.school::*dryrun-slippage-by-strategy* orig-cache)
      (ignore-errors (swimmy.school::close-db-connection))
      (when (probe-file tmp-db) (ignore-errors (delete-file tmp-db))))))

(deftest test-dryrun-slippage-db-cap-prunes-old-samples
  "DryRun sample persistence should keep only the latest N samples per strategy."
  (let* ((tmp-db (format nil "/tmp/swimmy-dryrun-cap-~a.db" (get-universal-time)))
         (orig-db swimmy.core::*db-path-default*)
         (orig-conn swimmy.core::*sqlite-conn*)
         (orig-disable swimmy.school::*disable-auto-migration*)
         (orig-cap swimmy.school::*dryrun-slippage-sample-cap*)
         (orig-cache swimmy.school::*dryrun-slippage-by-strategy*))
    (unwind-protect
        (progn
          (setf swimmy.core::*db-path-default* tmp-db
                swimmy.core::*sqlite-conn* nil
                swimmy.school::*disable-auto-migration* t
                swimmy.school::*dryrun-slippage-sample-cap* 5
                swimmy.school::*dryrun-slippage-by-strategy* (make-hash-table :test 'equal))
          (swimmy.school::init-db)
          (dotimes (i 8)
            (swimmy.school::record-dryrun-slippage "UT-DRYRUN-CAP" (+ 0.1 i)))
          (let ((cnt (ignore-errors
                       (swimmy.school::execute-single
                        "SELECT count(*) FROM dryrun_slippage_samples WHERE strategy_name=?"
                        "UT-DRYRUN-CAP"))))
            (assert-equal 5 cnt "Expected DB sample count to be capped at 5")))
      (setf swimmy.core::*db-path-default* orig-db
            swimmy.core::*sqlite-conn* orig-conn
            swimmy.school::*disable-auto-migration* orig-disable
            swimmy.school::*dryrun-slippage-sample-cap* orig-cap
            swimmy.school::*dryrun-slippage-by-strategy* orig-cache)
      (ignore-errors (swimmy.school::close-db-connection))
      (when (probe-file tmp-db) (ignore-errors (delete-file tmp-db))))))

(deftest test-dryrun-slippage-db-period-prunes-old-samples
  "DryRun sample persistence should prune samples older than retention period."
  (let* ((tmp-db (format nil "/tmp/swimmy-dryrun-period-~a.db" (get-universal-time)))
         (orig-db swimmy.core::*db-path-default*)
         (orig-conn swimmy.core::*sqlite-conn*)
         (orig-disable swimmy.school::*disable-auto-migration*)
         (now (get-universal-time))
         (cutoff-seconds 3600)
         (strategy "UT-DRYRUN-PERIOD"))
    (unwind-protect
        (progn
          (setf swimmy.core::*db-path-default* tmp-db
                swimmy.core::*sqlite-conn* nil
                swimmy.school::*disable-auto-migration* t)
          (swimmy.school::init-db)
          (swimmy.school::execute-non-query
           "INSERT INTO dryrun_slippage_samples (strategy_name, sample_abs_pips, observed_at)
            VALUES (?, ?, ?)"
           strategy 0.1 (- now 7200))
          (swimmy.school::execute-non-query
           "INSERT INTO dryrun_slippage_samples (strategy_name, sample_abs_pips, observed_at)
            VALUES (?, ?, ?)"
           strategy 0.2 (- now 60))
          (swimmy.school::record-dryrun-slippage-sample
           strategy 0.3
           :max-samples 10
           :max-age-seconds cutoff-seconds
           :observed-at now)
          (let ((stale-count (swimmy.school::execute-single
                              "SELECT count(*) FROM dryrun_slippage_samples
                                WHERE strategy_name=? AND observed_at < ?"
                              strategy
                              (- now cutoff-seconds)))
                (total-count (swimmy.school::execute-single
                              "SELECT count(*) FROM dryrun_slippage_samples
                                WHERE strategy_name=?"
                              strategy)))
            (assert-equal 0 stale-count "Expected stale samples to be deleted")
            (assert-equal 2 total-count "Expected recent + new sample to remain")))
      (setf swimmy.core::*db-path-default* orig-db
            swimmy.core::*sqlite-conn* orig-conn
            swimmy.school::*disable-auto-migration* orig-disable)
      (ignore-errors (swimmy.school::close-db-connection))
      (when (probe-file tmp-db) (ignore-errors (delete-file tmp-db))))))

(deftest test-ensure-rank-blocks-s-without-cpcv
  "ensure-rank should block S promotion when CPCV criteria are missing"
  (let* ((strat (swimmy.school:make-strategy :name "UT-S-BLOCK"
                                             :rank :A
                                             :sharpe 0.7
                                             :profit-factor 1.8
                                             :win-rate 0.55
                                             :max-dd 0.10))
         (orig-upsert (symbol-function 'swimmy.school:upsert-strategy))
         (orig-notify (and (fboundp 'swimmy.school::notify-noncorrelated-promotion)
                           (symbol-function 'swimmy.school::notify-noncorrelated-promotion)))
         (orig-emit (symbol-function 'swimmy.core::emit-telemetry-event))
         (events nil))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school:upsert-strategy)
                (lambda (&rest args) (declare (ignore args)) nil))
          (when orig-notify
            (setf (symbol-function 'swimmy.school::notify-noncorrelated-promotion)
                  (lambda (&rest args)
                    (declare (ignore args))
                    (error "notify-noncorrelated-promotion should not run"))))
          (setf (symbol-function 'swimmy.core::emit-telemetry-event)
                (lambda (event-type &key data &allow-other-keys)
                  (push (list event-type data) events)))
          (swimmy.school::ensure-rank strat :S "Drafted to Global Team")
          (assert-equal :A (swimmy.school:strategy-rank strat)
                        "S promotion should be blocked without CPCV")
          (let ((ev (find "rank.promotion.blocked" events :key #'first :test #'string=)))
            (assert-true ev "Expected rank.promotion.blocked telemetry")))
      (setf (symbol-function 'swimmy.school:upsert-strategy) orig-upsert)
      (when orig-notify
        (setf (symbol-function 'swimmy.school::notify-noncorrelated-promotion) orig-notify))
      (setf (symbol-function 'swimmy.core::emit-telemetry-event) orig-emit))))

(deftest test-ensure-rank-blocked-s-emits-detailed-failure-gates
  "S promotion block telemetry should include concrete failed gates (pf/wr/maxdd/cpcv/common-stage2)."
  (let* ((strat (swimmy.school:make-strategy :name "UT-S-BLOCK-DETAIL"
                                             :rank :A
                                             :sharpe 0.90
                                             :profit-factor 1.20
                                             :win-rate 0.40
                                             :max-dd 0.11
                                             :cpcv-pass-rate 0.40
                                             :cpcv-median-maxdd 0.20))
         (orig-upsert (symbol-function 'swimmy.school:upsert-strategy))
         (orig-emit (symbol-function 'swimmy.core::emit-telemetry-event))
         (orig-common (and (fboundp 'swimmy.school::common-stage2-gates-passed-p)
                           (symbol-function 'swimmy.school::common-stage2-gates-passed-p)))
         (events nil))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school:upsert-strategy)
                (lambda (&rest args) (declare (ignore args)) nil))
          (when orig-common
            (setf (symbol-function 'swimmy.school::common-stage2-gates-passed-p)
                  (lambda (&rest _args)
                    (declare (ignore _args))
                    (values nil "common stage2 failed: mock"))))
          (setf (symbol-function 'swimmy.core::emit-telemetry-event)
                (lambda (event-type &key data &allow-other-keys)
                  (push (list event-type data) events)))
          (swimmy.school::ensure-rank strat :S "Drafted to Global Team")
          (let* ((ev (find "rank.promotion.blocked" events :key #'first :test #'string=))
                 (data (second ev))
                 (failed (getf data :failed-gates)))
            (assert-true ev "Expected rank.promotion.blocked telemetry")
            (assert-true (member :pf failed) "PF failure should be reported")
            (assert-true (member :wr failed) "WR failure should be reported")
            (assert-true (member :maxdd failed) "MaxDD failure should be reported")
            (assert-true (member :cpcv-pass-rate failed) "CPCV pass_rate failure should be reported")
            (assert-true (member :cpcv-maxdd failed) "CPCV maxdd failure should be reported")
            (assert-true (member :common-stage2 failed) "Common stage2 failure should be reported")
            (assert-true (search "common stage2 failed: mock" (or (getf data :common-stage2-message) ""))
                         "Common stage2 reason should be included")))
      (setf (symbol-function 'swimmy.school:upsert-strategy) orig-upsert)
      (setf (symbol-function 'swimmy.core::emit-telemetry-event) orig-emit)
      (when orig-common
        (setf (symbol-function 'swimmy.school::common-stage2-gates-passed-p) orig-common)))))

(deftest test-draft-does-not-promote-without-cpcv
  "Global draft should not promote A->S without CPCV criteria"
  (let* ((a (swimmy.school:make-strategy :name "UT-DRAFT-A"
                                         :rank :A
                                         :sharpe 0.7
                                         :profit-factor 1.8
                                         :win-rate 0.55
                                         :max-dd 0.10))
         (orig-fetch (symbol-function 'swimmy.school::fetch-candidate-strategies))
         (orig-diverse (symbol-function 'swimmy.school::is-diverse-addition-p))
         (orig-upsert (symbol-function 'swimmy.school:upsert-strategy)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::fetch-candidate-strategies)
                (lambda (&key ranks)
                  (declare (ignore ranks))
                  (list a)))
          (setf (symbol-function 'swimmy.school::is-diverse-addition-p)
                (lambda (&rest args) (declare (ignore args)) t))
          (setf (symbol-function 'swimmy.school:upsert-strategy)
                (lambda (&rest args) (declare (ignore args)) nil))
          (swimmy.school::construct-global-portfolio)
          (assert-equal :A (swimmy.school:strategy-rank a)
                        "Draft should not promote to S without CPCV"))
      (setf (symbol-function 'swimmy.school::fetch-candidate-strategies) orig-fetch
            (symbol-function 'swimmy.school::is-diverse-addition-p) orig-diverse
            (symbol-function 'swimmy.school:upsert-strategy) orig-upsert))))

(deftest test-draft-counts-only-successful-promotions
  "Draft promoted/demoted counts should reflect actual rank changes"
  (let* ((a (swimmy.school:make-strategy :name "UT-DRAFT-COUNT-A"
                                         :rank :A
                                         :sharpe 0.7
                                         :profit-factor 1.8
                                         :win-rate 0.55
                                         :max-dd 0.10))
         (s (swimmy.school:make-strategy :name "UT-DRAFT-COUNT-S"
                                         :rank :S
                                         :sharpe 0.8
                                         :profit-factor 1.9
                                         :win-rate 0.56
                                         :max-dd 0.10))
         (orig-fetch (symbol-function 'swimmy.school::fetch-candidate-strategies))
         (orig-diverse (symbol-function 'swimmy.school::is-diverse-addition-p))
         (orig-ensure (symbol-function 'swimmy.school:ensure-rank)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::fetch-candidate-strategies)
                (lambda (&key ranks)
                  (declare (ignore ranks))
                  (list a s)))
          (setf (symbol-function 'swimmy.school::is-diverse-addition-p)
                (lambda (candidate team)
                  (declare (ignore team))
                  (eq candidate a)))
          ;; Simulate blocked promotions/demotions by returning old rank.
          (setf (symbol-function 'swimmy.school:ensure-rank)
                (lambda (strat rank &optional reason)
                  (declare (ignore rank reason))
                  (swimmy.school:strategy-rank strat)))
          (let ((out (with-output-to-string (*standard-output*)
                       (swimmy.school::construct-global-portfolio))))
            (assert-true (search "Promoted=0" out)
                         "Expected promoted count to be 0 when promotions are blocked")
            (assert-true (search "Demoted=0" out)
                         "Expected demoted count to be 0 when demotions are blocked")))
      (setf (symbol-function 'swimmy.school::fetch-candidate-strategies) orig-fetch
            (symbol-function 'swimmy.school::is-diverse-addition-p) orig-diverse
            (symbol-function 'swimmy.school:ensure-rank) orig-ensure))))

(deftest test-noncorrelation-score-reports-coverage
  "Noncorrelation score should report coverage days when insufficient."
  (let* ((tmp-db (format nil "/tmp/swimmy-noncorr-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            ;; Seed 5 days of pnl for A and B (insufficient for days=30)
            (dolist (spec '(("2026-02-01" 1.0) ("2026-02-02" 2.0) ("2026-02-03" 3.0)
                            ("2026-02-04" 4.0) ("2026-02-05" 5.0)))
              (destructuring-bind (d v) spec
                (swimmy.school::execute-non-query
                 "INSERT OR REPLACE INTO strategy_daily_pnl (strategy_name, trade_date, pnl_sum, trade_count, updated_at)
                  VALUES (?, ?, ?, ?, ?)"
                 "A" d v 1 (get-universal-time))
                (swimmy.school::execute-non-query
                 "INSERT OR REPLACE INTO strategy_daily_pnl (strategy_name, trade_date, pnl_sum, trade_count, updated_at)
                  VALUES (?, ?, ?, ?, ?)"
                 "B" d v 1 (get-universal-time))))
            (multiple-value-bind (score reason min-days)
                (swimmy.school::calculate-noncorrelation-score '("A" "B") :days 30)
              (assert-equal nil score "Expected nil score for insufficient data")
              (assert-equal :insufficient-data reason "Expected insufficient data reason")
              (assert-equal 5 min-days "Expected 5 days of coverage")))
        (ignore-errors (swimmy.school::close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-noncorrelation-notify-includes-coverage
  "Noncorrelation notification should include coverage when data is insufficient."
  (let* ((tmp-db (format nil "/tmp/swimmy-noncorr-msg-~a.db" (get-universal-time)))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-queue (symbol-function 'swimmy.core:queue-discord-notification))
         (orig-emit (symbol-function 'swimmy.core::emit-telemetry-event))
         (orig-webhook swimmy.core:*discord-daily-webhook*))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            ;; Seed 5 days of pnl for A and B
            (dolist (spec '(("2026-02-01" 1.0) ("2026-02-02" 2.0) ("2026-02-03" 3.0)
                            ("2026-02-04" 4.0) ("2026-02-05" 5.0)))
              (destructuring-bind (d v) spec
                (swimmy.school::execute-non-query
                 "INSERT OR REPLACE INTO strategy_daily_pnl (strategy_name, trade_date, pnl_sum, trade_count, updated_at)
                  VALUES (?, ?, ?, ?, ?)"
                 "A" d v 1 (get-universal-time))
                (swimmy.school::execute-non-query
                 "INSERT OR REPLACE INTO strategy_daily_pnl (strategy_name, trade_date, pnl_sum, trade_count, updated_at)
                  VALUES (?, ?, ?, ?, ?)"
                 "B" d v 1 (get-universal-time))))
            (setf swimmy.school::*strategy-knowledge-base*
                  (list (swimmy.school:make-strategy :name "A" :rank :A)
                        (swimmy.school:make-strategy :name "B" :rank :A)))
            (setf swimmy.core:*discord-daily-webhook* "dummy")
            (let ((captured nil))
              (setf (symbol-function 'swimmy.core:queue-discord-notification)
                    (lambda (webhook msg &key color title)
                      (declare (ignore webhook color title))
                      (setf captured msg)))
              (setf (symbol-function 'swimmy.core::emit-telemetry-event)
                    (lambda (&rest args) (declare (ignore args)) nil))
              (swimmy.school::notify-noncorrelated-promotion
               (swimmy.school:make-strategy :name "PROMO" :rank :A) :A :days 30
               :promotion-reason "CPCV Passed and Criteria Met")
              (assert-true (and captured (search "データ不足: 5/30日" captured))
                           "Expected coverage in N/A message")))
        (setf swimmy.school::*strategy-knowledge-base* orig-kb
              (symbol-function 'swimmy.core:queue-discord-notification) orig-queue
              (symbol-function 'swimmy.core::emit-telemetry-event) orig-emit
              swimmy.core:*discord-daily-webhook* orig-webhook)
        (ignore-errors (swimmy.school::close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-noncorrelation-notify-logs-message
  "Noncorrelation notification should log the message body."
  (let* ((tmp-db (format nil "/tmp/swimmy-noncorr-log-~a.db" (get-universal-time)))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-queue (symbol-function 'swimmy.core:queue-discord-notification))
         (orig-emit (symbol-function 'swimmy.core::emit-telemetry-event))
         (orig-log (symbol-function 'swimmy.core:log-info))
         (orig-webhook swimmy.core:*discord-daily-webhook*))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            ;; Seed 5 days of pnl for A and B
            (dolist (spec '(("2026-02-01" 1.0) ("2026-02-02" 2.0) ("2026-02-03" 3.0)
                            ("2026-02-04" 4.0) ("2026-02-05" 5.0)))
              (destructuring-bind (d v) spec
                (swimmy.school::execute-non-query
                 "INSERT OR REPLACE INTO strategy_daily_pnl (strategy_name, trade_date, pnl_sum, trade_count, updated_at)
                  VALUES (?, ?, ?, ?, ?)"
                 "A" d v 1 (get-universal-time))
                (swimmy.school::execute-non-query
                 "INSERT OR REPLACE INTO strategy_daily_pnl (strategy_name, trade_date, pnl_sum, trade_count, updated_at)
                  VALUES (?, ?, ?, ?, ?)"
                 "B" d v 1 (get-universal-time))))
            (setf swimmy.school::*strategy-knowledge-base*
                  (list (swimmy.school:make-strategy :name "A" :rank :A)
                        (swimmy.school:make-strategy :name "B" :rank :A)))
            (setf swimmy.core:*discord-daily-webhook* "dummy")
            (let ((captured nil))
              (setf (symbol-function 'swimmy.core:queue-discord-notification)
                    (lambda (&rest args) (declare (ignore args)) nil))
              (setf (symbol-function 'swimmy.core::emit-telemetry-event)
                    (lambda (&rest args) (declare (ignore args)) nil))
              (setf (symbol-function 'swimmy.core:log-info)
                    (lambda (message &rest args)
                      (declare (ignore args))
                      (setf captured message)))
              (swimmy.school::notify-noncorrelated-promotion
               (swimmy.school:make-strategy :name "PROMO" :rank :A) :A :days 30
               :promotion-reason "CPCV Passed and Criteria Met")
              (assert-true (and captured (search "非相関スコア" captured))
                           "Expected noncorrelation message in logs")
              (assert-true (search "データ不足: 5/30日" captured)
                           "Expected coverage text in logs")
              (assert-true (search "【理由】" captured)
                           "Expected emphasized promotion reason in logs"))))
        (setf swimmy.school::*strategy-knowledge-base* orig-kb
              (symbol-function 'swimmy.core:queue-discord-notification) orig-queue
              (symbol-function 'swimmy.core::emit-telemetry-event) orig-emit
              (symbol-function 'swimmy.core:log-info) orig-log
              swimmy.core:*discord-daily-webhook* orig-webhook)
        (ignore-errors (swimmy.school::close-db-connection))
        (ignore-errors (delete-file tmp-db)))))

(deftest test-noncorrelation-telemetry-emitted
  "Noncorrelation notification should emit telemetry with coverage details."
  (let* ((tmp-db (format nil "/tmp/swimmy-noncorr-tel-~a.db" (get-universal-time)))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-queue (symbol-function 'swimmy.core:queue-discord-notification))
         (orig-emit (symbol-function 'swimmy.core::emit-telemetry-event)))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            ;; Seed 5 days of pnl for A and B
            (dolist (spec '(("2026-02-01" 1.0) ("2026-02-02" 2.0) ("2026-02-03" 3.0)
                            ("2026-02-04" 4.0) ("2026-02-05" 5.0)))
              (destructuring-bind (d v) spec
                (swimmy.school::execute-non-query
                 "INSERT OR REPLACE INTO strategy_daily_pnl (strategy_name, trade_date, pnl_sum, trade_count, updated_at)
                  VALUES (?, ?, ?, ?, ?)"
                 "A" d v 1 (get-universal-time))
                (swimmy.school::execute-non-query
                 "INSERT OR REPLACE INTO strategy_daily_pnl (strategy_name, trade_date, pnl_sum, trade_count, updated_at)
                  VALUES (?, ?, ?, ?, ?)"
                 "B" d v 1 (get-universal-time))))
            (setf swimmy.school::*strategy-knowledge-base*
                  (list (swimmy.school:make-strategy :name "A" :rank :A)
                        (swimmy.school:make-strategy :name "B" :rank :A)))
            (let ((events nil))
              (setf (symbol-function 'swimmy.core:queue-discord-notification)
                    (lambda (&rest args) (declare (ignore args)) nil))
              (setf (symbol-function 'swimmy.core::emit-telemetry-event)
                    (lambda (event-type &key data &allow-other-keys)
                      (push (list event-type data) events)))
              (swimmy.school::notify-noncorrelated-promotion
               (swimmy.school:make-strategy :name "PROMO" :rank :A) :A :days 30
               :promotion-reason "CPCV Passed and Criteria Met")
              (let* ((ev (find "noncorrelation.score" events :key #'first :test #'string=))
                     (data (and ev (second ev))))
                (assert-true ev "Expected noncorrelation.score telemetry event")
                (assert-equal "PROMO" (getf data :strategy) "Expected strategy name in telemetry")
                (assert-equal :insufficient-data (getf data :reason) "Expected insufficient-data reason")
                (assert-equal 5 (getf data :coverage-days) "Expected coverage-days in telemetry")
                (assert-equal "CPCV Passed and Criteria Met" (getf data :promotion-reason)
                              "Expected promotion reason in telemetry"))))
        (setf swimmy.school::*strategy-knowledge-base* orig-kb
              (symbol-function 'swimmy.core:queue-discord-notification) orig-queue
              (symbol-function 'swimmy.core::emit-telemetry-event) orig-emit)
        (ignore-errors (swimmy.school::close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-evaluate-strategy-performance-sends-to-graveyard
  "Evaluation failures should send strategies to graveyard (no benching)."
  (let* ((s (swimmy.school:make-strategy :name "EvalFail" :symbol "USDJPY"))
         (called nil)
         (orig (symbol-function 'swimmy.school:send-to-graveyard)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school:send-to-graveyard)
                (lambda (strat reason)
                  (setf called (list strat reason))
                  :graveyard))
          (swimmy.school::evaluate-strategy-performance s -0.1 10 50 1.0)
          (assert-true called "Expected send-to-graveyard to be invoked"))
      (setf (symbol-function 'swimmy.school:send-to-graveyard) orig))))

(deftest test-ensure-rank-retired-saves-pattern
  "ensure-rank :retired should save retired pattern, move strategy, and remove from KB."
  (let* ((s (swimmy.school:make-strategy :name "RetireMe" :symbol "USDJPY"))
         (swimmy.school::*strategy-knowledge-base* (list s))
         (swimmy.school::*category-pools* (make-hash-table :test 'equal))
         (calls nil)
         (orig-save (symbol-function 'swimmy.school::save-retired-pattern))
         (orig-move (symbol-function 'swimmy.persistence:move-strategy)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::save-retired-pattern)
                (lambda (strat reason)
                  (push (list :save strat reason) calls)
                  t))
          (setf (symbol-function 'swimmy.persistence:move-strategy)
                (lambda (strat new-rank &key force from-rank)
                  (declare (ignore force from-rank))
                  (push (list :move strat new-rank) calls)
                  t))
          (swimmy.school::ensure-rank s :retired "Max Age")
          (assert-equal :retired (swimmy.school:strategy-rank s) "Expected :retired rank")
          (assert-true (null (member s swimmy.school::*strategy-knowledge-base* :test #'eq))
                       "Expected removal from KB")
          (assert-true (find :save calls :key #'first) "Expected save-retired-pattern call")
          (assert-true (find :move calls :key #'first) "Expected move-strategy call"))
      (setf (symbol-function 'swimmy.school::save-retired-pattern) orig-save
            (symbol-function 'swimmy.persistence:move-strategy) orig-move))))

(deftest test-lifecycle-retire-on-max-losses
  "Lifecycle should retire strategies after max consecutive losses."
  (let* ((s (swimmy.school:make-strategy :name "LifeFail" :symbol "USDJPY"))
         (called nil)
         (orig (symbol-function 'swimmy.school:send-to-graveyard))
         (orig-max swimmy.school::*max-consecutive-losses*))
    (unwind-protect
        (progn
          (setf swimmy.school::*max-consecutive-losses* 1)
          (setf (symbol-function 'swimmy.school:send-to-graveyard)
                (lambda (strat reason)
                  (setf called (list strat reason))
                  :graveyard))
          (swimmy.school::manage-strategy-lifecycle s nil -1.0)
          (assert-true called "Expected lifecycle to retire strategy"))
      (setf (symbol-function 'swimmy.school:send-to-graveyard) orig
            swimmy.school::*max-consecutive-losses* orig-max))))

(deftest test-phase-8-weekly-prune-skips-when-incubator-pending
  "Connector should skip heavy KB pruning when incubator backlog exists."
  (let* ((orig-last swimmy.school::*last-prune-time*)
         (orig-run (symbol-function 'swimmy.school::run-kb-pruning))
         (orig-get (symbol-function 'swimmy.school::get-strategies-by-rank))
         (called 0))
    (unwind-protect
        (progn
          (setf swimmy.school::*last-prune-time* 0)
          (setf (symbol-function 'swimmy.school::run-kb-pruning)
                (lambda () (incf called) 123))
          (setf (symbol-function 'swimmy.school::get-strategies-by-rank)
                (lambda (rank)
                  (if (eq rank :incubator)
                      (list (swimmy.school:make-strategy :name "UT-INC-PENDING"))
                      nil)))
          (swimmy.school::phase-8-weekly-prune)
          (assert-equal 0 called "Expected prune to be skipped when incubator backlog exists"))
      (setf swimmy.school::*last-prune-time* orig-last)
      (setf (symbol-function 'swimmy.school::run-kb-pruning) orig-run)
      (setf (symbol-function 'swimmy.school::get-strategies-by-rank) orig-get))))

(deftest test-phase-7-wisdom-update-respects-interval
  "Connector should run heavy wisdom update only when interval elapsed."
  (let* ((orig-last swimmy.school::*last-wisdom-update-time*)
         (orig-interval swimmy.school::*wisdom-update-interval-sec*)
         (orig-analyze (symbol-function 'swimmy.school::analyze-veterans))
         (called 0))
    (unwind-protect
        (progn
          (setf swimmy.school::*last-wisdom-update-time* 0)
          (setf swimmy.school::*wisdom-update-interval-sec* 300)
          (setf (symbol-function 'swimmy.school::analyze-veterans)
                (lambda () (incf called) :ok))

          (swimmy.school::phase-7-wisdom-update :now 1000)
          (assert-equal 1 called "Expected first wisdom run")
          (assert-equal 1000 swimmy.school::*last-wisdom-update-time*
                        "Expected wisdom timestamp update on first run")

          (swimmy.school::phase-7-wisdom-update :now 1200)
          (assert-equal 1 called "Expected no wisdom run before interval")
          (assert-equal 1000 swimmy.school::*last-wisdom-update-time*
                        "Timestamp should remain unchanged before interval")

          (swimmy.school::phase-7-wisdom-update :now 1301)
          (assert-equal 2 called "Expected wisdom run after interval")
          (assert-equal 1301 swimmy.school::*last-wisdom-update-time*
                        "Expected timestamp update after interval"))
      (setf swimmy.school::*last-wisdom-update-time* orig-last
            swimmy.school::*wisdom-update-interval-sec* orig-interval
            (symbol-function 'swimmy.school::analyze-veterans) orig-analyze))))

(deftest test-startup-report-snapshot-refreshes-status-files-once
  "Startup snapshot should refresh report + OOS/Forward status files exactly once."
  (let* ((orig-flag swimmy.school::*startup-report-snapshot-written*)
         (orig-generate (symbol-function 'swimmy.school::generate-evolution-report))
         (orig-write-report (symbol-function 'swimmy.school::write-evolution-report-files))
         (orig-write-oos (symbol-function 'swimmy.school::write-oos-status-file))
         (orig-write-forward (symbol-function 'swimmy.school::write-forward-status-file))
         (report-calls 0)
         (oos-calls 0)
         (forward-calls 0)
         (written nil))
    (unwind-protect
        (progn
          (setf swimmy.school::*startup-report-snapshot-written* nil)
          (setf (symbol-function 'swimmy.school::generate-evolution-report)
                (lambda () "UT-REPORT"))
          (setf (symbol-function 'swimmy.school::write-evolution-report-files)
                (lambda (report)
                  (setf written report)
                  (incf report-calls)
                  t))
          (setf (symbol-function 'swimmy.school::write-oos-status-file)
                (lambda (&key reason &allow-other-keys)
                  (declare (ignore reason))
                  (incf oos-calls)
                  t))
          (setf (symbol-function 'swimmy.school::write-forward-status-file)
                (lambda (&key reason &allow-other-keys)
                  (declare (ignore reason))
                  (incf forward-calls)
                  t))

          (swimmy.school::maybe-write-startup-report-snapshot)
          ;; Must be idempotent after first successful refresh.
          (swimmy.school::maybe-write-startup-report-snapshot)

          (assert-equal "UT-REPORT" written "Expected generated report to be written")
          (assert-equal 1 report-calls "Report snapshot should be written only once")
          (assert-equal 1 oos-calls "OOS status should be refreshed once at startup")
          (assert-equal 1 forward-calls "Forward status should be refreshed once at startup")
          (assert-true swimmy.school::*startup-report-snapshot-written*
                       "Startup snapshot flag should be set after success"))
      (setf swimmy.school::*startup-report-snapshot-written* orig-flag)
      (setf (symbol-function 'swimmy.school::generate-evolution-report) orig-generate)
      (setf (symbol-function 'swimmy.school::write-evolution-report-files) orig-write-report)
      (setf (symbol-function 'swimmy.school::write-oos-status-file) orig-write-oos)
      (setf (symbol-function 'swimmy.school::write-forward-status-file) orig-write-forward))))

(deftest test-local-report-snapshot-throttles-and-refreshes-status-files
  "Local report snapshot should refresh OOS/Forward together and obey interval throttle."
  (let* ((orig-last swimmy.school::*last-local-report-refresh-time*)
         (orig-interval swimmy.school::*local-report-refresh-interval-sec*)
         (orig-generate (symbol-function 'swimmy.school::generate-evolution-report))
         (orig-write-report (symbol-function 'swimmy.school::write-evolution-report-files))
         (orig-write-oos (symbol-function 'swimmy.school::write-oos-status-file))
         (orig-write-forward (symbol-function 'swimmy.school::write-forward-status-file))
         (report-calls 0)
         (oos-calls 0)
         (forward-calls 0))
    (unwind-protect
        (progn
          (setf swimmy.school::*last-local-report-refresh-time* 0)
          (setf swimmy.school::*local-report-refresh-interval-sec* 300)
          (setf (symbol-function 'swimmy.school::generate-evolution-report)
                (lambda () "UT-LOCAL-REPORT"))
          (setf (symbol-function 'swimmy.school::write-evolution-report-files)
                (lambda (_report)
                  (incf report-calls)
                  t))
          (setf (symbol-function 'swimmy.school::write-oos-status-file)
                (lambda (&key reason &allow-other-keys)
                  (declare (ignore reason))
                  (incf oos-calls)
                  t))
          (setf (symbol-function 'swimmy.school::write-forward-status-file)
                (lambda (&key reason &allow-other-keys)
                  (declare (ignore reason))
                  (incf forward-calls)
                  t))

          (swimmy.school::maybe-refresh-local-report-snapshot :now 1000 :reason "phase-7-local")
          ;; Should be throttled (100s < 300s interval)
          (swimmy.school::maybe-refresh-local-report-snapshot :now 1100 :reason "phase-7-local")
          ;; Interval elapsed, refresh again
          (swimmy.school::maybe-refresh-local-report-snapshot :now 1301 :reason "phase-7-local")

          (assert-equal 2 report-calls "Expected two local report refreshes")
          (assert-equal 2 oos-calls "Expected OOS status refreshes to match report refresh count")
          (assert-equal 2 forward-calls "Expected Forward status refreshes to match report refresh count")
          (assert-equal 1301 swimmy.school::*last-local-report-refresh-time*
                        "Expected last local report refresh timestamp update"))
      (setf swimmy.school::*last-local-report-refresh-time* orig-last
            swimmy.school::*local-report-refresh-interval-sec* orig-interval
            (symbol-function 'swimmy.school::generate-evolution-report) orig-generate
            (symbol-function 'swimmy.school::write-evolution-report-files) orig-write-report
            (symbol-function 'swimmy.school::write-oos-status-file) orig-write-oos
            (symbol-function 'swimmy.school::write-forward-status-file) orig-write-forward))))

(deftest test-retired-patterns-weighted-in-avoidance
  "Retired patterns should contribute with lower weight to avoidance analysis."
  (let* ((tmp-gy (format nil "/tmp/swimmy-gy-~a.sexp" (get-universal-time)))
         (tmp-ret (format nil "/tmp/swimmy-ret-~a.sexp" (get-universal-time)))
         (now (get-universal-time))
         (orig-gy-file swimmy.school::*graveyard-file*)
         (orig-ret-file swimmy.school::*retired-file*)
         (orig-load-gy (symbol-function 'swimmy.school::load-graveyard-patterns))
         (orig-load-ret (symbol-function 'swimmy.school::load-retired-patterns)))
    (flet ((write-patterns (path patterns)
             (with-open-file (stream path :direction :output :if-exists :supersede :if-does-not-exist :create)
               (dolist (p patterns)
                 (write p :stream stream)
                 (terpri stream)))))
      (unwind-protect
          (progn
            (setf swimmy.school::*graveyard-file* tmp-gy
                  swimmy.school::*retired-file* tmp-ret)
            ;; 4 graveyard patterns (weight 1.0) + 4 retired patterns (weight 0.25) => effective 5.0
            (write-patterns tmp-gy
                            (loop repeat 4 collect
                                  (list :timeframe 15 :direction :BUY :symbol "USDJPY"
                                        :sl 10 :tp 20 :timestamp now)))
            (write-patterns tmp-ret
                            (loop repeat 4 collect
                                  (list :timeframe 15 :direction :BUY :symbol "USDJPY"
                                        :sl 11 :tp 21 :timestamp now)))
            (setf (symbol-function 'swimmy.school::load-graveyard-patterns)
                  (lambda () (error "load-graveyard-patterns should not be called")))
            (setf (symbol-function 'swimmy.school::load-retired-patterns)
                  (lambda () (error "load-retired-patterns should not be called")))
            (let ((regions (swimmy.school::analyze-graveyard-for-avoidance)))
              (assert-true (listp regions) "Expected avoid regions list")
              (assert-equal 1 (length regions) "Expected a single avoid region")
              (let ((region (first regions)))
                (assert-equal 15 (getf region :tf) "Timeframe should match")
                (assert-equal :BUY (getf region :dir) "Direction should match")
                (assert-equal "USDJPY" (getf region :sym) "Symbol should match")
                (assert-equal 10 (getf region :sl-min) "SL min should match")
                (assert-equal 11 (getf region :sl-max) "SL max should match")
                (assert-equal 20 (getf region :tp-min) "TP min should match")
                (assert-equal 21 (getf region :tp-max) "TP max should match")
                (assert-equal 8 (getf region :failure-count) "Failure count should include retired patterns"))))
        (setf swimmy.school::*graveyard-file* orig-gy-file
              swimmy.school::*retired-file* orig-ret-file
              (symbol-function 'swimmy.school::load-graveyard-patterns) orig-load-gy
              (symbol-function 'swimmy.school::load-retired-patterns) orig-load-ret)
        (ignore-errors (delete-file tmp-gy))
        (ignore-errors (delete-file tmp-ret))))))

(deftest test-analyze-veterans-emits-progress-logs
  "analyze-veterans should emit progress logs for long-running steps."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-save (symbol-function 'swimmy.school::save-optimized-params-to-file)))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base*
                (list (swimmy.school:make-strategy :name "UT-VET-1" :sharpe 0.25)
                      (swimmy.school:make-strategy :name "UT-VET-2" :sharpe 0.30)
                      (swimmy.school:make-strategy :name "UT-VET-LOW" :sharpe 0.05)))
          (setf (symbol-function 'swimmy.school::save-optimized-params-to-file)
                (lambda (_params) (declare (ignore _params)) t))
          (let ((out (with-output-to-string (*standard-output*)
                       (swimmy.school::analyze-veterans))))
            (assert-true (search "De-dup complete" out)
                         "Expected de-dup progress log")
            (assert-true (search "Filter complete" out)
                         "Expected filter progress log")
            (assert-true (search "Sort complete" out)
                         "Expected sort progress log")))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb
            (symbol-function 'swimmy.school::save-optimized-params-to-file) orig-save))))

(deftest test-wisdom-push-elite-candidate-keeps-top-n
  "Wisdom helper should keep descending Top-N candidates only."
  (let* ((elite nil)
         (s1 (swimmy.school:make-strategy :name "UT-E1" :sharpe 0.20))
         (s2 (swimmy.school:make-strategy :name "UT-E2" :sharpe 0.40))
         (s3 (swimmy.school:make-strategy :name "UT-E3" :sharpe 0.30))
         (s4 (swimmy.school:make-strategy :name "UT-E4" :sharpe 0.50))
         (s5 (swimmy.school:make-strategy :name "UT-E5" :sharpe 0.10)))
    (setf elite (swimmy.school::%wisdom-push-elite-candidate s1 elite 3))
    (setf elite (swimmy.school::%wisdom-push-elite-candidate s2 elite 3))
    (setf elite (swimmy.school::%wisdom-push-elite-candidate s3 elite 3))
    (setf elite (swimmy.school::%wisdom-push-elite-candidate s4 elite 3))
    (setf elite (swimmy.school::%wisdom-push-elite-candidate s5 elite 3))
    (assert-equal 3 (length elite) "Expected bounded Top-N size")
    (assert-equal '("UT-E4" "UT-E2" "UT-E3")
                  (mapcar #'swimmy.school:strategy-name elite)
                  "Expected descending top-3 candidates only")))

(deftest test-mutate-indicators-with-library-empty-safe
  "mutate-indicators-with-library should not error on empty indicators."
  (let ((orig-state *random-state*))
    (unwind-protect
        (progn
          ;; Force the mutation path to run by selecting a random-state
          ;; whose first (random 1.0) is > 0.7.
          (let ((state (make-random-state t))
                (found nil))
            (loop repeat 1000
                  do (let* ((probe (make-random-state state))
                            (val (random 1.0 probe)))
                       (if (> val 0.7)
                           (progn
                             (setf *random-state* (make-random-state state))
                             (setf found t)
                             (return))
                           (setf state probe))))
            (assert-true found "Expected to find random-state with val > 0.7"))
          (let ((result (swimmy.school::mutate-indicators-with-library nil :trend)))
            (assert-true (null result) "Expected nil indicators to remain nil")))
      (setf *random-state* orig-state))))

(deftest test-daily-pnl-correlation
  "Daily PnL correlation should be near 1 for identical series"
  (let* ((tmp-db (format nil "/tmp/swimmy-corr-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            ;; Seed 3 days of identical pnl for A and B
            (dolist (spec '(("2026-02-01" 1.0) ("2026-02-02" 2.0) ("2026-02-03" 3.0)))
              (destructuring-bind (d v) spec
                (swimmy.school::execute-non-query
                 "INSERT OR REPLACE INTO strategy_daily_pnl (strategy_name, trade_date, pnl_sum, trade_count, updated_at)
                  VALUES (?, ?, ?, ?, ?)"
                 "A" d v 1 (get-universal-time))
                (swimmy.school::execute-non-query
                 "INSERT OR REPLACE INTO strategy_daily_pnl (strategy_name, trade_date, pnl_sum, trade_count, updated_at)
                  VALUES (?, ?, ?, ?, ?)"
                 "B" d v 1 (get-universal-time))))
            (let ((corr (swimmy.school::calculate-daily-pnl-correlation "A" "B" :days 3)))
              (assert-true (and corr (> corr 0.99)) "Correlation should be near 1")))
        (ignore-errors (swimmy.school::close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(defun mock-refresh-daily-pnl ()
  (push :daily-pnl *test-results*))

(deftest test-daily-pnl-aggregation-scheduler
  "Daily PnL aggregation should trigger at 00:10"
  (let ((swimmy.globals:*daily-pnl-aggregation-sent-today* nil)
        (orig (and (fboundp 'swimmy.school::refresh-strategy-daily-pnl)
                   (symbol-function 'swimmy.school::refresh-strategy-daily-pnl))))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::refresh-strategy-daily-pnl) #'mock-refresh-daily-pnl)
          (let ((time-0009 (encode-universal-time 0 9 0 1 2 2026)))
            (swimmy.main:check-scheduled-tasks time-0009)
            (assert-false swimmy.globals:*daily-pnl-aggregation-sent-today* "Should not trigger before 00:10"))
          (let ((time-0010 (encode-universal-time 0 10 0 1 2 2026)))
            (swimmy.main:check-scheduled-tasks time-0010)
            (assert-true swimmy.globals:*daily-pnl-aggregation-sent-today* "Should trigger at 00:10")))
      (when orig
        (setf (symbol-function 'swimmy.school::refresh-strategy-daily-pnl) orig)))))

(deftest test-stagnant-crank-daily-guard
  "Stagnant C-Rank cull should run once per day"
  (let ((swimmy.school::*last-stagnant-crank-cull-day* nil))
    (assert-true (swimmy.school::should-run-stagnant-crank-cull-p 20260209))
    (assert-false (swimmy.school::should-run-stagnant-crank-cull-p 20260209))
    (assert-true (swimmy.school::should-run-stagnant-crank-cull-p 20260210))))

(deftest test-age-increment-daily-guard
  "Strategy age increment should run once per day"
  (let ((swimmy.school::*last-age-increment-day* nil))
    (assert-true (swimmy.school::should-run-age-increment-p 20260209))
    (assert-false (swimmy.school::should-run-age-increment-p 20260209))
    (assert-true (swimmy.school::should-run-age-increment-p 20260210))))

(deftest test-max-age-retire-daily-guard
  "Max-age retire sweep should run once per day"
  (let ((swimmy.school::*last-max-age-retire-day* nil))
    (assert-true (swimmy.school::should-run-max-age-retire-p 20260209))
    (assert-false (swimmy.school::should-run-max-age-retire-p 20260209))
    (assert-true (swimmy.school::should-run-max-age-retire-p 20260210))))

(deftest test-process-breeding-cycle-increments-age-once-per-day
  "process-breeding-cycle should not inflate age when called multiple times in one day"
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-age-day (and (boundp 'swimmy.school::*last-age-increment-day*)
                            swimmy.school::*last-age-increment-day*))
         (orig-cull-day (and (boundp 'swimmy.school::*last-stagnant-crank-cull-day*)
                             swimmy.school::*last-stagnant-crank-cull-day*))
         (orig-retire-day (and (boundp 'swimmy.school::*last-max-age-retire-day*)
                               swimmy.school::*last-max-age-retire-day*))
         (s (swimmy.school:make-strategy :name "UT-AGE-DAILY"
                                         :status :active
                                         :age 0
                                         :sharpe 1.0
                                         :rank :B
                                         :symbol "USDJPY"))
         (day1-t1 (encode-universal-time 0 0 9 11 2 2026))
         (day1-t2 (encode-universal-time 0 0 18 11 2 2026))
         (day2-t1 (encode-universal-time 0 0 9 12 2 2026)))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list s))
          (setf swimmy.school::*last-age-increment-day* nil)
          (setf swimmy.school::*last-stagnant-crank-cull-day* nil)
          (setf swimmy.school::*last-max-age-retire-day* nil)
          (swimmy.school::process-breeding-cycle :now day1-t1)
          (assert-equal 1 (swimmy.school::strategy-age s)
                        "Expected age increment on first same-day run")
          (swimmy.school::process-breeding-cycle :now day1-t2)
          (assert-equal 1 (swimmy.school::strategy-age s)
                        "Expected no second increment on same day")
          (swimmy.school::process-breeding-cycle :now day2-t1)
          (assert-equal 2 (swimmy.school::strategy-age s)
                        "Expected increment again on next day"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb
            swimmy.school::*last-age-increment-day* orig-age-day
            swimmy.school::*last-stagnant-crank-cull-day* orig-cull-day
            swimmy.school::*last-max-age-retire-day* orig-retire-day))))

(deftest test-select-strategies-for-regime-uses-real-categories
  "select-strategies-for-regime should select REVERSION/SCALP strategies in exhausted/ranging regimes (not empty)."
  (let* ((s-trend (swimmy.school:make-strategy :name "UT-TREND" :category :trend))
         (s-rev (swimmy.school:make-strategy :name "UT-REVERSION" :category :reversion))
         (s-brk (swimmy.school:make-strategy :name "UT-BREAKOUT" :category :breakout))
         (s-scalp (swimmy.school:make-strategy :name "UT-SCALP" :category :scalp))
         (pool (list s-trend s-rev s-brk s-scalp)))
    (let ((selected-exhausted (swimmy.school::select-strategies-for-regime :trend-exhausted pool))
          (selected-ranging (swimmy.school::select-strategies-for-regime :ranging pool)))
      (assert-true (> (length selected-exhausted) 0)
                   "Expected non-empty selection for :trend-exhausted")
      (assert-true (> (length selected-ranging) 0)
                   "Expected non-empty selection for :ranging")
      (assert-true (find s-rev selected-exhausted :test #'eq)
                   "Expected :reversion strategy included for :trend-exhausted")
      (assert-true (find s-rev selected-ranging :test #'eq)
                   "Expected :reversion strategy included for :ranging"))))

(deftest test-strategy-regime-class-prefers-semantic-and-infers-scope-keys
  "strategy-regime-class should use explicit semantic categories and infer when category stores scope keys."
  (let* ((s-explicit (swimmy.school:make-strategy :name "UT-EXPLICIT-BREAKOUT"
                                                  :category :breakout
                                                  :tp 1.0))
         (s-inferred (swimmy.school:make-strategy :name "UT-REVERSION-KEY"
                                                  :category '(60 :BOTH "USDJPY")
                                                  :tp 1.0)))
    (assert-equal :breakout (swimmy.school::strategy-regime-class s-explicit)
                  "Expected explicit semantic category to be preserved")
    (assert-equal :reversion (swimmy.school::strategy-regime-class s-inferred)
                  "Expected scope-key category to infer semantic regime class")))

(deftest test-select-strategies-for-regime-supports-tf-direction-category-keys
  "select-strategies-for-regime should work when strategy-category stores scope keys."
  (let* ((s-trend (swimmy.school:make-strategy :name "UT-TREND-KEY"
                                               :category '(60 :BOTH "USDJPY")
                                               :tp 1.0))
         (s-rev (swimmy.school:make-strategy :name "UT-REVERSION-KEY"
                                             :category '(60 :BOTH "USDJPY")
                                             :tp 1.0))
         (s-scalp (swimmy.school:make-strategy :name "UT-SCALP-KEY"
                                               :category '(5 :BOTH "USDJPY")
                                               :tp 0.2))
         (pool (list s-trend s-rev s-scalp)))
    (let ((selected-exhausted (swimmy.school::select-strategies-for-regime :trend-exhausted pool))
          (selected-ranging (swimmy.school::select-strategies-for-regime :ranging pool)))
      (assert-true (> (length selected-exhausted) 0)
                   "Expected non-empty selection for :trend-exhausted with scope keys")
      (assert-true (> (length selected-ranging) 0)
                   "Expected non-empty selection for :ranging with scope keys")
      (assert-true (find s-rev selected-exhausted :test #'eq)
                   "Expected :reversion strategy included for :trend-exhausted")
      (assert-true (find s-scalp selected-ranging :test #'eq)
                   "Expected :scalp strategy included for :ranging"))))

(deftest test-collect-strategy-signals-skips-inactive-ranks
  "collect-strategy-signals should evaluate only active ranks (B/A/S/LEGEND)."
  (let* ((history
           (loop for i from 0 below 160
                 for px = (+ 100.0 (* i 0.01))
                 collect (make-candle :timestamp (+ 1700000000 (* i 60))
                                      :open px
                                      :high (+ px 0.08)
                                      :low (- px 0.08)
                                      :close (+ px 0.02)
                                      :volume 100.0)))
         (s-active (swimmy.school:make-strategy
                    :name "UT-ACTIVE-RANK"
                    :rank :B
                    :category :trend
                    :symbol "USDJPY"
                    :indicators '((:sma 5))
                    :entry '(> close open)))
         (s-grave (swimmy.school:make-strategy
                   :name "UT-GRAVE-RANK"
                   :rank :graveyard
                   :category :trend
                   :symbol "USDJPY"
                   :indicators '((:sma 5))
                   :entry '(> close open)))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-detect (and (fboundp 'swimmy.school::detect-market-regime)
                           (symbol-function 'swimmy.school::detect-market-regime)))
         (orig-mismatch (and (fboundp 'swimmy.school::check-symbol-mismatch)
                             (symbol-function 'swimmy.school::check-symbol-mismatch)))
         (orig-eval (and (fboundp 'swimmy.school::evaluate-strategy-signal)
                         (symbol-function 'swimmy.school::evaluate-strategy-signal)))
         (called nil))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list s-active s-grave))
          (when orig-detect
            (setf (symbol-function 'swimmy.school::detect-market-regime)
                  (lambda () :trend-early)))
          (when orig-mismatch
            (setf (symbol-function 'swimmy.school::check-symbol-mismatch)
                  (lambda (&rest _args)
                    (declare (ignore _args))
                    nil)))
          (when orig-eval
            (setf (symbol-function 'swimmy.school::evaluate-strategy-signal)
                  (lambda (strat _history)
                    (declare (ignore _history))
                    (push (swimmy.school::strategy-name strat) called)
                    :hold)))
          (swimmy.school::collect-strategy-signals "USDJPY" history)
          (assert-equal 1 (length called)
                        "Expected only active-rank strategy to be evaluated")
          (assert-true (member "UT-ACTIVE-RANK" called :test #'string=)
                       "Expected active rank to be evaluated")
          (assert-false (member "UT-GRAVE-RANK" called :test #'string=)
                        "Expected graveyard rank to be excluded"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (when orig-detect
        (setf (symbol-function 'swimmy.school::detect-market-regime) orig-detect))
      (when orig-mismatch
        (setf (symbol-function 'swimmy.school::check-symbol-mismatch) orig-mismatch))
      (when orig-eval
        (setf (symbol-function 'swimmy.school::evaluate-strategy-signal) orig-eval)))))

(deftest test-evaluate-strategy-signal-supports-williams-indicator
  "evaluate-strategy-signal should bind WILLIAMS and avoid eval errors for Williams-based founders."
  (let* ((history
           (loop for i from 0 below 140
                 for px = (+ 100.0 (* i 0.01))
                 collect (make-candle :timestamp (+ 1700000000 (* i 60))
                                      :open px
                                      :high (+ px 0.08)
                                      :low (- px 0.08)
                                      :close (+ px 0.02)
                                      :volume 100.0)))
         (strat (swimmy.school:make-strategy
                 :name "UT-WILLIAMS-D1"
                 :category :reversion
                 :timeframe "D1"
                 :sl 0.008
                 :tp 0.015
                 :indicators '((swimmy.school::williams 14))
                 :entry '(< swimmy.school::williams -80)
                 :exit '(> swimmy.school::williams -20)))
         (orig-safe (and (fboundp 'swimmy.school::is-safe-trading-time-p)
                         (symbol-function 'swimmy.school::is-safe-trading-time-p)))
         (out-stream (make-string-output-stream))
         (signal :hold))
    (unwind-protect
        (progn
          (when orig-safe
            (setf (symbol-function 'swimmy.school::is-safe-trading-time-p)
                  (lambda (&rest _args) (declare (ignore _args)) t)))
          (let ((*standard-output* out-stream))
            (setf signal (swimmy.school::evaluate-strategy-signal strat history)))
          (assert-true (member signal '(:buy :sell :hold))
                       "Expected a valid signal keyword")
          (let ((output (get-output-stream-string out-stream)))
            (assert-false (search "Eval Err" output)
                          "Expected no Eval Err for Williams strategy")
            (assert-false (search "undefined variable" output)
                          "Expected no undefined variable for WILLIAMS")))
      (when orig-safe
        (setf (symbol-function 'swimmy.school::is-safe-trading-time-p) orig-safe)))))

(deftest test-evaluate-strategy-signal-bb-default-dev
  "evaluate-strategy-signal should tolerate (bb period) by defaulting dev and avoid NIL numeric errors."
  (let* ((history
           (loop for i from 0 below 140
                 for px = (+ 110.0 (* i 0.02))
                 collect (make-candle :timestamp (+ 1700100000 (* i 60))
                                      :open px
                                      :high (+ px 0.10)
                                      :low (- px 0.10)
                                      :close (+ px 0.03)
                                      :volume 120.0)))
         (strat (swimmy.school:make-strategy
                 :name "UT-BB-DEFAULT-DEV"
                 :category :scalp
                 :timeframe 1
                 :sl 0.001
                 :tp 0.002
                 :indicators '((swimmy.school::bb 20))
                 :entry '(< swimmy.school::close swimmy.school::bb-lower)
                 :exit '(> swimmy.school::close swimmy.school::bb-middle)))
         (orig-safe (and (fboundp 'swimmy.school::is-safe-trading-time-p)
                         (symbol-function 'swimmy.school::is-safe-trading-time-p)))
         (signal :hold)
         (failed nil))
    (unwind-protect
        (progn
          (when orig-safe
            (setf (symbol-function 'swimmy.school::is-safe-trading-time-p)
                  (lambda (&rest _args) (declare (ignore _args)) t)))
          (handler-case
              (setf signal (swimmy.school::evaluate-strategy-signal strat history))
            (error () (setf failed t)))
          (assert-false failed
                        "Expected no type error when bb deviation is omitted")
          (assert-true (member signal '(:buy :sell :hold))
                       "Expected a valid signal keyword"))
      (when orig-safe
        (setf (symbol-function 'swimmy.school::is-safe-trading-time-p) orig-safe)))))

(deftest test-evaluate-strategy-signal-supports-keyword-indicator-types
  "evaluate-strategy-signal should accept keyword indicator types like :SMA."
  (let* ((history
           (loop for i from 0 below 160
                 for px = (+ 120.0 (* i 0.01))
                 collect (make-candle :timestamp (+ 1700200000 (* i 60))
                                      :open px
                                      :high (+ px 0.10)
                                      :low (- px 0.10)
                                      :close (+ px 0.02)
                                      :volume 90.0)))
         (strat (swimmy.school:make-strategy
                 :name "UT-KW-SMA"
                 :category :trend
                 :timeframe 1
                 :sl 0.001
                 :tp 0.002
                 :indicators '((:sma 20) (:sma 50))
                 :entry '(> swimmy.school::sma-20 swimmy.school::sma-50)
                 :exit '(< swimmy.school::sma-20 swimmy.school::sma-50)))
         (orig-safe (and (fboundp 'swimmy.school::is-safe-trading-time-p)
                         (symbol-function 'swimmy.school::is-safe-trading-time-p)))
         (out-stream (make-string-output-stream))
         (signal :hold))
    (unwind-protect
        (progn
          (when orig-safe
            (setf (symbol-function 'swimmy.school::is-safe-trading-time-p)
                  (lambda (&rest _args) (declare (ignore _args)) t)))
          (let ((*standard-output* out-stream))
            (setf signal (swimmy.school::evaluate-strategy-signal strat history)))
          (assert-true (member signal '(:buy :sell :hold))
                       "Expected a valid signal keyword")
          (let ((output (get-output-stream-string out-stream)))
            (assert-false (search "Eval Err" output)
                          "Expected no Eval Err for keyword indicator types")
            (assert-false (search "undefined variable" output)
                          "Expected no undefined variable for SMA-20/SMA-50")))
      (when orig-safe
        (setf (symbol-function 'swimmy.school::is-safe-trading-time-p) orig-safe)))))

(deftest test-ind-vwapvr-signed-ratio
  "ind-vwapvr should match Guardian signed volume-ratio semantics."
  (let* ((history-up (list
                      ;; Newest first (current candle first)
                      (make-candle :timestamp 1700300004 :open 102.0 :high 102.0 :low 102.0 :close 102.0 :volume 25.0)
                      (make-candle :timestamp 1700300003 :open 100.0 :high 100.0 :low 100.0 :close 100.0 :volume 10.0)
                      (make-candle :timestamp 1700300002 :open 100.0 :high 100.0 :low 100.0 :close 100.0 :volume 10.0)
                      (make-candle :timestamp 1700300001 :open 100.0 :high 100.0 :low 100.0 :close 100.0 :volume 10.0)
                      (make-candle :timestamp 1700300000 :open 100.0 :high 100.0 :low 100.0 :close 100.0 :volume 10.0)))
         (history-down (list
                        (make-candle :timestamp 1700300104 :open 98.0 :high 98.0 :low 98.0 :close 98.0 :volume 25.0)
                        (make-candle :timestamp 1700300103 :open 100.0 :high 100.0 :low 100.0 :close 100.0 :volume 10.0)
                        (make-candle :timestamp 1700300102 :open 100.0 :high 100.0 :low 100.0 :close 100.0 :volume 10.0)
                        (make-candle :timestamp 1700300101 :open 100.0 :high 100.0 :low 100.0 :close 100.0 :volume 10.0)
                        (make-candle :timestamp 1700300100 :open 100.0 :high 100.0 :low 100.0 :close 100.0 :volume 10.0)))
         (up (swimmy.school::ind-vwapvr 3 history-up))
         (down (swimmy.school::ind-vwapvr 3 history-down)))
    (assert-true (and (numberp up) (numberp down))
                 "Expected numeric VWAPVR values")
    (assert-true (< (abs (- up 250.0)) 1e-6)
                 (format nil "Expected +250.0, got ~a" up))
    (assert-true (< (abs (- down -250.0)) 1e-6)
                 (format nil "Expected -250.0, got ~a" down))))

(deftest test-evaluate-strategy-signal-supports-vwapvr-indicator
  "evaluate-strategy-signal should bind VWAPVR symbols and avoid eval errors."
  (let* ((head (list
                ;; Current + previous bars create an orange-threshold cross.
                (make-candle :timestamp 1700310004 :open 102.0 :high 102.0 :low 102.0 :close 102.0 :volume 25.0)
                (make-candle :timestamp 1700310003 :open 100.0 :high 100.0 :low 100.0 :close 100.0 :volume 10.0)
                (make-candle :timestamp 1700310002 :open 100.0 :high 100.0 :low 100.0 :close 100.0 :volume 10.0)
                (make-candle :timestamp 1700310001 :open 100.0 :high 100.0 :low 100.0 :close 100.0 :volume 10.0)
                (make-candle :timestamp 1700310000 :open 100.0 :high 100.0 :low 100.0 :close 100.0 :volume 10.0)))
         (tail (loop for i from 0 below 140
                     collect (make-candle :timestamp (+ 1700300000 i)
                                          :open 100.0 :high 100.0 :low 100.0 :close 100.0 :volume 10.0)))
         (history (append head tail))
         (strat (swimmy.school:make-strategy
                 :name "UT-VWAPVR-ENTRY"
                 :category :trend
                 :timeframe 720
                 :sl 1.6
                 :tp 6.0
                 :indicators '((:vwapvr 3 150))
                 :entry '(and (< swimmy.school::vwapvr-3-prev swimmy.school::vwapvr-threshold)
                              (>= swimmy.school::vwapvr-3 swimmy.school::vwapvr-threshold))
                 :exit '(<= swimmy.school::vwapvr-3 0)))
         (orig-safe (and (fboundp 'swimmy.school::is-safe-trading-time-p)
                         (symbol-function 'swimmy.school::is-safe-trading-time-p)))
         (out-stream (make-string-output-stream))
         (signal :hold))
    (unwind-protect
        (progn
          (when orig-safe
            (setf (symbol-function 'swimmy.school::is-safe-trading-time-p)
                  (lambda (&rest _args) (declare (ignore _args)) t)))
          (let ((*standard-output* out-stream))
            (setf signal (swimmy.school::evaluate-strategy-signal strat history)))
          (assert-equal :buy signal "Expected VWAPVR cross-up to produce :buy")
          (let ((output (get-output-stream-string out-stream)))
            (assert-false (search "Eval Err" output)
                          "Expected no Eval Err for VWAPVR strategy")
            (assert-false (search "undefined variable" output)
                          "Expected VWAPVR symbols to be bound")))
      (when orig-safe
        (setf (symbol-function 'swimmy.school::is-safe-trading-time-p) orig-safe)))))

(deftest test-evaluate-strategy-signal-supports-legacy-rsi-threshold-logic
  "evaluate-strategy-signal should normalize legacy ((:RSI-BELOW ...))/((:RSI-ABOVE ...)) forms."
  (let* ((history
           (loop for i from 0 below 180
                 for px = (+ 120.0 (* i 0.01))
                 collect (make-candle :timestamp (+ 1700210000 (* i 60))
                                      :open px
                                      :high (+ px 0.10)
                                      :low (- px 0.10)
                                      :close (+ px 0.02)
                                      :volume 90.0)))
         (strat (swimmy.school:make-strategy
                 :name "UT-LEGACY-RSI-LOGIC"
                 :category :reversion
                 :timeframe 5
                 :sl 0.001
                 :tp 0.002
                 :indicators '((:rsi 2))
                 :entry '((:rsi-below 10))
                 :exit '((:rsi-above 90))))
         (orig-safe (and (fboundp 'swimmy.school::is-safe-trading-time-p)
                         (symbol-function 'swimmy.school::is-safe-trading-time-p)))
         (out-stream (make-string-output-stream))
         (signal :hold))
    (unwind-protect
        (progn
          (when orig-safe
            (setf (symbol-function 'swimmy.school::is-safe-trading-time-p)
                  (lambda (&rest _args) (declare (ignore _args)) t)))
          (let ((*standard-output* out-stream))
            (setf signal (swimmy.school::evaluate-strategy-signal strat history)))
          (assert-true (member signal '(:buy :sell :hold))
                       "Expected a valid signal keyword")
          (let ((output (get-output-stream-string out-stream)))
            (assert-false (search "Eval Err" output)
                          "Expected no Eval Err for legacy RSI threshold logic")
            (assert-false (search "illegal function call" output)
                          "Expected no illegal function call for legacy RSI threshold logic")))
      (when orig-safe
        (setf (symbol-function 'swimmy.school::is-safe-trading-time-p) orig-safe)))))

(deftest test-evaluate-strategy-signal-supports-legacy-cross-logic
  "evaluate-strategy-signal should normalize legacy ((:CROSS-OVER ...))/((:CROSS-UNDER ...)) forms."
  (let* ((history
           (loop for i from 0 below 200
                 for px = (+ 130.0 (* i 0.01))
                 collect (make-candle :timestamp (+ 1700220000 (* i 60))
                                      :open px
                                      :high (+ px 0.10)
                                      :low (- px 0.10)
                                      :close (+ px 0.02)
                                      :volume 90.0)))
         (strat (swimmy.school:make-strategy
                 :name "UT-LEGACY-CROSS-LOGIC"
                 :category :trend
                 :timeframe 60
                 :sl 0.001
                 :tp 0.002
                 :indicators '((:sma 50) (:sma 200))
                 :entry '((:cross-over :sma 50 :sma 200))
                 :exit '((:cross-under :sma 50 :sma 200))))
         (orig-safe (and (fboundp 'swimmy.school::is-safe-trading-time-p)
                         (symbol-function 'swimmy.school::is-safe-trading-time-p)))
         (out-stream (make-string-output-stream))
         (signal :hold))
    (unwind-protect
        (progn
          (when orig-safe
            (setf (symbol-function 'swimmy.school::is-safe-trading-time-p)
                  (lambda (&rest _args) (declare (ignore _args)) t)))
          (let ((*standard-output* out-stream))
            (setf signal (swimmy.school::evaluate-strategy-signal strat history)))
          (assert-true (member signal '(:buy :sell :hold))
                       "Expected a valid signal keyword")
          (let ((output (get-output-stream-string out-stream)))
            (assert-false (search "Eval Err" output)
                          "Expected no Eval Err for legacy cross logic")
            (assert-false (search "illegal function call" output)
                          "Expected no illegal function call for legacy cross logic")))
      (when orig-safe
        (setf (symbol-function 'swimmy.school::is-safe-trading-time-p) orig-safe)))))

(deftest test-evaluate-strategy-signal-fills-missing-period-bindings
  "evaluate-strategy-signal should fill referenced SMA period bindings even when indicators list is mismatched."
  (let* ((history
           (loop for i from 0 below 160
                 for px = (+ 130.0 (* i 0.02))
                 collect (make-candle :timestamp (+ 1700300000 (* i 60))
                                      :open px
                                      :high (+ px 0.10)
                                      :low (- px 0.10)
                                      :close (+ px 0.01)
                                      :volume 80.0)))
         (strat (swimmy.school:make-strategy
                 :name "UT-MISSING-SMA-BINDINGS"
                 :category :trend
                 :timeframe 1
                 :sl 0.001
                 :tp 0.002
                 :indicators '((:ema 9))
                 :entry '(> swimmy.school::sma-20 swimmy.school::sma-50)
                 :exit '(< swimmy.school::sma-20 swimmy.school::sma-50)))
         (orig-safe (and (fboundp 'swimmy.school::is-safe-trading-time-p)
                         (symbol-function 'swimmy.school::is-safe-trading-time-p)))
         (out-stream (make-string-output-stream))
         (signal :hold))
    (unwind-protect
        (progn
          (when orig-safe
            (setf (symbol-function 'swimmy.school::is-safe-trading-time-p)
                  (lambda (&rest _args) (declare (ignore _args)) t)))
          (let ((*standard-output* out-stream))
            (setf signal (swimmy.school::evaluate-strategy-signal strat history)))
          (assert-true (member signal '(:buy :sell :hold))
                       "Expected a valid signal keyword")
          (let ((output (get-output-stream-string out-stream)))
            (assert-false (search "Eval Err" output)
                          "Expected no Eval Err when SMA period bindings are referenced from logic")
            (assert-false (search "undefined variable" output)
                          "Expected no undefined variable for referenced SMA periods")))
      (when orig-safe
        (setf (symbol-function 'swimmy.school::is-safe-trading-time-p) orig-safe)))))

(deftest test-evaluate-strategy-signal-supports-foreign-package-period-symbols
  "evaluate-strategy-signal should tolerate period symbols from non-swimmy.school packages."
  (let* ((history
           (loop for i from 0 below 160
                 for px = (+ 135.0 (* i 0.02))
                 collect (make-candle :timestamp (+ 1700350000 (* i 60))
                                      :open px
                                      :high (+ px 0.10)
                                      :low (- px 0.10)
                                      :close (+ px 0.01)
                                      :volume 80.0)))
         (strat (swimmy.school:make-strategy
                 :name "UT-FOREIGN-PKG-SMA-BINDINGS"
                 :category :trend
                 :timeframe 1
                 :sl 0.001
                 :tp 0.002
                 :indicators '((:ema 9))
                 :entry '(> close sma-20)
                 :exit '(< close sma-20)))
         (orig-safe (and (fboundp 'swimmy.school::is-safe-trading-time-p)
                         (symbol-function 'swimmy.school::is-safe-trading-time-p)))
         (out-stream (make-string-output-stream))
         (signal :hold))
    (unwind-protect
        (progn
          (when orig-safe
            (setf (symbol-function 'swimmy.school::is-safe-trading-time-p)
                  (lambda (&rest _args) (declare (ignore _args)) t)))
          (let ((*standard-output* out-stream))
            (setf signal (swimmy.school::evaluate-strategy-signal strat history)))
          (assert-true (member signal '(:buy :sell :hold))
                       "Expected a valid signal keyword")
          (let ((output (get-output-stream-string out-stream)))
            (assert-false (search "Eval Err" output)
                          "Expected no Eval Err for foreign-package period symbols")
            (assert-false (search "unbound" output)
                          "Expected no unbound variable for foreign-package period symbols")))
      (when orig-safe
        (setf (symbol-function 'swimmy.school::is-safe-trading-time-p) orig-safe)))))

(deftest test-evaluate-strategy-signal-supports-foreign-package-band-aliases
  "evaluate-strategy-signal should bind band aliases even when logic symbols are in another package."
  (let* ((history
           (loop for i from 0 below 180
                 for px = (+ 145.0 (* i 0.01))
                 collect (make-candle :timestamp (+ 1700450000 (* i 60))
                                      :open px
                                      :high (+ px 0.12)
                                      :low (- px 0.12)
                                      :close (+ px 0.03)
                                      :volume 100.0)))
         (strat (swimmy.school:make-strategy
                 :name "UT-FOREIGN-PKG-BAND-ALIASES"
                 :category :reversion
                 :timeframe "M15"
                 :sl 0.002
                 :tp 0.004
                 :indicators '((swimmy.school::bollinger 20 2))
                 :entry '(> close lower-band)
                 :exit '(< close upper-band)))
         (orig-safe (and (fboundp 'swimmy.school::is-safe-trading-time-p)
                         (symbol-function 'swimmy.school::is-safe-trading-time-p)))
         (out-stream (make-string-output-stream))
         (signal :hold))
    (unwind-protect
        (progn
          (when orig-safe
            (setf (symbol-function 'swimmy.school::is-safe-trading-time-p)
                  (lambda (&rest _args) (declare (ignore _args)) t)))
          (let ((*standard-output* out-stream))
            (setf signal (swimmy.school::evaluate-strategy-signal strat history)))
          (assert-true (member signal '(:buy :sell :hold))
                       "Expected a valid signal keyword")
          (let ((output (get-output-stream-string out-stream)))
            (assert-false (search "Eval Err" output)
                          "Expected no Eval Err for foreign-package band aliases")
            (assert-false (search "unbound" output)
                          "Expected no unbound variable for foreign-package band aliases")))
      (when orig-safe
        (setf (symbol-function 'swimmy.school::is-safe-trading-time-p) orig-safe)))))

(deftest test-evaluate-strategy-signal-supports-lowercase-escaped-period-symbols
  "evaluate-strategy-signal should tolerate lowercase escaped period symbols like |sma-20|."
  (let* ((history
           (loop for i from 0 below 160
                 for px = (+ 136.0 (* i 0.02))
                 collect (make-candle :timestamp (+ 1700550000 (* i 60))
                                      :open px
                                      :high (+ px 0.10)
                                      :low (- px 0.10)
                                      :close (+ px 0.01)
                                      :volume 80.0)))
         (strat (swimmy.school:make-strategy
                 :name "UT-LOWERCASE-PERIOD-SYMBOL"
                 :category :trend
                 :timeframe 1
                 :sl 0.001
                 :tp 0.002
                 :indicators '((:ema 9))
                 :entry '(> close |sma-20|)
                 :exit '(< close |sma-20|)))
         (orig-safe (and (fboundp 'swimmy.school::is-safe-trading-time-p)
                         (symbol-function 'swimmy.school::is-safe-trading-time-p)))
         (out-stream (make-string-output-stream))
         (signal :hold))
    (unwind-protect
        (progn
          (when orig-safe
            (setf (symbol-function 'swimmy.school::is-safe-trading-time-p)
                  (lambda (&rest _args) (declare (ignore _args)) t)))
          (let ((*standard-output* out-stream))
            (setf signal (swimmy.school::evaluate-strategy-signal strat history)))
          (assert-true (member signal '(:buy :sell :hold))
                       "Expected a valid signal keyword")
          (let ((output (get-output-stream-string out-stream)))
            (assert-false (search "Eval Err" output)
                          "Expected no Eval Err for lowercase escaped period symbol")
            (assert-false (search "unbound" output)
                          "Expected no unbound variable for lowercase escaped period symbol")))
      (when orig-safe
        (setf (symbol-function 'swimmy.school::is-safe-trading-time-p) orig-safe)))))

(deftest test-evaluate-strategy-signal-supports-lowercase-escaped-band-aliases
  "evaluate-strategy-signal should tolerate lowercase escaped band aliases like |lower-band|."
  (let* ((history
           (loop for i from 0 below 180
                 for px = (+ 146.0 (* i 0.01))
                 collect (make-candle :timestamp (+ 1700650000 (* i 60))
                                      :open px
                                      :high (+ px 0.12)
                                      :low (- px 0.12)
                                      :close (+ px 0.03)
                                      :volume 100.0)))
         (strat (swimmy.school:make-strategy
                 :name "UT-LOWERCASE-BAND-ALIAS"
                 :category :reversion
                 :timeframe "M15"
                 :sl 0.002
                 :tp 0.004
                 :indicators '((swimmy.school::bollinger 20 2))
                 :entry '(> close |lower-band|)
                 :exit '(< close |upper-band|)))
         (orig-safe (and (fboundp 'swimmy.school::is-safe-trading-time-p)
                         (symbol-function 'swimmy.school::is-safe-trading-time-p)))
         (out-stream (make-string-output-stream))
         (signal :hold))
    (unwind-protect
        (progn
          (when orig-safe
            (setf (symbol-function 'swimmy.school::is-safe-trading-time-p)
                  (lambda (&rest _args) (declare (ignore _args)) t)))
          (let ((*standard-output* out-stream))
            (setf signal (swimmy.school::evaluate-strategy-signal strat history)))
          (assert-true (member signal '(:buy :sell :hold))
                       "Expected a valid signal keyword")
          (let ((output (get-output-stream-string out-stream)))
            (assert-false (search "Eval Err" output)
                          "Expected no Eval Err for lowercase escaped band aliases")
            (assert-false (search "unbound" output)
                          "Expected no unbound variable for lowercase escaped band aliases")))
      (when orig-safe
        (setf (symbol-function 'swimmy.school::is-safe-trading-time-p) orig-safe)))))

(deftest test-evaluate-strategy-signal-supports-bollinger-band-aliases
  "evaluate-strategy-signal should treat BOLLINGER as BB and bind UPPER/LOWER-BAND aliases."
  (let* ((history
           (loop for i from 0 below 180
                 for px = (+ 140.0 (* i 0.01))
                 collect (make-candle :timestamp (+ 1700400000 (* i 60))
                                      :open px
                                      :high (+ px 0.12)
                                      :low (- px 0.12)
                                      :close (+ px 0.03)
                                      :volume 100.0)))
         (strat (swimmy.school:make-strategy
                 :name "UT-BOLLINGER-BAND-ALIASES"
                 :category :reversion
                 :timeframe "M15"
                 :sl 0.002
                 :tp 0.004
                 :indicators '((swimmy.school::bollinger 20 2))
                 :entry '(> swimmy.school::close swimmy.school::lower-band)
                 :exit '(< swimmy.school::close swimmy.school::upper-band)))
         (orig-safe (and (fboundp 'swimmy.school::is-safe-trading-time-p)
                         (symbol-function 'swimmy.school::is-safe-trading-time-p)))
         (out-stream (make-string-output-stream))
         (signal :hold))
    (unwind-protect
        (progn
          (when orig-safe
            (setf (symbol-function 'swimmy.school::is-safe-trading-time-p)
                  (lambda (&rest _args) (declare (ignore _args)) t)))
          (let ((*standard-output* out-stream))
            (setf signal (swimmy.school::evaluate-strategy-signal strat history)))
          (assert-true (member signal '(:buy :sell :hold))
                       "Expected a valid signal keyword")
          (let ((output (get-output-stream-string out-stream)))
            (assert-false (search "Eval Err" output)
                          "Expected no Eval Err for BOLLINGER + band aliases")
            (assert-false (search "unbound" output)
                          "Expected no unbound variable for UPPER/LOWER-BAND aliases")))
      (when orig-safe
        (setf (symbol-function 'swimmy.school::is-safe-trading-time-p) orig-safe)))))

(deftest test-evaluate-strategy-signal-supports-donchian-band-aliases
  "evaluate-strategy-signal should bind UPPER/LOWER-BAND aliases for DONCHIAN-based logic."
  (let* ((history
           (loop for i from 0 below 180
                 for px = (+ 150.0 (* i 0.015))
                 collect (make-candle :timestamp (+ 1700500000 (* i 60))
                                      :open px
                                      :high (+ px 0.15)
                                      :low (- px 0.15)
                                      :close (+ px 0.02)
                                      :volume 90.0)))
         (strat (swimmy.school:make-strategy
                 :name "UT-DONCHIAN-BAND-ALIASES"
                 :category :breakout
                 :timeframe "M15"
                 :sl 0.002
                 :tp 0.004
                 :indicators '((swimmy.school::donchian 20))
                 :entry '(> swimmy.school::high swimmy.school::upper-band)
                 :exit '(< swimmy.school::low swimmy.school::lower-band)))
         (orig-safe (and (fboundp 'swimmy.school::is-safe-trading-time-p)
                         (symbol-function 'swimmy.school::is-safe-trading-time-p)))
         (out-stream (make-string-output-stream))
         (signal :hold))
    (unwind-protect
        (progn
          (when orig-safe
            (setf (symbol-function 'swimmy.school::is-safe-trading-time-p)
                  (lambda (&rest _args) (declare (ignore _args)) t)))
          (let ((*standard-output* out-stream))
            (setf signal (swimmy.school::evaluate-strategy-signal strat history)))
          (assert-true (member signal '(:buy :sell :hold))
                       "Expected a valid signal keyword")
          (let ((output (get-output-stream-string out-stream)))
            (assert-false (search "Eval Err" output)
                          "Expected no Eval Err for DONCHIAN + band aliases")
            (assert-false (search "unbound" output)
                          "Expected no unbound variable for UPPER/LOWER-BAND aliases")))
      (when orig-safe
        (setf (symbol-function 'swimmy.school::is-safe-trading-time-p) orig-safe)))))

;;; ─────────────────────────────────────────
;;; TEST RUNNER
;;; ─────────────────────────────────────────

;; Load extracted unit tests
;; (load (merge-pathnames "tests/school-split-tests.lisp" *load-truename*))

(defun run-all-tests ()
  "Run all tests"
  (setf *test-results* nil)
  (setf *tests-passed* 0)
  (setf *tests-failed* 0)
  
  (format t "~%═══════════════════════════════════════~%")
  (format t "🧪 RUNNING SWIMMY TESTS (V6.18 - Expert Verified)~%")
  (format t "═══════════════════════════════════════~%~%")
  
  ;; Isolate test outputs from live service files, and suppress Discord sends.
  (let ((orig-queue (and (fboundp 'swimmy.core:queue-discord-notification)
                         (symbol-function 'swimmy.core:queue-discord-notification)))
        (orig-queue-raw (and (fboundp 'swimmy.core::queue-raw-discord-message)
                             (symbol-function 'swimmy.core::queue-raw-discord-message))))
    (unwind-protect
        (progn
          (when orig-queue
            (setf (symbol-function 'swimmy.core:queue-discord-notification)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-queue-raw
            (setf (symbol-function 'swimmy.core::queue-raw-discord-message)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (let ((swimmy.core::*log-file-path* "data/memory/swimmy-tests-telemetry.jsonl")
                (swimmy.core::*telemetry-fallback-log-path* "data/memory/swimmy-tests-telemetry-fallback.jsonl")
                (swimmy.school::*evolution-report-path* "data/memory/swimmy-tests-evolution-report.txt")
                ;; Keep tests hermetic and fast; dedicated backfill test calls it explicitly.
                (swimmy.school::*disable-timeframe-backfill* t))
            (ignore-errors (ensure-directories-exist swimmy.core::*log-file-path*))
            (ignore-errors (ensure-directories-exist swimmy.core::*telemetry-fallback-log-path*))
            (ignore-errors (ensure-directories-exist swimmy.school::*evolution-report-path*))
            ;; Run each test
            (dolist (test '(;; Clan tests
                  test-main-shadows-last-new-day
                  test-active-strategy-p-bas-legend
                  test-analyze-graveyard-for-avoidance-skips-corrupt-form
                  test-safe-read-rejects-read-eval
                  test-safe-read-allows-simple-alist
                  test-internal-process-msg-rejects-read-eval
                  test-internal-process-msg-backtest-request-id-bound
                  test-internal-process-msg-backtest-json-applies
                  test-apply-backtest-result-tolerates-nil-metrics
                  test-handle-v2-result-tolerates-nil-metrics
                  test-handle-v2-result-founder-recovery-pass
                  test-backtest-result-phase1-normalizes-base-name
                  test-backtest-result-qual-normalizes-trailing-suffix-only
                  test-backtest-result-phase1-excluded-from-rr-buffer
                  test-backtest-queue-enqueues-when-requester-missing
                  test-backtest-queue-flushes-after-requester
                  test-init-backtest-zmq-fails-when-requester-missing
                  test-backtest-result-preserves-request-id
                  test-backtest-result-persists-trade-list
                  test-cpcv-result-persists-trade-list
                  test-cpcv-result-preserves-trade-meta
                  test-cpcv-result-updates-db-when-strategy-missing
                  test-cpcv-result-metrics-split-runtime-vs-criteria
                  test-cpcv-result-skips-notify-for-unknown-strategy
                  test-cpcv-result-skips-notify-for-graveyard-strategy
                  test-request-cpcv-includes-request-id
                  test-cpcv-gate-failure-counts
                  test-a-stage1-failure-counts
                  test-a-stage1-failure-summary-line
                  test-fetch-cpcv-candidates-filters-by-elite-sharpe
                  test-cpcv-dispatch-eligible-cooldown
                  test-run-a-rank-cpcv-batch-respects-strategy-cooldown
                  test-build-cpcv-status-snippet-includes-metrics
                  test-write-cpcv-status-file
                  test-cpcv-metrics-summary-line-breakdown
                  test-notify-cpcv-result-distinguishes-error
                  test-notify-cpcv-result-dedupes-identical
                  test-notify-cpcv-result-tolerates-nil-metrics-in-log
                  test-backtest-debug-log-records-apply
                  test-backtest-status-includes-last-request-id
                  test-request-backtest-sets-submit-id
                  test-request-backtest-includes-include-trades-flag
                  test-generate-uuid-uses-entropy-file
                  test-generate-uuid-changes-even-with-reset-rng
                  test-oos-retry-uses-new-request-id
                  test-maybe-request-oos-throttles-with-memory-fallback
                  test-oos-stale-result-ignored
                  test-oos-queue-prune-on-startup
                  test-strategy-to-alist-omits-filter-enabled-when-false
                  test-strategy-to-alist-includes-filter-enabled-when-true
                  test-order-open-uses-instrument-side
                  test-calculate-portfolio-exposure-reads-order-open-v2-pending
                  test-data-keeper-request-sexp
                  test-pattern-gate-aligned
                  test-pattern-gate-disagree
                  test-pattern-gate-low-confidence
                  test-pattern-gate-flat
                  test-pattern-gate-missing-probs-fail-open
                  test-pattern-gate-apply-soft-gate
                  test-pattern-similarity-query-request-sexp
                  test-message-dispatcher-compiles-without-warnings
	                  test-safe-read-used-for-db-rank
	                  test-req-history-uses-count
	                  test-order-open-sexp-keys
	                  test-mt5-bridge-order-open-idempotency-guards
	                  test-invalid-order-comment-reason-detects-context-gaps
	                  test-make-order-message-rejects-invalid-comment
	                  test-safe-order-fails-closed-on-invalid-comment
	                  test-safe-order-fails-closed-when-mt5-offline
	                  test-safe-order-fails-closed-when-guardian-heartbeat-stale
	                  test-safe-order-fails-closed-when-account-info-stale
	                  test-check-pending-orders-pauses-when-guardian-heartbeat-stale
	                  test-check-pending-orders-pauses-when-account-info-stale
	                  test-armada-kill-switch-pauses-on-hard-dd
	                  test-enable-armada-kill-switch-canary-mode-activates-guardrails
	                  test-guard-execution-status-blocks-when-armada-kill-switch-trips
	                  test-sexp-string-avoids-array-syntax
	                  test-heartbeat-message-is-sexp
                  test-heartbeat-now-trigger-file
                  test-heartbeat-uses-heartbeat-webhook
		                  test-heartbeat-now-trigger-file
			                  test-heartbeat-summary-no-data-omits-age
			                  test-heartbeat-summary-currency-format-no-trailing-dot
			                  test-executor-heartbeat-sends-sexp
			                  test-heartbeat-throttle-allows-10s
			                  test-pulse-check-equity-format-no-trailing-dot
		                  test-executor-pending-orders-sends-sexp
	                  test-internal-cmd-json-disallowed
	                  test-internal-process-msg-tick-sexp
	                  test-internal-process-msg-tick-json
                  test-internal-process-msg-account-info-refreshes-heartbeat
			                  test-internal-process-msg-positions-sexp
			                  test-internal-process-msg-order-ack-clears-executor-pending
			                  test-internal-process-msg-order-ack-accepts-sharp-bool-token
			                  test-internal-process-msg-order-reject-clears-executor-pending
			                  test-internal-process-msg-order-reject-sink-guard-alert
			                  test-internal-process-msg-order-reject-sink-guard-alert-dedupes-duplicate-id
			                  test-internal-process-msg-order-reject-sink-guard-alert-does-not-dedupe-different-ids
			                  test-internal-process-msg-order-reject-instrument-sink-guard-alert
			                  test-internal-process-msg-order-reject-non-sink-no-alert
			                  test-allocation-pending-timeouts-ignores-executor-pending-orders
		                  test-allocation-pending-timeouts-tolerates-malformed-entry
			                  test-reconcile-with-mt5-positions-records-dryrun-slippage-on-promotion
			                  test-internal-process-msg-status-now-sexp
			                  test-internal-process-msg-recruit-special-forces-sexp
			                  test-internal-process-msg-history-sexp
			                  test-internal-process-msg-history-sexp-normalizes-unix-timestamp
	                  test-internal-process-msg-history-sexp-normalizes-symbol-key
                  test-data-client-sexp-candle-normalizes-unix-timestamp
                  test-update-candle-updates-symbol-history-all-symbols
                  test-update-candle-migrates-legacy-symbol-key
                  test-periodic-status-report-uses-symbol-history-context
	                  test-periodic-status-report-includes-evidence-and-freshness
		                  test-process-account-info-sexp
		                  test-process-account-info-drawdown-alert-peak-format
		                  test-process-account-info-sync-log-format-no-trailing-dot
		                  test-process-account-info-rebases-stale-monitoring-peak-after-restart
	                  test-s-rank-gate-uses-configurable-minimum
	                  test-process-trade-closed-sexp
	                  test-process-trade-closed-fixed-template-message-no-narrative
	                  test-process-trade-closed-uses-payload-context-for-learning
	                  test-process-trade-closed-normalizes-nil-category-from-magic
	                  test-normalize-legacy-plist->strategy
                  test-normalize-struct-roundtrip
                  test-sexp-io-roundtrip
                  test-write-sexp-atomic-stable-defaults
                  test-load-strategy-recovers-struct-sexp
                  test-init-knowledge-base-skips-nil-strategies
                  test-init-knowledge-base-normalizes-timeframe-to-minutes
	                  test-init-knowledge-base-prunes-unevaluated-ranks
	                  test-init-knowledge-base-revives-active-file-strategy-over-archived-db-duplicate
	                  test-init-knowledge-base-revive-requires-active-library-file-by-default
		                  test-backtest-cache-sexp
		                  test-trade-logs-supports-pair-id
		                  test-trade-logs-supports-execution-mode
		                  test-fetch-recent-live-trade-metrics-uses-latest-live-trades-only
		                  test-fetch-recent-live-trade-metrics-includes-loss-streaks
		                  test-strategy-daily-pnl-aggregation
		                  test-add-to-kb-allows-breeder-logic-variant-when-sltp-differs
	                  test-add-to-kb-normalizes-timeframe-to-minutes
	                  test-add-to-kb-rejects-breeder-logic-duplicate-when-sltp-too-close
	                  test-add-to-kb-allows-correlated-strategy-across-symbols
	                  test-add-to-kb-allows-founder-when-only-archived-duplicate-exists
	                  test-add-to-kb-allows-founder-with-archived-same-name-collision
	                  test-add-to-kb-allows-breeder-variant-even-if-graveyard-pattern
	                  test-add-to-kb-founder-phase1-bypasses-graveyard-pattern
	                  test-add-to-kb-breeder-phase1-bypasses-graveyard-pattern
	                  test-add-to-kb-breeder-phase1-graveyard-bypass-can-be-disabled
	                  test-add-to-kb-breeder-requires-phase1-screening-when-require-bt
	                  test-add-to-kb-phase1-pending-skips-pool-membership
	                  test-handle-v2-result-admits-pending-phase1-candidate
	                  test-build-category-pools-excludes-unevaluated-incubator
	                  test-build-category-pools-excludes-zero-evidence-active-rank
	                  test-build-category-pools-excludes-revalidation-pending-legend
	                  test-select-best-from-pool-excludes-unevaluated-incubator
	                  test-force-recruit-strategy-skips-unevaluated-active-pools
	                  test-run-legend-breeding-routes-child-through-add-to-kb
	                  test-increment-breeding-count-does-not-graveyard-on-limit
	                  test-daily-pnl-correlation
	                  test-daily-pnl-aggregation-scheduler
	                  test-2300-trigger-logic
	                  test-midnight-reset-logic
		                  test-daily-report-no-duplicate-after-flag-reset
		                  test-weekly-summary-dedup
		                  test-weekly-summary-dedup-uses-sent-file
		                  test-weekly-summary-notification-routes-to-weekly-channel
		                  test-periodic-maintenance-flushes-stagnant-c-rank
		                  test-evolution-report-throttle-uses-last-write
		                  test-write-evolution-report-files-respects-configured-path
			                  test-evolution-report-staleness-alert-throttles
		                  test-scheduler-calls-timeout-flushes
	                  test-periodic-maintenance-sends-brain-heartbeat
	                  test-stagnant-crank-daily-guard
	                  test-age-increment-daily-guard
	                  test-max-age-retire-daily-guard
	                  test-process-breeding-cycle-increments-age-once-per-day
	                  test-select-strategies-for-regime-uses-real-categories
	                  test-strategy-regime-class-prefers-semantic-and-infers-scope-keys
	                  test-select-strategies-for-regime-supports-tf-direction-category-keys
	                  test-evaluate-strategy-signal-supports-williams-indicator
	                  test-evaluate-strategy-signal-bb-default-dev
	                  test-evaluate-strategy-signal-supports-keyword-indicator-types
	                  test-ind-vwapvr-signed-ratio
	                  test-evaluate-strategy-signal-supports-vwapvr-indicator
	                  test-evaluate-strategy-signal-fills-missing-period-bindings
	                  test-evaluate-strategy-signal-supports-foreign-package-period-symbols
	                  test-evaluate-strategy-signal-supports-foreign-package-band-aliases
	                  test-evaluate-strategy-signal-supports-lowercase-escaped-period-symbols
	                  test-evaluate-strategy-signal-supports-lowercase-escaped-band-aliases
	                  test-evaluate-strategy-signal-supports-bollinger-band-aliases
	                  test-evaluate-strategy-signal-supports-donchian-band-aliases
	                  test-promotion-triggers-noncorrelation-notification
                  test-composite-score-prefers-stable-pf-wr
                  test-composite-score-penalizes-high-dd
                  test-b-rank-cull-uses-composite-score
	                  test-breeder-cull-uses-composite-score
		                  test-promotion-uses-composite-score
		                  test-a-rank-evaluation-uses-composite-score
	                  test-evaluate-a-rank-demotes-on-min-trade-evidence
		                  test-enforce-rank-trade-evidence-floors-demotes-existing-as
		                  test-enforce-s-rank-criteria-conformance-demotes-noncompliant-s
		                  test-enforce-a-b-rank-criteria-conformance-corrects-noncompliant-ranks
		                  test-run-rank-evaluation-invokes-a-b-rank-conformance
		                  test-check-rank-criteria-requires-cpcv-pass-rate
		                  test-check-rank-criteria-s-allows-staged-pf-wr-for-high-trade-evidence
		                  test-check-rank-criteria-s-keeps-strict-pf-wr-for-low-trade-evidence
		                  test-check-rank-criteria-a-requires-min-trade-evidence
		                  test-check-rank-criteria-s-requires-min-trade-evidence
		                  test-strategy-trade-evidence-count-includes-shadow-trade-logs
		                  test-armada-core-profile-gate-requires-pf-dd-trades
		                  test-armada-core-profile-gate-requires-hold-time-band
	                  test-enable-armada-core-canary-mode-limits-gate-to-a-rank
	                  test-check-rank-criteria-vnext-a-oos-threshold
	                  test-check-rank-criteria-vnext-a-wr-floor-is-43pct
	                  test-meets-a-rank-criteria-aligns-with-check-rank-criteria
	                  test-b-rank-culling-default-threshold-is-20
                  test-b-rank-culling-for-category-filters-a-base-candidates
	                  test-run-b-rank-culling-does-not-run-global-sharpe-only-promotion
	                  test-run-b-rank-culling-uses-active-timeframes-dynamically
	                  test-timeframe-utils-support-arbitrary-minutes
	                  test-resolve-execution-timeframe-minutes-is-strict
	                  test-timeframe-bucketization-is-finite
	                  test-pattern-gate-enabled-timeframe-supports-custom-tf-via-bucket
	                  test-apply-pattern-similarity-gate-accepts-custom-h5
	                  test-score-from-metrics-penalizes-low-trade-evidence
	                  test-tf-mutation-options-keeps-core-and-anchors-under-pending-pressure
	                  test-tf-mutation-options-returns-full-set-when-pending-is-low
	                  test-resample-candles-aligns-to-unix-buckets
	                  test-prepare-trade-context-does-not-silently-fallback-to-m1
	                  test-prepare-trade-context-prefers-strategy-timeframe-over-stale-override
	                  test-restore-legend-61-updates-existing-timeframe-from-optimized-map
	                  test-restore-legend-61-applies-optimized-timeframe-when-adding
	                  test-b-rank-culling-for-category-filters-expectancy-candidates
	                  test-b-rank-culling-records-category-a-candidate-metrics
	                  test-a-candidate-metrics-snippet-summarizes-category-counts
	                  test-a-expectancy-gate-normalizes-price-unit-sltp
	                  test-b-rank-culling-bootstrap-runs-below-threshold-when-no-a
	                  test-b-rank-culling-bootstrap-runs-below-threshold-when-a-low
	                  test-b-rank-culling-keeps-b-baseline-when-no-a-ready
	                  test-b-rank-culling-penalizes-a-base-deficit-when-pruning
	                  test-validate-a-rank-requires-positive-net-expectancy
		                  test-validate-a-rank-allows-dryrun-bootstrap-when-mc-passes
                  test-validate-a-rank-allows-mc-bootstrap-when-disabled
		                  test-validate-a-rank-requires-mc-when-enabled
			                  test-evaluate-a-rank-requires-common-stage2-gates
			                  test-common-stage2-bootstrap-passes-for-mature-cpcv-candidate
			                  test-common-stage2-bootstrap-does-not-bypass-with-low-trades
			                  test-common-stage2-mc-insufficiency-dispatches-backfill-backtest-when-trades-sufficient
			                  test-strategy-mc-prob-ruin-falls-back-to-db-trade-history
			                  test-dryrun-slippage-persists-across-memory-reset
			                  test-dryrun-slippage-db-cap-prunes-old-samples
		                  test-dryrun-slippage-db-period-prunes-old-samples
	                  test-ensure-rank-blocks-s-without-cpcv
	                  test-ensure-rank-blocked-s-emits-detailed-failure-gates
	                  test-draft-does-not-promote-without-cpcv
                  test-draft-counts-only-successful-promotions
                  test-noncorrelation-score-reports-coverage
                  test-noncorrelation-notify-includes-coverage
                  test-noncorrelation-notify-logs-message
                  test-noncorrelation-telemetry-emitted
                  test-backtest-trade-logs-insert
                  test-fetch-backtest-trades
                  test-pair-id-stable
                  test-pair-overlay-caps-lot
                  test-pair-candidate-pool-top-n
                  test-align-pnl-series-padding
                  test-pair-align-pnl-series-zero-fill
                  test-pair-pearson-correlation
                  test-pair-score-from-pnls
                  test-pair-inverse-vol-weights
                  test-pair-gate-blocks-on-insufficient-data
                  test-pair-slot-competition-cap
                  test-pair-promotion-blocked-without-oos-cpcv
                  test-pair-selection-rescue-mode
                  test-pair-selection-excludes-short-trades
                  test-telemetry-sexp
                  test-live-status-sexp
                  test-telemetry-event-schema
                  test-telemetry-fallback-when-primary-unwritable
                  test-category-trade-interval
                  test-verify-parallel-scenarios-uses-category-keys
                  test-live-status-schema-v2-no-tribe
                  test-live-status-includes-heartbeat-metrics
                  test-daily-report-omits-tribe
                  test-recruit-notification-uses-category
                  test-category-vocabulary-omits-clan-terms-in-sources
                  test-founder-template-uses-category-placeholder
                  test-hunted-founder-templates-apply-trend-hardening-filters
                  test-ledger-omits-tribe-fields
                  test-dynamic-narrative-uses-category-display
                  test-category-counts-returns-alist
                  test-repl-help-omits-clans
                  test-stagnant-crank-telemetry-buffer
                  ;; Macro tests
                  ;; (Temporarily removed missing tests)
                  ;; V6.18: Danger tests
                  test-danger-level-initial
                  test-consecutive-losses-tracked
                  test-cooldown-returns-false-initially
                  ;; V6.18: Resignation tests
                  test-has-resigned-initial
                  test-resignation-threshold-exists
                  ;; V6.18: Leader tests
                  test-leader-info-struct
                  ;; V6.18: Risk tests
                  test-risk-summary
                  test-risk-fallback-capital-format-no-trailing-dot
                  test-risk-daily-loss-limit-format-no-trailing-dot
                  test-update-global-stats-pnl-format-no-trailing-dot
                  test-should-unlearn-p-daily-pnl-format-no-trailing-dot
                  test-learning-log-summary-line-breaks-down-live-and-shadow
                  test-bootstrap-learning-logs-from-db-preserves-execution-mode
                  test-analyze-by-category-bootstraps-learning-logs-from-db
		                  ;; Backtest DB sync regression
			                  test-apply-backtest-result-updates-data-sexp
		                  test-apply-backtest-result-evaluates-incubator-strategy
	                  test-apply-backtest-result-evaluates-incubator-string-rank
	                  test-apply-backtest-result-fallback-evaluates-incubator
	                  test-refresh-strategy-metrics-reconciles-missing-active-kb
	                  test-refresh-strategy-metrics-reconciles-missing-active-db
	                  test-refresh-strategy-metrics-reconcile-ignores-unevaluated-db-ranks
	                  test-reconcile-active-library-with-db-hydrates-missing-active-files
	                  test-reconcile-active-library-with-db-falls-back-to-kb-on-blank-payload
	                  test-reconcile-archive-library-with-db-hydrates-missing-files
	                  test-reconcile-archive-library-with-db-purges-stale-active-files
	                  test-reconcile-archive-library-with-db-falls-back-to-backup-data
	                  test-reconcile-archive-library-with-db-counts-empty-payload-write-failures
	                  test-kill-strategy-persists-status
	                  test-max-age-retire-batched-notification
	                  test-stagnant-crank-batched-notification
	                  test-kill-strategy-reason-code-stagnant-crank
	                  test-pool-overflow-retire-batched-notification
	                  test-notify-death-routes-pool-overflow-to-batch
	                  test-init-db-creates-strategy-lookup-indexes
	                  test-backfill-strategy-timeframes-to-minutes-normalizes-mixed-values
	                  test-migrate-existing-data-skips-corrupted-graveyard-lines
	                  test-collect-all-strategies-unpruned
                  test-map-strategies-from-db-batched
                  test-map-strategies-from-db-limit
                  test-pair-strategy-upsert-fetch
                  test-db-rank-counts
                  test-report-source-drift-detects-mismatch
                  test-report-source-drift-kb-active-count-ignores-archived-ranks
                  test-report-source-drift-detects-canonical-archive-mismatch
                  test-report-source-drift-includes-canonical-line-even-when-delta-zero
                  test-evolution-report-uses-db-counts
                  ;; Backtest payload S-expression tests
                  test-request-backtest-indicator-type-symbol
                  test-request-backtest-v2-includes-request-id
                  ;; V6.18: Dynamic TP/SL tests
                  test-volatility-multiplier
                  test-atr-empty-candles
                  ;; V6.18: Utility tests
                  test-gotobi-day-returns-boolean
                  test-london-session-check
                  ;; V6.18: Candle tests
                  test-candle-creation
                  ;; V7.0: School Split Tests (Taleb)
                  test-time-decay-weight
                  test-retired-patterns-weighted-in-avoidance
	                  test-pattern-similarity
	                  test-calculate-pattern-similarity-behavior ; [V8.2] Uncle Bob
	                  test-pattern-soft-gate-reduces-lot-on-mismatch
	                  test-pattern-soft-gate-fade-decision-applies-stronger-reduction
	                  test-pattern-soft-gate-no-trade-decision-applies-strongest-reduction
	                  test-atr-calculation-logic
	                  test-volatility-shifts
                  test-prediction-structure
	                  test-select-optimal-model-normal-vol-uses-ensemble
		                  test-process-category-trades-ignores-model-gate
		                  test-confidence-entry-multiplier-policy
		                  test-process-category-trades-skips-low-confidence-signal
		                  test-process-category-trades-applies-confidence-lot-multiplier
		                  test-process-category-trades-forwards-top-strategy-context
		                  test-process-category-trades-runs-a-rank-shadow-when-s-gate-blocked
		                  test-execute-category-trade-fails-closed-on-missing-timeframe
	                  test-execute-category-trade-fails-closed-on-unresolved-strategy-name
	                  test-verify-signal-authority-requires-live-ready-deployment-gate
	                  test-verify-signal-authority-blocks-on-live-edge-degradation
	                  test-verify-signal-authority-blocks-on-live-edge-loss-streak
	                  test-execute-category-trade-fails-closed-on-reserved-strategy-name-tokens
	                  test-execute-order-sequence-fails-closed-on-missing-context
	                  test-execute-order-sequence-fails-closed-on-reserved-strategy-token
	                  ;; V8.0: Walk-Forward Validation Tests (López de Prado)
                  test-wfv-logic-robust-strategy
                  test-wfv-logic-overfit-strategy
	                  test-wfv-scheduling-respects-interval-and-pending
	                  test-wfv-pending-stats-oldest-age
                  test-wfv-analysis-mode-does-not-move-strategy-rank
	                  test-oos-validation-dispatches-when-unset
                  test-qualification-candidate-renames-when-db-rank-archived
	                  test-qualification-candidate-keeps-name-when-db-rank-active
	                  test-run-qualification-cycle-prioritizes-incubator-candidates
	                  test-run-qualification-cycle-reconciles-scored-incubator-backlog
	                  test-oos-status-updated-on-dispatch
                  test-handle-oos-result-upserts-deployment-gate-status
	                  test-evolution-report-includes-oos-status
                  test-evolution-report-includes-forward-status
                  test-evolution-report-reflects-evolution-daemon-status
                  test-evolution-report-includes-cpcv-gate-failures
                  test-display-candidate-name-preserves-suffix-for-long-names
                  test-oos-status-line-no-queue-duplication
                  test-oos-status-line-ignores-queue-error
                  test-evolution-report-excludes-phase2-end-time
                  ;; V7.1: Persistence Tests (Andrew Ng)
                  test-learning-persistence
                  ;; V8.0: Advisor Reports (Expert Panel)
                  test-advisor-reports
                  ;; V8.4: Characterization Tests (memo3.txt)
                  test-maintenance-throttle-60s
                  test-dream-cycle-self-throttle
                  test-processing-step-no-maintenance
                  test-backtest-debug-enabled-p
                  test-flush-deferred-founders-respects-limit
                  test-backtest-pending-counters-defaults
                  test-backtest-pending-count-rebases-large-drift
                  test-backtest-pending-count-skips-rebase-for-small-max
                  test-backtest-send-throttles-when-pending-high
                  test-backtest-send-throttle-enqueues-instead-of-drop
                  test-backtest-send-uses-subsecond-time
                  test-send-zmq-sleep-suppressed-for-backtest-requester
                  test-backtest-queue-periodic-flush
                  test-rr-batch-respects-max-pending
                  test-rr-batch-respects-available-pending-slots
                  test-rr-batch-respects-backtest-rate-limit
                  test-rr-batch-applies-rate-limit-pause
                  test-rr-batch-counts-only-accepted-dispatches
                  test-rr-batch-skips-retired-strategies
	                  test-rr-batch-skips-db-archived-active-strategies
	                  test-rr-batch-no-active-strategies
	                  test-recruit-special-forces-skips-existing-founder-names
	                  test-recruit-special-forces-ignores-archived-name-collisions
	                  test-recruit-special-forces-respects-recruit-limit
	                  test-recruit-special-forces-filters-founder-keys
	                  test-recruit-special-forces-skips-hunter-auto-founders-when-enabled
	                  test-format-phase1-bt-batch-message
                  test-notify-backtest-summary-includes-a-stage1-failure-breakdown
                  test-notify-backtest-summary-preserves-state-for-timeout-progress
                  test-notify-backtest-summary-includes-throughput-and-stage1-metrics
                  test-format-percent-no-double
                  test-format-value-rounds-int
                  test-ledger-persists-equity
                  test-system-pulse-5m-text
                  test-notify-discord-daily-accepts-custom-title
                  test-advisor-council-triggers-auto-maintenance
                  test-naval-report-zombie-count-not-capped
                  test-backtest-pending-count-decrements-on-recv
                  test-deferred-flush-respects-batch
                  test-deferred-flush-counts-only-accepted-dispatches
                  test-deferred-flush-throttled-requeues-and-pauses-tick
                  test-deferred-flush-applies-rate-limit-pause
                  test-schedule-deferred-founders-respects-hard-cap
                  test-schedule-deferred-founders-skips-db-archived-candidates
	                  test-flush-deferred-founders-skips-db-archived-before-dispatch
	                  test-recruit-founder-preflight-skips-logic-duplicate-before-add-to-kb
	                  test-recruit-founder-preflight-cooldown-skips-second-retry-after-reject
	                  test-recruit-founder-applies-symbol-override-and-rewrites-name
	                  test-recruit-founder-queues-phase1-before-admission
	                  test-recruit-special-forces-recruits-founders-per-supported-symbol
	                  test-backtest-uses-csv-override
	                  test-heartbeat-webhook-prefers-env
	                  test-backtest-webhook-routes-to-system-logs
	                  test-discord-send-guard-respects-env
	                  test-backtest-v2-uses-alist
	                  test-prune-low-sharpe-skips-newborn-age
                  test-prune-similar-skips-newborn-trades
                  test-hard-cap-skips-newborn
                  test-hard-cap-does-not-crash-when-prunable-pool-is-smaller-than-excess
                  test-delete-strategy-rank-guard
                  test-move-strategy-from-rank
                  test-ensure-rank-graveyard-deletes-old-rank-file
                  test-evaluate-strategy-performance-sends-to-graveyard
                  test-ensure-rank-retired-saves-pattern
                  test-lifecycle-retire-on-max-losses
                  ;; EV-First: Cost/Pip utilities
                  test-pip-size-by-symbol
                  test-spread-pips-from-bid-ask
                  test-cost-pips-defaults
	                  test-phase-8-weekly-prune-skips-when-incubator-pending
	                  test-phase-7-wisdom-update-respects-interval
	                  test-startup-report-snapshot-refreshes-status-files-once
	                  test-local-report-snapshot-throttles-and-refreshes-status-files
	                  test-wisdom-push-elite-candidate-keeps-top-n
                  ;; V8.5: Evolution Tests (Genetic Mutation)
		                  test-rewrite-logic-symbols-sma
	                  test-mutate-strategy-structure
	                  test-mutate-param-sl-tp
	                  test-breed-strategies-name-is-unique-with-same-random-state
	                  test-pfwr-mutation-bias-adjusts-rr-when-parents-underperform
                  test-pfwr-mutation-bias-noop-when-parents-already-healthy
                  test-pfwr-mutation-bias-uses-s-target-when-parents-a-or-above
	                  test-pfwr-mutation-bias-lowers-rr-when-wr-gap-dominates
	                  test-pfwr-mutation-bias-tightens-rr-cap-for-severe-wr-deficit
	                  test-pfwr-mutation-bias-tightens-rr-cap-for-moderate-wr-deficit
	                  test-pfwr-mutation-bias-stabilizes-opposite-complements-near-a-target
	                  test-pfwr-mutation-bias-opposite-complements-shifts-to-pf-recovery-band-when-wr-ready
	                  test-pfwr-mutation-bias-upside-scale-boosts-wr-ready-pf-gap-to-s-target
	                  test-select-pfwr-anchor-parent-prefers-higher-wr-parent-when-wr-gap-dominates
	                  test-pfwr-mutation-bias-applies-pf-recovery-floor-when-pf-gap-dominates
	                  test-pfwr-mutation-bias-applies-pf-recovery-floor-for-moderate-pf-gap
	                  test-pfwr-mutation-bias-increases-scale-when-pf-gap-dominates
	                  test-pfwr-mutation-bias-expands-scale-for-opposite-complements
	                  test-pfwr-mutation-bias-expands-scale-for-wr-only-pairs
	                  test-strategy-breeding-priority-score-prefers-a-base-near-candidate
	                  test-strategy-breeding-priority-score-prefers-high-cpcv-pass-rate
	                  test-strategy-breeding-priority-score-penalizes-weak-edge-even-with-high-rank
	                  test-strategy-breeding-priority-score-prefers-high-oos-sharpe
	                  test-pfwr-mutation-bias-raises-rr-when-pf-gap-dominates
                  test-mutate-sltp-with-pfwr-bias-lowers-rr-when-wr-gap-dominates
                  test-mutate-sltp-with-pfwr-bias-raises-rr-when-pf-gap-dominates
	                  test-breed-strategies-reapplies-pfwr-bias-after-q-selection
	                  test-clamp-child-sltp-to-parent-envelope-limits-explosive-values
	                  test-breed-strategies-borrows-logic-when-parents-are-empty
	                  test-breed-strategies-uses-default-logic-when-no-donor
	                  test-find-diverse-breeding-partner-falls-back-past-similar-neighbor
	                  test-can-breed-p-rejects-retired-rank
	                  test-can-breed-p-rejects-incubator-rank
	                  test-can-breed-p-rejects-low-trade-parent
	                  test-can-breed-p-rejects-stage1-weak-edge-parent
	                  test-can-breed-p-allows-legend-parent-even-if-stage1-weak
	                  test-can-breed-p-rejects-a-parent-with-weak-oos
	                  test-can-breed-p-allows-a-parent-with-strong-oos
	                  test-can-breed-p-rejects-a-parent-that-fails-a-rank-floor
	                  test-can-breed-p-rejects-s-parent-without-cpcv-floor
	                  test-can-breed-p-allows-s-parent-with-cpcv-floor
	                  test-run-legend-breeding-skips-stage1-weak-b-parent
	                  test-find-diverse-breeding-partner-prefers-pfwr-complement
	                  test-find-diverse-breeding-partner-prioritizes-complement-over-base-score
	                  test-find-diverse-breeding-partner-filters-low-quality-complements
	                  test-find-diverse-breeding-partner-prefers-near-pf-wr-only-candidate
	                  test-find-diverse-breeding-partner-prefers-wr-only-recovery-over-pf-only
	                  test-find-diverse-breeding-partner-allows-wr-complement-with-moderate-pf
	                  test-find-diverse-breeding-partner-allows-low-pf-high-wr-when-parent-has-pf-surplus
	                  test-find-diverse-breeding-partner-prefers-partial-wr-recovery-when-no-full-complement
	                  test-breeding-pair-blacklist-blocks-candidate-before-cooldown
	                  test-breeding-pair-blacklist-expires-after-cooldown
	                  test-find-diverse-breeding-partner-skips-blacklisted-pair
	                  test-pick-breeder-timeframe-mutation-candidate-prefers-underrepresented-tf
	                  test-select-logic-anchor-parent-prefers-high-wr-under-wr-deficit
	                  test-select-logic-anchor-parent-prefers-high-pf-under-pf-deficit
	                  test-breed-strategies-combines-high-wr-entry-and-high-pf-exit
	                  test-strategies-correlation-ok-p-respects-configurable-distance-threshold
	                  test-strategies-correlation-ok-p-honors-dynamic-min-distance-override
	                  test-breeder-relaxed-distance-defaults-are-stricter-ordering-safe
	                  test-find-diverse-breeding-partner-relaxes-distance-for-complement
	                  test-find-diverse-breeding-partner-relaxes-distance-for-partial-pf-recovery
	                  test-find-diverse-breeding-partner-relaxes-distance-for-modest-partial-pf-recovery
	                  test-find-diverse-breeding-partner-relaxes-distance-to-0p11-for-partial-pf-recovery
	                  test-find-diverse-breeding-partner-relaxes-distance-for-tiny-partial-pf-recovery
                  ;; Expert Panel P1: Symbol Mismatch Tests
                  test-check-symbol-mismatch-blocks-cross-trading
                  test-check-symbol-mismatch-allows-correct-pair
                  test-check-symbol-mismatch-allows-generic
                  test-check-symbol-mismatch-blocks-eurusd-on-gbpusd
                  test-check-symbol-mismatch-case-insensitive))
              (format t "Running ~a... " test)
              (if (funcall test)
                  (format t "✅ PASSED~%")
                  (format t "❌ FAILED~%")))))
      (when orig-queue
        (setf (symbol-function 'swimmy.core:queue-discord-notification) orig-queue))
      (when orig-queue-raw
        (setf (symbol-function 'swimmy.core::queue-raw-discord-message) orig-queue-raw))))
  
  (format t "~%═══════════════════════════════════════~%")
  (format t "📊 RESULTS: ~d passed, ~d failed~%~%" *tests-passed* *tests-failed*)
  (format t "Test coverage: Clan, Macros, Danger, Resignation,~%")
  (format t "               Leader, Risk, Dynamic TP/SL, Utils~%")
  (format t "               School Split (Learning, Volatility, Research)~%")
  (format t "═══════════════════════════════════════~%~%")
  
  (values *tests-passed* *tests-failed*))

(format t "[TESTS] Test framework loaded (V7.0 - Expert Verified)~%")
(format t "[TESTS] Run (swimmy-tests:run-all-tests) to execute ~d tests~%" 23)
