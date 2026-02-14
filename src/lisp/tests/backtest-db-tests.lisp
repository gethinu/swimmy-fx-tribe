;;; backtest-db-tests.lisp - DB regression tests for backtest updates

(in-package :swimmy.tests)

(deftest test-apply-backtest-result-updates-data-sexp
  "Ensure fallback path updates data_sexp when strategy is not in memory."
  (let* ((name "TEST-APPLY-BT-UPDATE")
         (initial-sharpe 0.12)
         (updated-sharpe 1.23)
         (tmp-db (format nil "/tmp/swimmy-test-~a.db" (get-universal-time)))
         (metrics (list :sharpe updated-sharpe
                        :profit-factor 1.5
                        :win-rate 0.6
                        :trades 10
                        :max-dd 0.2
                        :oos-sharpe 1.1
                        :cpcv-median 0.9
                        :cpcv-median-pf 1.4
                        :cpcv-median-wr 0.52
                        :cpcv-median-maxdd 0.12
                        :cpcv-pass-rate 0.8)))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (*default-pathname-defaults* #P"/tmp/"))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (let ((strat (make-strategy
                          :name name
                          :indicators '((sma 20))
                          :entry '(> close (sma 20))
                          :exit '(< close (sma 20))
                          :sharpe initial-sharpe
                          :symbol "USDJPY"
                          :timeframe 1
                          :direction :BOTH)))
              (upsert-strategy strat))
            (let ((*strategy-knowledge-base* nil)
                  (swimmy.globals:*evolved-strategies* nil))
              (apply-backtest-result name metrics)
              (let* ((sexp-str (execute-single "SELECT data_sexp FROM strategies WHERE name = ?" name))
                     (*package* (find-package :swimmy.school))
                     (obj (read-from-string sexp-str)))
                (assert-true (swimmy.school::strategy-p obj) "Stored object should be a strategy")
                (assert-true (< (abs (- (strategy-sharpe obj) updated-sharpe)) 0.0001)
                             "data_sexp should reflect updated sharpe")
                (assert-true (< (abs (- (strategy-cpcv-median-pf obj) 1.4)) 0.0001)
                             "data_sexp should reflect updated cpcv median PF")
                (assert-true (< (abs (- (strategy-cpcv-median-wr obj) 0.52)) 0.0001)
                             "data_sexp should reflect updated cpcv median WR")
                (assert-true (< (abs (- (strategy-cpcv-median-maxdd obj) 0.12)) 0.0001)
                             "data_sexp should reflect updated cpcv median MaxDD")))
        (ignore-errors (execute-non-query "DELETE FROM strategies WHERE name = ?" name))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db)))))))

(deftest test-apply-backtest-result-evaluates-incubator-strategy
  "Incubator strategy should trigger phase-1 rank evaluation after backtest."
  (let* ((name "TEST-INCUBATOR-EVAL")
         (strat (make-strategy :name name
                               :symbol "USDJPY"
                               :timeframe 60
                               :direction :BOTH
                               :rank :incubator))
         (metrics (list :sharpe 0.42
                        :profit-factor 1.21
                        :win-rate 0.47
                        :trades 123
                        :max-dd 0.11))
         (orig-evaluate (symbol-function 'swimmy.school:evaluate-new-strategy))
         (orig-upsert (symbol-function 'swimmy.school:upsert-strategy))
         (called nil))
    (let ((*strategy-knowledge-base* (list strat))
          (swimmy.globals:*evolved-strategies* nil))
      (unwind-protect
          (progn
            (setf (symbol-function 'swimmy.school:evaluate-new-strategy)
                  (lambda (s)
                    (when (eq s strat)
                      (setf called t))
                    :B))
            (setf (symbol-function 'swimmy.school:upsert-strategy)
                  (lambda (&rest args)
                    (declare (ignore args))
                    nil))
            (apply-backtest-result name metrics)
            (assert-true called
                         "Expected incubator strategy to be evaluated for B/graveyard"))
        (setf (symbol-function 'swimmy.school:evaluate-new-strategy) orig-evaluate)
        (setf (symbol-function 'swimmy.school:upsert-strategy) orig-upsert)))))

(deftest test-apply-backtest-result-evaluates-incubator-string-rank
  "String rank tokens (e.g. INCUBATOR) should still trigger phase-1 evaluation."
  (let* ((name "TEST-INCUBATOR-STRING-RANK")
         (strat (make-strategy :name name
                               :symbol "USDJPY"
                               :timeframe 60
                               :direction :BOTH
                               :rank :incubator))
         (metrics (list :sharpe 0.42
                        :profit-factor 1.21
                        :win-rate 0.47
                        :trades 123
                        :max-dd 0.11))
         (orig-evaluate (symbol-function 'swimmy.school:evaluate-new-strategy))
         (orig-upsert (symbol-function 'swimmy.school:upsert-strategy))
         (called nil))
    (setf (strategy-rank strat) "INCUBATOR")
    (let ((*strategy-knowledge-base* (list strat))
          (swimmy.globals:*evolved-strategies* nil))
      (unwind-protect
          (progn
            (setf (symbol-function 'swimmy.school:evaluate-new-strategy)
                  (lambda (s)
                    (when (eq s strat)
                      (setf called t))
                    :B))
            (setf (symbol-function 'swimmy.school:upsert-strategy)
                  (lambda (&rest args)
                    (declare (ignore args))
                    nil))
            (apply-backtest-result name metrics)
            (assert-true called
                         "Expected string INCUBATOR rank to be evaluated for B/graveyard"))
        (setf (symbol-function 'swimmy.school:evaluate-new-strategy) orig-evaluate)
        (setf (symbol-function 'swimmy.school:upsert-strategy) orig-upsert)))))

(deftest test-apply-backtest-result-fallback-evaluates-incubator
  "Fallback path should still trigger phase-1 evaluation for incubator rows."
  (let* ((name "TEST-APPLY-BT-FALLBACK-EVAL")
         (tmp-db (format nil "/tmp/swimmy-test-~a.db" (get-universal-time)))
         (metrics (list :sharpe 0.42
                        :profit-factor 1.21
                        :win-rate 0.47
                        :trades 123
                        :max-dd 0.11))
         (orig-evaluate (symbol-function 'swimmy.school:evaluate-new-strategy))
         (called nil))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (*default-pathname-defaults* #P"/tmp/"))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (upsert-strategy (make-strategy :name name
                                            :symbol "USDJPY"
                                            :timeframe 60
                                            :direction :BOTH
                                            :rank :incubator))
            (let ((*strategy-knowledge-base* nil)
                  (swimmy.globals:*evolved-strategies* nil))
              (setf (symbol-function 'swimmy.school:evaluate-new-strategy)
                    (lambda (s)
                      (when (string= (strategy-name s) name)
                        (setf called t))
                      :B))
              (apply-backtest-result name metrics)
              (assert-true called
                           "Expected fallback path to evaluate incubator strategy")))
        (setf (symbol-function 'swimmy.school:evaluate-new-strategy) orig-evaluate)
        (ignore-errors (execute-non-query "DELETE FROM strategies WHERE name = ?" name))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-upsert-preserves-cpcv-when-missing
  "Upsert should not overwrite existing CPCV metrics with zero defaults."
  (let* ((name "TEST-CPCV-PRESERVE")
         (tmp-db (format nil "/tmp/swimmy-cpcv-preserve-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (*strategy-knowledge-base* nil)
          (*default-pathname-defaults* #P"/tmp/"))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (let ((strat (make-strategy
                          :name name
                          :symbol "USDJPY"
                          :cpcv-median-sharpe 1.1
                          :cpcv-median-pf 1.4
                          :cpcv-median-wr 0.52
                          :cpcv-median-maxdd 0.12
                          :cpcv-pass-rate 0.8)))
              (upsert-strategy strat))
            (let ((strat2 (make-strategy :name name :symbol "USDJPY")))
              (upsert-strategy strat2))
            (let ((median (execute-single "SELECT cpcv_median FROM strategies WHERE name = ?" name))
                  (pf (execute-single "SELECT cpcv_median_pf FROM strategies WHERE name = ?" name))
                  (wr (execute-single "SELECT cpcv_median_wr FROM strategies WHERE name = ?" name))
                  (maxdd (execute-single "SELECT cpcv_median_maxdd FROM strategies WHERE name = ?" name))
                  (pass (execute-single "SELECT cpcv_pass_rate FROM strategies WHERE name = ?" name)))
              (assert-true (< (abs (- median 1.1)) 0.0001)
                           "cpcv_median should be preserved")
              (assert-true (< (abs (- pf 1.4)) 0.0001)
                           "cpcv_median_pf should be preserved")
              (assert-true (< (abs (- wr 0.52)) 0.0001)
                           "cpcv_median_wr should be preserved")
              (assert-true (< (abs (- maxdd 0.12)) 0.0001)
                           "cpcv_median_maxdd should be preserved")
              (assert-true (< (abs (- pass 0.8)) 0.0001)
                           "cpcv_pass_rate should be preserved")))
        (ignore-errors (execute-non-query "DELETE FROM strategies WHERE name = ?" name))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-upsert-preserves-archive-rank
  "Upsert should not accidentally revive RETIRED/GRAVEYARD rows to active/NIL."
  (let* ((name "TEST-ARCHIVE-RANK-LOCK")
         (tmp-db (format nil "/tmp/swimmy-archive-rank-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t)
          (*strategy-knowledge-base* nil)
          (*default-pathname-defaults* #P"/tmp/"))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            ;; First write archived rank.
            (upsert-strategy (make-strategy :name name :symbol "USDJPY" :rank :retired))
            ;; Later upsert without rank information should not revive it.
            (upsert-strategy (make-strategy :name name :symbol "USDJPY" :rank nil))
            (let ((rank (execute-single "SELECT rank FROM strategies WHERE name = ?" name)))
              (assert-equal ":RETIRED" rank "Archive rank must be preserved")))
        (ignore-errors (execute-non-query "DELETE FROM strategies WHERE name = ?" name))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-upsert-preserves-active-rank-when-incoming-nil
  "Upsert should keep existing active rank when incoming strategy rank is NIL."
  (let* ((name "TEST-ACTIVE-RANK-LOCK")
         (tmp-db (format nil "/tmp/swimmy-active-rank-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t)
          (*strategy-knowledge-base* nil)
          (*default-pathname-defaults* #P"/tmp/"))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            ;; First write active rank.
            (upsert-strategy (make-strategy :name name :symbol "USDJPY" :rank :A))
            ;; Later upsert from stale object without rank should not demote to NIL.
            (upsert-strategy (make-strategy :name name :symbol "USDJPY" :rank nil))
            (let ((rank (execute-single "SELECT rank FROM strategies WHERE name = ?" name)))
              (assert-equal ":A" rank "Active rank must be preserved")))
        (ignore-errors (execute-non-query "DELETE FROM strategies WHERE name = ?" name))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-upsert-preserves-active-rank-when-incoming-lower
  "Upsert should keep existing active rank when incoming rank regresses (e.g., A -> B)."
  (let* ((name "TEST-ACTIVE-RANK-REGRESSION-LOCK")
         (tmp-db (format nil "/tmp/swimmy-active-regression-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t)
          (*strategy-knowledge-base* nil)
          (*default-pathname-defaults* #P"/tmp/"))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            ;; First write active rank A.
            (upsert-strategy (make-strategy :name name :symbol "USDJPY" :rank :A))
            ;; Later upsert from stale object with lower rank should not demote DB rank.
            (let ((stale (make-strategy :name name :symbol "USDJPY" :rank :B)))
              (upsert-strategy stale)
              (assert-equal :A (strategy-rank stale)
                            "In-memory stale object should be corrected to DB rank"))
            (let ((rank (execute-single "SELECT rank FROM strategies WHERE name = ?" name)))
              (assert-equal ":A" rank "Active rank must not regress to B on stale upsert")))
        (ignore-errors (execute-non-query "DELETE FROM strategies WHERE name = ?" name))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-upsert-allows-explicit-rank-regression
  "Upsert should allow explicit rank regression when the guard is intentionally bypassed."
  (let* ((name "TEST-ACTIVE-RANK-REGRESSION-EXPLICIT")
         (tmp-db (format nil "/tmp/swimmy-active-regression-explicit-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t)
          (*strategy-knowledge-base* nil)
          (*default-pathname-defaults* #P"/tmp/"))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (upsert-strategy (make-strategy :name name :symbol "USDJPY" :rank :A))
            (let ((regression (make-strategy :name name :symbol "USDJPY" :rank :B)))
              (let ((swimmy.school::*allow-rank-regression-write* t))
                (upsert-strategy regression)))
            (let ((rank (execute-single "SELECT rank FROM strategies WHERE name = ?" name)))
              (assert-equal ":B" rank "Explicit downgrade path should be honored")))
        (ignore-errors (execute-non-query "DELETE FROM strategies WHERE name = ?" name))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-ensure-rank-triggers-report-sync-on-promotion
  "ensure-rank should trigger evolution report sync on A/S promotion."
  (let* ((strat (make-strategy :name "TEST-RANK-SYNC" :symbol "USDJPY" :rank :B))
         (orig-upsert (symbol-function 'swimmy.school:upsert-strategy))
         (orig-notify (and (fboundp 'swimmy.school::notify-noncorrelated-promotion)
                           (symbol-function 'swimmy.school::notify-noncorrelated-promotion)))
         (had-sync (fboundp 'swimmy.school::maybe-sync-evolution-report-on-promotion))
         (orig-sync (and had-sync
                         (symbol-function 'swimmy.school::maybe-sync-evolution-report-on-promotion)))
         (sync-calls 0)
         (last-rank nil))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school:upsert-strategy)
                (lambda (&rest args) (declare (ignore args)) nil))
          (when orig-notify
            (setf (symbol-function 'swimmy.school::notify-noncorrelated-promotion)
                  (lambda (&rest args) (declare (ignore args)) nil)))
          (setf (symbol-function 'swimmy.school::maybe-sync-evolution-report-on-promotion)
                (lambda (&key rank &allow-other-keys)
                  (incf sync-calls)
                  (setf last-rank rank)
                  t))
          (swimmy.school::ensure-rank strat :A "test promotion")
          (assert-equal 1 sync-calls "Expected one report sync on promotion")
          (assert-equal :A last-rank "Expected promotion rank to be forwarded"))
      (setf (symbol-function 'swimmy.school:upsert-strategy) orig-upsert)
      (when orig-notify
        (setf (symbol-function 'swimmy.school::notify-noncorrelated-promotion) orig-notify))
      (if had-sync
          (setf (symbol-function 'swimmy.school::maybe-sync-evolution-report-on-promotion) orig-sync)
          (fmakunbound 'swimmy.school::maybe-sync-evolution-report-on-promotion)))))

(deftest test-kill-strategy-persists-status
  "Soft kill should persist status to DB to survive restarts."
  (let* ((name "TEST-KILL-PERSIST")
         (tmp-db (format nil "/tmp/swimmy-kill-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (*strategy-knowledge-base* nil)
          (*default-pathname-defaults* #P"/tmp/"))
      (let ((orig-notify (and (fboundp 'swimmy.school::notify-discord-alert)
                              (symbol-function 'swimmy.school::notify-discord-alert))))
        (unwind-protect
            (progn
              (when orig-notify
                (setf (symbol-function 'swimmy.school::notify-discord-alert)
                      (lambda (&rest args) (declare (ignore args)) nil)))
              (swimmy.school::init-db)
              (let ((strat (make-strategy :name name :symbol "USDJPY")))
                (setf *strategy-knowledge-base* (list strat))
                (upsert-strategy strat)
                (kill-strategy name "Unit Test Kill")
                (let* ((sexp-str (execute-single "SELECT data_sexp FROM strategies WHERE name = ?" name))
                       (*package* (find-package :swimmy.school))
                       (obj (read-from-string sexp-str)))
                  (assert-equal :killed (strategy-status obj) "DB should persist :killed status")
                  (assert-true (search "SOFT_KILL" (or (strategy-status-reason obj) ""))
                               "DB should persist status reason"))))
          (when orig-notify
            (setf (symbol-function 'swimmy.school::notify-discord-alert) orig-notify))
          (ignore-errors (execute-non-query "DELETE FROM strategies WHERE name = ?" name))
          (ignore-errors (close-db-connection))
          (ignore-errors (delete-file tmp-db)))))))

(deftest test-max-age-retire-batched-notification
  "Max Age Retirement soft-kill should batch notifications and flush hourly."
  (let* ((name "TEST-MAX-AGE-RETIRE")
         (tmp-db (format nil "/tmp/swimmy-max-age-~a.db" (get-universal-time)))
         (sent-messages nil))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (*strategy-knowledge-base* nil)
          (*default-pathname-defaults* #P"/tmp/"))
      (let ((orig-notify (and (fboundp 'swimmy.school::notify-discord-alert)
                              (symbol-function 'swimmy.school::notify-discord-alert))))
        (unwind-protect
            (progn
              (setf swimmy.core::*max-age-retire-buffer* nil)
              (setf swimmy.core::*max-age-retire-first-seen* 0)
              (when orig-notify
                (setf (symbol-function 'swimmy.school::notify-discord-alert)
                      (lambda (msg &key color)
                        (declare (ignore color))
                        (push msg sent-messages)
                        t)))
              (swimmy.school::init-db)
              (let ((strat (make-strategy :name name :symbol "USDJPY")))
                (setf *strategy-knowledge-base* (list strat))
                (upsert-strategy strat)
                (kill-strategy name "Max Age Retirement")
                (assert-equal 0 (length sent-messages) "Should suppress individual alert")
                (assert-equal 1 (length swimmy.core::*max-age-retire-buffer*)
                              "Should buffer max-age retire notification")
                (let ((start swimmy.core::*max-age-retire-first-seen*))
                  (swimmy.core::maybe-flush-max-age-retire (+ start 3601))
                  (assert-equal 1 (length sent-messages) "Should flush summary after 1 hour")
                  (assert-true (search "Max Age Retirement Summary" (car sent-messages))
                               "Summary title should be included")
                  (assert-true (search name (car sent-messages))
                               "Summary should include strategy name"))))
          (when orig-notify
            (setf (symbol-function 'swimmy.school::notify-discord-alert) orig-notify))
          (ignore-errors (execute-non-query "DELETE FROM strategies WHERE name = ?" name))
          (ignore-errors (close-db-connection))
          (ignore-errors (delete-file tmp-db)))))))

(deftest test-refresh-strategy-metrics-uses-local-index
  "refresh-strategy-metrics-from-db should not depend on find-strategy per row."
  (let* ((name "TEST-REFRESH-INDEX")
         (updated-sharpe 1.23)
         (tmp-db (format nil "/tmp/swimmy-refresh-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (*default-pathname-defaults* #P"/tmp/")
          (*strategy-knowledge-base* nil)
          (swimmy.globals:*evolved-strategies* nil))
      (let ((orig-find (symbol-function 'swimmy.school::find-strategy)))
        (unwind-protect
            (progn
              (swimmy.school::init-db)
              (let ((strat (make-strategy :name name :symbol "USDJPY" :sharpe 0.1)))
                (setf *strategy-knowledge-base* (list strat))
                (upsert-strategy strat)
                (execute-non-query
                 "UPDATE strategies SET sharpe = ?, updated_at = ? WHERE name = ?"
                 updated-sharpe (get-universal-time) name)
                (setf (symbol-function 'swimmy.school::find-strategy)
                      (lambda (&rest args)
                        (declare (ignore args))
                        (error "find-strategy should not be called during refresh")))
                (swimmy.school::refresh-strategy-metrics-from-db :force t)
                (assert-true (< (abs (- (strategy-sharpe strat) updated-sharpe)) 0.0001)
                             "strategy sharpe should refresh from DB")))
          (setf (symbol-function 'swimmy.school::find-strategy) orig-find)
          (ignore-errors (execute-non-query "DELETE FROM strategies WHERE name = ?" name))
          (ignore-errors (close-db-connection))
          (ignore-errors (delete-file tmp-db)))))))

(deftest test-stagnant-crank-batched-notification
  "Stagnant C-Rank soft-kill should batch notifications and flush hourly."
  (let* ((name "TEST-STAGNANT-C-RANK")
         (tmp-db (format nil "/tmp/swimmy-stagnant-crank-~a.db" (get-universal-time)))
         (sent-messages nil))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (*strategy-knowledge-base* nil)
          (*default-pathname-defaults* #P"/tmp/"))
      (let ((orig-notify (and (fboundp 'swimmy.school::notify-discord-alert)
                              (symbol-function 'swimmy.school::notify-discord-alert))))
        (unwind-protect
            (progn
              (setf swimmy.core::*stagnant-crank-retire-buffer* nil)
              (setf swimmy.core::*stagnant-crank-retire-first-seen* 0)
              (when orig-notify
                (setf (symbol-function 'swimmy.school::notify-discord-alert)
                      (lambda (msg &key color)
                        (declare (ignore color))
                        (push msg sent-messages)
                        t)))
              (swimmy.school::init-db)
              (let ((strat (make-strategy :name name :symbol "USDJPY")))
                (setf *strategy-knowledge-base* (list strat))
                (upsert-strategy strat)
                (kill-strategy name "Cull: Stagnant C-Rank (0.15) after 10 days")
                (assert-equal 0 (length sent-messages) "Should suppress individual alert")
                (assert-equal 1 (length swimmy.core::*stagnant-crank-retire-buffer*)
                              "Should buffer stagnant C-rank notification")
                (let ((start swimmy.core::*stagnant-crank-retire-first-seen*))
                  (swimmy.core::maybe-flush-stagnant-crank-retire (+ start 3601))
                  (assert-equal 1 (length sent-messages) "Should flush summary after 1 hour")
                  (assert-true (search "Stagnant C-Rank Summary" (car sent-messages))
                               "Summary title should be included")
                  (assert-true (search name (car sent-messages))
                               "Summary should include strategy name"))))
          (when orig-notify
            (setf (symbol-function 'swimmy.school::notify-discord-alert) orig-notify))
          (ignore-errors (execute-non-query "DELETE FROM strategies WHERE name = ?" name))
          (ignore-errors (close-db-connection))
          (ignore-errors (delete-file tmp-db)))))))

(deftest test-kill-strategy-reason-code-stagnant-crank
  "Reason code should route Stagnant C-Rank without string search"
  (let* ((name "TEST-STAGNANT-C-RANK-CODE")
         (tmp-db (format nil "/tmp/swimmy-stagnant-crank-code-~a.db" (get-universal-time)))
         (sent-messages nil)
         (ok nil))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (*strategy-knowledge-base* nil)
          (*default-pathname-defaults* #P"/tmp/"))
      (let ((orig-notify (and (fboundp 'swimmy.school::notify-discord-alert)
                              (symbol-function 'swimmy.school::notify-discord-alert))))
        (unwind-protect
            (progn
              (setf swimmy.core::*stagnant-crank-retire-buffer* nil)
              (setf swimmy.core::*stagnant-crank-retire-first-seen* 0)
              (when orig-notify
                (setf (symbol-function 'swimmy.school::notify-discord-alert)
                      (lambda (msg &key color)
                        (declare (ignore color))
                        (push msg sent-messages)
                        t)))
              (swimmy.school::init-db)
              (let ((strat (make-strategy :name name :symbol "USDJPY")))
                (setf *strategy-knowledge-base* (list strat))
                (upsert-strategy strat)
                (handler-case
                    (progn
                      (kill-strategy name "Unrelated reason" :reason-code :stagnant-crank)
                      (setf ok t))
                  (error () (setf ok nil)))
                (assert-true ok "kill-strategy should accept reason-code")
                (assert-equal 0 (length sent-messages) "Should suppress individual alert")
                (assert-equal 1 (length swimmy.core::*stagnant-crank-retire-buffer*)
                              "Should buffer stagnant C-rank notification")))
          (when orig-notify
            (setf (symbol-function 'swimmy.school::notify-discord-alert) orig-notify))
          (ignore-errors (execute-non-query "DELETE FROM strategies WHERE name = ?" name))
          (ignore-errors (close-db-connection))
          (ignore-errors (delete-file tmp-db)))))))

(deftest test-sqlite-wal-mode-enabled
  "Ensure init-db sets SQLite to WAL mode for concurrency."
  (let ((tmp-db (format nil "/tmp/swimmy-wal-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
           (progn
             (swimmy.school::init-db)
             (let ((mode (execute-single "PRAGMA journal_mode")))
               (assert-true (member (string-downcase mode) '("wal" "wal2") :test #'string=)
                            "journal_mode should be WAL")))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-init-db-creates-strategy-lookup-indexes
  "Ensure init-db creates rank/category lookup indexes for strategies."
  (let ((tmp-db (format nil "/tmp/swimmy-index-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
           (progn
             (swimmy.school::init-db)
             (let* ((rows (execute-to-list "PRAGMA index_list('strategies')"))
                    (names (mapcar (lambda (r) (string-upcase (or (second r) ""))) rows)))
               (assert-true (find "IDX_STRATEGIES_RANK_SHARPE" names :test #'string=)
                            "rank+sharpe index should exist")
               (assert-true (find "IDX_STRATEGIES_CATEGORY_RANK" names :test #'string=)
                            "category+rank index should exist")))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-migrate-existing-data-skips-corrupted-graveyard-lines
  "migrate-existing-data should skip malformed graveyard lines and continue with valid entries."
  (let* ((tmp-db (format nil "/tmp/swimmy-migrate-~a.db" (get-universal-time)))
         (tmp-root (format nil "/tmp/swimmy-migrate-root-~a/" (get-universal-time)))
         (graveyard-dir (merge-pathnames "data/memory/" tmp-root))
         (graveyard-file (merge-pathnames "graveyard.sexp" graveyard-dir)))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t)
          (*strategy-knowledge-base* nil)
          (swimmy.globals:*evolved-strategies* nil)
          (*default-pathname-defaults* (uiop:ensure-directory-pathname tmp-root)))
      (unwind-protect
          (progn
            (ensure-directories-exist graveyard-dir)
            (with-open-file (out graveyard-file
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
              ;; Broken line first: old code would stop here and skip the valid line below.
              (write-line ")" out)
              (write-line "(:name \"VALID-1\" :indicators ((:SMA 20)) :entry (> close 0) :exit (< close 0))" out))
            (swimmy.school::init-db)
            (swimmy.school::%migrate-graveyard-patterns graveyard-file)
            (let* ((gy-count (execute-single
                              "SELECT count(*) FROM strategies WHERE rank = ':GRAVEYARD'"))
                   (row (first (execute-to-list
                                "SELECT name FROM strategies WHERE rank = ':GRAVEYARD'"))))
              (assert-equal 1 gy-count "Expected one valid graveyard entry to be migrated")
              (assert-equal "GY-VALID-1" (first row)
                            "Expected valid graveyard entry name to be migrated")))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))
        (ignore-errors (delete-file graveyard-file))))))

(deftest test-refresh-strategy-metrics-incremental
  "Incremental refresh should update only rows changed since timestamp."
  (let ((tmp-db (format nil "/tmp/swimmy-inc-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (*strategy-knowledge-base* nil))
      (unwind-protect
           (progn
             (swimmy.school::init-db)
             (let* ((s1 (make-strategy :name "INC-A" :sharpe 0.1 :symbol "USDJPY"))
                    (s2 (make-strategy :name "INC-B" :sharpe 0.1 :symbol "USDJPY")))
               (setf *strategy-knowledge-base* (list s1 s2))
               (upsert-strategy s1)
               (upsert-strategy s2)
               (let ((ts (get-universal-time)))
                 (sleep 1)
                 (setf (strategy-sharpe s1) 0.9)
                 (upsert-strategy s1)
                 ;; Corrupt in-memory s2 to ensure it doesn't refresh without update
                 (setf (strategy-sharpe s2) 5.0)
                 (refresh-strategy-metrics-from-db :force t :since-timestamp ts)
                (assert-true (> (strategy-sharpe s1) 0.8) "s1 should refresh to latest DB sharpe")
                (assert-true (= (strategy-sharpe s2) 5.0) "s2 should remain unchanged without update"))))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-collect-all-strategies-unpruned
  "Ensure DB strategies are returned even if rank is graveyard."
  (let* ((name-a "TEST-ALL-STRATS-A")
         (name-g "TEST-ALL-STRATS-GRAVE")
         (tmp-db (format nil "/tmp/swimmy-test-~a.db" (get-universal-time)))
         (tmp-lib (format nil "/tmp/swimmy-lib-~a/" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.persistence::*library-path* (merge-pathnames tmp-lib #P"/"))
          (*default-pathname-defaults* #P"/tmp/"))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (swimmy.persistence:init-library)
            (let ((strat-a (make-strategy
                            :name name-a
                            :indicators '((sma 20))
                            :entry '(> close (sma 20))
                            :exit '(< close (sma 20))
                            :sharpe 0.2
                            :symbol "USDJPY"
                            :timeframe 1
                            :direction :BOTH
                            :rank :A))
                  (strat-g (make-strategy
                            :name name-g
                            :indicators '((sma 10))
                            :entry '(> close (sma 10))
                            :exit '(< close (sma 10))
                            :sharpe -0.5
                            :symbol "USDJPY"
                            :timeframe 1
                            :direction :BOTH
                            :rank :graveyard)))
              (upsert-strategy strat-a)
              (upsert-strategy strat-g)
              (let ((all (swimmy.school::collect-all-strategies-unpruned)))
                (assert-true (= 2 (length all)) "Should return both strategies")
                (assert-true (find name-a all :key #'strategy-name :test #'string=)
                             "A-rank strategy should be present")
                (assert-true (find name-g all :key #'strategy-name :test #'string=)
                             "Graveyard strategy should be present"))))
        (ignore-errors (execute-non-query "DELETE FROM strategies WHERE name IN (?,?)" name-a name-g))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-map-strategies-from-db-batched
  "Ensure batched DB iteration yields all strategies."
  (let* ((name-a "TEST-MAP-A")
         (name-b "TEST-MAP-B")
         (name-c "TEST-MAP-C")
         (tmp-db (format nil "/tmp/swimmy-test-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (*default-pathname-defaults* #P"/tmp/"))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (dolist (name (list name-a name-b name-c))
              (upsert-strategy
               (make-strategy
                :name name
                :indicators '((sma 20))
                :entry '(> close (sma 20))
                :exit '(< close (sma 20))
                :sharpe 0.2
                :symbol "USDJPY"
                :timeframe 1
                :direction :BOTH)))
            (let* ((names nil)
                   (count (swimmy.school::map-strategies-from-db
                           (lambda (s) (push (strategy-name s) names))
                           :batch-size 2)))
              (assert-true (= 3 count) "Should visit all rows")
              (assert-true (find name-a names :test #'string=) "Name A present")
              (assert-true (find name-b names :test #'string=) "Name B present")
              (assert-true (find name-c names :test #'string=) "Name C present")))
        (ignore-errors (execute-non-query "DELETE FROM strategies WHERE name IN (?,?,?)" name-a name-b name-c))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-map-strategies-from-db-limit
  "Ensure limit stops DB iteration early."
  (let* ((name-a "TEST-MAP-LIMIT-A")
         (name-b "TEST-MAP-LIMIT-B")
         (tmp-db (format nil "/tmp/swimmy-test-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (*default-pathname-defaults* #P"/tmp/"))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (dolist (name (list name-a name-b))
              (upsert-strategy
               (make-strategy
                :name name
                :indicators '((sma 20))
                :entry '(> close (sma 20))
                :exit '(< close (sma 20))
                :sharpe 0.2
                :symbol "USDJPY"
                :timeframe 1
                :direction :BOTH)))
            (let* ((names nil)
                   (count (swimmy.school::map-strategies-from-db
                           (lambda (s) (push (strategy-name s) names))
                           :batch-size 1
                           :limit 1)))
              (assert-true (= 1 count) "Should stop at limit")
              (assert-true (= 1 (length names)) "Only one strategy should be visited")))
        (ignore-errors (execute-non-query "DELETE FROM strategies WHERE name IN (?,?)" name-a name-b))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-db-rank-counts
  "DB rank counts should reflect stored ranks (including graveyard and unranked)."
  (let ((tmp-db (format nil "/tmp/swimmy-ranks-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (dolist (spec '(("R-S" :S) ("R-A" :A) ("R-B" :B) ("R-G" :GRAVEYARD) ("R-R" :RETIRED) ("R-N" nil)))
              (destructuring-bind (name rank) spec
                (swimmy.school::upsert-strategy
                 (make-strategy :name name :sharpe 0.2 :symbol "USDJPY" :rank rank))))
            (let* ((counts (swimmy.school::get-db-rank-counts))
                   (s (getf counts :s))
                   (a (getf counts :a))
                   (b (getf counts :b))
                   (g (getf counts :graveyard))
                   (r (getf counts :retired))
                   (u (getf counts :unranked))
                   (total (getf counts :total))
                   (active (getf counts :active)))
              (assert-true (= 1 s) "S count")
              (assert-true (= 1 a) "A count")
              (assert-true (= 1 b) "B count")
              (assert-true (= 1 g) "Graveyard count")
              (assert-true (= 1 r) "Retired count")
              (assert-true (= 1 u) "Unranked count")
              (assert-true (= 6 total) "Total count")
              (assert-true (= 4 active) "Active excludes graveyard + retired")))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-trade-logs-supports-pair-id
  "trade_logs should accept pair_id and persist it"
  (let* ((tmp-db (format nil "/tmp/swimmy-pair-~a.db" (get-universal-time)))
         (pair-id "PAIR-ABC"))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (swimmy.school::record-trade-to-db
             (swimmy.school::make-trade-record
              :timestamp 1
              :strategy-name "TEST"
              :symbol "USDJPY"
              :direction :buy
              :category :trend
              :regime :trend
              :pnl 1.0
              :hold-time 10
              :pair-id pair-id))
            (let ((row (swimmy.school::execute-single
                        "SELECT pair_id FROM trade_logs WHERE strategy_name = ?" "TEST")))
              (assert-equal pair-id row "pair_id should persist")))
        (ignore-errors (swimmy.school::close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-strategy-daily-pnl-aggregation
  "Daily aggregation should sum pnl per strategy per day"
  (let* ((tmp-db (format nil "/tmp/swimmy-daily-~a.db" (get-universal-time)))
         (t1 (encode-universal-time 0 0 12 1 2 2026))
         (t2 (encode-universal-time 0 30 12 1 2 2026))
         (t3 (encode-universal-time 0 0 12 2 2 2026)))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (dolist (spec (list (list t1 1.0) (list t2 2.0) (list t3 -1.0)))
              (destructuring-bind (ts pnl) spec
                (swimmy.school::record-trade-to-db
                 (swimmy.school::make-trade-record
                  :timestamp ts
                  :strategy-name "TEST"
                  :symbol "USDJPY"
                  :direction :buy
                  :category :trend
                  :regime :trend
                  :pnl pnl
                  :hold-time 10))))
            (swimmy.school::refresh-strategy-daily-pnl)
            (let* ((day1 (swimmy.school::execute-single
                          "SELECT date(datetime(? - 2208988800, 'unixepoch', 'localtime'))" t1))
                   (day2 (swimmy.school::execute-single
                          "SELECT date(datetime(? - 2208988800, 'unixepoch', 'localtime'))" t3))
                   (rows (swimmy.school::execute-to-list
                          "SELECT trade_date, pnl_sum, trade_count FROM strategy_daily_pnl WHERE strategy_name = ?"
                          "TEST"))
                   (table (make-hash-table :test 'equal)))
              (dolist (row rows)
                (setf (gethash (first row) table) (list (second row) (third row))))
              (assert-true (= (length rows) 2) "Expected two daily rows")
              (let ((day1-row (gethash day1 table))
                    (day2-row (gethash day2 table)))
                (assert-true (and day1-row (= (first day1-row) 3.0) (= (second day1-row) 2))
                             "Day1 sum/count")
                (assert-true (and day2-row (= (first day2-row) -1.0) (= (second day2-row) 1))
                             "Day2 sum/count"))))
        (ignore-errors (swimmy.school::close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-backtest-trade-logs-insert
  "backtest_trade_logs should accept trade_list rows"
  (let* ((tmp-db (format nil "/tmp/swimmy-bt-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (swimmy.school:record-backtest-trades
             "RID-1" "STRAT-A" "BACKTEST"
             (list (list :timestamp 1 :pnl 1.0 :symbol "USDJPY" :direction "BUY" :volume 0.01)))
            (let ((count (swimmy.school::execute-single
                          "SELECT count(*) FROM backtest_trade_logs")))
              (assert-true (= count 1) "Expected 1 backtest trade row")))
        (ignore-errors (swimmy.school::close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-backtest-trade-logs-insert-accepts-alist-symbol-keys
  "record-backtest-trades should accept alist trade_list keys even when read in another package (e.g., swimmy.main)."
  (let* ((tmp-db (format nil "/tmp/swimmy-bt-alist-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (let* ((ts-key (intern "TIMESTAMP" :swimmy.main))
                   (pnl-key (intern "PNL" :swimmy.main))
                   (trade (list (cons ts-key 1) (cons pnl-key 1.0))))
              (swimmy.school:record-backtest-trades
               "RID-ALIST" "STRAT-ALIST" "BACKTEST"
               (list trade)))
            (let* ((row (first (swimmy.school::execute-to-list
                                "SELECT timestamp, pnl FROM backtest_trade_logs WHERE request_id = ?"
                                "RID-ALIST"))))
              (assert-true row "Expected one inserted row")
              (assert-equal 1 (first row) "timestamp should persist from alist keys")
              (let ((pnl (second row)))
                ;; SQLite drivers may round-trip REAL as double-float.
                (assert-true (and (numberp pnl)
                                  (< (abs (- (float pnl 0.0) 1.0)) 1e-6))
                             "pnl should persist from alist keys"))))
        (ignore-errors (swimmy.school::close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-fetch-backtest-trades
  "fetch-backtest-trades should return rows with optional oos_kind filter"
  (let* ((tmp-db (format nil "/tmp/swimmy-bt-fetch-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (swimmy.school:record-backtest-trades
             "RID-1" "STRAT-A" "BACKTEST"
             (list (list :timestamp 1 :pnl 1.0 :symbol "USDJPY" :direction "BUY")))
            (swimmy.school:record-backtest-trades
             "RID-2" "STRAT-A" "OOS"
             (list (list :timestamp 2 :pnl 2.0 :symbol "USDJPY" :direction "SELL")))
            (let ((rows-all (swimmy.school:fetch-backtest-trades "STRAT-A"))
                  (rows-oos (swimmy.school:fetch-backtest-trades "STRAT-A" :oos-kind "OOS")))
              (assert-true (= (length rows-all) 2) "Expected 2 rows")
              (assert-true (= (length rows-oos) 1) "Expected 1 OOS row")
              (let ((row (first rows-oos)))
                (assert-equal "RID-2" (first row) "request_id should match")
                (assert-equal "OOS" (nth 16 row) "oos_kind should match"))))
        (ignore-errors (swimmy.school::close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-pair-strategy-upsert-fetch
  "pair_strategies should upsert and fetch records"
  (let* ((tmp-db (format nil "/tmp/swimmy-pair-strat-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (*default-pathname-defaults* #P"/tmp/"))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (let ((pair (list :pair-id "P1"
                              :strategy-a "A"
                              :strategy-b "B"
                              :weight-a 0.6
                              :weight-b 0.4
                              :symbol "USDJPY"
                              :timeframe 1
                              :sharpe 0.8
                              :profit-factor 1.5
                              :score 1.0
                              :corr 0.1
                              :rank :A
                              :oos-sharpe 0.7
                              :cpcv-median 0.6
                              :cpcv-pass-rate 0.8)))
              (swimmy.school::upsert-pair-strategy pair)
              (let ((fetched (swimmy.school::fetch-pair-strategy "P1")))
                (assert-equal "P1" (getf fetched :pair-id) "pair_id should match")
                (assert-equal "A" (getf fetched :strategy-a) "strategy_a should match")
                (assert-equal :A (getf fetched :rank) "rank should match")
                (assert-true (> (getf fetched :last-updated) 0) "last_updated should be set"))))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-report-source-drift-detects-mismatch
  "Drift check should flag mismatched DB/KB/Library counts."
  (let* ((tmp-db (format nil "/tmp/swimmy-drift-~a.db" (get-universal-time)))
         (tmp-lib (format nil "/tmp/swimmy-lib-drift-~a/" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.persistence::*library-path* (merge-pathnames tmp-lib #P"/"))
          (swimmy.school::*disable-auto-migration* t)
          (*strategy-knowledge-base* nil))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (swimmy.persistence:init-library)
            ;; DB: one graveyard, one active
            (swimmy.school::upsert-strategy (make-strategy :name "D-A" :sharpe 0.2 :symbol "USDJPY" :rank :B))
            (swimmy.school::upsert-strategy (make-strategy :name "D-G" :sharpe -0.2 :symbol "USDJPY" :rank :graveyard))
            ;; KB: empty
            (setf *strategy-knowledge-base* nil)
            ;; Library: no graveyard files
            (let ((warnings (swimmy.school::report-source-drift)))
              (assert-true (> (length warnings) 0) "Drift warnings should be reported")
              (assert-true
               (find-if (lambda (w) (search "delta(DB-LibraryCanonical)=" w)) warnings)
               "Drift warnings should include delta(DB-LibraryCanonical) direction")))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-report-source-drift-detects-canonical-archive-mismatch
  "Drift check should include canonical archive mismatch for DB/LIB unique-name delta."
  (let* ((tmp-db (format nil "/tmp/swimmy-drift-canon-~a.db" (get-universal-time)))
         (tmp-lib (format nil "/tmp/swimmy-lib-drift-canon-~a/" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.persistence::*library-path* (merge-pathnames tmp-lib #P"/"))
          (swimmy.school::*disable-auto-migration* t)
          (*strategy-knowledge-base* nil))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (swimmy.persistence:init-library)
            ;; DB archive has 1 unique name.
            (swimmy.school::upsert-strategy (make-strategy :name "CAN-DB-G" :sharpe -0.2 :symbol "USDJPY" :rank :graveyard))
            ;; Library archive has 2 unique names (one overlaps DB, one extra).
            (let* ((gy-dir (merge-pathnames "GRAVEYARD/" swimmy.persistence::*library-path*))
                   (rt-dir (merge-pathnames "RETIRED/" swimmy.persistence::*library-path*))
                   (gy-file (merge-pathnames "CAN-DB-G.lisp" gy-dir))
                   (rt-file (merge-pathnames "CAN-LIB-R.lisp" rt-dir)))
              (ensure-directories-exist gy-file)
              (ensure-directories-exist rt-file)
              (with-open-file (s gy-file :direction :output :if-exists :supersede :if-does-not-exist :create)
                (write-line "(dummy)" s))
              (with-open-file (s rt-file :direction :output :if-exists :supersede :if-does-not-exist :create)
                (write-line "(dummy)" s)))
            (let ((warnings (swimmy.school::report-source-drift)))
              (assert-true
               (find-if (lambda (w) (search "Archive canonical (" w)) warnings)
               "Drift warnings should include canonical archive mismatch")
              (assert-true
               (find-if (lambda (w) (search "delta(DB-LibraryCanonical)=" w)) warnings)
               "Canonical archive warning should include delta(DB-LibraryCanonical)")
              (assert-true
               (find-if (lambda (w) (search "Library canonical=2" w)) warnings)
               "Canonical archive warning should include expected library canonical count")
              (assert-true
               (find-if (lambda (w) (search "delta(DB-LibraryCanonical)=-1" w)) warnings)
               "Canonical archive warning should include expected canonical delta")))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-report-source-drift-includes-canonical-line-even-when-delta-zero
  "Canonical archive line should appear for archive drift even when DB/LIB canonical counts match."
  (let* ((tmp-db (format nil "/tmp/swimmy-drift-canon-zero-~a.db" (get-universal-time)))
         (tmp-lib (format nil "/tmp/swimmy-lib-drift-canon-zero-~a/" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.persistence::*library-path* (merge-pathnames tmp-lib #P"/"))
          (swimmy.school::*disable-auto-migration* t)
          (*strategy-knowledge-base* nil))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (swimmy.persistence:init-library)
            ;; DB archive unique count = 2
            (swimmy.school::upsert-strategy (make-strategy :name "CAN-ZERO-A" :sharpe -0.2 :symbol "USDJPY" :rank :graveyard))
            (swimmy.school::upsert-strategy (make-strategy :name "CAN-ZERO-B" :sharpe -0.2 :symbol "USDJPY" :rank :retired))
            ;; Library per-dir counts differ, but canonical unique count still = 2.
            (let* ((gy-dir (merge-pathnames "GRAVEYARD/" swimmy.persistence::*library-path*))
                   (rt-dir (merge-pathnames "RETIRED/" swimmy.persistence::*library-path*)))
              (flet ((write-dummy (path)
                       (ensure-directories-exist path)
                       (with-open-file (s path :direction :output :if-exists :supersede :if-does-not-exist :create)
                         (write-line "(dummy)" s))))
                (write-dummy (merge-pathnames "CAN-ZERO-A.lisp" gy-dir))
                (write-dummy (merge-pathnames "CAN-ZERO-A.lisp" rt-dir))
                (write-dummy (merge-pathnames "CAN-ZERO-B.lisp" rt-dir))))
            (let ((warnings (swimmy.school::report-source-drift)))
              (assert-true
               (find-if (lambda (w) (search "Archive canonical (" w)) warnings)
               "Drift warnings should include canonical archive line")
              (assert-true
               (find-if (lambda (w) (search "delta(DB-LibraryCanonical)=" w)) warnings)
               "Canonical archive line should include delta(DB-LibraryCanonical)")
              (assert-true
               (find-if (lambda (w) (search "Library canonical=2" w)) warnings)
               "Canonical archive line should include expected library canonical count")
              (assert-true
               (find-if (lambda (w) (search "delta(DB-LibraryCanonical)=+0" w)) warnings)
               "Canonical archive line should include +0 canonical delta")))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-evolution-report-uses-db-counts
  "Evolution report should reflect DB counts, not KB/library drift."
  (let* ((tmp-db (format nil "/tmp/swimmy-report-~a.db" (get-universal-time)))
         (tmp-lib (format nil "/tmp/swimmy-lib-report-~a/" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.persistence::*library-path* (merge-pathnames tmp-lib #P"/"))
          (swimmy.school::*disable-auto-migration* t)
          (*strategy-knowledge-base* nil))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (swimmy.persistence:init-library)
            ;; DB: 1 active (B) + 1 graveyard
            (swimmy.school::upsert-strategy (make-strategy :name "ER-B" :sharpe 0.2 :symbol "USDJPY" :rank :B))
            (swimmy.school::upsert-strategy (make-strategy :name "ER-G" :sharpe -0.2 :symbol "USDJPY" :rank :graveyard))
            ;; KB empty to force mismatch if KB used
            (setf *strategy-knowledge-base* nil)
            (let* ((report (swimmy.school::generate-evolution-report))
                   (report-clean (remove #\Return report))
                   (active-needle (format nil "Knowledge Base (Active)~%1 Strategies"))
                   (grave-needle (format nil "👻 Graveyard~%1")))
              (assert-true (search active-needle report-clean)
                           "Active count should be 1 (DB)")
              (assert-true (search grave-needle report-clean)
                           "Graveyard count should be 1 (DB)")))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-top-candidates-excludes-archive-and-hides-nil-rank
  "Top Candidates should exclude Graveyard/Retired and never display NIL."
  (let* ((tmp-db (format nil "/tmp/swimmy-topcand-~a.db" (get-universal-time)))
         (tmp-lib (format nil "/tmp/swimmy-lib-topcand-~a/" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.persistence::*library-path* (merge-pathnames tmp-lib #P"/"))
          (swimmy.school::*disable-auto-migration* t)
          (*strategy-knowledge-base* nil))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (swimmy.persistence:init-library)
            ;; Insert 4 strategies where archive ranks have higher Sharpe (should be excluded).
            (swimmy.school::upsert-strategy (make-strategy :name "TC-ACT" :sharpe 1.0 :symbol "USDJPY" :rank :B))
            (swimmy.school::upsert-strategy (make-strategy :name "TC-GY" :sharpe 2.0 :symbol "USDJPY" :rank :graveyard))
            (swimmy.school::upsert-strategy (make-strategy :name "TC-RT" :sharpe 1.5 :symbol "USDJPY" :rank :retired))
            (swimmy.school::upsert-strategy (make-strategy :name "TC-NIL" :sharpe 3.0 :symbol "USDJPY" :rank nil))
            (let ((snippet (swimmy.school::build-top-candidates-snippet-from-db)))
              (assert-true (search "TC-NIL" snippet) "Unranked strategy should appear")
              (assert-true (search "INCUBATOR" snippet) "NIL rank must be displayed as INCUBATOR")
              (assert-false (search "TC-GY" snippet) "Graveyard strategy must be excluded")
              (assert-false (search "TC-RT" snippet) "Retired strategy must be excluded")
              (assert-false (search ", NIL)" snippet) "Must never display NIL rank literal")))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-cpcv-status-snippet-reads-status-file-and-last-start
  "CPCV status snippet should read cpcv_status.txt (shared) and include last_start_unix."
  (let* ((tmp-status (format nil "/tmp/swimmy-cpcv-status-~a.txt" (get-universal-time)))
         (orig swimmy.school::*cpcv-status-path*)
         (orig-start swimmy.globals:*cpcv-start-time*))
    (unwind-protect
        (progn
          (setf swimmy.school::*cpcv-status-path* tmp-status)
          (setf swimmy.globals:*cpcv-start-time* 0)
          (with-open-file (out tmp-status :direction :output :if-exists :supersede :if-does-not-exist :create)
            (write-line "1 queued | 1 sent | 1 received | 1 failed (send 0 / result 1) | inflight 0" out)
            (write-line "last_start_unix: 1" out)
            (write-line "updated: 01/01 00:00 JST / 00:00 UTC reason: no-candidates" out))
          (let ((snippet (swimmy.school::build-cpcv-status-snippet)))
            (assert-true (search "1 queued" snippet) "Should include summary from file")
            (assert-false (search "last start: N/A" snippet) "Should not be N/A when last_start_unix exists")
            (assert-true (search "reason: no-candidates" snippet)
                         "Should include updated reason from status file")))
      (setf swimmy.school::*cpcv-status-path* orig)
      (setf swimmy.globals:*cpcv-start-time* orig-start)
      (ignore-errors (delete-file tmp-status)))))
