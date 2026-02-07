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
                             "data_sexp should reflect updated sharpe"))))
        (ignore-errors (execute-non-query "DELETE FROM strategies WHERE name = ?" name))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

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

(deftest test-sqlite-wal-mode-enabled
  "Ensure init-db sets SQLite to WAL mode for concurrency."
  (let ((tmp-db (format nil "/tmp/swimmy-wal-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil))
      (unwind-protect
           (progn
             (swimmy.school::init-db)
             (let ((mode (execute-single "PRAGMA journal_mode")))
               (assert-true (member (string-downcase mode) '("wal" "wal2") :test #'string=)
                            "journal_mode should be WAL")))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

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
              (assert-true (> (length warnings) 0) "Drift warnings should be reported")))
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
