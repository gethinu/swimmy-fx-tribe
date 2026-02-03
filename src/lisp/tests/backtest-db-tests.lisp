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
            (dolist (spec '(("R-S" :S) ("R-A" :A) ("R-B" :B) ("R-G" :GRAVEYARD) ("R-N" nil)))
              (destructuring-bind (name rank) spec
                (swimmy.school::upsert-strategy
                 (make-strategy :name name :sharpe 0.2 :symbol "USDJPY" :rank rank))))
            (let* ((counts (swimmy.school::get-db-rank-counts))
                   (s (getf counts :s))
                   (a (getf counts :a))
                   (b (getf counts :b))
                   (g (getf counts :graveyard))
                   (u (getf counts :unranked))
                   (total (getf counts :total))
                   (active (getf counts :active)))
              (assert-true (= 1 s) "S count")
              (assert-true (= 1 a) "A count")
              (assert-true (= 1 b) "B count")
              (assert-true (= 1 g) "Graveyard count")
              (assert-true (= 1 u) "Unranked count")
              (assert-true (= 5 total) "Total count")
              (assert-true (= 4 active) "Active excludes graveyard")))
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
