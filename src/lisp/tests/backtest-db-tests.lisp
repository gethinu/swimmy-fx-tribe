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
