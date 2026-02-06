;;; pair-composite-tests.lisp - Pair composite unit tests

(in-package :swimmy.tests)

(deftest test-pair-id-stable
  "pair-id should be order-independent"
  (let ((a (swimmy.school::pair-id "A" "B"))
        (b (swimmy.school::pair-id "B" "A")))
    (assert-equal a b "pair-id should be stable")))

(deftest test-pair-overlay-caps-lot
  "pair overlay should cap lot to 0.05 when enabled"
  (let* ((swimmy.school::*pair-strategy-enabled* t)
         (swimmy.school::*pair-active-defs*
           (list (list :pair-id "P1" :a "A" :b "B" :weight-a 0.5 :weight-b 0.5)))
         (lot (swimmy.school::apply-pair-overlay "A" :buy "USDJPY" 0.2)))
    (assert-true (<= (first lot) 0.05) "lot should be capped")))

(deftest test-pair-candidate-pool-top-n
  "pair candidate pool should select top N per symbol/timeframe"
  (let* ((s1 (swimmy.school::make-strategy :name "S1" :symbol "USDJPY" :timeframe 1 :sharpe 0.1 :rank :A))
         (s2 (swimmy.school::make-strategy :name "S2" :symbol "USDJPY" :timeframe 1 :sharpe 0.2 :rank :A))
         (s3 (swimmy.school::make-strategy :name "S3" :symbol "USDJPY" :timeframe 1 :sharpe 0.3 :rank :A))
         (s4 (swimmy.school::make-strategy :name "E1" :symbol "EURUSD" :timeframe 1 :sharpe 0.5 :rank :A))
         (s5 (swimmy.school::make-strategy :name "B1" :symbol "USDJPY" :timeframe 1 :sharpe 0.9 :rank :B))
         (pool (swimmy.school::pair-candidate-pool (list s1 s2 s3 s4 s5) :per-group 2)))
    (let ((names (mapcar #'swimmy.school::strategy-name pool)))
      (assert-true (= (length pool) 3) "Expected top 2 from USDJPY + 1 from EURUSD")
      (assert-true (and (member "S3" names :test #'string=)
                        (member "S2" names :test #'string=)
                        (member "E1" names :test #'string=))
                   "Expected S3,S2,E1 in pool")
      (assert-false (member "B1" names :test #'string=) "Rank B should be excluded"))))

