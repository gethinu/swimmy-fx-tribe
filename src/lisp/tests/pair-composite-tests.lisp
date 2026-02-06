;;; pair-composite-tests.lisp - Pair composite unit tests

(in-package :swimmy.tests)

(defun approx= (a b &optional (eps 1e-6))
  (< (abs (- a b)) eps))


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

(deftest test-align-pnl-series-padding
  "align-pnl-series should align timestamps and fill missing with 0.0"
  (let* ((series-a '((1 . 1.0) (3 . -0.5)))
         (series-b '((2 . 2.0) (3 . 0.5)))
         (values (multiple-value-list
                  (swimmy.school::align-pnl-series series-a series-b)))
         (aligned-a (first values))
         (aligned-b (second values)))
    (assert-equal aligned-a '(1.0 0.0 -0.5) "Series A should be padded")
    (assert-equal aligned-b '(0.0 2.0 0.5) "Series B should be padded")))
(deftest test-pair-align-pnl-series-zero-fill
  "align pnl series should zero-fill missing timestamps"
  (let* ((a '(((timestamp . 1) (pnl . 1.0))
              ((timestamp . 3) (pnl . -1.0))))
         (b '(((timestamp . 2) (pnl . 2.0))
              ((timestamp . 3) (pnl . 1.0))))
         (series-a (swimmy.school::trade-list->series a :max-trades 10))
         (series-b (swimmy.school::trade-list->series b :max-trades 10)))
    (multiple-value-bind (xs ys)
        (swimmy.school::align-pnl-series series-a series-b)
      (assert-equal '(1.0 0.0 -1.0) xs "Expected zero-fill for A")
      (assert-equal '(0.0 2.0 1.0) ys "Expected zero-fill for B"))))

(deftest test-pair-pearson-correlation
  "pearson correlation should be +/-1 for identical/inverse series"
  (let* ((xs '(1.0 2.0 3.0))
         (ys '(1.0 2.0 3.0))
         (zs '(3.0 2.0 1.0)))
    (assert-true (approx= 1.0 (swimmy.school::pearson-correlation xs ys)) "Expected corr=1")
    (assert-true (approx= -1.0 (swimmy.school::pearson-correlation xs zs)) "Expected corr=-1")))

(deftest test-pair-score-from-pnls
  "score should use sharpe/pf on composite pnls"
  (let* ((pnls '(1.0 -0.5 1.0 -0.5))
         (result (swimmy.school::pair-score-from-pnls pnls))
         (sharpe (getf result :sharpe))
         (pf (getf result :pf))
         (score (getf result :score)))
    (assert-true (approx= 0.333333 sharpe 1e-4) "Expected sharpe ~0.3333")
    (assert-true (approx= 2.0 pf 1e-6) "Expected PF 2.0")
    (assert-true (approx= 0.833333 score 1e-4) "Expected score ~0.8333")))

(deftest test-pair-inverse-vol-weights
  "inverse vol weights should favor lower volatility"
  (multiple-value-bind (w1 w2)
      (swimmy.school::inverse-vol-weights '(1.0 -1.0) '(2.0 -2.0))
    (assert-true (approx= 0.666666 w1 1e-4) "Expected w1 ~0.6667")
    (assert-true (approx= 0.333333 w2 1e-4) "Expected w2 ~0.3333")))
(deftest test-pair-selection-rescue-mode
  "rescue mode should allow |corr|<=0.3 when < max pairs"
  (let* ((s1 (swimmy.school::make-strategy :name "A" :symbol "USDJPY" :timeframe 1 :sharpe 0.5 :rank :A))
         (s2 (swimmy.school::make-strategy :name "B" :symbol "USDJPY" :timeframe 1 :sharpe 0.4 :rank :A))
         (trade-map (list (cons "A" '(((timestamp . 1) (pnl . 1.0))
                                       ((timestamp . 2) (pnl . -1.0))
                                       ((timestamp . 3) (pnl . 1.0))))
                           (cons "B" '(((timestamp . 1) (pnl . 1.0))
                                       ((timestamp . 2) (pnl . -1.0))
                                       ((timestamp . 3) (pnl . 1.0)))))))
    (multiple-value-bind (pairs diag)
        (swimmy.school::select-pair-candidates
         (list s1 s2)
         trade-map
         :per-group 2
         :min-trades 3
         :max-pairs 5
         :corr-threshold 0.2
         :rescue-threshold 0.3
         :corr-fn (lambda (xs ys) 0.25))
      (declare (ignore diag))
      (assert-true (= (length pairs) 1) "Expected rescue to include pair"))))

(deftest test-pair-selection-excludes-short-trades
  "strategies with < min trades should be excluded"
  (let* ((s1 (swimmy.school::make-strategy :name "A" :symbol "USDJPY" :timeframe 1 :sharpe 0.5 :rank :A))
         (s2 (swimmy.school::make-strategy :name "B" :symbol "USDJPY" :timeframe 1 :sharpe 0.4 :rank :A))
         (trade-map (list (cons "A" '(((timestamp . 1) (pnl . 1.0))))
                           (cons "B" '(((timestamp . 1) (pnl . 1.0))
                                       ((timestamp . 2) (pnl . -1.0))
                                       ((timestamp . 3) (pnl . 1.0)))))))
    (multiple-value-bind (pairs diag)
        (swimmy.school::select-pair-candidates
         (list s1 s2)
         trade-map
         :per-group 2
         :min-trades 3
         :max-pairs 5)
      (assert-true (null pairs) "Expected no pairs when A is short")
      (assert-true (> (getf diag :excluded) 0) "Expected excluded count"))))
