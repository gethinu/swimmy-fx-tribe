;;; src/lisp/tests/school-v47-tests.lisp
;;; Unit tests for V47.5 Strategy Lifecycle features
;;; ============================================================================

(in-package :swimmy.tests)

;;; ==========================================
;;; V47.5 PIP AUDIT TESTS
;;; ==========================================

(deftest test-calculate-avg-pips
  "V47.5: Test expected pips calculation"
  ;; WR 60%, TP 50pips, SL 30pips
  ;; Expected = 0.6 * 50 - 0.4 * 30 = 30 - 12 = 18 pips
  (let ((strat (cl-user::make-strategy 
                :name "TestPips" 
                :sl 30 :tp 50 
                :win-rate 0.6
                :sharpe 0.5)))
    (let ((avg-pips (swimmy.school:calculate-avg-pips strat)))
      (assert-true (> avg-pips 15) "Expected pips should be ~18")
      (assert-true (< avg-pips 20) "Sanity check upper bound"))))

(deftest test-pip-value-by-symbol
  "V47.5: Test currency-specific pip values"
  (let ((eur (swimmy.school:get-pip-value "EURUSD"))
        (jpy (swimmy.school:get-pip-value "USDJPY"))
        (unknown (swimmy.school:get-pip-value "UNKNOWN")))
    (assert-equal 10.0 eur "EURUSD should be 10.0")
    (assert-true (< jpy 10.0) "JPY pairs have lower pip value")
    (assert-equal 10.0 unknown "Unknown should default to 10.0")))

(deftest test-analyze-sl-tp-ratio
  "V47.5: Test SL/TP ratio analysis"
  (let* ((strat (cl-user::make-strategy :name "RatioTest" :sl 50 :tp 100 :win-rate 0.5))
         (analysis (swimmy.school:analyze-sl-tp-ratio strat)))
    (assert-true (= 2.0 (getf analysis :ratio)) "TP/SL should be 2.0")
    (assert-true (= (getf analysis :expected-pips) 25) "Expected pips = 0.5*100 - 0.5*50 = 25")))

(deftest test-check-pip-design-health-warnings
  "V47.5: Test GPT criteria warnings"
  ;; Bad strategy: low expected pips, low Sharpe, low PF
  (let ((bad-strat (cl-user::make-strategy 
                     :name "BadStrat" 
                     :sl 100 :tp 10 :win-rate 0.3
                     :sharpe 0.1 :profit-factor 0.8)))
    (let ((warnings (swimmy.school:check-pip-design-health bad-strat)))
      (assert-true (> (length warnings) 0) "Should have warnings")
      (assert-true (>= (length warnings) 2) "Should have multiple warnings"))))

(deftest test-calculate-required-wr
  "V47.5: Test break-even WR calculation"
  ;; SL=50, TP=100 -> Required WR = 50/(50+100) = 0.333
  (let ((req-wr (swimmy.school:calculate-required-wr 50 100)))
    (assert-true (< req-wr 0.35) "Required WR should be ~0.33")
    (assert-true (> req-wr 0.30) "Sanity check")))

;;; ==========================================
;;; EXECUTION COST TESTS
;;; ==========================================

(deftest test-pip-size-by-symbol
  "Execution cost: pip size per symbol"
  (assert-true (< (abs (- (swimmy.school:get-pip-size "USDJPY") 0.01)) 0.000001)
               "USDJPY pip size should be 0.01")
  (assert-true (< (abs (- (swimmy.school:get-pip-size "EURUSD") 0.0001)) 0.000001)
               "EURUSD pip size should be 0.0001"))

(deftest test-spread-pips-from-bid-ask
  "Execution cost: spread pips calculation"
  (let ((spread (swimmy.school:spread-pips-from-bid-ask "USDJPY" 150.00 150.02)))
    (assert-true (< (abs (- spread 2.0)) 0.0001) "USDJPY spread should be ~2.0 pips")))

(deftest test-slippage-pips-from-fill
  "Execution cost: slippage pips calculation"
  (let ((buy-slip (swimmy.school:slippage-pips-from-fill "USDJPY" :buy 150.00 150.02 150.025))
        (sell-slip (swimmy.school:slippage-pips-from-fill "USDJPY" :sell 150.00 150.02 149.99)))
    (assert-true (< (abs (- buy-slip 0.5)) 0.0001) "BUY slippage should be ~0.5 pips")
    (assert-true (< (abs (- sell-slip 1.0)) 0.0001) "SELL slippage should be ~1.0 pips")))

;;; ==========================================
;;; V47.5 LIVE TRADE AUDIT TESTS
;;; ==========================================

(deftest test-trade-count-tracking
  "V47.5: Test trade count increment"
  (let ((old-count (swimmy.school:get-strategy-trade-count "TestCountStrat")))
    (swimmy.school:increment-strategy-trade-count "TestCountStrat")
    (let ((new-count (swimmy.school:get-strategy-trade-count "TestCountStrat")))
      (assert-equal (1+ old-count) new-count "Count should increment by 1"))))

(deftest test-live-audit-needed-p
  "V47.5: Test live audit trigger at 20 trades"
  ;; Set trade count to 19, then increment to 20
  (setf (gethash "AuditTestStrat" swimmy.school:*strategy-trade-counts*) 19)
  (swimmy.school:increment-strategy-trade-count "AuditTestStrat")
  (let ((strat (cl-user::make-strategy :name "AuditTestStrat")))
    (assert-true (swimmy.school:live-trade-audit-needed-p strat) 
                 "Should trigger at 20 trades")))

(deftest test-determine-live-audit-action-keep
  "V47.5: Test audit action - KEEP for good strategy"
  (let ((good-strat (cl-user::make-strategy 
                      :name "GoodStrat" 
                      :rank :S
                      :sl 30 :tp 60 :win-rate 0.6
                      :sharpe 0.5 :profit-factor 1.5
                      :pnl-history '(100 50 -30 80 60 40 -20 90))))
    (let ((action (swimmy.school:determine-live-audit-action good-strat)))
      (assert-equal :keep action "Good strategy should be kept"))))

(deftest test-determine-live-audit-action-demote
  "V47.5: Test audit action - DEMOTE for failing strategy"
  (let ((bad-strat (cl-user::make-strategy 
                     :name "BadStrat" 
                     :rank :S
                     :sl 100 :tp 20 :win-rate 0.3
                     :sharpe 0.1 :profit-factor 0.5
                     :pnl-history '(-50 -30 -40 10 -60))))
    (let ((action (swimmy.school:determine-live-audit-action bad-strat)))
      (assert-true (member action '(:demote-to-a :demote-to-breeding :demote-to-graveyard))
                   "Bad strategy should be demoted"))))

;;; ==========================================
;;; V47.5 Q-TABLE TESTS
;;; ==========================================

(deftest test-q-value-update
  "V47.5: Test Q-value update logic"
  ;; Clear Q-table entry
  (remhash (swimmy.school:q-key 60 :BUY "EURUSD" 50 100) swimmy.school:*q-table*)
  ;; Initial Q should be 0
  (let ((initial-q (swimmy.school:get-q-value 60 :BUY "EURUSD" 50 100)))
    (assert-equal 0.0 initial-q "Initial Q should be 0"))
  ;; Update with positive reward
  (swimmy.school:update-q-value 60 :BUY "EURUSD" 50 100 1.0)
  (let ((updated-q (swimmy.school:get-q-value 60 :BUY "EURUSD" 50 100)))
    (assert-true (> updated-q 0) "Q should increase after positive reward")))

(deftest test-explore-or-exploit
  "V47.5: Test epsilon-greedy exploration"
  ;; Run 100 times and check distribution (roughly 20% explore)
  (let ((explore-count 0))
    (dotimes (i 100)
      (when (swimmy.school:explore-or-exploit-p)
        (incf explore-count)))
    ;; Should be roughly 20% (allow 10-40 range for randomness)
    (assert-true (and (> explore-count 5) (< explore-count 50))
                 "Exploration should be ~20%")))

;;; ==========================================
;;; V47.5 GRAVEYARD ANALYSIS TESTS
;;; ==========================================

(deftest test-should-avoid-params-p
  "V47.5: Test SL/TP avoidance check"
  (let ((avoid-regions (list 
                         (list :sl-min 40 :sl-max 60 :tp-min 80 :tp-max 120))))
    ;; In avoid region
    (assert-true (swimmy.school:should-avoid-params-p 50 100 avoid-regions)
                 "50/100 should be in avoid region")
    ;; Outside avoid region
    (assert-false (swimmy.school:should-avoid-params-p 20 40 avoid-regions)
                  "20/40 should be outside avoid region")))

(deftest test-time-decay-applies
  "V47.5: Test time decay application"
  (let* ((now (get-universal-time))
         (old-timestamp (- now (* 60 24 60 60)))  ; 60 days ago
         (patterns (list (list :timestamp now :sl 50 :tp 100)
                         (list :timestamp old-timestamp :sl 30 :tp 60))))
    (let ((decayed (swimmy.school:apply-p3-time-decay patterns)))
      (let ((new-weight (getf (first decayed) :weight))
            (old-weight (getf (second decayed) :weight)))
        (assert-true (> new-weight old-weight) "Recent should have higher weight")))))

;;; ==========================================
;;; V47.5 RANK SYSTEM TESTS
;;; ==========================================

(deftest test-meets-rank-criteria-b
  "V47.5: Test B-RANK criteria check"
  (let ((good-strat (cl-user::make-strategy 
                      :name "GoodB" 
                      :sharpe 0.15 :profit-factor 1.1 
                      :win-rate 0.35 :max-dd 0.25))
        (bad-strat (cl-user::make-strategy 
                     :name "BadB" 
                     :sharpe 0.05 :profit-factor 0.8 
                     :win-rate 0.20 :max-dd 0.40)))
    (assert-true (swimmy.school:meets-rank-criteria-p good-strat :B) "Should meet B")
    (assert-false (swimmy.school:meets-rank-criteria-p bad-strat :B) "Should fail B")))

;;; ==========================================
;;; P8 ADD-TO-KB TESTS
;;; ==========================================

(deftest test-add-to-kb-basic
  "P8: Test basic add-to-kb functionality"
  (let ((strat (cl-user::make-strategy 
                 :name "TestKB-Basic" 
                 :sharpe 0.2
                 :profit-factor 1.1)))
    ;; Remove if exists
    (setf *strategy-knowledge-base* 
          (remove "TestKB-Basic" *strategy-knowledge-base* 
                  :key #'cl-user::strategy-name :test #'string=))
    ;; Add
    (let ((result (swimmy.school:add-to-kb strat :founder :notify nil)))
      (assert-true result "Should add strategy"))))

(deftest test-add-to-kb-duplicate
  "P8: Test duplicate rejection"
  (let ((strat (cl-user::make-strategy :name "TestKB-Dup" :sharpe 0.2)))
    ;; Ensure exists
    (setf *strategy-knowledge-base* 
          (remove "TestKB-Dup" *strategy-knowledge-base* 
                  :key #'cl-user::strategy-name :test #'string=))
    (swimmy.school:add-to-kb strat :founder :notify nil)
    ;; Try add again
    (let ((result (swimmy.school:add-to-kb strat :founder :notify nil)))
      (assert-false result "Should reject duplicate"))))

(deftest test-add-to-kb-sharpe-gate
  "P8: Test B-RANK gate (Sharpe >= 0.1)"
  ;; Low Sharpe should fail
  (let ((bad-strat (cl-user::make-strategy :name "TestKB-LowSharpe" :sharpe 0.05)))
    (setf *strategy-knowledge-base* 
          (remove "TestKB-LowSharpe" *strategy-knowledge-base* 
                  :key #'cl-user::strategy-name :test #'string=))
    (let ((result (swimmy.school:add-to-kb bad-strat :founder :notify nil :require-bt t)))
      (assert-false result "Low Sharpe should fail gate")))
  ;; Good Sharpe should pass
  (let ((good-strat (cl-user::make-strategy :name "TestKB-GoodSharpe" :sharpe 0.15)))
    (setf *strategy-knowledge-base* 
          (remove "TestKB-GoodSharpe" *strategy-knowledge-base* 
                  :key #'cl-user::strategy-name :test #'string=))
    (let ((result (swimmy.school:add-to-kb good-strat :founder :notify nil :require-bt t)))
      (assert-true result "Good Sharpe should pass gate"))))

(deftest test-startup-mode
  "P8: Test startup mode suppresses notifications"
  (let ((swimmy.school:*startup-mode* t))
    ;; In startup mode, notifications should be suppressed
    ;; (We can't easily test notification side effect, just verify flag works)
    (assert-true swimmy.school:*startup-mode* "Startup mode should be T"))
  (swimmy.school:end-startup-mode)
  (assert-false swimmy.school:*startup-mode* "Should be NIL after end"))

;;; ==========================================
;;; P9 E2E TESTS: OOS VALIDATION & A-RANK
;;; ==========================================

(deftest test-meets-a-rank-criteria-pass
  "P9: Test A-RANK criteria - passing strategy"
  (let ((good-strat (cl-user::make-strategy 
                      :name "TestARank-Good"
                      :sharpe 0.5 :profit-factor 1.5 
                      :win-rate 0.5 :max-dd 0.1)))
    (assert-true (swimmy.school:meets-a-rank-criteria good-strat)
                 "Good strategy should meet A-RANK criteria")))

(deftest test-meets-a-rank-criteria-fail-sharpe
  "P9: Test A-RANK criteria - failing on Sharpe"
  (let ((bad-strat (cl-user::make-strategy 
                     :name "TestARank-BadSharpe"
                     :sharpe 0.2 :profit-factor 1.5 
                     :win-rate 0.5 :max-dd 0.1)))
    (assert-false (swimmy.school:meets-a-rank-criteria bad-strat)
                  "Low Sharpe should fail A-RANK criteria")))

(deftest test-meets-a-rank-criteria-fail-maxdd
  "P9: Test A-RANK criteria - failing on MaxDD"
  (let ((bad-strat (cl-user::make-strategy 
                     :name "TestARank-BadDD"
                     :sharpe 0.5 :profit-factor 1.5 
                     :win-rate 0.5 :max-dd 0.25)))
    (assert-false (swimmy.school:meets-a-rank-criteria bad-strat)
                  "High MaxDD should fail A-RANK criteria")))

(deftest test-e2e-add-to-kb-lifecycle
  "P9 E2E: Test strategy lifecycle add-to-kb → KB membership"
  (let ((test-strat (cl-user::make-strategy 
                      :name "TestE2E-Lifecycle-001"
                      :sharpe 0.2 :category :scalp)))
    ;; Remove if exists
    (setf *strategy-knowledge-base* 
          (remove "TestE2E-Lifecycle-001" *strategy-knowledge-base* 
                  :key #'cl-user::strategy-name :test #'string=))
    ;; Add via add-to-kb
    (swimmy.school:add-to-kb test-strat :founder :notify nil :require-bt t)
    ;; Verify in KB
    (let ((found (find "TestE2E-Lifecycle-001" *strategy-knowledge-base* 
                       :key #'cl-user::strategy-name :test #'string=)))
      (assert-true found "Strategy should be in KB after add-to-kb"))))

;;; ==========================================
;;; P11 TESTS: PRUNING & STRATEGY DISTANCE
;;; ==========================================

(deftest test-strategy-distance-identical
  "P11: Test strategy-distance for identical strategies"
  (let ((strat1 (cl-user::make-strategy :name "Dist-A" :sl 30 :tp 60 :timeframe 60
                                        :indicators '((sma 20)) :symbol "EURUSD"))
        (strat2 (cl-user::make-strategy :name "Dist-A2" :sl 30 :tp 60 :timeframe 60
                                        :indicators '((sma 20)) :symbol "EURUSD")))
    (assert-true (< (swimmy.school:strategy-distance strat1 strat2) 0.01)
                 "Identical strategies should have distance near 0")))

(deftest test-strategy-distance-different-indicator
  "P11: Test strategy-distance with different indicators"
  (let ((strat1 (cl-user::make-strategy :name "Dist-B" :sl 30 :tp 60 :timeframe 60
                                        :indicators '((sma 20)) :symbol "EURUSD"))
        (strat2 (cl-user::make-strategy :name "Dist-B2" :sl 30 :tp 60 :timeframe 60
                                        :indicators '((rsi 14)) :symbol "EURUSD")))
    ;; Different indicator type = 0.25 distance contribution
    (assert-true (>= (swimmy.school:strategy-distance strat1 strat2) 0.25)
                 "Different indicator should increase distance")))

(deftest test-strategy-distance-different-symbol
  "P11: Test strategy-distance with different symbols"
  (let ((strat1 (cl-user::make-strategy :name "Dist-C" :sl 30 :tp 60 :timeframe 60
                                        :indicators '((sma 20)) :symbol "EURUSD"))
        (strat2 (cl-user::make-strategy :name "Dist-C2" :sl 30 :tp 60 :timeframe 60
                                        :indicators '((sma 20)) :symbol "GBPUSD")))
    ;; Different symbol = 0.15 distance contribution
    (assert-true (>= (swimmy.school:strategy-distance strat1 strat2) 0.15)
                 "Different symbol should increase distance")))

(deftest test-prune-sharpe-threshold
  "P11: Test prune sharpe threshold is 0.08"
  (assert-true (= swimmy.school:*prune-sharpe-threshold* 0.08)
               "Prune threshold should be 0.08 (Graham recommendation)"))

;;; ==========================================
;;; REGISTER TESTS
;;; ==========================================

(format t "[V47.5+P8+P9+P11 TESTS] 27 tests loaded~%")
