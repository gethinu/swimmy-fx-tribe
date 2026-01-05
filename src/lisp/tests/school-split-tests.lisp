;;; src/lisp/tests/school-split-tests.lisp
;;; Unit tests for extracted school modules (Uncle Bob Split)

(in-package :swimmy.tests)

;;; ==========================================
;;; SCHOOL-LEARNING TESTS
;;; ==========================================

(deftest test-time-decay-weight
  "Test that older failures have less weight"
  (let ((now (get-universal-time)))
    (let ((recent (cl-user::time-decay-weight (- now 60)))      ; 1 min old
          (old (cl-user::time-decay-weight (- now 7200))))      ; 2 hours old
      (assert-true (> recent old) "Recent events should have higher weight")
      (assert-true (<= recent 1.0) "Weight should not exceed 1.0")
      (assert-true (>= old 0.0) "Weight should not be negative"))))

(deftest test-pattern-similarity
  "Test fuzzy pattern matching similarity"
  (let* ((p1 (list :regime :trending :volatility :high :session :london
                   :sma-position :above :rsi-zone :neutral :momentum :flat
                   :rsi-value 50 :hour-of-day 10 :day-of-week 1))
         (p2 (list :regime :trending :volatility :high :session :london
                   :sma-position :above :rsi-zone :neutral :momentum :flat
                   :rsi-value 50 :hour-of-day 10 :day-of-week 1))  ; Exact match
         (p3 (list :regime :ranging :volatility :low :session :tokyo
                   :sma-position :below :rsi-zone :oversold :momentum :decelerating
                   :rsi-value 20 :hour-of-day 14 :day-of-week 5))) ; No match
    
    (assert-true (= 1.0 (cl-user::pattern-similarity p1 p2)) "Identical patterns should be 1.0")
    (assert-true (< (cl-user::pattern-similarity p1 p3) 0.5) "Different patterns should be low")))

;;; ==========================================
;;; SCHOOL-VOLATILITY TESTS
;;; ==========================================

(deftest test-atr-calculation-logic
  "Test ATR calculation with known values"
  ;; Create 3 candles:
  ;; 1. High 100, Low 90, Close 95 (Range 10)
  ;; 2. High 105, Low 95, Close 100 (Range 10, Gap 5) -> TR = max(10, |105-95|=10, |95-95|=0) = 10
  ;; 3. High 110, Low 100, Close 105 (Range 10, Gap 5) -> TR = 10
  (let* ((c1 (cl-user::make-candle :high 100 :low 90 :close 95 :open 90))
         (c2 (cl-user::make-candle :high 105 :low 95 :close 100 :open 95))
         (c3 (cl-user::make-candle :high 110 :low 100 :close 105 :open 100))
         (history (list c3 c2 c1)) ; Reverse order (newest first)
         (atr (cl-user::calculate-atr history 2)))
    
    (assert-true (= 10.0 atr) "ATR should be 10.0 for constant range candles")))


(deftest test-volatility-shifts
  "Test shift detection thresholds"
  (let ((current 2.5)
        (baseline 1.0))
    ;; Ratio 2.5 > 2.0 (threshold) -> Elevated
    (assert-true (> (/ current baseline) cl-user::*volatility-shift-threshold*) "Should detect shift")))

;;; ==========================================
;;; WALK-FORWARD VALIDATION TESTS
;;; ==========================================

(defparameter *mock-wfv-calls* nil)

;; Mock dependencies
(defun mock-request-backtest (strat &key candles suffix)
  (declare (ignore candles))
  (push (list :strat strat :suffix suffix) *mock-wfv-calls*))

(defun mock-notify-discord (msg &key color)
  (declare (ignore color))
  (push msg *mock-wfv-calls*))

(deftest test-wfv-logic-robust-strategy
  "Test WFV logic with a robust strategy (Good IS, Good OOS)"
  (setf cl-user::*candle-history* (loop for i from 1 to 200 collect (cl-user::make-candle :close 100)))
  (let ((strat (cl-user::make-strategy :name "TestStrat" :indicators nil :entry nil :exit nil :sl 0.1 :tp 0.1 :volume 0.01)))
    
    ;; Swap functions with mocks
    (let ((orig-req (symbol-function 'cl-user::request-backtest))
          (orig-not (symbol-function 'cl-user::notify-discord-alert)))
      (setf (symbol-function 'cl-user::request-backtest) #'mock-request-backtest)
      (setf (symbol-function 'cl-user::notify-discord-alert) #'mock-notify-discord)
      
      (unwind-protect
           (progn
             ;; 1. Start WFV
             (setf *mock-wfv-calls* nil)
             (cl-user::start-walk-forward-validation strat)
             
             (assert-true (gethash "TestStrat" cl-user::*wfv-pending-strategies*) "Should generally pending entry")
             (assert-equal 2 (length *mock-wfv-calls*) "Should request 2 backtests")
             
             ;; 2. Receive IS Result (Sharpe 2.0)
             (cl-user::process-wfv-result "TestStrat_IS" (list :sharpe 2.0 :trades 100))
             
             ;; 3. Receive OOS Result (Sharpe 1.8) -> Degradation 10% (OK)
             (setf *mock-wfv-calls* nil) ;; Clear for notification check
             (cl-user::process-wfv-result "TestStrat_OOS" (list :sharpe 1.8 :trades 20))
             
             ;; 4. Check Verdict
             (assert-true (null (gethash "TestStrat" cl-user::*wfv-pending-strategies*)) "Should clear pending")
             (assert-true (search "Valid Strategy" (first *mock-wfv-calls*)) "Should notify validation success"))
        
        ;; Restore
        (setf (symbol-function 'cl-user::request-backtest) orig-req)
        (setf (symbol-function 'cl-user::notify-discord-alert) orig-not)))))

(deftest test-wfv-logic-overfit-strategy
  "Test WFV logic with an overfit strategy (Good IS, Bad OOS)"
  (setf cl-user::*candle-history* (loop for i from 1 to 200 collect (cl-user::make-candle :close 100)))
  (let ((strat (cl-user::make-strategy :name "OverfitStrat" :indicators nil :entry nil :exit nil :sl 0.1 :tp 0.1 :volume 0.01)))
    
    (let ((orig-req (symbol-function 'cl-user::request-backtest))
          (orig-not (symbol-function 'cl-user::notify-discord-alert)))
      (setf (symbol-function 'cl-user::request-backtest) #'mock-request-backtest)
      (setf (symbol-function 'cl-user::notify-discord-alert) #'mock-notify-discord)
      
      (unwind-protect
           (progn
             (cl-user::start-walk-forward-validation strat)
             
             ;; IS: Great (Sharpe 3.0)
             (cl-user::process-wfv-result "OverfitStrat_IS" (list :sharpe 3.0))
             
             ;; OOS: Terrible (Sharpe 0.2) -> Degradation > 90%
             (setf *mock-wfv-calls* nil)
             (cl-user::process-wfv-result "OverfitStrat_OOS" (list :sharpe 0.2))
             
             ;; Should NOT notify success
             (assert-true (null *mock-wfv-calls*) "Should NOT notify success for overfit"))
        
        (setf (symbol-function 'cl-user::request-backtest) orig-req)
        (setf (symbol-function 'cl-user::notify-discord-alert) orig-not)))))

;;; ==========================================
;;; SCHOOL-RESEARCH TESTS
;;; ==========================================

(deftest test-prediction-structure
  "Test trade prediction object creation"
  (let ((pred (cl-user::make-trade-prediction 
               :symbol "USDJPY" 
               :direction :buy 
               :confidence 0.8)))
    (assert-equal "USDJPY" (cl-user::trade-prediction-symbol pred))
    (assert-equal 0.8 (cl-user::trade-prediction-confidence pred))))

;;; ==========================================
;;; PERSISTENCE TESTS (Andrew Ng)
;;; ==========================================

(deftest test-learning-persistence
  "Test saving and loading of failure logs"
  (let ((tmp-file (merge-pathnames "test_state.sexp" (user-homedir-pathname)))
        (orig-path cl-user::*state-file-path*)
        (test-log '((:pattern "TEST" :confidence 0.9))))
    
    (setf cl-user::*state-file-path* tmp-file)
    (setf cl-user::*failure-log* test-log)
    
    (unwind-protect
         (progn
           ;; Save
           (cl-user::save-state)
           (assert-true (probe-file tmp-file) "State file should exist")
           
           ;; Clear
           (setf cl-user::*failure-log* nil)
           
           ;; Load
           (cl-user::load-state)
           (let ((restored (first cl-user::*failure-log*)))
             (assert-true restored "Should have restored log entry")
             (assert-equal "TEST" (getf restored :pattern) "Should restore pattern")
             (assert-equal 0.9 (getf restored :confidence) "Should restore confidence")))
      
      ;; Cleanup
      (setf cl-user::*state-file-path* orig-path)
      (when (probe-file tmp-file) (delete-file tmp-file)))))

;;; ==========================================
;;; ADVISOR REPORTS TESTS
;;; ==========================================

(deftest test-advisor-reports
  "Test interaction with advisor report modules"
  (format t "~%[TEST] Running Advisor Reports Test...~%")
  
  ;; Mock GLOBALS
  (setf cl-user::*max-total-exposure* 5.0)
  (setf cl-user::*danger-level* 3)
  (setf cl-user::*consecutive-losses* 1)
  (setf cl-user::*current-equity* 100000.0)
  (setf cl-user::*locked-treasury* 20000.0)
  (setf cl-user::*daily-pnl* 500.0)
  (setf cl-user::*evolved-strategies* nil) 
  (setf cl-user::*daily-loss-limit* -5000)

  ;; Test Taleb
  (let ((taleb (cl-user::generate-taleb-report)))
    (assert-true (search "Taleb" taleb) "Taleb report should contain title"))

  ;; Test Graham
  (let ((graham (cl-user::generate-graham-report)))
    (assert-true (search "Graham" graham) "Graham report should contain title"))

  ;; Test Naval
  (let ((naval (cl-user::generate-naval-report)))
    (assert-true (search "Naval" naval) "Naval report should contain title"))
    
  ;; Test Orchestrator
  (let ((full (cl-user::generate-advisor-reports)))
    (assert-true (> (length full) 50) "Full report should not be empty")))
