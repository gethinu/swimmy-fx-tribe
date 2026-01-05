;; stress-test.lisp - Black Swan Resistance Testing
;; Taleb Advisor Homework #2: CHF Shock Level Stress Tests

(in-package :swimmy.tests)
;; V1.0: Initial implementation

;;; ==========================================
;;; STRESS TEST SCENARIOS
;;; ==========================================
;;; Inspired by:
;;; - 2015 CHF Shock: 30% move in minutes, SL slippage
;;; - 2010 Flash Crash: 5% move in seconds
;;; - Weekend Gap: Market opens far from Friday close

(defparameter *stress-test-results* nil "Results from last stress test run")

;;; ==========================================
;;; SCENARIO DEFINITIONS
;;; ==========================================

(defstruct stress-scenario
  name
  description
  move-percent      ;; Price move as percentage
  sl-slippage       ;; SL slippage factor (1.0 = no slippage, 2.0 = double)
  duration-seconds  ;; How fast the move happens
  affects-all-pairs ;; T if all pairs affected (correlation breakdown)
  )

(defparameter *stress-scenarios*
  (list
   ;; CHF Shock: 30% adverse move, massive SL slippage, instant
   (make-stress-scenario
    :name "CHF-SHOCK"
    :description "2015 CHF depegging: 30% sudden move, no liquidity"
    :move-percent 30.0
    :sl-slippage 5.0  ;; SL executed at 5x normal distance
    :duration-seconds 60
    :affects-all-pairs nil)
   
   ;; Flash Crash: 5% move, moderate slippage
   (make-stress-scenario
    :name "FLASH-CRASH"
    :description "Flash crash: 5% move in 1 minute"
    :move-percent 5.0
    :sl-slippage 2.0
    :duration-seconds 60
    :affects-all-pairs t)
   
   ;; Weekend Gap: 3% gap, no slippage (market opens there)
   (make-stress-scenario
    :name "WEEKEND-GAP"
    :description "Weekend gap: Market opens 3% from Friday close"
    :move-percent 3.0
    :sl-slippage 1.0  ;; Gap, so SL executes at open price
    :duration-seconds 0
    :affects-all-pairs nil)
   
   ;; Correlation Breakdown: All USD pairs move same direction
   (make-stress-scenario
    :name "CORRELATION-BREAKDOWN"
    :description "All USD pairs move 2% same direction (correlation = 1.0)"
    :move-percent 2.0
    :sl-slippage 1.5
    :duration-seconds 300
    :affects-all-pairs t)
   
   ;; Extreme Volatility: 10% intraday range
   (make-stress-scenario
    :name "EXTREME-VOLATILITY"
    :description "10% intraday range, multiple whipsaws"
    :move-percent 10.0
    :sl-slippage 1.5
    :duration-seconds 3600
    :affects-all-pairs nil)
   
   ;; TALEB ADDITIONAL: Unknown Unknown - the unimaginable
   (make-stress-scenario
    :name "UNKNOWN-UNKNOWN"
    :description "Taleb: 50% move + ALL pairs same direction + 10x slippage"
    :move-percent 50.0
    :sl-slippage 10.0  ;; Complete liquidity vacuum
    :duration-seconds 10
    :affects-all-pairs t)
   
   ;; TALEB ADDITIONAL: Correlation Velocity - rapid change detection
   (make-stress-scenario
    :name "CORRELATION-VELOCITY"
    :description "Taleb: Correlation jumps 0.3→1.0 in 5 minutes"
    :move-percent 8.0
    :sl-slippage 3.0
    :duration-seconds 300
    :affects-all-pairs t)))


;;; ==========================================
;;; SIMULATION ENGINE
;;; ==========================================

(defstruct simulated-position
  symbol
  direction      ;; :long or :short
  entry-price
  lot
  sl-distance    ;; In price units
  tp-distance)

(defun calculate-position-loss (position scenario adverse-direction-p)
  "Calculate loss for a single position under stress scenario"
  (let* ((entry (simulated-position-entry-price position))
         (lot (simulated-position-lot position))
         (sl-dist (simulated-position-sl-distance position))
         (move-pct (stress-scenario-move-percent scenario))
         (slippage (stress-scenario-sl-slippage scenario))
         (direction (simulated-position-direction position))
         ;; Price move in adverse direction
         (price-move (* entry (/ move-pct 100)))
         ;; Actual SL execution price with slippage
         (sl-execution-dist (* sl-dist slippage))
         ;; Loss is minimum of: full move or SL hit with slippage
         (loss-per-lot (if adverse-direction-p
                           (min price-move sl-execution-dist)
                           0)))
    ;; Convert to yen (approximate for JPY pairs)
    (let ((pip-value (if (search "JPY" (simulated-position-symbol position))
                         100  ;; 1 pip = 100 yen per lot
                         1000))) ;; 1 pip = ~1000 yen per lot for EUR/GBP
      (* loss-per-lot lot pip-value 100)))) ;; * 100 for pip conversion

(defun simulate-scenario (scenario positions)
  "Simulate a stress scenario on a portfolio of positions"
  (let ((total-loss 0)
        (affected-positions nil))
    (dolist (pos positions)
      ;; Determine if this position is adversely affected
      ;; For simplicity: long positions lose on down moves, shorts on up
      ;; Assume 50% chance of adverse move for each position
      (let* ((adverse-p t)  ;; Worst case: always adverse
             (loss (calculate-position-loss pos scenario adverse-p)))
        (incf total-loss loss)
        (push (list :symbol (simulated-position-symbol pos)
                    :direction (simulated-position-direction pos)
                    :loss loss)
              affected-positions)))
    
    (list :scenario-name (stress-scenario-name scenario)
          :description (stress-scenario-description scenario)
          :total-loss total-loss
          :max-position-loss (if affected-positions
                                 (apply #'max (mapcar (lambda (p) (getf p :loss)) 
                                                      affected-positions))
                                 0)
          :positions-affected (length affected-positions)
          :details affected-positions)))

;;; ==========================================
;;; PORTFOLIO SNAPSHOT
;;; ==========================================

(defun get-current-portfolio ()
  "Get current open positions as simulated portfolio.
   Uses *warrior-allocation* from school.lisp if available."
  (let ((positions nil))
    (if (and (boundp '*warrior-allocation*) 
             (hash-table-p *warrior-allocation*))
        ;; Real positions from trading system
        (maphash
         (lambda (key warrior)
           (declare (ignore key))
           (push (make-simulated-position
                  :symbol (getf warrior :symbol)
                  :direction (getf warrior :direction)
                  :entry-price (or (getf warrior :entry) 150.0)
                  :lot (or (getf warrior :lot) 0.01)
                  :sl-distance (or (getf warrior :sl-dist) 0.5)
                  :tp-distance (or (getf warrior :tp-dist) 1.0))
                 positions))
         *warrior-allocation*)
        ;; Create mock portfolio for testing
        (setf positions
              (list
               (make-simulated-position :symbol "USDJPY" :direction :long
                                        :entry-price 150.0 :lot 0.05 
                                        :sl-distance 0.5 :tp-distance 1.0)
               (make-simulated-position :symbol "EURUSD" :direction :long
                                        :entry-price 1.08 :lot 0.03
                                        :sl-distance 0.003 :tp-distance 0.008)
               (make-simulated-position :symbol "GBPUSD" :direction :short
                                        :entry-price 1.27 :lot 0.02
                                        :sl-distance 0.004 :tp-distance 0.010))))
    positions))

;;; ==========================================
;;; STRESS TEST RUNNER
;;; ==========================================

(defun run-stress-test (scenario &optional portfolio)
  "Run a single stress test scenario"
  (let ((positions (or portfolio (get-current-portfolio))))
    (format t "~%[T] 🧪 STRESS TEST: ~a~%" (stress-scenario-name scenario))
    (format t "[T]    ~a~%" (stress-scenario-description scenario))
    (format t "[T]    Move: ~,1f% | SL Slippage: ~,1fx~%" 
            (stress-scenario-move-percent scenario)
            (stress-scenario-sl-slippage scenario))
    
    (let ((result (simulate-scenario scenario positions)))
      (format t "[T]    RESULT: Max Loss ¥~:d (~d positions)~%"
              (round (getf result :total-loss))
              (getf result :positions-affected))
      result)))

(defun run-all-stress-tests (&optional portfolio)
  "Run all stress test scenarios and generate summary"
  (let ((positions (or portfolio (get-current-portfolio)))
        (results nil)
        (max-loss 0)
        (worst-scenario nil))
    
    (format t "~%")
    (format t "[T] ════════════════════════════════════════════════~%")
    (format t "[T] 🦢 BLACK SWAN STRESS TEST SUITE (Taleb)~%")
    (format t "[T] ════════════════════════════════════════════════~%")
    (format t "[T] Portfolio: ~d positions~%" (length positions))
    (format t "~%")
    
    (dolist (scenario *stress-scenarios*)
      (let ((result (run-stress-test scenario positions)))
        (push result results)
        (when (> (getf result :total-loss) max-loss)
          (setf max-loss (getf result :total-loss))
          (setf worst-scenario (getf result :scenario-name)))))
    
    (setf *stress-test-results* (nreverse results))
    
    ;; Summary
    (format t "~%[T] ════════════════════════════════════════════════~%")
    (format t "[T] 📊 STRESS TEST SUMMARY~%")
    (format t "[T] ════════════════════════════════════════════════~%")
    (format t "[T] Worst Case: ~a~%" worst-scenario)
    (format t "[T] Maximum Potential Loss: ¥~:d~%" (round max-loss))
    (format t "[T]~%")
    (format t "[T] By Scenario:~%")
    (dolist (r *stress-test-results*)
      (format t "[T]   ~a: ¥~:d~%" 
              (getf r :scenario-name)
              (round (getf r :total-loss))))
    (format t "[T] ════════════════════════════════════════════════~%~%")
    
    ;; Return summary
    (list :worst-scenario worst-scenario
          :max-loss max-loss
          :all-results *stress-test-results*)))

;;; ==========================================
;;; REPORT GENERATION
;;; ==========================================

(defun generate-stress-report ()
  "Generate detailed stress test report for logging/Discord"
  (let ((summary (run-all-stress-tests)))
    ;; Create report string
    (let ((report (format nil "🦢 **BLACK SWAN STRESS TEST**~%~%Worst Case: ~a~%Max Potential Loss: ¥~:d~%~%**All Scenarios:**~%~{~a~%~}"
                          (getf summary :worst-scenario)
                          (round (getf summary :max-loss))
                          (mapcar (lambda (r) 
                                    (format nil "• ~a: ¥~:d"
                                            (getf r :scenario-name)
                                            (round (getf r :total-loss))))
                                  (getf summary :all-results)))))
      ;; Send to Discord if available
      (when (fboundp 'notify-discord-daily)
        (notify-discord-daily report :color 15158332)) ;; Red for warnings
      report)))

;;; ==========================================
;;; RISK METRICS
;;; ==========================================

(defun calculate-var-99 ()
  "Calculate 99% Value at Risk from stress test results"
  (when *stress-test-results*
    (let* ((losses (mapcar (lambda (r) (getf r :total-loss)) *stress-test-results*))
           (sorted (sort (copy-list losses) #'>)))
      ;; 99th percentile = worst case for small sample
      (first sorted))))

(defun stress-test-passed-p (max-acceptable-loss)
  "Check if portfolio passes stress test with acceptable max loss"
  (let ((summary (run-all-stress-tests)))
    (< (getf summary :max-loss) max-acceptable-loss)))

;;; ==========================================
;;; INITIALIZATION  
;;; ==========================================

(format t "[L] 🦢 stress-test.lisp loaded - Black Swan testing ready~%")
(format t "[L] 📋 ~d stress scenarios defined~%" (length *stress-scenarios*))
(format t "[L] 🎯 Run (run-all-stress-tests) to test current portfolio~%")
