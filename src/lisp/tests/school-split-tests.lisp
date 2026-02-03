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

;; [V8.2] Expert Panel (Uncle Bob): Test behavior, not just existence
(deftest test-calculate-pattern-similarity-behavior
  "Test that calculate-pattern-similarity returns 0 for nil/random and high for matches"
  ;; Create a mock memory-episode for testing
  (let* ((episode (swimmy.school::make-memory-episode
                   :timestamp (get-universal-time)
                   :symbol "USDJPY"
                   :regime :trending
                   :volatility :high
                   :rsi-value 50
                   :momentum-direction :flat
                   :sma-position :above
                   :hour-of-day 10
                   :day-of-week 1))
         ;; Matching pattern
         (matching-pattern (list :regime :trending :volatility :high
                                 :sma-position :above :momentum :flat
                                 :rsi-value 50 :hour 10))
         ;; Non-matching pattern
         (different-pattern (list :regime :ranging :volatility :low
                                  :sma-position :below :momentum :decelerating
                                  :rsi-value 80 :hour 23)))
    
    (let ((high-sim (swimmy.school::calculate-pattern-similarity matching-pattern episode))
          (low-sim (swimmy.school::calculate-pattern-similarity different-pattern episode)))
      (assert-true (> high-sim 0.5) "Matching patterns should have high similarity")
      (assert-true (< low-sim 0.5) "Different patterns should have low similarity")
      (assert-true (> high-sim low-sim) "Matching should be higher than non-matching"))))

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

(deftest test-wfv-scheduling-respects-interval-and-pending
  (let ((calls 0)
        (orig-start (symbol-function 'swimmy.school::start-walk-forward-validation))
        (orig-top (symbol-function 'swimmy.school::get-top-strategies)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::start-walk-forward-validation)
                (lambda (strat) (declare (ignore strat)) (incf calls)))
          (setf (symbol-function 'swimmy.school::get-top-strategies)
                (lambda (n) (declare (ignore n)) (list (swimmy.school:make-strategy :name "WFV-1"))))
          (let ((swimmy.school::*wfv-pending-strategies* (make-hash-table :test 'equal))
                (swimmy.school::*last-wfv-qualify-time* (get-universal-time))
                (swimmy.core::*wfv-enabled* t)
                (swimmy.core::*wfv-interval-sec* 3600)
                (swimmy.core::*wfv-max-pending* 0)
                (swimmy.core::*wfv-max-per-run* 1))
            (setf (gethash "PENDING" swimmy.school::*wfv-pending-strategies*)
                  (list :started-at (- (get-universal-time) 10)))
            (swimmy.school::maybe-run-wfv-qualification)
            (assert-equal 0 calls)))
      (setf (symbol-function 'swimmy.school::start-walk-forward-validation) orig-start)
      (setf (symbol-function 'swimmy.school::get-top-strategies) orig-top))))

(deftest test-wfv-pending-stats-oldest-age
  (let* ((now 1000)
         (swimmy.school::*wfv-pending-strategies* (make-hash-table :test 'equal)))
    (setf (gethash "A" swimmy.school::*wfv-pending-strategies*) (list :started-at 900))
    (setf (gethash "B" swimmy.school::*wfv-pending-strategies*) (list :started-at 950))
    (multiple-value-bind (count oldest-age) (swimmy.school::wfv-pending-stats :now now)
      (assert-equal 2 count)
      (assert-equal 100 oldest-age))))

;;; ==========================================
;;; OOS VALIDATION TESTS
;;; ==========================================

(deftest test-oos-validation-dispatches-when-unset
  "OOS validation should dispatch a backtest when no OOS Sharpe is set."
  (let* ((data-path (swimmy.core::swimmy-path "data/historical/USDJPY_M1.csv"))
         (created-file nil)
         (tmp-db (format nil "/tmp/swimmy-test-~a.db" (get-universal-time)))
         (swimmy.school::*oos-pending* (make-hash-table :test 'equal)))
    ;; Ensure data fixture exists with one valid row
    (unless (probe-file data-path)
      (ensure-directories-exist data-path)
      (with-open-file (s data-path :direction :output :if-does-not-exist :create :if-exists :supersede)
        (write-line "timestamp,open,high,low,close,volume" s)
        (write-line "1700000000,145.1,145.2,145.0,145.15,1000" s))
      (setf created-file t))

    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil))
      (unwind-protect
           (progn
             ;; Ensure DB schema exists for oos_queue
             (swimmy.school::init-db)
             (let ((dispatch-count 0)
                   (captured-req-ids nil)
                   (orig-request (symbol-function 'swimmy.school::request-backtest)))
               (unwind-protect
                    (progn
                      ;; Stub request-backtest to count invocations
                      (setf (symbol-function 'swimmy.school::request-backtest)
                            (lambda (strat &key suffix request-id &allow-other-keys)
                              (declare (ignore strat suffix))
                              (push request-id captured-req-ids)
                              (incf dispatch-count)
                              t))

                      (let ((strat (cl-user::make-strategy :name "UnitTest-OOS"
                                                           :symbol "USDJPY"
                                                           :oos-sharpe nil)))
                        (multiple-value-bind (passed sharpe msg)
                            (swimmy.school::run-oos-validation strat)
                          (assert-true (null passed) "OOS should be pending asynchronously")
                          (assert-equal 0.0 sharpe)
                          (assert-equal "OOS pending (async)" msg)
                          (assert-equal 1 dispatch-count)
                          (assert-true (car captured-req-ids) "request-id should be generated"))))
                 (setf (symbol-function 'swimmy.school::request-backtest) orig-request))))
        (swimmy.core:close-db-connection)
        (when created-file (delete-file data-path))
        (when (probe-file tmp-db) (delete-file tmp-db))))))

(deftest test-oos-data-quality-gate
  "Invalid CSV should be rejected and not dispatch OOS."
  (let* ((data-path (swimmy.core::swimmy-path "data/historical/BAD_M1.csv"))
         (created-file nil))
    (ensure-directories-exist data-path)
    (with-open-file (s data-path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-line "foo,bar,baz" s))
    (setf created-file t)
    (let ((dispatch-count 0)
          (orig-request (symbol-function 'swimmy.school::request-backtest)))
      (unwind-protect
           (progn
             (setf (symbol-function 'swimmy.school::request-backtest)
                   (lambda (&rest _args) (declare (ignore _args)) (incf dispatch-count)))
             (let ((strat (cl-user::make-strategy :name "BadData" :symbol "BAD" :oos-sharpe nil)))
               (multiple-value-bind (passed sharpe msg)
                   (swimmy.school::run-oos-validation strat)
                 (assert-false passed)
                 (assert-equal 0.0 sharpe)
                 (assert-true (search "Invalid data file" msg))
                 (assert-equal 0 dispatch-count))))
        (setf (symbol-function 'swimmy.school::request-backtest) orig-request)
        (when created-file (delete-file data-path)))))) 

(deftest test-oos-queue-persists-and-clears
  "Queue entry should persist and clear when result arrives."
  (let* ((tmp-db (format nil "data/memory/test-oos-~a.db" (get-universal-time))))
    (unwind-protect
         (let ((swimmy.core::*db-path-default* tmp-db))
           (swimmy.core:close-db-connection)
           (swimmy.school::init-db)
           (let ((dispatch-count 0)
                 (last-req-id nil)
                 (orig-request (symbol-function 'swimmy.school::request-backtest)))
             (unwind-protect
                  (progn
                    (setf (symbol-function 'swimmy.school::request-backtest)
                          (lambda (strat &key request-id &allow-other-keys)
                            (declare (ignore strat))
                            (setf last-req-id request-id)
                            (incf dispatch-count)
                            t))
                    (let ((strat (cl-user::make-strategy :name "QueueTest" :symbol "USDJPY" :oos-sharpe nil)))
                      (swimmy.school::run-oos-validation strat)
                      (assert-equal 1 dispatch-count)
                      (multiple-value-bind (rid rat status) (swimmy.school::lookup-oos-request "QueueTest")
                        (declare (ignore rat))
                        (assert-equal last-req-id rid)
                        (assert-true (stringp status)))
                      ;; Simulate restart by closing connection
                      (swimmy.core:close-db-connection)
                      (multiple-value-bind (rid2 rat2 status2) (swimmy.school::lookup-oos-request "QueueTest")
                        (declare (ignore rat2))
                        (assert-equal last-req-id rid2)
                        (assert-true (stringp status2)))
                      ;; Receive result -> clears queue
                      (swimmy.school::handle-oos-backtest-result "QueueTest" (list :sharpe 0.8 :request-id last-req-id))
                      (multiple-value-bind (rid3 _time3 _status3) (swimmy.school::lookup-oos-request "QueueTest")
                        (assert-true (null rid3)))) )
             (setf (symbol-function 'swimmy.school::request-backtest) orig-request))))
      (swimmy.core:close-db-connection)
      (when (probe-file tmp-db) (delete-file tmp-db)))))

(deftest test-oos-status-updated-on-dispatch
  "OOS dispatch should update oos_status.txt via writer."
  (let* ((data-path (swimmy.core::swimmy-path "data/historical/USDJPY_M1.csv"))
         (created-file nil)
         (tmp-db (format nil "data/memory/test-oos-status-~a.db" (get-universal-time))))
    (unless (probe-file data-path)
      (ensure-directories-exist data-path)
      (with-open-file (s data-path :direction :output :if-does-not-exist :create :if-exists :supersede)
        (write-line "timestamp,open,high,low,close,volume" s)
        (write-line "1700000000,145.1,145.2,145.0,145.15,1000" s))
      (setf created-file t))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil))
      (unwind-protect
           (progn
             (swimmy.school::init-db)
             (let* ((write-count 0)
                    (had-write (fboundp 'swimmy.school::write-oos-status-file))
                    (orig-write (and had-write (symbol-function 'swimmy.school::write-oos-status-file)))
                    (orig-request (symbol-function 'swimmy.school::request-backtest)))
               (unwind-protect
                    (progn
                      (setf (symbol-function 'swimmy.school::write-oos-status-file)
                            (lambda (&rest _args) (declare (ignore _args)) (incf write-count) t))
                      (setf (symbol-function 'swimmy.school::request-backtest)
                            (lambda (&rest _args) (declare (ignore _args)) t))
                      (let ((strat (cl-user::make-strategy :name "UnitTest-OOS-Status"
                                                           :symbol "USDJPY"
                                                           :oos-sharpe nil)))
                        (swimmy.school::run-oos-validation strat)
                        (assert-true (> write-count 0) "OOS status should update on dispatch")))
                 (setf (symbol-function 'swimmy.school::request-backtest) orig-request)
                (if had-write
                    (setf (symbol-function 'swimmy.school::write-oos-status-file) orig-write)
                    (fmakunbound 'swimmy.school::write-oos-status-file)))))
        (swimmy.core:close-db-connection)
        (when created-file (delete-file data-path))
        (when (probe-file tmp-db) (delete-file tmp-db))))))

(deftest test-evolution-report-includes-oos-status
  "Evolution report should include OOS status line."
  (let* ((tmp-db (format nil "/tmp/swimmy-report-oos-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
           (progn
             (swimmy.school::init-db)
             (let ((orig-refresh (symbol-function 'swimmy.school::refresh-strategy-metrics-from-db)))
               (unwind-protect
                    (progn
                      (setf (symbol-function 'swimmy.school::refresh-strategy-metrics-from-db)
                            (lambda (&rest _args) (declare (ignore _args)) nil))
                      (let ((report (swimmy.school::generate-evolution-report)))
                        (assert-true (search "OOS sent:" report) "Report should contain OOS status line")))
                 (setf (symbol-function 'swimmy.school::refresh-strategy-metrics-from-db) orig-refresh))))
        (swimmy.core:close-db-connection)
        (when (probe-file tmp-db) (delete-file tmp-db))))))

(deftest test-evolution-report-includes-phase2-end-time
  "Evolution report should include Phase2 end_time line."
  (let* ((tmp-db (format nil "/tmp/swimmy-report-phase2-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
           (progn
             (swimmy.school::init-db)
             (let ((orig-refresh (symbol-function 'swimmy.school::refresh-strategy-metrics-from-db)))
               (unwind-protect
                    (progn
                      (setf (symbol-function 'swimmy.school::refresh-strategy-metrics-from-db)
                            (lambda (&rest _args) (declare (ignore _args)) nil))
                      (let ((report (swimmy.school::generate-evolution-report)))
                        (assert-true (search "Phase2 EndTime:" report) "Report should contain Phase2 EndTime line")))
                 (setf (symbol-function 'swimmy.school::refresh-strategy-metrics-from-db) orig-refresh))))
        (swimmy.core:close-db-connection)
        (when (probe-file tmp-db) (delete-file tmp-db))))))

(deftest test-backtest-dead-letter-replay
  "Malformed BACKTEST_RESULT should go to DLQ and be replayable."
  (let ((tmp-db (format nil "data/memory/test-dlq-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil))
      (unwind-protect
           (progn
             (swimmy.school::init-db)
             (let* ((strat (cl-user::make-strategy :name "DLQ-TEST" :symbol "USDJPY" :sharpe 0.1))
                    (*strategy-knowledge-base* (list strat))
                    (*evolved-strategies* nil)
                    (bad "{\"type\":\"BACKTEST_RESULT\",\"result\":{\"sharpe\":1.0}}")
                    (good "{\"type\":\"BACKTEST_RESULT\",\"result\":{\"strategy_name\":\"DLQ-TEST\",\"sharpe\":1.2,\"trades\":5}}"))
               (setf swimmy.main::*backtest-dead-letter* nil)
               (swimmy.main::internal-process-msg bad)
               (assert-equal 1 (length swimmy.main::*backtest-dead-letter*))
               ;; Replace payload with a valid one and replay
               (let ((entry (first swimmy.main::*backtest-dead-letter*)))
                 (setf (getf entry :payload) good)
                 (setf swimmy.main::*backtest-dead-letter* (list entry)))
               (let ((processed (swimmy.main::replay-dead-letter)))
                 (assert-equal 1 processed)
                 (assert-true (null swimmy.main::*backtest-dead-letter*))
                 (assert-true (> (cl-user::strategy-sharpe strat) 1.0) "Sharpe should be updated from replay"))))
        (swimmy.core:close-db-connection)
        (when (probe-file tmp-db) (delete-file tmp-db)))))) 

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

;;; V17: New Prediction System Tests
(deftest test-prediction-weights-configurable
  "V17: Test that prediction weights are configurable (Issue 1)"
  ;; Check that weight variables exist and are numbers
  (assert-true (boundp 'swimmy.school::*prediction-weight-trend*) "Trend weight should exist")
  (assert-true (boundp 'swimmy.school::*prediction-weight-volatility*) "Volatility weight should exist")
  (assert-true (boundp 'swimmy.school::*prediction-min-confidence*) "Min confidence should exist")
  (assert-true (numberp swimmy.school::*prediction-weight-trend*) "Trend weight should be a number")
  (assert-true (> swimmy.school::*prediction-weight-trend* 0) "Weight should be positive"))

(deftest test-prediction-feedback-functions
  "V17: Test feedback loop functions exist (Issue 2)"
  ;; Check that feedback functions are defined
  (assert-true (fboundp 'cl-user::record-prediction-outcome) "record-prediction-outcome should exist")
  (assert-true (fboundp 'cl-user::update-prediction-accuracy) "update-prediction-accuracy should exist")
  (assert-true (fboundp 'cl-user::find-prediction-by-symbol-direction) "finder should exist"))

(deftest test-prediction-threshold-effects
  "V17: Test that threshold affects should-take-trade-p"
  (let ((high-conf-pred (cl-user::make-trade-prediction 
                          :symbol "USDJPY" :direction :buy 
                          :predicted-outcome :win :confidence 0.7))
        (low-conf-pred (cl-user::make-trade-prediction 
                         :symbol "USDJPY" :direction :buy 
                         :predicted-outcome :win :confidence 0.3)))
    ;; High confidence should pass (0.7 > 0.5 default threshold)
    (assert-true (swimmy.school::should-take-trade-p high-conf-pred) "High conf should pass")
    ;; Low confidence should fail
    (assert-false (swimmy.school::should-take-trade-p low-conf-pred) "Low conf should fail")))

;;; V18: Extended Tests (Uncle Bob)
(deftest test-factor-correlation-calculation
  "V18: Test dynamic correlation calculation (López de Prado)"
  ;; High correlation when trend and momentum agree
  (assert-equal 0.8 (swimmy.school::calculate-factor-correlation t t nil) 
                "Trend+momentum aligned = high correlation")
  ;; Zero correlation when reversion is active
  (assert-equal 0.0 (swimmy.school::calculate-factor-correlation t nil t)
                "Reversion = no correlation")
  ;; Low correlation in mixed cases
  (assert-true (< (swimmy.school::calculate-factor-correlation nil nil nil) 0.5)
               "Mixed case = low correlation"))

(deftest test-extracted-factor-functions
  "V18: Test that factor functions are properly extracted (Fowler)"
  (assert-true (fboundp 'cl-user::calculate-trend-factor) "Trend factor extracted")
  (assert-true (fboundp 'cl-user::calculate-volatility-factor) "Volatility factor extracted")
  (assert-true (fboundp 'cl-user::calculate-momentum-factor) "Momentum factor extracted")
  (assert-true (fboundp 'cl-user::calculate-session-factor) "Session factor extracted"))

(deftest test-learning-cycle-adjustment
  "V18: Test that learning cycle can adjust thresholds (Naval/Ng)"
  (let ((orig-threshold swimmy.school::*prediction-min-confidence*))
    (declare (ignore orig-threshold))
    ;; Verify adjustment functions exist and threshold is settable
    (assert-true (numberp swimmy.school::*prediction-min-confidence*) "Threshold is number")
    (assert-true (and (>= swimmy.school::*prediction-min-confidence* 0.3)
                      (<= swimmy.school::*prediction-min-confidence* 0.7))
                 "Threshold in valid range")))

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
  (setf swimmy.globals::*max-total-exposure* 5.0)
  (setf swimmy.globals::*danger-level* 3)
  (setf swimmy.globals::*consecutive-losses* 1)
  (setf swimmy.globals::*current-equity* 100000.0)
  (setf swimmy.globals::*locked-treasury* 20000.0)
  (setf swimmy.globals::*daily-pnl* 500.0)
  (setf swimmy.globals::*evolved-strategies* nil) 
  (setf swimmy.globals::*daily-loss-limit* -5000)

  ;; Test Taleb
  (let ((taleb (swimmy.school::generate-taleb-report)))
    (assert-true (search "Taleb" taleb) "Taleb report should contain title"))

  ;; Test Graham (PG)
  (let ((graham (swimmy.school::generate-pg-report)))
    (assert-true (search "Graham" graham) "PG report should contain title"))

  ;; Test Naval
  (let ((naval (swimmy.school::generate-naval-report)))
    (assert-true (search "Naval" naval) "Naval report should contain title"))
    
  ;; Test Orchestrator
  (let ((full (swimmy.school::generate-advisor-reports)))
    (assert-true (> (length full) 50) "Full report should not be empty")))
