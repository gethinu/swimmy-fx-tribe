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

(deftest test-pattern-soft-gate-reduces-lot-on-mismatch
  "Pattern gate should reduce lot by 0.7x on H1 mismatch."
  (let ((orig-query (symbol-function 'swimmy.core:query-pattern-similarity)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.core:query-pattern-similarity)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (values '((p_up . 0.30) (p_down . 0.55) (p_flat . 0.15)
                            (distortion_passed . t))
                          nil)))
          (multiple-value-bind (lot gate)
              (swimmy.school::apply-pattern-soft-gate
               :trend "USDJPY" :buy "H1" nil 0.10 "UT-STRAT")
            (assert-true (< (abs (- lot 0.07)) 1e-6)
                         "Expected 0.7x lot on mismatch")
            (assert-true (eq t (cdr (assoc :applied gate)))
                         "Gate should be applied")
            (assert-equal "MISMATCH" (cdr (assoc :reason gate))
                          "Reason should be MISMATCH")))
      (setf (symbol-function 'swimmy.core:query-pattern-similarity) orig-query))))

(deftest test-pattern-soft-gate-fade-decision-applies-stronger-reduction
  "Pattern gate should apply fade multiplier when decision_action=fade."
  (let ((orig-query (symbol-function 'swimmy.core:query-pattern-similarity)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.core:query-pattern-similarity)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (values '((p_up . 0.70) (p_down . 0.20) (p_flat . 0.10)
                            (distortion_passed . t)
                            (decision_action . "fade")
                            (vector_weight_applied . 0.80)
                            (weight_source . "file_symbol_timeframe"))
                          nil)))
          (multiple-value-bind (lot gate)
              (swimmy.school::apply-pattern-soft-gate
               :trend "USDJPY" :buy "H1" nil 0.10 "UT-STRAT")
            (assert-true (< (abs (- lot 0.055)) 1e-6)
                         "Expected 0.55x lot on decision_action=fade")
            (assert-true (eq t (cdr (assoc :applied gate)))
                         "Gate should be applied")
            (assert-equal "DECISION_FADE" (cdr (assoc :reason gate))
                          "Reason should be DECISION_FADE")
            (assert-true (< (abs (- 0.80 (cdr (assoc :vector_weight_applied gate)))) 1e-6)
                         "Expected vector_weight_applied telemetry field")
            (assert-equal "file_symbol_timeframe" (cdr (assoc :weight_source gate))
                          "Expected weight_source telemetry field")))
      (setf (symbol-function 'swimmy.core:query-pattern-similarity) orig-query))))

(deftest test-pattern-soft-gate-no-trade-decision-applies-strongest-reduction
  "Pattern gate should apply no-trade multiplier when decision_action=no-trade."
  (let ((orig-query (symbol-function 'swimmy.core:query-pattern-similarity)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.core:query-pattern-similarity)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (values '((p_up . 0.65) (p_down . 0.25) (p_flat . 0.10)
                            (distortion_passed . t)
                            (decision_action . "no-trade")
                            (enforce_no_trade . t))
                          nil)))
          (multiple-value-bind (lot gate)
              (swimmy.school::apply-pattern-soft-gate
               :trend "USDJPY" :buy "H1" nil 0.10 "UT-STRAT")
            (assert-true (< (abs (- lot 0.035)) 1e-6)
                         "Expected 0.35x lot on decision_action=no-trade")
            (assert-true (eq t (cdr (assoc :applied gate)))
                         "Gate should be applied")
            (assert-equal "DECISION_NO_TRADE" (cdr (assoc :reason gate))
                          "Reason should be DECISION_NO_TRADE")
            (assert-true (eq t (cdr (assoc :enforce_no_trade gate)))
                         "Expected enforce_no_trade telemetry field")))
      (setf (symbol-function 'swimmy.core:query-pattern-similarity) orig-query))))

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

(deftest test-maybe-request-oos-backtest-includes-explicit-range
  "OOS dispatch should include explicit start/end range derived from CSV."
  (let* ((symbol "UT-OOS-RANGE")
         (data-path (swimmy.core::swimmy-path (format nil "data/historical/~a_M1.csv" symbol)))
         (tmp-db (format nil "/tmp/swimmy-oos-range-~a.db" (get-universal-time)))
         (orig-db swimmy.core::*db-path-default*)
         (orig-request (symbol-function 'swimmy.school::request-backtest))
         (orig-interval swimmy.school::*oos-request-interval*)
         (orig-dispatch-map swimmy.school::*oos-last-dispatch-at*)
         (captured-start nil)
         (captured-end nil)
         (captured-suffix nil))
    (unwind-protect
         (progn
           (ensure-directories-exist data-path)
           (with-open-file (s data-path :direction :output :if-exists :supersede :if-does-not-exist :create)
             (write-line "timestamp,open,high,low,close,volume" s)
             ;; 10 bars, ascending by 60s.
             (dotimes (i 10)
               (format s "~d,145.1,145.2,145.0,145.15,1000~%" (+ 1700000000 (* i 60)))))

           (setf swimmy.core::*db-path-default* tmp-db)
           (swimmy.core:close-db-connection)
           (swimmy.school::init-db)
           (setf swimmy.school::*oos-request-interval* 0)
           (setf swimmy.school::*oos-last-dispatch-at* (make-hash-table :test 'equal))

           (setf (symbol-function 'swimmy.school::request-backtest)
                 (lambda (strat &key suffix request-id start-ts end-ts &allow-other-keys)
                   (declare (ignore strat request-id))
                   (setf captured-suffix suffix
                         captured-start start-ts
                         captured-end end-ts)
                   t))

           (let* ((strat (cl-user::make-strategy :name "UnitTest-OOS-Range"
                                                 :symbol symbol
                                                 :oos-sharpe nil))
                  (req-id (swimmy.school::maybe-request-oos-backtest strat)))
             (assert-true req-id "dispatch should return request_id")
             (assert-equal "-OOS" captured-suffix "A-rank OOS suffix should be canonical")
             ;; n=10, ratio=0.3 => OOS starts at index floor(10*0.7)=7
             (assert-equal 1700000420 captured-start "expected OOS start timestamp from CSV split")
             (assert-equal 1700000540 captured-end "expected OOS end timestamp from CSV split")))
      (setf swimmy.school::*oos-request-interval* orig-interval)
      (setf swimmy.school::*oos-last-dispatch-at* orig-dispatch-map)
      (setf (symbol-function 'swimmy.school::request-backtest) orig-request)
      (setf swimmy.core::*db-path-default* orig-db)
      (swimmy.core:close-db-connection)
      (when (probe-file data-path) (delete-file data-path))
      (when (probe-file tmp-db) (delete-file tmp-db)))))

(deftest test-top-candidates-displays-official-and-composite-evidence
  "Top Candidates should display official/composite evidence and sort by composite-adjusted evidence."
  (let* ((tmp-db (format nil "/tmp/swimmy-topcand-evidence-split-~a.db" (get-universal-time)))
         (tmp-lib (format nil "/tmp/swimmy-lib-topcand-evidence-split-~a/" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.persistence::*library-path* (merge-pathnames tmp-lib #P"/"))
          (swimmy.school::*disable-auto-migration* t)
          (*strategy-knowledge-base* nil))
      (unwind-protect
           (progn
             (swimmy.school::init-db)
             (swimmy.persistence:init-library)
             (swimmy.school::upsert-strategy
              (make-strategy :name "TC-COMP-HIGH"
                             :sharpe 5.0
                             :trades 10
                             :symbol "USDJPY"
                             :rank :B))
             (swimmy.school::upsert-strategy
              (make-strategy :name "TC-OFFICIAL-HIGH"
                             :sharpe 4.8
                             :trades 100
                             :symbol "USDJPY"
                             :rank :B))
             ;; Composite evidence from persisted trade_list should lift TC-COMP-HIGH.
             (let ((trade-list
                     (loop for i from 1 to 120
                           collect (list (cons 'timestamp (+ 1700000000 i))
                                         (cons 'pnl 1.0)
                                         (cons 'symbol "USDJPY")))))
               (swimmy.school:record-backtest-trades
                "RID-TC-COMP-HIGH" "TC-COMP-HIGH" "OOS" trade-list))
             (let* ((snippet (swimmy.school::build-top-candidates-snippet-from-db))
                    (pos-comp (search "TC-COMP-HIGH" snippet))
                    (pos-official (search "TC-OFFICIAL-HIGH" snippet)))
               (assert-not-nil pos-comp "Composite-evidence candidate should appear")
               (assert-not-nil pos-official "Official-evidence candidate should appear")
               (assert-true (search "TE official/composite=10/120" snippet)
                            "Expected explicit official/composite evidence display")
               (assert-true (< pos-comp pos-official)
                            "Composite-adjusted ordering should prioritize TC-COMP-HIGH")))
        (swimmy.core:close-db-connection)
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
         (let ((swimmy.core::*db-path-default* tmp-db)
               (swimmy.core::*sqlite-conn* nil)
               (swimmy.school::*disable-auto-migration* t))
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
                      ;; Persist strategy to DB so OOS result handling remains deterministic
                      ;; even when in-memory KB is empty.
                      (swimmy.school::upsert-strategy strat)
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
                        (declare (ignore _time3 _status3))
                        (assert-true (null rid3)))) )
             (setf (symbol-function 'swimmy.school::request-backtest) orig-request))))
      (swimmy.core:close-db-connection)
      (when (probe-file tmp-db) (delete-file tmp-db)))))

(deftest test-oos-stale-result-ignored
  "Stale OOS result (request_id mismatch) should be ignored."
  (let* ((tmp-db (format nil "data/memory/test-oos-stale-~a.db" (get-universal-time)))
         (orig-db swimmy.core::*db-path-default*))
    (unwind-protect
         (let ((swimmy.core::*db-path-default* tmp-db)
               (swimmy.core::*sqlite-conn* nil)
               (swimmy.school::*disable-auto-migration* t))
           (swimmy.core:close-db-connection)
           (swimmy.school::init-db)
           (let* ((strat (cl-user::make-strategy :name "UT-OOS-STALE"
                                                 :symbol "USDJPY"
                                                 :oos-sharpe nil))
                  (*strategy-knowledge-base* (list strat))
                  (*evolved-strategies* nil))
             (swimmy.school::enqueue-oos-request "UT-OOS-STALE" "RID-NEW" :status "sent")
             (swimmy.school::handle-oos-backtest-result
              "UT-OOS-STALE" (list :sharpe 0.9 :request-id "RID-OLD"))
             (assert-true (null (cl-user::strategy-oos-sharpe strat))
                          "stale should not update oos_sharpe")
             (multiple-value-bind (rid _time _status)
                 (swimmy.school::lookup-oos-request "UT-OOS-STALE")
               (declare (ignore _time _status))
               (assert-equal "RID-NEW" rid
                             "stale result should not clear queue"))))
      (setf swimmy.core::*db-path-default* orig-db)
      (swimmy.core:close-db-connection)
      (when (probe-file tmp-db) (delete-file tmp-db)))))

(deftest test-oos-result-falls-back-to-db-when-strategy-missing-in-memory
  "OOS result should update strategy from DB even when in-memory lookup misses."
  (let* ((tmp-db (format nil "data/memory/test-oos-db-fallback-~a.db" (get-universal-time)))
         (orig-db swimmy.core::*db-path-default*))
    (unwind-protect
         (let ((swimmy.core::*db-path-default* tmp-db)
               (swimmy.core::*sqlite-conn* nil)
               (swimmy.school::*disable-auto-migration* t))
           (swimmy.core:close-db-connection)
           (swimmy.school::init-db)
           (let* ((name "UT-OOS-DB-FALLBACK")
                  (rid "RID-OOS-DB-FALLBACK")
                  (strat (cl-user::make-strategy :name name
                                                 :rank :B
                                                 :symbol "USDJPY"
                                                 :sharpe 0.20
                                                 :profit-factor 1.05
                                                 :win-rate 0.30
                                                 :max-dd 0.20
                                                 :oos-sharpe nil)))
             ;; Persist strategy only in DB (simulate daemon restart / memory miss).
             (swimmy.school::upsert-strategy strat)
             (swimmy.school::enqueue-oos-request name rid :status "sent")
             (let ((*strategy-knowledge-base* nil)
                   (*evolved-strategies* nil))
               (swimmy.school::handle-oos-backtest-result
                name
                (list :sharpe 0.88 :request-id rid)))
             (multiple-value-bind (queued _at _status)
                 (swimmy.school::lookup-oos-request name)
               (declare (ignore _at _status))
               (assert-true (null queued)
                            "handled OOS result should clear oos_queue row"))
             (let ((oos (swimmy.school::execute-single
                         "SELECT oos_sharpe FROM strategies WHERE name=?"
                         name)))
               (assert-true (and (numberp oos) (> (float oos 0.0) 0.87))
                            "DB row should store OOS sharpe from result"))))
      (setf swimmy.core::*db-path-default* orig-db)
      (swimmy.core:close-db-connection)
      (when (probe-file tmp-db) (delete-file tmp-db)))))

(deftest test-oos-dispatch-failure-marks-error-not-sent
  "Rejected OOS dispatch should not remain as sent in oos_queue."
  (let* ((tmp-db (format nil "/tmp/swimmy-oos-dispatch-fail-~a.db" (get-universal-time)))
         (orig-db swimmy.core::*db-path-default*)
         (orig-auto swimmy.school::*disable-auto-migration*)
         (orig-request (symbol-function 'swimmy.school::request-backtest)))
    (unwind-protect
         (let ((swimmy.core::*db-path-default* tmp-db)
               (swimmy.core::*sqlite-conn* nil))
           (setf swimmy.school::*disable-auto-migration* t)
           (swimmy.core:close-db-connection)
           (swimmy.school::init-db)
           (setf (symbol-function 'swimmy.school::request-backtest)
                 (lambda (&rest _args) (declare (ignore _args)) nil))
           (let* ((name "UT-OOS-DISPATCH-FAIL")
                  (rid (swimmy.school::maybe-request-oos-backtest
                        (cl-user::make-strategy :name name :symbol "USDJPY" :oos-sharpe nil))))
             (assert-true (null rid) "dispatch failure should return NIL")
             (multiple-value-bind (_req-id _req-at status) (swimmy.school::lookup-oos-request name)
               (declare (ignore _req-id _req-at))
               (assert-equal "error" status "queue row should be marked error"))
             (let ((metrics (swimmy.school::report-oos-db-metrics)))
               (assert-equal 0 (getf metrics :sent 0) "sent should remain zero"))))
      (setf swimmy.core::*db-path-default* orig-db)
      (setf swimmy.school::*disable-auto-migration* orig-auto)
      (setf (symbol-function 'swimmy.school::request-backtest) orig-request)
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

(deftest test-evolution-report-includes-validation-coverage
  "Evolution report should include DB cumulative validation coverage."
  (let* ((tmp-db (format nil "/tmp/swimmy-report-validation-coverage-~a.db" (get-universal-time))))
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
                      ;; OOS/CPCV coverage fixture:
                      ;; total   -> OOS=3, CPCV=4
                      ;; active  -> OOS=2, CPCV=2 (exclude GRAVEYARD/RETIRED)
                      (swimmy.school::execute-non-query
                       "INSERT OR REPLACE INTO strategies (name, rank, oos_sharpe, cpcv_median, cpcv_pass_rate) VALUES (?, ?, ?, ?, ?)"
                       "COV-ACT-B" ":B" 0.61 0.0 0.0)
                      (swimmy.school::execute-non-query
                       "INSERT OR REPLACE INTO strategies (name, rank, oos_sharpe, cpcv_median, cpcv_pass_rate) VALUES (?, ?, ?, ?, ?)"
                       "COV-ACT-I" ":INCUBATOR" 0.0 1.20 0.72)
                      (swimmy.school::execute-non-query
                       "INSERT OR REPLACE INTO strategies (name, rank, oos_sharpe, cpcv_median, cpcv_pass_rate) VALUES (?, ?, ?, ?, ?)"
                       "COV-ACT-A" ":A" 0.53 1.10 0.75)
                      (swimmy.school::execute-non-query
                       "INSERT OR REPLACE INTO strategies (name, rank, oos_sharpe, cpcv_median, cpcv_pass_rate) VALUES (?, ?, ?, ?, ?)"
                       "COV-GRAVE" ":GRAVEYARD" 0.91 0.95 0.77)
                      (swimmy.school::execute-non-query
                       "INSERT OR REPLACE INTO strategies (name, rank, oos_sharpe, cpcv_median, cpcv_pass_rate) VALUES (?, ?, ?, ?, ?)"
                       "COV-RET" ":RETIRED" 0.0 1.01 0.71)
                      (let ((report (swimmy.school::generate-evolution-report)))
                        (assert-true
                         (search "Validation Coverage (DB): OOS done=3 CPCV done=4 | Active OOS=2 CPCV=2" report)
                         "Report should contain cumulative OOS/CPCV coverage line")))
                 (setf (symbol-function 'swimmy.school::refresh-strategy-metrics-from-db) orig-refresh))))
        (swimmy.core:close-db-connection)
        (when (probe-file tmp-db) (delete-file tmp-db))))))

(deftest test-evolution-report-includes-a-gate-pressure-active-b
  "Evolution report should include A Stage1 gate pressure for active B candidates."
  (let* ((tmp-db (format nil "/tmp/swimmy-report-a-gate-pressure-~a.db" (get-universal-time))))
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
                      ;; Active B fixture:
                      ;; total=5, pass_all=1
                      ;; pass sharpe/pf/wr/maxdd = 4/4/4/4
                      ;; pf_near[1.24,1.30)=1
                      (swimmy.school::execute-non-query
                       "INSERT OR REPLACE INTO strategies (name, rank, sharpe, profit_factor, win_rate, max_dd) VALUES (?, ?, ?, ?, ?, ?)"
                       "AGP-ALL-PASS" ":B" 0.60 1.32 0.45 0.10)
                      (swimmy.school::execute-non-query
                       "INSERT OR REPLACE INTO strategies (name, rank, sharpe, profit_factor, win_rate, max_dd) VALUES (?, ?, ?, ?, ?, ?)"
                       "AGP-PF-NEAR" ":B" 0.70 1.28 0.46 0.10)
                      (swimmy.school::execute-non-query
                       "INSERT OR REPLACE INTO strategies (name, rank, sharpe, profit_factor, win_rate, max_dd) VALUES (?, ?, ?, ?, ?, ?)"
                       "AGP-WR-FAIL" ":B" 0.80 1.40 0.40 0.10)
                      (swimmy.school::execute-non-query
                       "INSERT OR REPLACE INTO strategies (name, rank, sharpe, profit_factor, win_rate, max_dd) VALUES (?, ?, ?, ?, ?, ?)"
                       "AGP-SHARPE-FAIL" ":B" 0.44 1.50 0.50 0.10)
                      (swimmy.school::execute-non-query
                       "INSERT OR REPLACE INTO strategies (name, rank, sharpe, profit_factor, win_rate, max_dd) VALUES (?, ?, ?, ?, ?, ?)"
                       "AGP-DD-FAIL" ":B" 0.90 1.35 0.50 0.20)
                      ;; Non-B should not be counted
                      (swimmy.school::execute-non-query
                       "INSERT OR REPLACE INTO strategies (name, rank, sharpe, profit_factor, win_rate, max_dd) VALUES (?, ?, ?, ?, ?, ?)"
                       "AGP-INCUBATOR" ":INCUBATOR" 0.90 1.40 0.50 0.10)
                      (let ((report (swimmy.school::generate-evolution-report)))
                        (assert-true
                         (search "A Gate Pressure (Active B): total=5 pass_all=1 | pass sharpe=4 pf=4 wr=4 maxdd=4 | pf_near[1.24,1.30)=1"
                                 report)
                         "Report should include Active B gate pressure summary line")))
                 (setf (symbol-function 'swimmy.school::refresh-strategy-metrics-from-db) orig-refresh))))
        (swimmy.core:close-db-connection)
        (when (probe-file tmp-db) (delete-file tmp-db))))))

(deftest test-evolution-report-reflects-evolution-daemon-status
  "Evolution report should reflect evolution daemon service state instead of hardcoded Active."
  (let* ((tmp-db (format nil "/tmp/swimmy-report-daemon-status-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
           (progn
             (swimmy.school::init-db)
             (let ((orig-refresh (symbol-function 'swimmy.school::refresh-strategy-metrics-from-db))
                   (had-status (fboundp 'swimmy.school::%systemd-service-state))
                   (orig-status (and (fboundp 'swimmy.school::%systemd-service-state)
                                     (symbol-function 'swimmy.school::%systemd-service-state))))
               (unwind-protect
                    (progn
                      (setf (symbol-function 'swimmy.school::refresh-strategy-metrics-from-db)
                            (lambda (&rest _args) (declare (ignore _args)) nil))
                      (setf (symbol-function 'swimmy.school::%systemd-service-state)
                            (lambda (&rest _args) (declare (ignore _args)) "inactive"))
                      (let ((report (swimmy.school::generate-evolution-report)))
                        (assert-true (search "⚠️ Evolution Daemon Inactive" report)
                                     "Report should show inactive daemon status")
                        (assert-false (search "✅ Evolution Daemon Active" report)
                                      "Report should not hardcode active daemon status")))
                 (setf (symbol-function 'swimmy.school::refresh-strategy-metrics-from-db) orig-refresh)
                 (if had-status
                     (setf (symbol-function 'swimmy.school::%systemd-service-state) orig-status)
                     (fmakunbound 'swimmy.school::%systemd-service-state)))))
        (swimmy.core:close-db-connection)
        (when (probe-file tmp-db) (delete-file tmp-db))))))

(deftest test-evolution-report-includes-cpcv-gate-failures
  "Evolution report should include CPCV gate failure summary."
  (let* ((tmp-db (format nil "/tmp/swimmy-report-cpcv-gate-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
           (progn
             (swimmy.school::init-db)
             (let ((orig-refresh (symbol-function 'swimmy.school::refresh-strategy-metrics-from-db))
                   (orig-fetch (symbol-function 'swimmy.school::fetch-candidate-strategies)))
               (unwind-protect
                    (progn
                      (setf (symbol-function 'swimmy.school::refresh-strategy-metrics-from-db)
                            (lambda (&rest _args) (declare (ignore _args)) nil))
                      (setf (symbol-function 'swimmy.school::fetch-candidate-strategies)
                            (lambda (&rest _args)
                              (declare (ignore _args))
                              (list
                               (cl-user::make-strategy :name "Gate-1" :symbol "USDJPY"
                                                       :sharpe 0.4 :profit-factor 1.8
                                                       :win-rate 0.55 :max-dd 0.09 :rank :A)
                               (cl-user::make-strategy :name "Gate-2" :symbol "USDJPY"
                                                       :sharpe 0.8 :profit-factor 1.0
                                                       :win-rate 0.4 :max-dd 0.2 :rank :A))))
                      (let ((report (swimmy.school::generate-evolution-report)))
                        (assert-true (search "CPCV Gate Failures:" report)
                                     "Report should include CPCV gate failure line")
                        (assert-true (search "CPCV Stage2 Failures: pass_rate<70%=0" report)
                                     "Stage2 line should render single %")
                        (assert-false (search "%%" report)
                                      "Report should not contain doubled percent symbols")
                        (assert-true (search "sharpe<0.75=1" report)
                                     "Gate sharpe count should appear")
                        (assert-true (search "pf<1.70=1" report)
                                     "Gate PF count should appear")
                        (assert-true (search "wr<0.50=1" report)
                                     "Gate WR count should appear")
                        (assert-true (search "maxdd>=0.10=1" report)
                                     "Gate MaxDD count should appear")
                        (assert-true (search "elite=1" report)
                                     "Gate elite count should appear")
                        (assert-true (search "total=2" report)
                                     "Gate total count should appear")))
                 (setf (symbol-function 'swimmy.school::refresh-strategy-metrics-from-db) orig-refresh)
                 (setf (symbol-function 'swimmy.school::fetch-candidate-strategies) orig-fetch))))
        (swimmy.core:close-db-connection)
        (when (probe-file tmp-db) (delete-file tmp-db))))))

(deftest test-evolution-report-includes-a-near-miss-candidates
  "Evolution report should include A near-miss B-rank candidates."
  (let* ((tmp-db (format nil "/tmp/swimmy-report-near-miss-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
           (progn
             (swimmy.school::init-db)
             (let ((orig-refresh (symbol-function 'swimmy.school::refresh-strategy-metrics-from-db))
                   (orig-fetch (symbol-function 'swimmy.school::fetch-candidate-strategies)))
               (unwind-protect
                    (progn
                      (setf (symbol-function 'swimmy.school::refresh-strategy-metrics-from-db)
                            (lambda (&rest _args) (declare (ignore _args)) nil))
                      (setf (symbol-function 'swimmy.school::fetch-candidate-strategies)
                            (lambda (&rest _args)
                              (declare (ignore _args))
                              (list
                               (cl-user::make-strategy :name "Near-Best" :symbol "USDJPY" :rank :B
                                                       :sharpe 1.30 :profit-factor 1.29
                                                       :win-rate 0.42 :max-dd 0.10)
                               (cl-user::make-strategy :name "Near-Best" :symbol "USDJPY" :rank :B
                                                       :sharpe 1.25 :profit-factor 1.20
                                                       :win-rate 0.41 :max-dd 0.11)
                               (cl-user::make-strategy :name "Near-Mid" :symbol "USDJPY" :rank :B
                                                       :sharpe 0.95 :profit-factor 1.22
                                                       :win-rate 0.40 :max-dd 0.12)
                               (cl-user::make-strategy :name "Near-Far" :symbol "USDJPY" :rank :B
                                                       :sharpe 0.30 :profit-factor 1.08
                                                       :win-rate 0.36 :max-dd 0.20))))
                      (let ((report (swimmy.school::generate-evolution-report)))
                        (labels ((count-substring (needle haystack)
                                   (loop with count = 0
                                         with start = 0
                                         for pos = (search needle haystack :start2 start)
                                         while pos
                                         do (incf count)
                                            (setf start (+ pos (length needle)))
                                         finally (return count))))
                        (assert-true (search "A Near-Miss Candidates (B):" report)
                                     "Report should include near-miss section")
                        (assert-true (search "Near-Best" report)
                                     "Near-miss section should include closest candidate")
                        (assert-true (search "Near-Far" report)
                                     "Near-miss section should include lower-ranked candidate")
                        (assert-equal 1 (count-substring "Near-Best" report)
                                      "Near-miss should dedupe same strategy name")
                        (assert-false (search "%%" report)
                                      "Near-miss should render single %")
                        (assert-true (< (search "Near-Best" report)
                                        (search "Near-Far" report))
                                     "Near-miss candidates should be ordered by deficit asc"))))
                 (setf (symbol-function 'swimmy.school::refresh-strategy-metrics-from-db) orig-refresh)
                 (setf (symbol-function 'swimmy.school::fetch-candidate-strategies) orig-fetch))))
        (swimmy.core:close-db-connection)
        (when (probe-file tmp-db) (delete-file tmp-db))))))

(deftest test-display-candidate-name-preserves-suffix-for-long-names
  "Long candidate names should keep a suffix marker to avoid same-prefix collisions."
  (let* ((a "Bred-Bred--295-Gen1-N3979001")
         (b "Bred-Bred--295-Gen1-N3979002")
         (short-a (swimmy.school::%display-candidate-name a))
         (short-b (swimmy.school::%display-candidate-name b)))
    (assert-false (string= short-a short-b)
                  "Long names with same prefix should remain distinguishable")
    (assert-true (<= (length short-a) 25) "Display name should fit max length")
    (assert-true (search ".." short-a) "Display name should include truncation marker")))

(deftest test-a-base-deficit-score-prioritizes-pf-shortfall
  "A-base deficit score should penalize PF shortfall more than equivalent WR shortfall."
  (let* ((pf-short (cl-user::make-strategy :name "PF-SHORT"
                                           :symbol "USDJPY"
                                           :rank :B
                                           :sharpe 0.60
                                           :profit-factor 1.17
                                           :win-rate 0.50
                                           :max-dd 0.10))
         (wr-short (cl-user::make-strategy :name "WR-SHORT"
                                           :symbol "USDJPY"
                                           :rank :B
                                           :sharpe 0.60
                                           :profit-factor 1.35
                                           :win-rate 0.387
                                           :max-dd 0.10))
         (pf-deficit (swimmy.school::a-base-deficit-score pf-short))
         (wr-deficit (swimmy.school::a-base-deficit-score wr-short)))
    ;; Both are ~10% short on one A gate each; PF should be treated as more severe.
    (assert-true (> pf-deficit wr-deficit)
                 "PF deficit should be weighted heavier than WR deficit in culling penalty")))

(deftest test-strategy-culling-priority-score-prefers-pf-near-band
  "Culling keep order should prioritize PF-near candidate over PF-far high-sharpe candidate."
  (let* ((pf-near (cl-user::make-strategy :name "PF-NEAR"
                                          :symbol "USDJPY"
                                          :rank :B
                                          :sharpe 0.75
                                          :profit-factor 1.28
                                          :win-rate 0.45
                                          :max-dd 0.10))
         (pf-far-high-sharpe (cl-user::make-strategy :name "PF-FAR-HIGH-SH"
                                                     :symbol "USDJPY"
                                                     :rank :B
                                                     :sharpe 1.80
                                                     :profit-factor 1.10
                                                     :win-rate 0.55
                                                     :max-dd 0.06))
         (near-band (swimmy.school::strategy-culling-pf-priority-band pf-near))
         (far-band (swimmy.school::strategy-culling-pf-priority-band pf-far-high-sharpe))
         (near-score (swimmy.school::strategy-culling-priority-score pf-near))
         (far-score (swimmy.school::strategy-culling-priority-score pf-far-high-sharpe)))
    (assert-equal 1 near-band "PF near candidate should be classified as near band")
    (assert-equal 0 far-band "PF-far candidate should be classified as non-near band")
    (assert-true (> near-score far-score)
                 "PF near band should outrank PF-far candidate even when Sharpe is lower")))

(deftest test-oos-status-line-uses-db-metrics
  "OOS status line should reflect DB queue and OOS results."
  (let* ((tmp-db (format nil "/tmp/swimmy-oos-line-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
           (progn
             (swimmy.school::init-db)
             ;; Seed OOS queue
             (swimmy.school::enqueue-oos-request "Sent-1" "req-sent" :status "sent" :requested-at 1)
             (swimmy.school::enqueue-oos-request "Retry-1" "req-retry" :status "retry" :requested-at 2)
             (swimmy.school::enqueue-oos-request "Err-1" "req-err" :status "sent" :requested-at 3)
             (swimmy.school::record-oos-error "Err-1" "req-err" "boom")
             ;; Seed OOS results
             (swimmy.school::execute-non-query
              "INSERT OR REPLACE INTO strategies (name, oos_sharpe) VALUES (?, ?)"
              "OOS-SUCCESS" 0.5)
             (swimmy.school::execute-non-query
              "INSERT OR REPLACE INTO strategies (name, oos_sharpe) VALUES (?, ?)"
              "OOS-FAIL" 0.1)
             (let ((line (swimmy.school::oos-metrics-summary-line)))
               (assert-true (search "sent: 1" line) "DB sent count should appear")
               (assert-true (search "retry: 1" line) "DB retry count should appear")
               (assert-true (search "success: 1" line) "DB success count should appear")
               (assert-true (search "failure: 1" line) "DB failure count should appear")
              (assert-true (search "pending: 2" line) "DB pending count should appear")
              (assert-true (search "oldest:" line) "DB oldest age should appear")))
        (swimmy.core:close-db-connection)
        (when (probe-file tmp-db) (delete-file tmp-db))))))

(deftest test-oos-status-line-includes-failure-breakdown-and-latency
  "OOS status line should include data/send/db failure stats and latency metrics."
  (let ((orig-db (and (fboundp 'swimmy.school::report-oos-db-metrics)
                      (symbol-function 'swimmy.school::report-oos-db-metrics)))
        (orig-queue (and (fboundp 'swimmy.school::fetch-oos-queue-stats)
                         (symbol-function 'swimmy.school::fetch-oos-queue-stats)))
        (orig-failure (and (fboundp 'swimmy.school::report-oos-failure-stats)
                           (symbol-function 'swimmy.school::report-oos-failure-stats)))
        (orig-latency (and (fboundp 'swimmy.school::report-oos-metrics)
                           (symbol-function 'swimmy.school::report-oos-metrics))))
    (unwind-protect
         (progn
           (setf (symbol-function 'swimmy.school::report-oos-db-metrics)
                 (lambda () (list :sent 2 :retry 1 :success 7 :failure 3)))
           (setf (symbol-function 'swimmy.school::fetch-oos-queue-stats)
                 (lambda () (list :pending 4 :oldest-age 12)))
           (setf (symbol-function 'swimmy.school::report-oos-failure-stats)
                 (lambda () (list :data-invalid 4 :send-failure 5 :db-error 6)))
           (setf (symbol-function 'swimmy.school::report-oos-metrics)
                 (lambda () (list :latency-avg 1.25 :latency-min 0.8 :latency-max 2.4)))
           (let ((line (swimmy.school::oos-metrics-summary-line)))
             (assert-true (search "sent: 2" line) "Sent count should appear")
             (assert-true (search "retry: 1" line) "Retry count should appear")
             (assert-true (search "success: 7" line) "Success count should appear")
             (assert-true (search "failure: 3" line) "Failure count should appear")
             (assert-true (search "(data 4 send 5 db 6)" line)
                          "Failure breakdown should use real values")
             (assert-true (search "latency(avg/min/max): 1.25/0.80/2.40 sec" line)
                          "Latency metrics should use real values")))
      (when orig-db
        (setf (symbol-function 'swimmy.school::report-oos-db-metrics) orig-db))
      (when orig-queue
        (setf (symbol-function 'swimmy.school::fetch-oos-queue-stats) orig-queue))
      (when orig-failure
        (setf (symbol-function 'swimmy.school::report-oos-failure-stats) orig-failure))
      (when orig-latency
        (setf (symbol-function 'swimmy.school::report-oos-metrics) orig-latency)))))

(deftest test-oos-status-line-no-queue-duplication
  "OOS status line should not duplicate queue stats suffix."
  (let* ((tmp-db (format nil "/tmp/swimmy-oos-line-dup-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
           (progn
             (swimmy.school::init-db)
             (swimmy.school::enqueue-oos-request "Sent-1" "req-sent" :status "sent" :requested-at 1)
             (let ((line (swimmy.school::build-oos-status-line)))
               (assert-true (search "pending:" line) "Pending should be present in base line")
               (assert-false (search "queue pending:" line) "Queue pending suffix should not appear")))
        (swimmy.core:close-db-connection)
        (when (probe-file tmp-db) (delete-file tmp-db))))))

(deftest test-oos-status-line-ignores-queue-error
  "OOS status line should not append queue error details."
  (let ((orig-fetch (and (fboundp 'swimmy.school::fetch-oos-queue-stats)
                         (symbol-function 'swimmy.school::fetch-oos-queue-stats))))
    (unwind-protect
         (progn
           (setf (symbol-function 'swimmy.school::fetch-oos-queue-stats)
                 (lambda () (list :error "boom")))
           (let ((line (swimmy.school::build-oos-status-line)))
             (assert-false (search "queue error:" line) "Queue error suffix should not appear")))
      (when orig-fetch
        (setf (symbol-function 'swimmy.school::fetch-oos-queue-stats) orig-fetch)))))

(deftest test-evolution-report-excludes-phase2-end-time
  "Evolution report should not include Phase2 end_time line after removal."
  (let* ((tmp-db (format nil "/tmp/swimmy-report-~a.db" (get-universal-time))))
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
                        (assert-false (search "Phase2 EndTime:" report)
                                      "Report should not contain Phase2 EndTime line")))
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

;;; ==========================================
;;; DEFERRED BACKTEST FLUSH TESTS
;;; ==========================================

(deftest test-flush-deferred-founders-respects-limit
  "flush-deferred-founders should support a :limit to avoid startup storms."
  (let ((orig-kb *strategy-knowledge-base*)
        (orig-request (symbol-function 'swimmy.school:request-backtest))
        (calls 0)
        (swimmy.school::*deferred-flush-queue* nil)
        (swimmy.school::*deferred-flush-queue-count* 0)
        (swimmy.school::*deferred-flush-queued-names* (make-hash-table :test 'equal))
        (swimmy.school::*deferred-flush-last-run* 0))
    (unwind-protect
        (progn
          (setf *strategy-knowledge-base*
                (list (make-strategy :name "UT-DEFER-1" :rank nil)
                      (make-strategy :name "UT-DEFER-2" :rank nil)
                      (make-strategy :name "UT-DEFER-3" :rank nil)
                      (make-strategy :name "UT-DEFER-4" :rank nil)))
          (setf (symbol-function 'swimmy.school:request-backtest)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (incf calls)))
          (let ((sent (swimmy.school::flush-deferred-founders :limit 2)))
            (assert-equal 2 sent "Expected flush to return the number sent in this call")
            (assert-equal 2 calls "Expected exactly :limit backtest requests")))
      (setf *strategy-knowledge-base* orig-kb)
      (setf (symbol-function 'swimmy.school:request-backtest) orig-request))))
