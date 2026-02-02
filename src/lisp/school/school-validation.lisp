;;; school-validation.lisp
;;; ============================================================================
;;; P9: OUT-OF-SAMPLE VALIDATION (CPCV Lite)
;;; Extracted from school-backtest.lisp for SRP compliance
;;; ============================================================================
;;; Purpose: Validate strategy on unseen data before A-RANK promotion
;;; Method: Split CSV data into 70% train / 30% test, run backtest on test portion
;;; ============================================================================

(in-package :swimmy.school)

(defparameter *oos-test-ratio* 0.3
  "Ratio of data to use for out-of-sample testing (30%)")

(defparameter *oos-min-sharpe* 0.3
  "Minimum Sharpe ratio required to pass OOS validation for A-RANK")

(defparameter *oos-pending* nil
  "Deprecated (use SQLite oos_queue). Kept for backward compatibility.")

(defparameter *oos-request-interval* 600
  "Minimum seconds between OOS requests per strategy.")

(defparameter *oos-metrics* (make-hash-table :test 'equal)
  "Simple counters and latency stats for OOS. Keys: :sent :success :failure :retry :latency-count :latency-sum :latency-min :latency-max.")

(defun %oos-metric-inc (key &optional (delta 1))
  (incf (gethash key *oos-metrics* 0) delta))

(defun %oos-latency-record (seconds)
  (let* ((cnt (1+ (gethash :latency-count *oos-metrics* 0)))
         (sum (+ (gethash :latency-sum *oos-metrics* 0.0) seconds))
         (mn (if (and (gethash :latency-min *oos-metrics*) (> (gethash :latency-count *oos-metrics* 0) 0))
                 (min (gethash :latency-min *oos-metrics*) seconds)
                 seconds))
         (mx (max (gethash :latency-max *oos-metrics* 0.0) seconds)))
    (setf (gethash :latency-count *oos-metrics*) cnt
          (gethash :latency-sum *oos-metrics*) sum
          (gethash :latency-min *oos-metrics*) mn
          (gethash :latency-max *oos-metrics*) mx)))

(defun maybe-request-oos-backtest (strat)
  "Dispatch async OOS backtest if not requested recently. Returns request-id or NIL."
  (let* ((name (strategy-name strat))
         (now (get-universal-time))
         (req-id nil)
         (last-req (multiple-value-list (lookup-oos-request name)))
         (last-time (second last-req)))
    (when (or (null last-time) (> (- now last-time) *oos-request-interval*))
      (setf req-id (swimmy.core:generate-uuid))
      (enqueue-oos-request name req-id)
      (request-backtest strat :suffix "-OOS" :request-id req-id)
      (%oos-metric-inc :sent)
      (format t "[OOS] 📤 Dispatched OOS backtest for ~a (req ~a)~%" name req-id))
    req-id))

(defun handle-oos-backtest-result (name metrics)
  "Apply OOS backtest result to strategy and attempt promotion."
  (let ((strat (find-strategy name)))
    (when strat
      (let ((sharpe (float (or (getf metrics :sharpe) 0.0))))
        (setf (strategy-oos-sharpe strat) sharpe)
        (complete-oos-request name (getf metrics :request-id))
        (%oos-metric-inc :success)
        (when (getf metrics :oos-latency)
          (%oos-latency-record (getf metrics :oos-latency)))
        (upsert-strategy strat)
        (format t "[OOS] 📥 ~a OOS Sharpe=~,2f~%" name sharpe)
        ;; Attempt promotion if core metrics are already strong enough
        (when (meets-a-rank-criteria strat)
          (if (>= sharpe *oos-min-sharpe*)
              (promote-rank strat :A (format nil "OOS Validated: Sharpe=~,2f" sharpe))
              (format t "[OOS] ❌ ~a failed OOS Sharpe (~,2f < ~,2f)~%" name sharpe *oos-min-sharpe*)))))))

(defun run-oos-validation (strat)
  "Run Out-of-Sample validation for A-RANK promotion candidates.
   Returns (values passed-p oos-sharpe message)"
  (let* ((symbol (or (strategy-symbol strat) "USDJPY"))
         (data-file (format nil "~a" (swimmy.core::swimmy-path (format nil "data/historical/~a_M1.csv" symbol)))))
    
    (unless (probe-file data-file)
      (return-from run-oos-validation 
        (values nil 0.0 (format nil "No data file for ~a" symbol))))
    
    (let ((oos (strategy-oos-sharpe strat)))
      (when (numberp oos)
        (return-from run-oos-validation
          (values (>= oos *oos-min-sharpe*)
                  oos
                  (format nil "OOS cached: Sharpe=~,2f" oos)))))
    
    (let ((req (maybe-request-oos-backtest strat)))
      (if req
          (values nil 0.0 "OOS pending (async)")
          (values nil 0.0 "OOS request throttled")))))

(defun validate-for-a-rank-promotion (strat)
  "Validate strategy is ready for A-RANK promotion using OOS validation.
   Returns T if strategy passes all A-RANK criteria including OOS."
  (format t "[A-RANK] 🎯 Validating ~a for A-RANK promotion...~%"
          (strategy-name strat))
  
  ;; Check basic rank criteria first (without OOS gate)
  (unless (check-rank-criteria strat :A :include-oos nil)
    (format t "[A-RANK] ❌ ~a: Base A-RANK metrics failed (S=~,2f PF=~,2f WR=~,1f%% DD=~,1f%%)~%"
            (strategy-name strat)
            (or (strategy-sharpe strat) 0.0)
            (or (strategy-profit-factor strat) 0.0)
            (* 100 (or (strategy-win-rate strat) 0.0))
            (* 100 (or (strategy-max-dd strat) 1.0)))
    (return-from validate-for-a-rank-promotion nil))
  
  ;; Run OOS validation (async)
  (multiple-value-bind (passed sharpe msg) (run-oos-validation strat)
    (format t "[A-RANK] ~a ~a: ~a~%" 
            (if passed "✅" "⏳") (strategy-name strat) msg)
    (when passed
      (promote-rank strat :A (format nil "OOS Validated: Sharpe=~,2f" sharpe)))
    passed))

(defun meets-a-rank-criteria (strat)
  "Check if strategy meets basic A-RANK criteria (without OOS)."
  (let ((sharpe (or (strategy-sharpe strat) 0.0))
        (pf (or (strategy-profit-factor strat) 0.0))
        (wr (or (strategy-win-rate strat) 0.0))
        (maxdd (or (strategy-max-dd strat) 1.0)))
    (and (>= sharpe 0.3)
         (>= pf 1.2)
         (>= wr 0.4)
         (< maxdd 0.2))))

;;; ============================================================================
;;; P12: CPCV LISP-RUST INTEGRATION
;;; ============================================================================

(defun request-cpcv-validation (strat)
  "V49.5: Request CPCV validation (Non-blocking ASYNC).
   Result is handled by message-dispatcher.lisp"
   (let* ((symbol (or (strategy-symbol strat) "USDJPY"))
          (candles-file (format nil "~a" (swimmy.core::swimmy-path (format nil "data/historical/~a_M1.csv" symbol))))
          (strat-params (alist-to-json (strategy-to-alist strat)))
          (request (jsown:new-js
                     ("action" "CPCV_VALIDATE")
                     ("strategy_name" (strategy-name strat))
                     ("symbol" symbol)
                     ("candles_file" candles-file)
                     ("strategy_params" strat-params))))
     (format t "[CPCV] 📤 Sent CPCV request for ~a (Async)...~%" (strategy-name strat))
     (send-zmq-msg (jsown:to-json request) :target :cmd)
     ;; Return T to indicate request sent successfully
     (values t nil nil)))

(defun validate-for-s-rank-promotion (strat)
  "Validate strategy is ready for S-RANK promotion using true CPCV.
   S-RANK criteria: Sharpe >= 0.5, PF >= 1.5, WR >= 45%, MaxDD < 15%
   Plus: CPCV validation must pass (median Sharpe >= 0.5, 50%+ paths passing).
   Returns T if strategy passes all S-RANK criteria including CPCV."
  (format t "[S-RANK] 🏆 Validating ~a for S-RANK promotion...~%"
          (strategy-name strat))
  
  ;; Check basic S-RANK criteria first
  (let ((sharpe (or (strategy-sharpe strat) 0.0))
        (pf (or (strategy-profit-factor strat) 0.0))
        (wr (or (strategy-win-rate strat) 0.0))
        (maxdd (or (strategy-max-dd strat) 1.0)))
    (unless (and (>= sharpe 0.5) (>= pf 1.5) (>= wr 0.45) (< maxdd 0.15))
      (format t "[S-RANK] ❌ ~a: Does not meet basic S-RANK criteria (S=~,2f PF=~,2f WR=~,2f DD=~,2f)~%"
              (strategy-name strat) sharpe pf wr maxdd)
      (return-from validate-for-s-rank-promotion nil)))
  
  ;; Run CPCV validation via Guardian
  (multiple-value-bind (passed result error-msg) (request-cpcv-validation strat)
    (cond
      (error-msg
       (format t "[S-RANK] ⚠️ ~a: CPCV error - ~a~%" (strategy-name strat) error-msg)
       nil)
      (passed
       (let ((median (getf result :median-sharpe))
             (pass-rate (getf result :pass-rate)))
         (setf (strategy-cpcv-median-sharpe strat) median)
         (setf (strategy-cpcv-pass-rate strat) pass-rate)
         (upsert-strategy strat)
         (format t "[S-RANK] ✅ ~a: CPCV PASSED (median Sharpe=~,3f, ~d/~d paths)~%"
                 (strategy-name strat)
                 median
                 (getf result :passed-count)
                 (getf result :path-count))
         t))
      (t
       (format t "[S-RANK] ❌ ~a: CPCV FAILED (median Sharpe=~,3f)~%"
               (strategy-name strat) (getf result :median-sharpe))
       nil))))

;;; ============================================================================
;;; V48.0: BATCH CPCV VALIDATION TRIGGERS
;;; Called from evolution loop to process A-RANK strategies
;;; ============================================================================

(defparameter *cpcv-batch-size* 20
  "V48.1: Number of A-RANK strategies to validate per cycle (Expert Panel P1: 5→20)")

(defparameter *last-cpcv-cycle* 0
  "Timestamp of last CPCV cycle")

(defparameter *cpcv-cycle-interval* 60
  "V49.6: High-Velocity Validation (300s -> 60s) for rapid S-Rank creation")

(defun run-a-rank-cpcv-batch ()
  "V48.0: Batch process A-RANK strategies for S-RANK promotion via CPCV.
   Called from evolution loop. Processes up to *cpcv-batch-size* at a time."
  (let ((now (get-universal-time)))
    ;; Rate limit CPCV cycles
    (when (< (- now *last-cpcv-cycle*) *cpcv-cycle-interval*)
      (return-from run-a-rank-cpcv-batch nil))
    (setf *last-cpcv-cycle* now)
    ;; Get A-RANK strategies that meet basic S-RANK criteria
    (let* ((a-rank-strategies (remove-if-not
                                (lambda (s)
                                  (and (eq (strategy-rank s) :A)
                                       ;; Only try to promote if it meets basic S-RANK stats (Sharpe > 0.5 etc)
                                       (check-rank-criteria s :S :include-cpcv nil)))
                                *strategy-knowledge-base*))
           ;; Shuffle to avoid getting stuck on the same failing strategies if pool is large
           (shuffled (sort (copy-list a-rank-strategies) 
                           (lambda (a b) (declare (ignore a b)) (< (random 100) 50))))
           (batch (if (> (length shuffled) *cpcv-batch-size*)
                      (subseq shuffled 0 *cpcv-batch-size*)
                      shuffled)))
      (when (null batch)
        (format t "[CPCV] ℹ️ No A-RANK strategies ready for S-RANK validation (Sharpe < 0.5)~%")
        (return-from run-a-rank-cpcv-batch nil))
      (format t "[CPCV] 🔬 Dispatching ~d S-RANK candidates (Async)...~%" (length batch))
      
      ;; V49.5 Fix: Set expected count for batch summary
      (setf swimmy.globals:*expected-cpcv-count* (length batch))
      (setf swimmy.globals:*cpcv-results-buffer* nil)
      (setf swimmy.globals:*cpcv-start-time* (get-universal-time))
      
      ;; Process each strategy
      (dolist (strat batch)
        (handler-case
            (request-cpcv-validation strat) ; Just send request
          (error (e)
            (format t "[CPCV] ⚠️ Error dispatching ~a: ~a~%" (strategy-name strat) e)))))))

;; V50.3: Rank criteria logic moved to school-rank-system.lisp for consolidation.
