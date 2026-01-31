;;; school-lifecycle.lisp - Strategy Lifecycle Management (Benching/Kill)
;;; Phase 6 Implementation (memo3 Section 10)

(in-package :swimmy.school)

;;; ============================================================================
;;; LIFECYCLE PARAMETERS
;;; ============================================================================

(defparameter *max-consecutive-losses* 5 "Max consecutive losses before benching")
(defparameter *cooldown-duration* (* 24 3600) "Cooldown duration in seconds (24 hours)")
(defparameter *kill-threshold-edge* -0.5 "Adaptation edge threshold for Soft Kill")
(defparameter *min-trades-for-kill* 50 "Minimum trades required before Kill consideration")

;;; ============================================================================
;;; LIFECYCLE MANAGEMENT
;;; ============================================================================

(defun manage-strategy-lifecycle (strategy outcome pnl)
  "Update strategy lifecycle state based on trade outcome.
   Called after every trade close."
  (when strategy
    (let ((name (strategy-name strategy)))
      
      ;; 1. Update Consecutive Losses
      (if (> pnl 0)
          (setf (strategy-consecutive-losses strategy) 0)
          (incf (strategy-consecutive-losses strategy)))
      
      ;; 2. Check for Immediate Bench (Consecutive Losses)
      (when (>= (strategy-consecutive-losses strategy) *max-consecutive-losses*)
        (bench-strategy-lifecycle strategy "Consecutive Losses Limit Reached"))
      
      ;; 3. Check for Soft Kill (Poor Adaptation)
      ;; Only check if we have enough trades to be statistically significant
      (when (> (strategy-trades strategy) *min-trades-for-kill*)
        (let* ((regime (if (boundp '*current-regime*) *current-regime* :unknown))
               (adapt-score (if (fboundp 'get-adaptation-multiplier)
                                ;; We want the raw edge, not multiplier, but for now infer from multiplier
                                ;; Multiplier = 1.0 + (Sigmoid(Edge) - 0.5)
                                ;; Roughly, if Multiplier < 0.7, Edge is very bad.
                                (get-adaptation-multiplier strategy regime)
                                1.0)))
          (when (< adapt-score 0.7) ;; Equivalent to Edge < -0.5 approx
            (kill-strategy-lifecycle strategy (format nil "Poor Adaptation Score (~,2f) in ~a" adapt-score regime)))))
      
      ;; Update timestamp
      (setf (strategy-last-update strategy) (get-universal-time)))))

(defun bench-strategy-lifecycle (strategy reason)
  "Bench a strategy for the cooldown period."
  (let ((name (strategy-name strategy)))
    (setf (strategy-status strategy) :benched)
    (setf (strategy-status-reason strategy) reason) ;; Expert Panel Cleanup
    (setf (strategy-cooldown-until strategy) (+ (get-universal-time) *cooldown-duration*))
    
    ;; Sync with legacy system (if using *benched-strategies* hash)
    ;; REMOVED: Expert Panel Cleanup (Hickey) - Single Source of Truth
    
    ;; V50.5.1 Persistence Fix
    (upsert-strategy strategy)
    
    (format t "[LIFECYCLE] 🧊 ~a BENCHED! Reason: ~a (Cooldown: ~dS)~%" 
            name reason *cooldown-duration*)))

(defun kill-strategy-lifecycle (strategy reason)
  "Soft Kill a strategy (Long Term Bench or Removal)."
  (let ((name (strategy-name strategy)))
    (setf (strategy-status strategy) :killed)
    (setf (strategy-status-reason strategy) (format nil "KILLED: ~a" reason)) ;; Expert Panel Cleanup
    ;; Killed strategies might need manual intervention or very long cooldown
    (setf (strategy-cooldown-until strategy) (+ (get-universal-time) (* 7 *cooldown-duration*))) 
    
    ;; REMOVED: Expert Panel Cleanup (Hickey)
    
    ;; V50.5.1 Persistence Fix
    (upsert-strategy strategy)
    
    (format t "[LIFECYCLE] 💀 ~a SOFT KILLED! Reason: ~a~%" name reason)))

(defun check-unbench-condition (strategy)
  "Check if a strategy can be unbenched."
  (when (and (eq (strategy-status strategy) :benched)
             (strategy-cooldown-until strategy)
             (> (get-universal-time) (strategy-cooldown-until strategy)))
    
    (setf (strategy-status strategy) :active)
    (setf (strategy-status-reason strategy) "") ;; Clear reason
    (setf (strategy-consecutive-losses strategy) (floor *max-consecutive-losses* 2)) ;; Reset to half capacity
    (setf (strategy-cooldown-until strategy) 0)
    
    ;; REMOVED: Expert Panel Cleanup (Hickey)
    
    ;; V50.5.1 Persistence Fix
    (upsert-strategy strategy)
    
    (format t "[LIFECYCLE] 🔥 ~a UNBENCHED! Ready for battle.~%" (strategy-name strategy))))

(defun perform-daily-lifecycle-review ()
  "Iterate through all strategies and check for unbenching."
  (when (boundp '*strategy-knowledge-base*)
    (dolist (strat *strategy-knowledge-base*)
      (check-unbench-condition strat))))

(defun count-benched-strategies ()
  "Count the number of strategies currently in BENCHED status.
   Single Source of Truth: Checks strategy-status slot."
  (let ((count 0))
    (when (boundp '*strategy-knowledge-base*)
      (dolist (s *strategy-knowledge-base*)
        (when (eq (strategy-status s) :benched)
          (incf count))))
    (when (boundp 'swimmy.globals:*evolved-strategies*)
      (dolist (s swimmy.globals:*evolved-strategies*)
        (when (eq (strategy-status s) :benched)
          (incf count))))
    count))
