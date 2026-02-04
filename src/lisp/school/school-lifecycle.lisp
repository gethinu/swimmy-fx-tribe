;;; school-lifecycle.lisp - Strategy Lifecycle Management (Retire/Kill)
;;; Phase 6 Implementation (memo3 Section 10)

(in-package :swimmy.school)

;;; ============================================================================
;;; LIFECYCLE PARAMETERS
;;; ============================================================================

(defparameter *max-consecutive-losses* 5 "Max consecutive losses before retirement")
(defparameter *min-trades-for-kill* 50 "Minimum trades required before Kill consideration")

;;; ============================================================================
;;; LIFECYCLE MANAGEMENT
;;; ============================================================================

(defun manage-strategy-lifecycle (strategy outcome pnl)
  "Update strategy lifecycle state based on trade outcome.
   Called after every trade close."
  (declare (ignore outcome))
  (when strategy
    ;; 1. Update Consecutive Losses
    (if (> pnl 0)
        (setf (strategy-consecutive-losses strategy) 0)
        (incf (strategy-consecutive-losses strategy)))
    
    ;; 2. Check for Immediate Retirement (Consecutive Losses)
    (when (>= (strategy-consecutive-losses strategy) *max-consecutive-losses*)
      (retire-strategy-lifecycle strategy "Consecutive Losses Limit Reached"))
    
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
    (setf (strategy-last-update strategy) (get-universal-time))))

(defun retire-strategy-lifecycle (strategy reason)
  "Retire a strategy from active use (send to graveyard)."
  (let ((name (strategy-name strategy)))
    (send-to-graveyard strategy reason)
    (format t "[LIFECYCLE] 🪦 ~a RETIRED! Reason: ~a~%" name reason)))

(defun kill-strategy-lifecycle (strategy reason)
  "Kill a strategy and move it to graveyard."
  (let ((name (strategy-name strategy)))
    (send-to-graveyard strategy (format nil "KILLED: ~a" reason))
    (format t "[LIFECYCLE] 💀 ~a KILLED! Reason: ~a~%" name reason)))

(defun perform-daily-lifecycle-review ()
  "Lifecycle review placeholder (benching removed)."
  nil)
