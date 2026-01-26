;;; ============================================================================
;;; engine/goals.lisp - Goal Decomposition System
;;; ============================================================================
;;; Pure goal calculation logic without UI/emoji
;;; Part of "The Efficient Gardener" refactoring

(in-package :swimmy.engine)
;;;
;;; Dependencies: src/lisp/core/config.lisp (for *daily-pnl*)
;;;               src/lisp/engine/risk.lisp (for loss limits)
;;; ============================================================================


;;; ==========================================
;;; GOAL STATE
;;; ==========================================
;; NOTE: *monthly-goal*, *trading-days-in-month*, *accumulated-pnl* are defined
;; in core/config.lisp. Do not redefine here to avoid overriding runtime config.
(defvar *goal-start-date* nil)       ; When we started tracking

;; Note: *risk-tolerance* is in config.lisp
;; Note: *daily-loss-limit* is in config.lisp

;;; ==========================================
;;; GOAL CALCULATION FUNCTIONS
;;; ==========================================

(defun set-monthly-goal (amount)
  "Set monthly profit target and initialize tracking.
   Returns the calculated daily target."
  (setf *monthly-goal* amount)
  (setf *goal-start-date* (get-universal-time))
  (setf *accumulated-pnl* 0.0)
  (calculate-daily-targets)
  (get-daily-target))

(defun safe-trading-days ()
  "Return valid trading days in month or a safe fallback."
  (if (and (boundp '*trading-days-in-month*)
           (numberp *trading-days-in-month*)
           (> *trading-days-in-month* 0))
      *trading-days-in-month*
      22))

(defun set-daily-loss-limit-from-target (daily-target &key (multiplier 1.0) (min-limit 1000))
  "Set *daily-loss-limit* based on daily target. Returns applied absolute limit."
  (when (and (numberp daily-target) (> daily-target 0))
    (let ((limit (max min-limit (round (* daily-target multiplier)))))
      (setf *daily-loss-limit* (- limit))
      limit)))

(defun calculate-daily-targets ()
  "Calculate daily targets based on monthly goal.
   Delegates risk limit setting to risk module."
  (let ((daily-target (ceiling (/ *monthly-goal* (safe-trading-days)))))
    ;; Forward to Risk Authority if available
    (when (fboundp 'set-daily-loss-limit-from-target)
      (set-daily-loss-limit-from-target daily-target))))

(defun get-daily-target ()
  "Get current daily profit target."
  (ceiling (/ *monthly-goal* (safe-trading-days))))

(defun get-daily-risk-limit ()
  "Get maximum acceptable daily loss (from global config)."
  (abs *daily-loss-limit*))

(defun get-days-elapsed ()
  "Get trading days since goal start."
  (if *goal-start-date*
      (let ((seconds-elapsed (- (get-universal-time) *goal-start-date*)))
        (max 1 (floor seconds-elapsed (* 24 3600))))  ; At least 1 day
      1))

(defun get-goal-progress ()
  "Calculate goal progress and status.
   Returns a property list."
  (let* ((days-elapsed (get-days-elapsed))
         (expected-pnl (* (get-daily-target) days-elapsed))
         (actual-pnl (+ (if (boundp '*accumulated-pnl*) *accumulated-pnl* 0.0)
                        (if (boundp '*daily-pnl*) *daily-pnl* 0.0)))
         (progress-pct (if (> *monthly-goal* 0)
                           (* 100 (/ actual-pnl *monthly-goal*))
                           0))
         (pace-pct (if (> expected-pnl 0)
                       (* 100 (/ actual-pnl expected-pnl))
                       100)))
    (list :days-elapsed days-elapsed
          :expected-pnl expected-pnl
          :actual-pnl actual-pnl
          :progress-pct progress-pct
          :pace-pct pace-pct
          :on-track (>= pace-pct 80)
          :daily-target (get-daily-target)
          :remaining (- *monthly-goal* actual-pnl))))

(defun update-accumulated-pnl ()
  "Update accumulated PnL at end of day. Resets *daily-pnl* to 0."
  (incf *accumulated-pnl* *daily-pnl*)
  (setf *daily-pnl* 0.0))

;;; ==========================================
;;; INITIALIZATION
;;; ==========================================

(calculate-daily-targets)

(format t "[ENGINE] goals.lisp loaded~%")
