;;; ============================================================================
;;; shell/briefing.lisp - Daily Briefing & Status

(in-package :swimmy.shell)
;;; ============================================================================
;;; User-facing briefings with natural language and emoji
;;; Part of "The Efficient Gardener" refactoring
;;;
;;; Dependencies:
;;;   - src/lisp/engine/goals.lisp
;;;   - src/lisp/shell/notifications.lisp
;;;               src/lisp/school/advisors.lisp (for advisor text)
;;; ============================================================================

;;; ==========================================
;;; BRIEFING STATE
;;; ==========================================

(defparameter *last-briefing-hour* -1)

;;; ==========================================
;;; GOAL STATUS REPORT (Emoji-rich)
;;; ==========================================

(defun report-goal-status ()
  "Output current goal status to log with emoji and formatting."
  (let ((progress (get-goal-progress)))
    (format t "~%[L] ═══════════════════════════════════════~%")
    (format t "[L] 🎯 GOAL STATUS: ¥~:d / ¥~:d (~,1f%)~%"
            (round (getf progress :actual-pnl))
            *monthly-goal*
            (getf progress :progress-pct))
    (format t "[L] 📅 Day ~d | Expected: ¥~:d | Actual: ¥~:d~%"
            (getf progress :days-elapsed)
            (round (getf progress :expected-pnl))
            (round (getf progress :actual-pnl)))
    (format t "[L] ~a Pace: ~,0f% (~a)~%"
            (if (getf progress :on-track) "✅" "⚠️")
            (getf progress :pace-pct)
            (if (getf progress :on-track) "ON TRACK" "BEHIND"))
    (format t "[L] 📊 Today's target: ¥~:d | Today's PnL: ¥~:d~%"
            (get-daily-target)
            (round *daily-pnl*))
    (format t "[L] ═══════════════════════════════════════~%~%")
    progress))

;;; ==========================================
;;; DAILY BRIEFING
;;; ==========================================

(defun generate-daily-briefing ()
  "Generate natural language morning briefing with full formatting."
  (let* ((progress (get-goal-progress))
         (days-elapsed (getf progress :days-elapsed))
         (actual-pnl (getf progress :actual-pnl))
         (daily-target (getf progress :daily-target))
         (pace-pct (getf progress :pace-pct))
         (remaining (getf progress :remaining))
         (trading-days-left (- 22 days-elapsed)))
    
    (format t "~%")
    (format t "[L] ════════════════════════════════════════════════════~%")
    (format t "[L] 🌅 GOOD MORNING! Swimmy Daily Briefing~%")
    (format t "[L] ════════════════════════════════════════════════════~%")
    (format t "~%")
    
    ;; Goal Status
    (format t "[L] 🎯 GOAL STATUS:~%")
    (format t "[L]    Monthly target: ¥~:d~%" *monthly-goal*)
    (format t "[L]    Current progress: ¥~:d (~,1f%)~%" (round actual-pnl) (getf progress :progress-pct))
    (format t "[L]    Remaining: ¥~:d over ~d trading days~%" (round remaining) trading-days-left)
    (format t "~%")
    
    ;; Pace Analysis
    (format t "[L] 📊 PACE ANALYSIS:~%")
    (cond
      ((>= pace-pct 120)
       (format t "[L]    ✅ EXCELLENT! Ahead of schedule (~,0f%)~%" pace-pct)
       (format t "[L]    💡 Consider reducing risk or taking profits~%"))
      ((>= pace-pct 80)
       (format t "[L]    ✅ ON TRACK (~,0f%)~%" pace-pct)
       (format t "[L]    💡 Continue current strategy~%"))
      ((>= pace-pct 50)
       (format t "[L]    ⚠️ SLIGHTLY BEHIND (~,0f%)~%" pace-pct)
       (format t "[L]    💡 Consider slightly more aggressive approach~%"))
      (t
       (format t "[L]    ❌ SIGNIFICANTLY BEHIND (~,0f%)~%" pace-pct)
       (format t "[L]    💡 Review strategy, but don't chase losses~%")))
    (format t "~%")
    
    ;; Today's Plan
    (format t "[L] 📋 TODAY'S PLAN:~%")
    (format t "[L]    Daily target: ¥~:d~%" (round daily-target))
    (format t "[L]    Max daily loss: ¥~:d~%" (abs (if (boundp '*resignation-threshold*)
                                                       *resignation-threshold*
                                                       (- (get-daily-risk-limit)))))
    (format t "[L]    Active systems: Swarm + Memory + Leader + All Protections~%")
    (format t "~%")
    
    ;; Current Market
    (when (and (boundp '*candle-history*) *candle-history*)
      (format t "[L] 📈 MARKET STATUS:~%")
      (format t "[L]    Regime: ~a | Volatility: ~a~%"
              (if (boundp '*current-regime*) *current-regime* "UNKNOWN")
              (if (boundp '*current-volatility-state*) *current-volatility-state* "UNKNOWN"))
      (when (and (boundp '*current-leader*) *current-leader*)
        (format t "[L]    Current leader: ~a~%"
                (leader-info-strategy-name *current-leader*))))
    
    (format t "~%")
    (format t "[L] 🐟🐟🐟 The school is ready. Let's go!~%")
    (format t "[L] ════════════════════════════════════════════════════~%~%")))

(defun maybe-generate-briefing ()
  "Generate briefing once per day at start."
  (let ((current-hour (mod (floor (get-universal-time) 3600) 24)))
    (when (or (= *last-briefing-hour* -1)
              (and (= current-hour 0) (/= *last-briefing-hour* 0)))
      (setf *last-briefing-hour* current-hour)
      (generate-daily-briefing)
      (when (fboundp 'reset-danger-state)
        (reset-danger-state)))))

;;; ==========================================
;;; NATURAL LANGUAGE STATUS
;;; ==========================================

(defun swimmy-status ()
  "Generate current status in natural language - for Intent Trading."
  (let* ((progress (get-goal-progress))
         (pnl (round (getf progress :actual-pnl)))
         (pace (getf progress :pace-pct)))
    (format nil "現在 ¥~:d (~,0f% ペース). ~a. ~a."
            pnl pace
            (if (getf progress :on-track) "順調です" "ペースが遅れています")
            (if (and (fboundp 'has-resigned-p) (has-resigned-p))
                "本日のトレード終了"
                "トレード中"))))

;;; ==========================================
;;; PERFORMANCE SUMMARY (Formatted)
;;; ==========================================

(defun get-performance-summary ()
  "Generate formatted performance summary for user display."
  (let ((stats (if (fboundp 'get-performance-stats)
                   (get-performance-stats)
                   (list :avg-pnl 0 :best-day 0 :total-records 0))))
    (format nil "📊 Performance Summary~%Average daily: ¥~,2f~%Best day: ¥~,2f~%Total records: ~d"
            (getf stats :avg-pnl)
            (getf stats :best-day)
            (getf stats :total-records))))

(format t "[SHELL] briefing.lisp loaded~%")
