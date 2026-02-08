;;; ============================================================================
;;; shell/notifications.lisp - Discord Notifications

(in-package :swimmy.shell)
;;; ============================================================================
;;; All Discord messaging with emoji and narrative
;;; Part of "The Efficient Gardener" refactoring
;;;
;;; Dependencies:
;;;   - src/lisp/engine/goals.lisp (for goal progress)
;;;   - src/lisp/core/config.lisp (for webhook URLs)
;;;   - src/lisp/core/discord.lisp (for notify-discord, notify-discord-symbol)
;;; ============================================================================

;;; Note: notify-discord and notify-discord-symbol are defined in core/discord.lisp
;;; and inherited via (:use :swimmy.core) in the package definition.

;;; ==========================================
;;; MULTI-CURRENCY DISCORD SETUP
;;; ==========================================

(defparameter *symbol-webhooks* (make-hash-table :test 'equal))

(defun setup-symbol-webhooks ()
  "Setup Discord webhooks for each currency pair. (Consolidated 2026-01-10)"
  ;; All symbols now route to LIVE_FEED
  (let ((live-feed (uiop:getenv "SWIMMY_DISCORD_LIVE_FEED")))
    (setf (gethash "USDJPY" *symbol-webhooks*) live-feed)
    (setf (gethash "EURUSD" *symbol-webhooks*) live-feed)
    (setf (gethash "GBPUSD" *symbol-webhooks*) live-feed)))

;;; ==========================================
;;; TRADE NOTIFICATIONS (Hooks from Engine)
;;; ==========================================

(defun on-trade-opened (idx symbol direction warmup-p conf)
  "Called when a trade is opened. Sends Discord notification."
  (declare (ignore symbol))
  (let* ((emoji (if warmup-p "🔥" "🧠"))
         (dir-str (if (eq direction :LONG) "BUY" "SELL"))
         (suffix (if warmup-p " [WARMUP]" (format nil " (~,0f%)" (* conf 100)))))
    (notify-discord (format nil "~a ~a #~d~a" emoji dir-str idx suffix)
                    :color (if (eq direction :LONG) 3066993 15158332))))

(defun on-trade-closed (idx symbol won pnl)
  "Called when a trade is closed. Sends Discord notification."
  (declare (ignore symbol))
  (let ((emoji (if won "✅" "❌"))
        (color (if won 3066993 15158332)))
    (notify-discord (format nil "~a ~,2f #~d | Day:~,0f" emoji pnl idx *daily-pnl*)
                    :color color)))

;;; ==========================================
;;; BRIEFING NOTIFICATIONS
;;; ==========================================

(defun send-discord-briefing ()
  "Send daily briefing to Discord."
  (let* ((progress (get-goal-progress))
         (msg (format nil "🌅 **Daily Briefing**~%~%🎯 Goal: ¥~:d / ¥~:d (~,1f%)~%📊 Pace: ~,0f% (~a)~%📋 Today's target: ¥~:d~%~%🐟 Swimmy ready!"
                      (round (getf progress :actual-pnl))
                      *monthly-goal*
                      (getf progress :progress-pct)
                      (getf progress :pace-pct)
                      (if (getf progress :on-track) "ON TRACK" "BEHIND")
                      (get-daily-target))))
    (notify-discord msg :color 3066993)))

(defun send-discord-trade-update (symbol direction action pnl)
  "Send trade update to Discord with Intent Trading context."
  (let* ((progress (get-goal-progress))
         (emoji (case action
                  (:open "📈")
                  (:close (if (> pnl 0) "✅" "❌"))
                  (t "📊")))
         (msg (format nil "~a **~a ~a**~%~%PnL: ¥~,2f~%Daily: ¥~:d / ¥~:d~%Goal: ~,1f% complete"
                      emoji direction symbol pnl
                      (round *daily-pnl*) (get-daily-target)
                      (getf progress :progress-pct))))
    (notify-discord-symbol symbol msg :color (if (> pnl 0) 3066993 15158332))))

;;; ==========================================
;;; ALERT NOTIFICATIONS
;;; ==========================================

(defparameter *autonomous-mode* t)
(defparameter *critical-alert-threshold* -3000)

(defun send-critical-discord-alert (alert-type message)
  "Send critical alert to Discord - use sparingly!"
  (when *autonomous-mode*
    (let ((msg (format nil "🚨 **CRITICAL ALERT** 🚨~%~%**Type**: ~a~%**Message**: ~a~%~%*This is an autonomous notification. Check when you can.*"
                       alert-type message)))
      (notify-discord msg :color 15158332))))  ; Red

(defun check-autonomous-alerts ()
  "Check for critical issues and send Discord alerts."
  (when *autonomous-mode*
    ;; Critical: Large daily loss
    (when (< *daily-pnl* *critical-alert-threshold*)
      (send-critical-discord-alert
       "LARGE LOSS"
       (format nil "Daily loss ¥~:d exceeds threshold ¥~:d"
               (round *daily-pnl*) *critical-alert-threshold*)))
    
    ;; Critical: Resigned early with poor performance
    (when (and (fboundp 'has-resigned-p) (has-resigned-p))
      (let* ((progress (get-goal-progress))
             (pace (getf progress :pace-pct)))
        (when (< pace 50)
          (send-critical-discord-alert
           "EARLY RESIGNATION"
           (format nil "Trading stopped at ~,0f% pace. Check conditions." pace)))))
    
    ;; Warning: Consecutive losses
    (when (and (boundp '*consecutive-losses*) (>= *consecutive-losses* 5))
      (send-critical-discord-alert
       "LOSING STREAK"
       (format nil "~d consecutive losses. Strategy may need review." *consecutive-losses*)))))

;;; ==========================================
;;; LIVE STATUS S-EXPRESSION
;;; ==========================================

(defparameter *live-status-path* (swimmy.core::swimmy-path ".opus/live_status.sexp"))
(defparameter *live-status-interval* 60)
(defparameter *last-status-write* 0)

(defun save-live-status ()
  "Write live status to S-expression for Discord bot."
  (let ((now (get-universal-time)))
    (when (> (- now *last-status-write*) *live-status-interval*)
      (setf *last-status-write* now)
      (handler-case
          (progn
            (let* ((progress (get-goal-progress))
                   (ecosystem-health (if (fboundp 'calculate-ecosystem-health)
                                         (* 100 (funcall 'calculate-ecosystem-health))
                                         0))
                   (leader-name (if (and (boundp '*current-leader*) *current-leader*)
                                    (leader-info-strategy-name *current-leader*)
                                    "UNKNOWN"))
                   (regime-str (if (and (boundp '*current-regime*) *current-regime*)
                                   (symbol-name *current-regime*)
                                   "UNKNOWN"))
                   (volatility-str (if (and (boundp '*volatility-regime*) *volatility-regime*)
                                       (symbol-name *volatility-regime*)
                                       "UNKNOWN"))
                   (danger (if (boundp '*danger-level*) *danger-level* 0))
                   (payload `((schema_version . 2)
                              (daily_pnl . ,*daily-pnl*)
                              (accumulated_pnl . ,*accumulated-pnl*)
                              (monthly_goal . ,*monthly-goal*)
                              (goal_progress . ,(or (getf progress :progress-pct) 0))
                              (regime . ,regime-str)
                              (volatility . ,volatility-str)
                              (leader . ,leader-name)
                              (danger_level . ,danger)
                              (ecosystem_health . ,ecosystem-health)
                              (total_trades . ,*total-trades*)
                              (warmup_progress . ,(min 100 (* 2 *total-trades*)))
                              (warmup_complete . ,(>= *total-trades* 50))
                              (last_updated . ,(get-jst-str)))))
              (swimmy.core:write-sexp-atomic *live-status-path* payload)
              (swimmy.core::emit-telemetry-event "status.snapshot"
                :service "shell"
                :severity "info"
                :correlation-id (format nil "~a" (get-universal-time))
                :data (jsown:new-js
                        ("daily_pnl" *daily-pnl*)
                        ("accumulated_pnl" *accumulated-pnl*)
                        ("goal_progress" (or (getf progress :progress-pct) 0))
                        ("regime" regime-str)
                        ("volatility" volatility-str)
                        ("leader" leader-name)
                        ("danger_level" danger)
                        ("ecosystem_health" ecosystem-health)
                        ("total_trades" *total-trades*))))
            (format t "[SHELL] 📝 Live status saved~%"))
        (error (e)
          (format t "[SHELL] ⚠️ Failed to save live status: ~a~%" e))))))

(format t "[SHELL] notifications.lisp loaded~%")
