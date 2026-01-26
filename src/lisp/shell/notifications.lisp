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
  (let* ((emoji (if warmup-p "🔥" "🧠"))
         (dir-str (if (eq direction :LONG) "BUY" "SELL"))
         (suffix (if warmup-p " [WARMUP]" (format nil " (~,0f%)" (* conf 100)))))
    (notify-discord (format nil "~a ~a #~d~a" emoji dir-str idx suffix)
                    :color (if (eq direction :LONG) 3066993 15158332))))

(defun on-trade-closed (idx symbol won pnl)
  "Called when a trade is closed. Sends Discord notification."
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
;;; LIVE STATUS JSON
;;; ==========================================

(defparameter *live-status-path* (swimmy.core::swimmy-path ".opus/live_status.json"))
(defparameter *live-status-interval* 60)
(defparameter *last-status-write* 0)

(defun save-live-status ()
  "Write live status to JSON for Discord bot."
  (let ((now (get-universal-time)))
    (when (> (- now *last-status-write*) *live-status-interval*)
      (setf *last-status-write* now)
      (handler-case
          (progn
            (ensure-directories-exist *live-status-path*)
            (with-open-file (out *live-status-path* :direction :output :if-exists :supersede)
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
                     ;; Tribe status
                     (hunter-sig (and (boundp '*tribe-status*) (gethash :hunters *tribe-status*)))
                     (shaman-sig (and (boundp '*tribe-status*) (gethash :shamans *tribe-status*)))
                     (breaker-sig (and (boundp '*tribe-status*) (gethash :breakers *tribe-status*)))
                     (raider-sig (and (boundp '*tribe-status*) (gethash :raiders *tribe-status*))))
                (format out "{~%")
                (format out "  \"daily_pnl\": ~,2f,~%" *daily-pnl*)
                (format out "  \"accumulated_pnl\": ~,2f,~%" *accumulated-pnl*)
                (format out "  \"monthly_goal\": ~d,~%" *monthly-goal*)
                (format out "  \"goal_progress\": ~,2f,~%" (or (getf progress :progress-pct) 0))
                (format out "  \"regime\": \"~a\",~%" regime-str)
                (format out "  \"volatility\": \"~a\",~%" volatility-str)
                (format out "  \"leader\": \"~a\",~%" leader-name)
                (format out "  \"danger_level\": ~d,~%" danger)
                (format out "  \"ecosystem_health\": ~,0f,~%" ecosystem-health)
                (format out "  \"total_trades\": ~d,~%" *total-trades*)
                (format out "  \"warmup_progress\": ~d,~%" (min 100 (* 2 *total-trades*)))
                (format out "  \"warmup_complete\": ~a,~%" (if (>= *total-trades* 50) "true" "false"))
                ;; Tribe details
                (format out "  \"tribes\": {~%")
                (format out "    \"hunters\": {\"direction\": \"~a\", \"confidence\": ~,0f, \"reason\": \"MACD+ADX+Kalman\"},~%"
                        (if hunter-sig (getf hunter-sig :direction) :hold)
                        (* 100 (or (and hunter-sig (getf hunter-sig :confidence)) 0)))
                (format out "    \"shamans\": {\"direction\": \"~a\", \"confidence\": ~,0f, \"reason\": \"RSI+BB Mean Reversion\"},~%"
                        (if shaman-sig (getf shaman-sig :direction) :hold)
                        (* 100 (or (and shaman-sig (getf shaman-sig :confidence)) 0)))
                (format out "    \"breakers\": {\"direction\": \"~a\", \"confidence\": ~,0f, \"reason\": \"Bollinger Breakout\"},~%"
                        (if breaker-sig (getf breaker-sig :direction) :hold)
                        (* 100 (or (and breaker-sig (getf breaker-sig :confidence)) 0)))
                (format out "    \"raiders\": {\"direction\": \"~a\", \"confidence\": ~,0f, \"reason\": \"EMA Scalp+Kalman\"}~%"
                        (if raider-sig (getf raider-sig :direction) :hold)
                        (* 100 (or (and raider-sig (getf raider-sig :confidence)) 0)))
                (format out "  },~%")
                ;; Consensus
                (format out "  \"tribe_consensus\": {\"direction\": \"~a\", \"strength\": ~,0f},~%"
                        (if (boundp '*tribe-direction*) (or *tribe-direction* :hold) :hold)
                        (* 100 (if (boundp '*tribe-consensus*) (or *tribe-consensus* 0) 0)))
                (format out "  \"last_updated\": \"~a\"~%" (get-jst-str))
                (format out "}~%")))
            (format t "[SHELL] 📝 Live status saved~%"))
        (error (e)
          (format t "[SHELL] ⚠️ Failed to save live status: ~a~%" e))))))

(format t "[SHELL] notifications.lisp loaded~%")
