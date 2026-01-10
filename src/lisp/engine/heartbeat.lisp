;;; ============================================================================
;;; engine/heartbeat.lisp - Mobile Monitoring (Naval's Feedback)
;;; ============================================================================
;;; "If you can't see it from your phone, it doesn't exist." - Naval
;;; Sends periodic heartbeat to Discord so owner knows system is alive.

(in-package :swimmy.engine)
;;; ============================================================================

;;; ==========================================
;;; HEARTBEAT PARAMETERS
;;; ==========================================

(defparameter *heartbeat-interval* 3600
  "Seconds between heartbeat notifications (default: 1 hour).")

(defparameter *last-heartbeat-sent* 0
  "Timestamp of last heartbeat sent to Discord.")

(defparameter *heartbeat-enabled* t
  "Enable/disable heartbeat notifications.")

(defparameter *heartbeat-webhook-url* 
  (or (and (fboundp 'swimmy.core::get-discord-webhook)
           (funcall 'swimmy.core::get-discord-webhook "heartbeat"))
      "https://discord.com/api/webhooks/1458820892623634686/Nv_POY_W0E_iD130bTQM1eDJyTJmU5ZweDOEOpMvEW6ZnEmMCSoconLlxqd5bUuug72k")
  "Heartbeat webhook - loaded from config/discord_webhooks.json")

;;; ==========================================
;;; HEARTBEAT LOGIC
;;; ==========================================

(defun get-system-summary ()
  "Create a detailed system status summary with monitoring metrics."
  (let* (;; Basic stats
         (pnl (if (boundp '*accumulated-pnl*) *accumulated-pnl* 0))
         (treasury (if (boundp '*locked-treasury*) *locked-treasury* 0))
         (trades (if (boundp '*daily-trade-count*) *daily-trade-count* 0))
         (uptime-min (floor (/ (- (get-universal-time) *last-heartbeat-sent*) 60)))
         ;; Strategy stats
         (active-count (if (and (boundp '*strategy-knowledge-base*) *strategy-knowledge-base*)
                           (length *strategy-knowledge-base*) 0))
         (benched-count (if (and (boundp '*benched-strategies*) (hash-table-p *benched-strategies*))
                            (hash-table-count *benched-strategies*) 0))
         ;; Last tick info
         (last-tick-ago (if (and (boundp '*last-tick-time*) (numberp *last-tick-time*) (> *last-tick-time* 0))
                            (floor (/ (- (get-universal-time) *last-tick-time*) 60))
                            -1)))
    (format nil "~{~a~^~%~}"
            (list
             (format nil "📊 PnL: ¥~:d | Treasury: ¥~:d" pnl treasury)
             (format nil "📈 Trades: ~d | Uptime: ~d min" trades uptime-min)
             (format nil "🎯 Strategies: ~d active / ~d benched" active-count benched-count)
             (if (>= last-tick-ago 0)
                 (format nil "📡 Last TICK: ~d min ago" last-tick-ago)
                 "📡 Last TICK: ⚠️ No data (market closed?)")))))

(defun send-discord-heartbeat ()
  "Send heartbeat notification to dedicated Discord channel."
  (when *heartbeat-enabled*
    (let* ((hour (nth 2 (multiple-value-list (get-decoded-time))))
           (emoji (cond
                   ((< hour 6) "🌙")
                   ((< hour 12) "🌅")
                   ((< hour 18) "☀️")
                   (t "🌆")))
           (message (format nil "~a **Swimmy Heartbeat** ~a~%~%~a~%~%_System is running normally._"
                           emoji
                           (get-time-string-full)
                           (get-system-summary)))
           ;; Escape quotes for JSON (Naval: no cl-ppcre needed)
           (safe-message (substitute #\' #\" message)))
      (handler-case
          (progn
            ;; Use dedicated heartbeat webhook
            (uiop:run-program 
              (list "curl" "-s" "-X" "POST" 
                    "-H" "Content-Type: application/json"
                    "-d" (format nil "{\"content\": \"~a\"}" safe-message)
                    *heartbeat-webhook-url*)
              :ignore-error-status t)
            (setf *last-heartbeat-sent* (get-universal-time))
            (format t "[HEARTBEAT] 💓 Sent to Discord~%"))
        (error (e)
          (format t "[HEARTBEAT] ❌ Failed: ~a~%" e))))))

(defun get-time-string-full ()
  "Get full timestamp string."
  (multiple-value-bind (sec min hour day month year)
      (get-decoded-time)
    (declare (ignore sec))
    (format nil "~d/~2,'0d/~2,'0d ~2,'0d:~2,'0d"
            year month day hour min)))

(defun check-discord-heartbeat ()
  "Check if it's time to send a heartbeat."
  (when *heartbeat-enabled*
    (let ((elapsed (- (get-universal-time) *last-heartbeat-sent*)))
      (when (>= elapsed *heartbeat-interval*)
        (send-discord-heartbeat)))))

;;; ==========================================
;;; IMMEDIATE HEARTBEAT (For testing)
;;; ==========================================

(defun heartbeat-now ()
  "Send heartbeat immediately (manual trigger)."
  (format t "[HEARTBEAT] 💓 Sending immediate heartbeat...~%")
  (send-discord-heartbeat))

(format t "[ENGINE] heartbeat.lisp loaded - Mobile monitoring active~%")

;; Initialize timestamp (first heartbeat will be sent after interval)
(setf *last-heartbeat-sent* (get-universal-time))
