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

;;; ==========================================
;;; SYSTEM SUMMARY (Expert Panel P0 - 20260110)
;;; ==========================================

(defun get-system-summary ()
  "Generate system status summary for heartbeat notification.
   Includes: TICK time, strategy counts, equity."
  (let* ((now (get-universal-time))
         ;; Last TICK time (from globals)
         (last-tick (if (boundp 'swimmy.globals::*last-guardian-heartbeat*)
                        swimmy.globals::*last-guardian-heartbeat* 0))
         (tick-age (- now last-tick))
         (tick-status (cond
                        ((= last-tick 0) "❓ No data")
                        ((< tick-age 60) "🟢 LIVE")
                        ((< tick-age 300) "🟡 Delayed")
                        (t "🔴 OFFLINE")))
         ;; Strategy counts
         (active-count (if (boundp 'swimmy.globals::*evolved-strategies*)
                           (length swimmy.globals::*evolved-strategies*) 0))
         (kb-count (if (boundp 'swimmy.globals::*strategy-knowledge-base*)
                       (length swimmy.globals::*strategy-knowledge-base*) 0))
         ;; Single Source of Truth (Hickey Cleanup)
         (benched-count (if (fboundp 'swimmy.school::count-benched-strategies)
                            (swimmy.school:count-benched-strategies)
                            0))
         ;; Equity
         (equity (if (boundp 'swimmy.globals::*current-equity*)
                     swimmy.globals::*current-equity* 0))
         (daily-pnl (if (boundp 'swimmy.globals::*daily-pnl*)
                        swimmy.globals::*daily-pnl* 0)))
    (format nil "📊 **Status**
MT5: ~a (~d秒前)
💼 Equity: ¥~,0f | Today: ~a¥~,0f
📈 Strategies: ~d active + ~d KB | 🪑 ~d benched"
            tick-status tick-age
            equity (if (>= daily-pnl 0) "+" "") daily-pnl
            active-count kb-count benched-count)))

;; V8.6: Using shared config from core/config.lisp (Environment Variables)
;; (defparameter *heartbeat-webhook-url* ... removed to avoid shadowing)

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
           ;; V15.2: Dynamic lookup to avoid load-order issues
           (webhook (or (and (boundp 'swimmy.core::*heartbeat-webhook-url*)
                             swimmy.core::*heartbeat-webhook-url*)
                        (and (fboundp 'swimmy.core::get-discord-webhook)
                             (swimmy.core::get-discord-webhook "alerts"))
                        (uiop:getenv "SWIMMY_DISCORD_ALERTS"))))
      (if webhook
          (handler-case
              (progn
                ;; Use centralized ZMQ notifier (Async)
                (swimmy.core:queue-discord-notification 
                  webhook message 
                  :title "💓 Heartbeat" 
                  :color 3066993)
                (setf *last-heartbeat-sent* (get-universal-time))
                (format t "[HEARTBEAT] 💓 Queued to Discord (ZMQ)~%"))
            (error (e)
              (format t "[HEARTBEAT] ❌ Failed: ~a~%" e)))
          (format t "[HEARTBEAT] ⚠️ No webhook URL configured!~%")))))

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
