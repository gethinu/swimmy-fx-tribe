;; tools/broadcast_test.lisp
(in-package :cl-user)
(load "src/lisp/core/config.lisp")
(load "src/lisp/core/discord.lisp")

(in-package :swimmy.core)

(format t "[BROADCAST] Starting Webhook Audit...~%")

(defun test-key (key name)
  (let ((webhook (get-discord-webhook key)))
    (if webhook
        (progn
          (format t "  [~a] Sending to ~a...~%" name webhook)
          (queue-discord-notification webhook (format nil "📡 **Broadcast Audit: ~a**
Status: TEST MESSAGE
Timestamp: ~a" name (get-jst-timestamp)) :title "🐟 System Audit"))
        (format t "  [~a] ❌ WEBHOOK NOT FOUND FOR KEY: ~a~%" name key))))

(test-key "usdjpy" "LIVE_FEED_USDJPY")
(test-key "eurusd" "LIVE_FEED_EURUSD")
(test-key "gbpusd" "LIVE_FEED_GBPUSD")
(test-key "status" "SYSTEM_LOGS")
(test-key "daily" "REPORTS_DAILY")
(test-key "alerts" "URGENT_ALERTS")
(test-key "fallback" "FALLBACK")
(test-key "apex" "APEX")

(format t "[BROADCAST] Complete. Monitor logs/notifier.log.~%")
(sb-ext:exit)
