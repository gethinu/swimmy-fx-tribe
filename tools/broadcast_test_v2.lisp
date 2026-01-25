;; tools/broadcast_test_v2.lisp
(in-package :cl-user)
(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init) (load quicklisp-init)))

(format t "[BROADCAST] Loading Swimmy System...~%")
(handler-bind ((style-warning #'muffle-warning)
               (warning #'muffle-warning))
  (asdf:load-system :swimmy))

(in-package :swimmy.core)

(format t "[BROADCAST] Starting Webhook Audit (v2)...~%")

(defun test-key (key name)
  (let ((webhook (get-discord-webhook key)))
    (if (and webhook (> (length webhook) 0))
        (progn
          (format t "  [~a] Sending to ~a...~%" name webhook)
          (queue-discord-notification webhook (format nil "📡 **Broadcast Audit: ~a**
Status: TEST MESSAGE
Timestamp: ~a" name (get-jst-timestamp)) :title "🐟 System Audit"))
        (format t "  [~a] ❌ WEBHOOK INVALID FOR KEY: ~a (~s)~%" name key webhook))))

(test-key "usdjpy" "LIVE_FEED_USDJPY")
(test-key "eurusd" "LIVE_FEED_EURUSD")
(test-key "gbpusd" "LIVE_FEED_GBPUSD")
(test-key "status" "SYSTEM_LOGS")
(test-key "daily" "REPORTS_DAILY")
(test-key "alerts" "URGENT_ALERTS")
(test-key "fallback" "FALLBACK")
(test-key "apex" "APEX")

(format t "[BROADCAST] Complete. Monitor logs/notifier.log for 404 errors.~%")
(sb-ext:exit :code 0)
