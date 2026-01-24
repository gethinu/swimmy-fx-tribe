;; tools/test_all_notifications.lisp
;; Fires a test message to every configured notification channel

(require :asdf)
(require :uiop)

;; Load Quicklisp if available (for message separation/libraries if needed)
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :pzmq :silent t)
(ql:quickload :jsown :silent t)

(load "src/lisp/packages.lisp")
(load "src/lisp/core/config.lisp")
(load "src/lisp/core/discord.lisp")
(load "src/lisp/shell/notifications.lisp")

;; Initialize Webhooks from ENV (config.lisp does this, but ensure setup-symbol-webhooks is called)
;; Note: core/config.lisp loads ENV vars into *parameters* at load time.
;; shell/notifications.lisp defines setup-symbol-webhooks for the hash table.

(in-package :swimmy.core)

(format t "~%🧪 STARTING NOTIFICATION CHANNEL TEST (V2.0)~%")
(format t "=============================================~%")

;; Check Connection
(ensure-notifier-connection)
(sleep 1)

;; 1. APEX / GENERAL
(format t "[TEST] >> Sending to APEX/GENERAL...~%")
(notify-apex "🧪 TEST: Apex Channel (System Config Check)")
(sleep 0.5)

;; 2. DEFAULT FALLBACK
(format t "[TEST] >> Sending to DEFAULT/FALLBACK...~%")
(notify-discord "🧪 TEST: Default Fallback (Should match Apex)")
(sleep 0.5)

;; 3. HEARTBEAT
(format t "[TEST] >> Sending to HEARTBEAT...~%")
;; Fix: Heartbeat consolidated to ALERTS in config.lisp
(let ((webhook (get-discord-webhook "heartbeat")))
  (if webhook
      (queue-discord-notification webhook "🧪 TEST: Heartbeat Channel Checking In" :title "💓 Heartbeat Test")
      (format t "⚠️ Heartbeat Webhook NOT FOUND~%")))
(sleep 0.5)

;; 4. DAILY
(format t "[TEST] >> Sending to DAILY...~%")
(notify-discord-daily "🧪 TEST: Daily Report Channel")
(sleep 0.5)

;; 5. WEEKLY
(format t "[TEST] >> Sending to WEEKLY...~%")
(notify-discord-weekly "🧪 TEST: Weekly Report Channel")
(sleep 0.5)

;; 6. ALERTS
(format t "[TEST] >> Sending to ALERTS...~%")
(notify-discord-alert "🧪 TEST: Alerts Channel")
(sleep 0.5)

;; 7. EMERGENCY
(format t "[TEST] >> Sending to EMERGENCY...~%")
(notify-discord-emergency "🧪 TEST: Emergency Channel")
(sleep 0.5)

;; 8. BACKTEST
(format t "[TEST] >> Sending to BACKTEST...~%")
(notify-discord-backtest "🧪 TEST: Backtest Channel")
(sleep 0.5)

;; 9. RECRUIT
(format t "[TEST] >> Sending to RECRUIT...~%")
(notify-discord-recruit "🧪 TEST: Recruit Channel")
(sleep 0.5)

;; 10. JOURNAL
(format t "[TEST] >> Sending to JOURNAL...~%")
;; Fix: Journal consolidated to REPORTS in config.lisp
(let ((webhook (get-discord-webhook "journal")))
  (if webhook
      (queue-discord-notification webhook "🧪 TEST: Journal Channel" :title "📓 Journal Test")
      (format t "⚠️ Journal Webhook NOT FOUND~%")))
(sleep 0.5)

;; 11. SYMBOLS
(swimmy.shell::setup-symbol-webhooks) ;; Populate hash table

(format t "[TEST] >> Sending to USDJPY...~%")
(notify-discord-symbol "USDJPY" "🧪 TEST: USDJPY Channel")
(sleep 0.5)

(format t "[TEST] >> Sending to EURUSD...~%")
(notify-discord-symbol "EURUSD" "🧪 TEST: EURUSD Channel")
(sleep 0.5)

(format t "[TEST] >> Sending to GBPUSD...~%")
(notify-discord-symbol "GBPUSD" "🧪 TEST: GBPUSD Channel")
(sleep 0.5)

(format t "=============================================~%")
(format t "✅ TEST COMPLETE. Please verify Discord Channels.~%")
