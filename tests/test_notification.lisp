(load "/home/swimmy/.sbclrc")
(ql:quickload :dexador)
(ql:quickload :jsown)
(ql:quickload :uiop)

;; SECURITY:
;;   Do NOT hardcode Discord webhook URLs in this repo.
;;   Discord webhook tokens are embedded in the URL.
;;   Use environment variables instead (see config/.env.template).

(defun test-notify (webhook title msg color)
  (format t "Sending: ~a...~%" title)
  (handler-case
      (dex:post webhook
                :content (jsown:to-json (jsown:new-js ("embeds" (list (jsown:new-js
                          ("title" title)
                          ("description" msg)
                          ("color" color))))))
                :headers '(("Content-Type" . "application/json")))
    (error (e) (format t "Error: ~a~%" e))))

(defun maybe-test-notify (webhook title msg color)
  (if (and webhook (> (length webhook) 0))
      (test-notify webhook title msg color)
      (format t "Skipping: ~a (webhook not set)~%" title)))

;; Webhooks from env
(defparameter *usdjpy-webhook* (uiop:getenv "SWIMMY_DISCORD_WEBHOOK_USDJPY"))
(defparameter *eurusd-webhook* (uiop:getenv "SWIMMY_DISCORD_WEBHOOK_EURUSD"))
(defparameter *gbpusd-webhook* (uiop:getenv "SWIMMY_DISCORD_WEBHOOK_GBPUSD"))
(defparameter *alerts-webhook*
  (or (uiop:getenv "SWIMMY_DISCORD_ALERTS")
      (uiop:getenv "SWIMMY_DISCORD_WEBHOOK")))

(format t "--- Starting Notification Test ---~%")

;; 1) Status Report Test
(let ((status-msg "TEST STATUS REPORT"))
  (maybe-test-notify *usdjpy-webhook* "USDJPY (TEST)" status-msg 10070709)
  (maybe-test-notify *eurusd-webhook* "EURUSD (TEST)" status-msg 10070709)
  (maybe-test-notify *gbpusd-webhook* "GBPUSD (TEST)" status-msg 10070709))

;; 2) Alert Test
(maybe-test-notify *alerts-webhook* "TEST ALERT" "This is a test notification." 15158332)

(format t "--- Test Complete ---~%")

