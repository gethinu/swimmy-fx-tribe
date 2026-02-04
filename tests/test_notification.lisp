(load "/home/swimmy/.sbclrc")
(ql:quickload :dexador)
(ql:quickload :jsown)

(defun test-notify (webhook title msg color)
  (format t "Sending to ~a...~%" title)
  (handler-case
      (dex:post webhook
                :content (jsown:to-json (jsown:new-js ("embeds" (list (jsown:new-js 
                          ("title" title) 
                          ("description" msg) 
                          ("color" color))))))
                :headers '(("Content-Type" . "application/json")))
    (error (e) (format t "Error: ~a~%" e))))

;; Webhooks from brain.lisp
(defparameter *usdjpy-webhook* "https://discord.com/api/webhooks/1455548858921652442/pxCwnTnnMVd8-X8LIO3NrwtxQ0T2dm31GiS-SaHAkqQ0AAR5G5ABVcfKKJ0awKnGhnLk")
(defparameter *eurusd-webhook* "https://discord.com/api/webhooks/1455549049540313189/lw9iSajiYjzogZIUEuymaUaOIePL8yT0ya-qc8Utpyr5nM6bAZv6l8ekYTdf0knRRKZa")
(defparameter *gbpusd-webhook* "https://discord.com/api/webhooks/1455558971367882762/gOf_SFW0JvQd7tX1CqSGZbtGMcOz5wcwAiVgPhvCzEp7QAkl1g8u1dNx9qhfXbt5lAyB")
(defparameter *alerts-webhook* "https://discord.com/api/webhooks/1455549266301812849/r5Rv8rQrwgVsppGS0qIDJPNyz2KphVIzwshu6vTPABC-E6OSFrS89tZ9xAGQJEzmRqBH")

(format t "--- Starting Notification Test ---~%")

;; 1. Status Report Test (Mocking send-periodic-status-report)
(let ((status-msg "🕒 TEST STATUS REPORT\nPrice: 150.000\n🏛️ Tribes: HOLD (30%)\n🐟 Swarm: 60%\n⚔️ Warriors: 1"))
  (test-notify *usdjpy-webhook* "🐟 USDJPY (TEST)" status-msg 10070709)
  (test-notify *eurusd-webhook* "🐟 EURUSD (TEST)" status-msg 10070709)
  (test-notify *gbpusd-webhook* "🐟 GBPUSD (TEST)" status-msg 10070709))

;; 2. Alert Test
(test-notify *alerts-webhook* "🚨 TEST ALERT" "This is a test notification for swimmy-alerts.\nUsed for:\n- FLEE mode activation\n- Danger Level > 4\n- System Errors" 15158332)

(format t "--- Test Complete ---~%")
