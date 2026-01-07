
(in-package :cl-user)
(require :asdf)
(asdf:load-system :swimmy)

(format t "Testing Recruitment Webhook...~%")
(format t "URL: ~a~%" swimmy.globals:*discord-recruit-webhook*)

(handler-case
    (progn
      (swimmy.core:notify-discord-recruit "🐟 Apex: Webhook Verification Test" :color 3066993)
      (format t "✅ Notification queued successfully.~%"))
  (error (e)
    (format t "❌ Error: ~a~%" e)))
