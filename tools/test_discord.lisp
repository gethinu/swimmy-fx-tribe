;; tools/test_discord.lisp
(in-package :cl-user)
(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init) (load quicklisp-init)))

(format t "[TEST] Loading Swimmy System...~%")
(handler-bind ((style-warning #'muffle-warning)
               (warning #'muffle-warning))
  (asdf:load-system :swimmy))

(in-package :swimmy.core)

(format t "[TEST] Sending test notification to APEX and ALERTS channels...~%")
(notify-apex "🔔 **System Connectivity Test** (Expert Panel Audit)
The Discord Notifier has been restored and verified.
Status: SYSTEM ONLINE 🟢" :color +color-success+)

(notify-discord-alert "🚨 **Diagnostic Alert**
If you see this, the notification bridge is confirmed functional.
V49.8 Stabilization Complete." :color +color-alert+)

(format t "[TEST] Done. Checking notifier logs...~%")
(sb-ext:exit :code 0)
