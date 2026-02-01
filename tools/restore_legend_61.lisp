(in-package :cl-user)
(require :asdf)

;; Quiet startup and fail fast
(sb-ext:disable-debugger)
(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore h))
        (format *error-output* "[LEGENDS-61] 💥 Fatal error: ~a~%" c)
        (sb-ext:exit :code 1)))

;; Ensure project root is on ASDF path
(push (uiop:getcwd) asdf:*central-registry*)
(let ((ql-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file ql-init) (load ql-init)))

(format t "[LEGENDS-61] Loading Swimmy system for restoration...~%")
(handler-bind ((style-warning #'muffle-warning))
  (asdf:load-system :swimmy))

(let ((*package* (find-package :swimmy.school)))
  (format t "[LEGENDS-61] Restoring 61 legends...~%")
  (when (fboundp 'restore-legend-61)
    (restore-legend-61))
  ;; Flag-only queue to avoid premature BACKTEST dispatch before daemon wiring
  (when (fboundp 'queue-legend-revalidation)
    (queue-legend-revalidation :send-requests nil)))

(format t "[LEGENDS-61] Done.~%")
(sb-ext:exit :code 0)
