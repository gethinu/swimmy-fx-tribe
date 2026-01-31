;; tools/run_lisp_evolution.lisp
(in-package :cl-user)
(require :asdf)

;; EXPERT PANEL 2 (2026-01-30): "Let It Crash" Policy
;; Disable interactive debugger to prevent daemon hangs.
(sb-ext:disable-debugger)
(setf *debugger-hook* 
      (lambda (condition hook)
        (declare (ignore hook))
        (format *error-output* "~%[EVO-CRASH] 💥 Unhandled Error: ~a~%" condition)
        (sb-ext:exit :code 1)))

(push (uiop:getcwd) asdf:*central-registry*)
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init) (load quicklisp-init)))

(format t "[EVO] Loading Swimmy System...~%")
(handler-bind ((style-warning #'muffle-warning)
               (warning #'muffle-warning))
  (asdf:load-system :swimmy))

;; Unified Bootstrap with Package Safety (Phase 39 Resilience Fix)
(let ((*package* (find-package :swimmy.school)))
  (format t "[EVO] Initializing System (Pkg: ~a)...~%" *package*)
  (swimmy.main::initialize-system)
  
  (format t "[EVO] Starting Native Evolution Service...~%")
  (swimmy.school:start-evolution-service))
