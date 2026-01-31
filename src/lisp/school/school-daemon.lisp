;;; src/lisp/school/school-daemon.lisp
;;; ============================================================================
;;; SWIMMY SCHOOL DAEMON (Systemd Entry Point)
;;; ============================================================================
;;; This script is loaded by systemd (swimmy-school.service).
;;; It launches the standard Lisp evolution service loop.

(in-package :cl-user)

;; EXPERT PANEL 2: "Let It Crash"
;; If any error occurs during loading (quickload included), EXIT immediately.
(sb-ext:disable-debugger)
(setf *debugger-hook* 
      (lambda (condition hook)
        (declare (ignore hook))
        (format *error-output* "~%[DAEMON-CRASH] 💥 Unhandled Error: ~a~%" condition)
        (sb-ext:exit :code 1)))

(load "swimmy.asd")
(ql:quickload :swimmy)

(format t "~%[DAEMON] 🏰 Swimmy School Daemon Initialized.~%")
(format t "[DAEMON] 🚀 Launching Pure Lisp Evolution Service...~%")

;; Start the service (Infinite Loop)
(handler-case
    (progn
      ;; V51.0: Unified Bootstrap (Phase 39 Resilience Fix)
      ;; This ensures init-knowledge-base and all subsystems are loaded correctly.
      (swimmy.main::initialize-system)
      (swimmy.school:start-evolution-service))
  (sb-sys:interactive-interrupt ()
    (format t "~%[DAEMON] 🛑 Service Interrupted by Signal.~%"))
  (error (e)
    (format t "~%[DAEMON] 💥 CRITICAL FAILURE: ~a~%" e)
    (sb-ext:exit :code 1)))
