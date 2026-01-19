;;; src/lisp/school/school-daemon.lisp
;;; ============================================================================
;;; SWIMMY SCHOOL DAEMON (Systemd Entry Point)
;;; ============================================================================
;;; This script is loaded by systemd (swimmy-school.service).
;;; It launches the standard Lisp evolution service loop.

(in-package :cl-user)

(load "swimmy.asd")
(ql:quickload :swimmy)

(format t "~%[DAEMON] 🏰 Swimmy School Daemon Initialized.~%")
(format t "[DAEMON] 🚀 Launching Pure Lisp Evolution Service...~%")

;; Start the service (Infinite Loop)
(handler-case
    (swimmy.school:start-evolution-service)
  (sb-sys:interactive-interrupt ()
    (format t "~%[DAEMON] 🛑 Service Interrupted by Signal.~%"))
  (error (e)
    (format t "~%[DAEMON] 💥 CRITICAL FAILURE: ~a~%" e)
    (sb-ext:exit :code 1)))
