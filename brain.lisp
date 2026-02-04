;;; brain.lisp - Swimmy Entry Point
;;; =======================================================
;;; 🐟 SWIMMY Ver 41.5 (Fowler's Perfection)
;;; =======================================================
;;; Refactored to separate Definition (ASDF) from Execution.
;;; All logic moved to src/lisp/main.lisp

(in-package :cl-user)

(require :asdf)

;; Add current directory to ASDF registry so it finds swimmy.asd
(push (uiop:getcwd) asdf:*central-registry*)

;; Initialize Quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(format t "~%[LOADER] Loading Swimmy System via ASDF...~%")

;; Load the System
(handler-case
    (handler-bind ((style-warning #'muffle-warning)
                   (warning #'muffle-warning))
      (asdf:load-system :swimmy))
  (error (c)
    (format t "~%[FATAL] Failed to load system: ~a~%" c)
    (sb-ext:exit :code 1)))

;; Start
(uiop:symbol-call :swimmy.main :start-system)
