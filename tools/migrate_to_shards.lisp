
(require :sb-posix)
;; Ensure Quicklisp
(unless (find-package :ql)
  (let ((ql-setup "~/quicklisp/setup.lisp"))
    (if (probe-file ql-setup)
        (load ql-setup)
        (format t "⚠️ Quicklisp not found at ~a~%" ql-setup))))

;; Load Dependencies
(ql:quickload :cl-json)
(ql:quickload :cffi)
(ql:quickload :pzmq)
(ql:quickload :jsown)
(format t "Packages: ~a~%" (list-all-packages))

;; Load Phase 1: Packages & Globals
(format t "[LOAD] Loading packages.lisp...~%")
(load "src/lisp/packages.lisp")

(format t "[LOAD] Loading packages-school.lisp...~%")
(load "src/lisp/packages-school.lisp")

(format t "[LOAD] Loading core/globals.lisp...~%")
(load "src/lisp/core/globals.lisp")

(format t "[LOAD] Loading dsl.lisp...~%")
(load "src/lisp/dsl.lisp")

(unless (find-package :swimmy.school)
  (error "CRITICAL: Package :swimmy.school NOT created after loading packages-school.lisp"))

;; Load Phase 2: State & Structs
(format t "[LOAD] Loading school-state.lisp...~%")
(load "src/lisp/school/school-state.lisp")

(format t "[LOAD] Loading school-strategy.lisp...~%")
(load "src/lisp/school/school-strategy.lisp")

;; Load Phase 3: Strategy Definitions
(format t "[LOAD] Loading strategy sub-files...~%")
(load "src/lisp/strategies/strategies-trend.lisp")
(load "src/lisp/strategies/strategies-reversion.lisp")
(load "src/lisp/strategies/strategies-breakout.lisp")
(load "src/lisp/strategies/strategies-scalp.lisp")

(format t "[LOAD] Loading strategies.lisp...~%")
(load "src/lisp/strategies/strategies.lisp")

(format t "[LOAD] Loading persistence.lisp...~%")
(load "src/lisp/core/persistence.lisp")


(defpackage :swimmy.tools.migrate-shards
  (:use :cl :swimmy.school :swimmy.persistence))

(in-package :swimmy.tools.migrate-shards)

(defun migrate-strategies-to-shards ()
  (format t "[MIGRATE] STARTING MIGRATION TO SHARDED LIBRARY~%")
  
  ;; 1. Load Legacy Params (School Optimized)
  (load "src/lisp/school/school-optimized-params.lisp")
  (if (find-symbol "APPLY-OPTIMIZED-PARAMS" :swimmy.school)
      (funcall (find-symbol "APPLY-OPTIMIZED-PARAMS" :swimmy.school))
      (format t "[WARN] apply-optimized-params not found!~%"))


  (format t "[MIGRATE] Strategies loaded: ~D~%" (length swimmy.school::*strategy-knowledge-base*))
  
  (let ((count 0)
        (total (length swimmy.school::*strategy-knowledge-base*)))
    
    (dolist (strat swimmy.school::*strategy-knowledge-base*)
      (incf count)
      ;; Use the new persistence DAO to save
      (swimmy.persistence:save-strategy strat)
      
      (when (zerop (mod count 50))
        (format t "[MIGRATE] Sharded ~d/~d strategies...~%" count total)))
      
    (format t "[MIGRATE] COMPLETED. Sharded ~d strategies into data/library/.~%" count)))

(migrate-strategies-to-shards)
(sb-ext:exit)
