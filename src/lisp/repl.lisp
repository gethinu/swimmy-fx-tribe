;;; repl.lisp - Interactive REPL Interface for Swimmy
;;; ═══════════════════════════════════════════════════

(in-package :swimmy.shell)
;;; "The best debugging tool ever invented is the REPL."
;;;  - Rich Hickey (though Paul Graham would agree)

(defpackage :swimmy-repl
  (:use :cl)
  (:export :swimmy :help))

(in-package :swimmy-repl)

;;; ─────────────────────────────────────────
;;; CORE REPL COMMAND: swimmy
;;; ─────────────────────────────────────────

(defun swimmy (&rest args)
  "Interactive Swimmy control. Usage:
   (swimmy :status)           - Get current status
   (swimmy :elders)           - List Hall of Fame
   (swimmy :reputation name)  - Get strategy reputation
   (swimmy :patterns)         - Show detected patterns
   (swimmy :constitution)     - Show Constitution values
   (swimmy :goal)             - Show goal progress
   (swimmy :test name)        - Backtest a strategy"
  (let ((cmd (first args)))
    (case cmd
      (:help (print-help))
      (:status (print-status))
      (:elders (print-elders))
      (:reputation (print-reputation (second args)))
      (:patterns (print-patterns))
      (:constitution (print-constitution))
      (:goal (print-goal))
      (:test (run-backtest (second args)))
      (otherwise (print-help)))))

;;; ─────────────────────────────────────────
;;; STATUS
;;; ─────────────────────────────────────────

(defun print-status ()
  "Print current Swimmy status"
  (format t "~%═══════════════════════════════════════~%")
  (format t "🦈 SWIMMY STATUS~%")
  (format t "═══════════════════════════════════════~%")
  (format t "📊 Daily PnL: ¥~,2f~%" (or (and (boundp 'cl-user::*daily-pnl*) cl-user::*daily-pnl*) 0))
  (format t "💰 Total PnL: ¥~,2f~%" (or (and (boundp 'cl-user::*accumulated-pnl*) cl-user::*accumulated-pnl*) 0))
  (format t "🎯 Goal: ~,1f%~%" (* 100 (/ (or (and (boundp 'cl-user::*accumulated-pnl*) cl-user::*accumulated-pnl*) 0) 
                                          (max 1 (or (and (boundp 'cl-user::*monthly-goal*) cl-user::*monthly-goal*) 100000)))))
  (format t "📈 Regime: ~a~%" (or (and (boundp 'cl-user::*current-regime*) cl-user::*current-regime*) "UNKNOWN"))
  (format t "⚡ Volatility: ~a~%" (or (and (boundp 'cl-user::*current-volatility-state*) cl-user::*current-volatility-state*) "UNKNOWN"))
  (format t "⚠️  Danger: ~d~%" (or (and (boundp 'cl-user::*danger-level*) cl-user::*danger-level*) 0))
  (format t "═══════════════════════════════════════~%~%"))


;;; ─────────────────────────────────────────
;;; ELDERS
;;; ─────────────────────────────────────────

(defun print-elders ()
  "Print Hall of Fame"
  (format t "~%═══════════════════════════════════════~%")
  (format t "🏛️ HALL OF FAME - The Elders~%")
  (format t "═══════════════════════════════════════~%")
  (if (and (boundp 'cl-user::*hall-of-fame*) cl-user::*hall-of-fame*)
      (dolist (elder cl-user::*hall-of-fame*)
        (format t "👑 ~a~%" (cl-user::elder-name elder))
        (format t "   Peak PnL: ¥~:d~%" (round (cl-user::elder-peak-pnl elder)))
        (format t "   Wisdom: \"~a\"~%" (cl-user::elder-wisdom elder)))
      (format t "No elders yet. Great strategies will be inducted here.~%"))
  (format t "═══════════════════════════════════════~%~%"))

;;; ─────────────────────────────────────────
;;; CONSTITUTION
;;; ─────────────────────────────────────────

(defun print-constitution ()
  "Print Constitution values"
  (format t "~%═══════════════════════════════════════~%")
  (format t "📜 THE CONSTITUTION~%")
  (format t "═══════════════════════════════════════~%")
  (format t "「勝とうとするな。ただ、生き残れ。」~%~%")
  (when (and (boundp 'cl-user::*constitution*) cl-user::*constitution*)
    (dolist (value cl-user::*constitution*)
      (format t "~d. ~a: ~a~%"
              (cl-user::core-value-priority value)
              (cl-user::core-value-name value)
              (cl-user::core-value-description value))))
  (format t "═══════════════════════════════════════~%~%"))

;;; ─────────────────────────────────────────
;;; PATTERNS
;;; ─────────────────────────────────────────

(defun print-patterns ()
  "Print tribal dialect patterns"
  (format t "~%═══════════════════════════════════════~%")
  (format t "🗣️ TRIBAL DIALECT - Patterns~%")
  (format t "═══════════════════════════════════════~%")
  (let* ((dialect-sym 'swimmy.core::*tribal-dialect*)
         (dialect (and (boundp dialect-sym)
                       (symbol-value dialect-sym))))
    (if (hash-table-p dialect)
        (maphash (lambda (name info)
                   (format t "~a: ~a~%"
                           name (getf info :description)))
                 dialect)
        (format t "(no active patterns)~%")))
  (format t "═══════════════════════════════════════~%~%"))

;;; ─────────────────────────────────────────
;;; GOAL
;;; ─────────────────────────────────────────

(defun print-goal ()
  "Print goal progress"
  (let ((goal (or (and (boundp 'cl-user::*monthly-goal*) cl-user::*monthly-goal*) 100000))
        (current (or (and (boundp 'cl-user::*accumulated-pnl*) cl-user::*accumulated-pnl*) 0)))
    (format t "~%═══════════════════════════════════════~%")
    (format t "🎯 GOAL PROGRESS~%")
    (format t "═══════════════════════════════════════~%")
    (format t "Monthly Target: ¥~:d~%" goal)
    (format t "Current:        ¥~:d~%" (round current))
    (format t "Progress:       ~,1f%~%" (* 100 (/ current (max 1 goal))))
    (format t "Remaining:      ¥~:d~%" (round (- goal current)))
    (format t "═══════════════════════════════════════~%~%")))

;;; ─────────────────────────────────────────
;;; BACKTEST
;;; ─────────────────────────────────────────

(defun run-backtest (name)
  "Run backtest for a strategy"
  (format t "~%Running backtest for ~a...~%" name)
  (format t "(This would call the backtest function)~%"))

;;; ─────────────────────────────────────────
;;; REPUTATION
;;; ─────────────────────────────────────────

(defun print-reputation (name)
  "Print reputation for strategy"
  (if (and (boundp 'swimmy.core::*reputation-scores*) 
           swimmy.core::*reputation-scores*
           name)
      (let ((rep (gethash name swimmy.core::*reputation-scores*)))
        (if rep
            (progn
              (format t "~%═══════════════════════════════════════~%")
              (format t "📊 REPUTATION: ~a~%" name)
              (format t "═══════════════════════════════════════~%")
              (format t "Trust:       ~,0f%~%" (* 100 (cl-user::reputation-trust-score rep)))
              (format t "Profit:      ¥~:d~%" (round (cl-user::reputation-profit-score rep)))
              (format t "Reliability: ~,0f%~%" (* 100 (cl-user::reputation-reliability rep)))
              (format t "═══════════════════════════════════════~%~%"))
            (format t "No reputation found for ~a~%" name)))
      (format t "Usage: (swimmy :reputation \"Strategy-Name\")~%")))

;;; ─────────────────────────────────────────
;;; HELP
;;; ─────────────────────────────────────────

(defun print-help ()
  "Print help"
  (format t "~%═══════════════════════════════════════~%")
  (format t "🦈 SWIMMY REPL COMMANDS~%")
  (format t "═══════════════════════════════════════~%")
  (format t "(swimmy :status)          - Current status~%")
  (format t "(swimmy :elders)          - Hall of Fame~%")
  (format t "(swimmy :constitution)    - Show values~%")
  (format t "(swimmy :patterns)        - Tribal patterns~%")
  (format t "(swimmy :goal)            - Goal progress~%")
  (format t "(swimmy :reputation name) - Strategy rep~%")
  (format t "(swimmy :test name)       - Backtest~%")
  (format t "═══════════════════════════════════════~%~%"))

(format t "[REPL] Swimmy REPL interface loaded~%")
(format t "[REPL] Type (swimmy :help) for commands~%")
