#!/usr/bin/env sbcl --script
;;; reset_sharpe.lisp - Reset all strategy Sharpe values to nil for fresh recalculation
;;; Usage: sbcl --script reset_sharpe.lisp

(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(asdf:load-system :swimmy :verbose nil)

(format t "~%┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓~%")
(format t "┃  🔄 SHARPE RESET UTILITY                    ┃~%")
(format t "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛~%~%")

;; Reset Knowledge Base strategies
(let ((kb-count 0))
  (dolist (s swimmy.globals:*strategy-knowledge-base*)
    (when s
      (setf (swimmy.school:strategy-sharpe s) nil)
      (incf kb-count)))
  (format t "✅ Reset ~d Knowledge Base strategies~%" kb-count))

;; Reset Evolved strategies  
(let ((ev-count 0))
  (dolist (s swimmy.globals:*evolved-strategies*)
    (when s
      (setf (swimmy.school:strategy-sharpe s) nil)
      (incf ev-count)))
  (format t "✅ Reset ~d Evolved strategies~%~%" ev-count))

(format t "🎯 All Sharpe values cleared. Run batch backtest to recalculate.~%")
(format t "   Next 'make run' will trigger fresh backtest with fixed algorithm.~%~%")

(sb-ext:exit :code 0)
