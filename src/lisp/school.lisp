;;; school.lisp - Swimmy School: Team-based Portfolio Management
;;; V6.15: Refactored (Expert Panel 2026-01-13) - SRP Compliant
;;; Logic moved to:
;;; - school-market.lisp (Regime/Vol)
;;; - school-risk.lisp (Correlation/Pruning)
;;; - school-execution.lisp (Trade Logic)
;;; - school-narrative.lisp (Discord)

(in-package :swimmy.school)

;;; ==========================================
;;; SYSTEM ORCHESTRATION
;;; ==========================================

(defun init-school ()
  ;; V9.2: Taleb's Antifragility (Safe Load)
  (safely-load-hunter-strategies)

  ;; V8.7: Reclassify ALL strategies (KB + Evolved) to fix category bugs
  (build-category-pools) ; Clears pools and adds *strategy-knowledge-base*
  
  ;; Add Evolved/Recruited strategies to pools with NEW categorization logic
  (when (boundp '*evolved-strategies*)
    (dolist (strat *evolved-strategies*)
      (let ((cat (categorize-strategy strat)))
        (setf (strategy-category strat) cat) ; Update the slot permanently
        (push strat (gethash cat *category-pools* nil)))))
        
  (recruit-special-forces) ; V7.0: Inject special forces
  (clrhash *category-positions*)
  (format t "[SCHOOL] Swimmy School ready (Strategies Reclassified & Pools Built)~%"))

(format t "[SCHOOL] 🦅 Refactored School System Loaded (Musk/Uncle Bob Edition)~%")
