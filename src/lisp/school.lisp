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

;; V48: Defined locally to prevent UNDEFINED-FUNCTION error
(defun apply-optimized-params ()
  "Apply optimized parameters from *optimized-params* to strategies."
  (format t "[EVOLUTION] 🧬 Applying Evolutionary Genes (Code-as-Data)...~%")
  (when (boundp '*optimized-params*)
    (dolist (params *optimized-params*)
      (let* ((name (getf params :name))
             (strat (find name *strategy-knowledge-base* :key #'strategy-name :test #'string=)))
        (when strat
          (setf (strategy-timeframe strat) (getf params :timeframe))
          (setf (strategy-sl strat) (getf params :sl))
          (setf (strategy-tp strat) (getf params :tp)))))))

(defun sanitize-strategies ()
  "Repair strategies with malformed indicators (strings instead of lists) to prevent crashes."
  (let ((repaired-count 0))
    (dolist (s *strategy-knowledge-base*)
      (let ((dirty nil)
            (new-inds nil))
        (dolist (ind (strategy-indicators s))
          (if (listp ind)
              (push ind new-inds)
              (progn
                ;; Convert string/symbol "SMA" -> (:SMA)
                (let ((sym (if (stringp ind) (intern (string-upcase ind) :keyword) 
                               (if (symbolp ind) (intern (symbol-name ind) :keyword) :sma))))
                  (push (list sym) new-inds)
                  (setf dirty t)))))
        (when dirty
          (setf (strategy-indicators s) (nreverse new-inds))
          (incf repaired-count))))
    (when (> repaired-count 0)
      (format t "[SCHOOL] 🔧 Sanitized KB: Repaired ~d strategies with legacy indicators~%" repaired-count))))

(defun init-school ()
  ;; V9.2: Taleb's Antifragility (Safe Load)
  (safely-load-hunter-strategies)
  (sanitize-strategies) ; V48: Clean bad data immediately after load
  (load-strategy-ranks) ; V46.1: Load Rank DB
  (apply-optimized-params) ; Phase 4: Apply Evolutionary Genes

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
