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
    (handler-case
        (dolist (params *optimized-params*)
          (let* ((name (getf params :name))
                 (strat (find name *strategy-knowledge-base* :key #'strategy-name :test #'string=)))
            (when strat
              (setf (strategy-timeframe strat) (getf params :timeframe))
              (setf (strategy-sl strat) (getf params :sl))
              (setf (strategy-tp strat) (getf params :tp)))))
      (error (e)
        (format t "[EVOLUTION] ⚠️ Error applying optimized params: ~a~%" e)))))

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
  (format t "[SCHOOL] ⏱️ init-school: safely-load-hunter-strategies start~%")
  (safely-load-hunter-strategies)
  (format t "[SCHOOL] ⏱️ init-school: safely-load-hunter-strategies done~%")
  (format t "[SCHOOL] ⏱️ init-school: sanitize-strategies start~%")
  (sanitize-strategies) ; V48: Clean bad data immediately after load
  (format t "[SCHOOL] ⏱️ init-school: sanitize-strategies done~%")
  (format t "[SCHOOL] ⏱️ init-school: load-strategy-ranks start~%")
  (load-strategy-ranks) ; V46.1: Load Rank DB
  (format t "[SCHOOL] ⏱️ init-school: load-strategy-ranks done~%")
  (format t "[SCHOOL] ⏱️ init-school: apply-optimized-params start~%")
  (apply-optimized-params) ; Phase 4: Apply Evolutionary Genes
  (format t "[SCHOOL] ⏱️ init-school: apply-optimized-params done~%")

  ;; V8.7: Reclassify ALL strategies (KB + Evolved) to fix category bugs
  (format t "[SCHOOL] ⏱️ init-school: build-category-pools start~%")
  (build-category-pools) ; Clears pools and adds *strategy-knowledge-base*
  (format t "[SCHOOL] ⏱️ init-school: build-category-pools done~%")
  
  (format t "[SCHOOL] ⏱️ init-school: merge evolved strategies start~%")
  ;; Add Evolved/Recruited strategies to pools with NEW categorization logic
  (when (boundp '*evolved-strategies*)
    (dolist (strat *evolved-strategies*)
      (let ((cat (categorize-strategy strat)))
        (push strat (gethash cat *category-pools* nil)))
      (when (boundp '*regime-pools*)
        (let ((regime-class (strategy-regime-class strat)))
          (push strat (gethash regime-class *regime-pools* nil))))))
  (format t "[SCHOOL] ⏱️ init-school: merge evolved strategies done~%")
        
  ;; V51.1: Defer auto-recruit on startup to avoid long boot stalls.
  ;; Operators can still trigger recruitment explicitly via RECRUIT_SPECIAL_FORCES.
  (if (and (boundp '*startup-mode*) *startup-mode*)
      (format t "[RECRUIT] ⏳ Startup mode: deferring auto recruit-special-forces.~%")
      (recruit-special-forces)) ; V7.0: Inject special forces
  (clrhash *category-positions*)
  
  ;; V51.1: Avoid blocking startup on heavy rank evaluation.
  ;; Let Brain bind ZMQ ports first, then periodic maintenance can evaluate.
  (format t "[SCHOOL] ⏱️ init-school: rank evaluation gate check~%")
  (when (fboundp 'run-rank-evaluation)
    (if (and (boundp '*startup-mode*) *startup-mode*)
        (format t "[RANK] ⏳ Startup mode: deferring rank evaluation.~%")
        (run-rank-evaluation)))
  
  ;; Phase 25: Start Scribe Service (I/O Isolation)
  (format t "[SCHOOL] ⏱️ init-school: start-scribe gate check~%")
  (when (fboundp 'swimmy.school.scribe:start-scribe)
    (swimmy.school.scribe:start-scribe))
  
  (format t "[SCHOOL] Swimmy School ready (Strategies Reclassified & Pools Built)~%"))

(format t "[SCHOOL] 🦅 Refactored School System Loaded (Musk/Uncle Bob Edition)~%")
