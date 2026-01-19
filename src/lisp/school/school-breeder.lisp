;;; src/lisp/school/school-breeder.lisp
;;; ============================================================================
;;; STRATEGY BREEDING LAB (Evolution Engine)
;;; ============================================================================
;;; Implements "Breeding" as requested by Naval/Musk.
;;; Takes fittest strategies and generates offspring for the Incubator.
;;; ============================================================================

(in-package :swimmy.school)

(defun random-range (min max)
  "Return random number between min and max."
  (+ min (random (- max min))))

(defun mutate-value (val factor)
  "Mutate a numeric value by +/- factor."
  (if (numberp val)
      (let ((delta (* val factor (random-range -1.0 1.0))))
        (+ val delta))
      val))

(defun crossover-indicators (p1 p2)
  "Combine indicators from parent 1 and parent 2."
  (let ((i1 (strategy-indicators p1))
        (i2 (strategy-indicators p2)))
    ;; Simple logic: Take half from each, randomly
    (remove-duplicates (append (subseq i1 0 (ceiling (/ (length i1) 2)))
                               (subseq i2 0 (ceiling (/ (length i2) 2))))
                       :test #'equal)))

(defun breed-strategies (parent1 parent2)
  "Create a child strategy from two parents."
  (let* ((child-name (format nil "Bred-~a-~a-Gen~d" 
                             (subseq (strategy-name parent1) 0 (min 5 (length (strategy-name parent1))))
                             (random 1000)
                             (1+ (max (strategy-generation parent1) (strategy-generation parent2)))))
         (child-sl (mutate-value (/ (+ (strategy-sl parent1) (strategy-sl parent2)) 2.0) 0.1))
         (child-tp (mutate-value (/ (+ (strategy-tp parent1) (strategy-tp parent2)) 2.0) 0.1))
         (child-is (crossover-indicators parent1 parent2)))
    
    (make-strategy
      :name child-name
      :category (strategy-category parent1) ;; Inherit from P1
      :timeframe (strategy-timeframe parent1)
      :generation (1+ (max (strategy-generation parent1) (strategy-generation parent2)))
      :sl child-sl
      :tp child-tp
      :volume 0.01
      :indicators child-is
      ;; For now, inherit P1 entry/exit logic directly, but mutation should happen here too eventually
      :entry (strategy-entry parent1)
      :exit (strategy-exit parent1)
      :tier :incubator ;; Born in the Incubator
      :status :active)))

(defun run-breeding-cycle ()
  "Breed top strategies from Battlefield, Training, and Selection tiers."
  (format t "[BREEDER] 🧬 Starting Breeding Cycle (Battlefield, Training, Selection)...~%")
  (let ((categories '(:trend :reversion :breakout :scalp))
        (tiers '(:battlefield :training :selection)))
        
    (dolist (cat categories)
      (dolist (tier tiers)
        (let* ((warriors (get-strategies-by-tier tier cat))
               (sorted (sort (copy-list warriors) #'> :key (lambda (s) (or (strategy-sharpe s) 0)))))
               
          (when (>= (length sorted) 2)
            (let ((p1 (first sorted))
                  (p2 (second sorted)))
              (format t "[BREEDER] 💕 Breeding [~a] ~a + ~a~%" tier (strategy-name p1) (strategy-name p2))
              (let ((child (breed-strategies p1 p2)))
                (push child *strategy-knowledge-base*)
                (save-recruit-to-lisp child) ;; Persist to disk
                (format t "[BREEDER] 👶 Born: ~a (Tier: Incubator)~%" (strategy-name child))))))))))

;;; ----------------------------------------------------------------------------
;;; Phase 6b: Persistence Implementation
;;; ----------------------------------------------------------------------------

(defun save-recruit-to-lisp (strat)
  "Save the new strategy to The Great Library (Sharded Persistence)."
  (swimmy.persistence:save-strategy strat)
  (format t "[PERSIST] 💾 Saved recruited strategy ~a to Library~%" (strategy-name strat)))

;;; ----------------------------------------------------------------------------
;;; Phase 3.7: The Entrance Exam (Recruitment Filter)
;;; ----------------------------------------------------------------------------

(defun recruit-elite-strategy ()
  "Recruit a new strategy using Lisp-Native Scout (Phase 11).
   Directly calls school-scout:recruit-scout."
  (format t "[RECRUIT] ⚔️ Starting Lisp-Native Recruitment (Phase 11)...~%")
  (handler-case
      (let ((count (recruit-scout)))
        (if (> count 0)
            (format t "[RECRUIT] 🎉 Successfully recruited ~d strategies!~%" count)
            (format t "[RECRUIT] ❌ No suitable recruits found this cycle.~%")))
    (error (e)
      (format t "[RECRUIT] 💥 Error during recruitment: ~a~%" e))))

;;; ----------------------------------------------------------------------------
;;; Phase 13: Wisdom Native (Civilization Handover)
;;; ----------------------------------------------------------------------------

(defun extract-params-from-strategy (strat)
  "Convert a strategy struct into an optimized-param plist."
  (list :name (strategy-name strat)
        :timeframe (strategy-timeframe strat)
        :sl (strategy-sl strat)
        :tp (strategy-tp strat)
        ;; Extract Indicators (Reverse engineering the strings)
        ;; This is a simplification: We assume SMA-S and SMA-L format for now.
        :indicators (strategy-indicators strat)))
        ;; TODO: Parse indicators more robustly if needed for gene regeneration

(defun save-optimized-params-to-file (params-list)
  "Save the optimized parameters to school-optimized-params.lisp."
  (let ((filepath "src/lisp/school/school-optimized-params.lisp"))
    (with-open-file (stream filepath
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format stream ";;; school-optimized-params.lisp~%")
      (format stream ";;; GENERATED BY LISP WISDOM ENGINE (analyze-veterans)~%")
      (format stream ";;; DO NOT EDIT MANUALLY.~%")
      (format stream ";;; Generated at: ~a~%~%" (swimmy.shell:get-date-string))
      (format stream "(in-package :swimmy.school)~%~%")
      (format stream "(defparameter *optimized-params*~%")
      (format stream "  '(~%")
      (dolist (p params-list)
        (format stream "    ~s~%" p))
      (format stream "   ))~%"))
    (format t "[WISDOM] 💾 Saved ~d veteran genes to ~a~%" (length params-list) filepath)))

(defun analyze-veterans ()
  "Analyze the Knowledge Base and extract 'Wisdom' (Best Genes).
   Replaces extract_wisdom.py."
  (format t "[WISDOM] 🧠 Analyzing Veterans for Gene Extraction...~%")
  (let* ((all-strats (append *strategy-knowledge-base* 
                             (get-strategies-by-tier :battlefield :trend) 
                             (get-strategies-by-tier :battlefield :reversion))) 
                             ;; Note: get-strategies-by-tier might duplicates KB if KB holds everything.
                             ;; *strategy-knowledge-base* usually holds ALL.
         (unique-strats (remove-duplicates all-strats :key #'strategy-name :test #'string=))
         ;; Filter: Positive Sharpe only, or just take top N?
         ;; For now, let's take anyone with Sharpe > 0.1 to allow early evolution.
         (candidates (remove-if-not (lambda (s) (and (strategy-sharpe s) (> (strategy-sharpe s) 0.1))) unique-strats))
         ;; Sort by Sharpe
         (best (sort (copy-list candidates) #'> :key #'strategy-sharpe))
         ;; Take Top 50
         (elite (subseq best 0 (min (length best) 50)))
         (genes (mapcar #'extract-params-from-strategy elite)))
    
    (format t "[WISDOM] Found ~d candidates (Sharpe > 0.1). Extracting ~d Elite Genes.~%" (length candidates) (length elite))
    
    (if genes
        (save-optimized-params-to-file genes)
        (format t "[WISDOM] ⚠️ No eligible veterans found. Keeping existing genes.~%"))
    
    (length genes)))
