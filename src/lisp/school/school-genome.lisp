;;; school-genome.lisp - The Genome Engine (ゲノムエンジン)
;;;
;;; "Evolution without Sex is just decay."
;;; This module implements Sexual Reproduction (Crossover) and Structural Mutation.
;;; It transforms the "Strategy" monolith into a discrete "Genome" that can be spliced.
;;;
;;; Concepts:
;;; - Genome: The complete set of genes (indicators, entry-logic, etc.)
;;; - Crossover: Combining genes from two parents to create a child.
;;; - Structural Mutation: Modifying the Lisp Code structure (Logic Mutation).

(in-package :swimmy.school)

;;; ==========================================
;;; GENETIC DISTANCE & SIMILARITY
;;; ==========================================

(defun calculate-genetic-distance (genome-a genome-b)
  "Calculate genetic distance (0.0 = Clone, 1.0 = Alien) between two genomes."
  (let ((dist 0.0)
        (total-weight 0.0))
    
    ;; 1. Indicator Set Similarity (Jaccard Distance)
    (let* ((inds-a (getf genome-a :indicators))
           (inds-b (getf genome-b :indicators))
           (union (remove-duplicates (append inds-a inds-b) :test #'equal))
           (intersection (remove-if-not (lambda (i) (member i inds-b :test #'equal)) inds-a)))
      (incf total-weight 2.0)
      (when union
        (let ((jaccard (/ (length intersection) (length union))))
          (incf dist (* 2.0 (- 1.0 jaccard))))))
    
    ;; 2. Entry Logic Similarity (Tree Depth/Size difference?) -> Hard.
    ;; Let's simplify: Same Category?
    (incf total-weight 1.0)
    (unless (eq (getf genome-a :category) (getf genome-b :category))
      (incf dist 1.0))

    ;; 3. Parameter Similarity (Euclidean-ish)
    (let ((sl-a (getf genome-a :sl)) (sl-b (getf genome-b :sl))
          (tp-a (getf genome-a :tp)) (tp-b (getf genome-b :tp)))
      (incf total-weight 1.0)
      (when (and sl-a sl-b)
        (let ((diff (abs (- sl-a sl-b))))
          (incf dist (min 1.0 diff))))
      (incf total-weight 1.0)
      (when (and tp-a tp-b)
        (let ((diff (abs (- tp-a tp-b))))
          (incf dist (min 1.0 diff)))))

    (/ dist total-weight)))

(defun genetic-compatibility-p (genome-a genome-b &key (min-dist 0.2) (max-dist 0.8))
  "Check if two genomes are compatible for breeding (Sweet Spot)."
  (let ((dist (calculate-genetic-distance genome-a genome-b)))
    (and (>= dist min-dist) (<= dist max-dist))))

;;; ==========================================
;;; GENOME INTERFACE
;;; ==========================================

(defun extract-genome (strategy)
  "Extract genes from a strategy object to a plist"
  (list :indicators (strategy-indicators strategy)
        :entry (strategy-entry strategy)
        :exit (strategy-exit strategy)
        :sl (strategy-sl strategy)
        :tp (strategy-tp strategy)
        :volume (strategy-volume strategy)
        :timeframe (strategy-timeframe strategy)
        :category (strategy-category strategy)))

(defun implant-genome (child-strat genome)
  "Implant genes back into a child strategy"
  (setf (strategy-indicators child-strat) (getf genome :indicators))
  (setf (strategy-entry child-strat) (getf genome :entry))
  (setf (strategy-exit child-strat) (getf genome :exit))
  (setf (strategy-sl child-strat) (getf genome :sl))
  (setf (strategy-tp child-strat) (getf genome :tp))
  (setf (strategy-volume child-strat) (getf genome :volume))
  (setf (strategy-timeframe child-strat) (getf genome :timeframe))
  (setf (strategy-category child-strat) (getf genome :category))
  child-strat)

;;; ==========================================
;;; CROSSOVER LOGIC (SEXUAL REPRODUCTION)
;;; ==========================================

(defun crossover-genes (genome-a genome-b)
  "Mix genes from two parents (Uniform Crossover)"
  (let ((child-genome nil)
        ;; V14.1 FIX (Taleb's Warning): Merge indicators to prevent Lethal Genes.
        ;; If Child inherits Exit Logic from B, it MUST have B's indicators.
        (all-indicators (remove-duplicates (append (getf genome-a :indicators) 
                                                   (getf genome-b :indicators)) 
                                           :test #'equal)))
    
    (if (> (random 1.0) 0.5)
        ;; Inherit Block 1 from A, Block 2 from B
        (setf child-genome 
              (list :indicators all-indicators ; SAFETY FIX
                    :entry (getf genome-a :entry)
                    :timeframe (getf genome-a :timeframe)
                    :category (getf genome-a :category)
                    :exit (getf genome-b :exit) 
                    :sl (getf genome-b :sl)
                    :tp (getf genome-b :tp)
                    :volume (getf genome-b :volume)))
        
        ;; Inherit Block 1 from B, Block 2 from A
        (setf child-genome 
              (list :indicators all-indicators ; SAFETY FIX
                    :entry (getf genome-b :entry)
                    :timeframe (getf genome-b :timeframe)
                    :category (getf genome-b :category)
                    :exit (getf genome-a :exit)
                    :sl (getf genome-a :sl)
                    :tp (getf genome-a :tp)
                    :volume (getf genome-a :volume))))
    
    ;; Simple Parameter Mix (50/50 chance for each scalar)
    ;; Overwrite scalar genes randomly
    (when (> (random 1.0) 0.5) (setf (getf child-genome :sl) (getf genome-a :sl)))
    (when (> (random 1.0) 0.5) (setf (getf child-genome :tp) (getf genome-a :tp)))
    
    child-genome))

(defun crossover-strategy (parent-a parent-b)
  "Create a child strategy from two parents using The Genome Engine"
  (let* ((genome-a (extract-genome parent-a))
         (genome-b (extract-genome parent-b))
         (child-genome (crossover-genes genome-a genome-b))
         (child (make-strategy :name (format nil "Child-~a" (get-universal-time)))))
    
    (implant-genome child child-genome)
    
    ;; Name Generation: Mom-Dad-Mix
    (let ((root-a (subseq (strategy-name parent-a) 0 (min 10 (length (strategy-name parent-a)))))
          (root-b (subseq (strategy-name parent-b) 0 (min 10 (length (strategy-name parent-b))))))
      (setf (strategy-name child) 
            (format nil "Mix-~a-~a-Gen~d" root-a root-b (+ 1 (max (or (strategy-generation parent-a) 0) 
                                                                  (or (strategy-generation parent-b) 0))))))
    
    (setf (strategy-generation child) (+ 1 (max (or (strategy-generation parent-a) 0) 
                                                (or (strategy-generation parent-b) 0))))
    child))

;;; ==========================================
;;; STRUCTURAL MUTATION (LOGIC SPLICING)
;;; ==========================================

(defun mutate-sexp-tree (tree mutation-rate)
  "Recursively walk the S-Expression tree and mutate operators."
  (cond
    ;; Base case: Atom (return as is, or mutate number slightly?)
    ((atom tree) 
     (if (and (numberp tree) (> (random 1.0) 0.8))
         ;; Small numeric drift for constants in logic (e.g. RSI > 30 -> 32)
         (let ((new-val (round (* tree (+ 0.9 (random 0.2))))))
           (if (= new-val tree) (if (> (random 1.0) 0.5) (1+ tree) (1- tree)) new-val))
         tree))
    
    ;; Recursive case: List (Function call)
    ((listp tree)
     (let ((op (car tree))
           (args (cdr tree)))
       (cond
         ;; Operator Flipping
         ((and (eq op 'and) (< (random 1.0) mutation-rate))
          (cons 'or (mapcar (lambda (x) (mutate-sexp-tree x mutation-rate)) args)))
         
         ((and (eq op 'or) (< (random 1.0) mutation-rate))
          (cons 'and (mapcar (lambda (x) (mutate-sexp-tree x mutation-rate)) args)))
         
         ((and (eq op '>) (< (random 1.0) mutation-rate))
          (cons '< (mapcar (lambda (x) (mutate-sexp-tree x mutation-rate)) args)))
         
         ((and (eq op '<) (< (random 1.0) mutation-rate))
          (cons '> (mapcar (lambda (x) (mutate-sexp-tree x mutation-rate)) args)))
         
         ;; Default: Keep operator, recurse on args
         (t
          (cons op (mapcar (lambda (x) (mutate-sexp-tree x mutation-rate)) args))))))))

(defun perform-structural-mutation (strategy)
  "Mutate the Logic (S-Expression) of a strategy."
  (let ((mutant (copy-structure strategy)))
    ;; Mutate Entry Logic
    (setf (strategy-entry mutant) 
          (mutate-sexp-tree (strategy-entry strategy) 0.1)) ; 10% chance per node
    
    (setf (strategy-name mutant)
          (format nil "~a-StructMut" (strategy-name strategy)))
    
    (format t "[GENOME] 🧬 Structural Mutation performed on ~a~%" (strategy-name strategy))
    mutant))

;;; ==========================================
;;; SELECTION MECHANISMS (INCEST PREVENTION)
;;; ==========================================

(defun select-parent-tournament (population &key (k 3))
  "Select a parent using Tournament Selection (K-way).
   Prevents 'Top 2' dominance and maintains diversity."
  (let ((len (length population)))
    (when (plusp len)
      (let ((candidates (loop repeat k 
                              collect (nth (random len) population))))
        ;; Return the one with highest sharpe (or 0 if nil)
        (first (sort candidates #'> :key (lambda (s) (or (strategy-sharpe s) -999))))))))

(defun select-tribal-pair (population)
  "Select two parents from DIFFERENT clans/categories.
   Promotes 'Hybrid Vigor' and innovation."
  (let* ((categories (remove-duplicates (mapcar #'strategy-category population)))
         (cat-a (nth (random (length categories)) categories))
         ;; Force cat-b to be different if possible
         (cat-b (if (> (length categories) 1)
                    (loop for c = (nth (random (length categories)) categories)
                          until (not (eq c cat-a))
                          return c)
                    cat-a))
         ;; Get best of each tribe (or random elite)
         ;; Actually, let's use Tournament within the Tribe to pick the representative
         (tribe-a (remove-if-not (lambda (s) (eq (strategy-category s) cat-a)) population))
         (tribe-b (remove-if-not (lambda (s) (eq (strategy-category s) cat-b)) population))
         (parent-a (select-parent-tournament tribe-a :k 2))
         (parent-b (select-parent-tournament tribe-b :k 2)))
    
    (cons parent-a parent-b)))

(format t "[GENOME] 🧬 The Genome Engine v14.1 (Incest Prevention) Loaded.~%")
