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

;; ═══════════════════════════════════════════════════════════════════════════
;; V2d (2026-07-18) — B-4 / R4 BEHAVIOURAL DISTANCE
;;   (regen_engine_redesign_20260703.md §B-4, §A-1(3); tribe rebuild §R4)
;;
;; The legacy distance (indicators-Jaccard 2 + category 1 + SL 1 + TP 1) is BLIND to
;; symbol / timeframe / behaviour. Two USDJPY-TREND SMA-cross clones that differ only in
;; their periods (SMA-27/93 vs SMA-36/144) share NO indicator token, so their Jaccard = 0
;; and they score ~0.41 apart — above the 0.02 clone floor AND inside the 0.2–0.8 breeding
;; sweet-spot. That is the structural reason an 18.5k-strong USDJPY monoculture reads as
;; "diverse": clone detection (strategies-correlation-ok-p) and fitness-sharing literally
;; cannot see it (regen §A-1(3), §R4).
;;
;; Under *enable-primitive-diversity* the distance is rebuilt as a COMPOSITE niche/behaviour
;; distance in [0,1]:
;;   behaviour (1-|corr|, PRIMARY when a return series exists) + symbol + category/regime
;;   + timeframe-bucket + indicator-family + entry-tree + sl/tp.
;; Weights renormalise over the terms that actually carry data (the same idea as the legacy
;; conditional SL/TP weighting), so an absent behaviour series simply hands its weight to the
;; structural niche terms — a real, wired hook, never fabricated data. The live swimmy.db
;; currently ships NO return series (strategy_daily_pnl is empty), so the behaviour term stays
;; dormant and the structural niche terms carry clone detection today.
;;
;; Flag OFF => byte-identical legacy formula (every existing comparator unchanged).
;; ═══════════════════════════════════════════════════════════════════════════

(defparameter *b4-w-behavior* 0.48
  "Weight of behavioural distance 1-|corr| (regen B-4 PRIMARY). Applied only when both
   genomes carry a usable return series; otherwise its weight renormalises onto structure.")
(defparameter *b4-w-symbol* 0.20
  "Weight of symbol (currency) mismatch — the axis the monoculture collapses onto.")
(defparameter *b4-w-category* 0.12 "Weight of category/regime mismatch.")
(defparameter *b4-w-timeframe* 0.08 "Weight of timeframe-bucket distance.")
(defparameter *b4-w-family* 0.06
  "Weight of indicator-family (sma/rsi/bb/...) mismatch — behavioural proxy for entry logic.")
(defparameter *b4-w-tree* 0.03 "Weight of entry S-expression edit distance.")
(defparameter *b4-w-param* 0.03 "Weight of SL/TP parameter distance.")
(defparameter *b4-tf-log-scale* 11.0
  "log2 span that normalises timeframe distance (log2(10080 min) ~= 13.3).")

(defun %b4-symbol-key (sym)
  "Normalise a symbol gene to an upcased string for comparison (NIL -> NIL)."
  (cond ((null sym) nil)
        ((stringp sym) (string-upcase sym))
        (t (string-upcase (princ-to-string sym)))))

(defun genome-indicator-family (indicators)
  "Behavioural family of the leading indicator (regen R2: scored behaviour is set by the
   first indicator family, not the exact periods). ((SMA 50)(SMA 200)) -> \"SMA\";
   (SMA-27 SMA-93) -> \"SMA\"; ((RSI 14)) -> \"RSI\". NIL when unknown."
  (when indicators
    (let* ((head (first indicators))
           (name (cond ((consp head) (princ-to-string (car head)))
                       (head (princ-to-string head))
                       (t nil))))
      (when name
        ;; strip a trailing "-<digits>" period suffix (SMA-27 -> SMA)
        (let* ((up (string-upcase name))
               (dash (position #\- up :from-end t)))
          (if (and dash (< (1+ dash) (length up))
                   (every #'digit-char-p (subseq up (1+ dash))))
              (subseq up 0 dash)
              up))))))

(defun genome-timeframe-distance (tf-a tf-b)
  "Normalised log2 timeframe-bucket distance in [0,1]. Same TF -> 0."
  (if (and (numberp tf-a) (numberp tf-b) (> tf-a 0) (> tf-b 0))
      (min 1.0 (/ (abs (- (log (float tf-a 1.0) 2.0) (log (float tf-b 1.0) 2.0)))
                  *b4-tf-log-scale*))
      0.0))

(defun %sexp-node-count (tree)
  "Count nodes in an S-expression (atoms and cons cells)."
  (cond ((null tree) 0)
        ((atom tree) 1)
        (t (1+ (reduce #'+ (mapcar #'%sexp-node-count tree) :initial-value 0)))))

(defun genome-tree-distance (entry-a entry-b)
  "Cheap entry S-expression distance in [0,1]: normalised node-count difference plus a
   root-operator mismatch. Full tree-edit distance is a later refinement (regen B-4)."
  (let ((na (%sexp-node-count entry-a))
        (nb (%sexp-node-count entry-b)))
    (if (and (zerop na) (zerop nb))
        0.0
        (let* ((size-term (/ (abs (- na nb)) (max 1 na nb)))
               (op-a (and (consp entry-a) (car entry-a)))
               (op-b (and (consp entry-b) (car entry-b)))
               (op-term (if (eql op-a op-b) 0.0 1.0)))
          (min 1.0 (+ (* 0.5 size-term) (* 0.5 op-term)))))))

(defun genome-behavior-distance (beh-a beh-b)
  "Behavioural distance 1-|corr(returns)| (regen B-4 PRIMARY metric). Returns NIL when the
   series are unusable (missing, <3 aligned points, or zero variance) so the caller
   renormalises onto structure — a real hook, not fabricated data. The live swimmy.db ships
   no return series today (strategy_daily_pnl empty), so this stays dormant until behaviour
   vectors are wired into extract-genome's :behavior gene."
  (when (and (consp beh-a) (consp beh-b))
    (let ((n (min (length beh-a) (length beh-b))))
      (when (>= n 3)
        (let* ((x (subseq beh-a 0 n))
               (y (subseq beh-b 0 n))
               (mx (/ (reduce #'+ x) n))
               (my (/ (reduce #'+ y) n))
               (num (reduce #'+ (mapcar (lambda (xi yi) (* (- xi mx) (- yi my))) x y)))
               (dx (sqrt (reduce #'+ (mapcar (lambda (xi) (expt (- xi mx) 2)) x))))
               (dy (sqrt (reduce #'+ (mapcar (lambda (yi) (expt (- yi my) 2)) y)))))
          (when (and (> dx 0) (> dy 0))
            (- 1.0 (min 1.0 (abs (/ num (* dx dy)))))))))))

(defun b4-genetic-distance (genome-a genome-b)
  "Composite niche/behavioural distance in [0,1] (regen B-4). Weights renormalise over the
   terms that carry data; behaviour is PRIMARY when a return series exists, else the
   structural niche terms (symbol/category/timeframe/family/tree/params) carry the signal.
   Same (symbol,category,timeframe,family) niche -> ~0 (clone); different symbol/regime -> far."
  (let ((dist 0.0) (tw 0.0))
    ;; behaviour 1-|corr| (PRIMARY, conditional on data)
    (let ((bd (genome-behavior-distance (getf genome-a :behavior) (getf genome-b :behavior))))
      (when bd (incf tw *b4-w-behavior*) (incf dist (* *b4-w-behavior* bd))))
    ;; symbol / currency niche
    (let ((sa (%b4-symbol-key (getf genome-a :symbol)))
          (sb (%b4-symbol-key (getf genome-b :symbol))))
      (when (and sa sb)
        (incf tw *b4-w-symbol*)
        (unless (string= sa sb) (incf dist *b4-w-symbol*))))
    ;; category / regime niche
    (incf tw *b4-w-category*)
    (unless (eq (getf genome-a :category) (getf genome-b :category))
      (incf dist *b4-w-category*))
    ;; timeframe bucket
    (incf tw *b4-w-timeframe*)
    (incf dist (* *b4-w-timeframe*
                  (genome-timeframe-distance (getf genome-a :timeframe)
                                             (getf genome-b :timeframe))))
    ;; indicator family (behavioural proxy of the entry logic)
    (let ((fa (genome-indicator-family (getf genome-a :indicators)))
          (fb (genome-indicator-family (getf genome-b :indicators))))
      (when (and fa fb)
        (incf tw *b4-w-family*)
        (unless (string= fa fb) (incf dist *b4-w-family*))))
    ;; entry S-expression edit distance
    (incf tw *b4-w-tree*)
    (incf dist (* *b4-w-tree* (genome-tree-distance (getf genome-a :entry)
                                                    (getf genome-b :entry))))
    ;; sl / tp parameters (behaviourally minor -> light weight)
    (let ((sl-a (getf genome-a :sl)) (sl-b (getf genome-b :sl))
          (tp-a (getf genome-a :tp)) (tp-b (getf genome-b :tp))
          (p 0.0) (pn 0))
      (when (and sl-a sl-b) (incf p (min 1.0 (abs (- sl-a sl-b)))) (incf pn))
      (when (and tp-a tp-b) (incf p (min 1.0 (abs (- tp-a tp-b)))) (incf pn))
      (when (> pn 0)
        (incf tw *b4-w-param*)
        (incf dist (* *b4-w-param* (/ p pn)))))
    (if (> tw 0.0) (/ dist tw) 0.0)))

(defun legacy-genetic-distance (genome-a genome-b)
  "Pre-diversity distance: indicators Jaccard(2) + category(1) + SL(1) + TP(1). Kept
   verbatim so *enable-primitive-diversity*=NIL is byte-identical to the old engine."
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

(defun calculate-genetic-distance (genome-a genome-b)
  "Genetic distance (0.0 = Clone, 1.0 = Alien) between two genomes.
   Flag ON  => B-4 composite niche/behaviour distance (symbol/TF/family/tree/behaviour).
   Flag OFF => byte-identical legacy formula (indicators Jaccard + category + SL + TP)."
  (if *enable-primitive-diversity*
      (b4-genetic-distance genome-a genome-b)
      (legacy-genetic-distance genome-a genome-b)))

(defun genetic-compatibility-p (genome-a genome-b &key (min-dist 0.2) (max-dist 0.8))
  "Check if two genomes are compatible for breeding (Sweet Spot)."
  (let ((dist (calculate-genetic-distance genome-a genome-b)))
    (and (>= dist min-dist) (<= dist max-dist))))

;;; ==========================================
;;; GENOME INTERFACE
;;; ==========================================

(defun extract-genome (strategy)
  "Extract genes from a strategy object to a plist.
   V2d (regen B-1): :symbol is now a first-class gene so calculate-genetic-distance can see
   the currency niche (previously symbol was never carried, so 18.5k USDJPY clones were
   indistinguishable from aliens). :behavior carries the return series for the B-4 corr hook
   (dormant until populated). Both extra keys are inert under the legacy distance / crossover
   paths, which read only the keys they name — so flag-OFF behaviour is unchanged."
  (list :indicators (strategy-indicators strategy)
        :entry (strategy-entry strategy)
        :exit (strategy-exit strategy)
        :sl (strategy-sl strategy)
        :tp (strategy-tp strategy)
        :volume (strategy-volume strategy)
        :timeframe (strategy-timeframe strategy)
        :category (strategy-category strategy)
        :symbol (strategy-symbol strategy)
        :behavior (strategy-pnl-history strategy)))

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
  ;; V2d (regen B-1): propagate the symbol gene under the diversity flag. Guarded so
  ;; *enable-primitive-diversity*=NIL is byte-identical (legacy left strategy-symbol at its
  ;; make-strategy default rather than inheriting the parent block's symbol).
  (when (and *enable-primitive-diversity* (getf genome :symbol))
    (setf (strategy-symbol child-strat) (getf genome :symbol)))
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
                    ;; V2d: symbol travels with the entry/timeframe/category block (genome-a)
                    :symbol (getf genome-a :symbol)
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
                    ;; V2d: symbol travels with the entry/timeframe/category block (genome-b)
                    :symbol (getf genome-b :symbol)
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

;; V2c (2026-07-14): fitness-sharing selection accessor (regen doc B-2). Flag OFF =>
;; returns raw strategy-sharpe, so every legacy comparator is byte-identical. Flag ON =>
;; evidence-adjusted sharpe (shrinks sparse-sample over-optimization) DIVIDED by a niche
;; crowding count, so the k-th clone of a (symbol,category) niche is divided by ~k and a
;; 484-strong USDJPY/TREND monoculture is structurally unable to dominate selection.
(defun fitness-sharing-denominator (strategy population)
  "Count population members sharing STRATEGY's (symbol, category) niche (>=1)."
  (if (null population)
      1
      (let ((sym (strategy-symbol strategy))
            (cat (strategy-category strategy))
            (n 0))
        (dolist (s population)
          (when (and (equal (strategy-symbol s) sym)
                     (eq (strategy-category s) cat))
            (incf n)))
        (max 1 n))))

(defun selection-fitness (strategy &optional population)
  "Selection score. Flag OFF => raw strategy-sharpe (legacy). Flag ON => evidence-adjusted
   sharpe / niche-crowding (fitness sharing forces diversity by division)."
  (let ((raw (or (strategy-sharpe strategy) 0.0)))
    (if *enable-primitive-diversity*
        (let* ((trades (or (strategy-trades strategy) 0))
               (adj (if (fboundp 'evidence-adjusted-sharpe)
                        (evidence-adjusted-sharpe raw trades)
                        raw))
               (share (fitness-sharing-denominator strategy population)))
          (/ adj (max 1.0 (float share 1.0))))
        raw)))

(defun select-parent-tournament (population &key (k 3))
  "Select a parent using Tournament Selection (K-way).
   Prevents 'Top 2' dominance and maintains diversity.
   V2c: ranks by selection-fitness (fitness-sharing when the diversity flag is on)."
  (let ((len (length population)))
    (when (plusp len)
      (let ((candidates (loop repeat k
                              collect (nth (random len) population))))
        ;; Return the one with highest selection-fitness (raw sharpe when flag OFF).
        (first (sort candidates #'> :key (lambda (s) (or (selection-fitness s population) -999))))))))

(defun select-category-pair (population)
  "Select two parents from DIFFERENT categories.
   Promotes 'Hybrid Vigor' and innovation."
  (let* ((categories (remove-duplicates (mapcar #'strategy-category population)))
         (cat-a (nth (random (length categories)) categories))
         ;; Force cat-b to be different if possible
         (cat-b (if (> (length categories) 1)
                    (loop for c = (nth (random (length categories)) categories)
                          until (not (eq c cat-a))
                          return c)
                    cat-a))
         ;; Get best of each category (or random elite)
         ;; Use Tournament within the category to pick the representative
         (category-a (remove-if-not (lambda (s) (eq (strategy-category s) cat-a)) population))
         (category-b (remove-if-not (lambda (s) (eq (strategy-category s) cat-b)) population))
         (parent-a (select-parent-tournament category-a :k 2))
         (parent-b (select-parent-tournament category-b :k 2)))
    
    (cons parent-a parent-b)))

(format t "[GENOME] 🧬 The Genome Engine v14.1 (Incest Prevention) Loaded.~%")
