;;; school-evolution-orchestration.lisp - Evolution orchestration

(in-package :swimmy.school)

;;; ==========================================
;;; ORCHESTRATION (Migrated from school-learning.lisp)
;;; ==========================================

(defun evolve-population-via-mutation ()
  "Evolve population by applying random mutations (Genetic Algorithm).
   FIX: 2026-01-12 (Ponkotsu Repair)"
  (format t "[EVOLUTION] 🧬 Running Genetic Mutation Cycle...~%")
  (dolist (strat *strategy-knowledge-base*)
    ;; 5% Mutation Rate per cycle
    (when (< (random 1.0) 0.05)
      (mutate-strategy strat))))

(defun perform-reproduction ()
  "Allow dominant species to reproduce using Tournament Selection and Tribal Crossover."
  (let* ((population *evolved-strategies*)
         (current-count (length population))
         (max-population 50)
         (slots-available (- max-population current-count))
         (born-count 0))

    (when (> slots-available 0)
      (dotimes (i (min slots-available 5))
        (let ((roll (random 1.0))
              (child nil))

          (cond
            ;; 30% Tribal Crossover (Generative / AI-Driven)
            ((< roll 0.30)
             ;; Optimized Tribal Selection (Naval's O(1) Fix)
             (let* ((cats (loop for k being the hash-keys of *category-pools* collect k)))
               (when (> (length cats) 0)
                 (let* ((cat-a (nth (random (length cats)) cats))
                        (cat-b (if (> (length cats) 1)
                                   (loop for c = (nth (random (length cats)) cats)
                                         until (not (eq c cat-a))
                                         return c)
                                   cat-a)))
                   ;; Pick best from each tribe
                   (let ((tribe-a (gethash cat-a *category-pools*))
                         (tribe-b (gethash cat-b *category-pools*)))
                     (when (and tribe-a tribe-b)
                       (let ((p1 (select-parent-tournament tribe-a :k 2))
                             (p2 (select-parent-tournament tribe-b :k 2)))
                         ;; GENETIC DISTANCE CHECK (Musk's Sweet Spot)
                         (when (and p1 p2 (genetic-compatibility-p (extract-genome p1) (extract-genome p2)))
                           (format t "[L] 🧬 Tribal Crossover: ~a + ~a~%" (strategy-name p1) (strategy-name p2))

                           ;; V17: Generative Crossover
                           (if (and (boundp 'swimmy.main::*gemini-api-key*)
                                    swimmy.main::*gemini-api-key*)
                               (setf child (crossover-strategy-generative p1 p2))
                               (setf child (crossover-strategy p1 p2)))))))))))

            ;; 40% Tournament Crossover (Meritocracy)
            ((< roll 0.70)
             (let ((p1 (select-parent-tournament population))
                   (p2 (select-parent-tournament population)))
               (when (and p1 p2)
                 ;; GENETIC DISTANCE CHECK
                 (when (genetic-compatibility-p (extract-genome p1) (extract-genome p2))
                   (format t "[L] ⚔️ Tournament Crossover: ~a + ~a~%" (strategy-name p1) (strategy-name p2))
                   (setf child (crossover-strategy p1 p2))))))

            ;; 30% Asexual Mutation (Local Optimization)
            (t
             (let ((p (select-parent-tournament population)))
               (when p
                 (setf child (mutate-strategy p 0.2))))))

          (when child
            (push child *evolved-strategies*)
            (incf born-count)))))
    born-count))

(defun perform-immigration ()
  "Inject diverse strategies if ecosystem is unbalanced."
  (let ((immigrant-count 0))
    (when (ecosystem-needs-diversity-p)
      (let* ((weak-niche (get-underpopulated-niche))
             (candidates (remove-if-not (lambda (s) (eq (strategy-category s) weak-niche))
                                      *evolved-strategies*)))
        (when candidates
          (let ((parent (first (sort candidates #'> :key (lambda (s) (or (strategy-sharpe s) -999))))))
             (format t "[L] 🧬 DIVERSITY INJECTION: Need ~a strategies~%" weak-niche)
             (if (and (fboundp 'evolve-via-llm)
                      (boundp 'swimmy.main::*gemini-api-key*)
                      swimmy.main::*gemini-api-key*)
                 (progn
                   (format t "[L] 🧠 Triggering LLM Evolution (VS) for ~a...~%" weak-niche)
                   (evolve-via-llm weak-niche)
                   (incf immigrant-count))
                 (let ((child (mutate-strategy parent 0.3)))
                   (push child *evolved-strategies*)
                   (incf immigrant-count)))))))
    immigrant-count))

(defun apply-natural-selection ()
  "Orchestrate Evolution (Extinction -> Reproduction -> Immigration)"
  (let ((removed (perform-extinction))
        (born (perform-reproduction))
        (immigrated (perform-immigration)))

    (when (or (> removed 0) (> born 0) (> immigrated 0))
      (format t "[L] 🌿 ECOSYSTEM: ~d extinction, ~d reproduction, ~d immigration | Health: ~,0f%~%"
              removed born immigrated (* 100 (calculate-ecosystem-health))))))

(defun maintain-ecosystem-balance ()
  "Periodic ecosystem maintenance"
  ;; Apply natural selection pressure
  (apply-natural-selection)

  ;; Check diversity and suggest focus for new strategies
  (when (ecosystem-needs-diversity-p)
    (let ((weak-niche (get-underpopulated-niche)))
      (format t "[L] 🌱 Ecosystem needs ~a strategies for diversity~%" weak-niche)))

  ;; Report ecosystem state every N calls
  (let ((state (get-ecosystem-state)))
    (format t "[L] 🏞️ Population: ~d | Diversity: ~,2f | Balance: ~,0f% | Health: ~,0f%~%"
            (ecosystem-state-total-population state)
            (ecosystem-state-diversity-score state)
            (* 100 (ecosystem-state-niche-balance state))
            (* 100 (ecosystem-state-health-score state)))))

(defun get-ecosystem-recommendation ()
  "Get recommendation for new strategy generation"
  (let ((weak-niche (get-underpopulated-niche))
        (health (calculate-ecosystem-health)))
    (cond
      ((< health 0.3)
       (list :action :diversify
             :focus weak-niche
             :message "Ecosystem unhealthy - need diverse strategies"))
      ((ecosystem-needs-diversity-p)
       (list :action :specialize
             :focus weak-niche
             :message (format nil "Focus on ~a category" weak-niche)))
      (t
       (list :action :evolve
             :focus nil
             :message "Ecosystem healthy - continue evolution")))))
