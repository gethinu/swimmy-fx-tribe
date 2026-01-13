;;; school-ecosystem.lisp - Ecosystem Dynamics (生態系ダイナミクス)
;;; Part of the Swimmy School System
;;; Extracted from school-learning.lisp (Expert Panel 2026-01-13)

(in-package :swimmy.school)

;;; ==========================================
;;; ECOSYSTEM DYNAMICS (生態系ダイナミクス)
;;; ==========================================
;;; Features:
;;; - Strategy diversity maintenance
;;; - Niche management (category balance)
;;; - Natural selection pressure
;;; - Population health monitoring
;;; - Symbiotic relationships

(defparameter *min-diversity-score* 0.3)    ; Minimum acceptable diversity
(defparameter *max-species-per-niche* 10)   ; Max strategies per category
(defparameter *extinction-threshold* -0.5)  ; Sharpe below this = extinction risk
(defparameter *reproduction-threshold* 0.5) ; Sharpe above this = reproduction chance

(defstruct ecosystem-state
  total-population
  diversity-score
  niche-balance
  health-score
  dominant-species
  endangered-species
  timestamp)

(defun calculate-diversity-score ()
  "Calculate Shannon diversity index of strategy population"
  (let* ((categories '(:trend :reversion :breakout :scalp))
         (counts (mapcar (lambda (cat) 
                          (length (gethash cat *category-pools*)))
                        categories))
         (total (reduce #'+ counts)))
    (if (> total 0)
        (let ((proportions (mapcar (lambda (c) (if (> c 0) (/ c total) 0)) counts)))
          ;; Shannon diversity: -sum(p * ln(p))
          (- (reduce #'+ (mapcar (lambda (p) 
                                  (if (> p 0) (* p (log p)) 0))
                                proportions))))
        0)))

(defun calculate-niche-balance ()
  "Calculate how balanced the niches (categories) are"
  (let* ((categories '(:trend :reversion :breakout :scalp))
         (counts (mapcar (lambda (cat) 
                          (length (gethash cat *category-pools*)))
                        categories))
         (total (max 1 (reduce #'+ counts)))
         (ideal (/ total 4.0))
         (deviations (mapcar (lambda (c) (abs (- c ideal))) counts))
         (avg-deviation (/ (reduce #'+ deviations) 4)))
    ;; Return 0-1 where 1 is perfect balance
    (max 0 (- 1 (/ avg-deviation ideal)))))

(defun identify-endangered-species ()
  "Find strategies at risk of extinction"
  (let ((endangered nil))
    (dolist (strat *evolved-strategies*)
      (when (and (strategy-sharpe strat)
                 (< (strategy-sharpe strat) *extinction-threshold*))
        (push (strategy-name strat) endangered)))
    endangered))

(defun identify-dominant-species ()
  "Find most successful strategies"
  (let ((dominant nil))
    (dolist (strat *evolved-strategies*)
      (when (and (strategy-sharpe strat)
                 (> (strategy-sharpe strat) *reproduction-threshold*))
        (push (cons (strategy-name strat) (strategy-sharpe strat)) dominant)))
    (sort dominant #'> :key #'cdr)))

(defun calculate-ecosystem-health ()
  "Calculate overall ecosystem health score"
  (let* ((diversity (calculate-diversity-score))
         (balance (calculate-niche-balance))
         (endangered (length (identify-endangered-species)))
         (dominant (length (identify-dominant-species)))
         (total-pop (length *evolved-strategies*)))
    ;; Health = diversity + balance - endangered ratio + dominant bonus
    (let ((health (+ (* diversity 0.3)
                     (* balance 0.3)
                     (if (> total-pop 0) (* 0.2 (- 1 (/ endangered total-pop))) 0.2)
                     (if (> total-pop 0) (* 0.2 (min 1 (/ dominant total-pop 0.5))) 0))))
      (max 0 (min 1 health)))))

;; Alias for brain.lisp compatibility
(defun get-population-health ()
  "Get overall ecosystem health score (alias)"
  (calculate-ecosystem-health))

(defun get-ecosystem-state ()
  "Capture current ecosystem state"
  (make-ecosystem-state
   :total-population (length *evolved-strategies*)
   :diversity-score (calculate-diversity-score)
   :niche-balance (calculate-niche-balance)
   :health-score (calculate-ecosystem-health)
   :dominant-species (mapcar #'car (identify-dominant-species))
   :endangered-species (identify-endangered-species)
   :timestamp (get-universal-time)))

(defun ecosystem-needs-diversity-p ()
  "Check if ecosystem needs more diversity"
  (< (calculate-diversity-score) *min-diversity-score*))

(defun get-underpopulated-niche ()
  "Find the category with fewest strategies"
  (let* ((categories '(:trend :reversion :breakout :scalp))
         (counts (mapcar (lambda (cat) 
                          (cons cat (length (gethash cat *category-pools*))))
                        categories))
         (sorted (sort counts #'< :key #'cdr)))
    (car (first sorted))))

(defun perform-extinction ()
  "Remove poor performers (Sharpe < -0.5) with a 70% chance."
  (let ((endangered (identify-endangered-species))
        (removed-count 0))
    (when endangered
      (dolist (name endangered)
        (let ((strat (find name *evolved-strategies* :key #'strategy-name :test #'string=)))
          (when (and strat (> (random 1.0) 0.3)) ; 70% chance of death
            (let ((final-pnl (or (strategy-sharpe strat) 0))
                  (lessons (format nil "Strategy ~a failed in ~a regime" 
                                   name (symbol-name (or *current-regime* :unknown)))))
              (when (fboundp 'hold-funeral) ; Wait, hold-funeral is in school-learning.lisp core?
                (hold-funeral name final-pnl lessons))
              (setf *evolved-strategies* 
                  (remove strat *evolved-strategies*))
              (incf removed-count))))))
    removed-count))
