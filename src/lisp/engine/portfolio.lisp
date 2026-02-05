;;; ============================================================================
;;; engine/portfolio.lisp - Portfolio Optimization
;;; ============================================================================
;;; Logic for selecting the best strategies (Arms) to deploy.
;;; Uses Thompson Sampling (Beta distribution).
;;; Part of "The Efficient Gardener" refactoring (Phase 6)
;;; ============================================================================

(in-package :swimmy.engine)

;;; ==========================================
;;; MATH UTILITIES (Probabilistic)
;;; ==========================================

(defun box-muller ()
  "Generate standard normal random variable using Box-Muller transform."
  (* (sqrt (* -2.0 (log (max 1e-10 (random 1.0)))))
     (cos (* 2.0 pi (random 1.0)))))

(defun ensure-real (x)
  "Ensure value is a real number."
  (float (if (listp x) (or (car x) 1) (or x 1))))

(defun random-gamma (k)
  "Generate gamma-distributed random variable."
  (setf k (max 0.01 (ensure-real k)))
  (if (< k 1.0)
      (* (random-gamma (1+ k)) (expt (max 1e-10 (random 1.0)) (/ 1.0 k)))
      (let* ((d (- k 0.333)) (c (/ 1.0 (sqrt (* 9.0 d)))))
        (loop (let* ((x (box-muller)) (v (expt (+ 1.0 (* c x)) 3)))
                (when (and (> v 0) (< (random 1.0) (- 1.0 (* 0.0331 (expt x 4)))))
                  (return (* d v))))))))

;;; ==========================================
;;; ALLOCATION LOGIC
;;; ==========================================

(defun get-sharpe-boost (arm-idx)
  "Return bonus score based on matching evolved strategy Sharpe."
  (declare (ignore arm-idx))
  (if (and (boundp '*evolved-strategies*) *evolved-strategies* (> (length *evolved-strategies*) 0))
      (let* ((top-strat (first *evolved-strategies*))
             (top-sharpe (strategy-sharpe top-strat)))
        (if (and top-sharpe (> top-sharpe 0))
            (* 0.1 (min top-sharpe 3.0))
            0.0))
      0.0))

(defun rebalance-portfolio ()
  "Rebalance portfolio using Thompson Sampling.
   Selects the best performing arms to fill *portfolio-indices*."
  (let ((scores nil))
    (dotimes (i (length *arms*))
      (let* ((stats (cdr (nth i *arms*)))
             (a (ensure-real (car stats))) (b (ensure-real (cdr stats)))
             (ga (random-gamma a)) (gb (random-gamma b))
             (base-score (/ ga (+ ga gb 1e-10)))
             (boost (get-sharpe-boost i))
             (score (+ base-score boost)))
        (push (cons score i) scores)))
    
    ;; Sort by score descending
    (setf scores (sort scores #'> :key #'car))
    
    ;; Select top N
    (let ((new-p (mapcar #'cdr (subseq scores 0 (min (length scores) *max-portfolio-size*)))))
      (unless (equal new-p *portfolio-indices*)
        (format t "~%[ENGINE] Portfolio rebalanced: ~a~%" new-p)
        (setf *portfolio-indices* new-p)
        ;; Save changes
        (when (fboundp 'save-genome)
          (save-genome))))))

(format t "[ENGINE] portfolio.lisp loaded~%")
