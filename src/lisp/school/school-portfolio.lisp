(in-package :swimmy.school)

;;; ============================================================================
;;; PORTFOLIO MANAGEMENT (V49.6 - Swarm Scaling)
;;; ============================================================================
;;; Responsibility: Global Draft, Correlation Analysis, Team Selection.
;;; ============================================================================

(defparameter *global-s-rank-cap* 100
  "V49.6: Scaled to 100 warriors (Musk mandate).")

(defun construct-global-portfolio ()
  "V49.6: Global S-RANK Draft. Selects best strategies across all categories.
   Prioritizes P&L Correlation over simple Genetic Distance."
  (format t "[PORTFOLIO] 🏟️ Starting Global Portfolio Draft (Cap=~d)...~%" *global-s-rank-cap*)
  (let* ((candidates (sort (copy-list (append (get-strategies-by-rank :S)
                                              (get-strategies-by-rank :A)))
                           #'> :key (lambda (s) (or (strategy-sharpe s) 0.0))))
         (selected-team nil)
         (demoted-count 0)
         (promoted-count 0))
    
    (dolist (candidate candidates)
      (if (and (< (length selected-team) *global-s-rank-cap*)
               (>= (or (strategy-sharpe candidate) 0.0) 0.5) ; S-Rank Floor
               (is-diverse-addition-p candidate selected-team))
          (progn
            (push candidate selected-team)
            (when (eq (strategy-rank candidate) :A)
              (incf promoted-count)
              (ensure-rank candidate :S "Drafted to Global Team")))
          (progn
            (when (eq (strategy-rank candidate) :S)
              (incf demoted-count)
              (ensure-rank candidate :A "Cut from Global Team")))))
    
    (format t "[PORTFOLIO] 📊 Draft Complete: Team Size=~d | Promoted=~d Demoted=~d~%"
            (length selected-team) promoted-count demoted-count)))

(defun is-diverse-addition-p (strategy team)
  "V49.6: Dual-Factor Diversity Check.
   1. P&L Correlation (if trade history exists)
   2. Genetic Distance (fallback)"
  (every (lambda (member)
           (let ((correlation (calculate-pnl-correlation strategy member)))
             (if correlation
                 (< correlation 0.7) ; Prado: Max 70% P&L Correlation
                 (let ((dist (if (fboundp 'calculate-genetic-distance)
                                 (calculate-genetic-distance 
                                  (extract-genome strategy) 
                                  (extract-genome member))
                                 1.0)))
                   (> dist 0.3))))) ; Diversity Threshold
         team))

(defun calculate-pnl-correlation (s1 s2)
  "V49.6: Calculate Pearson correlation of P&L curves.
   Uses trade data from school-learning logs."
  (let* ((name1 (strategy-name s1))
         (name2 (strategy-name s2))
         (trades1 (get-strategy-trades name1))
         (trades2 (get-strategy-trades name2)))
    (if (and (> (length trades1) 5) (> (length trades2) 5))
        (calculate-pearson-correlation (mapcar #'trade-record-pnl trades1)
                                      (mapcar #'trade-record-pnl trades2))
        nil)))

(defun get-strategy-trades (name)
  "Extract all recorded trades for a specific strategy."
  (remove-if-not (lambda (r) (string= (trade-record-strategy-name r) name))
                 (append *success-log* *failure-log*)))

(defun calculate-pearson-correlation (v1 v2)
  "Pearson Correlation Coefficient implementation."
  (let* ((n (min (length v1) (length v2)))
         (x (subseq v1 0 n))
         (y (subseq v2 0 n))
         (mean-x (/ (reduce #'+ x) n))
         (mean-y (/ (reduce #'+ y) n))
         (num (reduce #'+ (mapcar (lambda (xi yi) (* (- xi mean-x) (- yi mean-y))) x y)))
         (den (* (sqrt (reduce #'+ (mapcar (lambda (xi) (expt (- xi mean-x) 2)) x)))
                 (sqrt (reduce #'+ (mapcar (lambda (yi) (expt (- yi mean-y) 2)) y))))))
    (if (> den 0) (/ num den) 0)))
