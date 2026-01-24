;;; school-factory.lisp - Predictor Factory (Jim Simons Adaptation)
;;; ============================================================================
;;; Purpose: Generate and Sieve thousands of 'Weak Learners'
;;; ============================================================================

(in-package :swimmy.school)

;;; ----------------------------------------------------------------------------
;;; 1. EVALUATORS
;;; ----------------------------------------------------------------------------

(defun evaluate-predictor-forward-return (predictor history &key (horizon 10))
  "Calculate the average forward return of a predictor's signals.
   Horizon is the number of bars to wait for price movement."
  (let ((signals nil))
    (loop for i from horizon to (- (length history) 1)
          do (let* ((sub-hist (subseq history i))
                    (sig (evaluate-predictor predictor sub-hist)))
               (when (/= sig 0)
                 (let* ((entry-price (candle-close (first sub-hist)))
                        (exit-price (candle-close (nth (- i horizon) history)))
                        (ret (* sig (/ (- exit-price entry-price) entry-price))))
                   (push ret signals)))))
    
    (if (null signals)
        0.0
        (let* ((count (length signals))
               (avg (/ (reduce #'+ signals) count))
               (variance (/ (reduce #'+ (mapcar (lambda (x) (expt (- x avg) 2)) signals)) count))
               (stddev (sqrt (max 0.000001 variance))))
          (values (/ avg stddev) count))))) ; Returns Sharpe-like ratio

;;; ----------------------------------------------------------------------------
;;; 2. GENERATORS
;;; ----------------------------------------------------------------------------

(defun generate-combinatorial-library ()
  "Generate a list of candidate S-expressions for predictors."
  (let ((library nil)
        (rsi-periods '(7 14 21 28))
        (rsi-levels '(30 40 50 60 70))
        (ma-periods '(10 20 50 100 200)))
    
    ;; RSI Variations
    (dolist (n rsi-periods)
      (dolist (lvl rsi-levels)
        (push `(> (ind-rsi ,n history) ,lvl) library)
        (push `(< (ind-rsi ,n history) ,lvl) library)))
    
    ;; MA Crosses
    (dolist (fast ma-periods)
      (dolist (slow ma-periods)
        (when (< fast slow)
          (push `(> (ind-sma ,fast history) (ind-sma ,slow history)) library)
          (push `(< (ind-sma ,fast history) (ind-sma ,slow history)) library))))
    
    ;; Price vs MA
    (dolist (n ma-periods)
      (push `(> (candle-close (first history)) (ind-sma ,n history)) library)
      (push `(< (candle-close (first history)) (ind-sma ,n history)) library))
    
    library))

;;; ----------------------------------------------------------------------------
;;; 3. THE SIEVE (FILTER)
;;; ----------------------------------------------------------------------------

(defun run-factory-sieve (symbol &key (max-candidates 100))
  "Generate and filter predictors for a specific symbol.
   This fulfills the 'Haystack' vision: test everything, keep what works."
  (let* ((history (gethash symbol *candle-histories*))
         (candidates (generate-combinatorial-library))
         (passed nil))
    
    (unless (and history (> (length history) 200))
      (format t "[FACTORY] ⚠️ Insufficient history for ~a sieve.~%" symbol)
      (return-from run-factory-sieve nil))
    
    (format t "[FACTORY] 🏭 Sifting through ~d candidates for ~a...~%" (length candidates) symbol)
    
    (dolist (logic candidates)
      (let ((p (make-predictor :logic logic)))
        (multiple-value-bind (sharpe count) (evaluate-predictor-forward-return p history)
          ;; Weak Learners: even a slightly positive sharpe with decent trade count is kept
          (when (and (> sharpe 0.1) (> count 50))
            (setf (predictor-sharpe p) sharpe)
            (setf (predictor-trades p) count)
            (push p passed)))))
    
    (let ((sorted (sort passed #'> :key #'predictor-sharpe)))
      (let ((final (subseq sorted 0 (min max-candidates (length sorted)))))
        (format t "[FACTORY] ✅ Harvested ~d predictors for ~a.~%" (length final) symbol)
        final))))

;;; ----------------------------------------------------------------------------
;;; 4. SWARM INITIALIZATION
;;; ----------------------------------------------------------------------------

(defun create-symbol-swarm (symbol)
  "Create and populate a Swarm strategy for a specific symbol."
  (let* ((predictors (run-factory-sieve symbol))
         (swarm (make-swarm (format nil "Swarm-~a" symbol) :trend :predictors predictors)))
    (setf (strategy-symbol swarm) symbol)
    swarm))

(format t "[FACTORY] Predictor Factory online - Ready to process the haystack.~%")
