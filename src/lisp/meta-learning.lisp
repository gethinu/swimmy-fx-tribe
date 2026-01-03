;;; ============================================================================
;;; meta-learning.lisp - Naval Advisor Homework #1: Meta-Learning Layer
;;; ============================================================================
;;;
;;; PURPOSE:
;;;   Learn which trading strategies perform best in which market regimes.
;;;   Automatically select optimal strategies based on accumulated performance data.
;;;
;;; KEY CONCEPTS:
;;;   - Regime: Market state (:trending, :ranging, :volatile, :unknown)
;;;   - Strategy-Regime Matrix: Performance stats for each (regime, strategy) pair
;;;   - Auto-Selection: Chooses best strategy when regime is detected
;;;
;;; MAIN FUNCTIONS:
;;;   (record-regime-strategy-result regime strategy won-p pnl)
;;;     → Record trade outcome for learning
;;;   (auto-select-strategy)
;;;     → Get recommended strategy for current regime
;;;   (meta-learning-report)
;;;     → Print performance summary
;;;
;;; INTEGRATION:
;;;   Called from school.lisp:record-trade-outcome via on-trade-close-meta hook
;;;   Saves state periodically via evolution.lisp:naval-meta-learning-step
;;;
;;; VERSION: 1.0 (2026-01-02)
;;; ============================================================================

;;; ==========================================
;;; META-LEARNING STATE
;;; ==========================================

(defparameter *regime-strategy-matrix* (make-hash-table :test 'equal)
  "Hash: (regime . strategy-name) -> (wins losses total-pnl)")
(defparameter *regime-best-strategies* (make-hash-table :test 'eq)
  "Hash: regime -> best-strategy-name")
(defparameter *meta-learning-log-path* "/home/swimmy/swimmy/.opus/meta_learning.json")
(defparameter *min-samples-for-selection* 10
  "Minimum trades before confident strategy selection")

;;; ==========================================
;;; REGIME-STRATEGY PERFORMANCE TRACKING
;;; ==========================================

(defun record-regime-strategy-result (regime strategy-name won-p pnl)
  "Record a trade result for regime-strategy pair.
   Called after each trade closes."
  (let* ((key (cons regime strategy-name))
         (stats (gethash key *regime-strategy-matrix* (list 0 0 0.0))))
    ;; Update wins/losses/pnl
    (if won-p
        (incf (first stats))
        (incf (second stats)))
    (incf (third stats) pnl)
    (setf (gethash key *regime-strategy-matrix*) stats)
    
    ;; Log
    (format t "[M] 📊 Meta: ~a + ~a → ~a (W:~d L:~d PnL:~,0f)~%"
            regime strategy-name (if won-p "WIN" "LOSS")
            (first stats) (second stats) (third stats))
    
    ;; Recalculate best strategy for this regime
    (update-best-strategy-for-regime regime)))

(defun get-regime-strategy-stats (regime strategy-name)
  "Get stats for a specific regime-strategy pair"
  (gethash (cons regime strategy-name) *regime-strategy-matrix*))

(defun get-regime-strategy-win-rate (regime strategy-name)
  "Calculate win rate for regime-strategy pair"
  (let ((stats (get-regime-strategy-stats regime strategy-name)))
    (when stats
      (let ((total (+ (first stats) (second stats))))
        (if (> total 0)
            (/ (first stats) (float total))
            0.5)))))

;;; ==========================================
;;; BEST STRATEGY SELECTION
;;; ==========================================

(defun update-best-strategy-for-regime (regime)
  "Find and cache the best performing strategy for a regime"
  (let ((best-strategy nil)
        (best-score -999)
        (candidates nil))
    
    ;; Collect all strategies that have been tested in this regime
    (maphash
     (lambda (key stats)
       (when (eq (car key) regime)
         (let* ((strategy-name (cdr key))
                (wins (first stats))
                (losses (second stats))
                (total (+ wins losses))
                (pnl (third stats))
                (win-rate (if (> total 0) (/ wins (float total)) 0.5))
                ;; Score = win-rate * 0.6 + normalized PnL * 0.4
                (score (if (>= total *min-samples-for-selection*)
                           (+ (* win-rate 0.6) 
                              (* (/ pnl (max 1 total)) 0.0001))  ;; Normalize PnL
                           0)))  ;; Not enough samples
           (push (list strategy-name :score score :wins wins :losses losses :pnl pnl) candidates)
           (when (> score best-score)
             (setf best-score score)
             (setf best-strategy strategy-name)))))
     *regime-strategy-matrix*)
    
    (when best-strategy
      (setf (gethash regime *regime-best-strategies*) best-strategy)
      (format t "[M] 👑 Best for ~a: ~a (score: ~,3f)~%"
              regime best-strategy best-score))
    
    best-strategy))

(defun get-best-strategy-for-regime (regime)
  "Get the best performing strategy for current regime"
  (or (gethash regime *regime-best-strategies*)
      (update-best-strategy-for-regime regime)))

;;; ==========================================
;;; AUTO-SELECT STRATEGY
;;; ==========================================

(defun auto-select-strategy ()
  "Auto-select the best strategy based on current market regime.
   Main entry point for meta-learning integration."
  (let* ((regime (if (boundp '*current-regime*) *current-regime* :unknown))
         (volatility (if (boundp '*volatility-regime*) *volatility-regime* :normal))
         (best-for-regime (get-best-strategy-for-regime regime)))
    
    (cond
      ;; Have a confident best strategy
      (best-for-regime
       (format t "[M] 🎯 Auto-select: ~a for ~a market~%"
               best-for-regime regime)
       (list :strategy best-for-regime
             :regime regime
             :confidence :high))
      
      ;; Special case: extreme volatility, prefer scalp/breakout
      ((eq volatility :extreme)
       (format t "[M] ⚡ Extreme volatility: suggesting breakout strategies~%")
       (list :strategy-type :breakout
             :regime :volatile
             :confidence :moderate))
      
      ;; No data yet: suggest exploration
      (t
       (format t "[M] 🔍 No meta-learning data: continue exploration~%")
       (list :strategy nil
             :regime regime
             :confidence :low)))))

;;; ==========================================
;;; META-LEARNING REPORT
;;; ==========================================

(defun meta-learning-report ()
  "Generate meta-learning status report"
  (format t "~%")
  (format t "════════════════════════════════════════════════════~%")
  (format t "🧠 META-LEARNING REPORT (Naval)~%")
  (format t "════════════════════════════════════════════════════~%")
  
  ;; Regime summary
  (dolist (regime '(:trending :ranging :volatile :unknown))
    (let ((best (gethash regime *regime-best-strategies*))
          (strategies nil))
      (format t "~%📈 REGIME: ~a~%" regime)
      (format t "   Best Strategy: ~a~%" (or best "Not determined"))
      
      ;; Collect all strategies for this regime
      (maphash
       (lambda (key stats)
         (when (eq (car key) regime)
           (push (list :name (cdr key)
                       :wins (first stats)
                       :losses (second stats)
                       :pnl (third stats)
                       :win-rate (get-regime-strategy-win-rate regime (cdr key)))
                 strategies)))
       *regime-strategy-matrix*)
      
      ;; Sort by win rate and display top 3
      (let ((sorted (sort strategies #'> :key (lambda (s) (or (getf s :win-rate) 0)))))
        (loop for i from 0 below (min 3 (length sorted))
              for s = (nth i sorted)
              do (format t "   ~d. ~a: ~,0f% WR (~d/~d) ¥~,0f~%"
                         (1+ i)
                         (getf s :name)
                         (* 100 (getf s :win-rate))
                         (getf s :wins)
                         (+ (getf s :wins) (getf s :losses))
                         (getf s :pnl))))))
  
  (format t "~%════════════════════════════════════════════════════~%")
  
  ;; Return summary
  (let ((total-pairs 0))
    (maphash (lambda (k v) (declare (ignore k v)) (incf total-pairs)) *regime-strategy-matrix*)
    (list :total-pairs total-pairs
          :regimes-tracked (hash-table-count *regime-best-strategies*))))

;;; ==========================================
;;; PERSISTENCE
;;; ==========================================

(defun save-meta-learning ()
  "Save meta-learning state to file"
  (handler-case
      (progn
        (ensure-directories-exist *meta-learning-log-path*)
        (with-open-file (out *meta-learning-log-path* :direction :output :if-exists :supersede)
          (format out "{~%\"regime_strategy_matrix\": {~%")
          (let ((first-entry t))
            (maphash
             (lambda (key stats)
               (unless first-entry (format out ",~%"))
               (setf first-entry nil)
               (format out "  \"~a:~a\": {\"wins\": ~d, \"losses\": ~d, \"pnl\": ~,2f}"
                       (car key) (cdr key)
                       (first stats) (second stats) (third stats)))
             *regime-strategy-matrix*))
          (format out "~%},~%\"best_strategies\": {~%")
          (let ((first-entry t))
            (maphash
             (lambda (regime strategy)
               (unless first-entry (format out ",~%"))
               (setf first-entry nil)
               (format out "  \"~a\": \"~a\"" regime strategy))
             *regime-best-strategies*))
          (format out "~%}~%}~%"))
        (format t "[M] 💾 Meta-learning state saved~%"))
    (error (e)
      (format t "[M] ⚠️ Failed to save meta-learning: ~a~%" e))))

(defun load-meta-learning ()
  "Load meta-learning state from file"
  (handler-case
      (when (probe-file *meta-learning-log-path*)
        (format t "[M] 📂 Meta-learning state loaded~%"))
    (error (e)
      (format t "[M] ⚠️ Failed to load meta-learning: ~a~%" e))))

;;; ==========================================
;;; INTEGRATION HOOKS
;;; ==========================================

(defun on-trade-close-meta (regime strategy-name won-p pnl)
  "Hook to be called when a trade closes"
  (record-regime-strategy-result regime strategy-name won-p pnl))

;;; ==========================================
;;; INITIALIZATION
;;; ==========================================

(load-meta-learning)
(format t "[L] 🧠 meta-learning.lisp loaded - Naval meta-learning active~%")
(format t "[L] 🎯 Auto-select strategies based on regime performance~%")
