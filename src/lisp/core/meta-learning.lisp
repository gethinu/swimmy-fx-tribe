;;; src/lisp/core/meta-learning.lisp
;;; META-LEARNING v2.0 (最高品質メタ学習)
;;; ==========================================
;;; Features:
;;; - Sharpe ratio tracking per regime
;;; - Risk-adjusted lot sizing (Kelly criterion inspired)
;;; - Adaptive learning rate based on confidence
;;; - Historical performance decay
;;; - Multi-factor performance optimization


(in-package :swimmy.school)

(defparameter *nn-wins* 0)
(defparameter *nn-losses* 0)

(defstruct performance-stats
  wins
  losses
  total-pnl
  pnl-squared-sum    ; For variance calculation
  max-drawdown
  win-streak
  lose-streak
  current-streak
  last-updated)

(defparameter *regime-performance* (make-hash-table :test 'equal))
(defparameter *learning-rate* 0.1)        ; How fast to adapt
(defparameter *performance-decay* 0.99)   ; Decay old data slightly
(defparameter *min-trades-for-adjust* 10) ; Minimum trades before adjusting
(defparameter *best-strategy-by-regime* (make-hash-table :test 'eq)
  "Regime -> best strategy reference (manual/optional).")

(defun get-best-strategy-for-regime (regime)
  "Return the best-known strategy for REGIME (or NIL)."
  (gethash regime *best-strategy-by-regime*))

(defun update-best-strategy-for-regime (regime &optional strategy)
  "Update best strategy mapping for REGIME when STRATEGY is provided."
  (when strategy
    (setf (gethash regime *best-strategy-by-regime*) strategy))
  (gethash regime *best-strategy-by-regime*))

(defun save-meta-learning ()
  "Persist meta-learning state (placeholder)."
  nil)

(defun get-or-create-stats (key)
  "Get or initialize performance stats for a key"
  (or (gethash key *regime-performance*)
      (setf (gethash key *regime-performance*)
            (make-performance-stats 
             :wins 0 :losses 0 :total-pnl 0.0
             :pnl-squared-sum 0.0 :max-drawdown 0.0
             :win-streak 0 :lose-streak 0 :current-streak 0
             :last-updated (get-universal-time)))))

(defun record-regime-performance (won pnl)
  "Track comprehensive performance in current market conditions"
  (let* ((key (format nil "~a:~a" *current-regime* *volatility-regime*))
         (stats (get-or-create-stats key)))
    ;; Update basic stats
    (if won
        (progn
          (incf (performance-stats-wins stats))
          (if (> (performance-stats-current-streak stats) 0)
              (incf (performance-stats-current-streak stats))
              (setf (performance-stats-current-streak stats) 1))
          (setf (performance-stats-win-streak stats)
                (max (performance-stats-win-streak stats)
                     (performance-stats-current-streak stats))))
        (progn
          (incf (performance-stats-losses stats))
          (if (< (performance-stats-current-streak stats) 0)
              (decf (performance-stats-current-streak stats))
              (setf (performance-stats-current-streak stats) -1))
          (setf (performance-stats-lose-streak stats)
                (max (performance-stats-lose-streak stats)
                     (abs (performance-stats-current-streak stats))))))
    
    ;; Update PnL stats for Sharpe calculation
    (incf (performance-stats-total-pnl stats) pnl)
    (incf (performance-stats-pnl-squared-sum stats) (* pnl pnl))
    
    ;; Track drawdown
    (when (< pnl 0)
      (setf (performance-stats-max-drawdown stats)
            (min (performance-stats-max-drawdown stats) pnl)))
    
    (setf (performance-stats-last-updated stats) (get-universal-time))
    (setf (gethash key *regime-performance*) stats)))

(defun calculate-regime-sharpe (regime volatility)
  "Calculate Sharpe ratio for a specific regime/volatility combo"
  (let* ((key (format nil "~a:~a" regime volatility))
         (stats (gethash key *regime-performance*)))
    (if (and stats (> (+ (performance-stats-wins stats) 
                         (performance-stats-losses stats)) 5))
        (let* ((n (+ (performance-stats-wins stats) (performance-stats-losses stats)))
               (mean-pnl (/ (performance-stats-total-pnl stats) n))
               (variance (- (/ (performance-stats-pnl-squared-sum stats) n)
                           (* mean-pnl mean-pnl)))
               (std-dev (if (> variance 0) (sqrt variance) 1.0)))
          (/ mean-pnl (max std-dev 0.001)))
        0.0)))

(defun calculate-kelly-fraction (regime volatility)
  "Calculate Kelly criterion inspired position sizing"
  (let* ((key (format nil "~a:~a" regime volatility))
         (stats (gethash key *regime-performance*)))
    (if (and stats (> (+ (performance-stats-wins stats) 
                         (performance-stats-losses stats)) 10))
        (let* ((wins (performance-stats-wins stats))
               (losses (performance-stats-losses stats))
               (total (+ wins losses))
               (avg-win (if (> wins 0) 
                           (/ (max 0 (performance-stats-total-pnl stats)) wins)
                           0.01))
               (avg-loss (if (> losses 0)
                            (/ (abs (min 0 (performance-stats-total-pnl stats))) losses)
                            0.01))
               (win-rate (/ wins total))
               (win-loss-ratio (/ avg-win (max avg-loss 0.001)))
               ;; Kelly formula: f = (bp - q) / b where b=win/loss ratio, p=win rate, q=1-p
               (kelly (/ (- (* win-loss-ratio win-rate) (- 1 win-rate)) 
                        (max win-loss-ratio 0.001))))
          ;; Half-Kelly for safety, clamped to reasonable range
          (max 0.25 (min 1.5 (* kelly 0.5))))
        1.0)))  ; Default to full size if not enough data

(defun get-regime-win-rate (regime volatility)
  "Get win rate for specific regime/volatility combo"
  (let* ((key (format nil "~a:~a" regime volatility))
         (stats (gethash key *regime-performance*)))
    (if (and stats (> (+ (performance-stats-wins stats) 
                         (performance-stats-losses stats)) 0))
        (let ((total (+ (performance-stats-wins stats) 
                        (performance-stats-losses stats))))
          (/ (performance-stats-wins stats) total))
        0.5)))

(defun meta-adjust-lot (base-lot)
  "Advanced lot size adjustment based on meta-learned performance"
  (let* ((sharpe (calculate-regime-sharpe *current-regime* *volatility-regime*))
         (kelly (calculate-kelly-fraction *current-regime* *volatility-regime*))
         (win-rate (get-regime-win-rate *current-regime* *volatility-regime*))
         (key (format nil "~a:~a" *current-regime* *volatility-regime*))
         (stats (gethash key *regime-performance*))
         (streak (if stats (performance-stats-current-streak stats) 0)))
    
    ;; Multi-factor adjustment
    (let ((multiplier 1.0))
      ;; Sharpe-based adjustment
      (cond
        ((> sharpe 1.0) (setf multiplier (* multiplier 1.3)))   ; Great Sharpe
        ((> sharpe 0.5) (setf multiplier (* multiplier 1.1)))   ; Good Sharpe
        ((< sharpe -0.5) (setf multiplier (* multiplier 0.6)))  ; Bad Sharpe
        ((< sharpe 0) (setf multiplier (* multiplier 0.8))))    ; Negative Sharpe
      
      ;; Kelly adjustment (blend with 1.0)
      (setf multiplier (* multiplier (+ (* 0.5 kelly) 0.5)))
      
      ;; Win rate adjustment
      (cond
        ((> win-rate 0.65) (setf multiplier (* multiplier 1.15)))
        ((< win-rate 0.35) (setf multiplier (* multiplier 0.7))))
      
      ;; Streak adjustment (reduce after losses, increase after wins)
      (cond
        ((< streak -3) (setf multiplier (* multiplier 0.5)))   ; Losing streak
        ((> streak 3) (setf multiplier (* multiplier 1.2))))   ; Winning streak
      
      ;; Log significant adjustments
      (when (or (> multiplier 1.15) (< multiplier 0.85))
        (format t "[L] 🧠 META-LOT: ~,2fx (Sharpe:~,2f Kelly:~,2f WR:~,0f% Streak:~d)~%"
                multiplier sharpe kelly (* 100 win-rate) streak))
      
      (* base-lot (max 0.3 (min 2.0 multiplier))))))

(defun decay-old-performance ()
  "Apply decay to old performance data to adapt to changing markets"
  (maphash (lambda (key stats)
             (declare (ignore key))
             ;; Slightly decay old stats
             (setf (performance-stats-wins stats) 
                   (floor (* (performance-stats-wins stats) *performance-decay*)))
             (setf (performance-stats-losses stats)
                   (floor (* (performance-stats-losses stats) *performance-decay*)))
             (setf (performance-stats-total-pnl stats)
                   (* (performance-stats-total-pnl stats) *performance-decay*))
             (setf (performance-stats-pnl-squared-sum stats)
                   (* (performance-stats-pnl-squared-sum stats) *performance-decay*)))
           *regime-performance*))

(defun update-nn-threshold (won)
  "Update NN threshold based on comprehensive performance analysis"
  (if won (incf *nn-wins*) (incf *nn-losses*))
  (let ((total (+ *nn-wins* *nn-losses*)))
    (when (> total *min-trades-for-adjust*)
      (let* ((win-rate (/ *nn-wins* total))
             (sharpe (calculate-regime-sharpe *current-regime* *volatility-regime*))
             ;; Adjust threshold based on both win rate and Sharpe
             (adjustment (* *learning-rate* 
                           (+ (* 0.5 (- 0.5 win-rate))  ; Win rate component
                              (* 0.5 (- 0 sharpe))))))  ; Sharpe component
        (cond
          ((> win-rate 0.6)
           (setf *nn-threshold* (max 0.30 (- *nn-threshold* (* adjustment 2))))
           (format t "[L] 📈 WR:~,0f% Sharpe:~,2f → threshold:~,2f~%" 
                   (* win-rate 100) sharpe *nn-threshold*))
          ((< win-rate 0.4)
           (setf *nn-threshold* (min 0.80 (+ *nn-threshold* (* (abs adjustment) 2))))
           (format t "[L] 📉 WR:~,0f% Sharpe:~,2f → threshold:~,2f~%" 
                   (* win-rate 100) sharpe *nn-threshold*))))))
  ;; Record for meta-learning
  (record-regime-performance won 0))

(defun get-meta-learning-summary ()
  "Get summary of meta-learning state"
  (let ((summaries nil))
    (maphash (lambda (key stats)
               (let ((total (+ (performance-stats-wins stats) 
                              (performance-stats-losses stats))))
                 (when (> total 5)
                   (push (format nil "~a: WR=~,0f% Sharpe=~,2f"
                                 key
                                 (* 100 (/ (performance-stats-wins stats) total))
                                 (calculate-regime-sharpe 
                                  (intern (subseq key 0 (position #\: key)) :keyword)
                                  (intern (subseq key (1+ (position #\: key))) :keyword)))
                         summaries))))
             *regime-performance*)
    (or summaries (list "No data yet"))))
