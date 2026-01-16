;; strategies.lisp - Historical Strategy Knowledge Base
;; 歴史的に有効な戦略パターンのナレッジベース（100+種類）
;; Refactored (Expert Panel 2026-01-13) - Strategies split into subfiles

(in-package :swimmy.school)

(defparameter *strategy-knowledge-base* nil)

(defun init-knowledge-base ()
  "Initialize with historically proven strategies"
  (setf *strategy-knowledge-base*
        (append (get-trend-strategies)
                (get-reversion-strategies)
                (get-breakout-strategies)
                (get-scalp-strategies)))
  
  (format t "[L] 📚 Knowledge base loaded: ~d strategies~%" 
          (length *strategy-knowledge-base*)))

;; ===== Sharpe フィルター;; Thresholds
(defparameter *min-sharpe-threshold* 0.0 "Minimum Sharpe to be adopted/kept (Taleb's Rule: Block negative EV)")
(defparameter *min-win-rate-threshold* 0.4 "Minimum Win Rate")
(defparameter *approved-strategies* nil)

(defun filter-by-sharpe (strategies)
  "Filter strategies with Sharpe > threshold"
  (let ((approved (remove-if-not 
                    (lambda (s) (and (strategy-sharpe s) (> (strategy-sharpe s) *min-sharpe-threshold*)))
                    strategies)))
    (format t "[L] 🔍 Filtered: ~d/~d passed Sharpe > ~,1f~%"
            (length approved) (length strategies) *min-sharpe-threshold*)
    approved))

;; V19.2: Round-Robin Backtest Cursor (Musk's Decision)
(defparameter *backtest-cursor* 0 "Cursor for round-robin backtesting")

(defun batch-backtest-knowledge ()
  "Backtest knowledge base strategies with Round-Robin Pagination (V19.2)"
  (load-backtest-cache) 
  (setf *backtest-results-buffer* nil)
  (setf *expected-backtest-count* (length *strategy-knowledge-base*))
  
  (let* ((max-batch-size 200) ; Limit concurrent requests (Increased to 200 via P5.1)
         (total (length *strategy-knowledge-base*))
         (start-idx (mod *backtest-cursor* total))
         (end-idx (min total (+ start-idx max-batch-size)))
         (batch-strategies (subseq *strategy-knowledge-base* start-idx end-idx))
         ;; Handle wrap-around if needed
         (wrap-strategies (if (< (- end-idx start-idx) max-batch-size)
                              (subseq *strategy-knowledge-base* 0 (min total (- max-batch-size (- end-idx start-idx))))
                              nil))
         (final-batch (append batch-strategies wrap-strategies))
         (cached-count 0)
         (requested-count 0))
    
    ;; Update cursor for next time
    (setf *backtest-cursor* (mod (+ start-idx (length final-batch)) total))
        
    (format t "[L] 🧪 Batch testing ~d strategies (Round-Robin: ~d -> ~d)...~%" 
            (length final-batch) start-idx (mod (+ start-idx (length final-batch)) total))
    
    ;; Snapshot history to prevent ID drift (P5.1 Fix)
    (let ((snapshot *candle-history*))
      (dolist (strat final-batch)
        (when (and snapshot (> (length snapshot) 100))
          (let ((cached (get-cached-backtest (strategy-name strat))))
            (if cached
                (incf cached-count)
                (progn 
                  (incf requested-count)
                  (request-backtest strat :candles snapshot)))))))
                
    (format t "[L] 🏁 Batch Request Complete. Cached: ~d, Queued: ~d (Cursor: ~d)~%" 
            cached-count requested-count *backtest-cursor*)
    
    ;; Notify Discord only if actual requests were made
    (when (> requested-count 0)
      (notify-discord-alert 
        (format nil "🧪 **Backtest Batch Queued (RR)**\n- Requested: ~d / ~d\n- Cursor: ~d / ~d" 
                requested-count total *backtest-cursor* total) 
        :color 3066993))))

(defun adopt-proven-strategies ()
  "Adopt only strategies that passed Sharpe filter"
  (setf *approved-strategies* (filter-by-sharpe *strategy-knowledge-base*))
  (when *approved-strategies*
    (format t "[L] ✅ Adopted ~d proven strategies~%" (length *approved-strategies*))
    (dolist (s *approved-strategies*)
      (unless (find (strategy-name s) *evolved-strategies* :key #'strategy-name :test #'string=)
        (push s *evolved-strategies*))))
  (when *evolved-strategies*
    (setf *evolved-strategies* 
          (sort *evolved-strategies* #'> 
                :key (lambda (s) (or (strategy-sharpe s) 0))))))

;; Auto-initialize
(init-knowledge-base)

;;; ==========================================
;;; V7.9++: INDICATOR TYPE INFERENCE (Sharpe=-3.75 Bug Fix)
;;; Infer indicator_type from strategy indicators for correct backtesting
;;; ==========================================

(defun infer-indicator-type (strategy)
  "Infer the primary indicator type from strategy indicators.
   This fixes the Sharpe=-3.75 bug where all strategies defaulted to SMA."
  (let* ((indicators (strategy-indicators strategy))
         (first-indicator (first indicators))
         (indicator-name (when first-indicator 
                           (string-downcase (symbol-name (first first-indicator))))))
    (cond
      ;; MACD strategies
      ((and indicator-name (search "macd" indicator-name)) "macd")
      ;; RSI strategies (including elder, momentum)
      ((and indicator-name (search "rsi" indicator-name)) "rsi")
      ;; Stochastic strategies
      ((and indicator-name (search "stoch" indicator-name)) "stoch")
      ;; Bollinger Band strategies
      ((and indicator-name (search "bb" indicator-name)) "bb")
      ;; EMA strategies
      ((and indicator-name (search "ema" indicator-name)) "ema")
      ;; Default to SMA
      (t "sma"))))

(defun apply-indicator-types ()
  "Apply inferred indicator types to all strategies in knowledge base."
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (strat *strategy-knowledge-base*)
      (let ((ind-type (infer-indicator-type strat)))
        (setf (strategy-indicator-type strat) ind-type)
        (incf (gethash ind-type counts 0))))
    ;; Log the distribution
    (format t "[STRATEGIES] Indicator types assigned:~%")
    (maphash (lambda (k v) (format t "  ~a: ~a strategies~%" k v)) counts)))

;; Apply indicator types after initialization
(apply-indicator-types)

(format t "[STRATEGIES] ~d strategies loaded from Knowledge Base~%" 
        (length *strategy-knowledge-base*))

;;; ==========================================
;;; BENCH SYSTEM (V5.1 Restored)
;;; ==========================================

(defparameter *benched-strategies* (make-hash-table :test 'equal))

(defun strategy-benched-p (name)
  "Check if strategy is benched"
  (gethash name *benched-strategies*))

(defun bench-strategy (name reason)
  "Bench a strategy with reason"
  (setf (gethash name *benched-strategies*) reason)
  (format t "[L] 🚫 BENCHED: ~a (~a)~%" name reason))

(defun unbench-all ()
  "Unbench all strategies"
  (clrhash *benched-strategies*)
  (format t "[L] ♻️  All strategies unbenched for new week~%"))

(defun should-weekly-unbench-p ()
  "Check if it is Monday morning (before 9am) to reset bench"
  (multiple-value-bind (s m h d mo y dow) (decode-universal-time (get-universal-time))
    (declare (ignore s m d mo y))
    (and (= dow 0) (< h 9))))

(defun weekly-unbench-all ()
  "Weekly unbench wrapper"
  (unbench-all))

(defun current-trading-session ()
  "Determine current market session (JST based approximation)"
  (let ((h (nth 2 (multiple-value-list (decode-universal-time (get-universal-time))))))
    (cond 
      ((and (>= h 9) (< h 15)) :asian)
      ((and (>= h 16) (< h 21)) :london)
      ((or (>= h 22) (< h 6)) :ny)
      (t :mixed))))

;;; ==========================================
;;; STRATEGY PERFORMANCE EVALUATION
;;; ==========================================

(defun evaluate-strategy-performance (strat sharpe trades win-rate)
  "Adjust strategy parameters based on backtest performance"
  (let ((name (strategy-name strat)))
    (when (> trades 10)
      ;; CRITICAL FAILURE CHECK (Bench System)
      (when (and (> trades 30) (or (< sharpe -1.5) (< win-rate 30)))
        (bench-strategy name (format nil "Critical Failure: S=~,2f WR=~,0f%" sharpe win-rate))
        (return-from evaluate-strategy-performance))
      
      (cond
        ;; Poor performance: tighten SL, reduce volume
        ((or (< sharpe 0) (< win-rate 40))
         (when (strategy-sl strat)
           (setf (strategy-sl strat) (* 0.9 (strategy-sl strat))))
         (when (strategy-volume strat)
           (setf (strategy-volume strat) (max 0.01 (* 0.8 (strategy-volume strat)))))
         (format t "[L] ⚙️ 📉 ~a: Tightening params (poor perf)~%" name))
        ;; Good performance: widen TP, increase volume (Aggressive V7.0)
        ((and (> sharpe 1.0) (> win-rate 55))
         (when (strategy-tp strat)
           (setf (strategy-tp strat) (* 1.1 (strategy-tp strat))))
         (when (strategy-volume strat)
           ;; Scale faster: 1.2x -> 1.5x, Cap 0.1 -> 0.3
           (setf (strategy-volume strat) (min 0.3 (* 1.5 (strategy-volume strat)))))
         (format t "[L] ⚙️ 📈 🚀 ~a: Aggressive Expansion (Good Perf)~%" name))
        ;; Average: adjust SL/TP ratio for better risk/reward
        ((and (> sharpe 0.5) (> win-rate 45))
         (when (and (strategy-sl strat) (strategy-tp strat))
           (let ((rr (/ (strategy-tp strat) (max 0.01 (strategy-sl strat)))))
             (when (< rr 2.0)
               (setf (strategy-tp strat) (* 1.05 (strategy-tp strat)))
               (format t "[L] ⚙️ 🎯 ~a: Improving R:R ratio~%" name)))))))))
