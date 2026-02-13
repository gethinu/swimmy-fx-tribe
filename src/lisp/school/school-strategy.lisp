;; school-strategy.lisp - Swimmy School: Strategy & Category Management
;; Extracted from school.lisp (V6.14) for better modularity

(in-package :swimmy.school)

;;; ══════════════════════════════════════════════════════════════════
;;;  CATEGORY DISPLAY HELPERS
;;; ══════════════════════════════════════════════════════════════════

(defun get-category-display (category-id)
  "Get display string for category"
  (string-upcase (symbol-name category-id)))

(defun calculate-strategy-hash (strat)
  "Generate a stable hash string for strategy logic (Indicators/Entry/Exit).
   Now uses structural canonicalization (Phase 31)."
  (let* ((entry (canonicalize-logic (strategy-entry strat)))
         (exit (canonicalize-logic (strategy-exit strat)))
         (indicators (canonicalize-logic (strategy-indicators strat)))
         (canonical (prin1-to-string (list entry exit indicators))))
    (format nil "~x" (sxhash canonical))))

;; announce-trade removed in V3.0 - duplicate notification (narrative already sent)


;;; ══════════════════════════════════════════════════════════════════
;;;  HIERARCHY SYSTEM (階級制度)
;;; ══════════════════════════════════════════════════════════════════
;;; Warriors (戦士) vs Incubators (育成) - 実弾 vs テスト
;;; *strategy-ranks* defined in school-state.lisp (V41.4: removed duplicate)

(defun get-strategy-rank (name)
  "Get or create rank for strategy"
  (or (gethash name *strategy-ranks*)
      (setf (gethash name *strategy-ranks*)
            (make-strategy-rank
             :name name
             :rank :incubator  ; All start as Incubators
             :trades 0 :wins 0 :total-pnl 0
             :promotion-date nil :last-trade nil))))

(defun calculate-rank-multiplier (rank)
  "Get lot multiplier based on rank"
  (case rank
    ((:incubator :scout) 0.25)   ; 25% - Learning (legacy :scout alias)
    (:warrior  1.00)   ; 100% - Full combat
    (:veteran  1.25)   ; 125% - Proven
    (:legend   1.50)   ; 150% - Hall of Fame material
    (otherwise 0.50)))

(defparameter *score-min-warrior* 0.20
  "Minimum composite score for Incubator -> Warrior promotion.")
(defparameter *score-min-veteran* 0.25
  "Minimum composite score for Warrior -> Veteran promotion.")
(defparameter *score-min-legend* 0.30
  "Minimum composite score for Veteran -> Legend promotion.")
(defparameter *score-demote-threshold* 0.05
  "Composite score threshold for demotion back to Incubator.")

(defun check-promotion (strategy-name)
  "Check if strategy deserves promotion (Composite Score)"
  (let ((rank-data (get-strategy-rank strategy-name)))
    (let* ((trades (strategy-rank-trades rank-data))
           (wins (strategy-rank-wins rank-data))
           (pnl (strategy-rank-total-pnl rank-data))
           (win-rate (if (> trades 0) (/ wins trades) 0))
           (current-rank (strategy-rank-rank rank-data))
           ;; V7.1: Resolve strategy to check Sharpe Ratio
           (strat (or (find strategy-name *strategy-knowledge-base* :key #'strategy-name :test #'string=)
                      (find strategy-name *evolved-strategies* :key #'strategy-name :test #'string=)))
           (sharpe (if strat (or (strategy-sharpe strat) 0.0) 0.0))
           (score (if (and strat (fboundp 'score-from-metrics))
                      (score-from-metrics
                       (list :sharpe (strategy-sharpe strat)
                             :profit-factor (strategy-profit-factor strat)
                             :win-rate (strategy-win-rate strat)
                             :max-dd (strategy-max-dd strat)))
                      sharpe)))
      
      ;; Promotion criteria (Composite score to reduce Sharpe bias)
      (cond
        ;; Incubator → Warrior: 10+ trades, 40%+ win rate, positive PnL, composite score
        ((and (member current-rank '(:scout :incubator))
              (>= trades 10)
              (>= win-rate 0.40)
              (> pnl 0)
              (>= score *score-min-warrior*))
         (setf (strategy-rank-rank rank-data) :warrior)
         (setf (strategy-rank-promotion-date rank-data) (get-universal-time))
         ;; Coming of Age ceremony
         (coming-of-age strategy-name "Incubator" "Warrior")
         :warrior)
        
        ;; Warrior → Veteran: 50+ trades, 50%+ win rate, 500+ PnL, composite score
        ((and (eq current-rank :warrior)
              (>= trades 50)
              (>= win-rate 0.50)
              (> pnl 500)
              (>= score *score-min-veteran*))
         (setf (strategy-rank-rank rank-data) :veteran)
         (setf (strategy-rank-promotion-date rank-data) (get-universal-time))
         (coming-of-age strategy-name "Warrior" "Veteran")
         :veteran)
        
        ;; Veteran → Legend: 30+ trades, 55%+ win rate, 500+ PnL, composite score
        ((and (eq current-rank :veteran)
              (>= trades 30)
              (>= win-rate 0.55)
              (> pnl 500)
              (>= score *score-min-legend*))
         (setf (strategy-rank-rank rank-data) :legend)
         (setf (strategy-rank-promotion-date rank-data) (get-universal-time))
         (coming-of-age strategy-name "Veteran" "Legend")
         ;; Induct to Hall of Fame
         (induct-to-hall-of-fame strategy-name pnl current-rank 
                                  (format nil "WR ~,0f%% | Score ~,2f" (* 100 win-rate) score))
         :legend)
        
        ;; Demotion: 5+ consecutive losses at Warrior level OR composite score degrades
        ((and (member current-rank '(:warrior :veteran))
              (or (< pnl -300)
                  (and (> trades 20) (< score *score-demote-threshold*))))  ; Significant degradation
         (setf (strategy-rank-rank rank-data) :incubator)
         (format t "[L] ⚠️ ~a demoted to Incubator due to poor performance (Score: ~,2f)~%" strategy-name score)
         :incubator)
        
        (t current-rank)))))

(defun record-strategy-trade (strategy-name outcome pnl)
  "Record trade and update rank"
  (let ((rank-data (get-strategy-rank strategy-name)))
    (incf (strategy-rank-trades rank-data))
    (when (eq outcome :win) (incf (strategy-rank-wins rank-data)))
    (incf (strategy-rank-total-pnl rank-data) pnl)
    (setf (strategy-rank-last-trade rank-data) (get-universal-time))
    ;; Check for promotion
    (check-promotion strategy-name)
    (save-strategy-ranks))) ; V46.1: Auto-save ranks


;;; ==========================================
;;; CATEGORY ALLOCATION
;;; ===========================================

(defparameter *category-allocation*
  '((:trend . 0.40) (:reversion . 0.30) (:breakout . 0.20) (:scalp . 0.10)))

(defparameter *slots-per-category*
  '((:trend . 2) (:reversion . 2) (:breakout . 1) (:scalp . 1)))

;; *category-pools* moved to school-state.lisp for early initialization
(defparameter *active-team* (make-hash-table :test 'equal))

(defun make-category-key (strat)
  "V47.8: Category Key = (Timeframe Direction Symbol)"
  (list (strategy-timeframe strat)
        (strategy-direction strat)
        (strategy-symbol strat)))

(defun categorize-strategy (strat)
  "V47.8: Categorize strategy by TF x Direction x Symbol.
   Returns a list key e.g. (60 :BUY \"USDJPY\")"
  (if (and (fboundp 'strategy-timeframe) 
           (fboundp 'strategy-direction) 
           (fboundp 'strategy-symbol))
      (make-category-key strat)
      (progn
        (format t "⚠️ Legacy Strategy found: ~a. Defaulting to (60 :BOTH \"USDJPY\")~%" (strategy-name strat))
        (list 60 :BOTH "USDJPY"))))

(defun build-category-pools ()
  (clrhash *category-pools*)
  (dolist (strat *strategy-knowledge-base*)
    (let ((cat (categorize-strategy strat)))
      (push strat (gethash cat *category-pools* nil)))))

;;; ============================================================================
;;; TEAM SELECTION & RECRUITMENT (Moved from school-execution.lisp for SRP)
;;; ============================================================================

(defun get-regime-weights ()
  "V49.1: Granular Regime Enforcement (Musk/López de Prado).
   Maps 7 market states to the 4 core categories."
  (let* ((effective-regime (or *predicted-regime* *current-regime*))
         (effective-volatility (or *predicted-volatility* *volatility-regime*))
         (base-weights 
           (case effective-regime
             ;; Trending States
             ((:trend-early :trend-mature)
              '((:trend . 0.50) (:breakout . 0.30) (:reversion . 0.10) (:scalp . 0.10)))
             (:trend-exhausted
              '((:reversion . 0.60) (:trend . 0.10) (:breakout . 0.10) (:scalp . 0.20)))
             
             ;; Range States
             (:range-expansion 
              '((:breakout . 0.50) (:reversion . 0.20) (:trend . 0.10) (:scalp . 0.20)))
             (:range-compression
              '((:reversion . 0.60) (:scalp . 0.20) (:trend . 0.10) (:breakout . 0.10)))
             
             ;; Extreme / Illiquid States
             ((:volatile-spike :illiquid)
              '((:scalp . 0.80) (:trend . 0.05) (:reversion . 0.05) (:breakout . 0.10)))
              
             (otherwise *category-allocation*))))
    ;; Adjust for volatility
    (case effective-volatility
      (:high  ; High volatility: reduce all, favor scalping/breakout
       (mapcar (lambda (cw) 
                 (let ((cat (car cw)) (w (cdr cw)))
                   (cons cat (* w (if (member cat '(:scalp :breakout)) 1.2 0.5)))))
               base-weights))
      (:low   ; Low volatility: increase positions, favor reversion
       (mapcar (lambda (cw) 
                 (let ((cat (car cw)) (w (cdr cw)))
                   (cons cat (* w (if (eq cat :reversion) 1.5 0.8)))))
               base-weights))
      (otherwise base-weights))))

;; V49.2: Data-Driven Tactical Mapping (Fowler/Musk Recommendation)
(defparameter *regime-tactics*
  '((:trend-early     :indicators ((adx 14) (sma 20))    :wisdom "Early trend detected. Focus on breakouts and momentum.")
    (:trend-mature    :indicators ((sma 50) (sma 200))   :wisdom "Mature trend. Use trailing stops and long-term averages.")
    (:trend-exhausted :indicators ((rsi 14) (bollinger 20)) :wisdom "Trend exhausted. Watch for mean reversion and overbought/oversold.")
    (:range-expansion :indicators ((bollinger 20) (atr 14)) :wisdom "Range expanding. Trade the volatility and breakout levels.")
    (:range-compression :indicators ((rsi 14) (stochastic 5)) :wisdom "Range compressing. Scalp the edges and watch for low-vol triggers.")
    (:volatile-spike  :indicators ((atr 14) (rsi 2))      :wisdom "High volatility. Tighten stops, focus on extreme reversions.")
    (:illiquid       :indicators ((sma 5) (vwap 1))      :wisdom "Illiquid market. Be cautious, focus on short-term price action."))
  "Maps market regimes to recommended indicators and tactical wisdom.")

(defun get-regime-tactics (&optional regime)
  "Retrieve tactical wisdom for the current or specified regime."
  (let ((r (or regime (when (boundp '*predicted-regime*) *predicted-regime*) (when (boundp '*current-regime*) *current-regime*))))
    (cdr (assoc r *regime-tactics*))))

(defun select-best-from-pool (category n)
  (let* ((pool (gethash category *category-pools*))
         (sorted (sort (copy-list pool) #'> 
                       :key (lambda (s) (or (strategy-sharpe s) -999)))))
    (subseq sorted 0 (min n (length sorted)))))

(defun infer-strategy-category (strat)
  "Infer strategy category from name/indicators AND TP/SL values"
  (let ((name (string-downcase (strategy-name strat)))
        (tp (strategy-tp strat)))
    (cond
      ;; Breakout strategies
      ((or (search "breakout" name) (search "squeeze" name) (search "low-vol" name)) :breakout)
      ;; Reversion strategies
      ((or (search "oversold" name) (search "overbought" name) (search "reversal" name) 
           (search "bounce" name) (search "reversion" name)) :reversion)
      ;; Scalp: tight TP (under 0.30 = 30 pips) and specific keywords
      ((and (<= tp 0.30) (or (search "scalp" name) (search "pop" name) (search "1m" name))) :scalp)
      ;; Everything else is trend
      (t :trend))))
;; P8: recruit-from-evolution DELETED - redundant KB entry point

(defun assemble-team ()
  ;; P8: recruit-from-evolution call removed
  (detect-market-regime)
  (record-regime)          ; Track for pattern analysis
  (predict-next-regime)    ; Forecast next regime
  (let ((weights (get-regime-weights)))
    (clrhash *active-team*)
    (dolist (cat-weight weights)
      (let* ((cat (car cat-weight))
             (slots (cdr (assoc cat *slots-per-category*)))
             (best (select-best-from-pool cat slots)))
        (setf (gethash cat *active-team*) best)))))

(defun safe-notify-discord-recruit (msg &key color)
  (if (fboundp 'notify-discord-recruit)
      (notify-discord-recruit msg :color color)
      (format t "[DISCORD] ~a~%" msg)))
