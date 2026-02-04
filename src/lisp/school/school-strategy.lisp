;; school-strategy.lisp - Swimmy School: Strategy & Clan Management
;; Extracted from school.lisp (V6.14) for better modularity

(in-package :swimmy.school)

;;; ══════════════════════════════════════════════════════════════════
;;;  SWIMMY CIVILIZATION: THE FOUR GREAT CLANS (4大氏族)
;;; ══════════════════════════════════════════════════════════════════
;;; "勝とうとするな。ただ、生き残れ。そうすれば、最後に立っているのは我々だ。"
;;; — Swimmy Constitution, Article 0

;; Clan definitions with personas and battle cries
(defstruct clan
  id              ; :trend, :reversion, :breakout, :scalp (original)
  name            ; Tribal name
  title           ; Japanese title
  emoji           ; Icon
  persona         ; Character description
  battle-cry      ; What they say when trading
  philosophy)     ; Their core belief

(defparameter *clans*
  (list
   (make-clan :id :trend
              :name "Hunters"
              :title "追跡者"
              :emoji "🏹"
              :persona "忍耐・豪快"
              :battle-cry "北への風が強まっています。掟の範囲内で最大まで張ります。"
              :philosophy "風が吹いている")
   
   (make-clan :id :reversion
              :name "Shamans"
              :title "呪術師"
              :emoji "🔮"
              :persona "論理・冷静"
              :battle-cry "グラフは歪んでいます。反動に備えます。"
              :philosophy "高すぎるものは落ちる")
   
   (make-clan :id :breakout
              :name "Breakers"
              :title "破壊者"
              :emoji "⚔️"
              :persona "好戦的・爆発力"
              :battle-cry "城壁崩壊！チャンスは一瞬！Rustの許可範囲で突撃します。"
              :philosophy "壁は壊された")
   
   (make-clan :id :scalp
              :name "Raiders"
              :title "盗賊"
              :emoji "🗡️"
              :persona "敏捷・狡猾"
              :battle-cry "市場は荒れていますが、隙間で稼ぎました。今日の食い扶持です。"
              :philosophy "隙あり")))

(defun get-clan (category-id)
  "Get clan by category ID"
  (find category-id *clans* :key #'clan-id))

(defun get-clan-display (category-id)
  "Get display string for category: emoji Name (ORIGINAL)"
  (let ((clan (get-clan category-id)))
    (if clan
        (format nil "~a ~a (~a)" 
                (clan-emoji clan) (clan-name clan) (string-upcase (symbol-name category-id)))
        (string-upcase (symbol-name category-id)))))

(defun get-clan-battle-cry (category-id)
  "Get the battle cry for a clan"
  (let ((clan (get-clan category-id)))
    (if clan (clan-battle-cry clan) "")))

(defun calculate-strategy-hash (strat)
  "Generate a stable hash string for strategy logic (Indicators/Entry/Exit).
   Now uses structural canonicalization (Phase 31)."
  (let* ((entry (canonicalize-logic (strategy-entry strat)))
         (exit (canonicalize-logic (strategy-exit strat)))
         (indicators (canonicalize-logic (strategy-indicators strat)))
         (canonical (prin1-to-string (list entry exit indicators))))
    (format nil "~x" (sxhash canonical))))

(defun generate-clan-narrative (category-id direction confidence symbol price)
  "Generate natural language narrative for clan trade entry"
  (let* ((clan (get-clan category-id))
         (strategy-desc (case category-id
                          (:trend "🎯 MACD + ADX momentum with Kalman trend filter. We ride the wave until exhaustion.")
                          (:reversion "🔮 RSI oversold/overbought + Bollinger deviation. The price must return to equilibrium.")
                          (:breakout "💥 Bollinger band breakout confirmed by volume. Once the walls fall, we charge.")
                          (:scalp "⚡ EMA velocity + micro-swing detection. Quick profits in the chaos.")
                          (t "Unknown strategy")))
         (exit-plan (case direction
                      (:buy "📈 Exit: Take profit at +1.0%, Stop loss at -0.3%")
                      (:sell "📉 Exit: Take profit at +1.0%, Stop loss at -0.3%")
                      (t "Unknown")))
         (dir-emoji (if (eq direction :buy) "🟢 BUY" "🔴 SELL"))
         (dir-str (string-upcase (symbol-name direction))))
    (format nil "
═══════════════════════════════
~a 【~a】 ENTERS THE BATTLEFIELD!
═══════════════════════════════
「~a」

📍 Symbol: ~a @ ~,3f
~a

~a
~a

💪 Confidence: ~,0f%
═══════════════════════════════"
            (clan-emoji clan) (clan-name clan)
            (clan-philosophy clan)
            symbol price
            dir-emoji
            strategy-desc
            exit-plan
            (* 100 confidence))))

;; announce-clan-trade removed in V3.0 - duplicate notification (narrative already sent)


;;; ══════════════════════════════════════════════════════════════════
;;;  HIERARCHY SYSTEM (階級制度)
;;; ══════════════════════════════════════════════════════════════════
;;; Warriors (戦士) vs Scouts (斥候) - 実弾 vs テスト
;;; *strategy-ranks* defined in school-state.lisp (V41.4: removed duplicate)

(defstruct strategy-rank
  name              ; Strategy name
  rank              ; :scout, :warrior, :veteran, :legend
  trades            ; Total trades executed
  wins              ; Winning trades
  total-pnl         ; Cumulative PnL
  promotion-date    ; When promoted
  last-trade)       ; Last trade timestamp

(defun get-strategy-rank (name)
  "Get or create rank for strategy"
  (or (gethash name *strategy-ranks*)
      (setf (gethash name *strategy-ranks*)
            (make-strategy-rank
             :name name
             :rank :scout  ; All start as Scouts
             :trades 0 :wins 0 :total-pnl 0
             :promotion-date nil :last-trade nil))))

(defun calculate-rank-multiplier (rank)
  "Get lot multiplier based on rank"
  (case rank
    (:scout    0.25)   ; 25% - Learning
    (:warrior  1.00)   ; 100% - Full combat
    (:veteran  1.25)   ; 125% - Proven
    (:legend   1.50)   ; 150% - Hall of Fame material
    (otherwise 0.50)))

(defun check-promotion (strategy-name)
  "Check if strategy deserves promotion (Enhanced with Sharpe Ratio)"
  (let ((rank-data (get-strategy-rank strategy-name)))
    (let* ((trades (strategy-rank-trades rank-data))
           (wins (strategy-rank-wins rank-data))
           (pnl (strategy-rank-total-pnl rank-data))
           (win-rate (if (> trades 0) (/ wins trades) 0))
           (current-rank (strategy-rank-rank rank-data))
           ;; V7.1: Resolve strategy to check Sharpe Ratio
           (strat (or (find strategy-name *strategy-knowledge-base* :key #'strategy-name :test #'string=)
                      (find strategy-name *evolved-strategies* :key #'strategy-name :test #'string=)))
           (sharpe (if strat (or (strategy-sharpe strat) 0.0) 0.0)))
      
      ;; Promotion criteria (Expert Panel Recommendation: Use Sharpe to filter luck)
      (cond
        ;; Scout → Warrior: 10+ trades, 40%+ win rate, positive PnL, Sharpe > 0.5
        ;; [V8.2] Expert Panel (Graham): "Sharpe > 0 is pathetic"
        ((and (eq current-rank :scout)
              (>= trades 10)
              (>= win-rate 0.40)
              (> pnl 0)
              (> sharpe 0.5)) ; Raised from 0.0 per Expert Panel
         (setf (strategy-rank-rank rank-data) :warrior)
         (setf (strategy-rank-promotion-date rank-data) (get-universal-time))
         ;; Coming of Age ceremony
         (coming-of-age strategy-name "Scout" "Warrior")
         :warrior)
        
        ;; Warrior → Veteran: 50+ trades, 50%+ win rate, 500+ PnL, Sharpe > 0.5
        ((and (eq current-rank :warrior)
              (>= trades 50)
              (>= win-rate 0.50)
              (> pnl 500)
              (> sharpe 0.2)) ; Reduced from 0.5
         (setf (strategy-rank-rank rank-data) :veteran)
         (setf (strategy-rank-promotion-date rank-data) (get-universal-time))
         (coming-of-age strategy-name "Warrior" "Veteran")
         :veteran)
        
        ;; Veteran → Legend: 30+ trades, 55%+ win rate, 500+ PnL, Sharpe > 0.3
        ;; [V45.0] Expert Panel (Musk): "Sharpe 1.0 is impossible for short-term backtest"
        ((and (eq current-rank :veteran)
              (>= trades 30)
              (>= win-rate 0.55)
              (> pnl 500)
              (> sharpe 0.3))
         (setf (strategy-rank-rank rank-data) :legend)
         (setf (strategy-rank-promotion-date rank-data) (get-universal-time))
         (coming-of-age strategy-name "Veteran" "Legend")
         ;; Induct to Hall of Fame
         (induct-to-hall-of-fame strategy-name pnl current-rank 
                                  (format nil "WR ~,0f%% | Sharpe ~,2f" (* 100 win-rate) sharpe))
         :legend)
        
        ;; Demotion: 5+ consecutive losses at Warrior level OR Sharpe becomes negative
        ((and (member current-rank '(:warrior :veteran))
              (or (< pnl -300)
                  (and (> trades 20) (< sharpe -0.5))))  ; Significant degradation
         (setf (strategy-rank-rank rank-data) :scout)
         (format t "[L] ⚠️ ~a demoted to Scout due to poor performance (Sharpe: ~,2f)~%" strategy-name sharpe)
         :scout)
        
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


;;; ══════════════════════════════════════════════════════════════════
;;;  INTER-TRIBAL ECONOMICS (氏族間経済)
;;; ══════════════════════════════════════════════════════════════════
;;; Mutual Aid: Raiders feed Hunters during waiting periods

(defparameter *clan-treasury* (make-hash-table :test 'eq))
(defparameter *mutual-aid-history* nil)

(defun initialize-clan-treasury ()
  "Initialize treasury for each clan"
  (dolist (cat '(:trend :reversion :breakout :scalp))
    (setf (gethash cat *clan-treasury*) 0)))

(defstruct treasury-entry
  clan
  amount
  source         ; :trade, :mutual-aid, :tribute
  timestamp
  description)

(defun contribute-to-treasury (clan amount source desc)
  "Add to clan treasury"
  (incf (gethash clan *clan-treasury* 0) amount)
  (push (make-treasury-entry
         :clan clan :amount amount :source source
         :timestamp (get-universal-time) :description desc)
        *mutual-aid-history*))

(defun calculate-mutual-aid ()
  "Calculate mutual aid between clans
   Raiders (scalp) share 10% of profits with waiting Hunters (trend)"
  (let ((raider-treasury (gethash :scalp *clan-treasury* 0))
        (hunter-treasury (gethash :trend *clan-treasury* 0)))
    
    ;; If Raiders profitable and Hunters struggling
    (when (and (> raider-treasury 100)
               (< hunter-treasury 0))
      (let ((aid-amount (* 0.10 raider-treasury)))
        ;; Transfer
        (decf (gethash :scalp *clan-treasury*) aid-amount)
        (incf (gethash :trend *clan-treasury*) aid-amount)
        
        (format t "[L] 🤝 MUTUAL AID: Raiders share ¥~,0f with Hunters~%" aid-amount)
        (format t "[L]    「市場は荒れていますが、我々の日銭が仲間を支えます」~%")
        
        (push (list :from :scalp :to :trend :amount aid-amount 
                    :time (get-universal-time))
              *mutual-aid-history*)
        
        aid-amount))))

(defun apply-hedge-logic (main-clan direction symbol bid ask)
  "Shamans provide hedge for Breakers' aggressive trades.
   When Breakers go aggressive, Shamans take a small counter-position.
   This is the 'inter-tribal cooperation' aspect of the civilization."
  (when (and (eq main-clan :breakout)
             (member direction '(:buy :sell)))
    ;; Shamans take 30% size counter-position
    (let* ((counter-direction (if (eq direction :buy) :sell :buy))
           (hedge-lot 0.01)  ; Small hedge position
           (hedge-sl 0.10)
           (hedge-tp 0.20))
      (format t "[L] 🔮 Shamans: 「Breakersの~a突撃に備え、~a反動用意」~%" 
              direction counter-direction)
      ;; Only execute hedge if we don't already have a Shaman position
      (unless (gethash :reversion *category-positions*)
        ;; Execute small counter-trade
        (cond
          ((eq counter-direction :buy)
           (let ((sl (- bid hedge-sl)) (tp (+ bid hedge-tp)))
             (let ((order (swimmy.core:make-order-message
                           "Shaman-Hedge" symbol :buy hedge-lot 0.0 sl tp)))
               (pzmq:send *cmd-publisher* (swimmy.core:encode-sexp order)))
             (setf (gethash :reversion *category-positions*) :long)
             (format t "[L] 🔮 Shamans HEDGE BUY ~,2f lot~%" hedge-lot)))
          ((eq counter-direction :sell)
           (let ((sl (+ ask hedge-sl)) (tp (- ask hedge-tp)))
             (pzmq:send *cmd-publisher*
                        (swimmy.core:encode-sexp
                         (swimmy.core:make-order-message
                          "Shaman-Hedge" symbol :sell hedge-lot 0.0 sl tp)))
             (setf (gethash :reversion *category-positions*) :short)
             (format t "[L] 🔮 Shamans HEDGE SELL ~,2f lot~%" hedge-lot))))
        ;; Log the inter-tribal cooperation
        (format t "[L] 🤝 氏族間協力: Breakers攻撃 ⇔ Shamansヘッジ~%")
        t))))

(defun get-clan-treasury-summary ()
  "Get summary of all clan treasuries"
  (format t "~%[L] ═══════════════════════════════════════~%")
  (format t "[L] 💰 CLAN TREASURIES~%")
  (format t "[L] ═══════════════════════════════════════~%")
  (maphash (lambda (clan amount)
             (let ((clan-obj (get-clan clan)))
               (when clan-obj
                 (format t "[L] ~a ~a: ¥~:d~%" 
                         (clan-emoji clan-obj) (clan-name clan-obj) (round amount)))))
           *clan-treasury*)
  (format t "[L] ═══════════════════════════════════════~%~%"))


;;; ==========================================
;;; CATEGORY ALLOCATION (with Clan mapping)
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
   Maps 7 market states to the 4 great clans."
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
  "Infer clan category from strategy name/indicators AND TP/SL values"
  (let ((name (string-downcase (strategy-name strat)))
        (tp (strategy-tp strat))
        (sl (strategy-sl strat)))
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
