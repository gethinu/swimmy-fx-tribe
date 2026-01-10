;; V5.2: Warrior ID System - 16 Global Slots (4 clans x 4 warriors)
;; V6.2 (Graham): Gotobi/Kelly/WhyLog moved to school-fortress.lisp
;; ============================================================

(defparameter *warrior-allocation* (make-hash-table :test 'equal))

(defun get-clan-id (category)
  "Get numeric ID for clan (used in Magic Number calculation)"
  (case category
    (:hunters 10) (:shamans 20) (:breakers 30) (:raiders 40) (t 90)))

(defun get-warrior-magic (category index)
  "Generate unique Magic Number for warrior: BASE + CLAN*10 + INDEX"
  (+ 123456 (* (get-clan-id category) 10) index))

(defun find-free-warrior-slot (category)
  "Find first available warrior slot (0-3) for the clan, returns nil if full"
  (loop for i from 0 to 3
        for key = (format nil "~a-~d" category i)
        when (null (gethash key *warrior-allocation*))
        return i))

(defun execute-category-trade (category direction symbol bid ask)
  (format t "[TRACE] execute-category-trade ~a ~a symbol=~a bid=~a ask=~a~%" category direction symbol bid ask)
  (format t "[TRACE] Conditions: numberp-bid=~a numberp-ask=~a exposure-ok=~a~%" (numberp bid) (numberp ask) (total-exposure-allowed-p))
  (handler-case
    (when (and (numberp bid) (numberp ask) (total-exposure-allowed-p))  ; Safety + exposure check
    (let* ((strategies (gethash category *active-team*))
           (lead-strat (first strategies))
           (lead-name (when lead-strat (strategy-name lead-strat)))
           (rank-data (when lead-name (get-strategy-rank lead-name)))
           (rank (if rank-data (strategy-rank-rank rank-data) :scout))
           (rank-mult (calculate-rank-multiplier rank))  ; V2.0: Rank-based lot multiplier
           (base-lot (get-category-lot category))
           (history (gethash symbol *candle-histories*))
           ;; V2.0 Research Paper #18: Volatility-scaled lot size
           (vol-scaled-lot (if (and (fboundp 'volatility-scaled-lot) history)
                               (volatility-scaled-lot base-lot history)
                               base-lot))
           ;; V3.0: Apply volatility multiplier (previously unused!)
           (vol-mult (handler-case (get-volatility-lot-multiplier)
                       (error () 1.0)))
           ;; V3.0: Consider risk-parity lot (previously unused!)
           (rp-lot (handler-case (get-risk-parity-lot category)
                     (error () base-lot)))
           ;; V5.2 Research Paper #34: HDRL portfolio risk adjustment
           (hdrl-lot (handler-case (hdrl-adjusted-lot symbol base-lot)
                       (error () base-lot)))
           ;; V5.7 (Thorp): Kelly Criterion adjustment
           (kelly-adj (if lead-name (get-strategy-kelly-lot lead-name base-lot) base-lot))
           ;; Final lot: min of all adjustments, then apply rank multiplier
           (lot (max 0.01 (* rank-mult vol-mult 
                             (min (correlation-adjusted-lot symbol vol-scaled-lot) rp-lot hdrl-lot kelly-adj))))
           ;; V3.0: Track positions by strategy (not by category) for multi-position support
           (conf (or (and (boundp '*last-confidence*) *last-confidence*) 0.0))
           (pred (or (and (boundp '*last-prediction*) *last-prediction*) "HOLD"))
           (swarm-consensus (or (and (boundp '*last-swarm-consensus*) *last-swarm-consensus*) 0))
           (sl-pips 0.15) (tp-pips 0.40)
           (warmup-p (< *category-trades* 50))  ; First 50 trades = warmup
           ;; V2.1: Conditions for High Council convening
           (large-lot-p (> lot 0.05))      ; Large position needs council approval
           (high-rank-p (member rank '(:veteran :legend)))  ; High rank strategy
           (danger-p (and (boundp '*danger-level*) (> *danger-level* 2))))  ; High danger
      ;; Ensure conf is a number
      (setf conf (if (numberp conf) conf 0.0))
      
      ;; ════════════════════════════════════════════════════════════════════
      ;; P0 STARTUP SAFEGUARDS (Expert Panel 2026-01-07)
      ;; ════════════════════════════════════════════════════════════════════
      
      ;; P0-1: WARMUP GUARD - Block ALL trades during warmup period
      (let ((now (get-universal-time)))
        ;; Transition from warmup to trading when time expires
        (when (and (eq *system-state* :warmup) 
                   (> now *warmup-end-time*))
          (setf *system-state* :trading)
          (format t "[L] ✅ P0 WARMUP COMPLETE: Trading ENABLED~%"))
        
        ;; Block trades during warmup
        (when (eq *system-state* :warmup)
          (format t "[L] ⏳ P0 WARMUP: Trade blocked (~d sec remaining)~%" 
                  (- *warmup-end-time* now))
          (return-from execute-category-trade nil))
        
        ;; P0-2: ENTRY RATE LIMIT - Max 1 entry per second
        (when (< (- now *last-entry-time*) *min-entry-interval-seconds*)
          (format t "[L] ⚡ P0 RATE LIMIT: Too fast, skipping (last entry ~a sec ago)~%"
                  (- now *last-entry-time*))
          (return-from execute-category-trade nil))
        
        ;; P0-3: STARTUP POSITION LIMIT - Max 1 position during first 60 sec after warmup
        (let ((startup-period-end (+ *warmup-end-time* 60)))
          (when (and (< now startup-period-end)
                     (> (hash-table-count *warrior-allocation*) 0))
            (format t "[L] 🛡️ P0 STARTUP LIMIT: Max 1 position during startup~%")
            (return-from execute-category-trade nil)))
            
        ;; ════════════════════════════════════════════════════════════════════
        ;; P1 FAILURE SAFETY (Dynamic Circuit Breaker)
        ;; ════════════════════════════════════════════════════════════════════
        (when *circuit-breaker-active*
          (if (> now *breaker-cooldown-end*)
              (progn
                (setf *circuit-breaker-active* nil)
                (setf *recent-losses* nil)
                (format t "[L] ✅ CIRCUIT BREAKER RESET: Cooldown expired. Resuming trading.~%"))
              (progn
                (format t "[L] ⚡ CIRCUIT BREAKER ACTIVE: Trading HALTED for ~ds~%"
                        (- *breaker-cooldown-end* now))
                (return-from execute-category-trade nil)))))
      
      ;; V2.1: Convene High Council for important decisions
      (when (and (not warmup-p)
                 (or large-lot-p high-rank-p danger-p)
                 (fboundp 'convene-high-council))
        (let* ((proposal (format nil "~a ~a ~,2f lot (~a戦略: ~a)"
                                 symbol direction lot rank lead-name))
               (urgency (cond (danger-p :critical)
                              (large-lot-p :high)
                              (t :normal)))
               (council-result (convene-high-council proposal category :urgency urgency)))
          ;; If council rejects, skip the trade
          (when (eq council-result :rejected)
            (format t "[L] 🏛️ HIGH COUNCIL REJECTED: ~a~%" proposal)
            (return-from execute-category-trade nil))))
      
      ;; Check for failure pattern BLOCK
      (unless (should-block-trade-p symbol direction category)
        ;; P0 HARDENED: ALL trades MUST pass prediction filter (no warmup bypass)
        (let* ((prediction (handler-case (predict-trade-outcome symbol direction)
                             (error (e) (progn (format t "[L] Prediction error: ~a~%" e) nil))))
               ;; P0: NO BYPASS - prediction filter is MANDATORY
               (should-trade (if prediction 
                                 (should-take-trade-p prediction)
                                 nil)))  ;; No prediction = no trade
          ;; Explain decision
          (when prediction
            (handler-case
                (let ((factors (trade-prediction-factors prediction))
                      (action (if should-trade :execute :skip)))
                  (explain-trade-decision symbol direction action factors))
              (error (e) (format t "[L] Explain error: ~a~%" e))))
          
          ;; V5.5 (Soros): Global Panic Protocol
          (when (global-panic-active-p)
            (format t "[L] 💉 SOROS: Elevated volatility - trading with reduced size~%"))
          
          ;; V5.5 (Darwin): Unlearning Check
          (when (should-unlearn-p symbol)
            (setf should-trade nil)
            (format t "[L] 🧬 DARWIN: Recent performance toxic. Unlearning active.~%"))

          ;; V5.6 (Paper #36): Parallel Verification Loops
          (when should-trade
            (unless (verify-parallel-scenarios symbol direction category)
              (setf should-trade nil)
              (format t "[L] 🧬 PARALLEL VERIFICATION: FAILED (Trade rejected)~%")))
              
            (when should-trade
              ;; V5.5 (Sun Tzu): Operation Mist (Entry Jitter & Feint)
              (sleep (/ (random 2000) 1000.0)) ; 0-2s delay
              (when (< (random 100) 5)     ; 5% Feint chance
                 (format t "[L] ⚔️ SUN TZU: Operation Mist - Feint executed (Trade skipped)~%")
                 (return-from execute-category-trade nil))
              
              ;; V5.2: Warrior Allocation - Find free slot (0-3) for this clan
              (let ((slot-index (find-free-warrior-slot category)))
                (if slot-index
                     ;; Execute trade with unique Magic Number
                    (let* ((magic (get-warrior-magic category slot-index))
                           (key (format nil "~a-~d" category slot-index))
                           ;; V8.5 (Panel Decision): Tiered lot sizing based on Sharpe
                           ;; Sharpe 0.0 ~ 0.3 = cap at 0.01 (proving ground)
                           (strat (or (find lead-name *evolved-strategies* :key #'strategy-name :test #'string=)
                                      (find lead-name *strategy-knowledge-base* :key #'strategy-name :test #'string=)))
                           (sharpe (if strat (or (strategy-sharpe strat) 0.0) 0.0))
                           (lot (if (and (>= sharpe 0.0) (< sharpe 0.3))
                                    (progn
                                      (format t "[L] 📊 TIERED LOT: ~a (Sharpe ~,2f) capped at 0.01~%" lead-name sharpe)
                                      0.01)
                                    lot)))
                      (format t "[TRACE] Slot found: ~d Magic: ~d Direction: ~a (eq :buy? ~a)~%" slot-index magic direction (eq direction :buy))
                      (cond
                        ((eq direction :buy)
                         (let ((sl (- bid sl-pips)) (tp (+ bid tp-pips)))
                           ;; V5.7 (Feynman): Why Log
                           (log-why-trade symbol :buy category 
                                         :strategy lead-name 
                                         :tribe-cons (if (boundp '*tribe-consensus*) *tribe-consensus* 0)
                                         :swarm-cons swarm-consensus
                                         :parallel-score 2
                                         :elder-ok (not (should-block-trade-p symbol :buy category)))
                           ;; USE SAFE-ORDER for centralized risk check
                           (when (safe-order "BUY" symbol lot sl tp magic)
                             (setf (gethash key *warrior-allocation*) 
                                   (list :symbol symbol :category category :direction :long :entry bid :magic magic :lot lot :start-time (get-universal-time)))
                             (update-symbol-exposure symbol lot :open)
                             (incf *category-trades*)
                             ;; P0: Update last entry time for rate limiting
                             (setf *last-entry-time* (get-universal-time))
                             (format t "[L] ⚔️ WARRIOR #~d DEPLOYED: ~a -> ~a BUY (Magic ~d)~%" (1+ slot-index) category symbol magic)
                             ;; V8.1: Discord Notification
                             (swimmy.shell:notify-discord-symbol symbol 
                               (format nil "⚔️ **WARRIOR DEPLOYED**~%Strategy: ~a~%Action: BUY ~a~%Lot: ~,2f~%Magic: ~d" 
                                       lead-name symbol lot magic)
                               :color 3066993))))
                        ((eq direction :sell)
                         (let ((sl (+ ask sl-pips)) (tp (- ask tp-pips)))
                           ;; USE SAFE-ORDER for centralized risk check
                           (when (safe-order "SELL" symbol lot sl tp magic)
                             (setf (gethash key *warrior-allocation*) 
                                   (list :symbol symbol :category category :direction :short :entry ask :magic magic :lot lot :start-time (get-universal-time)))
                             (update-symbol-exposure symbol lot :open)
                             (incf *category-trades*)
                             ;; P0: Update last entry time for rate limiting
                             (setf *last-entry-time* (get-universal-time))
                             (format t "[L] ⚔️ WARRIOR #~d DEPLOYED: ~a -> ~a SELL (Magic ~d)~%" (1+ slot-index) category symbol magic)
                             ;; V8.1: Discord Notification
                             (swimmy.shell:notify-discord-symbol symbol 
                               (format nil "⚔️ **WARRIOR DEPLOYED**~%Strategy: ~a~%Action: SELL ~a~%Lot: ~,2f~%Magic: ~d" 
                                       lead-name symbol lot magic)
                               :color 15158332))))))
                    ;; Clan is full (4/4 warriors deployed)
                    (format t "[L] ⚠️ Clan ~a is fully deployed (4/4 warriors)!~%" category))))))))
    (error (e) (format t "[TRACE] 🚨 ERROR in execute-category-trade: ~a~%" e))))

;; Track entry prices for each category
(defparameter *category-entries* (make-hash-table :test 'eq))

(defun close-category-positions (symbol bid ask)
  "V5.2: Close warrior positions at SL/TP using warrior-allocation"
  (maphash 
   (lambda (key warrior)
     (when (and warrior (equal (getf warrior :symbol) symbol))
       (let* ((category (getf warrior :category))
              (pos (getf warrior :direction))
              (entry (getf warrior :entry))
               (magic (getf warrior :magic))
               (lot (or (getf warrior :lot) 0.01)) ; Use stored lot or default
               (sl-pips 0.15) (tp-pips 0.40)
               (pnl 0) (closed nil))
         (when (and entry (numberp bid) (numberp ask))
           (cond
             ((eq pos :long)
              (let ((sl (- entry sl-pips)) (tp (+ entry tp-pips)))
                (when (or (<= bid sl) (>= bid tp))
                  (setf pnl (- bid entry) closed t))))
             ((eq pos :short)
              (let ((sl (+ entry sl-pips)) (tp (- entry tp-pips)))
                (when (or (>= ask sl) (<= ask tp))
                  (setf pnl (- entry ask) closed t)))))
           (when closed
             ;; Send CLOSE with Magic Number
             (pzmq:send *cmd-publisher* (jsown:to-json (jsown:new-js ("action" "CLOSE") ("symbol" symbol) ("magic" magic))))
             ;; Free the slot
             (remhash key *warrior-allocation*)
             (update-symbol-exposure symbol lot :close)
             (format t "[L] ⚔️ WARRIOR #~d RETURNS: ~a (~a) PnL: ~5f~%" (1+ (parse-integer (subseq (string key) (1+ (position #\- (string key)))))) category (if (> pnl 0) "WIN" "LOSS") pnl)
             (incf *daily-pnl* (round (* pnl 1000 100)))
             (record-trade-result (if (> pnl 0) :win :loss))
             (record-trade-outcome symbol (if (eq pos :long) :buy :sell) category "Warriors" pnl)
             (let ((lead-strat (first (gethash category *active-team*))))
               (when lead-strat
                 (record-strategy-trade (strategy-name lead-strat) (if (> pnl 0) :win :loss) pnl)))
             (format t "[L] 🏁 WARRIOR RETURNED (~a): ~a pips~%" category (round (* pnl 100)))
             (contribute-to-treasury category pnl :trade (format nil "Trade ~a" (if (> pnl 0) "win" "loss")))
             (notify-discord-symbol symbol (format nil "~a ~a closed ~,2f" (if (> pnl 0) "✅" "❌") category pnl) 
                            :color (if (> pnl 0) 3066993 15158332)))))))
   *warrior-allocation*))


;; trading-allowed-p removed - now in risk-manager.lispenabled*

;; Global consensus tracker
(defvar *last-swarm-consensus* 0)

(defun process-category-trades (symbol bid ask)
  "Process trades using Swarm Intelligence, Memory System, Danger Avoidance, AND Research Insights"
  (when (and (trading-allowed-p) *candle-history* (> (length *candle-history*) 100))
    ;; First check if any positions need to be closed
    (close-category-positions symbol bid ask)
    
    ;; ===== DANGER AVOIDANCE: Check if safe to trade =====
    (unless (is-safe-to-trade-p)
      (return-from process-category-trades nil))
    
    ;; ===== VOLATILITY CHECK: Extreme volatility blocks trading =====
    (unless (volatility-allows-trading-p)
      (return-from process-category-trades nil))
    
    ;; ===== RESEARCH ENHANCED ANALYSIS (Auto-integrated Paper Insights) =====
    (let ((research-analysis nil))
      (when (>= (length *candle-history*) 50)
        (setf research-analysis (research-enhanced-analysis *candle-history*))
        ;; Apply volatility-based model selection
        (select-optimal-model *candle-history*)
        ;; Detect HMM regime
        (detect-regime-hmm *candle-history*))
    
    ;; ===== SWARM INTELLIGENCE: Collect votes from all strategies =====
    (let* ((swarm-decision (swarm-trade-decision symbol *candle-history*))
           (consensus (swarm-decision-consensus-strength swarm-decision))
           (swarm-direction (swarm-decision-direction swarm-decision))
           ;; ===== MEMORY SYSTEM: Check past experience =====
           (memory-suggestion (memory-suggests-direction symbol))
           (memory-confidence (if memory-suggestion 
                                  (get-memory-confidence symbol memory-suggestion)
                                  0.5))
           ;; ===== RESEARCH: Dual trend agreement check =====
           (dual-trend (when research-analysis (getf research-analysis :dual-trend)))
           (trend-agrees (or (null dual-trend)
                            (not (listp dual-trend))
                            (eq (getf dual-trend :agreement) :aligned)
                            (eq (getf dual-trend :direction) 
                                (case swarm-direction (:BUY :UP) (:SELL :DOWN) (t :FLAT))))))
      
      ;; Log collective decision with research enhancement
      (format t "[L] 🐟🐟🐟 SWARM: ~a (~,0f% consensus)~%" swarm-direction (* 100 consensus))
      (when (and dual-trend (listp dual-trend))
        (format t "[L] 📊 RESEARCH: ~a trend ~a~%" 
                (getf dual-trend :direction)
                (if trend-agrees "✓ agrees" "⚠ diverges")))
      
      ;; Store consensus for trade execution (NEW)
      (setf *last-swarm-consensus* consensus)
      
      (when memory-suggestion
        (format t "[L] 🧠 MEMORY suggests: ~a (~,0f% confidence)~%" 
                memory-suggestion (* 100 memory-confidence)))
      
      ;; ===== LEADER FISH: Check leader's opinion =====
      (elect-leader)  ; Update leader based on current performance
      (let ((boosted-decision (get-leader-boosted-decision swarm-decision)))
        (setf swarm-decision boosted-decision))
      
      ;; ===== UNIFIED DECISION MAKING (with Research Enhancement) =====
      ;; V3.0: 61 strategies are the ONLY entry source (removed 4-clan hardcoded signals)
      (handler-case
        (let* ((min-consensus-to-trade 0.25)
               (any-strong-signal nil))
        ;; V3.0: Always use 61-STRATEGY SIGNALS (no more tribe-signals check)
        (when (or t  ; Always proceed - strategies have their own conditions
                 (> consensus min-consensus-to-trade))
            ;; V3.0: Use 61-STRATEGY SIGNALS
            (progn
              (format t "[L] 🎯 61-STRATEGY SIGNAL SCAN~%")
              (let ((strat-signals (collect-strategy-signals symbol *candle-history*)))
                (setf any-strong-signal (and strat-signals t))
                (when strat-signals
                  (format t "[L] 📊 ~d strategies triggered signals~%" (length strat-signals))
                  ;; Group by category and pick best for each clan
                  (let ((by-category (make-hash-table :test 'eq)))
                    ;; Group signals by category
                    (dolist (sig strat-signals)
                      (let ((cat (getf sig :category)))
                        (push sig (gethash cat by-category))))
                    ;; Trade TOP 3 strategies per clan (not just 1)
                    (dolist (category '(:trend :reversion :breakout :scalp))
                      (let ((cat-sigs (gethash category by-category)))
                        (when cat-sigs
                          ;; Take up to 3 strategies per clan
                          (let ((top-sigs (subseq cat-sigs 0 (min 4 (length cat-sigs)))))
                            (dolist (sig top-sigs)
                              (let* ((strat-name (getf sig :strategy-name))
                                     (direction (getf sig :direction))
                                     ;; Per-strategy cooldown
                                     (strat-key (intern (format nil "~a-~a" category strat-name) :keyword)))
                                ;; Check per-strategy cooldown (not per-clan)
                                (when (can-clan-trade-p strat-key)
                                  (let ((narrative (generate-dynamic-narrative sig symbol bid)))
                                    ;; Log the dynamic narrative
                                    (format t "~a~%" narrative)
                                    ;; Send to Discord
                                    (handler-case
                                        (swimmy.shell:notify-discord-symbol symbol narrative :color (if (eq direction :buy) 3066993 15158332))
                                      (error (e) (format t "[L] Discord error: ~a~%" e)))
                                    ;; Record trade time for this strategy
                                    (record-clan-trade-time strat-key)
                                    ;; Execute the trade
                                    (format t "[TRACE] Calling execute-category-trade for ~a~%" category)
                                    (execute-category-trade category direction symbol bid ask)
                                    ;; Record for rank promotion
                                    (record-strategy-trade strat-name :trade 0)))))))))))))
            
            ;; No trade - explain why
            (cond
              ((not any-strong-signal)
               (format t "[L] ⏸️ HOLD: No strong signals~%"))
              ((< consensus min-consensus-to-trade)
               (format t "[L] ⏸️ HOLD: Weak consensus (~,0f%)~%" (* 100 consensus)))
              ((not trend-agrees)
               (format t "[L] ⏸️ HOLD: Research trend divergence~%")))))
        (error (e) nil))))))  ;; Suppress tribe processing errors

(defun force-recruit-strategy (name)
  "Forcefully recruit a strategy from knowledge base into active service (Special Forces)"
  (let ((strat (find name *strategy-knowledge-base* :key #'strategy-name :test #'string=)))
    (if strat
        (progn
          ;; Add to evolved strategies if not present
          (pushnew strat *evolved-strategies* :test #'string= :key #'strategy-name)
          ;; Add to category pools
          (let ((cat (categorize-strategy strat)))
             ;; Ensure the category list exists or update it
            (setf (gethash cat *category-pools*) 
                  (cons strat (remove (strategy-name strat) (gethash cat *category-pools*) 
                                    :key #'strategy-name :test #'string=))))
          (format t "[L] 🎖️ Special Force Recruited: ~a~%" name)
          t)
        (format t "[L] ⚠️ Special Force NOT FOUND: ~a~%" name))))

(defun recruit-special-forces ()
  (force-recruit-strategy "T-Nakane-Gotobi"))

(defun init-school ()
  (build-category-pools)
  (recruit-special-forces) ; V7.0: Inject special forces
  (clrhash *category-positions*)
  (format t "[SCHOOL] Swimmy School ready (Special Forces Active)~%"))

;;; ══════════════════════════════════════════════════════════════════
;;;  V3.0: 4氏族シグナル関数を削除
;;;  61戦略が唯一のエントリーロジック源 (collect-strategy-signals)
;;;  各戦略はinfer-strategy-categoryで氏族(:trend,:reversion,:breakout,:scalp)に配属
;;;  Kalman/HMM等の研究論文実装は品質フィルターとして使用
;;; ══════════════════════════════════════════════════════════════════

;; V6.3 (Graham): TRIBES stubs removed - collect-all-tribe-signals, aggregate-tribe-signals
;; were dead code always returning nil/0%. Trade decisions now use SWARM only.

;;; ══════════════════════════════════════════════════════════════════
;;;  V4.0: STRATEGY CORRELATION ANALYSIS (教授指摘)
;;;  目的: 冗長な戦略を特定して除去
;;; ══════════════════════════════════════════════════════════════════

(defparameter *strategy-signal-history* (make-hash-table :test 'equal))
(defparameter *strategy-correlation-cache* nil)

(defun record-strategy-signal (strategy-name direction timestamp)
  "Record strategy signal for correlation analysis"
  (let* ((key strategy-name)
         (history (gethash key *strategy-signal-history* nil))
         (signal (list timestamp direction)))
    (push signal history)
    ;; Keep only last 100 signals
    (when (> (length history) 100)
      (setf history (subseq history 0 100)))
    (setf (gethash key *strategy-signal-history*) history)))

(defun calculate-signal-correlation (name1 name2)
  "Calculate correlation between two strategies' signals"
  (let ((h1 (gethash name1 *strategy-signal-history*))
        (h2 (gethash name2 *strategy-signal-history*)))
    (if (and h1 h2 (> (length h1) 10) (> (length h2) 10))
        (let ((matches 0)
              (comparisons 0))
          ;; Compare signals within same time windows
          (dolist (s1 h1)
            (let ((t1 (first s1))
                  (d1 (second s1)))
              (dolist (s2 h2)
                (let ((t2 (first s2))
                      (d2 (second s2)))
                  ;; Same minute window
                  (when (< (abs (- t1 t2)) 60)
                    (incf comparisons)
                    (when (eq d1 d2)
                      (incf matches)))))))
          (if (> comparisons 0)
              (float (/ matches comparisons))
              0.0))
        0.0)))

(defun analyze-strategy-correlation ()
  "Analyze all strategies for correlation and identify redundant ones"
  (let ((strategies (mapcar (lambda (s) (strategy-name s)) *strategy-knowledge-base*))
        (high-corr nil))
    (format t "~%[L] 🔬 V4.0: STRATEGY CORRELATION ANALYSIS~%")
    (format t "[L] Analyzing ~d strategies...~%" (length strategies))
    
    ;; Compare all pairs
    (loop for i from 0 below (length strategies)
          for s1 = (nth i strategies) do
          (loop for j from (1+ i) below (length strategies)
                for s2 = (nth j strategies) do
                (let ((corr (calculate-signal-correlation s1 s2)))
                  (when (> corr 0.85)
                    (push (list s1 s2 corr) high-corr)
                    (format t "[L] ⚠️ High correlation (~,0f%): ~a ↔ ~a~%"
                            (* 100 corr) s1 s2)))))
    
    (setf *strategy-correlation-cache* high-corr)
    
    (if high-corr
        (format t "[L] 📊 Found ~d highly correlated pairs~%" (length high-corr))
        (format t "[L] ✅ No highly correlated strategies found~%"))
    
    high-corr))

(defun get-redundant-strategies ()
  "Get list of strategies that might be redundant"
  (unless *strategy-correlation-cache*
    (analyze-strategy-correlation))
  (let ((redundant nil))
    (dolist (pair *strategy-correlation-cache*)
      (let ((s1 (first pair))
            (s2 (second pair)))
        ;; Keep the one with better name (shorter, more descriptive)
        (if (< (length s1) (length s2))
            (pushnew s2 redundant :test 'equal)
            (pushnew s1 redundant :test 'equal))))
    redundant))

;;; ══════════════════════════════════════════════════════════════════
;;;  V5.8 (Graham): STRATEGY PRUNING - Remove redundant strategies
;;; ══════════════════════════════════════════════════════════════════

(defun prune-redundant-strategies ()
  "Remove strategies with >85% correlation (Graham's simplicity)"
  (let ((redundant (get-redundant-strategies))
        (removed 0))
    (when redundant
      (format t "[L] 🧹 GRAHAM: Pruning ~d redundant strategies...~%" (length redundant))
      (dolist (name redundant)
        (let ((strat (find name *strategy-knowledge-base* 
                          :key #'strategy-name :test #'string=)))
          (when strat
            (setf *strategy-knowledge-base* 
                  (remove strat *strategy-knowledge-base*))
            (incf removed)
            (format t "[L]   ✂️ Removed: ~a~%" name))))
      (format t "[L] 🧹 GRAHAM: Removed ~d strategies. Remaining: ~d~%" 
              removed (length *strategy-knowledge-base*)))
    removed))

(defun count-strategies-by-type ()
  "Count strategies by indicator type for analysis"
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (strat *strategy-knowledge-base*)
      (let* ((indicators (strategy-indicators strat))
             (types (mapcar (lambda (ind) 
                             (if (listp ind) (car ind) ind)) 
                           indicators)))
        (dolist (type types)
          (incf (gethash (symbol-name type) counts 0)))))
    (format t "[L] 📊 Strategy Type Distribution:~%")
    (maphash (lambda (k v) (format t "[L]   ~a: ~d~%" k v)) counts)
    counts))

(init-school)
;;; ══════════════════════════════════════════════════════════════════
;;;  V6.0: Load Fortress Module (Graham's Simplicity)
;;;  Features: V5.5 (Global Panic, Unlearning), V5.6 (Parallel Verification)
;;;            V5.7 (Kelly, Why Log), V5.8 (Gotobi, Pruning), High Council
;;; ══════════════════════════════════════════════════════════════════
;; (load (merge-pathnames "school-fortress.lisp" (directory-namestring *load-truename*)))

