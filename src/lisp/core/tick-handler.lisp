(in-package :swimmy.main)

;;; ==========================================
;;; SWIMMY CORE: TICK HANDLER (tick-handler.lisp)
;;; ==========================================
;;; Contains the main tick processing loop, candle updates, and neural logic
;;; Extracted from brain.lisp (Strangler Fig Phase 2)

;;; ==========================================
;;; HELPER FUNCTIONS (Logic moved from brain.lisp)
;;; ==========================================

(defun has-resigned-p ()
  (or *has-resigned-today*
      (and *danger-cooldown-until* (> *danger-cooldown-until* (get-universal-time)))))

(defun get-date-string ()
  (multiple-value-bind (s m h d mo y) (decode-universal-time (get-universal-time))
    (declare (ignore s m h))
    (format nil "~D-~2,'0D-~2,'0D" y mo d)))

(defun get-time-string ()
  (multiple-value-bind (s m h) (decode-universal-time (get-universal-time))
    (format nil "~2,'0D:~2,'0D:~2,'0D" h m s)))

(defun train-nn-from-trade (symbol pnl direction)
  "Train NN from trade outcome - online learning"
  (when (and *candle-history* (> (length *candle-history*) 30))
    (handler-case
        (let* ((target (cond
                         ((> pnl 0.3) 0)   ;; Clear win -> UP correct
                         ((< pnl -0.3) 1)  ;; Clear loss -> DOWN correct
                         (t 2)))           ;; Small -> FLAT
               (candles-json (mapcar (lambda (c)
                                       (jsown:new-js
                                         ("open" (candle-open c))
                                         ("high" (candle-high c))
                                         ("low" (candle-low c))
                                         ("close" (candle-close c))
                                         ("volume" 1)))
                                     (subseq *candle-history* 0 (min 30 (length *candle-history*)))))
               (cmd (jsown:to-json (jsown:new-js
                      ("action" "TRAIN")
                      ("candles" candles-json)
                      ("target" target)))))
          (pzmq:send *cmd-publisher* cmd)
          (format t "[L] 🧠 NN TRAIN: target=~a (~a)~%" target 
                  (cond ((= target 0) "UP") ((= target 1) "DOWN") (t "FLAT"))))
      (error (e) (format t "[L] NN train error: ~a~%" e)))))

(defun run-periodic-maintenance ()
  "Handle periodic maintenance tasks (backtests, ecosystem, evolution)"
  (unless (and (boundp '*candle-histories*) *candle-histories*)
    (setf *candle-histories* (make-hash-table :test 'equal)))
  (let ((now (get-universal-time)))
    (request-prediction)
    ;; Batch backtest
    (when (and (not *initial-backtest-done*) *candle-history* (> (length *candle-history*) 1000))
      (setf *initial-backtest-done* t)
      (format t "~%[L] 🧪 Starting batch backtest of ~d strategies...~%" (length *strategy-knowledge-base*))
      (batch-backtest-knowledge))
    
    ;; Dream cycle
    (unless (numberp *last-dream-time*) (setf *last-dream-time* 0))
    (when (> (- now *last-dream-time*) *dream-interval*)
      (setf *last-dream-time* now)
      (when (fboundp 'assemble-team) (assemble-team))
      ;; Ecosystem health
      (let ((health (if (fboundp 'get-population-health) (get-population-health) 0.5)))
        (format t "[L] 🌿 ECOSYSTEM: Health=~,0f%~%" (* 100 health)))
      ;; Natural selection
      (when (zerop (mod *dream-cycle* 6))
        (format t "[L] 🌱 Running natural selection...~%")
        (maintain-ecosystem-balance))
      ;; Mutual aid
      (when (and (zerop (mod *dream-cycle* 3)) (fboundp 'calculate-mutual-aid))
        (calculate-mutual-aid))
      ;; Goal tracking
      (when (zerop (mod *dream-cycle* 3))
        (report-goal-status))
      ;; Evolution
      (check-evolution)
      (evolve-population)
      (incf *dream-cycle*)
      
      ;; V7.1: Persist learning state (Every 60 cycles ~ 1 hour)
      (when (and (zerop (mod *dream-cycle* 60)) (fboundp 'save-state))
        (funcall 'save-state)))
    
    ;; Discord Heartbeat for mobile monitoring (Naval)
    (when (fboundp 'check-discord-heartbeat)
      (check-discord-heartbeat))))

(defun process-tick-round-robin (symbol bid)
  "Optimize tick processing by handling one symbol per tick"
  (when (numberp bid)
    (let ((history (gethash symbol *candle-histories*)))
      (when (and history (> (length history) 50))
        (format t "[L] 🏛️ SWARM-ONLY mode (TRIBES removed V6.3)~%")))
        
    (let* ((symbol-count (length *supported-symbols*))
           (sym (nth *symbol-round-robin-index* *supported-symbols*)))
      (setf *symbol-round-robin-index* (mod (1+ *symbol-round-robin-index*) symbol-count))
      (let ((hist (gethash sym *candle-histories*))
            (curr-bid (if (string= sym symbol) bid 
                            (if (gethash sym *current-candles*) 
                                (candle-close (gethash sym *current-candles*)) 0))))
        (when (and hist (> (length hist) 50) (> curr-bid 0))
          (setf *candle-history* hist)
          (process-category-trades sym curr-bid (+ curr-bid 0.0002)))))))

(defun processing-step (symbol bid)
  (run-periodic-maintenance)
  (process-tick-round-robin symbol bid))

(defun update-candle (bid symbol)
  "Update candle history for a specific symbol - multi-currency support"
  (let* ((now (get-universal-time))
         (min-idx (floor now 60))
         (curr-candle (gethash symbol *current-candles*))
         (curr-minute (gethash symbol *current-minutes* -1))
         (history (gethash symbol *candle-histories*)))
    ;; New minute - save previous candle and process
    (when (and curr-candle (/= min-idx curr-minute))
      (push curr-candle (gethash symbol *candle-histories*))
      (setf *candle-history* (gethash symbol *candle-histories*))  ; Legacy compat
      (format t "[~a] ~a." (get-jst-str) (subseq symbol 0 3)) (force-output)
      (processing-step symbol bid)
      (setf (gethash symbol *current-candles*) nil))
    ;; Update or create candle
    (if (null (gethash symbol *current-candles*))
        (progn
          (setf (gethash symbol *current-minutes*) min-idx)
          (setf (gethash symbol *current-candles*) 
                (make-candle :timestamp now :open bid :high bid :low bid :close bid :volume 1)))
        (let ((c (gethash symbol *current-candles*)))
          (setf (candle-close c) bid)
          (incf (candle-volume c))
          (when (> bid (candle-high c)) (setf (candle-high c) bid))
          (when (< bid (candle-low c)) (setf (candle-low c) bid))))))

(defun get-flood-status ()
  "Convert Danger Level and Drawdown into a Flood Metaphor"
  (let ((danger (if (boundp '*danger-level*) *danger-level* 0))
        (dd (if (boundp '*max-drawdown*) *max-drawdown* 0.0)))
    (cond
      ((>= danger 5) "🌊🌊🌊 **TSUNAMI ALERT** (The Abyss)")
      ((>= danger 4) "🏊 **Underwater** (Oxygen Critical)")
      ((>= danger 3) "🚿 **Neck Deep** (Breathing Hard)")
      ((>= danger 2) "🩳 **Waist Deep** (Hard to Move)")
      ((>= danger 1) "👢 **Ankle Deep** (Wet Socks)")
      ((> dd 5.0)    "🌧️ **Heavy Rain** (Puddles Forming)")
      (t             "🏜️ **Dry Land** (Safe)"))))

(defun send-daily-tribal-narrative ()
  "Send a daily summary of tribal sentiments and results in Japanese with dynamic storytelling"
  (let* ((pnl *daily-pnl*)
         (wins *consecutive-wins*)
         (losses *consecutive-losses*)
         (tribe-dir (if (boundp '*tribe-direction*) *tribe-direction* "N/A"))
         ;; Generate dynamic quotes based on situation
         (hunter-quote (cond ((> pnl 0) "「獲物は十分に確保した。宴の準備を。」")
                             ((> losses 2) "「風向きが悪い...一度森へ退くぞ。」")
                             (t "「次の獲物を探して、矢を研いでおく。」")))
         (breaker-quote (cond ((equal tribe-dir "BUY") "「壁はずっと叩けば壊れるもんだぜ！」")
                              ((equal tribe-dir "SELL") "「崩れ落ちる足音を聞け！」")
                              (t "「静かすぎる...嵐の前触れか？」")))
         (raider-quote (cond ((> wins 0) "「いただいたぜ。追っ手が来る前にズラかるぞ。」")
                             ((< pnl 0) "「チッ、今日のシノギは渋いな。」")
                             (t "「隙を見せたら、いつでも頂くさ。」")))
         (shaman-quote (cond ((> losses 0) "「精霊たちが怒っている...鎮めねばならぬ。」")
                             ((> pnl 1000) "「星の巡りが良い。だが驕るなよ。」")
                             (t "「まだその時ではない...耐え忍ぶのだ。」")))
         (chief-quote (cond ((> pnl 0) "「今日も生き延びたか。だが、明日は明日の風が吹く。」")
                            ((< pnl 0) "「傷を癒やせ。負けから学ぶことこそが、最強への近道だ。」")
                            (t "「静寂もまた、戦略の一部である。」")))
         ;; V5.6: Flood Status
         (flood-status (get-flood-status)))
    
    (notify-discord-daily (format nil "
📜 **日刊・部族クロニクル**
━━━━━━━━━━━━━━━━━━━━━━━━━━
💰 昨日の戦果: ¥~,0f
🔥 現在の戦況: ~d 連勝中 | ~d 連敗中
🌊 **洪水警報 (Risk Level)**:
%  ~a

🗣️ **部族たちの焚き火会議**:
🏹 Hunters: ~a
⚔️ Breakers: ~a
🗡️ Raiders: ~a
🔮 Shamans: ~a

👑 **族長の言葉**:
~a
━━━━━━━━━━━━━━━━━━━━━━━━━━
📊 **Cold Reality (Kahneman's Data)**:
Total PnL: ¥~,2f
Win Rate : ~,1f%
Drawdown : ~,2f%
Sharpe   : ~,2f
━━━━━━━━━━━━━━━━━━━━━━━━━━" pnl wins losses flood-status hunter-quote breaker-quote raider-quote shaman-quote chief-quote
                    (if (boundp '*accumulated-pnl*) *accumulated-pnl* 0.0)
                    (if (boundp '*all-time-win-rate*) *all-time-win-rate* 50.0)
                    (if (boundp '*max-drawdown*) *max-drawdown* 0.0)
                    (if (boundp '*portfolio-sharpe*) *portfolio-sharpe* 0.0))
     :color (cond ((>= (if (boundp '*danger-level*) *danger-level* 0) 3) 15158332) ; Red
                  ((>= (if (boundp '*danger-level*) *danger-level* 0) 1) 16776960) ; Yellow
                  (t 3447003))))) ; Blue/Green

(defun check-daily-narrative ()
  (multiple-value-bind (s m h date month year day-of-week dst-p tz)
      (decode-universal-time (get-universal-time))
    (declare (ignore s m h month year day-of-week dst-p tz))
    (when (and *last-narrative-day* (/= date *last-narrative-day*))
      ;; New day detected!
      (send-daily-tribal-narrative)
      (setf *last-narrative-day* date)
      ;; Reset daily PnL
      (setf *daily-pnl* 0))))

(defun send-heartbeat ()
  (let ((now (get-internal-real-time)))
    (when (> (- now *last-heartbeat-sent*) (* 5 internal-time-units-per-second)) ; Every 5s (V41.7)
      (when (and (boundp '*cmd-publisher*) *cmd-publisher*)
        (pzmq:send *cmd-publisher* (jsown:to-json (jsown:new-js ("action" "HEARTBEAT")))))
      (setf *last-heartbeat-sent* now))))

(defun process-trade-closed (json msg)
  ;; DEBUG: Log raw message to understand grouping
  (format t "~%[L] 🔍 DEBUG TRADE_CLOSED: ~a~%" (subseq msg 0 (min 300 (length msg))))
  (handler-case
      (let* ((ticket (if (jsown:keyp json "ticket") (jsown:val json "ticket") "?"))
             (symbol (if (jsown:keyp json "symbol") (jsown:val json "symbol") "UNKNOWN"))
             (pnl (cond 
                    ((jsown:keyp json "profit") (jsown:val json "profit"))
                    ((jsown:keyp json "pnl") (jsown:val json "pnl"))
                    ((jsown:keyp json "close_profit") (jsown:val json "close_profit"))
                    (t 0)))
             (direction (if (jsown:keyp json "type") (jsown:val json "type") ""))
             (is-win (> pnl 0)))
        ;; Update PnL tracking
        (incf *daily-pnl* pnl)
        (incf *accumulated-pnl* pnl)
        
        ;; Record result for danger tracking
        (record-trade-result (if is-win :win :loss))
        
        ;; V5.1: Increment total trades for warmup tracking
        (incf *total-trades*)
        
        ;; V3.0: Track success count for win rate (PM feedback)
        (when is-win
          (incf *success-count*))
        
        ;; ═══════════════════════════════════════
        ;; LEARNING: Record for dreamer analysis
        ;; ═══════════════════════════════════════
        (let ((dir-keyword (if (search "BUY" (string-upcase direction)) :buy :sell))
              (category (cond 
                          ((jsown:keyp json "category") 
                           (intern (string-upcase (jsown:val json "category")) :keyword))
                          (t :trend)))
              (strategy-name (if (jsown:keyp json "strategy") 
                                (jsown:val json "strategy") 
                                "unknown")))
          (handler-case
              (record-trade-outcome symbol dir-keyword category strategy-name pnl)
            (error (e) (format t "[L] Learning record error: ~a~%" e))))
         
        ;; ═══════════════════════════════════════
        ;; V5.0: NEURAL NETWORK ONLINE LEARNING
        ;; ═══════════════════════════════════════
        (handler-case
            (train-nn-from-trade symbol pnl direction)
          (error (e) (format t "[L] NN train error: ~a~%" e)))
        
        ;; ═══════════════════════════════════════
        ;; ELDER LEARNING: Learn from failures (V3.0: 6-dimension)
        ;; ═══════════════════════════════════════
        (unless is-win
          (handler-case
              (let* ((history (gethash symbol *candle-histories*))
                     (rsi (when (and history (> (length history) 14))
                            (ind-rsi 14 history)))
                     (price-pos (when history (get-price-position history)))
                     (context (list :regime *market-regime*
                                   :volatility-state *current-volatility-state*
                                   :session (current-trading-session)
                                   :rsi-value rsi
                                   :price-position price-pos
                                   :symbol symbol
                                   :direction (if (search "BUY" (string-upcase direction)) :buy :sell))))
                (learn-from-failure context pnl)
                (format t "[L] 📚 長老会議に敗因を報告(6次元)~%"))
            (error (e) (format t "[L] Elder learning error: ~a~%" e))))
        
        ;; ═══════════════════════════════════════
        ;; V3.0: LEADER STATS + MEMORY (previously unused!)
        ;; ═══════════════════════════════════════
        (handler-case
            (progn
              ;; Update leader stats
              (update-leader-stats pnl)
              ;; Store in memory for pattern recall
              (let ((dir-keyword (if (search "BUY" (string-upcase direction)) :buy :sell)))
                (store-memory symbol dir-keyword (if is-win :win :loss) pnl 0)))
          (error (e) (format t "[L] Leader/Memory error: ~a~%" e)))
        
        ;; Funeral/Victory Ceremony
        (if is-win
            ;; 💀 VICTORY - Warrior Returns Triumphant
            (let ((msg (format nil "
⚔️ ═══════════════════════════════ ⚔️
  🎉 戦士凱旋！
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
📈 ~a | ~a
💰 利益: +¥~,0f

🏆 「勝利は準備の結果である」
⚔️ ═══════════════════════════════ ⚔️" symbol direction pnl)))
              (format t "[L] ~a~%" msg)
              (queue-discord-notification (gethash symbol *symbol-webhooks*) msg :color 3066993))
            
            ;; 💀 FUNERAL - Fallen Warrior Remembered
            (let ((msg (format nil "
🕯️ ═══════════════════════════════ 🕯️
  ⚰️ 戦士追悼
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
📉 ~a | ~a
💸 損失: ¥~,0f

🙏 「敗北もまた師である」
🕯️ ═══════════════════════════════ 🕯️" symbol direction (abs pnl))))
              (format t "[L] ~a~%" msg)
              (queue-discord-notification (gethash symbol *symbol-webhooks*) msg :color 15158332))))
    (error (e) (format t "[L] Trade close error: ~a~%" e))))

(defun internal-process-msg (msg)
  (handler-case
      (let* ((json (jsown:parse msg)) (type (jsown:val json "type")))
        (cond
          ((string= type "TICK") 
           (update-candle (jsown:val json "bid") (jsown:val json "symbol"))
           ;; V41.2: Throttled operations for performance
           ;; Only save status every 60 seconds (already implemented in save-live-status)
           (when (fboundp 'save-live-status) (save-live-status))
           ;; Only report every hour (already implemented in send-periodic-status-report)
           (when (fboundp 'send-periodic-status-report)
             (send-periodic-status-report (jsown:val json "symbol") (jsown:val json "bid")))
           ;; Learning step is already throttled by cycle count
           (handler-case (when (fboundp 'continuous-learning-step) (continuous-learning-step)) (error () nil)))
          ;; V5.0: Guardian Heartbeat
          ((string= type "HEARTBEAT")
           (unless (numberp *last-guardian-heartbeat*) (setf *last-guardian-heartbeat* 0))
           (setf *last-guardian-heartbeat* (get-universal-time)))
          ((string= type "HISTORY")
           ;; Self-healing: Ensure *candle-histories* is initialized
           (unless (and (boundp '*candle-histories*) *candle-histories*)
             (setf *candle-histories* (make-hash-table :test 'equal))
             (format t "[L] 🩹 Patched NIL *candle-histories*~%"))
           
           (let ((bars nil)
                 (symbol (if (jsown:keyp json "symbol") (jsown:val json "symbol") "USDJPY")))
             (dolist (b (jsown:val json "data"))
               (let ((c (jsown:val b "c")))
                 (push (make-candle :timestamp (jsown:val b "t") 
                                    :open c :high c :low c :close c :volume 1) bars)))
             (setf (gethash symbol *candle-histories*) bars)
             (setf *candle-history* bars)  ; Legacy compat - use first symbol
             (format t "[L] 📚 ~a: ~d bars~%" symbol (length bars))))
          ((string= type "BACKTEST_RESULT")
           (let* ((result (jsown:val json "result"))
                  (name (jsown:val result "strategy_name"))
                  (sharpe (or (handler-case (jsown:val result "sharpe") (error () 0.0)) 0.0))
                  (trades (or (handler-case (jsown:val result "trades") (error () 0)) 0))
                  (pnl (or (handler-case (jsown:val result "pnl") (error () 0.0)) 0.0))
                  (win-rate (or (handler-case (jsown:val result "win_rate") (error () 0.0)) 0.0)))
             
             ;; V8.0: Redirect WFV results (Walk-Forward Validation)
             (if (or (search "_IS" name :from-end t) (search "_OOS" name :from-end t))
                 (handler-case
                     (when (fboundp 'process-wfv-result)
                       (funcall 'process-wfv-result name 
                                (list :sharpe sharpe :trades trades :pnl pnl :win-rate win-rate)))
                   (error (e) (format t "[L] WFV Process Error: ~a~%" e)))
                 
                 ;; Normal Batch Processing
                 (progn
                   ;; V6.8: Buffer results instead of spamming
                   (push (cons name (list :sharpe sharpe :win-rate win-rate :trades trades :pnl pnl))
                         *backtest-results-buffer*)
                   
                   (when (>= (length *backtest-results-buffer*) *expected-backtest-count*)
                      (format t "[L] 🏁 Backtest Batch Complete! (Received ~d/~d)~%" 
                              (length *backtest-results-buffer*) *expected-backtest-count*)
                      (notify-backtest-summary))))


              ;; Update strategy's sharpe score
              (let ((strat (or (find name *evolved-strategies* :key #'strategy-name :test #'string=)
                               (find name *strategy-knowledge-base* :key #'strategy-name :test #'string=))))
                (when strat
                  (setf (strategy-sharpe strat) sharpe)
                  ;; V5.1: BENCH SYSTEM
                  ;; Weekly unbench for re-evaluation
                  (when (should-weekly-unbench-p)
                    (weekly-unbench-all))
                  ;; Evaluate and bench poor performers (50+ trades required)
                  (evaluate-strategy-performance strat sharpe trades win-rate)
                  ;; Always log performance
                  (format t "[L] 📊 Updated ~a sharpe=~,2f~a~%" 
                          name sharpe (if (strategy-benched-p name) " [BENCHED]" ""))
                  ;; Sort evolved strategies by sharpe (best first)
                  (setf *evolved-strategies* 
                        (sort *evolved-strategies* #'> :key #'strategy-sharpe))
                  (format t "[L] 🏆 Top strategies: ~{~a~^, ~}~%" 
                          (mapcar (lambda (s) (format nil "~a(~,1f)" (strategy-name s) (strategy-sharpe s)))
                                  (subseq *evolved-strategies* 0 (min 3 (length *evolved-strategies*)))))))
              ;; Note: Clone detection handled separately
              ))
          ((or (string= type "PREDICTION_RESULT") (string= type "PREDICTION"))
           (handler-case
               (let* ((pred-data (if (jsown:keyp json "prediction") (jsown:val json "prediction") json))
                      (sig (if (jsown:keyp pred-data "signal") (jsown:val pred-data "signal") "HOLD"))
                      (conf (if (jsown:keyp pred-data "confidence") (jsown:val pred-data "confidence") 0.0)))
                 (setf *last-prediction* sig
                       *last-confidence* conf))
             (error () nil)))
          ((string= type "EVOLVE_RESULT")
           (process-evolution-result json))
          ((string= type "MCTS_RESULT")
           (let* ((best (jsown:val json "best"))
                  (score (jsown:val (jsown:val json "score") "composite")))
             (format t "[L] 🔍 MCTS Optimized (score: ~,3f): ~a~%" score best)
             (notify-discord (format nil "🔍 MCTS Optimized: ~,3f" score) :color 130821)))
          
          ;; V5.0: Walk-Forward validation result
          ((string= type "WALK_FORWARD_RESULT")
           (handler-case
               (let* ((result (jsown:val json "result"))
                      (name (jsown:val result "strategy_name"))
                      (is-sharpe (jsown:val result "in_sample_sharpe"))
                      (oos-sharpe (jsown:val result "out_of_sample_sharpe"))
                      (efficiency (jsown:val result "efficiency_ratio"))
                      (is-overfit (jsown:val result "is_overfit")))
                 (format t "[L] 📊 WALK-FORWARD: ~a | IS:~,2f OOS:~,2f Eff:~,0f%~%" 
                         name is-sharpe oos-sharpe (* 100 efficiency))
                 (when is-overfit
                   (format t "[L] ⚠️ OVERFIT DETECTED: ~a - reducing volume~%" name)
                   ;; Find and penalize overfit strategy
                   (let ((strat (find name *strategy-knowledge-base* :key #'strategy-name :test #'string=)))
                     (when (and strat (strategy-volume strat))
                       (setf (strategy-volume strat) (* 0.5 (strategy-volume strat)))
                       (format t "[L] 📉 ~a volume reduced to ~,3f~%" name (strategy-volume strat))))))
             (error (e) (format t "[L] Walk-forward result error: ~a~%" e))))
          ;; ══════════════════════════════════════
          ;; TRADE CLOSED - 葬儀 / Victory Ceremony
          ;; ══════════════════════════════════════
          ((string= type "TRADE_CLOSED")
           (process-trade-closed json msg))
          
          (t (format t "[L] Unknown msg type: ~a~%" type))))
    (error (e) (format t "[L] Err: ~a~%" e))))

(defun process-msg (msg)
  (let ((start-time (get-internal-real-time)))
    (internal-process-msg msg)
    (check-daily-narrative) ; V5.4: Check for new day
    (send-heartbeat)        ; V5.5: Heartbeat to MT5
    (flush-discord-queue)   ; V41.3: Async Discord notifications
    (let ((duration (/ (- (get-internal-real-time) start-time) internal-time-units-per-second)))
      (when (> duration 0.5) ; 500ms threshold
        ;; V41.2: Structured logging for performance monitoring
        (log-warn (format nil "SLOW TICK: Processing took ~,3f seconds" duration) 
                  :data (jsown:new-js ("type" "slow_tick") 
                                      ("duration" duration) 
                                      ("threshold" 0.5)))))))

(defun request-prediction ()
  "Request neural network prediction from Rust"
  (when (and *candle-history* (> (length *candle-history*) 20))
    (let ((msg (jsown:to-json 
                 (jsown:new-js 
                   ("action" "PREDICT")
                   ("candles" (candles-to-json (subseq *candle-history* 0 (min 100 (length *candle-history*)))))))))
      (pzmq:send *cmd-publisher* msg))))

(defun train-neural (target)
  "Train neural network: 0=UP, 1=DOWN, 2=FLAT"
  (when (and *candle-history* (> (length *candle-history*) 20))
    (let ((msg (jsown:to-json 
                 (jsown:new-js 
                   ("action" "TRAIN")
                   ("candles" (candles-to-json (subseq *candle-history* 0 (min 100 (length *candle-history*)))))
                   ("target" target)))))
      (pzmq:send *cmd-publisher* msg)
      (format t "[L] 🎓 NN Train: ~a~%" (case target (0 "UP") (1 "DOWN") (t "FLAT"))))))
