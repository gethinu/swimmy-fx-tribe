(in-package :swimmy.main)

;;; ==========================================
;;; SWIMMY CORE: TICK HANDLER (tick-handler.lisp)
;;; ==========================================
;;; Contains the main tick processing loop, candle updates, and neural logic
;;; Extracted from brain.lisp (Strangler Fig Phase 2)

;;; ==========================================
;;; HELPER FUNCTIONS (Logic moved from brain.lisp)
;;; ==========================================
;;; Note: has-resigned-p is defined in school-resignation.lisp (swimmy.school)
;;; Note: get-date-string, get-time-string are defined in shell/handoff.lisp (swimmy.shell)
;;; These are inherited via (:use :swimmy.school :swimmy.shell) in package definition.

;; Updated to match the robust version from dreamer2.lisp (handles nil timestamps & chronology)
(defun candles-to-json (candles)
  "Convert a list of candles to JSON structures for neural network input"
  (mapcar (lambda (c)
            (let ((close (candle-close c)))
              (jsown:new-js
                ("t" (or (candle-timestamp c) 0))
                ("o" (or (candle-open c) close))
                ("h" (or (candle-high c) close))
                ("l" (or (candle-low c) close))
                ("c" close))))
          (reverse candles)))

;; Execution logic moved to executor.lisp

(defparameter *last-maintenance-time* 0)

(defun run-periodic-maintenance ()
  "Handle periodic maintenance tasks (backtests, ecosystem, evolution) - Throttled 60s
   STRUCTURE NOTE (V8.3):
   - L54-65: Prediction/Backtest → 60s throttle (from this function)
   - L67-92: Dream Cycle → Self-throttled by *dream-interval* (3600s)
   - L94-95: Discord Heartbeat → Self-throttled internally"
  (let ((now (get-universal-time)))
    ;; PROFILE SECTION 1
    (with-profiling "maintenance-section-1"
      ;; SECTION 1: 60s Throttled Operations (Prediction, Backtest)
      (when (> (- now *last-maintenance-time*) 59)
        (setf *last-maintenance-time* now)
        (unless (and (boundp '*candle-histories*) *candle-histories*)
          (setf *candle-histories* (make-hash-table :test 'equal)))
        ;; Prediction cache lookup
        (let ((cache-key (list now)))
          (if (gethash cache-key *prediction-cache*)
              (format t "[L] 🔄 Using cached prediction~%")
              (progn
                (request-prediction)
                (setf (gethash cache-key *prediction-cache*) t))))
        ;; Batch backtest (one-time operation)
        (when (and (not *initial-backtest-done*) *candle-history* (> (length *candle-history*) 1000))
          (setf *initial-backtest-done* t)
          (format t "~%[L] 🧪 Starting batch backtest of ~d strategies...~%" (length *strategy-knowledge-base*))
          (batch-backtest-knowledge))))
    ;; PROFILE SECTION 2
    (with-profiling "maintenance-section-2"
      ;; SECTION 2: Self-Throttled Operations (Dream Cycle - 1hr)
      (unless (numberp *last-dream-time*) (setf *last-dream-time* 0))
      (when (> (- now *last-dream-time*) *dream-interval*)
        (setf *last-dream-time* now)
        (when (fboundp 'assemble-team) (assemble-team))
        (let ((health (if (fboundp 'get-population-health) (get-population-health) 0.5)))
          (format t "[L] 🌿 ECOSYSTEM: Health=~,0f%~%" (* 100 health)))
        (when (zerop (mod *dream-cycle* 6))
          (format t "[L] 🌱 Running natural selection...~%")
          (maintain-ecosystem-balance))
        ;; [V8.2] Expert Panel: Disabled Mutual Aid (Taleb: "Let the losers die")
        ;; (when (and (zerop (mod *dream-cycle* 3)) (fboundp 'calculate-mutual-aid))
        ;;   (calculate-mutual-aid))
        (when (zerop (mod *dream-cycle* 3))
          (report-goal-status))
        (check-evolution)
        (evolve-population-via-mutation)
        (incf *dream-cycle*)
        (when (and (zerop (mod *dream-cycle* 60)) (fboundp 'save-state))
          (funcall 'save-state))))
    ;; PROFILE SECTION 3
    (with-profiling "maintenance-section-3"
      ;; SECTION 3: Self-Throttled Operations (Discord Heartbeat)
      (when (fboundp 'swimmy.engine::check-discord-heartbeat)
        (swimmy.engine::check-discord-heartbeat))
      ;; P7: Check for stress test trigger flag
      (when (fboundp 'swimmy.school::check-stress-test-trigger)
        (swimmy.school::check-stress-test-trigger)))))

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
      
      ;; V9.2: Data Persistence (Article 5 Compliant)
      ;; Save finalized candle to Data Keeper for 20-year archive
      (when (fboundp 'swimmy.core:add-candle-to-keeper)
        (swimmy.core:add-candle-to-keeper symbol curr-candle))
      
      (format t "[~a] ~a." (swimmy.core:get-jst-str) (subseq symbol 0 3)) (force-output)
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
         ;; Generate dynamic quotes based on situation (AGGRESSIVE V7.0)
         (hunter-quote (cond ((> pnl 0) "「血の匂いがする...群れで追い込め！」")
                             ((> losses 2) "「手負いの獲物ほど危険だ。だが怯むな。」")
                             (t "「守りは捨てた。矢を放て！」")))
         (breaker-quote (cond ((equal tribe-dir "BUY") "「全ての抵抗帯を破壊し尽くせ！」")
                              ((equal tribe-dir "SELL") "「底などない！叩き落とせ！」")
                              (t "「壁を壊すのは今だ！突撃！」")))
         (raider-quote (cond ((> wins 0) "「ごっつぁん！もっと奪えるぜ？」")
                             ((< pnl 0) "「かすり傷だ。倍にして取り返してやる。」")
                             (t "「俺たちのシマだ。好きにはさせねぇ。」")))
         (shaman-quote (cond ((> losses 0) "「痛みは進化の糧...過去の知恵を使え。」")
                             ((> pnl 1000) "「星々が並んだ。これが運命（さだめ）だ。」")
                             (t "「10年の歴史が見える...今が決戦の時だ。」")))
         (chief-quote (cond ((> pnl 0) "「見事だ。だが満足するな。全てを奪え。」")
                            ((< pnl 0) "「後退ではない。助走だ。死ぬ気で取り返せ。」")
                            (t "「地下壕から出ろ。世界を我らの色に染める時が来た。」")))
         ;; V5.6: Flood Status
         (flood-status (get-flood-status)))
    
    (notify-discord-daily (format nil "
📜 **日刊・部族クロニクル (ATTACK MODE)**
━━━━━━━━━━━━━━━━━━━━━━━━━━
💰 昨日の戦果: ¥~,0f
🔥 現在の戦況: ~d 連勝中 | ~d 連敗中
🌊 **洪水警報 (Risk Level)**:
~a

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
                    (if (boundp 'swimmy.school::*accumulated-pnl*) swimmy.school::*accumulated-pnl* 0.0)
                    (if (boundp 'swimmy.school::*all-time-win-rate*) swimmy.school::*all-time-win-rate* 50.0)
                    (if (boundp '*max-drawdown*) *max-drawdown* 0.0)
                    (if (boundp '*portfolio-sharpe*) *portfolio-sharpe* 0.0))
     :color (cond ((>= (if (boundp '*danger-level*) *danger-level* 0) 3) 15158332) ; Red
                  ((>= (if (boundp '*danger-level*) *danger-level* 0) 1) 16776960) ; Yellow
                  (t 3447003))))) ; Blue/Green

(defun check-scheduled-tasks (&optional (now (get-universal-time)))
  (multiple-value-bind (s m h date month year day-of-week dst-p tz)
      (decode-universal-time now)
    (declare (ignore s m month year dst-p tz)) ;; Don't ignore day-of-week
    
    ;; 1. New Day Processing (Reset Logic)
    (when (and *last-narrative-day* (/= date *last-narrative-day*))
      ;; Reset triggers for the new day
      (setf *last-narrative-day* date)
      (setf *daily-report-sent-today* nil)
      (setf *has-resigned-today* nil)
      
      ;; Reset daily counters
      (setf swimmy.school::*yesterday-pnl* *daily-pnl*) ;; Save for reporting if checked after reset
      (setf *daily-pnl* 0.0)
      (setf *daily-trade-count* 0)
      (format t "[RISK] 🌅 New Day: Daily PnL Reset.~%")
      
      ;; V19.8: Period-Based Risk Reset (Expert Panel)
      
      ;; Weekly Reset (Monday = 0)
      (when (= day-of-week 0)
        (format t "[RISK] 🔄 Weekly Risk Reset (Monday)~%")
        (setf *weekly-pnl* 0.0)
        (notify-discord "🔄 **Weekly Risk Reset**\nWeekly PnL counter reset." :color 3066993))
      
      ;; Monthly Reset (1st of Month)
      (when (= date 1)
        (format t "[RISK] 🗓️ Monthly Risk Reset (1st)~%")
        (setf *monthly-pnl* 0.0)
        (notify-discord "🗓️ **Monthly Risk Reset**\nMonthly PnL counter reset." :color 3066993))
      
      ;; Persist reset
      (when (fboundp 'swimmy.engine::save-state)
        (funcall 'swimmy.engine::save-state)))
        
    ;; 2. Scheduled Report (23:00 Trigger)
    (when (and (>= h 23) (not *daily-report-sent-today*))
      (format t "[SCHEDULER] ⏰ 23:00 Trigger - Sending Daily Report...~%")
      (send-daily-tribal-narrative)
      (setf *daily-report-sent-today* t))))

;; V5.5: Heartbeat to MT5
;; Heartbeat & Process Trade Logic moved to executor.lisp

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
          ;; V41.6: MT5 Account Sync - Update equity from MT5
           ((string= type "ACCOUNT_INFO")
            (swimmy.executor:process-account-info json))
          ((string= type "HISTORY")
           ;; V15.8: Throttle HISTORY processing to prevent tick blocking
           ;; Only process HISTORY once per 60 seconds per symbol
           (let* ((symbol (if (jsown:keyp json "symbol") (jsown:val json "symbol") "USDJPY"))
                  (tf (if (jsown:keyp json "tf") (jsown:val json "tf") "M1"))
                  (cache-key (format nil "~a-~a" symbol tf))
                  (now (get-universal-time))
                  (last-process (gethash cache-key *history-process-cache* 0)))
             (when (> (- now last-process) 60) ; Only process every 60 seconds
               (setf (gethash cache-key *history-process-cache*) now)
               
           ;; Self-healing: Ensure *candle-histories* is initialized
           (unless (and (boundp '*candle-histories*) *candle-histories*)
             (setf *candle-histories* (make-hash-table :test 'equal))
             (format t "[L] 🩹 Patched NIL *candle-histories*~%"))
              
           ;; Initialize MTF storage if needed
           (unless (and (boundp '*candle-histories-tf*) *candle-histories-tf*)
             (setf *candle-histories-tf* (make-hash-table :test 'equal))
             (format t "[L] 🩹 Patched NIL *candle-histories-tf*~%"))
            
           (let ((bars nil))
                  
              (dolist (b (jsown:val json "data"))
               (let ((c (jsown:val b "c")))
                 (push (make-candle :timestamp (jsown:val b "t") 
                                    :open c :high c :low c :close c :volume 1) bars)))
                                    
             ;; Store in MTF structure (Merge Strategy)
             (unless (gethash symbol *candle-histories-tf*)
               (setf (gethash symbol *candle-histories-tf*) (make-hash-table :test 'equal)))
             
             (let ((existing (gethash tf (gethash symbol *candle-histories-tf*))))
               (if existing
                   (setf bars (sort (remove-duplicates (append bars existing)
                                                       :key #'candle-timestamp
                                                       :test #'=)
                                    #'> :key #'candle-timestamp))
                   (setf bars (sort bars #'> :key #'candle-timestamp)))
               ;; V15.7: Limit to 50k bars to prevent slow merge operations
               (when (> (length bars) 50000)
                 (setf bars (subseq bars 0 50000))))
                   
             (setf (gethash tf (gethash symbol *candle-histories-tf*)) bars)
             (format t "[L] 📚 Loaded ~a ~a data: ~d bars (Merged, capped at 50k)~%" symbol tf (length bars))

             ;; If M1, also store in legacy/default locations
             (when (or (string= tf "M1") (string= tf "Default"))
                 (setf (gethash symbol *candle-histories*) bars)
                 (setf *candle-history* bars)  ; Legacy compat - use first symbol
              
                 ;; P1: Trigger Backtest after history load (Moved from runner.lisp)
             ;; V11.0: Data Gap Check (Expert Panel P0) + Resilience (P1)
             ;; Verify no gaps, but DO NOT BLOCK startup check (Taleb's Resilience)
             ;; Even if gap exists, we request backfill (internally in check-data-gap) 
             ;; and proceed to check cache.
             (let ((has-gap (swimmy.core:check-data-gap symbol bars)))
               (when has-gap
                 (format t "[L] ⚠️ Data Gap detected in ~a, but proceeding with Startup Check (Taleb's Resilience)...~%" symbol))
                   
               (when (and (fboundp 'swimmy.school:batch-backtest-knowledge)
                          (not (and (boundp '*initial-backtest-done*) *initial-backtest-done*))
                          (> (length bars) 100))
                 (format t "[L] 🧪 Starting Batch Backtest Verification...~%")
                 (swimmy.school:batch-backtest-knowledge)
                 (setf *initial-backtest-done* t)))))))) ; Close let bars, when throttle, let*, clause
           ((string= type "SYSTEM_COMMAND")
           (let ((action (jsown:val json "action")))
             (cond
               ((string= action "REPORT_STATUS")
       (format t "[L] 🏰 REPORT_STATUS command received.~%")
       (swimmy.school:report-active-positions))

               ((string= action "DAILY_REPORT")
                (format t "[L] 📊 DAILY_REPORT command received.~%")
                (send-daily-tribal-narrative))

               ((string= action "BACKTEST_SUMMARY")
                (format t "[L] 📊 BACKTEST_SUMMARY command received.~%")
                (notify-backtest-summary))

      ((string= action "RELOAD_CONFIG")
                (format t "[L] 🔄 Hot Reloading Configuration...~%")
                (handler-case
                    (progn
                      ;; Reload configs - defvar protects state, defparameter updates settings
                      (load #P"/home/swimmy/swimmy/src/lisp/core/globals.lisp")
                      (load #P"/home/swimmy/swimmy/src/lisp/core/config.lisp")
                      (format t "[L] ✅ Configuration Reloaded!~%"))
                  (error (e) (format t "[L] ❌ Reload Failed: ~a~%" e))))
               ((string= action "RESET_WARRIORS")
                (format t "[L] 🧹 RESET_WARRIORS command received. Clearing warrior allocation...~%")
                (handler-case
                    (progn
                      (when (fboundp 'swimmy.school:debug-reset-warriors)
                        (funcall 'swimmy.school:debug-reset-warriors))
                       (format t "[L] ✅ Warriors Reset Complete!~%"))
                  (error (e) (format t "[L] ❌ Reset Warriors Failed: ~a~%" e))))
               ((string= action "DEBUG_ENTRY")
                (let ((target-symbol (if (jsown:keyp json "symbol") (jsown:val json "symbol") "USDJPY")))
                  (format t "[L] 🧪 DEBUG_ENTRY command received for ~a. Triggering test entry...~%" target-symbol)
                  (handler-case
                      (let ((symbol target-symbol)
                            (magic 999999)
                            (lot 0.01))
                        ;; 1. Send Test Order to MT5
                        (swimmy.engine:safe-order "BUY" symbol lot 130.00 160.00 magic)
                        ;; 2. Send Test Notification to Discord
                        (swimmy.shell:notify-discord-symbol symbol 
                          (format nil "🧪 **DEBUG TEST ENTRY** (🕐 ~a)~%Strategy: MANUAL_TEST~%Action: BUY ~a~%Lot: ~,2f~%Magic: ~d" 
                                  (swimmy.core:get-jst-timestamp)
                                  symbol lot magic)
                          :color 3066993)
                        (format t "[L] ✅ DEBUG ENTRY (~a) triggered! Check Discord.~%" symbol))
                      (error (e) (format t "[L] ❌ DEBUG ENTRY Failed: ~a~%" e)))))
               ((string= action "DEBUG_FORCE_EVAL")
                (let ((target (if (jsown:keyp json "target") (jsown:val json "target") "ALL")))
                  (format t "[L] 🧪 FORCE EVAL: Clearing execution history for ~a~%" target)
                  (if (string= target "ALL")
                      (clrhash swimmy.school::*processed-candle-time*)
                      (remhash target swimmy.school::*processed-candle-time*))
                  (format t "[L] ✅ Cleared. Next tick will trigger re-evaluation.~%")))
               ;; V19 Emergency: Manual Risk State Reset
               ((string= action "RESET_RISK")
                (format t "[L] 🚨 RESET_RISK command received. Resetting drawdown and equity...~%")
                (handler-case
                    (progn
                      (setf swimmy.globals::*max-drawdown* 0.0)
                      (setf swimmy.globals::*daily-pnl* 0.0)
                      (setf swimmy.globals::*consecutive-losses* 0)
                      ;; Let MT5 sync handle equity, but reset peak to unlock DD calculation
                      (when (boundp 'swimmy.globals::*current-equity*)
                        (setf swimmy.globals::*peak-equity* swimmy.globals::*current-equity*))
                      (format t "[L] ✅ Risk State Reset! DD=0, PnL=0. Peak adjusted.~%"))
                  (error (e) (format t "[L] ❌ Reset Risk Failed: ~a~%" e))))
               (t (format t "[L] ⚠️ Unknown System Command: ~a~%" action)))))
          ((string= type "BACKTEST_RESULT")
           (let* ((result (jsown:val json "result"))
                  (name (jsown:val result "strategy_name"))
                  ;; V8.1: Coerce to FLOAT to prevent RATIO display bug (-0.19 everywhere)
                  (sharpe (float (or (handler-case (jsown:val result "sharpe") (error () 0.0)) 0.0)))
                  (trades (or (handler-case (jsown:val result "trades") (error () 0)) 0))
                  (pnl (float (or (handler-case (jsown:val result "pnl") (error () 0.0)) 0.0)))
                  (win-rate (float (or (handler-case (jsown:val result "win_rate") (error () 0.0)) 0.0)))
                  ;; V8.13: Profit Factor for Kodoku check
                  (profit-factor (float (or (handler-case (jsown:val result "profit_factor") (error () 0.0)) 0.0))))
              
             ;; V15.9: Persistence - Cache the result immediately! (Expert Panel)
             (when (and name (not (string= name "unknown")) (fboundp 'cache-backtest-result))
               (cache-backtest-result name result)
               ;; P5: Log to file for traceability (Expert Panel 2026-01-16)
               (handler-case
                   (with-open-file (log "logs/backtest.log" 
                                        :direction :output 
                                        :if-exists :append 
                                        :if-does-not-exist :create)
                     (format log "~a | ~a | S:~,2f | T:~d | PnL:~,2f | WR:~,1f% | PF:~,2f~%"
                             (swimmy.core:get-jst-str) name sharpe trades pnl win-rate profit-factor))
                 (error () nil)))

             ;; V8.0: Redirect WFV results (Walk-Forward Validation)
             (if (or (search "_IS" name :from-end t) (search "_OOS" name :from-end t))
                 (handler-case
                     (when (fboundp 'process-wfv-result)
                       (funcall 'process-wfv-result name 
                                (list :sharpe sharpe :trades trades :pnl pnl :win-rate win-rate :profit-factor profit-factor)))
                   (error (e) (format t "[L] WFV Process Error: ~a~%" e)))
                 
                 ;; Normal Batch Processing
                 (progn
                   ;; V6.8: Buffer results instead of spamming
                   (when (and name (not (string= name "unknown")) (not (string= name "")))
                     ;; Remove existing result for same strategy if present (update)
                     (setf *backtest-results-buffer* 
                           (remove name *backtest-results-buffer* :key #'car :test #'string=))
                     (push (cons name (list :sharpe sharpe :win-rate win-rate :trades trades :pnl pnl))
                           *backtest-results-buffer*))
                   
                   (when (>= (length *backtest-results-buffer*) *expected-backtest-count*)
                      (format t "[L] 🏁 Backtest Batch Complete! (Received ~d/~d)~%" 
                              (length *backtest-results-buffer*) *expected-backtest-count*)
                      (notify-backtest-summary)
                      ;; P1: Auto-adopt proven strategies
                      (when (fboundp 'swimmy.school:adopt-proven-strategies)
                        (swimmy.school:adopt-proven-strategies)))))


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
                  ;; Evaluate and bench poor performers (50+ trades required)
                  (evaluate-strategy-performance strat sharpe trades win-rate profit-factor)
                  ;; Always log performance IF it's decent
                  (when (or (> sharpe 0.1) (> win-rate 55.0))
                    (format t "[L] 📊 Updated ~a sharpe=~,2f~a~%" 
                            name sharpe (if (strategy-benched-p name) " [BENCHED]" "")))
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
          ;; CLONE CHECK RESULT (V8.9)
          ;; ══════════════════════════════════════
          ((string= type "CLONE_CHECK_RESULT")
           (let* ((result (jsown:val json "result"))
                  (is-clone (jsown:val result "is_clone"))
                  (similar (jsown:val result "most_similar"))
                  (sim (jsown:val result "similarity"))
                  (candidate-name (if (jsown:keyp result "candidate_name") (jsown:val result "candidate_name") "Unknown")))
             
             ;; Delegate to school-backtest.lisp handler
             (when (fboundp 'process-clone-check-result)
               (funcall 'process-clone-check-result candidate-name is-clone similar sim))))

          ;; ══════════════════════════════════════
          ;; TRADE CLOSED - 葬儀 / Victory Ceremony
          ;; ══════════════════════════════════════
           ((string= type "TRADE_CLOSED")
            (swimmy.executor:process-trade-closed json msg)
            ;; V44.3: Update Global Stats Immediately
            (when (fboundp 'swimmy.school:update-global-stats)
              (swimmy.school:update-global-stats)))
          
           ;; V19: Position Sync - Reconcile warrior-allocation with MT5 positions
           ;; V44.0: Entry Confirmation (Taleb: "Only celebrate when deal closes")
           ((string= type "POSITIONS")
            (handler-case
                (when (fboundp 'swimmy.school:reconcile-with-mt5-positions)
                  (let ((symbol (if (jsown:keyp json "symbol") (jsown:val json "symbol") "UNKNOWN"))
                        (positions (if (jsown:keyp json "data") (jsown:val json "data") nil)))
                    (format t "[L] 📊 POSITIONS received: ~a has ~d positions~%" symbol (length positions))
                    ;; V44.0: Check for newly confirmed entries
                    (when (and positions (boundp 'swimmy.school::*pending-orders*))
                      (dolist (pos positions)
                        (let* ((magic (if (jsown:keyp pos "magic") (jsown:val pos "magic") 0))
                               (pending (gethash magic swimmy.school::*pending-orders*)))
                          (when pending
                            (let ((strat-name (getf pending :strategy))
                                  (direction (getf pending :direction))
                                  (entry-price (if (jsown:keyp pos "price") (jsown:val pos "price") 0))
                                  (lot (if (jsown:keyp pos "volume") (jsown:val pos "volume") 0.01)))
                              (format t "[L] ✅ ENTRY CONFIRMED: ~a ~a @~,3f (Magic ~d)~%" symbol direction entry-price magic)
                              (notify-discord-symbol symbol
                                (format nil "✅ **ENTRY CONFIRMED**~%Strategy: ~a~%~a ~a @ ~,3f~%Lot: ~,2f | Magic: ~d"
                                        strat-name (if (eq direction :long) "BUY" "SELL") symbol entry-price lot magic)
                                :color (if (eq direction :long) 3066993 15158332))
                              ;; Remove from pending (now confirmed)
                              (remhash magic swimmy.school::*pending-orders*))))))
                    (funcall 'swimmy.school:reconcile-with-mt5-positions symbol positions)))
              (error (e) (format t "[L] Position reconcile error: ~a~%" e))))
          
          (t (format t "[L] Unknown msg type: ~a~%" type))))
    (error (e) (format t "[L] Err: ~a~%" e))))

(defun process-msg (msg)
  (let ((start-time (get-internal-real-time)))
    (internal-process-msg msg)
    (check-scheduled-tasks) ; V5.4: Check for scheduled tasks (23:00 report, midnight reset)
    (swimmy.executor:send-heartbeat) ; V5.5: Heartbeat to MT5 (via Executor)
    (flush-discord-queue)   ; V41.3: Async Discord notifications
    (let ((duration (/ (- (get-internal-real-time) start-time) internal-time-units-per-second)))
      (when (> duration 1.5) ; 1.5s threshold (Relaxed per Expert Panel)
        ;; V41.2: Structured logging for performance monitoring
        (log-warn (format nil "SLOW TICK: Processing took ~,3f seconds" duration) 
                  :data (jsown:new-js ("type" "slow_tick") 
                                      ("duration" duration) 
                                      ("threshold" 1.5)))))))

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
