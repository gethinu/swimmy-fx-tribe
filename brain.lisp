;; brain.lisp - Swimmy Ver 14.0 (V4.0 Modular)
;; V4.0: Learning logic → brain-learning.lisp
;; V4.0: Ritual logic → brain-ritual.lisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init) (load quicklisp-init)))
(ql:quickload :pzmq)
(ql:quickload :jsown)
(ql:quickload :dexador)

;; Define candle struct BEFORE loading other files
(defstruct candle timestamp open high low close volume)

;; V41.4: Core Configuration (Strangler Fig - Foundation)
(load (merge-pathnames "src/lisp/core/config.lisp" *load-truename*))

(load (merge-pathnames "src/lisp/dsl.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/dreamer2.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/strategies.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/school.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/school-danger.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/logger.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/core/rituals.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/mixseek.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/research.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/llm-integration.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/evolution.lisp" *load-truename*))

;; Quality modules (Ver 38.1 EXCELLENCE)
(load (merge-pathnames "src/lisp/error-handling.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/quality.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/repl.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/tests.lisp" *load-truename*))

;; V41.4: Strangler Fig Core Modules (Logic)
(load (merge-pathnames "src/lisp/discord-async.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/core/discord.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/core/tick-handler.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/core/governance.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/system/runner.lisp" *load-truename*))
;; V41.5: Optional Modules Extracted (Naval's Cleanup)
(load (merge-pathnames "src/lisp/core/evaluator.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/system/opus.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/core/meta-learning.lisp" *load-truename*))

(defun get-jst-str (&optional (ut (get-universal-time)))
  "Return current time as JST string [YYYY-MM-DD HH:MM:SS]"
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time ut -9)
    (format nil "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" year month day hour min sec)))

(defstruct arm-state position entry-price sl tp streak)

;; NOTE: Most defparameters moved to src/lisp/core/config.lisp (Strangler Fig)
;; Only keeping brain-specific items below

;; Genome path (brain-specific as it uses *load-truename*)
(defparameter *genome-path* (merge-pathnames "genome.lisp" *load-truename*))

;;; ==========================================
;;; GOAL DECOMPOSITION SYSTEM (目標分解)
;;; ==========================================
;;; Inspired by: LLM Chain-of-Thought + Planning
;;; Purpose: Break "I want to earn 100K" into daily actionable targets

;; Goal state
(defparameter *monthly-goal* 10000)        ; Ramen profitability target (yen) - realistic first milestone
(defparameter *goal-start-date* nil)       ; When we started tracking
(defparameter *accumulated-pnl* 0.0)       ; Total PnL since goal start
(defparameter *trading-days-in-month* 22)  ; Approximate trading days

;; Risk-adjusted targets
(defparameter *risk-tolerance* :moderate)  ; :conservative, :moderate, :aggressive
(defparameter *daily-risk-limit* nil)      ; Auto-calculated from goal

(defun set-monthly-goal (amount)
  "Set monthly profit target and initialize tracking"
  (setf *monthly-goal* amount)
  (setf *goal-start-date* (get-universal-time))
  (setf *accumulated-pnl* 0.0)
  (calculate-daily-targets)
  (format t "~%[L] 🎯 GOAL SET: ¥~:d/month~%" amount)
  (format t "[L] 📊 Daily target: ¥~:d~%" (get-daily-target))
  (format t "[L] ⚖️ Daily risk limit: ¥~:d~%~%" (get-daily-risk-limit)))

(defun calculate-daily-targets ()
  "Calculate daily targets based on monthly goal and risk tolerance"
  (let ((daily-target (ceiling (/ *monthly-goal* *trading-days-in-month*))))
    ;; Set risk limit based on tolerance
    (setf *daily-risk-limit*
          (case *risk-tolerance*
            (:conservative (ceiling (* daily-target 0.5)))  ; Risk 50% of target
            (:moderate     (ceiling (* daily-target 1.0)))  ; Risk 100% of target  
            (:aggressive   (ceiling (* daily-target 1.5))) ; Risk 150% of target
            (otherwise     daily-target)))))

(defun get-daily-target ()
  "Get current daily profit target"
  (ceiling (/ *monthly-goal* *trading-days-in-month*)))

(defun get-daily-risk-limit ()
  "Get maximum acceptable daily loss"
  (or *daily-risk-limit* (- (get-daily-target))))

(defun get-days-elapsed ()
  "Get trading days since goal start"
  (if *goal-start-date*
      (let ((seconds-elapsed (- (get-universal-time) *goal-start-date*)))
        (max 1 (floor seconds-elapsed (* 24 3600))))  ; At least 1 day
      1))

(defun get-goal-progress ()
  "Calculate goal progress and status"
  (let* ((days-elapsed (get-days-elapsed))
         (expected-pnl (* (get-daily-target) days-elapsed))
         (actual-pnl (+ *accumulated-pnl* *daily-pnl*))
         (progress-pct (if (> *monthly-goal* 0) 
                           (* 100 (/ actual-pnl *monthly-goal*))
                           0))
         (pace-pct (if (> expected-pnl 0)
                       (* 100 (/ actual-pnl expected-pnl))
                       100)))
    (list :days-elapsed days-elapsed
          :expected-pnl expected-pnl
          :actual-pnl actual-pnl
          :progress-pct progress-pct
          :pace-pct pace-pct
          :on-track (>= pace-pct 80)
          :daily-target (get-daily-target)
          :remaining (- *monthly-goal* actual-pnl))))

(defun report-goal-status ()
  "Output current goal status to log"
  (let ((progress (get-goal-progress)))
    (format t "~%[L] ═══════════════════════════════════════~%")
    (format t "[L] 🎯 GOAL STATUS: ¥~:d / ¥~:d (~,1f%)~%" 
            (round (getf progress :actual-pnl))
            *monthly-goal*
            (getf progress :progress-pct))
    (format t "[L] 📅 Day ~d | Expected: ¥~:d | Actual: ¥~:d~%"
            (getf progress :days-elapsed)
            (round (getf progress :expected-pnl))
            (round (getf progress :actual-pnl)))
    (format t "[L] ~a Pace: ~,0f% (~a)~%"
            (if (getf progress :on-track) "✅" "⚠️")
            (getf progress :pace-pct)
            (if (getf progress :on-track) "ON TRACK" "BEHIND"))
    (format t "[L] 📊 Today's target: ¥~:d | Today's PnL: ¥~:d~%"
            (get-daily-target)
            (round *daily-pnl*))
    (format t "[L] ═══════════════════════════════════════~%~%")
    progress))

(defun update-accumulated-pnl ()
  "Update accumulated PnL at end of day"
  (incf *accumulated-pnl* *daily-pnl*)
  (setf *daily-pnl* 0.0))

;; Initialize with default goal
(calculate-daily-targets)

;;; ==========================================
;;; DAILY BRIEFING (日次ブリーフィング)
;;; ==========================================
;;; The final piece of Intent Trading
;;; Natural language status reporting like a personal assistant

(defparameter *last-briefing-hour* -1)

(defun generate-daily-briefing ()
  "Generate natural language morning briefing"
  (let* ((progress (get-goal-progress))
         (days-elapsed (getf progress :days-elapsed))
         (actual-pnl (getf progress :actual-pnl))
         (daily-target (getf progress :daily-target))
         (pace-pct (getf progress :pace-pct))
         (remaining (getf progress :remaining))
         (trading-days-left (- 22 days-elapsed)))
    
    (format t "~%")
    (format t "[L] ════════════════════════════════════════════════════~%")
    (format t "[L] 🌅 GOOD MORNING! Swimmy Daily Briefing~%")
    (format t "[L] ════════════════════════════════════════════════════~%")
    (format t "~%")
    
    ;; Goal Status
    (format t "[L] 🎯 GOAL STATUS:~%")
    (format t "[L]    Monthly target: ¥~:d~%" *monthly-goal*)
    (format t "[L]    Current progress: ¥~:d (~,1f%)~%" (round actual-pnl) (getf progress :progress-pct))
    (format t "[L]    Remaining: ¥~:d over ~d trading days~%" (round remaining) trading-days-left)
    (format t "~%")
    
    ;; Pace Analysis
    (format t "[L] 📊 PACE ANALYSIS:~%")
    (cond
      ((>= pace-pct 120)
       (format t "[L]    ✅ EXCELLENT! Ahead of schedule (~,0f%)~%" pace-pct)
       (format t "[L]    💡 Consider reducing risk or taking profits~%"))
      ((>= pace-pct 80)
       (format t "[L]    ✅ ON TRACK (~,0f%)~%" pace-pct)
       (format t "[L]    💡 Continue current strategy~%"))
      ((>= pace-pct 50)
       (format t "[L]    ⚠️ SLIGHTLY BEHIND (~,0f%)~%" pace-pct)
       (format t "[L]    💡 Consider slightly more aggressive approach~%"))
      (t
       (format t "[L]    ❌ SIGNIFICANTLY BEHIND (~,0f%)~%" pace-pct)
       (format t "[L]    💡 Review strategy, but don't chase losses~%")))
    (format t "~%")
    
    ;; Today's Plan
    (format t "[L] 📋 TODAY'S PLAN:~%")
    (format t "[L]    Daily target: ¥~:d~%" (round daily-target))
    (format t "[L]    Max daily loss: ¥~:d~%" (abs *resignation-threshold*))
    (format t "[L]    Active systems: Swarm + Memory + Leader + All Protections~%")
    (format t "~%")
    
    ;; Current Market
    (when *candle-history*
      (format t "[L] 📈 MARKET STATUS:~%")
      (format t "[L]    Regime: ~a | Volatility: ~a~%" 
              *current-regime* *current-volatility-state*)
      (when *current-leader*
        (format t "[L]    Current leader: ~a~%" 
                (leader-info-strategy-name *current-leader*))))
    
    (format t "~%")
    (format t "[L] 🐟🐟🐟 The school is ready. Let's go!~%")
    (format t "[L] ════════════════════════════════════════════════════~%~%")))

(defun maybe-generate-briefing ()
  "Generate briefing once per day at start"
  (let ((current-hour (mod (floor (get-universal-time) 3600) 24)))
    ;; Generate briefing at 00:00 UTC or first run
    (when (or (= *last-briefing-hour* -1)
              (and (= current-hour 0) (/= *last-briefing-hour* 0)))
      (setf *last-briefing-hour* current-hour)
      (generate-daily-briefing)
      (reset-danger-state))))

(defun swimmy-status ()
  "Generate current status in natural language - for Intent Trading"
  (let* ((progress (get-goal-progress))
         (pnl (round (getf progress :actual-pnl)))
         (pace (getf progress :pace-pct)))
    (format nil "現在 ¥~:d (~,0f% ペース). ~a. ~a."
            pnl pace
            (if (getf progress :on-track) "順調です" "ペースが遅れています")
            (if (has-resigned-p) "本日のトレード終了" "トレード中"))))


(defun update-drawdown (pnl)
  "Update drawdown tracking"
  (incf *current-equity* pnl)
  (when (> *current-equity* *peak-equity*)
    (setf *peak-equity* *current-equity*))
  (let ((dd (if (> *peak-equity* 0) 
                (* 100 (/ (- *peak-equity* *current-equity*) *peak-equity*))
                0)))
    (when (> dd *max-drawdown*) 
      (setf *max-drawdown* dd))
    (when (> dd *max-dd-percent*)
      (notify-discord (format nil "⚠️ DRAWDOWN ~,1f%! (Peak: ~,0f, Current: ~,0f)" 
                              dd *peak-equity* *current-equity*) :color 15158332))))

(defun calculate-lot-size ()
  "Dynamic lot size based on drawdown and streak"
  (let* ((dd-factor (max 0.5 (- 1.0 (/ *max-drawdown* 100))))  ; Reduce lot on DD
         (equity-factor (if (> *current-equity* 0) 
                            (min 2.0 (+ 1.0 (/ *current-equity* 1000)))  ; Scale with equity
                            1.0)))
    (max 0.01 (* *base-lot-size* dd-factor equity-factor))))

(defun ensure-real (x) (float (if (listp x) (or (car x) 1) (or x 1))))
(defun trading-allowed-p ()
  (if (< *daily-pnl* *daily-loss-limit*)
      (progn (format t "~%[L] ⛔ DAILY LOSS LIMIT~%") nil) t))
(defun arm-benched-p (idx) (member idx *benched-arms*))
(defun bench-arm (idx)
  (unless (member idx *benched-arms*)
    (push idx *benched-arms*)
    (format t "[L] 🪑 Arm ~d BENCHED~%" idx)))
(defun load-genome ()
  (with-open-file (in *genome-path* :direction :input :if-does-not-exist nil)
    (when in
      (setf *genome* (read in))
      (setf *arms* (getf *genome* :arms))
      (setf *portfolio-indices* (or (getf *genome* :portfolio-indices) (list 0)))
      (setf *memory* (getf *genome* :memory))
      (format t "[L] 🧬 ~d arms. Portfolio: ~a~%" (length *arms*) *portfolio-indices*))))
(defun save-genome ()
  (setf (getf *genome* :arms) *arms*)
  (setf (getf *genome* :portfolio-indices) *portfolio-indices*)
  (setf (getf *genome* :memory) *memory*)
  (with-open-file (out *genome-path* :direction :output :if-exists :supersede)
    (write *genome* :stream out :pretty t)))
(defun notify-discord (msg &key (color 3447003))
  (when (and *discord-webhook-url* msg (not (equal msg "NIL")))
    (handler-case
        (dex:post *discord-webhook-url*
                  :content (jsown:to-json (jsown:new-js ("embeds" (list (jsown:new-js ("title" "🐟 Apex") ("description" (format nil "~a" msg)) ("color" color))))))
                  :headers '(("Content-Type" . "application/json")) :read-timeout 3)
      (error (e) nil))))

;; Multi-currency Discord webhooks (symbol -> webhook URL)
(defparameter *symbol-webhooks* (make-hash-table :test 'equal))

(defun setup-symbol-webhooks ()
  "Setup Discord webhooks for each currency pair"
  ;; Default: use main webhook for all
  (setf (gethash "USDJPY" *symbol-webhooks*) (uiop:getenv "SWIMMY_DISCORD_USDJPY"))
  (setf (gethash "EURUSD" *symbol-webhooks*) (uiop:getenv "SWIMMY_DISCORD_EURUSD"))
  (setf (gethash "GBPUSD" *symbol-webhooks*) (uiop:getenv "SWIMMY_DISCORD_GBPUSD")))

(defun notify-discord-symbol (symbol msg &key (color 3447003))
  "Send Discord notification to symbol-specific channel"
  (let ((webhook (or (gethash symbol *symbol-webhooks*) *discord-webhook-url*)))
    (when (and webhook msg (not (equal msg "NIL")))
      (handler-case
          (dex:post webhook
                    :content (jsown:to-json (jsown:new-js ("embeds" (list (jsown:new-js 
                              ("title" (format nil "🐟 ~a" symbol)) 
                              ("description" (format nil "~a" msg)) 
                              ("color" color))))))
                    :headers '(("Content-Type" . "application/json")) :read-timeout 3)
        (error (e) nil)))))


;; NOTE: MULTI-CHANNEL DISCORD functions moved to src/lisp/core/discord.lisp



;;; ==========================================
;;; LIVE STATUS JSON (Discord Bot Sync)
;;; ==========================================
;;; Write live status to JSON file for Discord bot to read

(defparameter *live-status-path* "/home/swimmy/swimmy/.opus/live_status.json")
(defparameter *live-status-interval* 60)  ; Write every 60 seconds
(defparameter *last-status-write* 0)

(defun save-live-status ()
  "Write live status to JSON for Discord bot"
  (let ((now (get-universal-time)))
    ;; Only write every *live-status-interval* seconds
    (when (> (- now *last-status-write*) *live-status-interval*)
      (setf *last-status-write* now)
      (handler-case
          (progn
            ;; Ensure directory exists
            (ensure-directories-exist *live-status-path*)
            (with-open-file (out *live-status-path* :direction :output :if-exists :supersede)
              (let* ((progress (get-goal-progress))
                     (ecosystem-health (if (fboundp 'calculate-ecosystem-health)
                                           (* 100 (funcall 'calculate-ecosystem-health))
                                           0))
                     (leader-name (if (and (boundp '*current-leader*) *current-leader*)
                                      (leader-info-strategy-name *current-leader*)
                                      "UNKNOWN"))
                     (regime-str (if (and (boundp '*current-regime*) *current-regime*)
                                     (symbol-name *current-regime*)
                                     "UNKNOWN"))
                     (volatility-str (if (and (boundp '*volatility-regime*) *volatility-regime*)
                                         (symbol-name *volatility-regime*)
                                         "UNKNOWN"))
                     (danger (if (boundp '*danger-level*) *danger-level* 0))
                     ;; Tribe status for Discord
                     (hunter-sig (gethash :hunters *tribe-status*))
                     (shaman-sig (gethash :shamans *tribe-status*))
                     (breaker-sig (gethash :breakers *tribe-status*))
                     (raider-sig (gethash :raiders *tribe-status*)))
                (format out "{~%")
                (format out "  \"daily_pnl\": ~,2f,~%" *daily-pnl*)
                (format out "  \"accumulated_pnl\": ~,2f,~%" *accumulated-pnl*)
                (format out "  \"monthly_goal\": ~d,~%" *monthly-goal*)
                (format out "  \"goal_progress\": ~,2f,~%" (or (getf progress :progress-pct) 0))
                (format out "  \"regime\": \"~a\",~%" regime-str)
                (format out "  \"volatility\": \"~a\",~%" volatility-str)
                (format out "  \"leader\": \"~a\",~%" leader-name)
                (format out "  \"danger_level\": ~d,~%" danger)
                (format out "  \"ecosystem_health\": ~,0f,~%" ecosystem-health)
                ;; Trade counts
                (format out "  \"total_trades\": ~d,~%" *total-trades*)
                (format out "  \"warmup_progress\": ~d,~%" (min 100 (* 2 *total-trades*)))
                (format out "  \"warmup_complete\": ~a,~%" (if (>= *total-trades* 50) "true" "false"))
                ;; Tribe detailed status
                (format out "  \"tribes\": {~%")
                (format out "    \"hunters\": {\"direction\": \"~a\", \"confidence\": ~,0f, \"reason\": \"MACD+ADX+Kalman\"},~%"
                        (if hunter-sig (getf hunter-sig :direction) :hold)
                        (* 100 (or (and hunter-sig (getf hunter-sig :confidence)) 0)))
                (format out "    \"shamans\": {\"direction\": \"~a\", \"confidence\": ~,0f, \"reason\": \"RSI+BB Mean Reversion\"},~%"
                        (if shaman-sig (getf shaman-sig :direction) :hold)
                        (* 100 (or (and shaman-sig (getf shaman-sig :confidence)) 0)))
                (format out "    \"breakers\": {\"direction\": \"~a\", \"confidence\": ~,0f, \"reason\": \"Bollinger Breakout\"},~%"
                        (if breaker-sig (getf breaker-sig :direction) :hold)
                        (* 100 (or (and breaker-sig (getf breaker-sig :confidence)) 0)))
                (format out "    \"raiders\": {\"direction\": \"~a\", \"confidence\": ~,0f, \"reason\": \"EMA Scalp+Kalman\"}~%"
                        (if raider-sig (getf raider-sig :direction) :hold)
                        (* 100 (or (and raider-sig (getf raider-sig :confidence)) 0)))
                (format out "  },~%")
                ;; Tribe consensus
                (format out "  \"tribe_consensus\": {\"direction\": \"~a\", \"strength\": ~,0f},~%"
                        (or *tribe-direction* :hold)
                        (* 100 (or *tribe-consensus* 0)))
                (format out "  \"last_updated\": \"~a\"~%" (get-jst-str))
                (format out "}~%")))
            (format t "[L] 📝 Live status saved to JSON~%"))
        (error (e) 
          (format t "[L] ⚠️ Failed to save live status: ~a~%" e))))))

;;; ==========================================
;;; EVALUATOR AI
;;; ==========================================
;;; Extracted to src/lisp/core/evaluator.lisp


;;; ══════════════════════════════════════════════════════════════════

;;; ==========================================
;;; GOVERNANCE & PHILOSOPHY
;;; ==========================================
;;; NOTE: Constitution, Philosophy Logger, and High Council 
;;; have been extracted to src/lisp/core/governance.lisp
;;; (Strangler Fig Phase 3)




;;; ══════════════════════════════════════════════════════════════════
;;;  2028 VISION: EXTERNAL AGENT AWARENESS (外部エージェント認識)
;;; ══════════════════════════════════════════════════════════════════
;;; Preparation for Machine-to-Machine (M2M) economy
;;; Swimmy will eventually interact with other AI agents

(defparameter *known-agents* nil)
(defparameter *agent-communication-protocol* "SWIMMY-AGENT-V1")

(defstruct external-agent
  id              ; Unique identifier
  name            ; Human-readable name
  type            ; :market-ai, :analysis-ai, :hedge-ai, :news-ai
  endpoint        ; How to reach this agent (future: URL, port, etc.)
  trust-level     ; 0.0-1.0 (how much we trust this agent)
  capabilities    ; List of what this agent can do
  last-contact    ; Last interaction timestamp
  interaction-history)  ; Past interactions

(defun register-agent (id name type &key (trust-level 0.5) capabilities)
  "Register an external AI agent that Swimmy might interact with"
  (let ((agent (make-external-agent
                :id id
                :name name
                :type type
                :endpoint nil
                :trust-level trust-level
                :capabilities capabilities
                :last-contact nil
                :interaction-history nil)))
    (push agent *known-agents*)
    (format t "[L] 🤝 AGENT REGISTERED: ~a (~a) [Trust: ~,0f%]~%"
            name type (* 100 trust-level))
    agent))

(defun get-agent-by-id (id)
  "Find an agent by ID"
  (find id *known-agents* :key #'external-agent-id))

(defun update-agent-trust (id delta)
  "Update trust level based on interactions"
  (let ((agent (get-agent-by-id id)))
    (when agent
      (setf (external-agent-trust-level agent)
            (max 0.0 (min 1.0 (+ (external-agent-trust-level agent) delta))))
      (format t "[L] 🤝 AGENT TRUST UPDATE: ~a → ~,0f%~%"
              (external-agent-name agent)
              (* 100 (external-agent-trust-level agent))))))

(defun query-agent (id query)
  "Query an external agent (FUTURE: will actually communicate)"
  (let ((agent (get-agent-by-id id)))
    (if agent
        (progn
          (format t "[L] 🤝 AGENT QUERY: ~a ← ~a~%" 
                  (external-agent-name agent) query)
          (setf (external-agent-last-contact agent) (get-universal-time))
          ;; Future: actually send query
          ;; For now, just log and return nil
          nil)
        (format t "[L] ⚠️ Unknown agent: ~a~%" id))))

(defun list-agents ()
  "List all known agents"
  (format t "[L] 🤝 Known Agents:~%")
  (dolist (agent *known-agents*)
    (format t "[L]    ~a (~a) Trust: ~,0f%~%"
            (external-agent-name agent)
            (external-agent-type agent)
            (* 100 (external-agent-trust-level agent)))))

;; Pre-register some conceptual agents for future
(defun initialize-agent-network ()
  "Initialize the agent network with conceptual agents"
  (register-agent :opus "Opus-Partner" :analysis-ai :trust-level 1.0
                  :capabilities '(:code-review :strategy-analysis :debugging))
  (register-agent :market-data "Market-Data-AI" :market-ai :trust-level 0.8
                  :capabilities '(:price-feed :volume-analysis))
  (format t "[L] 🌐 Agent Network initialized~%"))


;;; ══════════════════════════════════════════════════════════════════
;;;  COUNCIL OF ELDERS (長老会議)
;;; ══════════════════════════════════════════════════════════════════
;;; Wisdom from the past guides the present

(defparameter *hall-of-fame* nil)  ; Frozen legendary strategies
(defparameter *hall-of-fame-path* "/home/swimmy/swimmy/.opus/hall_of_fame.lisp")

(defstruct elder
  name              ; Strategy name
  peak-pnl          ; Highest profit achieved
  era               ; When they were active
  speciality        ; What they were good at
  wisdom            ; Lessons learned
  vote-weight)      ; How much their vote counts

(defun induct-to-hall-of-fame (strategy-name peak-pnl speciality wisdom)
  "Induct a legendary strategy into the Hall of Fame"
  (let ((elder (make-elder
                :name strategy-name
                :peak-pnl peak-pnl
                :era (get-date-string)
                :speciality speciality
                :wisdom wisdom
                :vote-weight (min 3 (/ peak-pnl 1000)))))  ; Higher profit = more weight
    (push elder *hall-of-fame*)
    
    ;; Ceremony
    (format t "~%[L] ═══════════════════════════════════════~%")
    (format t "[L] 🏛️ HALL OF FAME INDUCTION~%")
    (format t "[L] ═══════════════════════════════════════~%")
    (format t "[L] 👑 ~a joins the Elders!~%" strategy-name)
    (format t "[L] 💰 Peak PnL: ¥~:d~%" (round peak-pnl))
    (format t "[L] 📖 Wisdom: \"~a\"~%" wisdom)
    (format t "[L] ═══════════════════════════════════════~%~%")
    
    elder))

;;; ═══════════════════════════════════════════════════════════════════
;;; ELDER LESSONS V3.0 - ML Engineer Feedback Implementation
;;; 6-Dimension Feature Engineering + Structured Pattern Learning
;;; ═══════════════════════════════════════════════════════════════════

;; Pattern structure: (count total-pnl avg-pnl last-seen)
(defparameter *elder-lessons* (make-hash-table :test 'equal))

;; Failure history for proper ML analysis
(defparameter *failure-history* nil)
(defparameter *max-failure-history* 100)

;; V3.0: Success counter for win rate (PM feedback)
(defparameter *success-count* 0)

;; V3.0: Win rate trend history (PM feedback - show trend)
(defparameter *win-rate-history* nil)  ; List of (timestamp win-rate)
(defparameter *max-win-rate-history* 7)  ; Track 7 days

(defstruct failure-record
  timestamp
  ;; 6-dimension feature vector (ML Engineer recommendation)
  regime          ; :trending / :ranging
  volatility      ; :high / :normal / :low
  session         ; :tokyo / :london / :ny / :off
  hour            ; 0-23
  rsi-zone        ; :oversold / :neutral / :overbought
  price-position  ; :above-ma / :below-ma / :at-ma
  ;; Outcome
  pnl
  symbol
  direction)

(defun rsi-to-zone (rsi)
  "Convert RSI value to zone"
  (cond
    ((null rsi) :unknown)
    ((< rsi 30) :oversold)
    ((> rsi 70) :overbought)
    (t :neutral)))

(defun get-price-position (history)
  "Get price position relative to SMA50"
  (when (and history (> (length history) 50))
    (let* ((close (candle-close (first history)))
           (sma50 (/ (reduce #'+ (mapcar #'candle-close (subseq history 0 50))) 50))
           (diff-pct (/ (- close sma50) sma50)))
      (cond
        ((> diff-pct 0.005) :above-ma)
        ((< diff-pct -0.005) :below-ma)
        (t :at-ma)))))

(defun learn-from-failure (context pnl)
  "V3.0: 6-dimension feature learning from failed trades"
  (let* ((regime (or (getf context :regime) :unknown))
         (volatility (or (getf context :volatility-state) :normal))
         (session (or (getf context :session) :unknown))
         (rsi (getf context :rsi-value))
         (hour (mod (floor (get-universal-time) 3600) 24))
         (rsi-zone (rsi-to-zone rsi))
         (price-pos (or (getf context :price-position) :unknown))
         ;; Create 6-dimension pattern key
         (pattern-key (format nil "~a|~a|~a|~d|~a|~a" 
                              regime volatility session hour rsi-zone price-pos)))
    
    ;; Store structured failure record
    (push (make-failure-record
           :timestamp (get-universal-time)
           :regime regime
           :volatility volatility
           :session session
           :hour hour
           :rsi-zone rsi-zone
           :price-position price-pos
           :pnl pnl
           :symbol (getf context :symbol)
           :direction (getf context :direction))
          *failure-history*)
    
    ;; Trim history
    (when (> (length *failure-history*) *max-failure-history*)
      (setf *failure-history* (subseq *failure-history* 0 *max-failure-history*)))
    
    ;; Update pattern statistics
    (let ((existing (gethash pattern-key *elder-lessons*)))
      (if existing
          ;; Update existing: (count total-pnl avg-pnl last-seen)
          (let* ((count (1+ (first existing)))
                 (total-pnl (+ (second existing) pnl))
                 (avg-pnl (/ total-pnl count)))
            (setf (gethash pattern-key *elder-lessons*)
                  (list count total-pnl avg-pnl (get-universal-time)))
            (when (>= count 5)  ;; V3.0: Increased from 3 to 5 (Professor feedback: n=3 not significant)
              (format t "[L] 👴 パターン学習: ~a → ~d回, 平均損失 ¥~,0f~%" 
                      pattern-key count avg-pnl)))
          ;; New pattern
          (setf (gethash pattern-key *elder-lessons*)
                (list 1 pnl pnl (get-universal-time)))))
    
    ;; Also update simple dimension counters for quick lookup
    (incf (gethash (format nil "dim:regime:~a" regime) *elder-lessons* 0))
    (incf (gethash (format nil "dim:vol:~a" volatility) *elder-lessons* 0))
    (incf (gethash (format nil "dim:session:~a" session) *elder-lessons* 0))
    (incf (gethash (format nil "dim:hour:~d" hour) *elder-lessons* 0))
    (incf (gethash (format nil "dim:rsi:~a" rsi-zone) *elder-lessons* 0))
    (incf (gethash (format nil "dim:price:~a" price-pos) *elder-lessons* 0))))

(defparameter *elder-lessons-last-decay* (get-universal-time))
(defparameter *elder-decay-interval* 86400)  ; 24 hours in seconds
(defparameter *elder-decay-rate* 0.9)        ; Reduce by 10% per day

(defun decay-elder-lessons ()
  "Apply decay to elder lessons - old lessons fade as market changes"
  (let ((now (get-universal-time)))
    (when (> (- now *elder-lessons-last-decay*) *elder-decay-interval*)
      (setf *elder-lessons-last-decay* now)
      (maphash (lambda (key value)
                 (let ((new-val (* value *elder-decay-rate*)))
                   (if (< new-val 0.5)
                       (remhash key *elder-lessons*)  ; Remove if too small
                       (setf (gethash key *elder-lessons*) new-val))))
               *elder-lessons*)
      (format t "[L] 👴 長老の記憶が薄れる...（減衰適用）~%"))))

(defun elder-learned-lesson-p (lesson-key threshold)
  "Check if elders have learned a specific lesson (V3.0: handles structured patterns)"
  (decay-elder-lessons)  ; Apply decay before checking
  (let ((lesson (gethash lesson-key *elder-lessons* nil)))
    (cond
      ;; New structured pattern: (count total-pnl avg-pnl last-seen)
      ((and (listp lesson) (>= (length lesson) 1))
       (>= (first lesson) threshold))
      ;; Old simple counter format (for dim: keys)
      ((numberp lesson)
       (>= lesson threshold))
      (t nil))))

(defun elder-vote (proposal context)
  "Ask elders to vote on a proposal. Returns :approve, :caution, or :reject"
  (let ((approve-votes 0)
        (reject-votes 0)
        (total-weight 0))
    
    (dolist (elder *hall-of-fame*)
      (let ((weight (elder-vote-weight elder)))
        (incf total-weight weight)
        
        ;; Elder logic based on their wisdom
        (cond
          ;; Elder who learned about volatility warns during high vol
          ((and (search "volatility" (string-downcase (elder-wisdom elder)))
                (eq (getf context :volatility-state) :extreme))
           (incf reject-votes weight)
           (format t "[L] 👴 Elder ~a: 「ボラが高すぎる。わしの時代もそうだった。」~%"
                   (elder-name elder)))
          
          ;; Elder who learned about patience during ranging markets
          ((and (search "patience" (string-downcase (elder-wisdom elder)))
                (eq (getf context :regime) :ranging))
           (incf reject-votes (* 0.5 weight))
           (format t "[L] 👴 Elder ~a: 「待て。レンジでは焦るな。」~%"
                   (elder-name elder)))
          
          ;; Otherwise, approve
          (t
           (incf approve-votes weight)))))
    
    ;; ═══════════════════════════════════════
    ;; DYNAMIC LESSONS from failure analysis
    ;; ═══════════════════════════════════════
    (when (elder-learned-lesson-p "extreme-volatility" 3)
      (when (eq (getf context :volatility-state) :extreme)
        (incf reject-votes 2)
        (format t "[L] 👴 集合知: 「過去の失敗から学んだ。極端なボラは危険だ。」~%")))
    
    (when (elder-learned-lesson-p "ranging-losses" 3)
      (when (eq (getf context :regime) :ranging)
        (incf reject-votes 1)
        (format t "[L] 👴 集合知: 「レンジ相場での失敗を覚えている。」~%")))
    
    ;; Decision
    (cond
      ((> reject-votes (* 0.6 total-weight)) :reject)
      ((> reject-votes (* 0.3 total-weight)) :caution)
      (t :approve))))

(defun save-hall-of-fame ()
  "Save Hall of Fame to file"
  (handler-case
      (with-open-file (out *hall-of-fame-path* :direction :output :if-exists :supersede)
        (write *hall-of-fame* :stream out :pretty t))
    (error (e) nil)))


;;; ══════════════════════════════════════════════════════════════════
;;;  REPUTATION SYSTEM (評判システム)
;;; ══════════════════════════════════════════════════════════════════
;;; Trust is earned, not given

(defparameter *reputation-scores* (make-hash-table :test 'equal))
(defparameter *gossip-log* nil)

(defstruct reputation
  strategy-name
  trust-score       ; 0.0 - 1.0
  profit-score      ; Based on actual profit
  reliability       ; How often it follows through
  recklessness      ; Did it push limits?
  peer-reviews)     ; What other strategies think

(defun get-reputation (strategy-name)
  "Get or create reputation for a strategy"
  (or (gethash strategy-name *reputation-scores*)
      (setf (gethash strategy-name *reputation-scores*)
            (make-reputation
             :strategy-name strategy-name
             :trust-score 0.5
             :profit-score 0
             :reliability 0.5
             :recklessness 0
             :peer-reviews nil))))

(defun update-reputation (strategy-name outcome &key (pnl 0) (pushed-limits nil))
  "Update reputation after a trade"
  (let ((rep (get-reputation strategy-name)))
    (case outcome
      (:win
       (setf (reputation-trust-score rep) 
             (min 1.0 (+ (reputation-trust-score rep) 0.05)))
       (incf (reputation-profit-score rep) pnl)
       (incf (reputation-reliability rep) 0.02))
      (:loss
       (setf (reputation-trust-score rep)
             (max 0.0 (- (reputation-trust-score rep) 0.03)))
       (incf (reputation-profit-score rep) pnl)
       (decf (reputation-reliability rep) 0.01)))
    
    (when pushed-limits
      (incf (reputation-recklessness rep))
      ;; Other strategies will gossip
      (add-gossip strategy-name "pushed limits" -0.1))))

(defun add-gossip (about-strategy topic sentiment)
  "Add gossip about a strategy"
  (let ((gossip (list :about about-strategy
                      :topic topic
                      :sentiment sentiment
                      :time (get-universal-time))))
    (push gossip *gossip-log*)
    
    ;; Affect reputation
    (let ((rep (get-reputation about-strategy)))
      (setf (reputation-trust-score rep)
            (max 0.0 (min 1.0 (+ (reputation-trust-score rep) sentiment)))))
    
    (format t "[L] 💬 GOSSIP: 「~aが~aしたらしい...」~%"
            about-strategy topic)))

(defun get-tribal-mood ()
  "Analyze recent gossip to get tribal mood"
  (let* ((recent (subseq *gossip-log* 0 (min 10 (length *gossip-log*))))
         (total-sentiment (reduce #'+ recent :key (lambda (g) (getf g :sentiment)) :initial-value 0)))
    (cond
      ((> total-sentiment 0.5) :optimistic)
      ((< total-sentiment -0.5) :pessimistic)
      (t :neutral))))


;;; ══════════════════════════════════════════════════════════════════
;;;  TRIBAL DIALECT (部族方言)
;;; ══════════════════════════════════════════════════════════════════
;;; Unique language for market patterns

(defparameter *tribal-dialect* (make-hash-table :test 'equal))

(defun define-pattern (pattern-name description detection-fn)
  "Define a tribal pattern with unique name"
  (setf (gethash pattern-name *tribal-dialect*)
        (list :name pattern-name
              :description description
              :detector detection-fn
              :times-seen 0)))

;; Initialize tribal dialect
(defun initialize-tribal-dialect ()
  "Initialize the tribal language"
  (format t "[L] 📚 Loading Tribal Dialect...~%")
  
  (define-pattern "Dragon-Tail"
    "価格は上昇しているがRSIは下落（隠れダイバージェンス）"
    (lambda (history)
      (when (>= (length history) 10)
        (let* ((recent (subseq history 0 10))
               (prices (mapcar #'candle-close recent))
               (price-trend (- (first prices) (car (last prices)))))
          ;; Simplified detection
          (> price-trend 0)))))
  
  (define-pattern "Brittle-Silence"
    "ボラは低いが板が薄い。暴発寸前の静寂"
    (lambda (history)
      (when (>= (length history) 20)
        (let ((vol (calculate-volatility (subseq history 0 20))))
          (and vol (< vol 0.003))))))  ; Very low vol
  
  (define-pattern "Storm-Warning"
    "急激なボラティリティ上昇。嵐が来る"
    (lambda (history)
      (eq *current-volatility-state* :extreme)))
  
  (define-pattern "The-Calm"
    "全氏族が同意。珍しい静寂"
    (lambda (history)
      ;; When all swarm votes agree
      t))  ; Placeholder
  
  (format t "[L] 📚 ~d patterns defined~%" (hash-table-count *tribal-dialect*)))

(defun detect-patterns (history)
  "Detect active patterns and announce in tribal language"
  (let ((active-patterns nil))
    (maphash (lambda (name info)
               (let ((detector (getf info :detector)))
                 (when (and detector (funcall detector history))
                   (push name active-patterns)
                   (incf (getf info :times-seen)))))
             *tribal-dialect*)
    
    (when active-patterns
      (format t "[L] 🗣️ TRIBAL SIGHT: ~{~a~^, ~}~%" active-patterns))
    
    active-patterns))




;;; ENHANCED DISCORD FEEDBACK (改善1/3)
;;; ==========================================

(defun send-discord-briefing ()
  "Send daily briefing to Discord"
  (let* ((progress (get-goal-progress))
         (msg (format nil "🌅 **Daily Briefing**~%~%🎯 Goal: ¥~:d / ¥~:d (~,1f%)~%📊 Pace: ~,0f% (~a)~%📋 Today's target: ¥~:d~%~%🐟 Swimmy ready!"
                      (round (getf progress :actual-pnl))
                      *monthly-goal*
                      (getf progress :progress-pct)
                      (getf progress :pace-pct)
                      (if (getf progress :on-track) "ON TRACK" "BEHIND")
                      (get-daily-target))))
    (notify-discord msg :color 3066993)))

(defun send-discord-trade-update (symbol direction action pnl)
  "Send trade update to Discord with Intent Trading context"
  (let* ((progress (get-goal-progress))
         (emoji (case action (:open "📈") (:close (if (> pnl 0) "✅" "❌")) (t "📊")))
         (msg (format nil "~a **~a ~a**~%~%PnL: ¥~,2f~%Daily: ¥~:d / ¥~:d~%Goal: ~,1f% complete"
                      emoji direction symbol pnl
                      (round *daily-pnl*) (get-daily-target)
                      (getf progress :progress-pct))))
    (notify-discord-symbol symbol msg :color (if (> pnl 0) 3066993 15158332))))

;;; ==========================================
;;; PERFORMANCE VERIFICATION (改善2/3)
;;; ==========================================

(defparameter *performance-log* nil)
(defparameter *performance-log-max* 1000)

(defstruct performance-record
  timestamp
  daily-pnl
  accumulated-pnl
  goal-progress
  trades-today
  win-rate
  sharpe-estimate)

(defun record-daily-performance ()
  "Record end-of-day performance for verification"
  (let ((record (make-performance-record
                 :timestamp (get-universal-time)
                 :daily-pnl *daily-pnl*
                 :accumulated-pnl *accumulated-pnl*
                 :goal-progress (getf (get-goal-progress) :progress-pct)
                 :trades-today *total-trades*
                 :win-rate (float (/ *consecutive-wins* (max 1 (+ *consecutive-wins* *consecutive-losses*))))
                 :sharpe-estimate 0.0)))
    (push record *performance-log*)
    (when (> (length *performance-log*) *performance-log-max*)
      (setf *performance-log* (subseq *performance-log* 0 *performance-log-max*)))
    record))

(defun get-performance-summary ()
  "Generate performance summary for verification"
  (let* ((recent (subseq *performance-log* 0 (min 5 (length *performance-log*))))
         (avg-pnl (if recent (/ (reduce #'+ (mapcar #'performance-record-daily-pnl recent)) (length recent)) 0)))
    (format nil "📊 Performance Summary~%Average daily: ¥~,2f~%Best day: ¥~,2f~%Total records: ~d"
            avg-pnl
            (if recent (apply #'max (mapcar #'performance-record-daily-pnl recent)) 0)
            (length *performance-log*))))

;;; ==========================================
;;; NATURAL LANGUAGE INTERFACE (改善3/3)
;;; ==========================================

(defun swimmy-ask (query)
  "Natural language interface for Intent Trading - process user queries"
  (let ((q (string-downcase query)))
    (cond
      ;; Status queries
      ((or (search "status" q) (search "状況" q) (search "どう" q))
       (swimmy-status))
      
      ;; Goal queries
      ((or (search "goal" q) (search "目標" q) (search "いくら" q))
       (format nil "今月の目標: ¥~:d~%現在: ¥~:d (~,1f%)~%残り: ¥~:d"
               *monthly-goal*
               (round (getf (get-goal-progress) :actual-pnl))
               (getf (get-goal-progress) :progress-pct)
               (round (getf (get-goal-progress) :remaining))))
      
      ;; Performance queries
      ((or (search "performance" q) (search "成績" q) (search "結果" q))
       (get-performance-summary))
      
      ;; Market queries
      ((or (search "market" q) (search "相場" q) (search "マーケット" q))
       (format nil "レジーム: ~a~%ボラティリティ: ~a~%リーダー: ~a"
               *current-regime* *current-volatility-state*
               (if *current-leader* (leader-info-strategy-name *current-leader*) "未選出")))
      
      ;; Help
      (t
       "使えるクエリ: status/状況, goal/目標, performance/成績, market/相場"))))

;;; ==========================================
;;; OPUS INTEGRATION
;;; ==========================================
;;; Extracted to src/lisp/system/opus.lisp

;;; ==========================================
;;; DAILY HANDOFF SYSTEM (継続的AIコラボ)
;;; ==========================================
;;; Creates markdown file that Opus reads in next chat session

(defparameter *handoff-path* "/home/swimmy/swimmy/.opus/daily_handoff.md")

(defun generate-daily-handoff ()
  "Generate daily handoff markdown for Opus AI partner"
  (let* ((progress (get-goal-progress))
         (issues nil))
    
    ;; Detect issues
    (when (< (getf progress :pace-pct) 80)
      (push "ペースが目標の80%を下回っている" issues))
    (when (> *danger-level* 1)
      (push (format nil "危険レベルが高い: ~d" *danger-level*) issues))
    (when (has-resigned-p)
      (push "本日のトレードを投了した" issues))
    (when (>= *consecutive-losses* 3)
      (push (format nil "~d連敗中" *consecutive-losses*) issues))
    
    ;; Generate markdown
    (with-open-file (out *handoff-path* :direction :output :if-exists :supersede)
      (format out "# 🐟 Swimmy Daily Handoff~%")
      (format out "## Opus AI パートナーへの引き継ぎ~%~%")
      (format out "**日付**: ~a~%" (get-date-string))
      (format out "**更新時刻**: ~a~%" (get-time-string))
      (format out "**更新者**: Swimmy Bot (自動生成)~%~%")
      (format out "---~%~%")
      
      ;; Summary table
      (format out "## 📊 今日のサマリー~%~%")
      (format out "| 項目 | 値 |~%")
      (format out "|------|-----|~%")
      (format out "| 日次PnL | ¥~:d |~%" (round *daily-pnl*))
      (format out "| 累計PnL | ¥~:d |~%" (round (getf progress :actual-pnl)))
      (format out "| 目標進捗 | ~,1f% |~%" (getf progress :progress-pct))
      (format out "| ペース | ~,0f% |~%" (getf progress :pace-pct))
      (format out "| リーダー | ~a |~%" 
              (if *current-leader* (leader-info-strategy-name *current-leader*) "未選出"))
      (format out "| レジーム | ~a |~%" *current-regime*)
      (format out "| ボラティリティ | ~a |~%" *current-volatility-state*)
      (format out "| 危険レベル | ~d |~%" *danger-level*)
      (format out "| 投了 | ~a |~%" (if (has-resigned-p) "はい" "いいえ"))
      (format out "~%---~%~%")
      
      ;; Issues
      (format out "## 🔍 発生した問題~%~%")
      (if issues
          (dolist (issue issues)
            (format out "- ~a~%" issue))
          (format out "- 特になし~%"))
      (format out "~%---~%~%")
      
      ;; Questions for Opus
      (format out "## ❓ Opusへの質問~%~%")
      (let ((pending (remove-if-not (lambda (r) (eq (improvement-request-status r) :pending))
                                    *improvement-requests*)))
        (if pending
            (dolist (req pending)
              (format out "- [~a] ~a~%" 
                      (improvement-request-category req)
                      (improvement-request-description req)))
            (format out "- なし（順調）~%")))
      (format out "~%---~%~%")
      
      ;; Previous recommendations placeholder
      (format out "## 💡 前回のOpusからの提案~%~%")
      (format out "<!-- Opusが分析後にここに追記 -->~%")
      (format out "- 次回セッションで更新~%~%")
      (format out "---~%~%")
      
      ;; Actions taken placeholder
      (format out "## ✅ 実施したアクション~%~%")
      (format out "<!-- 前回の提案に対して実施したこと -->~%")
      (format out "- 次回セッションで更新~%~%")
      (format out "---~%~%")
      
      ;; Notes
      (format out "## 📝 メモ~%~%")
      (format out "<!-- 自由記述欄 -->~%~%")
      (format out "---~%~%")
      (format out "*このファイルはSwimmyとOpusの継続的コラボレーション用です。*~%")
      (format out "*`/daily-review` でOpusが分析を開始します。*~%"))
    
    (format t "[L] 📋 Daily handoff generated: ~a~%" *handoff-path*)
    *handoff-path*))

(defun get-date-string ()
  "Get current date as string"
  (multiple-value-bind (s m h day month year) (decode-universal-time (get-universal-time))
    (declare (ignore s m h))
    (format nil "~d-~2,'0d-~2,'0d" year month day)))

(defun get-time-string ()
  "Get current time as string"
  (multiple-value-bind (s m h) (decode-universal-time (get-universal-time))
    (format nil "~2,'0d:~2,'0d:~2,'0d UTC" h m s)))

;;; ==========================================
;;; AUTONOMOUS MODE (自律運用モード)
;;; ==========================================
;;; For when user isn't chatting - critical alerts via Discord

(defparameter *autonomous-mode* t)  ; Always on
(defparameter *critical-alert-threshold* -3000)  ; Alert if daily loss > this
(defparameter *last-weekly-summary* 0)

(defun send-critical-discord-alert (alert-type message)
  "Send critical alert to Discord - use sparingly!"
  (when *autonomous-mode*
    (let ((msg (format nil "🚨 **CRITICAL ALERT** 🚨~%~%**Type**: ~a~%**Message**: ~a~%~%*This is an autonomous notification. Check when you can.*" 
                       alert-type message)))
      (notify-discord msg :color 15158332))))  ; Red

(defun check-autonomous-alerts ()
  "Check for critical issues and send Discord alerts"
  (when *autonomous-mode*
    ;; Critical: Large daily loss
    (when (< *daily-pnl* *critical-alert-threshold*)
      (send-critical-discord-alert 
       "LARGE LOSS" 
       (format nil "Daily loss ¥~:d exceeds threshold ¥~:d" 
               (round *daily-pnl*) *critical-alert-threshold*)))
    
    ;; Critical: Resigned early
    (when (has-resigned-p)
      (let* ((progress (get-goal-progress))
             (pace (getf progress :pace-pct)))
        (when (< pace 50)
          (send-critical-discord-alert 
           "EARLY RESIGNATION"
           (format nil "Trading stopped at ~,0f% pace. Check conditions." pace)))))
    
    ;; Warning: 5+ consecutive losses
    (when (>= *consecutive-losses* 5)
      (send-critical-discord-alert 
       "LOSING STREAK"
       (format nil "~d consecutive losses. Strategy may need review." *consecutive-losses*)))))

(defun generate-weekly-summary ()
  "Generate weekly summary (call once per week)"
  (let* ((progress (get-goal-progress))
         (summary-path "/home/swimmy/swimmy/.opus/weekly_summary.md"))
    
    (with-open-file (out summary-path :direction :output :if-exists :supersede)
      (format out "# 📊 Swimmy 週次サマリー~%~%")
      (format out "**生成日**: ~a~%~%" (get-date-string))
      
      (format out "## パフォーマンス~%~%")
      (format out "| 項目 | 値 |~%")
      (format out "|------|-----|~%")
      (format out "| 累計PnL | ¥~:d |~%" (round (getf progress :actual-pnl)))
      (format out "| 目標達成率 | ~,1f% |~%" (getf progress :progress-pct))
      (format out "| ペース | ~,0f% |~%" (getf progress :pace-pct))
      (format out "~%")
      
      (format out "## 学習したパターン~%~%")
      (dolist (p (subseq *learned-patterns* 0 (min 5 (length *learned-patterns*))))
        (format out "- ~a (確信度: ~,0f%)~%" 
                (learned-pattern-description p)
                (* 100 (learned-pattern-confidence p))))
      (format out "~%")
      
      (format out "## Opusへの質問~%~%")
      (format out "- 今週のパフォーマンスをどう評価しますか？~%")
      (format out "- 戦略の調整は必要ですか？~%")
      (format out "- 次週の市場予測と対策は？~%")
      (format out "~%")
      
      (format out "*週次レビューを依頼: `/daily-review` で Opus と対話*~%"))
    
    ;; Send Discord notification
    (notify-discord 
     (format nil "📊 **週次サマリー生成完了**~%~%累計: ¥~:d (~,1f%)~%~%詳細: .opus/weekly_summary.md"
             (round (getf progress :actual-pnl))
             (getf progress :progress-pct))
     :color 3447003)
    
    (setf *last-weekly-summary* (get-universal-time))
    summary-path))

;;; ==========================================
;;; A/B TEST FRAMEWORK (A/Bテスト)
;;; ==========================================
;;; For comparing different strategies/parameters

(defparameter *ab-tests* nil)
(defparameter *current-ab-test* nil)

(defstruct ab-test
  name
  variant-a    ; Configuration for A
  variant-b    ; Configuration for B
  current      ; :a or :b
  results-a    ; PnL list for A
  results-b    ; PnL list for B
  trades-a
  trades-b
  start-time
  status)      ; :running, :complete

(defun create-ab-test (name variant-a variant-b)
  "Create a new A/B test"
  (let ((test (make-ab-test
               :name name
               :variant-a variant-a
               :variant-b variant-b
               :current :a
               :results-a nil
               :results-b nil
               :trades-a 0
               :trades-b 0
               :start-time (get-universal-time)
               :status :running)))
    (setf *current-ab-test* test)
    (push test *ab-tests*)
    (format t "[L] 🧪 A/B TEST STARTED: ~a~%" name)
    (format t "[L] 🧪 Variant A: ~a~%" variant-a)
    (format t "[L] 🧪 Variant B: ~a~%" variant-b)
    test))

(defun switch-ab-variant ()
  "Switch to the other variant"
  (when *current-ab-test*
    (setf (ab-test-current *current-ab-test*)
          (if (eq (ab-test-current *current-ab-test*) :a) :b :a))
    (format t "[L] 🧪 A/B: Switched to variant ~a~%" 
            (ab-test-current *current-ab-test*))))

(defun record-ab-result (pnl)
  "Record a result for current A/B test variant"
  (when *current-ab-test*
    (if (eq (ab-test-current *current-ab-test*) :a)
        (progn
          (push pnl (ab-test-results-a *current-ab-test*))
          (incf (ab-test-trades-a *current-ab-test*)))
        (progn
          (push pnl (ab-test-results-b *current-ab-test*))
          (incf (ab-test-trades-b *current-ab-test*))))))

(defun get-ab-test-results ()
  "Get current A/B test results"
  (when *current-ab-test*
    (let* ((a-results (ab-test-results-a *current-ab-test*))
           (b-results (ab-test-results-b *current-ab-test*))
           (a-sum (reduce #'+ a-results :initial-value 0))
           (b-sum (reduce #'+ b-results :initial-value 0))
           (a-avg (if a-results (/ a-sum (length a-results)) 0))
           (b-avg (if b-results (/ b-sum (length b-results)) 0)))
      (format t "[L] 🧪 A/B TEST RESULTS: ~a~%" (ab-test-name *current-ab-test*))
      (format t "[L] 🧪 Variant A: ~d trades, total ¥~,2f, avg ¥~,2f~%" 
              (ab-test-trades-a *current-ab-test*) a-sum a-avg)
      (format t "[L] 🧪 Variant B: ~d trades, total ¥~,2f, avg ¥~,2f~%"
              (ab-test-trades-b *current-ab-test*) b-sum b-avg)
      (format t "[L] 🧪 Winner: ~a~%" (if (> a-avg b-avg) "A" "B"))
      (list :winner (if (> a-avg b-avg) :a :b)
            :a-avg a-avg :b-avg b-avg
            :a-trades (ab-test-trades-a *current-ab-test*)
            :b-trades (ab-test-trades-b *current-ab-test*)))))




(defun calculate-sma (n history)
  (when (>= (length history) n)
    (/ (reduce #'+ (mapcar #'candle-close (subseq history 0 n))) n)))

(defun calculate-adx (history &optional (n 14))
  (when (>= (length history) (+ n 2))
    (let ((plus-dm 0) (minus-dm 0) (tr-sum 0))
      (dotimes (i n)
        (let* ((c (nth i history)) (p (nth (1+ i) history)))
          (incf plus-dm (max 0 (- (candle-high c) (candle-high p))))
          (incf minus-dm (max 0 (- (candle-low p) (candle-low c))))
          (incf tr-sum (- (candle-high c) (candle-low c)))))
      (/ (abs (- plus-dm minus-dm)) (max (+ plus-dm minus-dm) 0.001) 0.01))))

(defun detect-regime ()
  (let ((adx (calculate-adx *candle-history*)))
    (if (and adx (> adx 25)) "TREND" "RANGE")))

(defun call-gemini (prompt)
  (when *gemini-api-key*
    (handler-case
        (let* ((url (format nil "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash-exp:generateContent?key=~a" *gemini-api-key*))
               (body (jsown:to-json (jsown:new-js 
                       ("contents" (list (jsown:new-js ("parts" (list (jsown:new-js ("text" prompt))))))) 
                       ("generationConfig" (jsown:new-js ("temperature" 0.5) ("maxOutputTokens" 512))))))
               (resp (dex:post url :content body :headers '(("Content-Type" . "application/json")) :read-timeout 15))
               (json (jsown:parse resp)))
          (jsown:val (nth 0 (jsown:val (jsown:val (nth 0 (jsown:val json "candidates")) "content") "parts")) "text"))
      (error (e) nil))))

;; 旧dream関数は削除済み - dream-codeがdreamer2.lispで定義
(defun box-muller () (* (sqrt (* -2.0 (log (max 1e-10 (random 1.0))))) (cos (* 2.0 pi (random 1.0)))))
(defun random-gamma (k)
  (setf k (max 0.01 (ensure-real k)))
  (if (< k 1.0) (* (random-gamma (1+ k)) (expt (max 1e-10 (random 1.0)) (/ 1.0 k)))
      (let* ((d (- k 0.333)) (c (/ 1.0 (sqrt (* 9.0 d)))))
        (loop (let* ((x (box-muller)) (v (expt (+ 1.0 (* c x)) 3)))
                (when (and (> v 0) (< (random 1.0) (- 1.0 (* 0.0331 (expt x 4))))) (return (* d v))))))))
(defun rebalance-portfolio ()
  (let ((scores nil))
    ;; Score from existing arms (Thompson Sampling)
    (dotimes (i (length *arms*))
      (unless (arm-benched-p i)
        (let* ((stats (cdr (nth i *arms*)))
               (a (ensure-real (car stats))) (b (ensure-real (cdr stats)))
               (ga (random-gamma a)) (gb (random-gamma b))
               (base-score (/ ga (+ ga gb 1e-10)))
               ;; Boost score if arm matches a high-Sharpe evolved strategy
               (boost (get-sharpe-boost i))
               (score (+ base-score boost)))
          (push (cons score i) scores))))
    (setf scores (sort scores #'> :key #'car))
    (let ((new-p (mapcar #'cdr (subseq scores 0 (min (length scores) *max-portfolio-size*)))))
      (unless (equal new-p *portfolio-indices*)
        (format t "~%[L] 🐟 ~a~%" new-p)
        (notify-discord (format nil "~a" new-p) :color 16766720)
        (setf *portfolio-indices* new-p)
        (save-genome)))))

;; Get Sharpe boost for an arm index based on evolved strategies
(defun get-sharpe-boost (arm-idx)
  "Return bonus score based on matching evolved strategy Sharpe"
  (if (and *evolved-strategies* (> (length *evolved-strategies*) 0))
      (let* ((top-strat (first *evolved-strategies*))  ; Already sorted by Sharpe
             (top-sharpe (strategy-sharpe top-strat)))
        ;; Only boost if Sharpe is positive
        (if (and top-sharpe (> top-sharpe 0))
            (* 0.1 (min top-sharpe 3.0))  ; Max 0.3 boost for Sharpe >= 3
            0.0))
      0.0))
(defun update-arm-stats (idx won pnl)
  (incf *total-trades*)
  (incf *daily-pnl* pnl)
  (let* ((arm (nth idx *arms*)) (stats (cdr arm))
         (a (ensure-real (car stats))) (b (ensure-real (cdr stats)))
         (state (gethash idx *arm-states*)))
    (if won (progn (incf a) (setf (arm-state-streak state) 0))
        (progn (incf b) (decf (arm-state-streak state))))
    (setf (nth idx *arms*) (cons (car arm) (cons a b)))
    (save-genome)
    ;; Risk management: update drawdown and adaptive threshold
    (update-drawdown pnl)
    (update-nn-threshold won)
    ;; Train neural network with trade result
    (let ((pos (arm-state-position state)))
      (cond
        ((and won (eql pos :LONG)) (train-neural 0))   ; Correct BUY
        ((and won (eql pos :SHORT)) (train-neural 1))  ; Correct SELL
        ((and (not won) (eql pos :LONG)) (train-neural 1))  ; Wrong BUY -> should be SELL
        ((and (not won) (eql pos :SHORT)) (train-neural 0)))) ; Wrong SELL -> should be BUY
    (when (member idx *portfolio-indices*)
      (format t "[L] [~a] ~a ~d: ~,2f | DD:~,1f%~%" (get-jst-str) (if won "✅" "❌") idx pnl *max-drawdown*))
    (when (<= (arm-state-streak state) (- *max-streak-losses*)) (bench-arm idx))
    (rebalance-portfolio)))
(defun check-arm (idx symbol bid ask)
  (when (and (trading-allowed-p) (not (arm-benched-p idx)))
    (let* ((params (car (nth idx *arms*)))
           (sma-s (nth 0 params)) (sma-l (nth 1 params))
           (sl-p (nth 2 params)) (tp-p (nth 3 params)) (vol (nth 4 params))
           (state (or (gethash idx *arm-states*) (setf (gethash idx *arm-states*) (make-arm-state :streak 0))))
           (active (member idx *portfolio-indices*))
           (s-now (calculate-sma sma-s *candle-history*))
           (l-now (calculate-sma sma-l *candle-history*))
           (s-prev (calculate-sma sma-s (cdr *candle-history*)))
           (l-prev (calculate-sma sma-l (cdr *candle-history*))))
      (when (and s-now l-now s-prev l-prev)
        (let ((pos (arm-state-position state)) (entry (arm-state-entry-price state)))
          (when pos
            (let ((pnl 0) (closed nil))
              (cond
                ((eql pos :LONG)
                 (when (or (<= bid (arm-state-sl state)) (>= bid (arm-state-tp state)) (and (> s-prev l-prev) (< s-now l-now)))
                   (setf pnl (- bid entry) closed t)))
                ((eql pos :SHORT)
                 (when (or (>= ask (arm-state-sl state)) (<= ask (arm-state-tp state)) (and (< s-prev l-prev) (> s-now l-now)))
                   (setf pnl (- entry ask) closed t))))
              (when closed
                (update-arm-stats idx (> pnl 0) pnl)
                (setf (arm-state-position state) nil)
                (when active
                  (pzmq:send *cmd-publisher* (jsown:to-json (jsown:new-js ("action" "CLOSE") ("symbol" symbol))))
                  (notify-discord (format nil "~a ~,2f #~d | Day:~,0f" (if (> pnl 0) "✅" "❌") pnl idx *daily-pnl*) :color (if (> pnl 0) 3066993 15158332)))))))
        (unless (arm-state-position state)
          ;; NN-enhanced entry: warmup period trades without NN check to collect data
          (let* ((warmup-p (< *total-trades* 50))  ; First 50 trades = warmup
                 (conf (if (and (boundp '*last-confidence*) (numberp *last-confidence*)) *last-confidence* 0.0))
                 (pred (if (and (boundp '*last-prediction*) *last-prediction*) *last-prediction* "HOLD"))
                 (nn-ok-buy (or warmup-p (and (string= pred "BUY") (> conf *nn-threshold*))))
                 (nn-ok-sell (or warmup-p (and (string= pred "SELL") (> conf *nn-threshold*)))))
            (cond
              ;; BUY: SMA crossover UP + (warmup OR NN agrees)
              ((and (< s-prev l-prev) (> s-now l-now) nn-ok-buy)
               (setf (arm-state-position state) :LONG (arm-state-entry-price state) bid
                     (arm-state-sl state) (- bid sl-p) (arm-state-tp state) (+ bid tp-p))
               (when active
                 (pzmq:send *cmd-publisher* (jsown:to-json (jsown:new-js ("action" "BUY") ("symbol" symbol) ("volume" vol) ("sl" (arm-state-sl state)) ("tp" (arm-state-tp state)))))
                 (notify-discord (format nil "~a BUY #~d~a" (if warmup-p "🔥" "🧠") idx (if warmup-p " [WARMUP]" (format nil " (~,0f%)" (* conf 100)))) :color 3066993)))
              ;; SELL: SMA crossover DOWN + (warmup OR NN agrees)
              ((and (> s-prev l-prev) (< s-now l-now) nn-ok-sell)
               (setf (arm-state-position state) :SHORT (arm-state-entry-price state) ask
                     (arm-state-sl state) (+ ask sl-p) (arm-state-tp state) (- ask tp-p))
               (when active
                 (pzmq:send *cmd-publisher* (jsown:to-json (jsown:new-js ("action" "SELL") ("symbol" symbol) ("volume" vol) ("sl" (arm-state-sl state)) ("tp" (arm-state-tp state)))))
                 (notify-discord (format nil "~a SELL #~d~a" (if warmup-p "🔥" "🧠") idx (if warmup-p " [WARMUP]" (format nil " (~,0f%)" (* conf 100)))) :color 15158332)))
              ;; SMA signal but NN disagrees - skip (log for debugging)
              ((and (< s-prev l-prev) (> s-now l-now) (not nn-ok-buy) active)
               (format t "[L] ⏸️ Skip BUY #~d (NN:~a ~,0f%)~%" idx pred (* conf 100)))
              ((and (> s-prev l-prev) (< s-now l-now) (not nn-ok-sell) active)
               (format t "[L] ⏸️ Skip SELL #~d (NN:~a ~,0f%)~%" idx pred (* conf 100))))))))))
;; Counter for alternating between dream-code and evolve
(defparameter *dream-cycle* 0)

;; Flag for initial batch backtest
(defparameter *initial-backtest-done* nil)

;;; ==========================================
;;; MAIN LOOP & TICK HANDLING
;;; ==========================================
;;; Extracted to src/lisp/core/tick-handler.lisp
;;; and src/lisp/system/runner.lisp
(load "src/lisp/core/tick-handler.lisp")
(load "src/lisp/system/runner.lisp")


;; Adaptive threshold based on win rate
(defparameter *nn-wins* 0)
(defparameter *nn-losses* 0)

;;; ==========================================
;;; META-LEARNING & HIGH COUNCIL
;;; ==========================================
;;; Extracted to src/lisp/core/meta-learning.lisp
;;; and src/lisp/core/governance.lisp


;; Remove Duplicate Start-Brain - Handover to Runner
(start-brain)
