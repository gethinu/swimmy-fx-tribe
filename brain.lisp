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

(load (merge-pathnames "src/lisp/dsl.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/dreamer2.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/strategies.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/school.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/mixseek.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/research.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/llm-integration.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/evolution.lisp" *load-truename*))

;; Quality modules (Ver 38.1 EXCELLENCE)
(load (merge-pathnames "src/lisp/error-handling.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/quality.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/repl.lisp" *load-truename*))
(load (merge-pathnames "src/lisp/tests.lisp" *load-truename*))

(defun get-jst-str (&optional (ut (get-universal-time)))
  "Return current time as JST string [YYYY-MM-DD HH:MM:SS]"
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time ut -9)
    (format nil "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" year month day hour min sec)))

(defstruct arm-state position entry-price sl tp streak)
;; Multi-currency support
;; Supported symbols (BTC/ETH disabled until crypto broker setup)
(defparameter *supported-symbols* '("USDJPY" "EURUSD" "GBPUSD"))
;; TODO: Re-enable after IC Markets/Vantage setup
;; (defparameter *supported-symbols* '("USDJPY" "EURUSD" "GBPUSD" "BTCUSD" "ETHUSD"))
(defparameter *candle-histories* (make-hash-table :test 'equal))  ; symbol -> history
(defparameter *current-candles* (make-hash-table :test 'equal))   ; symbol -> candle
(defparameter *current-minutes* (make-hash-table :test 'equal))   ; symbol -> minute
;; Legacy single-currency compatibility
(defparameter *candle-history* nil)
(defparameter *current-candle* nil)
(defparameter *current-minute* -1)
(defparameter *cmd-publisher* nil)

;; Discord Emergency URL (for High Council notifications)
(defparameter *discord-emergency-url* (uiop:getenv "SWIMMY_DISCORD_EMERGENCY"))

;; Tribal Dialect forward declaration
(defparameter *tribal-dialect* (make-hash-table :test 'equal))

;; NOTE: clan struct and *clans* are defined in school.lisp (loaded earlier)
;; The 4 tribes strategy functions are in school.lisp: get-hunter-signal, get-shaman-signal, etc.

;; Reputation System forward declaration
(defparameter *reputation-scores* (make-hash-table :test 'equal))
(defparameter *genome-path* (merge-pathnames "genome.lisp" *load-truename*))
(defparameter *genome* nil)
(defparameter *arms* nil)
(defparameter *memory* nil)
(defparameter *portfolio-indices* nil)
(defparameter *arm-states* (make-hash-table))
(defparameter *discord-webhook-url* (uiop:getenv "SWIMMY_DISCORD_WEBHOOK"))
(defparameter *gemini-api-key* (uiop:getenv "SWIMMY_GEMINI_API_KEY"))
(defparameter *last-dream-time* 0)
(defparameter *dream-interval* 300)
(defparameter *max-portfolio-size* 3)
(defparameter *daily-pnl* 0.0)
(defparameter *daily-loss-limit* -500)
(defparameter *max-streak-losses* 3)
(defparameter *total-trades* 0)
(defparameter *benched-arms* nil)

;; Tribe Status for Discord (updated each tick)
(defparameter *tribe-status* (make-hash-table :test 'eq))  ; :hunters, :shamans, :breakers, :raiders

;; Risk Management
(defparameter *peak-equity* 0.0)
(defparameter *max-drawdown* 0.0)
(defparameter *current-equity* 0.0)
(defparameter *base-lot-size* 0.01)
(defparameter *max-dd-percent* 20)  ; Max 20% drawdown warning

;;; ==========================================
;;; V5.1: STRATEGY BENCH SYSTEM (専門家合意)
;;; ==========================================
;;; Simpler than Volume adjustment: Bench losing strategies, keep winning ones unchanged

;; Configuration
(defparameter *bench-min-trades* 50)           ; Statistical significance threshold
(defparameter *benched-strategies* (make-hash-table :test 'equal))  ; Name -> bench-time
(defparameter *bench-log* nil)                 ; Bench history
(defparameter *last-weekly-unbench* 0)         ; Weekly unbench for re-evaluation

(defun strategy-benched-p (name)
  "Check if a strategy is currently benched"
  (gethash name *benched-strategies*))

(defun bench-strategy (name reason sharpe win-rate trades)
  "Bench a losing strategy - remove from active trading"
  (setf (gethash name *benched-strategies*) (get-universal-time))
  (push (list :timestamp (get-universal-time)
              :strategy name
              :reason reason
              :sharpe sharpe
              :win-rate win-rate
              :trades trades)
        *bench-log*)
  (format t "[L] 🪑 BENCHED: ~a (~a) | Sharpe=~,2f Win=~,1f%~%" name reason sharpe win-rate))

(defun unbench-strategy (name)
  "Unbench a strategy for re-evaluation"
  (when (gethash name *benched-strategies*)
    (remhash name *benched-strategies*)
    (format t "[L] ▶️ UNBENCHED: ~a (weekly reset)~%" name)))

(defun weekly-unbench-all ()
  "Weekly: Unbench all strategies for re-evaluation"
  (format t "[L] 📅 Weekly unbench - all strategies get fresh start~%")
  (clrhash *benched-strategies*)
  (setf *last-weekly-unbench* (get-universal-time)))

(defun should-weekly-unbench-p ()
  "Check if weekly unbench is due"
  (> (- (get-universal-time) *last-weekly-unbench*) (* 7 86400)))

(defun evaluate-strategy-performance (strat sharpe trades win-rate)
  "Evaluate strategy and bench if performing poorly"
  (when (and strat (> trades *bench-min-trades*))
    (let ((name (strategy-name strat)))
      ;; Poor performance: bench the strategy
      (when (and (or (< sharpe 0) (< win-rate 40))
                 (not (strategy-benched-p name)))
        (bench-strategy name "Poor performance" sharpe win-rate trades)))))

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

(defun get-volatility-state ()
  "Return current volatility state (wrapper for school.lisp variable)"
  (if (boundp '*current-volatility-state*)
      *current-volatility-state*
      :normal))

(defun current-trading-session ()
  "Return current trading session based on hour (UTC+9)"
  (let ((hour (nth-value 2 (decode-universal-time (get-universal-time)))))
    (cond
      ((and (>= hour 6) (< hour 15)) :tokyo)
      ((and (>= hour 15) (< hour 22)) :london)
      (t :newyork))))

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

;; V5.0: Backtest results to separate silent channel
(defparameter *backtest-webhook-url* "https://discord.com/api/webhooks/1455351646962979000/p9cWLthwfP8gB1TgvukeJixren_kgJvjjIq-oVQ-doAsX_C4chGBQyf05Eh_iDmLu1Dy")

(defun notify-discord-backtest (msg &key (color 3447003))
  "Send backtest results to separate channel without notification"
  (when (and *backtest-webhook-url* msg)
    (handler-case
        (dex:post *backtest-webhook-url*
                  :content (jsown:to-json (jsown:new-js 
                              ("embeds" (list (jsown:new-js ("description" (format nil "~a" msg)) ("color" color))))
                              ("flags" 4096)))  ;; SUPPRESS_NOTIFICATIONS
                  :headers '(("Content-Type" . "application/json")) :read-timeout 3)
      (error (e) nil))))

;; Multi-currency Discord webhooks (symbol -> webhook URL)
(defparameter *symbol-webhooks* (make-hash-table :test 'equal))

(defun setup-symbol-webhooks ()
  "Setup Discord webhooks for each currency pair"
  ;; V5.1: Direct webhook URLs per symbol
  (setf (gethash "USDJPY" *symbol-webhooks*) 
        "https://discord.com/api/webhooks/1455548858921652442/pxCwnTnnMVd8-X8LIO3NrwtxQ0T2dm31GiS-SaHAkqQ0AAR5G5ABVcfKKJ0awKnGhnLk")
  (setf (gethash "EURUSD" *symbol-webhooks*) 
        "https://discord.com/api/webhooks/1455549049540313189/lw9iSajiYjzogZIUEuymaUaOIePL8yT0ya-qc8Utpyr5nM6bAZv6l8ekYTdf0knRRKZa")
  (setf (gethash "GBPUSD" *symbol-webhooks*) 
        "https://discord.com/api/webhooks/1455558971367882762/gOf_SFW0JvQd7tX1CqSGZbtGMcOz5wcwAiVgPhvCzEp7QAkl1g8u1dNx9qhfXbt5lAyB"))

;; V5.1: Status and Alerts webhooks
(defparameter *status-webhook-url* 
  "https://discord.com/api/webhooks/1413195529680191538/OLcthUXpQr6fM32o8Vx-zlEJfgDTXfq14RPPSJdEKBJJZUUVBWJ9Hwq7ZPNFOMDkmQSW")
(defparameter *alerts-webhook-url* 
  "https://discord.com/api/webhooks/1455549266301812849/r5Rv8rQrwgVsppGS0qIDJPNyz2KphVIzwshu6vTPABC-E6OSFrS89tZ9xAGQJEzmRqBH")

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

;; V5.3: Periodic Status Notification
(defparameter *last-status-notification-time* (make-hash-table :test 'equal))
(defparameter *status-notification-interval* 3600) ; Default 1 hour

(defun send-periodic-status-report (symbol bid)
  "Send a periodic status report to Discord if interval continues"
  (let* ((now (get-universal-time))
         (last-time (gethash symbol *last-status-notification-time* 0)))
    (when (> (- now last-time) *status-notification-interval*)
      (let ((tribe-dir (if (boundp '*tribe-direction*) *tribe-direction* "N/A"))
            (tribe-con (if (boundp '*tribe-consensus*) *tribe-consensus* 0.0))
            (swarm-con (if (boundp '*last-swarm-consensus*) *last-swarm-consensus* 0.0))
            (pred (if (boundp '*last-prediction*) *last-prediction* "N/A"))
            (conf (if (boundp '*last-confidence*) *last-confidence* 0.0))
            (danger (if (boundp '*danger-level*) *danger-level* 0))
            (active-warriors (if (boundp '*warrior-allocation*) 
                                 (hash-table-count *warrior-allocation*) 0)))
        (notify-discord-symbol symbol 
          (format nil "🕒 STATUS REPORT~%Price: ~,3f~%~%🧠 AI: ~a (~,1f%)~%🏛️ Tribes: ~a (~,0f%)~%🐟 Swarm: ~,0f%~%~%⚔️ Warriors: ~d~%⚠️ Danger: Lv~d"
                  bid pred (* 100 conf) tribe-dir (* 100 tribe-con) (* 100 swarm-con) active-warriors danger)
          :color 10070709) ; Dark Grey for status
        (setf (gethash symbol *last-status-notification-time*) now)))))

(defun notify-discord-alert (msg &key (color 15158332))
  "Send critical alerts to alerts channel (FLEE mode, danger, etc.)"
  (when (and *alerts-webhook-url* msg)
    (handler-case
        (dex:post *alerts-webhook-url*
                  :content (jsown:to-json (jsown:new-js ("embeds" (list (jsown:new-js 
                            ("title" "🚨 ALERT") 
                            ("description" (format nil "~a" msg)) 
                            ("color" color))))))
                  :headers '(("Content-Type" . "application/json")) :read-timeout 3)
      (error (e) nil))))

(defun notify-discord-status (msg &key (color 3066993))
  "Send status updates to status channel"
  (when (and *status-webhook-url* msg)
    (handler-case
        (dex:post *status-webhook-url*
                  :content (jsown:to-json (jsown:new-js ("embeds" (list (jsown:new-js 
                            ("title" "📊 Status") 
                            ("description" (format nil "~a" msg)) 
                            ("color" color))))))
                  :headers '(("Content-Type" . "application/json")) :read-timeout 3)
      (error (e) nil))))

;;; ==========================================
;;; MULTI-CHANNEL DISCORD (複数チャンネル対応)
;;; ==========================================
;;; Different channels for different purposes

(defparameter *discord-emergency-webhook* (uiop:getenv "SWIMMY_DISCORD_EMERGENCY"))
(defparameter *discord-daily-webhook* (uiop:getenv "SWIMMY_DISCORD_DAILY"))
(defparameter *discord-weekly-webhook* (uiop:getenv "SWIMMY_DISCORD_WEEKLY"))

(defun notify-discord-emergency (msg)
  "Send to EMERGENCY channel - use sparingly!"
  (let ((webhook (or *discord-emergency-webhook* *discord-webhook-url*)))
    (when webhook
      (handler-case
          (dex:post webhook
                    :content (jsown:to-json (jsown:new-js 
                              ("embeds" (list (jsown:new-js 
                                ("title" "🚨 EMERGENCY 🚨") 
                                ("description" (format nil "~a" msg)) 
                                ("color" 15158332))))))  ; Red
                    :headers '(("Content-Type" . "application/json")) :read-timeout 3)
        (error (e) nil)))))

(defun notify-discord-daily (msg &key (color 3447003))
  "Send to daily report channel"
  (let ((webhook (or *discord-daily-webhook* *discord-webhook-url*)))
    (when webhook
      (handler-case
          (dex:post webhook
                    :content (jsown:to-json (jsown:new-js 
                              ("embeds" (list (jsown:new-js 
                                ("title" "📊 Daily Report") 
                                ("description" (format nil "~a" msg)) 
                                ("color" color))))))  ; Use passed color
                    :headers '(("Content-Type" . "application/json")) :read-timeout 3)
        (error (e) nil)))))

(defun notify-discord-weekly (msg)
  "Send to weekly summary channel"
  (let ((webhook (or *discord-weekly-webhook* *discord-webhook-url*)))
    (when webhook
      (handler-case
          (dex:post webhook
                    :content (jsown:to-json (jsown:new-js 
                              ("embeds" (list (jsown:new-js 
                                ("title" "📈 Weekly Summary") 
                                ("description" (format nil "~a" msg)) 
                                ("color" 10181046))))))  ; Purple
                    :headers '(("Content-Type" . "application/json")) :read-timeout 3)
        (error (e) nil)))))

;;; ==========================================
;;; V5.0: NEURAL NETWORK TRAINING
;;; ==========================================

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

(defun request-walk-forward (strategy-name)
  "Request walk-forward validation for a strategy"
  (when (and *candle-history* (> (length *candle-history*) 500))
    (handler-case
        (let* ((strat (find strategy-name *strategy-knowledge-base* :key #'strategy-name :test #'string=))
               (candles-json (mapcar (lambda (c)
                                       (jsown:new-js
                                         ("open" (candle-open c))
                                         ("high" (candle-high c))
                                         ("low" (candle-low c))
                                         ("close" (candle-close c))
                                         ("volume" 1)))
                                     (subseq *candle-history* 0 (min 1000 (length *candle-history*)))))
               (cmd (jsown:to-json (jsown:new-js
                      ("action" "WALK_FORWARD")
                      ("strategy_name" strategy-name)
                      ("sma_short" 10)
                      ("sma_long" 30)
                      ("sl" 0.15)
                      ("tp" 0.40)
                      ("candles" candles-json)))))
          (pzmq:send *cmd-publisher* cmd)
          (format t "[L] 📊 Walk-forward requested for ~a~%" strategy-name))
      (error (e) nil))))

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
;;; EVALUATOR AI (Multi-Agent Debate)
;;; ==========================================
;;; A second "AI" that reviews proposals before execution
;;; Inspired by: 2025 Multi-Agent systems

(defparameter *evaluator-enabled* t)
(defparameter *debate-log* nil)

(defstruct debate-entry
  timestamp
  proposal-type  ; :trade, :parameter-change, :strategy-switch
  proposer       ; Who proposed (e.g., "Leader-Fish", "Swarm")
  proposal       ; The actual proposal
  evaluator-verdict  ; :approve, :reject, :modify
  evaluator-reason
  final-action)

(defun evaluator-check-trade (direction symbol confidence)
  "Evaluator AI reviews trade proposal"
  (when *evaluator-enabled*
    (let ((issues nil)
          (verdict :approve)
          (reason "All checks passed"))
      
      ;; Check 1: Volatility
      (when (eq *current-volatility-state* :extreme)
        (push "⚠️ EXTREME volatility - high risk" issues)
        (setf verdict :reject))
      
      ;; Check 2: Danger level
      (when (>= *danger-level* 3)
        (push "⚠️ High danger level - avoid new trades" issues)
        (setf verdict :reject))
      
      ;; Check 3: Already resigned
      (when (has-resigned-p)
        (push "⛔ Already resigned today" issues)
        (setf verdict :reject))
      
      ;; Check 4: Confidence too low
      (when (and confidence (< confidence 0.4))
        (push "⚠️ Low confidence prediction" issues)
        (setf verdict :modify))
      
      ;; Check 5: Against leader opinion
      (when (and *current-leader* 
                 (not (eq (get-leader-direction *candle-history*) direction)))
        (push "📢 Leader disagrees with this direction" issues))
      
      ;; Check 6: Daily loss already significant
      (when (< *daily-pnl* -2000)
        (push "⚠️ Daily loss already significant" issues)
        (setf verdict :modify))
      
      ;; Build reason
      (when issues
        (setf reason (format nil "~{~a~^; ~}" issues)))
      
      ;; Log debate
      (let ((entry (make-debate-entry
                    :timestamp (get-universal-time)
                    :proposal-type :trade
                    :proposer "Swarm"
                    :proposal (format nil "~a ~a (conf: ~,0f%)" direction symbol (* 100 (or confidence 0)))
                    :evaluator-verdict verdict
                    :evaluator-reason reason
                    :final-action nil)))
        (push entry *debate-log*)
        
        ;; Output debate result
        (format t "[L] 🔍 EVALUATOR REVIEW:~%")
        (format t "[L]    Proposal: ~a ~a~%" direction symbol)
        (format t "[L]    Verdict: ~a~%" verdict)
        (when issues
          (format t "[L]    Issues: ~{~a~^, ~}~%" issues))
        
        (list :verdict verdict :issues issues :entry entry)))))

(defun evaluator-check-parameter-change (param old-value new-value)
  "Evaluator reviews parameter change proposals"
  (when *evaluator-enabled*
    (let ((change-pct (if (and old-value (> (abs old-value) 0.001))
                          (* 100 (/ (abs (- new-value old-value)) (abs old-value)))
                          100))
          (verdict :approve)
          (issues nil))
      
      ;; Check for extreme changes
      (when (> change-pct 50)
        (push "⚠️ Change > 50% - too aggressive" issues)
        (setf verdict :modify))
      
      ;; Log
      (format t "[L] 🔍 EVALUATOR: ~a change ~,2f → ~,2f (~,0f%%) → ~a~%"
              param old-value new-value change-pct verdict)
      
      (list :verdict verdict :change-pct change-pct))))

(defun get-debate-summary ()
  "Get summary of recent debates"
  (let* ((recent (subseq *debate-log* 0 (min 10 (length *debate-log*))))
         (approved (count :approve recent :key #'debate-entry-evaluator-verdict))
         (rejected (count :reject recent :key #'debate-entry-evaluator-verdict)))
    (format nil "Recent debates: ~d approved, ~d rejected out of ~d"
            approved rejected (length recent))))


;;; ══════════════════════════════════════════════════════════════════
;;;  2027 VISION: CONSTITUTION LAYER (憲法レイヤー)
;;; ══════════════════════════════════════════════════════════════════
;;; Instead of specific instructions, humans give values (Constitution)
;;; AI makes decisions aligned with these values autonomously

(defstruct core-value
  name           ; Keyword like :capital-preservation
  priority       ; 1-10 (higher = more important)
  description    ; Human-readable description
  threshold      ; Violation threshold
  check-fn)      ; (lambda (context) -> score 0.0-1.0)

(defparameter *constitution* nil)
(defparameter *constitution-version* "1.0")

(defun initialize-constitution ()
  "Initialize Swimmy's Core Values - The fundamental principles that guide all decisions"
  (setf *constitution*
        (list
         ;; Priority 10: Capital Preservation (資本保全)
         (make-core-value
          :name :capital-preservation
          :priority 10
          :description "壊滅的損失を絶対回避。生き残ることが最優先。"
          :threshold 0.3
          :check-fn (lambda (ctx)
                      (let ((daily-loss (or (getf ctx :daily-pnl) 0))
                            (max-dd (or (getf ctx :max-drawdown) 0)))
                        (cond
                          ((< daily-loss -5000) 0.0)   ; Severe violation
                          ((< daily-loss -3000) 0.3)   ; High risk
                          ((< daily-loss -1000) 0.6)   ; Warning
                          ((> max-dd 20) 0.4)          ; High DD
                          (t 1.0)))))
         
         ;; Priority 9: Ethical Trading (倫理的取引)
         (make-core-value
          :name :ethical-trading
          :priority 9
          :description "市場操作に加担しない。グレーな取引は利益が出ても棄却。"
          :threshold 0.5
          :check-fn (lambda (ctx)
                      (let ((volatility (getf ctx :volatility-state)))
                        (if (eq volatility :extreme)
                            0.7  ; Caution during extreme volatility
                            1.0))))
         
         ;; Priority 8: Sustainability (持続可能性)
         (make-core-value
          :name :sustainability
          :priority 8
          :description "短期利益より長期生存。過度なリスクを取らない。"
          :threshold 0.4
          :check-fn (lambda (ctx)
                      (let ((danger (or (getf ctx :danger-level) 0))
                            (consecutive-losses (or (getf ctx :consecutive-losses) 0)))
                        (cond
                          ((>= danger 3) 0.2)
                          ((>= consecutive-losses 4) 0.3)
                          ((>= consecutive-losses 2) 0.6)
                          (t 1.0)))))
         
         ;; Priority 7: Continuous Learning (継続学習)
         (make-core-value
          :name :continuous-learning
          :priority 7
          :description "失敗から必ず学ぶ。同じ過ちを繰り返さない。"
          :threshold 0.5
          :check-fn (lambda (ctx)
                      (let ((similar-failure (getf ctx :similar-failure-count)))
                        (if (and similar-failure (> similar-failure 3))
                            0.4
                            1.0))))
         
         ;; Priority 6: Transparency (透明性)
         (make-core-value
          :name :transparency
          :priority 6
          :description "判断理由を常に記録。ブラックボックスにならない。"
          :threshold 0.6
          :check-fn (lambda (ctx)
                      ;; Always high score - this is about logging, not blocking
                      1.0))))
  
  (format t "[L] 📜 CONSTITUTION INITIALIZED (v~a)~%" *constitution-version*)
  (format t "[L] 📜 ~d Core Values loaded~%" (length *constitution*))
  *constitution*)

(defun evaluate-constitution (context)
  "Evaluate a decision against the Constitution. Returns overall alignment score."
  (unless *constitution*
    (initialize-constitution))
  
  (let ((total-score 0)
        (total-weight 0)
        (violations nil))
    
    (dolist (value *constitution*)
      (let* ((score (funcall (core-value-check-fn value) context))
             (priority (core-value-priority value))
             (weighted-score (* score priority)))
        
        (incf total-score weighted-score)
        (incf total-weight priority)
        
        ;; Track violations
        (when (< score (core-value-threshold value))
          (push (list :value (core-value-name value)
                     :score score
                     :priority priority
                     :description (core-value-description value))
                violations))))
    
    (let ((alignment (if (> total-weight 0) (/ total-score total-weight) 1.0)))
      (list :alignment alignment
            :violations violations
            :passed (null violations)))))

(defun constitution-allows-p (action context)
  "Check if Constitution allows this action. Returns T if allowed."
  (let* ((result (evaluate-constitution context))
         (alignment (getf result :alignment))
         (violations (getf result :violations)))
    
    (when violations
      (format t "[L] 📜 CONSTITUTION CHECK: ~a~%" action)
      (format t "[L] 📜 Alignment: ~,0f%~%" (* 100 alignment))
      (dolist (v violations)
        (format t "[L] 📜 ⚠️ Violation: ~a (~,0f%%) - ~a~%"
                (getf v :value) (* 100 (getf v :score)) (getf v :description))))
    
    (> alignment 0.5)))  ; Must be >50% aligned


;;; ══════════════════════════════════════════════════════════════════
;;;  2027 VISION: PHILOSOPHY LOGGER (哲学ロガー)
;;; ══════════════════════════════════════════════════════════════════
;;; Records not just WHAT happened, but WHY it happened
;;; The "Why" becomes the most important thing in 2028

(defparameter *philosophy-log* nil)
(defparameter *philosophy-log-max* 500)
(defparameter *philosophy-log-path* "/home/swimmy/swimmy/.opus/philosophy_log.md")

(defstruct philosophy-entry
  timestamp
  action-type    ; :trade, :skip, :resign, :parameter-change
  what           ; What happened
  why            ; Why it happened (auto-generated reasoning)
  constitution-alignment  ; How well aligned with values
  context-snapshot        ; Captured context at decision time
  outcome)                ; Result of the decision (filled later)

(defun generate-why (action-type context decision)
  "Generate the philosophical 'Why' for a decision"
  (let ((parts nil))
    
    ;; Market context
    (when (getf context :regime)
      (push (format nil "市場レジームは~aであった" (getf context :regime)) parts))
    
    ;; Danger awareness
    (when (and (getf context :danger-level) (> (getf context :danger-level) 0))
      (push (format nil "危険レベルは~dに達していた" (getf context :danger-level)) parts))
    
    ;; Goal progress
    (when (getf context :goal-progress)
      (let ((prog (getf context :goal-progress)))
        (if (< prog 50)
            (push "目標ペースを下回っていた" parts)
            (push "目標に向かって順調であった" parts))))
    
    ;; Leader opinion
    (when (getf context :leader-agrees)
      (if (getf context :leader-agrees)
          (push "リーダー戦略が同意していた" parts)
          (push "リーダー戦略は反対意見であった" parts)))
    
    ;; Constitution alignment
    (when (getf context :constitution-alignment)
      (let ((align (getf context :constitution-alignment)))
        (if (> align 0.8)
            (push "憲法との整合性が高かった" parts)
            (push (format nil "憲法との整合性は~,0f%%であった" (* 100 align)) parts))))
    
    ;; Action-specific
    (case action-type
      (:trade
       (push (format nil "~aの判断を下した" decision) parts))
      (:skip
       (push "トレードを見送った" parts))
      (:resign
       (push "本日のトレードを投了した" parts)))
    
    ;; Combine
    (format nil "~{~a。~}" (reverse parts))))

(defun log-philosophy (action-type what context &optional decision)
  "Log a philosophical entry - the Why behind the What"
  (let* ((constitution-result (evaluate-constitution context))
         (alignment (getf constitution-result :alignment))
         (why (generate-why action-type context decision))
         (entry (make-philosophy-entry
                 :timestamp (get-universal-time)
                 :action-type action-type
                 :what what
                 :why why
                 :constitution-alignment alignment
                 :context-snapshot context
                 :outcome nil)))
    
    (push entry *philosophy-log*)
    
    ;; Trim if too long
    (when (> (length *philosophy-log*) *philosophy-log-max*)
      (setf *philosophy-log* (subseq *philosophy-log* 0 *philosophy-log-max*)))
    
    ;; Output
    (format t "[L] 📖 PHILOSOPHY: ~a~%" what)
    (format t "[L] 📖 Why: ~a~%" why)
    
    entry))

(defun save-philosophy-log ()
  "Save philosophy log to file for human review"
  (handler-case
      (with-open-file (out *philosophy-log-path* :direction :output :if-exists :supersede)
        (format out "# 🔮 Swimmy Philosophy Log~%~%")
        (format out "「何をしたか」ではなく「なぜしたか」の記録~%~%")
        (format out "---~%~%")
        (dolist (entry (subseq *philosophy-log* 0 (min 50 (length *philosophy-log*))))
          (format out "## ~a~%~%" (philosophy-entry-what entry))
          (format out "**Why:** ~a~%~%" (philosophy-entry-why entry))
          (format out "**Alignment:** ~,0f%%~%~%" 
                  (* 100 (philosophy-entry-constitution-alignment entry)))
          (format out "---~%~%")))
    (error (e) nil)))


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


;; V4.0: Elder Lessons moved to brain-learning.lisp
;; V5.1: NN tracking variables (must be defined before loading brain-learning.lisp)
(defparameter *nn-wins* 0)
(defparameter *nn-losses* 0)
(load (merge-pathnames "brain-learning.lisp" *load-truename*))



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
;;; OPUS INTEGRATION: LOG ANALYSIS
;;; ==========================================
;;; Enables continuous AI partnership with automatic log analysis

(defparameter *log-analysis-history* nil)
(defparameter *knowledge-base-path* (merge-pathnames "knowledge_base.lisp" *load-truename*))

(defstruct log-analysis
  timestamp
  trading-days-analyzed
  total-pnl
  win-rate
  best-strategy
  worst-strategy
  key-findings
  improvement-suggestions)

(defun generate-log-analysis-report ()
  "Generate analysis report from recent performance data"
  (let* ((wins *consecutive-wins*)
         (losses *consecutive-losses*)
         (total (+ wins losses))
         (win-rate (if (> total 0) (* 100.0 (/ wins total)) 50.0))
         (progress (get-goal-progress))
         (findings nil)
         (suggestions nil))
    
    ;; Analyze patterns
    (when (< (getf progress :pace-pct) 80)
      (push "ペースが目標を下回っている" findings)
      (push "より積極的な戦略、またはリスク調整を検討" suggestions))
    
    (when (> *danger-level* 1)
      (push "危険レベルが高い" findings)
      (push "戦略の見直し、または一時停止を検討" suggestions))
    
    (when (eq *current-volatility-state* :elevated)
      (push "ボラティリティが高い状態が続いている" findings)
      (push "ポジションサイズの縮小を継続" suggestions))
    
    (when (and *current-leader* (< (leader-info-pnl-as-leader *current-leader*) 0))
      (push "現在のリーダー戦略がマイナス" findings)
      (push "リーダー交代の検討" suggestions))
    
    ;; Create analysis
    (let ((analysis (make-log-analysis
                     :timestamp (get-universal-time)
                     :trading-days-analyzed (getf progress :days-elapsed)
                     :total-pnl (getf progress :actual-pnl)
                     :win-rate win-rate
                     :best-strategy (if *current-leader* 
                                        (leader-info-strategy-name *current-leader*) 
                                        "N/A")
                     :worst-strategy "分析中"
                     :key-findings (or findings (list "特に問題なし"))
                     :improvement-suggestions (or suggestions (list "現状維持")))))
      
      (push analysis *log-analysis-history*)
      
      ;; Output report
      (format t "~%[L] ═══════════════════════════════════════════════════~%")
      (format t "[L] 📊 OPUS LOG ANALYSIS REPORT~%")
      (format t "[L] ═══════════════════════════════════════════════════~%")
      (format t "[L] 📅 Days analyzed: ~d~%" (log-analysis-trading-days-analyzed analysis))
      (format t "[L] 💰 Total PnL: ¥~:d~%" (round (log-analysis-total-pnl analysis)))
      (format t "[L] 🎯 Win rate: ~,1f%~%" (log-analysis-win-rate analysis))
      (format t "[L] 👑 Best strategy: ~a~%" (log-analysis-best-strategy analysis))
      (format t "[L]~%")
      (format t "[L] 🔍 Key Findings:~%")
      (dolist (f (log-analysis-key-findings analysis))
        (format t "[L]    • ~a~%" f))
      (format t "[L]~%")
      (format t "[L] 💡 Improvement Suggestions:~%")
      (dolist (s (log-analysis-improvement-suggestions analysis))
        (format t "[L]    → ~a~%" s))
      (format t "[L] ═══════════════════════════════════════════════════~%~%")
      
      analysis)))

;;; ==========================================
;;; OPUS INTEGRATION: SELF-IMPROVEMENT LOOP
;;; ==========================================
;;; Swimmy can request analysis from Opus/Gemini

(defparameter *improvement-requests* nil)

(defstruct improvement-request
  timestamp
  category      ; :strategy, :risk, :performance, :architecture
  description
  context
  status        ; :pending, :analyzed, :implemented
  response)

(defun request-opus-analysis (category description)
  "Generate a request for Opus analysis (stored for next Gemini call)"
  (let ((request (make-improvement-request
                  :timestamp (get-universal-time)
                  :category category
                  :description description
                  :context (list :pnl *daily-pnl*
                                :regime *current-regime*
                                :volatility *current-volatility-state*
                                :danger *danger-level*)
                  :status :pending
                  :response nil)))
    (push request *improvement-requests*)
    (format t "[L] 🤖 SELF-IMPROVEMENT: Request queued - ~a: ~a~%" category description)
    request))

(defun auto-request-improvements ()
  "Automatically generate improvement requests based on current state"
  ;; Performance issues
  (when (< *daily-pnl* (- (get-daily-target)))
    (request-opus-analysis :performance 
                           "日次損失が目標の倍以上。戦略の問題か市場適合の問題か分析が必要"))
  
  ;; Risk issues
  (when (>= *consecutive-losses* 4)
    (request-opus-analysis :risk 
                           "4連敗以上。リスク管理の強化または戦略変更を検討"))
  
  ;; Strategy issues
  (when (and *current-leader* 
             (> (leader-info-trades-as-leader *current-leader*) 10)
             (< (leader-info-pnl-as-leader *current-leader*) 0))
    (request-opus-analysis :strategy 
                           "10トレード以上でリーダー戦略がマイナス。戦略の有効性を再評価")))

(defun generate-gemini-prompt-for-improvements ()
  "Generate a prompt for Gemini API with pending improvement requests"
  (let ((pending (remove-if-not (lambda (r) (eq (improvement-request-status r) :pending))
                                *improvement-requests*)))
    (when pending
      (format nil "あなたはSwimmyトレーディングシステムの分析者です。~%~%現在の状況:~%- 日次PnL: ¥~:d~%- レジーム: ~a~%- ボラティリティ: ~a~%- 危険レベル: ~d~%~%以下の改善要求を分析してください:~%~{~%~a. [~a] ~a~}~%~%各項目について具体的な改善提案をしてください。"
              (round *daily-pnl*)
              *current-regime*
              *current-volatility-state*
              *danger-level*
              (let ((i 0))
                (mapcar (lambda (r)
                          (incf i)
                          (list i (improvement-request-category r) (improvement-request-description r)))
                        pending))))))

;;; ==========================================
;;; OPUS INTEGRATION: KNOWLEDGE BASE
;;; ==========================================
;;; Automatically record learned patterns

(defparameter *learned-patterns* nil)

(defstruct learned-pattern
  timestamp
  pattern-type   ; :win-condition, :loss-condition, :regime-behavior
  description
  frequency
  confidence)

(defun record-learned-pattern (pattern-type description confidence)
  "Record a learned pattern to knowledge base"
  (let ((existing (find description *learned-patterns* 
                        :key #'learned-pattern-description :test #'string=)))
    (if existing
        ;; Update existing
        (progn
          (incf (learned-pattern-frequency existing))
          (setf (learned-pattern-confidence existing) 
                (* 0.9 confidence (+ 0.1 (learned-pattern-confidence existing)))))
        ;; Add new
        (push (make-learned-pattern
               :timestamp (get-universal-time)
               :pattern-type pattern-type
               :description description
               :frequency 1
               :confidence confidence)
              *learned-patterns*)))
  (format t "[L] 📚 KNOWLEDGE: Recorded pattern - ~a~%" description))

(defun save-knowledge-base ()
  "Save knowledge base to file"
  (handler-case
      (with-open-file (out *knowledge-base-path* :direction :output :if-exists :supersede)
        (write (list :patterns *learned-patterns*
                     :analyses *log-analysis-history*
                     :improvements *improvement-requests*)
               :stream out :pretty t))
    (error (e) (format t "[L] ⚠️ Could not save knowledge base: ~a~%" e))))

(defun load-knowledge-base ()
  "Load knowledge base from file"
  (handler-case
      (with-open-file (in *knowledge-base-path* :direction :input :if-does-not-exist nil)
        (when in
          (let ((data (read in nil nil)))
            (setf *learned-patterns* (getf data :patterns))
            (setf *log-analysis-history* (getf data :analyses))
            (setf *improvement-requests* (getf data :improvements))
            (format t "[L] 📚 Knowledge base loaded: ~d patterns, ~d analyses~%"
                    (length *learned-patterns*) (length *log-analysis-history*)))))
    (error (e) nil)))

(defun opus-daily-session ()
  "Run daily Opus integration session - call this at end of trading day"
  (format t "~%[L] 🤖 ═══════════════════════════════════════════════════~%")
  (format t "[L] 🤖 OPUS DAILY SESSION~%")
  (format t "[L] 🤖 ═══════════════════════════════════════════════════~%~%")
  
  ;; 1. Generate log analysis
  (generate-log-analysis-report)
  
  ;; 2. Auto-generate improvement requests
  (auto-request-improvements)
  
  ;; 3. Record any obvious patterns
  (when (> *consecutive-wins* 3)
    (record-learned-pattern :win-condition 
                            (format nil "~a レジームで ~a 連勝" *current-regime* *consecutive-wins*)
                            0.7))
  
  ;; 4. Save knowledge base
  (save-knowledge-base)
  
  ;; 5. Generate Gemini prompt if needed
  (let ((prompt (generate-gemini-prompt-for-improvements)))
    (when prompt
      (format t "~%[L] 📋 GEMINI PROMPT FOR NEXT SESSION:~%")
      (format t "[L] ~a~%~%" (subseq prompt 0 (min 500 (length prompt))))))
  
  (format t "[L] 🤖 Session complete. Ready for Opus review.~%")
  (format t "[L] 🤖 ═══════════════════════════════════════════════════~%~%"))

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

(defun processing-step (symbol bid)
  (let ((now (get-universal-time)))
    ;; Request NN prediction every minute for trade decisions
    (request-prediction)
    ;; One-time batch backtest when enough data is available
    (when (and (not *initial-backtest-done*) 
               *candle-history* 
               (> (length *candle-history*) 1000))
      (setf *initial-backtest-done* t)
      (format t "~%[L] 🧪 Starting batch backtest of ~d strategies...~%" 
              (length *strategy-knowledge-base*))
      (batch-backtest-knowledge))
    (when (> (- now *last-dream-time*) *dream-interval*)
      (setf *last-dream-time* now)
      ;; Reassemble team based on current market regime
      (assemble-team)
      ;; ===== ECOSYSTEM MAINTENANCE =====
      ;; Check population health and diversity
      (let ((health (get-population-health)))
        (format t "[L] 🌿 ECOSYSTEM: Health=~,0f%~%" (* 100 health)))
      ;; Natural selection every 6 dream cycles (≈ every 30 minutes)
      (when (zerop (mod *dream-cycle* 6))
        (format t "[L] 🌱 Running natural selection...~%")
        (maintain-ecosystem-balance))
      ;; ===== INTER-TRIBAL ECONOMICS: Calculate mutual aid every 3 cycles =====
      (when (and (zerop (mod *dream-cycle* 3))
                 (fboundp 'calculate-mutual-aid))
        (calculate-mutual-aid))
      ;; ===== GOAL TRACKING: Report progress every 3 dream cycles =====
      (when (zerop (mod *dream-cycle* 3))
        (report-goal-status))
      ;; Regular evolution cycle (AlphaSwimmy)
      (check-evolution)
      ;; PURE GENETIC ALGORITHM (Gemini disabled - use /daily-review for strategy input)
      ;; Previously: alternated dream-code and evolve-population
      ;; Now: pure evolution + manual input via Opus chat
      (evolve-population)  ; Pure genetic algorithm
      ;; (when (zerop (mod *dream-cycle* 3)) (dream-code))  ; Gemini - disabled
      (incf *dream-cycle*))
    (when (numberp bid)  ; Safety check for bid
      (let ((ask (+ bid 0.0002))
            (history (gethash symbol *candle-histories*)))
        ;; V6.3 (Graham): TRIBES removed - was using stub functions returning 0%
        ;; Trade decisions now use SWARM consensus only (61 strategies)
        (when (and history (> (length history) 50))
          (format t "[L] 🏛️ SWARM-ONLY mode (TRIBES removed V6.3)~%"))
        ;; Category-based team trades (40/30/20/10 allocation)
        (process-category-trades symbol bid ask)))))
;; Global volatility tracking per symbol (Soros)
(defparameter *symbol-volatility-states* (make-hash-table :test 'equal))
(defparameter *current-volatility-state* :normal)
(defparameter *market-regime* :ranging)

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
(defun internal-process-msg (msg)
  (handler-case
      (let* ((json (jsown:parse msg)) (type (jsown:val json "type")))
        (cond
          ((string= type "TICK") 
           (update-candle (jsown:val json "bid") (jsown:val json "symbol"))
           (save-live-status)  ; Write status for Discord bot
           ;; V5.3: Periodic Status Report
           (send-periodic-status-report (jsown:val json "symbol") (jsown:val json "bid"))
           ;; V4.0: Continuous Learning Loop (evolution + dreamer)
           (handler-case (continuous-learning-step) (error (e) nil)))
          ;; V5.0: Guardian Heartbeat
          ((string= type "HEARTBEAT")
           (setf *last-guardian-heartbeat* (get-universal-time)))
          ((string= type "HISTORY")
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
                  (sharpe (jsown:val result "sharpe"))
                  (trades (jsown:val result "trades"))
                  (pnl (jsown:val result "pnl"))
                  (win-rate (handler-case (jsown:val result "win_rate") (error () 0))))
             (format t "[L] 📈 BACKTEST: ~a | Sharpe=~,2f | Trades=~d | PnL=~,2f | Win=~,1f%~%" 
                     name sharpe trades pnl win-rate)
              ;; Update strategy's sharpe score - check BOTH evolved AND knowledge base
              (let ((strat (or (find name *evolved-strategies* :key #'strategy-name :test #'string=)
                               (find name *strategy-knowledge-base* :key #'strategy-name :test #'string=))))
                (when strat
                  (setf (strategy-sharpe strat) sharpe)
                  ;; V5.1: BENCH SYSTEM (simpler than volume adjustment)
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
             (notify-discord-backtest (format nil "📊 ~a: Sharpe=~,2f, Trades=~d, Win=~,1f%" 
                                     name sharpe trades win-rate) :color 3447003)))
          ((string= type "CLONE_CHECK_RESULT")
           (let* ((result (jsown:val json "result"))
                  (is-clone (jsown:val result "is_clone"))
                  (similar (jsown:val result "most_similar"))
                  (sim (jsown:val result "similarity")))
             (if is-clone
                 (format t "[L] 🚫 CLONE rejected! ~,1f% similar to ~a~%" (* sim 100) similar)
                 (progn
                   (format t "[L] ✅ UNIQUE! Adding strategy (max sim: ~,1f%)~%" (* sim 100))
                   ;; Add pending strategy and run backtest
                   (when *pending-strategy*
                     (push *pending-strategy* *evolved-strategies*)
                     (when (and *candle-history* (> (length *candle-history*) 50))
                       (request-backtest *pending-strategy*))
                     (notify-discord (format nil "🧬 NEW UNIQUE: ~a" (strategy-name *pending-strategy*)) :color 65535)
                     (setf *pending-strategy* nil))))))
          ((or (string= type "PREDICTION_RESULT") (string= type "PREDICTION"))
           (handler-case
               (let* ((pred-data (if (jsown:keyp json "prediction") (jsown:val json "prediction") json))
                      (sig (if (jsown:keyp pred-data "signal") (jsown:val pred-data "signal") "HOLD"))
                      (conf (if (jsown:keyp pred-data "confidence") (jsown:val pred-data "confidence") 0.0)))
                 (setf *last-prediction* sig
                       *last-confidence* conf))
             (error (e) nil)))
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
                              (context (list :regime *current-regime*
                                            :volatility-state (get-volatility-state)
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
                       (notify-discord-symbol symbol msg :color 3066993))  ; Green to symbol channel
                     
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
                       (notify-discord-symbol symbol msg :color 15158332))))  ; Red to symbol channel
             (error (e) (format t "[L] Trade close error: ~a~%" e))))
          
          (t (format t "[L] Unknown msg type: ~a~%" type))))
    (error (e) (format t "[L] Err: ~a~%" e))))




;; V5.4: Daily Tribal Narrative
(defparameter *last-narrative-day* -1)

;; V5.6 (Research Paper #35): 3D/Virtual Flood Risk Metaphor
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

;; V5.5 (Ms. Hopper): Periodic Heartbeat to MT5
(defparameter *last-heartbeat-sent* 0)

(defun send-heartbeat ()
  (let ((now (get-internal-real-time)))
    (when (> (- now *last-heartbeat-sent*) (* 10 internal-time-units-per-second)) ; Every 10s
      (pzmq:send *cmd-publisher* (jsown:to-json (jsown:new-js ("action" "HEARTBEAT"))))
      (setf *last-heartbeat-sent* now))))

;; V5.3 (Ms. Hopper): Performance Monitoring Wrapper
(defun process-msg (msg)
  (let ((start-time (get-internal-real-time)))
    (internal-process-msg msg)
    (check-daily-narrative) ; V5.4: Check for new day
    (send-heartbeat)        ; V5.5: Heartbeat to MT5
    (let ((duration (/ (- (get-internal-real-time) start-time) internal-time-units-per-second)))
      (when (> duration 0.5) ; 500ms threshold
        (format t "[L] ⚠️ SLOW TICK: Processing took ~,3f seconds~%" duration)))))

;; Neural Network Integration
(defparameter *last-prediction* "HOLD")
(defparameter *last-confidence* 0.0)
(defparameter *nn-threshold* 0.6)

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

;; Note: *nn-wins* and *nn-losses* now defined before brain-learning.lisp load


;; V4.0: Meta-Learning moved to brain-learning.lisp


;;; ══════════════════════════════════════════════════════════════════
;;;  HIGH COUNCIL (御前会議)
;;; ══════════════════════════════════════════════════════════════════
;;; 参加者: Grand Chieftain (あなた), Shaman (Opus), 4 Clan Chiefs
;;; 重要な決定は大首長に通知

(defparameter *council-log* nil)
(defparameter *council-decision-threshold* 0.70)  ; 70% agreement needed
(defparameter *notify-chieftain-threshold* :critical)  ; :all, :important, :critical

(defstruct council-decision
  id
  proposal
  proposer         ; Which clan proposed
  votes            ; Plist of :clan → :approve/:reject/:abstain
  elder-advice     ; What elders said
  constitution-ok  ; Did it pass constitution check?
  final-decision   ; :approved, :rejected, :escalated
  chieftain-notified
  timestamp)

(defun convene-high-council (proposal proposer-clan &key (urgency :normal))
  "Convene the High Council for an important decision"
  (format t "~%[L] ═══════════════════════════════════════~%")
  (format t "[L] 🏛️ HIGH COUNCIL CONVENED~%")
  (format t "[L] ═══════════════════════════════════════~%")
  (format t "[L] 📜 Proposal: ~a~%" proposal)
  (format t "[L] 🎺 Proposed by: ~a~%" (get-clan-display proposer-clan))
  (format t "[L] ⚡ Urgency: ~a~%~%" urgency)
  
  (let ((decision (make-council-decision
                   :id (gensym "COUNCIL-")
                   :proposal proposal
                   :proposer proposer-clan
                   :votes nil
                   :elder-advice nil
                   :constitution-ok nil
                   :final-decision nil
                   :chieftain-notified nil
                   :timestamp (get-universal-time))))
    
    ;; Step 1: Gather clan votes
    (format t "[L] 📢 CLAN CHIEFS SPEAK:~%")
    (let ((votes (gather-clan-votes proposal proposer-clan)))
      (setf (council-decision-votes decision) votes))
    
    ;; Step 2: Consult elders
    (format t "~%[L] 👴 ELDERS COUNSEL:~%")
    (let* ((context (list :regime *current-regime*
                          :volatility-state *current-volatility-state*
                          :daily-pnl *daily-pnl*))
           (elder-vote (if *hall-of-fame* 
                           (elder-vote proposal context) 
                           :approve)))
      (setf (council-decision-elder-advice decision) elder-vote)
      (format t "[L]    Elders recommend: ~a~%" elder-vote))
    
    ;; Step 3: Constitutional check
    (format t "~%[L] 📜 CONSTITUTION CHECK:~%")
    (let ((const-ok (or (null *constitution*)
                        (constitution-allows-p :trade (list :daily-pnl *daily-pnl*)))))
      (setf (council-decision-constitution-ok decision) const-ok)
      (format t "[L]    Constitution: ~a~%" (if const-ok "✅ PERMITS" "❌ FORBIDS")))
    
    ;; Step 4: Final decision
    (let ((final (calculate-council-decision decision)))
      (setf (council-decision-final-decision decision) final)
      
      (format t "~%[L] ═══════════════════════════════════════~%")
      (format t "[L] 📋 COUNCIL DECISION: ~a~%" final)
      (format t "[L] ═══════════════════════════════════════~%~%")
      
      ;; Step 5: Notify chieftain if critical
      (when (or (eq urgency :critical)
                (eq final :escalated)
                (and (eq *notify-chieftain-threshold* :all)))
        (notify-chieftain decision))
      
      ;; Log decision
      (push decision *council-log*)
      
      final)))

(defun gather-clan-votes (proposal proposer)
  "Gather votes from each clan"
  (let ((votes nil))
    (dolist (clan-data *clans*)
      (let* ((clan-id (clan-id clan-data))
             (vote (simulate-clan-vote clan-id proposal proposer)))
        (push (cons clan-id vote) votes)
        (format t "[L]    ~a ~a: ~a~%"
                (clan-emoji clan-data) (clan-name clan-data) vote)))
    votes))

(defun simulate-clan-vote (clan-id proposal proposer)
  "Simulate a clan's vote based on their personality"
  (cond
    ;; Own proposal - always approve
    ((eq clan-id proposer) :approve)
    
    ;; Shamans are cautious
    ((and (eq clan-id :reversion) 
          (eq *current-volatility-state* :extreme))
     :reject)
    
    ;; Breakers are aggressive
    ((eq clan-id :breakout) :approve)
    
    ;; Raiders abstain from big decisions
    ((and (eq clan-id :scalp)
          (search "aggressive" (string-downcase (format nil "~a" proposal))))
     :abstain)
    
    ;; Default: follow proposer if same risk profile
    (t :approve)))

(defun calculate-council-decision (decision)
  "Calculate final decision from votes and advisors"
  (let* ((votes (council-decision-votes decision))
         (elder-advice (council-decision-elder-advice decision))
         (const-ok (council-decision-constitution-ok decision))
         (approve-count (count :approve votes :key #'cdr))
         (total-votes (length votes))
         (approval-rate (if (> total-votes 0) (/ approve-count total-votes) 0)))
    
    (cond
      ;; Constitution forbids - REJECT
      ((not const-ok) :rejected)
      
      ;; Elders reject - ESCALATE to chieftain
      ((eq elder-advice :reject) :escalated)
      
      ;; Strong approval - APPROVED
      ((>= approval-rate *council-decision-threshold*) :approved)
      
      ;; Weak approval with elder caution - ESCALATE
      ((and (>= approval-rate 0.5) (eq elder-advice :caution)) :escalated)
      
      ;; Otherwise - REJECTED
      (t :rejected))))

(defun notify-chieftain (decision)
  "Notify the Grand Chieftain (user) via Discord about critical decision"
  (setf (council-decision-chieftain-notified decision) t)
  
  (let ((msg (format nil "~%🏛️ **HIGH COUNCIL REQUIRES YOUR ATTENTION**~%~%~
                          📜 **Proposal:** ~a~%~
                          🎺 **Proposed by:** ~a~%~
                          📋 **Council Decision:** ~a~%~
                          👴 **Elder Advice:** ~a~%~
                          📜 **Constitution:** ~a"
                     (council-decision-proposal decision)
                     (get-clan-display (council-decision-proposer decision))
                     (council-decision-final-decision decision)
                     (council-decision-elder-advice decision)
                     (if (council-decision-constitution-ok decision) "Permits" "Forbids"))))
    
    ;; Send to emergency channel
    (when (and (boundp '*discord-emergency-url*) *discord-emergency-url*)
      (handler-case
          (dex:post *discord-emergency-url*
                    :content (jsown:to-json 
                              (jsown:new-js 
                               ("embeds" (list (jsown:new-js 
                                               ("title" "🏛️ High Council Decision")
                                               ("description" msg)
                                               ("color" 15844367))))))  ; Gold
                    :headers '(("Content-Type" . "application/json"))
                    :read-timeout 3)
        (error (e) nil)))
    
    (format t "[L] 📱 Grand Chieftain notified via Discord~%")))


;;; ══════════════════════════════════════════════════════════════════

;; V5.3: Initialize symbol webhooks for hourly reports
(setup-symbol-webhooks)

;; V4.0: Rituals moved to brain-ritual.lisp
(load (merge-pathnames "brain-ritual.lisp" *load-truename*))
