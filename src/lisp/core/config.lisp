(in-package :swimmy.core)

;; core/config.lisp - Extracted Configuration (Naval's Strangler Fig)
;; V41.4: First step of brain.lisp refactoring

;;; ==========================================
;;; SYSTEM CONFIGURATION
;;; ==========================================

;; Supported trading symbols
(defparameter *supported-symbols* '("USDJPY" "EURUSD" "GBPUSD")
  "Active trading symbols for multi-currency support")

;;; ==========================================
;;; PATH RESOLUTION
;;; ==========================================

(defun resolve-swimmy-home ()
  "Resolve Swimmy project root (env override supported)."
  (let ((env (uiop:getenv "SWIMMY_HOME")))
    (uiop:ensure-directory-pathname
     (if (and env (> (length env) 0))
         env
         (uiop:getcwd)))))

(defparameter *swimmy-home* (resolve-swimmy-home)
  "Project root directory for resolving runtime paths.")

(defun swimmy-path (relative)
  "Build an absolute path from project root."
  (merge-pathnames relative *swimmy-home*))

;;; ==========================================
;;; DISCORD WEBHOOKS (Environment Variables)
;;; ==========================================

;; Note: Access via environment variables is now the standard (2026-01-10)

(defun strip-quotes (str)
  "環境変数から二重引用符を削除 (防御的プログラミング)"
  (if (stringp str)
      (string-trim '(#\" #\Space #\Tab #\Newline #\Return) str)
      str))

(defparameter *dotenv-cache* nil
  "Cached .env key/value pairs for fallback env loading.")

(defun load-dotenv (&optional (path (merge-pathnames ".env" *swimmy-home*)))
  "Load .env file into a hash-table (one-time cache)."
  (unless *dotenv-cache*
    (setf *dotenv-cache* (make-hash-table :test 'equal)))
  (when (probe-file path)
    (with-open-file (in path :direction :input)
      (loop for line = (read-line in nil nil)
            while line do
              (let* ((s (string-trim '(#\Space #\Tab) line))
                     (s (if (uiop:string-prefix-p "export " s)
                            (string-trim '(#\Space #\Tab) (subseq s 7))
                            s)))
                (when (and (> (length s) 0)
                           (not (char= (aref s 0) #\#)))
                  (let ((pos (position #\= s)))
                    (when pos
                      (let ((key (string-trim '(#\Space #\Tab) (subseq s 0 pos)))
                            (val (string-trim '(#\Space #\Tab) (subseq s (1+ pos)))))
                        (when (> (length key) 0)
                          (setf (gethash key *dotenv-cache*) (strip-quotes val)))))))))))
  *dotenv-cache*)

(defun getenv-or-dotenv (key)
  "Get environment variable from process env or .env fallback."
  (or (uiop:getenv key)
      (let* ((cache (or *dotenv-cache* (load-dotenv)))
             (val (and cache (gethash key cache))))
        val)))

(defun get-discord-webhook (key)
  "Webhook URLを取得 (Consolidated 2026-01-10)
   Maps logical keys to consolidated environmental variables.
   
   - LIVE_FEED: usdjpy, eurusd, gbpusd
   - SYSTEM_LOGS: status, backtest, recruit, fallback
   - REPORTS: daily, weekly, journal
   - ALERTS: emergency, apex, alerts"
  (let ((k (string-downcase (string key))))
    (strip-quotes
     (cond
       ;; [MICRO] Live Feed
       ((member k '("usdjpy" "eurusd" "gbpusd") :test #'equal)
        (getenv-or-dotenv "SWIMMY_DISCORD_LIVE_FEED"))
       
       ;; [MESO] System Logs
       ((member k '("status" "recruit" "fallback" "apex") :test #'equal)
        (getenv-or-dotenv "SWIMMY_DISCORD_SYSTEM_LOGS"))
       
       ;; [MACRO] Reports
       ((member k '("daily" "weekly" "journal" "backtest") :test #'equal)
        (getenv-or-dotenv "SWIMMY_DISCORD_REPORTS"))
       
       ;; [URGENT] Alerts
       ((member k '("emergency" "alerts" "heartbeat") :test #'equal)
        (getenv-or-dotenv "SWIMMY_DISCORD_ALERTS"))
        
       ;; Default to Alerts if unknown
       (t (getenv-or-dotenv "SWIMMY_DISCORD_ALERTS"))))))

;; フォールバックなしでシンプルに取得
;; (Legacy variables initialization below remains valid)


;; 後方互換性のため既存変数も設定
(defparameter *discord-webhook-url* (get-discord-webhook "usdjpy"))
(defparameter *discord-emergency-url* (get-discord-webhook "emergency"))
(defparameter *discord-emergency-webhook* *discord-emergency-url*)
(defparameter *discord-daily-webhook* (get-discord-webhook "daily"))
(defparameter *discord-weekly-webhook* (get-discord-webhook "weekly"))
(defparameter *backtest-webhook-url* (get-discord-webhook "backtest"))
(defparameter *status-webhook-url* (get-discord-webhook "recruit"))
(defparameter *discord-recruit-webhook* *status-webhook-url*) ; Alias for compatibility
(defparameter *alerts-webhook-url* (get-discord-webhook "alerts"))
(defparameter *apex-webhook-url* (get-discord-webhook "apex"))
(defparameter *discord-fallback-webhook* (get-discord-webhook "fallback")) ; Routing failures
(defparameter *heartbeat-webhook-url* (get-discord-webhook "heartbeat"))

;; Symbol-specific webhooks
(defparameter *symbol-webhooks* (make-hash-table :test 'equal))
(setf (gethash "USDJPY" *symbol-webhooks*) (get-discord-webhook "usdjpy"))
(setf (gethash "EURUSD" *symbol-webhooks*) (get-discord-webhook "eurusd"))
(setf (gethash "GBPUSD" *symbol-webhooks*) (get-discord-webhook "gbpusd"))

;;; ==========================================
;;; STATE & BUFFERS
;;; ==========================================

;; Candle State
(defvar *candle-histories* (make-hash-table :test 'equal))  ; symbol -> history
(defvar *current-candles* (make-hash-table :test 'equal))   ; symbol -> candle
(defvar *current-minutes* (make-hash-table :test 'equal))   ; symbol -> minute

;; Legacy single-currency compatibility
(defvar *candle-history* nil)
(defvar *current-candle* nil)
(defvar *current-minute* -1)

;; Status Reporting State
(defvar *last-status-notification-time* (make-hash-table :test 'equal))
(defparameter *status-notification-interval* 3600) ; Default 1 hour
(defvar *tribe-status* (make-hash-table :test 'eq))

;;; ==========================================
;;; GLOBAL PLACEHOLDERS
;;; ==========================================
(defvar *tribal-dialect* (make-hash-table :test 'equal))
(defvar *reputation-scores* (make-hash-table :test 'equal))
(defvar *genome* nil)
(defvar *arms* nil)
(defvar *memory* nil)
(defvar *portfolio-indices* nil)
(defvar *arm-states* (make-hash-table))

;;; ==========================================
;;; API KEYS
;;; ==========================================

(defparameter *gemini-api-key* (getenv-or-dotenv "SWIMMY_GEMINI_API_KEY")
  "Gemini AI API key for strategy generation")

;;; ==========================================
;;; RISK MANAGEMENT
;;; ==========================================

(defparameter *base-lot-size* 0.01
  "Default lot size for trades")

(defparameter *max-dd-percent* 20
  "Maximum drawdown percentage before warning")


(defparameter *daily-loss-limit* -5000
  "Daily loss limit in yen (Updated by goals/risk)")

(defparameter *max-streak-losses* 3
  "Max consecutive losses before caution")

(defparameter *max-portfolio-size* 3
  "Maximum concurrent positions")

;; Equity tracking - Uses defvar to preserve state on hot-reload
;; Values will be updated by MT5 ACCOUNT_INFO sync when available
(defvar *peak-equity* 1000000.0 "Peak equity for drawdown calculation (MT5 synced)")
(defvar *max-drawdown* 0.0 "Maximum drawdown percentage observed")
(defvar *current-drawdown* 0.0 "Current drawdown percentage")
(defvar *current-equity* 1000000.0 "Current account equity (MT5 synced)")

;;; ==========================================
;;; TRADING PARAMETERS
;;; ==========================================

(defparameter *monthly-goal* 100000
  "Monthly profit target in yen")

(defparameter *trading-days-in-month* 22
  "Approximate trading days per month")

(defparameter *risk-tolerance* :moderate
  "Risk level: :conservative, :moderate, :aggressive")

;; *daily-risk-limit* removed - use *daily-loss-limit* instead
(defvar *daily-pnl* 0.0)
(defvar *total-trades* 0)
(defvar *benched-arms* nil)

(defvar *last-guardian-heartbeat* 0)
(defvar *all-time-win-rate* 50.0)
(defvar *portfolio-sharpe* 0.0)

;;; ==========================================
;;; VOLATILITY & REGIME (Soros)
;;; ==========================================
(defvar *symbol-volatility-states* (make-hash-table :test 'equal))
(defvar *current-volatility-state* :normal)
(defvar *market-regime* :ranging)

;;; ==========================================
;;; EXECUTION & INFRASTRUCTURE
;;; ==========================================
(defparameter *last-heartbeat-sent* 0)

;;; ==========================================
;;; EVOLUTION & LEARNING STATE
;;; ==========================================
(defvar *dream-cycle* 0)
(defvar *initial-backtest-done* nil)
(defvar *last-narrative-day* -1)

;;; ==========================================
;;; TRADE TRACKING STATE
;;; ==========================================
(defvar *accumulated-pnl* 0.0 "Cumulative profit/loss")
(defvar *consecutive-wins* 0 "Current winning streak")
(defvar *consecutive-losses* 0 "Current losing streak")
(defvar *success-count* 0 "Total successful trades")
(defvar *tribe-direction* :hold "Current tribe consensus direction")
(defvar *tribe-consensus* 0.0 "Tribe agreement level 0-1")
(defvar *danger-level* 0 "Current danger level 0-5")
(defvar *last-swarm-consensus* nil "Last swarm voting result")


;; School State
(defvar *trade-history* (make-hash-table :test 'equal))
(defvar *strategy-ranks* (make-hash-table :test 'equal))
(defvar *category-positions* (make-hash-table :test 'eq))
(defvar *pair-correlations* (make-hash-table :test 'equal))
(defvar *symbol-exposure* (make-hash-table :test 'equal))
(defparameter *max-symbol-exposure* 0.30)
(defparameter *max-total-exposure* 0.60)
(defvar *current-leader* nil)

;; Risk State
(defparameter *resignation-threshold* -2000.0)
(defparameter *danger-cooldown-until* 0)
(defparameter *has-resigned-today* nil)

;;; ==========================================
;;; NEURAL NETWORK STATE
;;; ==========================================

(defparameter *last-prediction* "HOLD")
(defparameter *last-confidence* 0.0)
(defparameter *nn-threshold* 0.6)

;;; ==========================================
;;; GOVERNANCE & HIGH COUNCIL
;;; ==========================================
(defparameter *council-log* nil)
(defparameter *council-decision-threshold* 0.70)
(defparameter *notify-chieftain-threshold* :critical)

;;; ==========================================
;;; CONSTITUTION
;;; ==========================================
(defparameter *constitution* nil)
(defparameter *constitution-version* "1.0")

;;; ==========================================
;;; PHILOSOPHY LOGGER
;;; ==========================================
(defparameter *philosophy-log* nil)
(defparameter *philosophy-log-max* 500)
(defparameter *philosophy-log-path* (swimmy-path ".opus/philosophy_log.md"))



;;; ==========================================
;;; TIMING
;;; ==========================================

(defparameter *dream-interval* 300
  "Seconds between Gemini dreaming cycles")
(defparameter *last-dream-time* 0)

;;; ==========================================
;;; ROUND-ROBIN TICK OPTIMIZATION
;;; ==========================================

(defparameter *symbol-round-robin-index* 0
  "V41.4: Index for round-robin symbol processing")


(format t "[L] ⚙️ core/config.lisp loaded - System configuration~%")
