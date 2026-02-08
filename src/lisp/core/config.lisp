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

;;; ==========================================
;;; ZMQ PORTS & ENDPOINTS
;;; ==========================================

(defun parse-int-safe (val default)
  "Parse integer safely with a default fallback."
  (handler-case
      (if (and val (stringp val) (> (length val) 0))
          (parse-integer val)
          default)
    (error () default)))

(defun env-int-or (key default)
  "Read integer env var with a default fallback."
  (parse-int-safe (getenv-or-dotenv key) default))

(defun env-bool-or (key default)
  "Read boolean env var with a default fallback."
  (let ((val (getenv-or-dotenv key)))
    (if val
        (member (string-downcase val) '("1" "true" "yes" "on" "y" "t") :test #'string=)
        default)))

(defparameter *zmq-host* (or (getenv-or-dotenv "SWIMMY_ZMQ_HOST") "localhost")
  "Default ZMQ host for local connections.")

(defparameter *port-market* (env-int-or "SWIMMY_PORT_MARKET" 5557))
(defparameter *port-exec* (env-int-or "SWIMMY_PORT_EXEC" 5560))
(defparameter *port-sensory* (env-int-or "SWIMMY_PORT_SENSORY" 5555))
(defparameter *port-motor* (env-int-or "SWIMMY_PORT_MOTOR" 5556))
(defparameter *port-external* (env-int-or "SWIMMY_PORT_EXTERNAL" 5559))
(defparameter *port-data-keeper* (env-int-or "SWIMMY_PORT_DATA_KEEPER" 5561))
(defparameter *port-notifier* (env-int-or "SWIMMY_PORT_NOTIFIER" 5562))
(defparameter *port-backtest-req* (env-int-or "SWIMMY_PORT_BACKTEST_REQ" 5580))
(defparameter *port-backtest-res* (env-int-or "SWIMMY_PORT_BACKTEST_RES" 5581))

(defparameter *backtest-service-enabled* (env-bool-or "SWIMMY_BACKTEST_SERVICE" nil)
  "Enable dedicated backtest service (5580/5581) when true.")

(defparameter *backtest-max-pending* (env-int-or "SWIMMY_BACKTEST_MAX_PENDING" 500)
  "Max pending backtest requests before throttling.")
(defparameter *backtest-rate-limit-per-sec* (env-int-or "SWIMMY_BACKTEST_RATE_LIMIT" 5)
  "Max backtest sends per second.")

(when (boundp 'swimmy.globals::*backtest-max-pending*)
  (setf swimmy.globals::*backtest-max-pending* *backtest-max-pending*))
(when (boundp 'swimmy.globals::*backtest-rate-limit-per-sec*)
  (setf swimmy.globals::*backtest-rate-limit-per-sec* *backtest-rate-limit-per-sec*))

(defparameter *deferred-flush-batch* (env-int-or "SWIMMY_DEFERRED_FLUSH_BATCH" 0)
  "Max deferred backtests per flush. 0 means unlimited.")
(defparameter *deferred-flush-interval-sec* (env-int-or "SWIMMY_DEFERRED_FLUSH_INTERVAL_SEC" 0)
  "Min seconds between deferred flushes. 0 means no interval.")

(defparameter *backtest-csv-override* (getenv-or-dotenv "SWIMMY_BACKTEST_CSV_OVERRIDE")
  "Optional override path for backtest CSV.")

(defun zmq-connect-endpoint (port &optional (host *zmq-host*))
  "Build ZMQ connect endpoint."
  (format nil "tcp://~a:~d" host port))

(defun zmq-bind-endpoint (port)
  "Build ZMQ bind endpoint."
  (format nil "tcp://*:~d" port))

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
       ((member k '("emergency" "alerts") :test #'equal)
        (getenv-or-dotenv "SWIMMY_DISCORD_ALERTS"))

       ;; [HEARTBEAT] Dedicated channel (fallback to alerts)
       ((equal k "heartbeat")
        (or (getenv-or-dotenv "SWIMMY_DISCORD_HEARTBEAT")
            (getenv-or-dotenv "SWIMMY_DISCORD_ALERTS")))
        
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
;;; STATUS REPORTING (Config)
;;; ==========================================
(defparameter *status-notification-interval* 3600) ; Default 1 hour

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

;;; ==========================================
;;; RISK & SCHOOL THRESHOLDS (Config)
;;; ==========================================
(defparameter *max-symbol-exposure* 0.30)
(defparameter *max-total-exposure* 0.60)
(defparameter *resignation-threshold* -2000.0)

;;; ==========================================
;;; NEURAL NETWORK (Config)
;;; ==========================================
(defparameter *nn-threshold* 0.6)

;;; ==========================================
;;; GOVERNANCE (Config)
;;; ==========================================
(defparameter *council-decision-threshold* 0.70)
(defparameter *notify-chieftain-threshold* :critical)
(defparameter *constitution-version* "1.0")
(defparameter *philosophy-log-max* 500)
(defparameter *philosophy-log-path* (swimmy-path ".opus/philosophy_log.md"))

;;; ==========================================
;;; TELEMETRY (Config)
;;; ==========================================
(defparameter *telemetry-enabled* t)
(defparameter *telemetry-schema-version* 1)
(defparameter *telemetry-max-bytes* (* 10 1024 1024))

;;; ==========================================
;;; WFV SCHEDULING (Config)
;;; ==========================================
(defparameter *wfv-enabled* t)
(defparameter *wfv-interval-sec* (* 60 60)) ; default 1h
(defparameter *wfv-max-pending* 2)
(defparameter *wfv-max-per-run* 1)

;;; ==========================================
;;; TIMING (Config)
;;; ==========================================
(defparameter *dream-interval* 300
  "Seconds between Gemini dreaming cycles")


(format t "[L] ⚙️ core/config.lisp loaded - System configuration~%")
