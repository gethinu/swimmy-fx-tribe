;; performance-tracker.lisp - Sharpe Ratio Measurement System
;; Taleb Advisor Homework #1: Live operation Sharpe tracking over 3 months
;; V1.0: Initial implementation

;;; ==========================================
;;; PERFORMANCE TRACKING STATE
;;; ==========================================

(defparameter *performance-log-path* "/home/swimmy/swimmy/.opus/performance_log.json")
(defparameter *daily-returns* nil "List of (timestamp return equity)")
(defparameter *last-performance-save* 0)
(defparameter *performance-save-interval* 3600) ;; Save every hour
(defparameter *base-equity* 100000.0 "Base equity for return calculation")
(defparameter *last-weekly-report-day* -1)

;;; ==========================================
;;; DAILY RETURN TRACKING
;;; ==========================================

(defun record-daily-return (pnl &optional (equity *base-equity*))
  "Record daily return for Sharpe calculation.
   Called at end of each trading day."
  (let* ((now (get-universal-time))
         (return-pct (if (and equity (> equity 0))
                         (/ pnl equity)
                         0.0)))
    (push (list :timestamp now 
                :pnl pnl 
                :return return-pct
                :equity equity)
          *daily-returns*)
    (format t "[P] 📊 Daily return recorded: ~,2f% (PnL: ¥~:d)~%" 
            (* 100 return-pct) (round pnl))
    ;; Auto-save periodically
    (maybe-save-performance-log)))

(defun get-returns-for-period (days)
  "Get list of returns for the last N days"
  (let* ((now (get-universal-time))
         (cutoff (- now (* days 86400))))
    (mapcar (lambda (r) (getf r :return))
            (remove-if (lambda (r) (< (getf r :timestamp) cutoff))
                       *daily-returns*))))

;;; ==========================================
;;; SHARPE RATIO CALCULATION
;;; ==========================================

(defun calculate-rolling-sharpe (days &optional (risk-free-rate 0.0))
  "Calculate annualized Sharpe ratio over rolling N-day window.
   Taleb requirement: Measure after 3 months (90 days).
   Formula: (mean_return - risk_free) / stdev * sqrt(252)"
  (let ((returns (get-returns-for-period days)))
    (if (< (length returns) 5)
        (progn
          (format t "[P] ⚠️ Insufficient data for Sharpe: ~d days (need 5+)~%" 
                  (length returns))
          nil)
        (let* ((n (length returns))
               (mean (/ (reduce #'+ returns) n))
               (excess-mean (- mean (/ risk-free-rate 252))) ;; Daily risk-free
               (variance (/ (reduce #'+ 
                                    (mapcar (lambda (r) (expt (- r mean) 2)) 
                                            returns))
                            (1- n))) ;; Sample variance
               (stdev (sqrt variance))
               (daily-sharpe (if (> stdev 0) (/ excess-mean stdev) 0))
               (annualized-sharpe (* daily-sharpe (sqrt 252))))
          (list :daily-sharpe daily-sharpe
                :annualized-sharpe annualized-sharpe
                :n-samples n
                :mean-return (* 100 mean)
                :volatility (* 100 stdev)
                :meets-threshold (> annualized-sharpe 1.0))))))

(defun get-sharpe-summary ()
  "Get current Sharpe status summary"
  (let ((s30 (calculate-rolling-sharpe 30))
        (s60 (calculate-rolling-sharpe 60))
        (s90 (calculate-rolling-sharpe 90)))
    (format nil "Sharpe Ratios | 30d: ~,2f | 60d: ~,2f | 90d: ~,2f"
            (or (getf s30 :annualized-sharpe) 0)
            (or (getf s60 :annualized-sharpe) 0)
            (or (getf s90 :annualized-sharpe) 0))))

;;; ==========================================
;;; PERSISTENCE
;;; ==========================================

(defun save-performance-log ()
  "Save performance log to JSON file"
  (handler-case
      (progn
        (ensure-directories-exist *performance-log-path*)
        (with-open-file (out *performance-log-path* :direction :output :if-exists :supersede)
          (format out "[~%")
          (loop for i from 0
                for r in *daily-returns*
                do (format out "  {\"ts\": ~d, \"pnl\": ~,2f, \"ret\": ~,6f, \"eq\": ~,2f}~a~%"
                           (getf r :timestamp)
                           (getf r :pnl)
                           (getf r :return)
                           (getf r :equity)
                           (if (< i (1- (length *daily-returns*))) "," "")))
          (format out "]~%"))
        (setf *last-performance-save* (get-universal-time))
        (format t "[P] 💾 Performance log saved (~d records)~%" (length *daily-returns*)))
    (error (e)
      (format t "[P] ⚠️ Failed to save performance log: ~a~%" e))))

(defun load-performance-log ()
  "Load performance log from JSON file on startup"
  (handler-case
      (when (probe-file *performance-log-path*)
        (with-open-file (in *performance-log-path* :direction :input)
          (let ((content (make-string (file-length in))))
            (read-sequence content in)
            ;; Simple JSON parsing (array of objects)
            (let ((entries nil)
                  (lines (remove-if (lambda (s) (or (string= s "") 
                                                    (string= (string-trim " " s) "[")
                                                    (string= (string-trim " " s) "]")))
                                    (uiop:split-string content :separator '(#\Newline)))))
              (dolist (line lines)
                (let* ((clean (string-trim " ," line))
                       ;; Extract values with regex-like parsing
                       (ts-start (search "\"ts\":" clean))
                       (pnl-start (search "\"pnl\":" clean))
                       (ret-start (search "\"ret\":" clean))
                       (eq-start (search "\"eq\":" clean)))
                  (when (and ts-start pnl-start ret-start eq-start)
                    (handler-case
                        (let ((ts (parse-integer (subseq clean (+ ts-start 5)) :junk-allowed t))
                              (pnl (read-from-string (subseq clean (+ pnl-start 6))))
                              (ret (read-from-string (subseq clean (+ ret-start 6))))
                              (eq (read-from-string (subseq clean (+ eq-start 5)))))
                          (push (list :timestamp ts :pnl pnl :return ret :equity eq) entries))
                      (error () nil)))))
              (setf *daily-returns* (nreverse entries))
              (format t "[P] 📂 Loaded ~d performance records~%" (length *daily-returns*))))))
    (error (e)
      (format t "[P] ⚠️ Failed to load performance log: ~a~%" e))))

(defun maybe-save-performance-log ()
  "Save performance log if interval has passed"
  (when (> (- (get-universal-time) *last-performance-save*) *performance-save-interval*)
    (save-performance-log)))

;;; ==========================================
;;; WEEKLY DISCORD REPORT
;;; ==========================================

(defun weekly-performance-report ()
  "Generate and send weekly performance report to Discord"
  (let* ((s30 (calculate-rolling-sharpe 30))
         (s60 (calculate-rolling-sharpe 60))
         (s90 (calculate-rolling-sharpe 90))
         (n-trades (length *daily-returns*))
         (total-pnl (reduce #'+ (mapcar (lambda (r) (getf r :pnl)) *daily-returns*) 
                            :initial-value 0)))
    (format t "~%[P] ════════════════════════════════════════~%")
    (format t "[P] 📈 WEEKLY PERFORMANCE REPORT (Taleb)~%")
    (format t "[P] ════════════════════════════════════════~%")
    (format t "[P] Trading Days: ~d | Total PnL: ¥~:d~%" n-trades (round total-pnl))
    (format t "[P] ~%")
    (format t "[P] Sharpe Ratios (Annualized):~%")
    (when s30
      (format t "[P]   30-day: ~,2f (~,2f% avg, ~,2f% vol)~%"
              (getf s30 :annualized-sharpe)
              (getf s30 :mean-return)
              (getf s30 :volatility)))
    (when s60
      (format t "[P]   60-day: ~,2f~%" (getf s60 :annualized-sharpe)))
    (when s90
      (let ((meets (getf s90 :meets-threshold)))
        (format t "[P]   90-day: ~,2f ~a~%" 
                (getf s90 :annualized-sharpe)
                (if meets "✅ MEETS THRESHOLD" "⚠️ BELOW 1.0"))))
    (format t "[P] ════════════════════════════════════════~%~%")
    
    ;; Send to Discord
    (when (fboundp 'notify-discord-daily)
      (notify-discord-daily 
       (format nil "📈 **Weekly Performance Report**~%~%Trading Days: ~d~%Total PnL: ¥~:d~%~%**Sharpe Ratios (Annualized)**~%30d: ~,2f~%60d: ~,2f~%90d: ~,2f ~a"
               n-trades (round total-pnl)
               (or (and s30 (getf s30 :annualized-sharpe)) 0)
               (or (and s60 (getf s60 :annualized-sharpe)) 0)
               (or (and s90 (getf s90 :annualized-sharpe)) 0)
               (if (and s90 (getf s90 :meets-threshold)) "✅" "⚠️"))
       :color 3066993))))

(defun maybe-send-weekly-report ()
  "Check if weekly report should be sent (every Sunday)"
  (multiple-value-bind (sec min hour day month year dow) 
      (decode-universal-time (get-universal-time) -9)  ;; JST
    (declare (ignore sec min hour day month year))
    (when (and (= dow 0) ;; Sunday
               (/= *last-weekly-report-day* dow))
      (setf *last-weekly-report-day* dow)
      (weekly-performance-report))))

;;; ==========================================
;;; INTEGRATION HOOKS
;;; ==========================================

(defun on-daily-close (daily-pnl equity)
  "Hook to be called at end of trading day"
  (record-daily-return daily-pnl equity)
  (maybe-send-weekly-report))

;;; ==========================================
;;; TALEB ADDITIONAL HOMEWORK #1: ANTIFRAGILE METRIC
;;; ==========================================
;;; "Does profit increase when volatility increases?"
;;; Antifragile systems gain from disorder.

(defparameter *volatility-profit-pairs* nil 
  "List of (volatility pnl) pairs for antifragility analysis")

(defun record-volatility-profit (volatility pnl)
  "Record volatility-profit pair for antifragility analysis"
  (push (list :volatility volatility :pnl pnl :timestamp (get-universal-time))
        *volatility-profit-pairs*)
  ;; Keep last 100 data points
  (when (> (length *volatility-profit-pairs*) 100)
    (setf *volatility-profit-pairs* (subseq *volatility-profit-pairs* 0 100))))

(defun calculate-antifragility-score ()
  "Calculate antifragility: correlation between volatility and profit.
   Score > 0 = Antifragile (gains from chaos)
   Score < 0 = Fragile (harmed by chaos)
   Score = 0 = Robust (unaffected)"
  (when (> (length *volatility-profit-pairs*) 10)
    (let* ((n (length *volatility-profit-pairs*))
           (vols (mapcar (lambda (p) (getf p :volatility)) *volatility-profit-pairs*))
           (pnls (mapcar (lambda (p) (getf p :pnl)) *volatility-profit-pairs*))
           (mean-vol (/ (reduce #'+ vols) n))
           (mean-pnl (/ (reduce #'+ pnls) n))
           (cov (/ (reduce #'+ (mapcar (lambda (v p) (* (- v mean-vol) (- p mean-pnl))) 
                                       vols pnls)) n))
           (var-vol (/ (reduce #'+ (mapcar (lambda (v) (expt (- v mean-vol) 2)) vols)) n))
           (var-pnl (/ (reduce #'+ (mapcar (lambda (p) (expt (- p mean-pnl) 2)) pnls)) n))
           (std-vol (sqrt var-vol))
           (std-pnl (sqrt var-pnl)))
      (if (and (> std-vol 0) (> std-pnl 0))
          (let ((correlation (/ cov (* std-vol std-pnl))))
            (list :antifragility-score correlation
                  :interpretation (cond 
                                    ((> correlation 0.3) :antifragile)
                                    ((< correlation -0.3) :fragile)
                                    (t :robust))
                  :n-samples n
                  :mean-volatility mean-vol
                  :mean-pnl mean-pnl))
          (list :antifragility-score 0 :interpretation :insufficient-data)))))

(defun get-antifragility-report ()
  "Generate antifragility report for Taleb"
  (let ((result (calculate-antifragility-score)))
    (if result
        (progn
          (format t "~%[A] 🦢 ANTIFRAGILITY ANALYSIS (Taleb)~%")
          (format t "[A] Score: ~,3f (~a)~%"
                  (getf result :antifragility-score)
                  (getf result :interpretation))
          (format t "[A] When volatility ↑: ~a~%"
                  (case (getf result :interpretation)
                    (:antifragile "Profit tends to INCREASE ✅")
                    (:fragile "Profit tends to DECREASE ❌")
                    (:robust "Profit is UNAFFECTED ➖")
                    (t "Insufficient data")))
          result)
        (format t "[A] ⚠️ Insufficient data for antifragility analysis~%"))))

;;; ==========================================
;;; TALEB ADDITIONAL HOMEWORK #2: FAT TAIL DETECTION
;;; ==========================================
;;; Track kurtosis of return distribution
;;; Normal distribution kurtosis = 3.0
;;; Kurtosis > 3.0 = Fat tails (Black Swan risk)

(defun calculate-kurtosis ()
  "Calculate excess kurtosis of return distribution.
   Excess kurtosis = kurtosis - 3
   > 0 = Leptokurtic (fat tails, Black Swan prone)
   < 0 = Platykurtic (thin tails)
   = 0 = Mesokurtic (normal distribution)"
  (let ((returns (mapcar (lambda (r) (getf r :return)) *daily-returns*)))
    (when (> (length returns) 10)
      (let* ((n (length returns))
             (mean (/ (reduce #'+ returns) n))
             (m2 (/ (reduce #'+ (mapcar (lambda (r) (expt (- r mean) 2)) returns)) n))
             (m4 (/ (reduce #'+ (mapcar (lambda (r) (expt (- r mean) 4)) returns)) n))
             (std (sqrt m2))
             ;; Kurtosis = m4 / m2^2, excess = kurtosis - 3
             (kurtosis (if (> m2 0) (/ m4 (expt m2 2)) 3.0))
             (excess-kurtosis (- kurtosis 3.0)))
        (list :kurtosis kurtosis
              :excess-kurtosis excess-kurtosis
              :has-fat-tails (> excess-kurtosis 1.0)
              :risk-level (cond
                            ((> excess-kurtosis 3.0) :extreme-fat-tails)
                            ((> excess-kurtosis 1.0) :fat-tails)
                            ((> excess-kurtosis 0) :slightly-fat)
                            (t :normal-or-thin))
              :n-samples n
              :std-dev (* 100 std))))))

(defun get-fat-tail-report ()
  "Generate fat tail analysis report"
  (let ((result (calculate-kurtosis)))
    (if result
        (progn
          (format t "~%[F] 📊 FAT TAIL ANALYSIS (Taleb)~%")
          (format t "[F] Kurtosis: ~,2f (Excess: ~,2f)~%"
                  (getf result :kurtosis)
                  (getf result :excess-kurtosis))
          (format t "[F] Risk Level: ~a~%"
                  (case (getf result :risk-level)
                    (:extreme-fat-tails "🔴 EXTREME FAT TAILS - High Black Swan risk!")
                    (:fat-tails "🟡 FAT TAILS - Black Swan possible")
                    (:slightly-fat "🟢 Slightly fat - Monitor")
                    (:normal-or-thin "⚪ Normal/Thin tails")))
          (format t "[F] Return StdDev: ~,2f%~%" (getf result :std-dev))
          ;; Alert if extreme
          (when (and (getf result :has-fat-tails) (fboundp 'notify-discord-alert))
            (notify-discord-alert 
             (format nil "📊 FAT TAIL WARNING~%Kurtosis: ~,2f~%Risk: ~a"
                     (getf result :kurtosis)
                     (getf result :risk-level))))
          result)
        (format t "[F] ⚠️ Insufficient data for kurtosis calculation~%"))))

;;; ==========================================
;;; TALEB ADDITIONAL HOMEWORK #3: BARBELL STRATEGY PROOF
;;; ==========================================
;;; Track 90% safe / 10% risk allocation
;;; Barbell = extreme safety + extreme risk, no middle

(defparameter *barbell-allocations* nil
  "Track allocation history: (timestamp safe-pct risk-pct)")
(defparameter *barbell-target-safe* 0.90 "Target safe allocation (90%)")
(defparameter *barbell-target-risk* 0.10 "Target risk allocation (10%)")

(defun record-barbell-allocation (safe-pct risk-pct)
  "Record current barbell allocation"
  (push (list :timestamp (get-universal-time)
              :safe-pct safe-pct
              :risk-pct risk-pct
              :compliant (and (>= safe-pct (* 0.85 *barbell-target-safe*))
                              (<= risk-pct (* 1.5 *barbell-target-risk*))))
        *barbell-allocations*)
  (when (> (length *barbell-allocations*) 100)
    (setf *barbell-allocations* (subseq *barbell-allocations* 0 100))))

(defun calculate-current-barbell ()
  "Calculate current barbell allocation from positions.
   Safe = Trend following with tight stops
   Risk = High volatility / breakout plays"
  ;; In absence of real position data, estimate from category lots
  (let* ((trend-lot (if (fboundp 'get-category-lot) (get-category-lot :trend) 0.03))
         (reversion-lot (if (fboundp 'get-category-lot) (get-category-lot :reversion) 0.02))
         (breakout-lot (if (fboundp 'get-category-lot) (get-category-lot :breakout) 0.01))
         (scalp-lot (if (fboundp 'get-category-lot) (get-category-lot :scalp) 0.01))
         ;; Safe = trend + reversion (controlled risk)
         (safe-exposure (+ trend-lot reversion-lot))
         ;; Risk = breakout + scalp (higher variance)
         (risk-exposure (+ breakout-lot scalp-lot))
         (total (+ safe-exposure risk-exposure)))
    (if (> total 0)
        (let ((safe-pct (/ safe-exposure total))
              (risk-pct (/ risk-exposure total)))
          (record-barbell-allocation safe-pct risk-pct)
          (list :safe-pct safe-pct
                :risk-pct risk-pct
                :compliant (and (>= safe-pct 0.80) (<= risk-pct 0.25))
                :trend-lot trend-lot
                :reversion-lot reversion-lot
                :breakout-lot breakout-lot
                :scalp-lot scalp-lot))
        (list :safe-pct 0 :risk-pct 0 :compliant nil))))

(defun get-barbell-compliance ()
  "Calculate barbell compliance rate over time"
  (when (> (length *barbell-allocations*) 0)
    (let* ((compliant-count (count-if (lambda (a) (getf a :compliant)) *barbell-allocations*))
           (total (length *barbell-allocations*))
           (compliance-rate (/ compliant-count total)))
      (list :compliance-rate compliance-rate
            :compliant-days compliant-count
            :total-days total
            :meets-standard (>= compliance-rate 0.8)))))

(defun get-barbell-report ()
  "Generate barbell strategy compliance report"
  (let ((current (calculate-current-barbell))
        (compliance (get-barbell-compliance)))
    (format t "~%[B] 🏋️ BARBELL STRATEGY ANALYSIS (Taleb)~%")
    (format t "[B] Target: ~,0f% Safe / ~,0f% Risk~%"
            (* 100 *barbell-target-safe*)
            (* 100 *barbell-target-risk*))
    (format t "[B] Current: ~,0f% Safe / ~,0f% Risk~%"
            (* 100 (getf current :safe-pct))
            (* 100 (getf current :risk-pct)))
    (format t "[B] Status: ~a~%"
            (if (getf current :compliant) 
                "✅ COMPLIANT" 
                "❌ NON-COMPLIANT"))
    (when compliance
      (format t "[B] Historical Compliance: ~,0f% (~d/~d days)~%"
              (* 100 (getf compliance :compliance-rate))
              (getf compliance :compliant-days)
              (getf compliance :total-days)))
    (list :current current :compliance compliance)))

;;; ==========================================
;;; TALEB COMPREHENSIVE REPORT
;;; ==========================================

(defun taleb-full-report ()
  "Generate comprehensive Taleb metrics report"
  (format t "~%")
  (format t "════════════════════════════════════════════════════~%")
  (format t "🦢 TALEB COMPREHENSIVE RISK REPORT~%")
  (format t "════════════════════════════════════════════════════~%")
  
  ;; 1. Sharpe (already existing)
  (let ((s90 (calculate-rolling-sharpe 90)))
    (format t "~%📈 SHARPE RATIO (90-day): ~,2f ~a~%"
            (or (getf s90 :annualized-sharpe) 0)
            (if (and s90 (getf s90 :meets-threshold)) "✅" "⚠️")))
  
  ;; 2. Antifragility
  (get-antifragility-report)
  
  ;; 3. Fat Tails
  (get-fat-tail-report)
  
  ;; 4. Barbell
  (get-barbell-report)
  
  (format t "~%════════════════════════════════════════════════════~%"))

;;; ==========================================
;;; NAVAL PROOF: 3-MONTH PROFITABILITY TRACKING
;;; ==========================================
;;; "まだデータがない。3ヶ月後、このシステムが利益を出せば本物"
;;; Track cumulative profitability and prove the system works

(defparameter *proof-start-date* (get-universal-time)
  "When the Naval proof period started")
(defparameter *proof-duration-days* 90
  "Naval requirement: Prove profitability over 90 days")
(defparameter *cumulative-pnl-proof* 0.0
  "Total PnL accumulated during proof period")
(defparameter *proof-trade-count* 0
  "Number of trades during proof period")
(defparameter *proof-win-count* 0
  "Number of winning trades during proof period")

(defun record-proof-trade (pnl)
  "Record a trade for the Naval 3-month proof"
  (incf *cumulative-pnl-proof* pnl)
  (incf *proof-trade-count*)
  (when (> pnl 0) (incf *proof-win-count*)))

(defun get-proof-progress ()
  "Get progress toward 3-month Naval proof"
  (let* ((now (get-universal-time))
         (elapsed-seconds (- now *proof-start-date*))
         (elapsed-days (/ elapsed-seconds 86400.0))
         (progress-pct (* 100 (/ elapsed-days *proof-duration-days*)))
         (win-rate (if (> *proof-trade-count* 0)
                       (/ *proof-win-count* (float *proof-trade-count*))
                       0.0))
         (is-profitable (> *cumulative-pnl-proof* 0))
         (is-complete (>= elapsed-days *proof-duration-days*)))
    (list :elapsed-days (floor elapsed-days)
          :remaining-days (max 0 (- *proof-duration-days* (floor elapsed-days)))
          :progress-pct (min 100 progress-pct)
          :cumulative-pnl *cumulative-pnl-proof*
          :trade-count *proof-trade-count*
          :win-rate (* 100 win-rate)
          :is-profitable is-profitable
          :is-complete is-complete
          :verdict (cond
                     ((not is-complete) :in-progress)
                     ((and is-complete is-profitable) :proven)
                     (t :failed)))))

(defun naval-proof-report ()
  "Generate Naval 3-month proof report"
  (let ((proof (get-proof-progress)))
    (format t "~%")
    (format t "════════════════════════════════════════════════════~%")
    (format t "🚀 NAVAL 3-MONTH PROOF~%")
    (format t "════════════════════════════════════════════════════~%")
    (format t "~%📅 PROGRESS:~%")
    (format t "   Day ~d / ~d (~,0f%)~%"
            (getf proof :elapsed-days) *proof-duration-days* (getf proof :progress-pct))
    (format t "   Remaining: ~d days~%" (getf proof :remaining-days))
    (format t "~%💰 PERFORMANCE:~%")
    (format t "   Cumulative PnL: ¥~:d~%" (round (getf proof :cumulative-pnl)))
    (format t "   Trades: ~d (~,0f% win rate)~%"
            (getf proof :trade-count) (getf proof :win-rate))
    (format t "~%🎯 VERDICT:~%")
    (format t "   ~a~%"
            (case (getf proof :verdict)
              (:in-progress "⏳ In Progress - Keep trading")
              (:proven "✅ PROVEN - System is profitable!")
              (:failed "❌ FAILED - System lost money")))
    (format t "~%════════════════════════════════════════════════════~%")
    proof))

(defun reset-naval-proof ()
  "Reset the proof period (use only if intentionally restarting)"
  (setf *proof-start-date* (get-universal-time))
  (setf *cumulative-pnl-proof* 0.0)
  (setf *proof-trade-count* 0)
  (setf *proof-win-count* 0)
  (format t "[N] 🔄 Naval proof period reset. Clock starts now.~%"))

;;; ==========================================
;;; INITIALIZATION
;;; ==========================================

(load-performance-log)
(format t "[L] 📊 performance-tracker.lisp loaded - Sharpe tracking active~%")
(format t "[L] 🎯 Taleb requirement: Sharpe > 1.0 after 90 days~%")
(format t "[L] 🦢 Taleb additional: Antifragility + Fat Tails + Barbell~%")
(format t "[L] 🚀 Naval proof: 3-month profitability tracking active~%")

