(in-package :swimmy.main)

(defun %normalize-rate (value &optional (default 0.0))
  "Normalize percentage rates to 0-1 range when needed."
  (let ((v (float (or value default))))
    (if (> v 1.0) (/ v 100.0) v)))

(defun %alist-val (alist keys &optional default)
  "Return first matching value from ALIST by key list."
  (or (loop for k in keys
            for cell = (assoc k alist)
            when cell do (return (cdr cell)))
      default))

(defun %json-val (obj keys &optional default)
  "Return first matching value from JSON object by key list."
  (or (loop for k in keys
            for val = (handler-case (jsown:val obj k) (error () nil))
            when val do (return val))
      default))

(defun %normalize-strategy-name (value)
  "Normalize strategy name to string or NIL."
  (cond
    ((stringp value) value)
    ((symbolp value) (symbol-name value))
    (t nil)))

(defun %plist-p (lst)
  "Return T if LST looks like a keyword plist."
  (and (listp lst) (keywordp (first lst))))

(defun %result-strategy-name (result)
  "Extract strategy name from result (alist or plist)."
  (cond
    ((%plist-p result)
     (or (getf result :strategy_name)
         (getf result :strategy-name)
         (getf result :name)))
    (t
     (%alist-val result '(strategy_name :strategy_name strategy-name :strategy-name name :name) nil))))

(defparameter *missing-strategy-name-log-limit* 3)
(defparameter *missing-strategy-name-log-count* 0)
(defparameter *backtest-recv-count* 0)
(defparameter *backtest-recv-last-log* 0)
(defparameter *backtest-recv-last-name* nil)
(defparameter *backtest-recv-last-sharpe* 0.0)
(defparameter *backtest-recv-last-trades* 0)
(defparameter *backtest-recv-last-ts* (get-universal-time))
(defparameter *backtest-stale-threshold* 900)
(defparameter *backtest-stale-alert-interval* 1800)
(defparameter *backtest-stale-last-alert* 0)
(defparameter *backtest-debug-env* "SWIMMY_BACKTEST_DEBUG_RECV")
(defparameter *backtest-debug-log* "data/reports/backtest_debug.log")
(defparameter *backtest-dead-letter* nil
  "Dead-letter queue for BACKTEST_RESULT messages that failed processing.")

(defun backtest-debug-enabled-p ()
  (let ((val (uiop:getenv *backtest-debug-env*)))
    (and val (> (length val) 0)
         (member (string-downcase val) '("1" "true" "yes" "on") :test #'string=))))

(defun backtest-debug-log (fmt &rest args)
  (when (backtest-debug-enabled-p)
    (handler-case
        (let ((path (swimmy.core::swimmy-path *backtest-debug-log*)))
          (ensure-directories-exist path)
          (with-open-file (s path :direction :output :if-exists :append :if-does-not-exist :create)
            (format s "~d " (get-universal-time))
            (apply #'format s fmt args)
            (terpri s)))
      (error (e)
        (format t "[BACKTEST] ⚠️ Debug log write failed: ~a~%" e)))))

(defun %dlq-record (reason payload &optional context)
  "Record a failed BACKTEST_RESULT payload into the DLQ."
  (push (list :ts (get-universal-time)
              :reason reason
              :payload payload
              :context context)
        *backtest-dead-letter*)
  (format t "[DISPATCH] 📨 DLQ recorded (~a) count=~d~%" reason (length *backtest-dead-letter*)))

(defun replay-dead-letter (&key limit)
  "Replay BACKTEST_RESULT messages stored in the DLQ."
  (let ((processed 0)
        (remaining nil))
    (dolist (entry *backtest-dead-letter*)
      (when (and limit (>= processed limit))
        (setf remaining (append remaining (list entry)))
        (return))
      (handler-case
          (progn
            (internal-process-msg (getf entry :payload))
            (incf processed))
        (error ()
          (push entry remaining))))
    (setf *backtest-dead-letter* (nreverse remaining))
    processed))

(defun maybe-log-backtest-recv ()
  "Log and persist backtest receive status periodically."
  (let ((now (get-universal-time)))
    (when (> (- now *backtest-recv-last-log*) 60)
      (setf *backtest-recv-last-log* now)
      (format t "[BACKTEST] ✅ Received ~d results (last: ~a S=~,2f T=~d)~%"
              *backtest-recv-count*
              (or *backtest-recv-last-name* "N/A")
              *backtest-recv-last-sharpe*
              *backtest-recv-last-trades*)
      (handler-case
          (progn
            (ensure-directories-exist "data/reports/")
            (with-open-file (s "data/reports/backtest_status.txt"
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
              (format s "timestamp: ~a~%" now)
              (format s "count: ~d~%" *backtest-recv-count*)
              (format s "last_name: ~a~%" (or *backtest-recv-last-name* "N/A"))
              (format s "last_sharpe: ~,4f~%" *backtest-recv-last-sharpe*)
              (format s "last_trades: ~d~%" *backtest-recv-last-trades*)))
        (error (e)
          (format t "[BACKTEST] ⚠️ Failed to write status file: ~a~%" e))))))

(defun maybe-alert-backtest-stale ()
  "Alert if backtest results have not arrived for a while."
  (let ((now (get-universal-time)))
    (when (and (> (- now *backtest-recv-last-ts*) *backtest-stale-threshold*)
               (> (- now *backtest-stale-last-alert*) *backtest-stale-alert-interval*))
      (setf *backtest-stale-last-alert* now)
      (format t "[BACKTEST] ⚠️ No results for ~d sec (count=~d)~%"
              (- now *backtest-recv-last-ts*) *backtest-recv-count*)
      (when (fboundp 'swimmy.core:notify-discord-alert)
        (swimmy.core:notify-discord-alert
         (format nil "Backtest停滞: ~d秒間結果なし (count=~d, last=~a)"
                 (- now *backtest-recv-last-ts*)
                 *backtest-recv-count*
                 (or *backtest-recv-last-name* "N/A")))))))

(defun %invalid-name-p (name)
  "Return T if strategy name is empty/absent/placeholder."
  (or (null name)
      (string= name "")
      (string-equal name "NIL")
      (string-equal name "nil")))

(defun internal-process-msg (msg)
  (multiple-value-bind (result err)
      (ignore-errors
        (if (and (> (length msg) 0) (char= (char msg 0) #\())
            (let* ((sexp (swimmy.core:safe-read-sexp msg :package :swimmy.main)))
              (unless (and sexp (listp sexp))
                (format t "[DISPATCH] ⚠️ Unsafe/invalid SEXP ignored~%")
                (return-from internal-process-msg nil))
              (let* ((type (cdr (assoc 'type sexp)))
                     (type-str (cond ((stringp type) type)
                                     ((symbolp type) (symbol-name type))
                                     (t ""))))
                (cond
                  ((or (member type '(backtest-result :backtest-result) :test #'eq)
                       (string-equal type-str "BACKTEST_RESULT"))
                   (let* ((result (cdr (assoc 'result sexp)))
                          (result (if (and (listp result)
                                           (not (assoc 'strategy_name result))
                                           (listp (car result)))
                                      (car result)
                                      result))
                          (full-name (%normalize-strategy-name (%result-strategy-name result)))
                          (sharpe (float (%alist-val result '(sharpe sharpe_ratio sharpe-ratio) 0.0)))
                          (trades (or (%alist-val result '(trades total_trades total-trades) 0) 0))
                          (pnl (float (%alist-val result '(pnl total_profit total-profit) 0.0)))
                          (win-rate (%normalize-rate (%alist-val result '(win_rate winrate win-rate) 0.0)))
                          (profit-factor (float (%alist-val result '(profit_factor profit-factor pf) 0.0)))
                          (max-dd (%normalize-rate (%alist-val result '(max_dd max-drawdown max_drawdown max-dd) 1.0)))
                          (is-rr (and full-name (search "-RR" full-name)))
                          (is-qual (and full-name (search "-QUAL" full-name)))
                          (is-oos (and full-name (search "-OOS" full-name)))
                          (is-wfv (and full-name (or (search "_IS" full-name :from-end t)
                                                     (search "_OOS" full-name :from-end t))))
                          (name (when full-name
                                  (cond (is-rr (subseq full-name 0 is-rr))
                                        (is-qual (subseq full-name 0 is-qual))
                                        (is-oos (subseq full-name 0 is-oos))
                                        (t full-name))))
                          (request-id (%alist-val result '(request_id request-id) nil))
                          (metrics (list :sharpe sharpe :trades trades :pnl pnl
                                         :win-rate win-rate :profit-factor profit-factor :max-dd max-dd
                                         :request-id request-id)))
                     (backtest-debug-log "recv sexp name=~a full=~a sharpe=~,4f trades=~d"
                                         (or name "N/A") (or full-name "N/A") sharpe trades)
                     (when (%invalid-name-p name)
                       (when (< *missing-strategy-name-log-count* *missing-strategy-name-log-limit*)
                         (incf *missing-strategy-name-log-count*)
                         (format t "[L] ⚠️ BACKTEST_RESULT missing/invalid strategy_name. Result=~s~%" result))
                       (%dlq-record "missing/invalid strategy_name" msg result)
                       (format t "[L] ⚠️ BACKTEST_RESULT missing/invalid strategy_name (~a). Skipping.~%" full-name)
                       (return-from internal-process-msg nil))
                     (incf *backtest-recv-count*)
                     (setf *backtest-recv-last-name* name
                           *backtest-recv-last-sharpe* sharpe
                           *backtest-recv-last-trades* trades
                           *backtest-recv-last-ts* (get-universal-time))
                     (maybe-log-backtest-recv)
                     ;; Attach latency if request-id matches queue entry
                     (when request-id
                       (multiple-value-bind (req-id req-at status) (swimmy.school:lookup-oos-request name)
                         (declare (ignore status))
                         (when (and req-id req-at (string= req-id request-id))
                           (setf (getf metrics :oos-latency) (- (get-universal-time) req-at)))))
                     (cond
                       (is-oos
                        (swimmy.school:cache-backtest-result full-name metrics)
                        (when (fboundp 'swimmy.school:handle-oos-backtest-result)
                          (swimmy.school:handle-oos-backtest-result name metrics)))
                       (is-wfv
                        (swimmy.school:cache-backtest-result full-name metrics)
                        (when (fboundp 'swimmy.school:process-wfv-result)
                          (swimmy.school:process-wfv-result full-name metrics)))
                       (t
                        (swimmy.school:cache-backtest-result name metrics)
                        (swimmy.school:apply-backtest-result name metrics)

                        ;; V50.2: Trigger V2 Handler (Screening/Validation)
                        (when (fboundp 'swimmy.school::handle-v2-result)
                          (swimmy.school::handle-v2-result full-name metrics))
                        (cond
                          (is-qual
                           (push (cons name metrics) swimmy.globals:*qual-backtest-results-buffer*)
                           (let ((count (length swimmy.globals:*qual-backtest-results-buffer*))
                                 (expected swimmy.globals:*qual-expected-backtest-count*))
                             (when (and (> expected 0)
                                        (>= count (max 1 (floor (* expected 0.9)))))
                               (swimmy.core:notify-backtest-summary :qual))))
                          (t
                           (push (cons name metrics) swimmy.globals:*rr-backtest-results-buffer*)
                           (let ((count (length swimmy.globals:*rr-backtest-results-buffer*))
                                 (expected swimmy.globals:*rr-expected-backtest-count*))
                             (when (and (> expected 0)
                                        (>= count (max 1 (floor (* expected 0.9)))))
                               (swimmy.core:notify-backtest-summary :rr)))))))))
                  ((or (member type '(cpcv-result :cpcv-result) :test #'eq)
                       (string-equal type-str "CPCV_RESULT"))
                   (let* ((result (cdr (assoc 'result sexp)))
                          (name (cdr (assoc 'strategy_name result)))
                          (median (cdr (assoc 'median_sharpe result)))
                          (paths (cdr (assoc 'path_count result)))
                          (passed (cdr (assoc 'passed_count result)))
                          (failed (cdr (assoc 'failed_count result)))
                          (pass-rate (cdr (assoc 'pass_rate result)))
                          (is-passed (cdr (assoc 'is_passed result))))
                     (let ((result-plist (list :strategy-name name :median-sharpe median
                                               :path-count paths :passed-count passed
                                               :failed-count failed :pass-rate pass-rate
                                               :is-passed is-passed)))
                       (swimmy.core:notify-cpcv-result result-plist)
                       (push result-plist swimmy.globals:*cpcv-results-buffer*)
                       (let ((count (length swimmy.globals:*cpcv-results-buffer*))
                             (expected swimmy.globals:*expected-cpcv-count*))
                         (when (and (> expected 0)
                                    (>= count (max 1 (floor (* expected 0.9)))))
                           (swimmy.core:notify-cpcv-summary)))
                       (when (and is-passed (not (eq is-passed 'nil)))
                         (let ((strat (or (find name swimmy.school::*strategy-knowledge-base*
                                                :key #'swimmy.school:strategy-name :test #'string=)
                                          (find name swimmy.globals:*evolved-strategies*
                                                :key #'swimmy.school:strategy-name :test #'string=))))
                           (when strat
                             (setf (swimmy.school:strategy-cpcv-median-sharpe strat) median)
                             (setf (swimmy.school:strategy-cpcv-pass-rate strat) pass-rate)
                             (swimmy.school:upsert-strategy strat)
                             (if (swimmy.school:check-rank-criteria strat :S)
                                 (swimmy.school:ensure-rank strat :S
                                                            "CPCV Passed and Criteria Met")
                                 (format t "[CPCV] Strategy ~a passed CPCV but failed overall S-Rank criteria.~%"
                                         name))))))))
               (t nil)))))
            (let* ((json (jsown:parse msg))
                   (type (jsown:val json "type")))
              (cond
                ((string= type swimmy.core:+MSG-SWAP-DATA+)
                 ;; Phase 28: Data Lake (Swap History)
                 (format t "[DISPATCH] 📥 Received SWAP_DATA for ~a~%" (jsown:val json "symbol"))
                 (let ((sym (jsown:val json "symbol"))
                       (s-long (%normalize-rate (jsown:val json "swap_long")))
                       (s-short (%normalize-rate (jsown:val json "swap_short")))
                       (spread (%normalize-rate (jsown:val json "spread"))))
                   (swimmy.school.scribe:scribe-record :RECORD-SWAPS sym s-long s-short spread)))
                ((string= type swimmy.core:+MSG-TICK+)
                 (swimmy.main:update-candle (jsown:val json "bid")
                                            (jsown:val json "symbol"))
                 (when (fboundp 'swimmy.school:process-category-trades)
                   (swimmy.school:process-category-trades (jsown:val json "symbol")
                                                          (jsown:val json "bid")
                                                          (jsown:val json "ask")))
                 (when (fboundp 'swimmy.shell:save-live-status)
                   (swimmy.shell:save-live-status))
                 (when (fboundp 'swimmy.shell:send-periodic-status-report)
                   (swimmy.shell:send-periodic-status-report (jsown:val json "symbol")
                                                             (jsown:val json "bid")))
                 (handler-case
                     (when (fboundp 'swimmy.school:continuous-learning-step)
                       (swimmy.school:continuous-learning-step))
                   (error () nil))
                 (maybe-alert-backtest-stale))
                ((string= type swimmy.core:+MSG-HEARTBEAT+)
                 (let ((hb-id (ignore-errors (jsown:val json "id")))
                       (hb-status (ignore-errors (jsown:val json "status")))
                       (hb-source (ignore-errors (jsown:val json "source"))))
                   (swimmy.core::emit-telemetry-event "heartbeat.recv"
                     :service "dispatcher"
                     :severity "info"
                     :correlation-id hb-id
                     :data (jsown:new-js
                             ("heartbeat_id" hb-id)
                             ("status" hb-status)
                             ("source" hb-source))))
                 (setf swimmy.globals:*last-guardian-heartbeat* (get-universal-time))
                 (maybe-alert-backtest-stale))
                ((string= type swimmy.core:+MSG-ACCOUNT-INFO+)
                 (swimmy.executor:process-account-info json))
                ((string= type "SYSTEM_COMMAND")
                 (let ((action (jsown:val json "action")))
                   (cond
                     ((string= action "REPORT_STATUS")
                      (swimmy.school:report-active-positions))
                     ((string= action "BACKTEST_SUMMARY")
                      (swimmy.core:notify-backtest-summary :rr))
                     ((string= action "BACKTEST_SUMMARY_QUAL")
                      (swimmy.core:notify-backtest-summary :qual))
                     ((string= action "HEARTBEAT_NOW")
                      (if (fboundp 'swimmy.engine::heartbeat-now)
                          (swimmy.engine:heartbeat-now)
                          (format t "[DISPATCH] ⚠️ HEARTBEAT_NOW ignored (function missing)."))))))
                ((string= type "BACKTEST_RESULT")
                 (let ((err
                        (nth-value 1
                          (ignore-errors
                            (let* ((result (jsown:val json "result"))
                                   (full-name (%normalize-strategy-name
                                               (or (%json-val result '("strategy_name" "strategy-name" "name") nil)
                                                   (jsown:val result "strategy_name"))))
                                   (sharpe (float (%json-val result '("sharpe" "sharpe_ratio" "sharpeRatio") 0.0)))
                                   (trades (or (%json-val result '("trades" "total_trades" "totalTrades") 0) 0))
                                   (pnl (float (%json-val result '("pnl" "total_profit" "totalProfit") 0.0)))
                                   (win-rate (%normalize-rate (%json-val result '("win_rate" "winrate" "winRate") 0.0)))
                                   (profit-factor (float (%json-val result '("profit_factor" "profitFactor" "pf") 0.0)))
                                   (max-dd (%normalize-rate (%json-val result '("max_dd" "max_drawdown" "maxDrawdown") 1.0)))
                                   (is-rr (and full-name (search "-RR" full-name)))
                                   (is-qual (and full-name (search "-QUAL" full-name)))
                                   (is-oos (and full-name (search "-OOS" full-name)))
                                   (is-wfv (and full-name (or (search "_IS" full-name :from-end t)
                                                              (search "_OOS" full-name :from-end t))))
                                   (name (when full-name
                                           (cond (is-rr (subseq full-name 0 is-rr))
                                                 (is-qual (subseq full-name 0 is-qual))
                                                 (is-oos (subseq full-name 0 is-oos))
                                                 (t full-name))))
                                   (request-id (%json-val result '("request_id" "request-id" "requestId") nil))
                                   (metrics (list :sharpe sharpe :trades trades :pnl pnl
                                                  :win-rate win-rate :profit-factor profit-factor :max-dd max-dd
                                                  :request-id request-id)))
                              (backtest-debug-log "recv json name=~a full=~a sharpe=~,4f trades=~d"
                                                  (or name "N/A") (or full-name "N/A") sharpe trades)
                              (when (%invalid-name-p name)
                                (when (< *missing-strategy-name-log-count* *missing-strategy-name-log-limit*)
                                  (incf *missing-strategy-name-log-count*)
                                  (format t "[L] ⚠️ BACKTEST_RESULT missing/invalid strategy_name. Result=~s~%" result))
                                (%dlq-record "missing/invalid strategy_name" msg result)
                                (format t "[L] ⚠️ BACKTEST_RESULT missing/invalid strategy_name (~a). Skipping.~%" full-name)
                                (return-from internal-process-msg nil))
                              ;; Attach latency if request-id matches queue timestamp
                              (when request-id
                                (multiple-value-bind (req-id req-at status) (swimmy.school:lookup-oos-request name)
                                  (declare (ignore status))
                                  (when (and req-id req-at (string= req-id request-id))
                                    (setf (getf metrics :oos-latency) (- (get-universal-time) req-at)))))
                              (cond
                                (is-oos
                                 (when (fboundp 'swimmy.school:handle-oos-backtest-result)
                                   (swimmy.school:handle-oos-backtest-result name metrics)))
                                (is-wfv
                                 (when (fboundp 'swimmy.school:process-wfv-result)
                                   (swimmy.school:process-wfv-result full-name metrics)))
                                (t
                                 (swimmy.school:cache-backtest-result name metrics)
                                 (swimmy.school:apply-backtest-result name metrics)
                                 (cond
                                   (is-qual
                                    (push (cons name metrics) swimmy.globals:*qual-backtest-results-buffer*)
                                    (let ((count (length swimmy.globals:*qual-backtest-results-buffer*))
                                          (expected swimmy.globals:*qual-expected-backtest-count*))
                                      (when (and (> expected 0)
                                                 (>= count (max 1 (floor (* expected 0.9)))))
                                        (swimmy.core:notify-backtest-summary :qual))))
                                   (t
                                    (push (cons name metrics) swimmy.globals:*rr-backtest-results-buffer*)
                                    (let ((count (length swimmy.globals:*rr-backtest-results-buffer*))
                                          (expected swimmy.globals:*rr-expected-backtest-count*))
                                      (when (and (> expected 0)
                                                 (>= count (max 1 (floor (* expected 0.9)))))
                                        (swimmy.core:notify-backtest-summary :rr)))))))
                              (maybe-alert-backtest-stale))))))
                   (when err
                     (%dlq-record (format nil "BACKTEST_RESULT json exception: ~a" err) msg json))
                   nil))
                (t nil))))
    (when err
      (format t "[L] Msg Error: ~a" err)
      nil)
    result))
(defun process-msg (msg) (internal-process-msg msg))
