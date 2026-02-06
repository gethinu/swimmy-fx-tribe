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

(defun %sexp-candle->struct (entry)
  "Convert HISTORY entry alist to candle struct."
  (when (listp entry)
    (let* ((ts (%alist-val entry '(t timestamp) nil))
           (open (%alist-val entry '(o open) nil))
           (high (%alist-val entry '(h high) nil))
           (low (%alist-val entry '(l low) nil))
           (close (%alist-val entry '(c close) nil))
           (vol (%alist-val entry '(v volume) 0)))
      (when ts
        (swimmy.globals:make-candle :timestamp ts
                                    :open (if open (float open) 0.0)
                                    :high (if high (float high) 0.0)
                                    :low (if low (float low) 0.0)
                                    :close (if close (float close) 0.0)
                                    :volume (if vol (float vol) 0.0))))))

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
(defparameter *backtest-recv-last-id* nil)
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
            (let ((pending (max 0 (- (if (boundp 'swimmy.globals::*backtest-submit-count*)
                                         swimmy.globals::*backtest-submit-count*
                                         0)
                                     *backtest-recv-count*))))
            (ensure-directories-exist "data/reports/")
            (with-open-file (s "data/reports/backtest_status.txt"
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
              (format s "timestamp: ~a~%" now)
              (format s "count: ~d~%" *backtest-recv-count*)
              (format s "pending: ~d~%" pending)
              (format s "last_name: ~a~%" (or *backtest-recv-last-name* "N/A"))
              (format s "last_sharpe: ~,4f~%" *backtest-recv-last-sharpe*)
              (format s "last_trades: ~d~%" *backtest-recv-last-trades*)
              (format s "last_request_id: ~a~%" (or *backtest-recv-last-id* "N/A")))))
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
                          *backtest-recv-last-id* request-id
                          *backtest-recv-last-ts* (get-universal-time))
                     (maybe-log-backtest-recv)
                     ;; Attach latency if request-id matches queue entry
                     (when request-id
                       (multiple-value-bind (req-id req-at status) (swimmy.school:lookup-oos-request name)
                         (declare (ignore status))
                         (when (and req-id req-at (string= req-id request-id))
                           (setf (getf metrics :oos-latency) (- (get-universal-time) req-at)))))
                     (let* ((trade-list (%alist-val result '(trade_list trade-list) nil))
                            (oos-kind (cond (is-oos "OOS")
                                            ((or is-qual is-rr) "BACKTEST")
                                            (t "BACKTEST"))))
                       (when (and trade-list request-id name)
                         (swimmy.school:record-backtest-trades request-id name oos-kind trade-list)))
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
                        (backtest-debug-log "apply-backtest-result start name=~a request_id=~a"
                                            name request-id)
                        (handler-case
                            (progn
                              (swimmy.school:apply-backtest-result name metrics)
                              (backtest-debug-log "apply-backtest-result end name=~a request_id=~a"
                                                  name request-id))
                          (error (e)
                            (backtest-debug-log "apply-backtest-result error name=~a request_id=~a err=~a"
                                                name request-id e)
                            (%dlq-record "apply-backtest-result error" msg result)
                            (error e)))

                        ;; V50.2: Trigger V2 Handler (Screening/Validation)
                        (when (fboundp 'swimmy.school::handle-v2-result)
                          (backtest-debug-log "handle-v2-result start name=~a request_id=~a"
                                              full-name request-id)
                          (handler-case
                              (progn
                                (swimmy.school::handle-v2-result full-name metrics)
                                (backtest-debug-log "handle-v2-result end name=~a request_id=~a"
                                                    full-name request-id))
                            (error (e)
                              (backtest-debug-log "handle-v2-result error name=~a request_id=~a err=~a"
                                                  full-name request-id e)
                              (%dlq-record "handle-v2-result error" msg result)
                              (error e))))
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
                  ((string= type-str swimmy.core:+MSG-TICK+)
                   (let* ((symbol (%alist-val sexp '(symbol :symbol) ""))
                          (bid (%alist-val sexp '(bid :bid) nil))
                          (ask (%alist-val sexp '(ask :ask) nil))
                          (symbol-str (if (symbolp symbol) (symbol-name symbol) symbol)))
                     (when bid
                       (swimmy.main:update-candle bid symbol-str)
                       (when (fboundp 'swimmy.school:process-category-trades)
                         (swimmy.school:process-category-trades symbol-str bid ask))
                       (when (fboundp 'swimmy.shell:save-live-status)
                         (swimmy.shell:save-live-status))
                       (when (fboundp 'swimmy.shell:send-periodic-status-report)
                         (swimmy.shell:send-periodic-status-report symbol-str bid))
                       (handler-case
                           (when (fboundp 'swimmy.school:continuous-learning-step)
                             (swimmy.school:continuous-learning-step))
                         (error () nil))
                       (maybe-alert-backtest-stale))))
                  ((string= type-str swimmy.core:+MSG-ACCOUNT-INFO+)
                   (swimmy.executor:process-account-info sexp))
                  ((string= type-str swimmy.core:+MSG-SWAP-DATA+)
                   (let* ((sym (%alist-val sexp '(symbol :symbol) ""))
                          (sym-str (if (symbolp sym) (symbol-name sym) sym))
                          (s-long (%normalize-rate (%alist-val sexp '(swap_long swap-long) 0.0)))
                          (s-short (%normalize-rate (%alist-val sexp '(swap_short swap-short) 0.0)))
                          (spread (%normalize-rate (%alist-val sexp '(spread) 0.0))))
                     (format t "[DISPATCH] 📥 Received SWAP_DATA for ~a~%" sym-str)
                     (swimmy.school.scribe:scribe-record :RECORD-SWAPS sym-str s-long s-short spread)))
                  ((string= type-str swimmy.core:+MSG-HEARTBEAT+)
                   (let* ((hb-id (%alist-val sexp '(id :id) nil))
                          (hb-status (%alist-val sexp '(status :status) nil))
                          (hb-source (%alist-val sexp '(source :source) nil)))
                     (swimmy.core::emit-telemetry-event "heartbeat.recv"
                       :service "dispatcher"
                       :severity "info"
                       :correlation-id hb-id
                       :data (jsown:new-js
                               ("heartbeat_id" hb-id)
                               ("status" hb-status)
                               ("source" hb-source)))
                     (setf swimmy.globals:*last-guardian-heartbeat* (get-universal-time))
                     (maybe-alert-backtest-stale)))
                  ((string= type-str swimmy.core:+MSG-ORDER-ACK+)
                   (let ((order-id (%alist-val sexp '(id :id) nil))
                         (ticket (%alist-val sexp '(ticket :ticket) nil)))
                     (format t "[DISPATCH] 📥 ORDER_ACK id=~a ticket=~a~%" order-id ticket)))
                  ((string= type-str swimmy.core:+MSG-TRADE-CLOSED+)
                   (swimmy.executor:process-trade-closed sexp msg))
                  ((string= type-str swimmy.core:+MSG-HISTORY+)
                   (let* ((symbol (%alist-val sexp '(symbol :symbol) ""))
                          (tf-raw (%alist-val sexp '(tf :tf) "M1"))
                          (tf (if (symbolp tf-raw) (symbol-name tf-raw) tf-raw))
                          (data (%alist-val sexp '(data :data) nil))
                          (candles (remove nil (mapcar #'%sexp-candle->struct (or data '()))))
                          (candles (sort candles #'> :key #'swimmy.globals:candle-timestamp)))
                     (when (and symbol candles)
                       (if (string= (string-upcase tf) "M1")
                           (setf (gethash symbol swimmy.globals:*candle-histories*) candles)
                           (let ((tf-map (or (gethash symbol swimmy.globals:*candle-histories-tf*)
                                             (setf (gethash symbol swimmy.globals:*candle-histories-tf*)
                                                   (make-hash-table :test 'equal)))))
                             (setf (gethash tf tf-map) candles)))
                       (when (string= (string-upcase tf) "M1")
                         (setf swimmy.globals:*candle-history* candles)
                         (when (boundp 'swimmy.main::*candle-history*)
                           (setf swimmy.main::*candle-history* candles)))
                       (format t "[DISPATCH] 📥 HISTORY stored: ~a ~a (~d bars)~%"
                               symbol tf (length candles)))))
                  (t nil))))
            (progn
              (format t "[DISPATCH] ⚠️ JSON payload ignored (internal ZMQ is S-expression only)~%")
              nil)))
    (when err
      (format t "[L] Msg Error: ~a" err)
      nil)
    result))
(defun process-msg (msg) (internal-process-msg msg))
