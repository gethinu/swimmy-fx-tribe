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

(defparameter +unix-to-universal-offset+ 2208988800)

(defun %as-integer (value)
  "Best-effort integer coercion for timestamp-ish values."
  (cond
    ((integerp value) value)
    ((numberp value) (truncate value))
    ((stringp value)
     (or (ignore-errors (parse-integer value :junk-allowed t))
         (let ((n (ignore-errors (swimmy.core:safe-parse-number value))))
           (when (numberp n) (truncate n)))))
    (t nil)))

(defun %normalize-external-timestamp (raw-ts &optional (now (get-universal-time)))
  "Normalize external timestamp to Lisp universal-time seconds.
   MT5/Data Keeper often emit Unix epoch seconds."
  (let* ((ts (%as-integer raw-ts))
         (now-unix (- now +unix-to-universal-offset+)))
    (cond
      ((null ts) nil)
      ((and (>= ts 500000000)
            (<= ts (+ now-unix (* 10 365 24 60 60)))
            (< (abs (- ts now-unix)) (abs (- ts now))))
       (+ ts +unix-to-universal-offset+))
      (t ts))))

(defun %sexp-candle->struct (entry)
  "Convert HISTORY entry alist to candle struct."
  (when (listp entry)
    (let* ((ts-raw (%alist-val entry '(t timestamp) nil))
           (ts (%normalize-external-timestamp ts-raw))
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

(defun %json-object-p (obj)
  "Return T when OBJ is a jsown object list (:OBJ ...)."
  (and (listp obj) (eq (first obj) :OBJ)))

(defun %json->alist (obj)
  "Convert jsown object/list into an alist with SWIMMY.MAIN symbols."
  (cond
    ((%json-object-p obj)
     (let ((out nil))
       (dolist (pair (rest obj) (nreverse out))
         (when (and (consp pair) (stringp (car pair)))
           (let* ((key (car pair))
                  (val (cdr pair))
                  (sym (intern (string-upcase key) :swimmy.main)))
             (push (cons sym (%json->alist val)) out))))))
    ((listp obj)
     (mapcar #'%json->alist obj))
    (t obj)))

(defun %json->sexp-backtest (json)
  "Convert BACKTEST_RESULT JSON into the equivalent S-expression."
  (let ((result (handler-case (jsown:val json "result") (error () nil))))
    (when result
      `((type . "BACKTEST_RESULT") (result . ,(%json->alist result))))))

(defun %normalize-strategy-name (value)
  "Normalize strategy name to string or NIL."
  (cond
    ((stringp value) value)
    ((symbolp value) (symbol-name value))
    (t nil)))

(defun %plist-p (lst)
  "Return T if LST looks like a keyword plist."
  (and (listp lst) (keywordp (first lst))))

(defun %plist->alist (plist)
  "Convert keyword plist into an alist."
  (let ((out nil))
    (loop for (k v) on plist by #'cddr
          do (push (cons k v) out))
    (nreverse out)))

(defun %normalize-key-name (key)
  "Normalize alist/plist key into a lowercase string with '-' separators."
  (let* ((raw (cond
                ((stringp key) key)
                ((symbolp key) (symbol-name key))
                (t nil))))
    (when raw
      (let* ((trimmed (string-left-trim ":" raw))
             (lower (string-downcase trimmed)))
        (substitute #\- #\_ lower)))))

(defun %alist-val-normalized (alist keys &optional default)
  "Return first matching value from ALIST by normalized key list."
  (let ((needles (remove nil (mapcar #'%normalize-key-name keys))))
    (or (loop for (k . v) in alist
              for nk = (%normalize-key-name k)
              when (and nk (member nk needles :test #'string=))
                do (return v))
        default)))

(defun %result-val-normalized (result keys &optional default)
  "Return value from RESULT (alist or plist) by normalized key list."
  (cond
    ((%plist-p result)
     (%alist-val-normalized (%plist->alist result) keys default))
    (t
     (%alist-val-normalized result keys default))))

(defun %normalize-bool (value &optional default)
  "Normalize truthy/falsey representations into T or NIL."
  (cond
    ((null value) default)
    ((eq value t) t)
    ((eq value 'nil) nil)
    ((numberp value) (not (zerop value)))
    ((symbolp value)
     (let ((s (string-downcase (symbol-name value))))
       (cond
         ((member s '("t" "true" "yes" "y" "1") :test #'string=) t)
         ((member s '("nil" "false" "no" "n" "0") :test #'string=) nil)
         (t default))))
    ((stringp value)
     (let ((s (string-downcase value)))
       (cond
         ((member s '("t" "true" "yes" "y" "1") :test #'string=) t)
         ((member s '("nil" "false" "no" "n" "0" "") :test #'string=) nil)
         (t default))))
    (t default)))

(defun %result-strategy-name (result)
  "Extract strategy name from result (alist or plist)."
  (cond
    ((%plist-p result)
     (or (getf result :strategy_name)
         (getf result :strategy-name)
         (getf result :name)))
    (t
     (%alist-val result '(strategy_name :strategy_name strategy-name :strategy-name name :name) nil))))

(defun %preview-msg (msg &optional (limit 220))
  "Return a sanitized preview string for logs."
  (let* ((raw (if (and msg (> (length msg) limit)) (subseq msg 0 limit) msg))
         (clean (if raw (substitute #\Space #\Newline (substitute #\Space #\Return raw)) "")))
    clean))

(defparameter *missing-strategy-name-log-limit* 3)
(defparameter *missing-strategy-name-log-count* 0)
(defparameter *json-ignored-log-limit* 20)
(defparameter *json-ignored-log-count* 0)
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

(defun %as-count (value)
  "Normalize VALUE into a non-negative integer count."
  (let ((n (cond
             ((numberp value) value)
             ((stringp value) (or (swimmy.core:safe-parse-number value) 0))
             (t 0))))
    (max 0 (truncate n))))

(defun %find-strategy-in-memory (name)
  "Find strategy object by NAME from KB/evolved caches."
  (when (and (stringp name) (not (%invalid-name-p name)))
    (or (find name swimmy.school::*strategy-knowledge-base*
              :key #'swimmy.school:strategy-name :test #'string=)
        (find name swimmy.globals:*evolved-strategies*
              :key #'swimmy.school:strategy-name :test #'string=))))

(defun %normalize-rank-token (rank)
  "Normalize rank value into uppercase token string."
  (let ((raw (cond
               ((null rank) nil)
               ((stringp rank) rank)
               ((symbolp rank) (symbol-name rank))
               (t (format nil "~a" rank)))))
    (when raw
      (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return) raw)))))

(defun %active-rank-token-p (rank)
  "Return T if rank token is active for CPCV notifications."
  (let ((token (%normalize-rank-token rank)))
    (and token
         (member token '("B" ":B" "A" ":A" "S" ":S" "LEGEND" ":LEGEND")
                 :test #'string=))))

(defun %strategy-active-in-memory-p (strat)
  "Return T when strategy object STRAT has an active rank."
  (and strat
       (handler-case
           (%active-rank-token-p (swimmy.school:strategy-rank strat))
         (error () nil))))

(defun %strategy-active-in-db-p (name)
  "Return T if strategy NAME exists in DB with an active rank."
  (and (stringp name)
       (not (%invalid-name-p name))
       (handler-case
           (and (fboundp 'swimmy.school::execute-single)
                (%active-rank-token-p
                 (swimmy.school::execute-single
                  "SELECT rank FROM strategies WHERE name = ? ORDER BY updated_at DESC LIMIT 1"
                  name)))
         (error () nil))))

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
                          (result (if (and (listp result)
                                           (not (%result-val-normalized result '(strategy_name strategy-name name) nil))
                                           (listp (car result)))
                                      (car result)
                                      result))
                          (name (%normalize-strategy-name
                                 (%result-val-normalized result '(strategy_name strategy-name name) nil)))
                          (request-id (%normalize-strategy-name
                                       (%result-val-normalized result '(request_id request-id) nil)))
                          (trade-list (%result-val-normalized result '(trade_list trade-list) nil))
                          (trades-truncated (%result-val-normalized result '(trades_truncated trades-truncated) nil))
                          (trades-ref (%result-val-normalized result '(trades_ref trades-ref) nil))
                          (median (%result-val-normalized result '(median_sharpe median-sharpe median) 0.0))
                          (median-pf (%result-val-normalized result '(median_pf median-pf) 0.0))
                          (median-wr (%result-val-normalized result '(median_wr median-wr) 0.0))
                          (median-maxdd (%result-val-normalized result '(median_maxdd median-maxdd) 0.0))
                          (missing-marker (cons :missing nil))
                          (paths-raw (%result-val-normalized result '(path_count path-count paths) missing-marker))
                          (passed-raw (%result-val-normalized result '(passed_count passed-count passed) missing-marker))
                          (failed-raw (%result-val-normalized result '(failed_count failed-count failed) missing-marker))
                          (counts-present-p (or (not (eq paths-raw missing-marker))
                                                (not (eq passed-raw missing-marker))
                                                (not (eq failed-raw missing-marker))))
                          (paths-count (%as-count (if (eq paths-raw missing-marker) 0 paths-raw)))
                          (passed-count (%as-count (if (eq passed-raw missing-marker) 0 passed-raw)))
                          (failed-count (%as-count (if (eq failed-raw missing-marker) 0 failed-raw)))
                          (pass-rate (%result-val-normalized result '(pass_rate pass-rate) 0.0))
                          (is-passed (%normalize-bool
                                      (%result-val-normalized result '(is_passed is-passed passed_ok) nil)
                                      nil))
                          (error-msg (%result-val-normalized result '(error err error_msg) nil))
                          (empty-result-p (and counts-present-p
                                               (zerop paths-count)
                                               (zerop passed-count)
                                               (zerop failed-count)))
                          (runtime-error-msg (or error-msg
                                                 (when empty-result-p
                                                   "empty CPCV result: path_count/passed_count/failed_count are all zero")))
                          (strat (%find-strategy-in-memory name))
                          (strat-active-p (%strategy-active-in-memory-p strat))
                          (known-strategy-p (or strat-active-p (%strategy-active-in-db-p name))))
                     (let ((result-plist (list :strategy-name name :median-sharpe median
                                               :median-pf median-pf :median-wr median-wr
                                               :median-maxdd median-maxdd
                                               :path-count paths-count :passed-count passed-count
                                               :failed-count failed-count :pass-rate pass-rate
                                               :is-passed is-passed :request-id request-id
                                               :error runtime-error-msg
                                               :trades-truncated trades-truncated
                                               :trades-ref trades-ref)))
                       (when (and trade-list name)
                         (swimmy.school:record-backtest-trades request-id name "CPCV" trade-list))
                       (when (fboundp 'swimmy.school::%cpcv-metric-inc)
                         (swimmy.school::%cpcv-metric-inc :received)
                         (cond
                           (runtime-error-msg
                            (swimmy.school::%cpcv-metric-inc :result_runtime_failed)
                            (swimmy.school::%cpcv-metric-inc :result_failed))
                           ((not is-passed)
                            (swimmy.school::%cpcv-metric-inc :result_criteria_failed)
                            (swimmy.school::%cpcv-metric-inc :result_failed)))
                         (when (fboundp 'swimmy.school::write-cpcv-status-file)
                           (ignore-errors (swimmy.school::write-cpcv-status-file :reason "result"))))
                       (when known-strategy-p
                         (swimmy.core:notify-cpcv-result result-plist))
                       (push result-plist swimmy.globals:*cpcv-results-buffer*)
                       (let ((count (length swimmy.globals:*cpcv-results-buffer*))
                             (expected swimmy.globals:*expected-cpcv-count*))
                         (when (and (> expected 0)
                                    (>= count (max 1 (floor (* expected 0.9)))))
                           (swimmy.core:notify-cpcv-summary)))
                       (cond
                           ((and strat strat-active-p (not runtime-error-msg))
                            (setf (swimmy.school:strategy-cpcv-median-sharpe strat) median)
                            (setf (swimmy.school:strategy-cpcv-median-pf strat) median-pf)
                            (setf (swimmy.school:strategy-cpcv-median-wr strat) median-wr)
                            (setf (swimmy.school:strategy-cpcv-median-maxdd strat) median-maxdd)
                            (setf (swimmy.school:strategy-cpcv-pass-rate strat) pass-rate)
                            (swimmy.school:upsert-strategy strat)
                            (when is-passed
                              (if (swimmy.school:check-rank-criteria strat :S)
                                  (swimmy.school:ensure-rank strat :S
                                                             "CPCV Passed and Criteria Met")
                                  (format t "[CPCV] Strategy ~a passed CPCV but failed overall S-Rank criteria.~%"
                                          name))))
                           ((and name (not runtime-error-msg)
                                 (fboundp 'swimmy.school::update-cpcv-metrics-by-name))
                           (swimmy.school::update-cpcv-metrics-by-name
                             name median median-pf median-wr median-maxdd pass-rate
                             :request-id request-id)))
                       (when (and (not known-strategy-p)
                                  (not (%invalid-name-p name)))
                         (format t "[CPCV] ℹ️ Unknown strategy result (alert suppressed): ~a req=~a~%"
                                 name (or request-id "N/A"))))))
                  ((string= type-str swimmy.core:+MSG-TICK+)
                   (let* ((symbol (%alist-val sexp '(symbol :symbol) ""))
                          (bid (%alist-val sexp '(bid :bid) nil))
                          (ask (%alist-val sexp '(ask :ask) nil))
                          (symbol-str (if (symbolp symbol) (symbol-name symbol) symbol)))
                     (when bid
                       (setf swimmy.globals:*last-guardian-heartbeat* (get-universal-time))
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
                  ((string= type-str "STATUS_NOW")
                   (let* ((symbol (%alist-val sexp '(symbol :symbol) ""))
                          (bid (%alist-val sexp '(bid :bid) 0.0))
                          (bid-num (cond
                                     ((numberp bid) (float bid))
                                     ((stringp bid)
                                      (let ((n (ignore-errors (swimmy.core:safe-parse-number bid))))
                                        (and (numberp n) (float n))))
                                     (t nil)))
                          (symbol-str (if (symbolp symbol) (symbol-name symbol) symbol)))
                     (when (and symbol-str (fboundp 'swimmy.shell:send-periodic-status-report))
                       (format t "[DISPATCH] 📥 STATUS_NOW recv: symbol=~a bid=~a~%"
                               symbol-str (or bid-num bid))
                       ;; Bypass per-symbol status throttle for explicit immediate status requests.
                       (when (hash-table-p swimmy.globals:*last-status-notification-time*)
                         (remhash symbol-str swimmy.globals:*last-status-notification-time*))
                       ;; Keep status freshness aligned with explicit "now" trigger.
                       (when (and bid-num (fboundp 'swimmy.main:update-candle))
                         (swimmy.main:update-candle bid-num symbol-str))
                       ;; Force immediate status emission even outside regular market hours.
                       (swimmy.shell:send-periodic-status-report symbol-str (or bid-num 0.0) t))))
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
                   (let* ((symbol-raw (%alist-val sexp '(symbol :symbol) ""))
                          (symbol (cond
                                    ((symbolp symbol-raw) (string-upcase (symbol-name symbol-raw)))
                                    ((stringp symbol-raw) (string-upcase symbol-raw))
                                    (t symbol-raw)))
                          (tf-raw (%alist-val sexp '(tf :tf) "M1"))
                          (tf (if (symbolp tf-raw) (symbol-name tf-raw) tf-raw))
                          (data (%alist-val sexp '(data :data) nil))
                          (candles (remove nil (mapcar #'%sexp-candle->struct (or data '()))))
                          (candles (sort candles #'> :key #'swimmy.globals:candle-timestamp)))
                     (when (and symbol candles)
                       ;; Migrate symbol-keyed history entries to canonical string keys.
                       (when (symbolp symbol-raw)
                         (remhash symbol-raw swimmy.globals:*candle-histories*)
                         (remhash symbol-raw swimmy.globals:*candle-histories-tf*))
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
            (let ((json (handler-case (jsown:parse msg) (error () nil))))
              (if json
                  (let* ((type (or (%json-val json '("type") "") ""))
                         (type-str (cond ((stringp type) type)
                                         ((symbolp type) (symbol-name type))
                                         (t ""))))
                    (cond
                      ((string-equal type-str "BACKTEST_RESULT")
                       (let ((sexp (%json->sexp-backtest json)))
                         (if sexp
                             (return-from internal-process-msg
                               (internal-process-msg (swimmy.core:encode-sexp sexp)))
                             (progn
                               (format t "[DISPATCH] ⚠️ JSON BACKTEST_RESULT missing result~%")
                               nil))))
                      (t
                       (let ((detail-p (< *json-ignored-log-count* *json-ignored-log-limit*)))
                         (when detail-p (incf *json-ignored-log-count*))
                         (if detail-p
                             (format t "[DISPATCH] ⚠️ JSON payload ignored (type=~a) head=~a~%"
                                     type-str (%preview-msg msg))
                             (format t "[DISPATCH] ⚠️ JSON payload ignored (type=~a)~%" type-str)))
                       nil)))
                  (let ((detail-p (< *json-ignored-log-count* *json-ignored-log-limit*)))
                    (when detail-p (incf *json-ignored-log-count*))
                    (if detail-p
                        (format t "[DISPATCH] ⚠️ JSON payload ignored (parse failed) head=~a~%"
                                (%preview-msg msg))
                        (format t "[DISPATCH] ⚠️ JSON payload ignored (parse failed)~%"))
                    nil)))))
    (when err
      (format t "[L] Msg Error: ~a" err)
      nil)
    result))
(defun process-msg (msg) (internal-process-msg msg))
