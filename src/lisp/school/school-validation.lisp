;;; school-validation.lisp
;;; ============================================================================
;;; P9: OUT-OF-SAMPLE VALIDATION (CPCV Lite)
;;; Extracted from school-backtest.lisp for SRP compliance
;;; ============================================================================
;;; Purpose: Validate strategy on unseen data before A-RANK promotion
;;; Method: Split CSV data into 70% train / 30% test, run backtest on test portion
;;; ============================================================================

(in-package :swimmy.school)

(defparameter *oos-test-ratio* 0.3
  "Ratio of data to use for out-of-sample testing (30%)")

(defparameter *oos-min-sharpe* 0.3
  "Minimum Sharpe ratio required to pass OOS validation for A-RANK")

(defparameter *oos-pending* nil
  "Deprecated (use SQLite oos_queue). Kept for backward compatibility.")

(defparameter *oos-request-interval* 600
  "Minimum seconds between OOS requests per strategy.")

(defparameter *oos-metrics* (make-hash-table :test 'equal)
  "Simple counters and latency stats for OOS. Keys: :sent :success :failure :retry :latency-count :latency-sum :latency-min :latency-max.")

(defparameter *oos-failure-stats* (list :data-invalid 0 :send-failure 0 :db-error 0)
  "Per-cycle failure counters for OOS pipeline.")

(defparameter *oos-status-path* (swimmy.core::swimmy-path "data/reports/oos_status.txt")
  "Path for OOS status summary report.")

;; CPCV Metrics
(defparameter *cpcv-metrics* (make-hash-table :test 'equal)
  "Counters for CPCV pipeline. Keys: :queued :sent :received :failed.")
(defparameter *cpcv-status-path* (swimmy.core::swimmy-path "data/reports/cpcv_status.txt")
  "Path for CPCV status summary report.")

(defun %cpcv-metric-inc (key &optional (delta 1))
  (incf (gethash key *cpcv-metrics* 0) delta))

(defun %cpcv-metric-set (key value)
  (setf (gethash key *cpcv-metrics* 0) value))

(defun reset-cpcv-metrics (&key (queued 0))
  "Reset CPCV counters for a new batch."
  (clrhash *cpcv-metrics*)
  (setf (gethash :queued *cpcv-metrics*) queued
        (gethash :sent *cpcv-metrics*) 0
        (gethash :received *cpcv-metrics*) 0
        (gethash :failed *cpcv-metrics*) 0))

(defun cpcv-metrics-summary-line ()
  "Build a one-line CPCV status summary."
  (format nil "~d queued | ~d sent | ~d received | ~d failed"
          (gethash :queued *cpcv-metrics* 0)
          (gethash :sent *cpcv-metrics* 0)
          (gethash :received *cpcv-metrics* 0)
          (gethash :failed *cpcv-metrics* 0)))

(defun write-cpcv-status-file (&key (reason "event"))
  "Persist CPCV status summary to file."
  (handler-case
      (let* ((line (cpcv-metrics-summary-line))
             (ts (get-universal-time))
             (stamp (if (fboundp 'format-timestamp)
                        (format-timestamp ts)
                        (format nil "~a" ts)))
             (content (format nil "~a~%updated: ~a reason: ~a" line stamp reason)))
        (ensure-directories-exist *cpcv-status-path*)
        (with-open-file (out *cpcv-status-path* :direction :output :if-exists :supersede :if-does-not-exist :create)
          (write-string content out))
        t)
    (error (e)
      (format t "[CPCV] ⚠️ Failed to write cpcv_status.txt: ~a~%" e)
      nil)))

(defun cpcv-elite-p (strategy)
  "Return T if strategy is elite enough for CPCV dispatch (Sharpe>=S threshold)."
  (let* ((criteria (get-rank-criteria :S))
         (min-sharpe (getf criteria :sharpe-min 0.5))
         (s-sharpe (or (strategy-sharpe strategy) 0.0)))
    (>= s-sharpe min-sharpe)))

(defun cpcv-gate-failure-counts (strategies)
  "Count CPCV elite-gate failures and S-base criteria distribution for A-rank strategies."
  (let ((total 0) (pass 0) (sharpe 0) (pf 0) (wr 0) (maxdd 0))
    (dolist (s (or strategies '()))
      (incf total)
      (let* ((s-pf (or (strategy-profit-factor s) 0.0))
             (s-wr (or (strategy-win-rate s) 0.0))
             (s-maxdd (or (strategy-max-dd s) 1.0))
             (elite (cpcv-elite-p s)))
        (when (not elite) (incf sharpe))
        (when (< s-pf 1.5) (incf pf))
        (when (< s-wr 0.45) (incf wr))
        (when (>= s-maxdd 0.15) (incf maxdd))
        (when elite (incf pass))))
    (list :total total :pass pass :sharpe sharpe :pf pf :wr wr :maxdd maxdd)))

(defun cpcv-gate-failure-summary (counts)
  "Format gate failure counts for logs."
  (format nil "[CPCV] ℹ️ No A-RANK elites ready for CPCV (Sharpe gate: sharpe<0.5=~d | S-base: pf<1.5=~d wr<0.45=~d maxdd>=0.15=~d elite=~d total=~d)"
          (getf counts :sharpe 0)
          (getf counts :pf 0)
          (getf counts :wr 0)
          (getf counts :maxdd 0)
          (getf counts :pass 0)
          (getf counts :total 0)))

(defun cpcv-median-failure-counts (strategies)
  "Count CPCV median failures for S criteria (PF/WR/MaxDD)."
  (let* ((criteria (get-rank-criteria :S))
         (total 0) (pf 0) (wr 0) (maxdd 0))
    (dolist (s (or strategies '()))
      (when (and (strategy-cpcv-median-sharpe s)
                 (> (strategy-cpcv-median-sharpe s) 0.0))
        (incf total)
        (when (< (or (strategy-cpcv-median-pf s) 0.0) (getf criteria :cpcv-pf-min 1.5))
          (incf pf))
        (when (< (or (strategy-cpcv-median-wr s) 0.0) (getf criteria :cpcv-wr-min 0.45))
          (incf wr))
        (when (>= (or (strategy-cpcv-median-maxdd s) 1.0) (getf criteria :cpcv-maxdd-max 0.15))
          (incf maxdd))))
    (list :total total :pf pf :wr wr :maxdd maxdd)))

(defun %oos-metric-inc (key &optional (delta 1))
  (incf (gethash key *oos-metrics* 0) delta))

(defun %oos-latency-record (seconds)
  (let* ((cnt (1+ (gethash :latency-count *oos-metrics* 0)))
         (sum (+ (gethash :latency-sum *oos-metrics* 0.0) seconds))
         (mn (if (and (gethash :latency-min *oos-metrics*) (> (gethash :latency-count *oos-metrics* 0) 0))
                 (min (gethash :latency-min *oos-metrics*) seconds)
                 seconds))
         (mx (max (gethash :latency-max *oos-metrics* 0.0) seconds)))
    (setf (gethash :latency-count *oos-metrics*) cnt
          (gethash :latency-sum *oos-metrics*) sum
          (gethash :latency-min *oos-metrics*) mn
          (gethash :latency-max *oos-metrics*) mx)))

(defun %oos-failure-inc (key &optional (delta 1))
  (let ((cur (getf *oos-failure-stats* key 0)))
    (setf (getf *oos-failure-stats* key) (+ cur delta))))

(defun reset-oos-failure-stats ()
  (setf *oos-failure-stats* (list :data-invalid 0 :send-failure 0 :db-error 0)))

(defun report-oos-failure-stats ()
  *oos-failure-stats*)

(defun build-oos-status-line ()
  "Build a one-line OOS status summary."
  (oos-metrics-summary-line))

(defun write-oos-status-file (&key (reason "event"))
  "Persist OOS status summary to file."
  (handler-case
      (let* ((line (build-oos-status-line))
             (ts (get-universal-time))
             (stamp (if (fboundp 'format-timestamp)
                        (format-timestamp ts)
                        (format nil "~a" ts)))
             (content (format nil "~a~%updated: ~a reason: ~a" line stamp reason)))
        (ensure-directories-exist *oos-status-path*)
        (with-open-file (out *oos-status-path* :direction :output :if-exists :supersede :if-does-not-exist :create)
          (write-string content out))
        t)
    (error (e)
      (format t "[OOS] ⚠️ Failed to write oos_status.txt: ~a~%" e)
      nil)))

(defun %numeric-string-p (s)
  "Return T if S cleanly parses to a number without trailing junk."
  (not (null (swimmy.core:safe-parse-number s))))

(defun validate-oos-data-file (path)
  "Lightweight CSV gate: ensure basic schema/row is present."
  (handler-case
      (progn
        (unless (probe-file path)
          (return-from validate-oos-data-file (values nil "missing data file")))
        (let ((size (ignore-errors (with-open-file (s path :direction :input) (file-length s)))))
          (when (or (null size) (<= size 0))
            (return-from validate-oos-data-file (values nil "empty file"))))
        (with-open-file (s path :direction :input)
          (let ((header (read-line s nil nil)))
            (unless header
              (return-from validate-oos-data-file (values nil "empty file")))
            (let* ((cols (mapcar #'string-downcase
                                 (remove "" (split-sequence:split-sequence #\, header) :test #'string=)))
                   (required '("timestamp" "open" "high" "low" "close")))
              (unless (every (lambda (c) (member c cols :test #'string=)) required)
                (return-from validate-oos-data-file (values nil "missing required columns")))
              (let ((row (read-line s nil nil)))
                (unless row
                  (return-from validate-oos-data-file (values nil "no data rows")))
                (let ((cells (remove "" (split-sequence:split-sequence #\, row) :test #'string=)))
                  (unless (>= (length cells) 5)
                    (return-from validate-oos-data-file (values nil "row missing columns")))
                  (unless (every #'%numeric-string-p (subseq cells 0 5))
                    (return-from validate-oos-data-file (values nil "non-numeric data")))
                  (values t nil)))))))
    (error (e) (values nil (format nil "validation error: ~a" e)))))

(defun maybe-request-oos-backtest (strat)
  "Dispatch async OOS backtest if not requested recently. Returns request-id or NIL."
  (ignore-errors (init-db))
  (let* ((name (strategy-name strat))
         (now (get-universal-time))
         (req-id nil)
         (last-req (handler-case
                       (multiple-value-list (lookup-oos-request name))
                     (error (e)
                       (%oos-failure-inc :db-error)
                       (format t "[OOS] ⚠️ lookup-oos-request failed (~a); recreating queue~%" e)
                       (init-db)
                       (multiple-value-list (lookup-oos-request name)))))
         (last-id (first last-req))
         (last-time (second last-req))
         (last-status (third last-req))
         (age (and last-time (- now last-time))))
    (cond
      ;; Throttle if a recent non-error request is still pending
      ((and age (<= age *oos-request-interval*) (not (and (stringp last-status) (string= last-status "error"))))
      (format t "[OOS] ⏳ Request throttled for ~a (age ~ds)~%" name age)
      (let ((data (jsown:new-js
                    ("request_id" last-id)
                    ("oos_kind" "a-rank")
                    ("status" "throttled")
                    ("age_sec" age))))
        (swimmy.core::emit-telemetry-event "oos.throttled"
          :service "school"
          :severity "info"
          :correlation-id last-id
          :data data))
      nil)
      (t
       (setf req-id (swimmy.core:generate-uuid))
       (let ((status (if last-id "retry" "sent")))
         (handler-case
             (progn
               (enqueue-oos-request name req-id :status status :requested-at now)
               (request-backtest strat :suffix "-OOS" :request-id req-id)
               (%oos-metric-inc (if last-id :retry :sent))
               (format t "[OOS] 📤 Dispatched OOS backtest for ~a (~a req ~a)~%" name status req-id)
               (let ((data (jsown:new-js
                             ("request_id" req-id)
                             ("oos_kind" "a-rank")
                             ("status" status))))
                 (swimmy.core::emit-telemetry-event "oos.requested"
                   :service "school"
                   :severity "info"
                   :correlation-id req-id
                   :data data))
               (ignore-errors (write-oos-status-file :reason status))
               req-id)
           (error (e)
             (%oos-failure-inc :send-failure)
             (record-oos-error name req-id (format nil "~a" e))
             (format t "[OOS] ❌ Failed to dispatch OOS for ~a (~a)~%" name e)
             (let ((data (jsown:new-js
                           ("request_id" req-id)
                           ("oos_kind" "a-rank")
                           ("status" "error")
                           ("error" (format nil "~a" e)))))
               (swimmy.core::emit-telemetry-event "oos.dispatch_failed"
                 :service "school"
                 :severity "error"
                 :correlation-id req-id
                 :data data))
             nil)))))))

(defun handle-oos-backtest-result (name metrics)
  "Apply OOS backtest result to strategy and attempt promotion."
  (let* ((req-id (getf metrics :request-id))
         (queued-id nil))
    (handler-case
        (multiple-value-bind (qid _req-at _status) (lookup-oos-request name)
          (declare (ignore _req-at _status))
          (setf queued-id qid))
      (error (e)
        (%oos-failure-inc :db-error)
        (format t "[OOS] ⚠️ lookup-oos-request failed while handling result for ~a (~a)~%" name e)))
    (unless (and (stringp req-id) (stringp queued-id) (string= queued-id req-id))
      (format t "[OOS] ⚠️ Stale OOS result ignored for ~a (req ~a, latest ~a)~%"
              name req-id queued-id)
      (return-from handle-oos-backtest-result nil)))
  (let ((strat (find-strategy name)))
    (unless strat
      (%oos-metric-inc :failure)
      (record-oos-error name (getf metrics :request-id) "Strategy not found for OOS result")
      (return-from handle-oos-backtest-result nil))
    (when strat
      (let ((sharpe (float (or (getf metrics :sharpe) 0.0)))
            (req-id (getf metrics :request-id)))
        (setf (strategy-oos-sharpe strat) sharpe)
        ;; Queue bookkeeping
        (handler-case (complete-oos-request name req-id)
          (error (e) (record-oos-error name req-id (format nil "~a" e))))
        (%oos-metric-inc :success)
        (when (getf metrics :oos-latency)
          (%oos-latency-record (getf metrics :oos-latency)))
        (upsert-strategy strat)
        (format t "[OOS] 📥 ~a OOS Sharpe=~,2f (req ~a)~%" name sharpe req-id)
        (let* ((latency (getf metrics :oos-latency))
               (data (jsown:new-js
                       ("request_id" req-id)
                       ("oos_kind" "a-rank")
                       ("status" "received"))))
          (when latency
            (setf (jsown:val data "latency_sec") latency))
          (swimmy.core::emit-telemetry-event "oos.result"
            :service "school"
            :severity "info"
            :correlation-id req-id
            :data data))
        ;; Attempt promotion if core metrics are already strong enough
        (when (meets-a-rank-criteria strat)
          (if (>= sharpe *oos-min-sharpe*)
              (promote-rank strat :A (format nil "OOS Validated: Sharpe=~,2f" sharpe))
              (format t "[OOS] ❌ ~a failed OOS Sharpe (~,2f < ~,2f)~%" name sharpe *oos-min-sharpe*)))
        (ignore-errors (write-oos-status-file :reason "result"))))))

(defun report-oos-metrics ()
  "Snapshot of OOS counters and latency stats (for narrative/reporting)."
  (let* ((cnt (gethash :latency-count *oos-metrics* 0))
         (sum (gethash :latency-sum *oos-metrics* 0.0)))
    (list :sent (gethash :sent *oos-metrics* 0)
          :retry (gethash :retry *oos-metrics* 0)
          :success (gethash :success *oos-metrics* 0)
          :failure (gethash :failure *oos-metrics* 0)
          :latency-count cnt
          :latency-avg (if (> cnt 0) (/ sum cnt) 0.0)
          :latency-min (gethash :latency-min *oos-metrics* nil)
          :latency-max (gethash :latency-max *oos-metrics* nil))))

(defun run-oos-validation (strat)
  "Run Out-of-Sample validation for A-RANK promotion candidates.
   Returns (values passed-p oos-sharpe message)"
  (let* ((symbol (or (strategy-symbol strat) "USDJPY"))
         (data-file (format nil "~a" (swimmy.core::swimmy-path (format nil "data/historical/~a_M1.csv" symbol)))))

    (unless (probe-file data-file)
      (%oos-failure-inc :data-invalid)
      (%oos-metric-inc :failure)
      (return-from run-oos-validation
        (values nil 0.0 (format nil "No data file for ~a" symbol))))

    ;; Data quality gate (fast, non-blocking)
    (multiple-value-bind (valid reason) (validate-oos-data-file data-file)
      (unless valid
        (%oos-failure-inc :data-invalid)
        (%oos-metric-inc :failure)
        (return-from run-oos-validation
          (values nil 0.0 (format nil "Invalid data file (~a)" reason)))))

    (let ((oos (strategy-oos-sharpe strat)))
      (when (numberp oos)
        (return-from run-oos-validation
          (values (>= oos *oos-min-sharpe*)
                  oos
                  (format nil "OOS cached: Sharpe=~,2f" oos)))))
    
    (let ((req (maybe-request-oos-backtest strat)))
      (if req
          (values nil 0.0 "OOS pending (async)")
          (values nil 0.0 "OOS request throttled")))))

(defun validate-for-a-rank-promotion (strat)
  "Validate strategy is ready for A-RANK promotion using OOS validation.
   Returns T if strategy passes all A-RANK criteria including OOS."
  (format t "[A-RANK] 🎯 Validating ~a for A-RANK promotion...~%"
          (strategy-name strat))
  
  ;; Check basic rank criteria first (without OOS gate)
  (unless (check-rank-criteria strat :A :include-oos nil)
    (format t "[A-RANK] ❌ ~a: Base A-RANK metrics failed (S=~,2f PF=~,2f WR=~,1f%% DD=~,1f%%)~%"
            (strategy-name strat)
            (or (strategy-sharpe strat) 0.0)
            (or (strategy-profit-factor strat) 0.0)
            (* 100 (or (strategy-win-rate strat) 0.0))
            (* 100 (or (strategy-max-dd strat) 1.0)))
    (return-from validate-for-a-rank-promotion nil))
  
  ;; Run OOS validation (async)
  (multiple-value-bind (passed sharpe msg) (run-oos-validation strat)
    (format t "[A-RANK] ~a ~a: ~a~%" 
            (if passed "✅" "⏳") (strategy-name strat) msg)
    (when passed
      (promote-rank strat :A (format nil "OOS Validated: Sharpe=~,2f" sharpe)))
    passed))

(defun meets-a-rank-criteria (strat)
  "Check if strategy meets basic A-RANK criteria (without OOS)."
  (let ((sharpe (or (strategy-sharpe strat) 0.0))
        (pf (or (strategy-profit-factor strat) 0.0))
        (wr (or (strategy-win-rate strat) 0.0))
        (maxdd (or (strategy-max-dd strat) 1.0)))
    (and (>= sharpe 0.3)
         (>= pf 1.2)
         (>= wr 0.4)
         (< maxdd 0.2))))

;;; ============================================================================
;;; P12: CPCV LISP-RUST INTEGRATION
;;; ============================================================================

(defun request-cpcv-validation (strat)
  "V49.5: Request CPCV validation (Non-blocking ASYNC).
   Result is handled by message-dispatcher.lisp"
   (let* ((symbol (or (strategy-symbol strat) "USDJPY"))
          (candles-file (format nil "~a" (swimmy.core::swimmy-path (format nil "data/historical/~a_M1.csv" symbol))))
          (strat-params (strategy-to-alist strat))
          (req-id (swimmy.core:generate-uuid))
          (payload `((action . "CPCV_VALIDATE")
                     (strategy_name . ,(strategy-name strat))
                     (symbol . ,symbol)
                     (candles_file . ,candles-file)
                     (request_id . ,req-id)
                     (strategy_params . ,strat-params))))
     (format t "[CPCV] 📤 Sent CPCV request for ~a (Async, req ~a)...~%" (strategy-name strat) req-id)
     (send-zmq-msg (swimmy.core::sexp->string payload :package *package*) :target :cmd)
     ;; Return T to indicate request sent successfully
     (values t nil nil)))

(defun validate-for-s-rank-promotion (strat)
  "Validate strategy is ready for S-RANK promotion using true CPCV.
   S-RANK criteria: IS Sharpe >= 0.5
   Plus: CPCV validation must pass (median Sharpe >= 0.5, 50%+ paths passing),
         and CPCV median PF/WR/MaxDD must satisfy S thresholds.
   Returns T if strategy passes all S-RANK criteria including CPCV."
  (format t "[S-RANK] 🏆 Validating ~a for S-RANK promotion...~%"
          (strategy-name strat))
  
  ;; Check IS Sharpe gate first
  (let ((sharpe (or (strategy-sharpe strat) 0.0)))
    (unless (>= sharpe 0.5)
      (format t "[S-RANK] ❌ ~a: Does not meet IS Sharpe gate (S=~,2f)~%"
              (strategy-name strat) sharpe)
      (return-from validate-for-s-rank-promotion nil)))
  
  ;; Run CPCV validation via Guardian
  (multiple-value-bind (passed result error-msg) (request-cpcv-validation strat)
    (cond
      (error-msg
       (format t "[S-RANK] ⚠️ ~a: CPCV error - ~a~%" (strategy-name strat) error-msg)
       nil)
      (passed
       (let ((median (getf result :median-sharpe))
             (median-pf (getf result :median-pf))
             (median-wr (getf result :median-wr))
             (median-maxdd (getf result :median-maxdd))
             (pass-rate (getf result :pass-rate)))
         (setf (strategy-cpcv-median-sharpe strat) median)
         (setf (strategy-cpcv-median-pf strat) median-pf)
         (setf (strategy-cpcv-median-wr strat) median-wr)
         (setf (strategy-cpcv-median-maxdd strat) median-maxdd)
         (setf (strategy-cpcv-pass-rate strat) pass-rate)
         (upsert-strategy strat)
         (format t "[S-RANK] ✅ ~a: CPCV PASSED (median Sharpe=~,3f, ~d/~d paths)~%"
                 (strategy-name strat)
                 median
                 (getf result :passed-count)
                 (getf result :path-count))
         (if (check-rank-criteria strat :S)
             t
             (progn
               (format t "[S-RANK] ❌ ~a: CPCV medians failed S criteria (PF=~,2f WR=~,2f DD=~,2f)~%"
                       (strategy-name strat)
                       (or median-pf 0.0)
                       (or median-wr 0.0)
                       (or median-maxdd 1.0))
               nil))))
      (t
       (format t "[S-RANK] ❌ ~a: CPCV FAILED (median Sharpe=~,3f)~%"
               (strategy-name strat) (getf result :median-sharpe))
       nil))))

;;; ============================================================================
;;; V48.0: BATCH CPCV VALIDATION TRIGGERS
;;; Called from evolution loop to process A-RANK strategies
;;; ============================================================================

(defparameter *cpcv-batch-size* 20
  "V48.1: Number of A-RANK strategies to validate per cycle (Expert Panel P1: 5→20)")

(defparameter *last-cpcv-cycle* 0
  "Timestamp of last CPCV cycle")

(defparameter *cpcv-cycle-interval* 60
  "V49.6: High-Velocity Validation (300s -> 60s) for rapid S-Rank creation")

(defun fetch-cpcv-candidates (&optional strategies)
  "Fetch CPCV candidates from DB (A-rank, elite by Sharpe threshold)."
  (let* ((source (or strategies
                     (fetch-candidate-strategies :min-sharpe 0.0 :ranks '(":A")))))
    (remove-if-not
     (lambda (s)
       (and (eq (strategy-rank s) :A)
            (cpcv-elite-p s)))
     source)))

(defun run-a-rank-cpcv-batch ()
  "V48.0: Batch process A-RANK strategies for S-RANK promotion via CPCV.
   Called from evolution loop. Processes up to *cpcv-batch-size* at a time."
  (let ((now (get-universal-time)))
    ;; Rate limit CPCV cycles
    (when (< (- now *last-cpcv-cycle*) *cpcv-cycle-interval*)
      (return-from run-a-rank-cpcv-batch nil))
    (setf *last-cpcv-cycle* now)
    ;; Get A-RANK strategies from DB (truth source) and filter by S-base criteria
    (let* ((a-rank-db (fetch-candidate-strategies :min-sharpe 0.0 :ranks '(":A")))
           (a-rank-strategies (fetch-cpcv-candidates a-rank-db))
           ;; Shuffle to avoid getting stuck on the same failing strategies if pool is large
           (shuffled (sort (copy-list a-rank-strategies)
                           (lambda (a b) (declare (ignore a b)) (< (random 100) 50))))
           (batch (if (> (length shuffled) *cpcv-batch-size*)
                      (subseq shuffled 0 *cpcv-batch-size*)
                      shuffled)))
      (when (null batch)
        (reset-cpcv-metrics :queued 0)
        (let ((counts (cpcv-gate-failure-counts a-rank-db)))
          (format t "~a~%" (cpcv-gate-failure-summary counts)))
        (ignore-errors (write-cpcv-status-file :reason "no-candidates"))
        (return-from run-a-rank-cpcv-batch nil))
      (format t "[CPCV] 🔬 Dispatching ~d S-RANK candidates (Async)...~%" (length batch))
      
      ;; V49.5 Fix: Set expected count for batch summary
      (setf swimmy.globals:*expected-cpcv-count* (length batch))
      (setf swimmy.globals:*cpcv-results-buffer* nil)
      (setf swimmy.globals:*cpcv-start-time* (get-universal-time))
      (reset-cpcv-metrics :queued (length batch))
      (ignore-errors (write-cpcv-status-file :reason "dispatch"))
      
      ;; Process each strategy
      (dolist (strat batch)
        (handler-case
            (multiple-value-bind (sent _res _err) (request-cpcv-validation strat)
              (declare (ignore _res _err))
              (if sent
                  (%cpcv-metric-inc :sent)
                  (%cpcv-metric-inc :failed)))
          (error (e)
            (%cpcv-metric-inc :failed)
            (format t "[CPCV] ⚠️ Error dispatching ~a: ~a~%" (strategy-name strat) e))))
      (ignore-errors (write-cpcv-status-file :reason "dispatch")))))

;; V50.3: Rank criteria logic moved to school-rank-system.lisp for consolidation.
