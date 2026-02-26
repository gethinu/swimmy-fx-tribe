;;; school-backtest-v2.lisp
;;; Phase 20: Architecture Upgrade (Data & Backtest)
;;; Implements screening backtests with explicit date ranges

(in-package :swimmy.school)

;;; =========================================================
;;; DATE UTILS (For Text -> Unix Timestamp)
;;; =========================================================

(defun parse-date-to-timestamp (date-str)
  "Convert YYYY.MM.DD string to Unix Timestamp.
   Format: '2006.01.01' -> 1136073600"
  (let* ((parts (uiop:split-string date-str :separator "."))
         (year (parse-integer (first parts)))
         (month (parse-integer (second parts)))
         (day (parse-integer (third parts))))
    (encode-universal-time 0 0 0 day month year 0)))

;;; =========================================================
;;; CORE BACKTEST V2 (With Date Range)
;;; =========================================================

(defun option-field (key value)
  "Encode Option<T> as empty/one-element list for S-expression payloads."
  (if (null value)
      (list key)
      (list key value)))

(defun request-backtest-v2 (strat &key start-date end-date start-ts end-ts phase range-id (symbol nil) (request-id nil))
  "Request backtest with specific date range.
   start-date/end-date: 'YYYY.MM.DD' strings."
  (let* ((req-id (or request-id (swimmy.core::generate-uuid)))
         (actual-symbol (or symbol (strategy-symbol strat) "USDJPY"))
         (tf-slot (if (slot-exists-p strat 'timeframe) (strategy-timeframe strat) 1))
         (timeframe (get-tf-minutes tf-slot))
         (start-ts (or start-ts
                       (and start-date (- (parse-date-to-timestamp start-date) 2208988800)))) ; Lisp Time -> Unix
         (end-ts (or end-ts
                     (and end-date (- (parse-date-to-timestamp end-date) 2208988800)))))      ; 2208988800 = 1970 offset

    (setf swimmy.globals:*backtest-submit-last-id* req-id)

    (format t "[BT-V2] 🚀 Requesting ~a Range: ~a - ~a (~a)~%" 
            (strategy-name strat) (or start-date "ALL") (or end-date "ALL") actual-symbol)

    ;; Load Data ID logic (Reuse existing cache if possible)
    ;; For now, assume we use file-based loading in Guardian which is efficient enough for monthly updates?
    ;; Actually, Guardian caches by 'data_id'.
    ;; If we use the same file, we can use "USDJPY_M1" as data_id.
    
    (let* ((strategy-alist (strategy-to-alist strat :name-suffix (format nil "_~a" (or range-id start-date "FULL"))))
           (override swimmy.core::*backtest-csv-override*)
           (data-file (if (and override (> (length override) 0))
                          override
                          (format nil "~a" (swimmy.core::swimmy-path (format nil "data/historical/~a_M1.csv" actual-symbol)))))
           ;; V31.0: Fetch historical swaps for more accurate PnL
           (swaps (fetch-swap-history actual-symbol :start-ts start-ts :end-ts end-ts)))

      (let* ((payload (list
                       (cons 'action "BACKTEST")
                       (cons 'strategy strategy-alist)
                       (cons 'request_id req-id)
                       (option-field 'start_time start-ts)
                       (option-field 'end_time end-ts)
                       (option-field 'data_id (format nil "~a_M1" actual-symbol))
                       (option-field 'candles_file data-file)
                       (cons 'symbol actual-symbol)
                       (cons 'swap_history swaps)
                       (option-field 'timeframe timeframe))))
        ;; Add Range if present
        (when phase (push `(phase . ,phase) payload))
        (when range-id (push `(range_id . ,range-id) payload))

        ;; Send S-expression payload to Backtest Service
        (let ((*print-case* :downcase)
              (*print-pretty* nil)
              (*print-right-margin* most-positive-fixnum)
              (*print-escape* t)
              (*package* (find-package :swimmy.school)))
          (let ((msg (format nil "~s" payload)))
            (send-zmq-msg msg :target :backtest)))))))


;;; =========================================================
;;; SCREENING PIPELINE
;;; =========================================================

(defun get-screening-range ()
  (getf *backtest-range-1* :range)) 
  ;; Wait, *backtest-range-1* is (:start "2011.01.01" :end "2020.12.31") logic
  ;; Let's access the plist directly.

(defun run-phase-1-screening (strat)
  "Phase 1: Screening (2011-2020).
   Gate: Sharpe > 0.1, PF > 1.0."
  (let ((start (getf *backtest-range-1* :start))
        (end (getf *backtest-range-1* :end)))
    (request-backtest-v2 strat :start-date start :end-date end :phase "phase1" :range-id "P1")))

(defun founder-phase1-recovery-candidate-p (strat)
  "Return T when STRAT should be eligible for Founder recovery gate in Phase1."
  (and *phase1-founder-recovery-enabled*
       strat
       (stringp (strategy-name strat))
       (search "HUNTED-" (string-upcase (strategy-name strat)))))

(defun founder-phase1-recovery-passed-p (strat sharpe pf trades max-dd)
  "Return T when Founder recovery gate passes for STRAT metrics."
  (and (founder-phase1-recovery-candidate-p strat)
       (>= sharpe (float *phase1-founder-min-sharpe* 1.0))
       (>= pf (float *phase1-founder-min-pf* 1.0))
       (>= trades (max 0 (truncate *phase1-founder-min-trades*)))
       (<= max-dd (float *phase1-founder-max-dd* 1.0))))

(defun phase1-screening-passed-p (strat sharpe pf trades max-dd)
  "Return two values: (passed-p founder-recovery-p)."
  (let* ((standard-pass (and (>= sharpe *phase1-min-sharpe*)
                             (>= pf 1.0)))
         (founder-recovery-pass (and (not standard-pass)
                                     (founder-phase1-recovery-passed-p strat sharpe pf trades max-dd))))
    (values (or standard-pass founder-recovery-pass)
            founder-recovery-pass)))

;;; =========================================================
;;; LOGIC HANDLER (Async Results)
;;; =========================================================

;; Note: Results come back asynchronously via 'process-backtest-result' in school.lisp.
;; We need to route them correctly.
;; The Strategy Name suffix "_2006.01.01" etc can be used to identify the stage.

(defun handle-v2-result (strat-name result)
  "Handle results from V2 backtests based on suffix."
  (cond
    ;; Phase 1 Result
    ((search "_P1" strat-name)
     (let* ((base-name (subseq strat-name 0 (search "_P1" strat-name)))
            ;; Resolve pending candidate first so same-name archived KB entries do not
            ;; shadow the latest Phase1 candidate.
            (pending-strat (when (fboundp 'take-phase1-pending-candidate)
                             (take-phase1-pending-candidate base-name)))
            (kb-strat (and (null pending-strat)
                           (find-strategy base-name)))
            (db-strat (and (null pending-strat)
                           (null kb-strat)
                           (when (fboundp '%load-strategy-from-db-for-phase1)
                             (%load-strategy-from-db-for-phase1 base-name))))
            (strat (or pending-strat kb-strat db-strat))
            ;; Normalize nil metrics to 0.0 to avoid type errors in comparisons/float coercions.
            (sharpe (float (or (getf result :sharpe) 0.0) 0.0))
            (pf (float (or (getf result :profit-factor) 0.0) 0.0))
            (wr (float (or (getf result :win-rate) 0.0) 0.0))
            (trades (max 0 (truncate (or (getf result :trades) 0))))
            (max-dd (float (or (getf result :max-dd) 0.0) 0.0)))
       (format t "[BT-V2] 📊 Phase 1 Result for ~a: Sharpe=~,2f PF=~,2f~%" strat-name sharpe pf)
       (if (null strat)
           (format t "[BT-V2] ⚠️ Strategy not found for Phase 1 result: ~a~%" base-name)
           (progn
             ;; Admit deferred candidate into KB just before rank evaluation.
             (unless (find strat *strategy-knowledge-base* :test #'eq)
               (bt:with-lock-held (*kb-lock*)
                 (unless (find strat *strategy-knowledge-base* :test #'eq)
                   (push strat *strategy-knowledge-base*))))
             ;; Sync metrics to the actual strategy object
             (setf (strategy-sharpe strat) sharpe
                   (strategy-profit-factor strat) pf
                   (strategy-win-rate strat) wr
                   (strategy-trades strat) trades
                   (strategy-max-dd strat) max-dd)
             (when (slot-exists-p strat 'status-reason)
               (setf (strategy-status-reason strat) "Phase1 Screening Result"))
             (upsert-strategy strat)
              (let* ((rank-raw (strategy-rank strat))
                     (rank-token
                       (let* ((raw (cond
                                     ((null rank-raw) "")
                                     ((symbolp rank-raw) (symbol-name rank-raw))
                                     (t (format nil "~a" rank-raw))))
                              (up (string-upcase
                                   (string-trim '(#\Space #\Tab #\Newline #\Return) raw))))
                         (if (and (> (length up) 0) (char= (char up 0) #\:))
                             (subseq up 1)
                             up)))
                     (legend-revalidation-p
                       (member rank-token '("LEGEND" "LEGEND-ARCHIVE") :test #'string=)))
                (if legend-revalidation-p
                    (progn
                      ;; Legend revalidation updates metrics only; rank stays immutable.
                      (format t "[BT-V2] 👑 Legend revalidation update. Keeping rank ~a for ~a.~%"
                              rank-token (strategy-name strat))
                      (ensure-rank strat (strategy-rank strat) "Legend Phase1 revalidation")
                      (when (fboundp 'add-strategy-to-active-pools)
                        (add-strategy-to-active-pools strat)))
                    (multiple-value-bind (passed founder-recovery-pass)
                        (phase1-screening-passed-p strat sharpe pf trades max-dd)
                      (if passed
                          (progn
                            (when founder-recovery-pass
                              (format t "[BT-V2] 🛟 Founder recovery gate used: S>=~,2f PF>=~,2f Trades>=~d MaxDD<=~,2f~%"
                                      (float *phase1-founder-min-sharpe* 1.0)
                                      (float *phase1-founder-min-pf* 1.0)
                                      (max 0 (truncate *phase1-founder-min-trades*))
                                      (float *phase1-founder-max-dd* 1.0)))
                            (format t "[BT-V2] ✅ PASSED Phase 1. Promoting to Rank B.~%")
                            (ensure-rank strat :B "Phase1 Screening Passed (V2)")
                            (when (fboundp 'add-strategy-to-active-pools)
                              (add-strategy-to-active-pools strat)))
                          (progn
                            (format t "[BT-V2] ❌ FAILED Phase 1. To Graveyard.~%")
                            (send-to-graveyard strat "Phase1 Screening Failed (V2)"))))))))))))

(defun %load-strategy-from-db-for-phase1 (name)
  "Best-effort fallback: load strategy object from strategies.data_sexp by NAME.
Used when Phase1 result arrives after pending in-memory state was lost."
  (handler-case
      (let ((sexp-str (ignore-errors
                        (execute-single "SELECT data_sexp FROM strategies WHERE name = ?" name))))
        (when (and (stringp sexp-str)
                   (> (length (string-trim '(#\Space #\Tab #\Newline #\Return) sexp-str)) 0))
          (let ((obj (swimmy.core:safe-read-sexp sexp-str :package :swimmy.school)))
            (when (strategy-p obj)
              obj))))
    (error (e)
      (format t "[BT-V2] ⚠️ DB fallback load failed for ~a: ~a~%" name e)
      nil)))
