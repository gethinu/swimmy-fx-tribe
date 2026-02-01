;;; school-backtest-v2.lisp
;;; Phase 20: Architecture Upgrade (Data & Backtest)
;;; Implements 2-Stage Backtesting Logic (Screening vs Validation)

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

(defun request-backtest-v2 (strat &key start-date end-date (symbol nil))
  "Request backtest with specific date range.
   start-date/end-date: 'YYYY.MM.DD' strings."
  (let* ((actual-symbol (or symbol (strategy-symbol strat) "USDJPY"))
         (tf-slot (if (slot-exists-p strat 'timeframe) (strategy-timeframe strat) 1))
         (timeframe (get-tf-minutes tf-slot))
         (start-ts (if start-date (- (parse-date-to-timestamp start-date) 2208988800) nil)) ; Lisp Time -> Unix
         (end-ts (if end-date (- (parse-date-to-timestamp end-date) 2208988800) nil)))      ; 2208988800 = 1970 offset
    
    (format t "[BT-V2] 🚀 Requesting ~a Range: ~a - ~a (~a)~%" 
            (strategy-name strat) (or start-date "ALL") (or end-date "ALL") actual-symbol)

    ;; Load Data ID logic (Reuse existing cache if possible)
    ;; For now, assume we use file-based loading in Guardian which is efficient enough for monthly updates?
    ;; Actually, Guardian caches by 'data_id'.
    ;; If we use the same file, we can use "USDJPY_M1" as data_id.
    
    (let* ((strategy-alist (strategy-to-alist strat :name-suffix (format nil "_~a" (or start-date "FULL"))))
           (data-file (format nil "~a" (swimmy.core::swimmy-path (format nil "data/historical/~a_M1.csv" actual-symbol))))
           ;; V31.0: Fetch historical swaps for more accurate PnL
           (swaps (fetch-swap-history actual-symbol :start-ts start-ts :end-ts end-ts)))

      (let* ((*print-case* :downcase)
             (payload (list
                       (cons 'action "BACKTEST")
                       (cons 'strategy strategy-alist)
                       (cons 'candles_file data-file)
                       (cons 'data_id (format nil "~a_FULL" actual-symbol))
                       (cons 'symbol actual-symbol)
                       (cons 'swap_history swaps)
                       (cons 'timeframe timeframe))))
        ;; Add Range if present
        (when start-ts (push `(start_time . ,start-ts) payload))
        (when end-ts (push `(end_time . ,end-ts) payload))

        ;; Send S-expression payload to Backtest Service
        (let ((msg (format nil "~s" payload)))
          (if (and (boundp 'swimmy.globals:*cmd-publisher*) swimmy.globals:*cmd-publisher*)
              (pzmq:send swimmy.globals:*cmd-publisher* msg)
              (format t "[BT-V2] ❌ CMD Publisher NOT BOUND.~%")))))))


;;; =========================================================
;;; 2-STAGE VALIDATION PIPELINE
;;; =========================================================

(defun get-screening-range ()
  (getf *backtest-range-1* :range)) 
  ;; Wait, *backtest-range-1* is (:start "2006.01.01" :end "2020.12.31") logic
  ;; Let's access the plist directly.

(defun run-phase-1-screening (strat)
  "Phase 1: Screening (2006-2020).
   Gate: Sharpe > 0.1, PF > 1.0."
  (let ((start (getf *backtest-range-1* :start))
        (end (getf *backtest-range-1* :end)))
    (request-backtest-v2 strat :start-date start :end-date end)))

(defun run-phase-2-validation (strat)
  "Phase 2: Validation (2021-Present).
   Gate: Sharpe > 0.5."
  (let ((start (getf *backtest-range-2* :start))
        (end (getf *backtest-range-2* :end)))
    (request-backtest-v2 strat :start-date start :end-date end)))

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
    ((search "_2006" strat-name)
     (let* ((base-name (subseq strat-name 0 (search "_2006" strat-name)))
            (strat (find-strategy base-name))
            (sharpe (getf result :sharpe))
            (pf (getf result :profit-factor)))
       (format t "[BT-V2] 📊 Phase 1 Result for ~a: Sharpe=~,2f PF=~,2f~%" strat-name sharpe pf)
       (if (null strat)
           (format t "[BT-V2] ⚠️ Strategy not found for Phase 1 result: ~a~%" base-name)
           (progn
             ;; Sync metrics to the actual strategy object
             (setf (strategy-sharpe strat) (float (getf result :sharpe 0.0))
                   (strategy-profit-factor strat) (float (getf result :profit-factor 0.0))
                   (strategy-win-rate strat) (float (getf result :win-rate 0.0))
                   (strategy-trades strat) (getf result :trades 0)
                   (strategy-max-dd strat) (float (getf result :max-dd 0.0)))
             (when (slot-exists-p strat 'status-reason)
               (setf (strategy-status-reason strat) "Phase1 Screening Result"))
             (upsert-strategy strat)

             (if (and (>= sharpe *phase1-min-sharpe*) (>= pf 1.0))
                 (progn
                   (format t "[BT-V2] ✅ PASSED Phase 1. Promoting to Rank B.~%")
                   (ensure-rank strat :B "Phase1 Screening Passed (V2)"))
                 (progn
                   (format t "[BT-V2] ❌ FAILED Phase 1. To Graveyard.~%")
                   (send-to-graveyard strat "Phase1 Screening Failed (V2)")))))))

    ;; Phase 2 Result
    ((search "_2021" strat-name)
     (let ((sharpe (getf result :sharpe)))
       (format t "[BT-V2] 📊 Phase 2 Result for ~a: Sharpe=~,2f~%" strat-name sharpe)
       (if (>= sharpe *phase2-min-sharpe*)
           (progn
             (format t "[BT-V2] 🌟 PASSED Phase 2! Candidate for Rank A/S.~%")
             ;; Logic to promote to Rank A
             )
           (format t "[BT-V2] ❌ FAILED Phase 2. Back to Pool or Graveyard.~%"))))))
