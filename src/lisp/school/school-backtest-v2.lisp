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
           (strategy-json (alist-to-json strategy-alist))
           (data-file (format nil "~a" (swimmy.core::swimmy-path (format nil "data/historical/~a_M1.csv" actual-symbol))))
           ;; V31.0: Fetch historical swaps for more accurate PnL
           (swaps (fetch-swap-history actual-symbol :start-ts start-ts :end-ts end-ts)))

      (let* ((payload (jsown:new-js
                       ("action" "BACKTEST")
                       ("strategy" strategy-json)
                       ("candles_file" data-file)
                       ("data_id" (format nil "~a_FULL" actual-symbol))
                       ("symbol" actual-symbol)
                       ("swap_history" swaps)
                       ("timeframe" timeframe))))
        
        ;; Add Range if present
        (when start-ts (jsown:extend-js payload ("start_time" start-ts)))
        (when end-ts (jsown:extend-js payload ("end_time" end-ts)))

        ;; Send
        (if (and (boundp 'swimmy.globals:*cmd-publisher*) swimmy.globals:*cmd-publisher*)
            (pzmq:send swimmy.globals:*cmd-publisher* (jsown:to-json payload))
            (format t "[BT-V2] ❌ CMD Publisher NOT BOUND.~%"))))))


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
     (let ((sharpe (getf result :sharpe))
           (pf (getf result :profit-factor)))
       (format t "[BT-V2] 📊 Phase 1 Result for ~a: Sharpe=~,2f PF=~,2f~%" strat-name sharpe pf)
       (if (and (>= sharpe *phase1-min-sharpe*) (>= pf 1.0))
           (progn
             (format t "[BT-V2] ✅ PASSED Phase 1. Promoting to Rank B pool.~%")
             ;; Logic to move to Rank B pool goes here (school-manager)
             ;; For now, just mark it?
             )
           (format t "[BT-V2] ❌ FAILED Phase 1. To Graveyard.~%"))))

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
