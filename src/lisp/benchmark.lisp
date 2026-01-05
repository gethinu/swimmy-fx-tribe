;;; ============================================================================
;;; benchmark.lisp - Graham Advisor: Performance Benchmarking

(in-package :swimmy.school)
;;; ============================================================================
;;;
;;; PURPOSE:
;;;   Quantitative verification of system performance.
;;;   Graham: "In the short run, the market is a voting machine.
;;;            In the long run, it is a weighing machine."
;;;
;;; KEY FEATURES:
;;;   - Externalized thresholds (.opus/thresholds.json)
;;;   - Multiple profiles (default, conservative, aggressive)
;;;   - Sharpe ratio verification (>1.0 over 90 days)
;;;   - Max drawdown tracking (<15%)
;;;
;;; MAIN FUNCTIONS:
;;;   (run-all-benchmarks)
;;;     → Run complete benchmark suite
;;;   (run-sharpe-benchmark)
;;;     → Verify Sharpe > 1.0
;;;   (set-benchmark-profile "conservative")
;;;     → Switch threshold profile
;;;
;;; CONFIGURATION:
;;;   Edit .opus/thresholds.json to adjust thresholds without code changes.
;;;
;;; VERSION: 2.0 (2026-01-02)
;;; ============================================================================

;;; ==========================================
;;; BENCHMARK PARAMETERS (EXTERNALIZED)
;;; ==========================================

(defparameter *thresholds-config-path* "/home/swimmy/swimmy/.opus/thresholds.json")
(defparameter *active-benchmark-profile* "default")

;; Fallback defaults if config not found
(defparameter *benchmark-thresholds*
  '((:sharpe-90d . 1.0)
    (:sharpe-60d . 0.8)
    (:sharpe-30d . 0.6)
    (:max-drawdown . 0.15)
    (:win-rate . 0.45)
    (:profit-factor . 1.3))
  "Quality thresholds - loaded from external config")

(defun load-thresholds-config ()
  "Graham improvement: Load thresholds from external JSON config"
  (handler-case
      (when (probe-file *thresholds-config-path*)
        (with-open-file (in *thresholds-config-path* :direction :input)
          (let* ((content (make-string (file-length in)))
                 (_ (read-sequence content in))
                 (parsed (jsown:parse content))
                 (active (or (jsown:val-safe parsed "active_profile") "default"))
                 (profiles (jsown:val-safe parsed "profiles"))
                 (profile (when profiles (jsown:val-safe profiles active))))
            (declare (ignore _))
            (when profile
              (setf *active-benchmark-profile* active)
              (setf *benchmark-thresholds*
                    (list (cons :sharpe-90d (or (jsown:val-safe profile "sharpe_90d") 1.0))
                          (cons :sharpe-60d (or (jsown:val-safe profile "sharpe_60d") 0.8))
                          (cons :sharpe-30d (or (jsown:val-safe profile "sharpe_30d") 0.6))
                          (cons :max-drawdown (or (jsown:val-safe profile "max_drawdown") 0.15))
                          (cons :win-rate (or (jsown:val-safe profile "win_rate") 0.45))
                          (cons :profit-factor (or (jsown:val-safe profile "profit_factor") 1.3))))
              (format t "[B] 📋 Loaded benchmark profile: ~a~%" active)))))
    (error (e)
      (format t "[B] ⚠️ Using default thresholds: ~a~%" e))))

(defun set-benchmark-profile (profile-name)
  "Switch to a different benchmark profile (default/conservative/aggressive)"
  (setf *active-benchmark-profile* profile-name)
  (format t "[B] 🔄 Switching to profile: ~a~%" profile-name)
  (load-thresholds-config))

(defun get-threshold (key)
  "Get threshold value by key"
  (cdr (assoc key *benchmark-thresholds*)))

;;; ==========================================
;;; BENCHMARK FUNCTIONS
;;; ==========================================


(defun run-sharpe-benchmark ()
  "Graham: Quantitatively verify Sharpe > 1.0"
  (format t "~%════════════════════════════════════════════════════~%")
  (format t "📊 SHARPE RATIO BENCHMARK (Graham)~%")
  (format t "════════════════════════════════════════════════════~%")
  
  (let* ((s30 (when (fboundp 'calculate-rolling-sharpe) 
                (calculate-rolling-sharpe 30)))
         (s60 (when (fboundp 'calculate-rolling-sharpe) 
                (calculate-rolling-sharpe 60)))
         (s90 (when (fboundp 'calculate-rolling-sharpe) 
                (calculate-rolling-sharpe 90)))
         (threshold (cdr (assoc :sharpe-90d *benchmark-thresholds*)))
         (passed nil))
    
    (format t "~%📈 SHARPE RATIOS:~%")
    (when s30
      (format t "   30-day: ~,2f~%" (or (getf s30 :annualized-sharpe) 0)))
    (when s60
      (format t "   60-day: ~,2f~%" (or (getf s60 :annualized-sharpe) 0)))
    (when s90
      (let ((sharpe (getf s90 :annualized-sharpe)))
        (format t "   90-day: ~,2f (threshold: ~,1f)~%" (or sharpe 0) threshold)
        (setf passed (and sharpe (> sharpe threshold)))))
    
    (format t "~%🎯 RESULT: ~a~%"
            (if passed "✅ PASS - Sharpe > 1.0" "❌ FAIL - Below threshold"))
    (format t "════════════════════════════════════════════════════~%")
    
    passed))

(defun run-drawdown-benchmark ()
  "Check max drawdown stays under threshold"
  (let* ((threshold (cdr (assoc :max-drawdown *benchmark-thresholds*)))
         (current-dd (if (boundp '*current-drawdown*) *current-drawdown* 0.0))
         (max-dd (if (boundp '*max-drawdown*) *max-drawdown* 0.0))
         (passed (<= max-dd threshold)))
    
    (format t "~%📉 DRAWDOWN BENCHMARK:~%")
    (format t "   Current: ~,1f%~%" (* 100 current-dd))
    (format t "   Max: ~,1f% (threshold: ~,1f%)~%" 
            (* 100 max-dd) (* 100 threshold))
    (format t "   Result: ~a~%" (if passed "✅ PASS" "❌ FAIL"))
    
    passed))

(defun run-all-benchmarks ()
  "Run complete Graham benchmark suite"
  (format t "~%")
  (format t "════════════════════════════════════════════════════~%")
  (format t "🧪 GRAHAM BENCHMARK SUITE~%")
  (format t "════════════════════════════════════════════════════~%")
  (format t "Running at: ~a~%" (get-jst-str))
  
  (let ((results nil)
        (all-passed t))
    
    ;; Sharpe benchmark
    (let ((sharpe-pass (run-sharpe-benchmark)))
      (push (cons :sharpe sharpe-pass) results)
      (unless sharpe-pass (setf all-passed nil)))
    
    ;; Drawdown benchmark
    (let ((dd-pass (run-drawdown-benchmark)))
      (push (cons :drawdown dd-pass) results)
      (unless dd-pass (setf all-passed nil)))
    
    ;; Naval proof progress
    (when (fboundp 'get-proof-progress)
      (let ((proof (get-proof-progress)))
        (format t "~%🚀 NAVAL PROOF:~%")
        (format t "   Day ~d/90 (~,0f%)~%"
                (getf proof :elapsed-days) (getf proof :progress-pct))
        (format t "   PnL: ¥~:d~%" (round (getf proof :cumulative-pnl)))))
    
    (format t "~%════════════════════════════════════════════════════~%")
    (format t "📋 SUMMARY: ~a~%"
            (if all-passed "✅ ALL BENCHMARKS PASSED" "❌ SOME BENCHMARKS FAILED"))
    (format t "════════════════════════════════════════════════════~%")
    
    (list :all-passed all-passed
          :results results)))

;;; ==========================================
;;; INITIALIZATION
;;; ==========================================

;; Load external thresholds on startup
(load-thresholds-config)
(format t "[L] 📊 benchmark.lisp loaded - Graham performance verification~%")
(format t "[L] 📋 Active profile: ~a~%" *active-benchmark-profile*)

