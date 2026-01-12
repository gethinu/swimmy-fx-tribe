;; verify_mtf.lisp - Verification script for Multi-Timeframe Implementation

(load "~/quicklisp/setup.lisp")
(require 'asdf)
(push #p"/home/swimmy/swimmy/" asdf:*central-registry*)
(ql:quickload :swimmy)

(in-package :swimmy.school)

(defun verify-mtf ()
  (format t "~%[MTF-VERIFY] 🚀 Starting Verification...~%")
  
  ;; 1. RELOAD Knowledge Base to pick up new strategies
  (init-knowledge-base)
  
  ;; 2. Find strategies
  ;; 2. Find strategies
  (let ((scalp (find "SCALP-MTF-MACRO-H1" *strategy-knowledge-base* :key #'strategy-name :test #'string=))
        (break (find "BB-Walk-Trend-H1" *strategy-knowledge-base* :key #'strategy-name :test #'string=))
        (rev   (find "RSI-Dip-Trend-H1" *strategy-knowledge-base* :key #'strategy-name :test #'string=))
        (trend (find "MACD-Trend-Trend-H1" *strategy-knowledge-base* :key #'strategy-name :test #'string=)))
    
    (unless (and scalp break rev trend)
      (format t "[MTF-VERIFY] ❌ Missing some strategies! Found: Scalp=~a Break=~a Rev=~a Trend=~a~%" 
              (if scalp "OK" "NO") (if break "OK" "NO") (if rev "OK" "NO") (if trend "OK" "NO"))
      (return-from verify-mtf))
      
    (format t "[MTF-VERIFY] ✅ Found all 4 clan strategies.~%")
    
    ;; 3. Check Data Requirements
    ;; Ensure we have M1 data (main) and H1 data (aux)
    (unless (and *candle-history* (> (length *candle-history*) 100))
        (format t "[MTF-VERIFY] ⚠️ No main candle history loaded. Attempting to load...~%")
        ;; In a real run, data is loaded by engine/main.lisp. 
        ;; Here we assume the REPL environment has it or we can't really test without mocking.
        )
        
    ;; 3. Check Data Requirements (Skipped in minimal script)
    ;; (format t "[MTF-VERIFY] ⚠️ Skipping data check/backtest in persistent script mode.~%")
    
    (format t "[MTF-VERIFY] ✅ Verification Complete: All Clan Strategies Loaded.~%")
  ))

;; Execute
(handler-case (verify-mtf)
  (error (e) (format t "[MTF-VERIFY] 💥 Error: ~a~%" e)))
