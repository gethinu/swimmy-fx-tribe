
(in-package :swimmy.school)

(defun test-db-persistence ()
  (format t "🧪 Testing DB Persistence...~%")
  
  ;; 1. Create Dummy Strategy
  (let ((name "Test-Persistence-Strat-V1")
        (strat (make-strategy :name "Test-Persistence-Strat-V1"
                              :indicators '((rsi 14))
                              :entry "cross check"
                              :exit "tp/sl"
                              :rank :incubator)))
    (setf (strategy-sharpe strat) 0.0)
    
    ;; 2. Initial Upsert
    (upsert-strategy strat)
    (format t "Saved initial strategy (S=0.00).~%")
    
    ;; 3. Verify Initial DB State
    (let ((db-s (execute-single "SELECT sharpe FROM strategies WHERE name=?" name)))
      (format t "DB Initial Sharpe: ~a~%" db-s)
      (unless (zerop (float db-s))
        (format t "❌ Initial DB Sharpe should be 0.0!~%")))
        
    ;; 4. Simulate Backtest Result
    (format t "Simulating Backtest Result (S=2.5)...~%")
    (apply-backtest-result name '(:sharpe 2.5 :profit-factor 1.5 :trades 100))
    
    ;; 5. Verify Updated DB State
    (let ((db-s-new (execute-single "SELECT sharpe FROM strategies WHERE name=?" name)))
      (format t "DB Updated Sharpe: ~a~%" db-s-new)
      (if (= (float db-s-new) 2.5)
          (format t "✅ DB Persistence Works!~%")
          (format t "❌ DB Persistence FAILED! Expected 2.5, got ~a~%" db-s-new)))
          
    ;; Cleanup
    (execute-non-query "DELETE FROM strategies WHERE name=?" name)))

(test-db-persistence)
