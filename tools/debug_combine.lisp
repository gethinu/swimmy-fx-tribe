(in-package :cl-user)
(require 'asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload :swimmy)

(in-package :swimmy.school)

;; Force params
(setf *min-sharpe* -10.0)
(setf *combine-currencies* '("USDJPY" "EURUSD"))

(format t "[DEBUG] Generating Candidate...~%")
(let ((candidate (make-scout-struct 
                  :name "DEBUG-TEST-1"
                  :timeframe 5
                  :sma-short 10
                  :sma-long 50
                  :sl 0.05
                  :tp 0.05)))
  
  (format t "[DEBUG] Testing candidate: ~a~%" candidate)
  
  ;; Inline version of run-combine-test with heavy logging
  (let ((best-symbol nil)
        (best-sharpe -99.0)
        (best-trades 0))
    
    (dolist (sym *combine-currencies*)
      (format t "[DEBUG] Testing Symbol: ~a~%" sym)
      (let ((payload (scout-to-json-payload candidate sym)))
        (if payload
            (progn
              (format t "[DEBUG] Payload generated.~%")
              (let ((resp (send-guardian-request payload)))
                (format t "[DEBUG] Guardian Response: ~a~%" resp)
                (if (and (consp resp) (equal (jsown:val resp "type") "BACKTEST_RESULT"))
                    (let* ((res (jsown:val resp "result"))
                           (sharpe (if (jsown:keyp res "sharpe") (jsown:val res "sharpe") 0.0))
                           (trades (if (jsown:keyp res "trades") (jsown:val res "trades") 0)))
                      
                      (format t "[DEBUG] Parsed - Sharpe: ~a, Trades: ~a~%" sharpe trades)
                      
                      (when (> sharpe best-sharpe)
                        (format t "[DEBUG] New Best!~%")
                        (setf best-sharpe sharpe)
                        (setf best-trades trades)
                        (setf best-symbol sym)))
                    (format t "[DEBUG] Invalid Response Type: ~a~%" (if (consp resp) (jsown:val resp "type") "Not Object")))))
            (format t "[DEBUG] No payload (missing file?)~%"))))
    
    (format t "[DEBUG] Result: Symbol=~a Sharpe=~a Trades=~a~%" best-symbol best-sharpe best-trades)
    (if best-symbol
        (format t "SUCCESS: ~a~%" best-symbol)
        (format t "FAILURE~%"))))

(uiop:quit)
