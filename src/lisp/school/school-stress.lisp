(in-package :swimmy.school)

;;; ==========================================
;;; P7: DEEP VALIDATION STRESS TEST
;;; ==========================================

(defun command-load-csv (path data-id)
  "Send LOAD_CSV command to Guardian"
  (let ((msg (jsown:to-json 
               (jsown:new-js 
                 ("action" "LOAD_CSV")
                 ("path" path)
                 ("data_id" data-id)))))
    (send-zmq-msg msg :target :cmd)
    (format t "[STRESS] 📤 Sent LOAD_CSV: ~a -> ~a~%" path data-id)))

(defun stress-test-strategy (strat symbol-name data-id)
  "Manually send BACKTEST request for a strategy against a specific ID"
  (declare (ignore symbol-name))
  (let* ((tf (strategy-timeframe strat))
         (tf-num (if (numberp tf) tf 1))
         (msg (jsown:to-json 
               (jsown:new-js 
                 ("action" "BACKTEST")
                 ("strategy" (strategy-to-json strat :name-suffix "-STRESS"))
                 ("data_id" data-id)
                 ("timeframe" tf-num)))))
    (send-zmq-msg msg :target :cmd)
    (format t "[STRESS] 📤 Testing ~a on ~a (TF: M~d)~%" (strategy-name strat) data-id tf-num)))

(defvar *stress-test-triggered* nil "Flag to prevent re-running stress test")

(defun check-stress-test-trigger ()
  "Check for /tmp/.swimmy_stress_test flag file and run stress test if found"
  (when (and (not *stress-test-triggered*)
             (probe-file "/tmp/.swimmy_stress_test"))
    (setf *stress-test-triggered* t)
    (format t "[STRESS] 🚀 Flag file detected! Starting Deep Stress Test...~%")
    (handler-case
        (progn
          (delete-file "/tmp/.swimmy_stress_test")
          (run-deep-stress-test))
      (error (e)
        (format t "[STRESS] ❌ Error during stress test: ~a~%" e)))))

(defun run-deep-stress-test ()
  "Orchestrate the Deep Stress Test on USDJPY"
  (format t "[STRESS] 🏗️ Initializing Deep Stress Test (20 years USDJPY)...~%")
  
  ;; 1. Load Data
  (let ((path (swimmy.core::swimmy-path "data/historical/USDJPY_M1.csv"))
        (id "STRESS-USDJPY-20Y"))
    (command-load-csv path id)
    
    ;; 2. Wait for Load (This is blocking/naive, ideally wait for response)
    (format t "[STRESS] ⏳ Waiting 10s for data load (~~7M bars)...~%")
    (sleep 10)
    
    ;; 3. Test Top Strategies
    (let ((targets (subseq *evolved-strategies* 0 (min 5 (length *evolved-strategies*)))))
      (if (null targets)
          (format t "[STRESS] ⚠️ No evolved strategies to test~%")
          (progn
            (format t "[STRESS] 📊 Testing ~d strategies...~%" (length targets))
            (dolist (strat targets)
              (stress-test-strategy strat "USDJPY" id)
              (sleep 0.5))
            (format t "[STRESS] ✅ Stress Test Complete. Check Guardian logs for results.~%"))))))
