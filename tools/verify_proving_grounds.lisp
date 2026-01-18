(in-package :swimmy.school)

(defun test-proving-grounds ()
  (format t "~%[TEST] 🧪 Testing The Proving Grounds...~%")
  
  ;; 1. Check Tier Slot
  (let ((s (make-strategy :name "Test-Strat" :tier :incubator)))
    (unless (eq (strategy-tier s) :incubator)
      (error "Tier slot not working!")))
  (format t "[TEST] ✅ Strategy Tier Slot OK.~%")

  ;; 2. Setup Dummy Strategies
  (format t "[TEST] 🛠️  Creating Dummy Strategies...~%")
  (setf *strategy-knowledge-base* nil)
  
  ;; Create 10 Trend Strategies with varying Sharpe
  (loop for i from 1 to 10 do
    (let ((s (make-strategy :name (format nil "Trend-~d" i)
                            :category :trend
                            :tier :training
                            :sharpe (* i 0.1)))) ;; Sharpe 0.1 to 1.0
       (push s *strategy-knowledge-base*)))
       
  ;; Create 2 Battlefield Defenders (Weak)
  (loop for i from 1 to 2 do
    (let ((s (make-strategy :name (format nil "Defender-~d" i)
                            :category :trend
                            :tier :battlefield
                            :sharpe 0.05))) ;; Very weak
       (push s *strategy-knowledge-base*)))
       
  ;; 3. Run Battle Royale
  (format t "[TEST] ⚔️  Running Battle Royale (Quota: ~d)...~%" *battlefield-quota*)
  (execute-proving-grounds)
  
  ;; 4. Verify Results
  (let ((bf (get-strategies-by-tier :battlefield :trend)))
    (format t "[TEST] 📊 Battlefield Count: ~d~%" (length bf))
    (dolist (s bf)
      (format t "   - ~a (Sharpe: ~,2f)~%" (strategy-name s) (strategy-sharpe s)))
      
    (unless (= (length bf) 4)
      (error "Battlefield Quota failed! Expected 4, gote ~d" (length bf)))
      
    ;; The strongest training strats should have promoted
    ;; Trend-10 (1.0), Trend-9 (0.9), Trend-8 (0.8), Trend-7 (0.7)
    (unless (find "Trend-10" bf :key #'strategy-name :test #'string=)
      (error "Strongest challenger (Trend-10) failed to promote!"))
      
    (when (find "Defender-1" bf :key #'strategy-name :test #'string=)
      (error "Weak defender (Defender-1) failed to be demoted!")))
      
  (format t "[TEST] ✅ Battle Royale Logic Verified.~%")
  
  ;; 5. Test Breeding
  (format t "[TEST] 🧬 Testing Breeder...~%")
  (run-breeding-cycle)
  
  (let ((babies (get-strategies-by-tier :incubator)))
    (format t "[TEST] 👶 Incubator Count: ~d~%" (length babies))
    (unless (> (length babies) 0)
      (error "Breeding failed! No babies born.")))
      
  (format t "[TEST] ✅ Breeding Verified.~%")
  (format t "[TEST] 🏆 ALL SYSTEMS GO. THE PROVING GROUNDS ARE OPEN.~%"))

(test-proving-grounds)
(sb-ext:exit :code 0)
