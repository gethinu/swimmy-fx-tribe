;;; src/lisp/tests/stress-test-kb.lisp
;;; ============================================================================
;;; KB CONCURRENCY & DEADLOCK STRESS TEST (V48.2)
;;; Requirement: Robert C. Martin (Uncle Bob) / Elon Musk
;;; ============================================================================

(in-package :swimmy.school)

(defun run-kb-stress-test (&key (threads 20) (iterations 100))
  "Hammer the KB with concurrent rank changes and additions.
   Ensures *kb-lock* and %ensure-rank-no-lock correctly handle nested calls."
  (format t "🚀 STARTING KB STRESS TEST (Threads: ~d, Iterations: ~d)~%" threads iterations)
  
  (let ((workers nil)
        (success-count 0))
    
    (dotimes (i threads)
      (push (bt:make-thread 
             (lambda ()
               (handler-case
                   (dotimes (j iterations)
                     (let ((test-strat (make-strategy :name (format nil "STRESS-~d-~d" i j)
                                                      :rank :B
                                                      :sharpe (random 1.0))))
                       ;; 1. Concurrent Addition (calls ensure-rank internally via init if any)
                       (bt:with-lock-held (*kb-lock*)
                         (push test-strat *strategy-knowledge-base*))
                       
                       ;; 2. Concurrent Rank Promotion/Demotion (Potential Deadlock Spot)
                       (ensure-rank test-strat :A "Stress Promotion")
                       
                       ;; 3. Concurrent Deletion (Graveyard)
                       (ensure-rank test-strat :graveyard "Stress Deletion")
                       
                       (bt:with-lock-held (*kb-lock*)
                         (incf success-count))))
                 (error (e)
                   (format t "❌ THREAD ~d ERROR: ~a~%" i e)))))
            workers))
    
    ;; Wait for all
    (dolist (w workers)
      (bt:join-thread w))
    
    (format t "🏁 STRESS TEST COMPLETE. Successful Ops: ~d/~d~%" 
            success-count (* threads iterations))
    (if (= success-count (* threads iterations))
        (format t "✅ PASS: No deadlocks detected under high contention.~%")
        (format t "❌ FAIL: Performance degraded or errors occurred.~%"))))

;; To run: (swimmy.school::run-kb-stress-test)
