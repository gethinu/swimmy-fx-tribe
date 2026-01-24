;;; test-regime-lock.lisp
;;; Self-contained test runner for Regime Hard Lock

(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(handler-case
    (asdf:load-system :swimmy)
  (error (e)
    (format t "[FATAL] ASDF Load Error: ~a~%" e)
    (sb-ext:exit :code 1)))

(in-package :swimmy.school)

(defun run-regime-lock-test ()
  (format t "~%🔒 REGIME HARD LOCK TEST (Musk's Wall)~%")
  
  ;; 1. Setup Dummy Strategy (TREND)
  ;; Ensure we can create a strategy without errors
  (let* ((trend-strat (make-strategy :name "TEST-TREND" :category :trend :symbol "USDJPY" :tp 1.0 :sl 0.5))
         (strategies (list trend-strat))
         (passed t))
    
    ;; 2. Mock RANGING Regime -> Should Reject
    (format t "---------------------------------------------------~%")
    (format t "Test 1: RANGING Regime vs TREND Strategy~%")
    (let ((selected (select-strategies-for-regime :ranging strategies)))
      (if (null selected)
          (format t "✅ PASSED: Trend strategy locked out of Ranging market.~%")
          (progn
            (setf passed nil)
            (format t "❌ FAILED: Trend strategy leaked into Ranging market!~%"))))
            
    ;; 3. Mock TRENDING Regime -> Should Accept
    (format t "Test 2: TRENDING Regime vs TREND Strategy~%")
    (let ((selected (select-strategies-for-regime :trend-early strategies)))
      (if selected
          (format t "✅ PASSED: Trend strategy accepted in Trending market.~%")
          (progn
            (setf passed nil)
            (format t "❌ FAILED: Trend strategy blocked from Trending market!~%"))))
            
    ;; 4. Mock EXTREME Regime -> Should Reject (unless LEGEND)
    (format t "Test 3: EXTREME Regime vs Normal Strategy~%")
    (let ((selected (select-strategies-for-regime :volatile-spike strategies)))
      (if (null selected)
          (format t "✅ PASSED: Normal strategy locked out of Extreme market.~%")
          (progn
             (setf passed nil)
             (format t "❌ FAILED: Normal strategy leaked into Extreme market!~%"))))

    (format t "---------------------------------------------------~%")
    (if passed
        (format t "🎉 ALL REGIME LOCK TESTS PASSED~%")
        (error "Regime Lock Verification Failed"))))

(run-regime-lock-test)
