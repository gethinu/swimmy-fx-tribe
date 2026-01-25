;; tools/test_sql_rank.lisp
(in-package :cl-user)
(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init) (load quicklisp-init)))

(handler-bind ((style-warning #'muffle-warning)
               (warning #'muffle-warning))
  (asdf:load-system :swimmy))

(in-package :swimmy.school)

(format t "--- SQL Rank Persistence Test ---~%")
(let* ((test-name "SQL-TEST-STRAT-123")
       (strat (make-strategy :name test-name :rank :B :sharpe 0.1 :timeframe 60 :symbol "USDJPY")))
  
  (format t "[TEST] Initializing test strategy...~%")
  (upsert-strategy strat)
  
  (format t "[TEST] Current rank in DB: ~s~%" 
          (swimmy.core:execute-single "SELECT rank FROM strategies WHERE name = ?" test-name))
  
  (format t "[TEST] Promoting to S-Rank via ensure-rank...~%")
  (ensure-rank strat :S "Test Promotion")
  
  (let ((db-rank (swimmy.core:execute-single "SELECT rank FROM strategies WHERE name = ?" test-name)))
    (format t "[TEST] New rank in DB: ~s~%" db-rank)
    
    (if (string-equal db-rank ":S")
        (format t "✅ SUCCESS: SQL Persistence verified.~%")
        (format t "❌ FAILURE: SQL Rank did not update (Target: :S, Got: ~s)~%" db-rank)))
  
  ;; Cleanup
  (swimmy.core:execute-non-query "DELETE FROM strategies WHERE name = ?" test-name))

(sb-ext:exit :code 0)
