
(require :asdf)
(load "swimmy.asd")
(asdf:load-system :swimmy)

(in-package :swimmy.school)

(defun prove-persistence ()
  (format t "🔍 Proving Persistence...~%")
  
  ;; 1. Initialize
  (init-db)
  
  ;; 2. Create Dummy Strategy
  (let ((name "PROVE-PERSISTENCE-TEST")
        (strat (make-strategy :name "PROVE-PERSISTENCE-TEST"
                              :indicators '((rsi 14))
                              :entry "test"
                              :exit "test"
                              :rank :incubator)))
                              
    ;; 3. Set Specific Metric
    (setf (strategy-sharpe strat) 99.99)
    (setf (strategy-profit-factor strat) 8.88)
    
    ;; 4. Upsert (This is what we are testing)
    (format t "[TEST] Upserting ~a with Sharpe=99.99...~%" name)
    (upsert-strategy strat)
    
    (format t "[TEST] Upsert Complete.~%")))

(prove-persistence)
