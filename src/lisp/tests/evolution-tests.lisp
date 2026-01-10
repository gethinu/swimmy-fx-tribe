;;; src/lisp/tests/evolution-tests.lisp
;;; Unit tests for Genetic Mutation (school-evolution.lisp)
;;; ============================================================================

(in-package :swimmy.tests)

(deftest test-rewrite-logic-symbols-sma
  "Test rewriting SMA parameters in logic"
  (let ((logic '(and (> close sma-50) (cross-above sma-20 sma-50)))
        (result (swimmy.school::rewrite-logic-symbols 
                 '(and (> close sma-50) (cross-above sma-20 sma-50)) 
                 50 60 "SMA")))
    ;; Should become: (and (> close sma-60) (cross-above sma-20 sma-60))
    (assert-equal '(and (> close sma-60) (cross-above sma-20 sma-60)) result "SMA-50 should be rewritten to SMA-60")))

(deftest test-mutate-strategy-structure
  "Test that mutating a strategy returns a valid strategy struct"
  ;; Create a mock parent strategy
  (let ((parent (swimmy.school:make-strategy 
                 :name "ParentStrat" 
                 :sl 0.05 
                 :tp 0.05 
                 :indicators '((sma 50) (rsi 14))
                 :entry '(> close sma-50)
                 :exit '(< close sma-50))))
    
    (let ((child (swimmy.school:mutate-strategy parent)))
      (assert-not-nil child "Mutated child should not be nil")
      (assert-true (string/= (swimmy.school:strategy-name child) "ParentStrat") "Child name should differ")
      (assert-equal "ParentStrat-mut" (subseq (swimmy.school:strategy-name child) 0 15) "Child should inherit name prefix"))))

(deftest test-mutate-param-sl-tp
  "Test that SL and TP can be mutated"
  ;; We can't guarantee mutation happens every time due to randomness,
  ;; but we can verify the function runs without error and returns reasonable values.
  (let ((parent (swimmy.school:make-strategy :name "Test" :sl 0.1 :tp 0.1 :indicators '((sma 20)))))
    (loop repeat 5 do
      (let ((child (swimmy.school:mutate-strategy parent)))
        (assert-true (and (> (swimmy.school:strategy-sl child) 0)
                          (< (swimmy.school:strategy-sl child) 1.0))
                     "SL should act in safe bounds")
        (assert-true (and (> (swimmy.school:strategy-tp child) 0)
                          (< (swimmy.school:strategy-tp child) 1.0))
                     "TP should act in safe bounds")))))
