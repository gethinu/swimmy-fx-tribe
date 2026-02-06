;;; pair-composite-tests.lisp - Pair composite unit tests

(in-package :swimmy.tests)

(deftest test-pair-id-stable
  "pair-id should be order-independent"
  (let ((a (swimmy.school::pair-id "A" "B"))
        (b (swimmy.school::pair-id "B" "A")))
    (assert-equal a b "pair-id should be stable")))
