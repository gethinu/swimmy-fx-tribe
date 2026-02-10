;;; src/lisp/tests/pattern-similarity-gate-tests.lisp

(in-package :swimmy.tests)

(deftest test-pattern-gate-aligned
  "pattern gate should keep multiplier=1.0 when aligned and confident"
  (multiple-value-bind (mult reason)
      (swimmy.school::pattern-gate-multiplier :buy 0.70 0.20 0.10 :threshold 0.60 :mismatch-multiplier 0.70)
    (declare (ignore reason))
    (assert-equal 1.0 mult "Expected multiplier 1.0 when aligned")))

(deftest test-pattern-gate-disagree
  "pattern gate should reduce when disagreeing"
  (multiple-value-bind (mult reason)
      (swimmy.school::pattern-gate-multiplier :buy 0.10 0.80 0.10 :threshold 0.60 :mismatch-multiplier 0.70)
    (declare (ignore reason))
    (assert-equal 0.70 mult "Expected mismatch multiplier when disagreeing")))

(deftest test-pattern-gate-low-confidence
  "pattern gate should reduce when confidence is below threshold"
  (multiple-value-bind (mult reason)
      (swimmy.school::pattern-gate-multiplier :sell 0.40 0.59 0.01 :threshold 0.60 :mismatch-multiplier 0.70)
    (declare (ignore reason))
    (assert-equal 0.70 mult "Expected mismatch multiplier under low confidence")))

(deftest test-pattern-gate-flat
  "pattern gate should reduce when FLAT is dominant"
  (multiple-value-bind (mult reason)
      (swimmy.school::pattern-gate-multiplier :buy 0.10 0.10 0.80 :threshold 0.60 :mismatch-multiplier 0.70)
    (declare (ignore reason))
    (assert-equal 0.70 mult "Expected mismatch multiplier for FLAT regime")))

(deftest test-pattern-gate-missing-probs-fail-open
  "pattern gate should fail-open when probabilities are missing"
  (multiple-value-bind (mult reason)
      (swimmy.school::pattern-gate-multiplier :buy nil nil nil :threshold 0.60 :mismatch-multiplier 0.70)
    (declare (ignore reason))
    (assert-equal 1.0 mult "Expected fail-open multiplier 1.0 when missing probs")))

(deftest test-pattern-gate-apply-soft-gate
  "apply-pattern-similarity-gate should scale lot when service disagrees"
  (flet ((mock-query (&rest args)
           (declare (ignore args))
           ;; Simulate a confident DOWN regime.
           '((type . "PATTERN_SIMILARITY_RESULT")
             (status . "ok")
             (result . ((p_up . 0.10) (p_down . 0.80) (p_flat . 0.10))))))
    (multiple-value-bind (new-lot mult reason)
        (swimmy.school::apply-pattern-similarity-gate
         "USDJPY" "H1" :buy 0.05 (make-list 120) :query-fn #'mock-query :threshold 0.60 :mismatch-multiplier 0.70)
      (declare (ignore reason))
      (assert-true (< (abs (- 0.035 new-lot)) 1e-6) "Expected lot scaled by 0.7")
      (assert-true (< (abs (- 0.70 mult)) 1e-6) "Expected multiplier 0.7"))))
