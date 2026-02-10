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

