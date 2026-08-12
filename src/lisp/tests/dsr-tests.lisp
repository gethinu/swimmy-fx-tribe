;;; src/lisp/tests/dsr-tests.lisp
;;; V-methodology (2026-08-11): unit tests for the real Deflated Sharpe Ratio and
;;; the total-trial counter added to school-ranking.lisp. These assert (a) the pure
;;; normal-CDF / inverse-CDF math, (b) that the S-rank gate is byte-identical to the
;;; legacy Sharpe-floor when *enable-real-dsr* is OFF, and (c) that the real DSR is a
;;; well-formed probability (and abstains cleanly on insufficient data).

(in-package :swimmy.tests)

(deftest test-dsr-norm-cdf-known-values
  "Standard normal CDF matches known reference points."
  (assert-true (< (abs (- (swimmy.school::%norm-cdf 0.0d0) 0.5d0)) 1d-6)
               "Phi(0) = 0.5")
  (assert-true (< (abs (- (swimmy.school::%norm-cdf 1.6448536269514722d0) 0.95d0)) 1d-4)
               "Phi(1.6449) ~ 0.95")
  (assert-true (< (abs (- (swimmy.school::%norm-cdf -1.959963984540054d0) 0.025d0)) 1d-4)
               "Phi(-1.96) ~ 0.025"))

(deftest test-dsr-norm-ppf-known-values
  "Inverse standard normal CDF matches known reference points."
  (assert-true (< (abs (- (swimmy.school::%norm-ppf 0.5d0) 0.0d0)) 1d-6)
               "Phi^-1(0.5) = 0")
  (assert-true (< (abs (- (swimmy.school::%norm-ppf 0.975d0) 1.959963984540054d0)) 1d-4)
               "Phi^-1(0.975) ~ 1.95996")
  (assert-true (< (abs (- (swimmy.school::%norm-ppf 0.025d0) -1.959963984540054d0)) 1d-4)
               "Phi^-1(0.025) ~ -1.95996"))

(deftest test-dsr-count-total-trials
  "count-total-trials counts EVERY strategy (the honest N), not just graveyard."
  (let ((*strategy-knowledge-base*
          (list (make-strategy :name "a")
                (make-strategy :name "b")
                (make-strategy :name "c"))))
    (assert-equal 3 (swimmy.school::count-total-trials))))

(deftest test-dsr-gate-off-equals-legacy
  "With *enable-real-dsr* OFF, the S-rank gate is byte-identical to the legacy floor."
  (let ((swimmy.school::*enable-real-dsr* nil)
        (*strategy-knowledge-base* nil))
    (dolist (sh '(0.0 1.0 2.5 3.5 5.0))
      (let ((s (make-strategy :name "x" :sharpe sh)))
        (assert-equal (swimmy.school::meets-s-rank-dsr-legacy-p s)
                      (swimmy.school::meets-s-rank-dsr-p s)
                      "OFF gate must equal the legacy Sharpe-floor")))))

(deftest test-dsr-short-history-abstains
  "Deflated Sharpe returns NIL (abstains) when history is below *dsr-min-samples*."
  (let ((*strategy-knowledge-base* nil)
        (s (make-strategy :name "short" :pnl-history '(1.0 -1.0 2.0))))
    (assert-true (null (swimmy.school::deflated-sharpe-ratio s))
                 "short history must abstain (NIL)")))

(deftest test-dsr-probability-in-range
  "With an adequate population, Deflated Sharpe is a probability in [0,1]."
  (let* ((mk (lambda (name seed)
               (make-strategy
                :name name
                :pnl-history
                (loop for i from 0 below 60
                      collect (+ 0.1d0
                                 (* 0.01d0 (mod (* seed (1+ i)) 7))
                                 (if (evenp i) 0.05d0 -0.05d0))))))
         (target (funcall mk "target" 3))
         (*strategy-knowledge-base*
           (list target
                 (funcall mk "b" 5)
                 (funcall mk "c" 2)
                 (funcall mk "d" 4))))
    (let ((dsr (swimmy.school::deflated-sharpe-ratio target)))
      (assert-not-nil dsr "DSR should be estimable with adequate data")
      (assert-true (and (>= dsr 0.0d0) (<= dsr 1.0d0))
                   "DSR must be a probability in [0,1]"))))
