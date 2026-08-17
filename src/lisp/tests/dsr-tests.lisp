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

(deftest test-statistical-s-gate-requires-refit-pbo-and-dsr
  "An S candidate is admitted only with measured DSR, acceptable PBO and refit provenance."
  (let* ((mk (lambda (name seed)
               (make-strategy
                :name name
                :cpcv-pbo 0.10
                :cpcv-refit t
                :pnl-history
                (loop for i from 0 below 60
                      collect (+ 0.1d0
                                 (* 0.01d0 (mod (* seed (1+ i)) 7))
                                 (if (evenp i) 0.05d0 -0.05d0))))))
         (target (funcall mk "target" 3))
         (*strategy-knowledge-base*
           (list target (funcall mk "b" 5) (funcall mk "c" 2) (funcall mk "d" 4)))
         (swimmy.school::*statistical-promotion-gates-enabled* t)
         (swimmy.school::*s-rank-pbo-max* 0.25d0)
         (swimmy.school::*dsr-prob-threshold* 0.95d0))
    (multiple-value-bind (passed code message dsr)
        (swimmy.school::s-rank-statistical-gates-p target)
      (declare (ignore message))
      (assert-true passed "Expected full statistical S gate to pass")
      (assert-equal nil code "No failure code on complete proof")
      (assert-true (numberp dsr) "DSR should be recorded in the gate result"))
    (setf (strategy-cpcv-pbo target) 0.50d0)
    (multiple-value-bind (passed code _message _dsr)
        (swimmy.school::s-rank-statistical-gates-p target)
      (declare (ignore _message _dsr))
       (assert-false passed "Overfit-prone candidate must not pass S gate")
       (assert-equal :pbo code "PBO must be the reported failure"))))

(deftest test-ensure-rank-refuses-overfit-cpcv-result
  "The actual unified promotion entry point must refuse an otherwise-eligible S candidate with excessive PBO."
  (let* ((mk (lambda (name seed)
               (make-strategy
                :name name :rank :A :sharpe 1.0d0 :profit-factor 2.0d0
                :win-rate 0.60d0 :max-dd 0.05d0 :trades 200
                :cpcv-pass-rate 0.80d0 :cpcv-median-maxdd 0.05d0
                :cpcv-pbo 0.50d0 :cpcv-refit t
                :pnl-history
                (loop for i from 0 below 60
                      collect (+ 0.1d0
                                 (* 0.01d0 (mod (* seed (1+ i)) 7))
                                 (if (evenp i) 0.05d0 -0.05d0))))))
         (target (funcall mk "overfit-target" 3))
         (*strategy-knowledge-base*
           (list target (funcall mk "b" 5) (funcall mk "c" 2) (funcall mk "d" 4)))
         (swimmy.school::*statistical-promotion-gates-enabled* t)
         (swimmy.school::*s-rank-pbo-max* 0.25d0)
         (swimmy.school::*dsr-prob-threshold* 0.95d0))
    (assert-equal :A (swimmy.school::promote-to-rank-s target))
    (assert-equal :A (strategy-rank target)
                  "PBO failure must leave the strategy below S rank")))

(deftest test-s-candidate-selection-requires-real-dsr
  "With the protocol on, CPCV candidate selection rejects an unmeasurable DSR before using CPCV capacity."
  (let* ((mk (lambda (name seed)
               (make-strategy
                :name name :rank :A :sharpe 1.0d0 :profit-factor 2.0d0
                :win-rate 0.60d0 :max-dd 0.05d0 :trades 200
                :pnl-history
                (loop for i from 0 below 60
                      collect (+ 0.1d0
                                 (* 0.01d0 (mod (* seed (1+ i)) 7))
                                 (if (evenp i) 0.05d0 -0.05d0))))))
         (target (funcall mk "candidate-target" 3))
         (*strategy-knowledge-base*
           (list target (funcall mk "b" 5) (funcall mk "c" 2) (funcall mk "d" 4)))
         (swimmy.school::*statistical-promotion-gates-enabled* t)
         (swimmy.school::*dsr-prob-threshold* 0.95d0))
    (assert-true (swimmy.school::check-rank-criteria target :S :include-cpcv nil)
                 "Adequate DSR evidence should enter the CPCV candidate pool")
    (setf (swimmy.school::strategy-pnl-history target) '(1.0d0 -1.0d0 2.0d0))
    (assert-false (swimmy.school::check-rank-criteria target :S :include-cpcv nil)
                  "Short PnL history must be excluded before CPCV dispatch")))

(deftest test-s-rank-block-diagnostics-returns-plist-without-common-stage2
  "Diagnostics must be returned for both :INCLUDE-COMMON-STAGE2 values.
Regression: the statistical-gate rewrite once nested the result inside the
Common-Stage2 WHEN, so callers that skipped that gate silently got NIL."
  (let* ((mk (lambda (name seed)
               (make-strategy
                :name name :rank :A :sharpe 1.0d0 :profit-factor 2.0d0
                :win-rate 0.60d0 :max-dd 0.05d0 :trades 200
                :cpcv-pass-rate 0.80d0 :cpcv-median-maxdd 0.05d0
                :cpcv-pbo 0.50d0 :cpcv-refit t
                :pnl-history
                (loop for i from 0 below 60
                      collect (+ 0.1d0
                                 (* 0.01d0 (mod (* seed (1+ i)) 7))
                                 (if (evenp i) 0.05d0 -0.05d0))))))
         (target (funcall mk "diag-target" 3))
         (*strategy-knowledge-base*
           (list target (funcall mk "b" 5) (funcall mk "c" 2) (funcall mk "d" 4)))
         (swimmy.school::*statistical-promotion-gates-enabled* t)
         (swimmy.school::*s-rank-pbo-max* 0.25d0)
         (swimmy.school::*dsr-prob-threshold* 0.95d0))
    (dolist (include '(t nil))
      (let ((diag (swimmy.school::s-rank-block-diagnostics
                   target :include-common-stage2 include)))
        (assert-not-nil diag "Diagnostics plist must be returned")
        (assert-true (listp diag) "Diagnostics must be a plist")
        (assert-true (member :pbo (getf diag :failed-gates))
                     "Excessive PBO must be reported as a failed gate")
        (assert-not-nil (getf diag :statistical-message)
                        "A statistical failure must carry an actionable message")
        (assert-equal t (getf diag :cpcv-refit)
                      "Refit provenance must be reported in diagnostics")))
    ;; The Common-Stage2 gate itself must stay opt-out-able.
    (assert-false (member :common-stage2
                          (getf (swimmy.school::s-rank-block-diagnostics
                                 target :include-common-stage2 nil)
                                :failed-gates))
                  "Skipping Common Stage2 must not report it as failed")))
