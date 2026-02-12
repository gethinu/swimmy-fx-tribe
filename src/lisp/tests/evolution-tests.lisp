;;; src/lisp/tests/evolution-tests.lisp
;;; Unit tests for Genetic Mutation (school-evolution.lisp)
;;; ============================================================================

(in-package :swimmy.tests)

(deftest test-rewrite-logic-symbols-sma
  "Test rewriting SMA parameters in logic"
  (let ((result (swimmy.school::rewrite-logic-symbols 
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
      (assert-true (search "ParentStrat" (swimmy.school:strategy-name child)) "Child should preserve Root Name")
      (assert-true (search "-Gen" (swimmy.school:strategy-name child)) "Child should track Generation"))))

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

(deftest test-breed-strategies-name-is-unique-with-same-random-state
  "Breeding should still produce unique child names even when RNG seed is replayed."
  (let* ((p1 (swimmy.school:make-strategy
              :name "Parent-Alpha-Gen10"
              :generation 10
              :category :trend
              :timeframe 3600
              :direction :BOTH
              :symbol "USDJPY"
              :sl 0.8
              :tp 1.6
              :indicators '((sma 20))
              :entry '(> close sma-20)
              :exit '(< close sma-20)))
         (p2 (swimmy.school:make-strategy
              :name "Parent-Beta-Gen9"
              :generation 9
              :category :trend
              :timeframe 3600
              :direction :BOTH
              :symbol "USDJPY"
              :sl 0.9
              :tp 1.8
              :indicators '((sma 50))
              :entry '(> close sma-50)
              :exit '(< close sma-50)))
         (seed (make-random-state t))
         (name-a nil)
         (name-b nil))
    (let ((*random-state* (make-random-state seed)))
      (setf name-a (swimmy.school:strategy-name (swimmy.school::breed-strategies p1 p2))))
    (let ((*random-state* (make-random-state seed)))
      (setf name-b (swimmy.school:strategy-name (swimmy.school::breed-strategies p1 p2))))
    (assert-true (string/= name-a name-b)
                 (format nil "Expected unique names, got ~a and ~a" name-a name-b))))

(deftest test-pfwr-mutation-bias-adjusts-rr-when-parents-underperform
  "PF/WR mutation bias should adjust RR toward better parent profile when parents underperform."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-PFWR-P1"
                                          :profit-factor 1.05 :win-rate 0.39
                                          :sl 30.0 :tp 40.0))
         (p2 (swimmy.school:make-strategy :name "UT-PFWR-P2"
                                          :profit-factor 0.90 :win-rate 0.32
                                          :sl 20.0 :tp 80.0))
         (orig-enabled swimmy.school::*pfwr-mutation-bias-enabled*)
         (orig-strength swimmy.school::*pfwr-mutation-bias-strength*)
         (orig-target-pf swimmy.school::*pfwr-target-pf*)
         (orig-target-wr swimmy.school::*pfwr-target-wr*))
    (unwind-protect
        (progn
          (setf swimmy.school::*pfwr-mutation-bias-enabled* t)
          (setf swimmy.school::*pfwr-mutation-bias-strength* 1.0)
          (setf swimmy.school::*pfwr-target-pf* 1.30)
          (setf swimmy.school::*pfwr-target-wr* 0.43)
          (multiple-value-bind (sl tp)
              (swimmy.school::apply-pfwr-mutation-bias 20.0 80.0 p1 p2)
            (let ((rr (/ tp sl)))
              (assert-true (< rr 4.0)
                           "Expected RR to move away from overly aggressive profile"))))
      (setf swimmy.school::*pfwr-mutation-bias-enabled* orig-enabled)
      (setf swimmy.school::*pfwr-mutation-bias-strength* orig-strength)
      (setf swimmy.school::*pfwr-target-pf* orig-target-pf)
      (setf swimmy.school::*pfwr-target-wr* orig-target-wr))))

(deftest test-pfwr-mutation-bias-noop-when-parents-already-healthy
  "PF/WR mutation bias should be no-op when parent PF/WR already meets targets."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-PFWR-OK1"
                                          :profit-factor 1.60 :win-rate 0.52
                                          :sl 25.0 :tp 35.0))
         (p2 (swimmy.school:make-strategy :name "UT-PFWR-OK2"
                                          :profit-factor 1.55 :win-rate 0.50
                                          :sl 22.0 :tp 34.0))
         (orig-enabled swimmy.school::*pfwr-mutation-bias-enabled*)
         (orig-target-pf swimmy.school::*pfwr-target-pf*)
         (orig-target-wr swimmy.school::*pfwr-target-wr*))
    (unwind-protect
        (progn
          (setf swimmy.school::*pfwr-mutation-bias-enabled* t)
          (setf swimmy.school::*pfwr-target-pf* 1.30)
          (setf swimmy.school::*pfwr-target-wr* 0.43)
          (multiple-value-bind (sl tp)
              (swimmy.school::apply-pfwr-mutation-bias 20.0 60.0 p1 p2)
            (assert-true (< (abs (- sl 20.0)) 1.0e-6)
                         "Expected SL unchanged when parents are already healthy")
            (assert-true (< (abs (- tp 60.0)) 1.0e-6)
                         "Expected TP unchanged when parents are already healthy")))
      (setf swimmy.school::*pfwr-mutation-bias-enabled* orig-enabled)
      (setf swimmy.school::*pfwr-target-pf* orig-target-pf)
      (setf swimmy.school::*pfwr-target-wr* orig-target-wr))))

(deftest test-pfwr-mutation-bias-lowers-rr-when-wr-gap-dominates
  "PF/WR mutation bias should lower RR when WR deficit dominates."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-PFWR-WR1"
                                          :profit-factor 1.35 :win-rate 0.30
                                          :sl 20.0 :tp 60.0))
         (p2 (swimmy.school:make-strategy :name "UT-PFWR-WR2"
                                          :profit-factor 1.40 :win-rate 0.31
                                          :sl 24.0 :tp 72.0))
         (orig-enabled swimmy.school::*pfwr-mutation-bias-enabled*)
         (orig-strength swimmy.school::*pfwr-mutation-bias-strength*)
         (orig-target-pf swimmy.school::*pfwr-target-pf*)
         (orig-target-wr swimmy.school::*pfwr-target-wr*))
    (unwind-protect
        (progn
          (setf swimmy.school::*pfwr-mutation-bias-enabled* t)
          (setf swimmy.school::*pfwr-mutation-bias-strength* 1.0)
          (setf swimmy.school::*pfwr-target-pf* 1.30)
          (setf swimmy.school::*pfwr-target-wr* 0.43)
          (multiple-value-bind (sl tp)
              (swimmy.school::apply-pfwr-mutation-bias 20.0 80.0 p1 p2)
            (let ((rr (/ tp sl)))
              (assert-true (< rr 3.0)
                           "Expected RR to be pulled down when WR deficit dominates"))))
      (setf swimmy.school::*pfwr-mutation-bias-enabled* orig-enabled)
      (setf swimmy.school::*pfwr-mutation-bias-strength* orig-strength)
      (setf swimmy.school::*pfwr-target-pf* orig-target-pf)
      (setf swimmy.school::*pfwr-target-wr* orig-target-wr))))

(deftest test-pfwr-mutation-bias-tightens-rr-cap-for-severe-wr-deficit
  "PF/WR mutation bias should clamp RR harder when WR deficit is severe."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-PFWR-WR-CAP1"
                                          :profit-factor 1.38 :win-rate 0.30
                                          :sl 20.0 :tp 80.0))
         (p2 (swimmy.school:make-strategy :name "UT-PFWR-WR-CAP2"
                                          :profit-factor 1.35 :win-rate 0.31
                                          :sl 24.0 :tp 96.0))
         (orig-enabled swimmy.school::*pfwr-mutation-bias-enabled*)
         (orig-strength swimmy.school::*pfwr-mutation-bias-strength*)
         (orig-target-pf swimmy.school::*pfwr-target-pf*)
         (orig-target-wr swimmy.school::*pfwr-target-wr*))
    (unwind-protect
        (progn
          (setf swimmy.school::*pfwr-mutation-bias-enabled* t)
          (setf swimmy.school::*pfwr-mutation-bias-strength* 1.0)
          (setf swimmy.school::*pfwr-target-pf* 1.30)
          (setf swimmy.school::*pfwr-target-wr* 0.43)
          (multiple-value-bind (sl tp)
              (swimmy.school::apply-pfwr-mutation-bias 20.0 100.0 p1 p2)
            (let ((rr (/ tp sl)))
              (assert-true (<= rr 2.2)
                           (format nil "Expected severe WR deficit to cap RR at 2.2 or less, got ~,3f" rr)))))
      (setf swimmy.school::*pfwr-mutation-bias-enabled* orig-enabled)
      (setf swimmy.school::*pfwr-mutation-bias-strength* orig-strength)
      (setf swimmy.school::*pfwr-target-pf* orig-target-pf)
      (setf swimmy.school::*pfwr-target-wr* orig-target-wr))))

(deftest test-pfwr-mutation-bias-tightens-rr-cap-for-moderate-wr-deficit
  "PF/WR mutation bias should also tighten RR cap for moderate WR deficits near A-gate."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-PFWR-WR-MOD1"
                                          :profit-factor 1.36 :win-rate 0.39
                                          :sl 20.0 :tp 80.0))
         (p2 (swimmy.school:make-strategy :name "UT-PFWR-WR-MOD2"
                                          :profit-factor 1.34 :win-rate 0.38
                                          :sl 24.0 :tp 96.0))
         (orig-enabled swimmy.school::*pfwr-mutation-bias-enabled*)
         (orig-strength swimmy.school::*pfwr-mutation-bias-strength*)
         (orig-target-pf swimmy.school::*pfwr-target-pf*)
         (orig-target-wr swimmy.school::*pfwr-target-wr*))
    (unwind-protect
        (progn
          (setf swimmy.school::*pfwr-mutation-bias-enabled* t)
          (setf swimmy.school::*pfwr-mutation-bias-strength* 1.0)
          (setf swimmy.school::*pfwr-target-pf* 1.30)
          (setf swimmy.school::*pfwr-target-wr* 0.43)
          (multiple-value-bind (sl tp)
              (swimmy.school::apply-pfwr-mutation-bias 20.0 100.0 p1 p2)
            (let ((rr (/ tp sl)))
              (assert-true (<= rr 2.1)
                           (format nil "Expected moderate WR deficit to cap RR at 2.1 or less, got ~,3f" rr)))))
      (setf swimmy.school::*pfwr-mutation-bias-enabled* orig-enabled)
      (setf swimmy.school::*pfwr-mutation-bias-strength* orig-strength)
      (setf swimmy.school::*pfwr-target-pf* orig-target-pf)
      (setf swimmy.school::*pfwr-target-wr* orig-target-wr))))

(deftest test-pfwr-mutation-bias-stabilizes-opposite-complements-near-a-target
  "Opposite complement parents should still get RR stabilization even when average meets A-targets."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-PFWR-COMP-WR"
                                          :profit-factor 1.22 :win-rate 0.49
                                          :sl 20.0 :tp 30.0))
         (p2 (swimmy.school:make-strategy :name "UT-PFWR-COMP-PF"
                                          :profit-factor 1.38 :win-rate 0.38
                                          :sl 20.0 :tp 80.0))
         (orig-enabled swimmy.school::*pfwr-mutation-bias-enabled*)
         (orig-strength swimmy.school::*pfwr-mutation-bias-strength*)
         (orig-target-pf swimmy.school::*pfwr-target-pf*)
         (orig-target-wr swimmy.school::*pfwr-target-wr*))
    (unwind-protect
        (progn
          (setf swimmy.school::*pfwr-mutation-bias-enabled* t)
          (setf swimmy.school::*pfwr-mutation-bias-strength* 1.0)
          (setf swimmy.school::*pfwr-target-pf* 1.30)
          (setf swimmy.school::*pfwr-target-wr* 0.43)
          (multiple-value-bind (sl tp)
              ;; Extreme initial RR should be pulled back to convergence band.
              (swimmy.school::apply-pfwr-mutation-bias 20.0 100.0 p1 p2)
            (let ((rr (/ tp sl)))
              (assert-true (<= rr 1.86)
                           (format nil "Expected complement stabilization RR<=1.86, got ~,3f" rr))
              (assert-true (>= rr 1.55)
                           (format nil "Expected complement stabilization RR>=1.55, got ~,3f" rr)))))
      (setf swimmy.school::*pfwr-mutation-bias-enabled* orig-enabled)
      (setf swimmy.school::*pfwr-mutation-bias-strength* orig-strength)
      (setf swimmy.school::*pfwr-target-pf* orig-target-pf)
      (setf swimmy.school::*pfwr-target-wr* orig-target-wr))))

(deftest test-select-pfwr-anchor-parent-prefers-higher-wr-parent-when-wr-gap-dominates
  "When WR deficit dominates, anchor parent should prefer higher WR profile."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-PFWR-ANCHOR-P1"
                                          :profit-factor 1.40 :win-rate 0.34
                                          :sl 20.0 :tp 80.0))
         (p2 (swimmy.school:make-strategy :name "UT-PFWR-ANCHOR-P2"
                                          :profit-factor 1.20 :win-rate 0.48
                                          :sl 20.0 :tp 30.0))
         (anchor (swimmy.school::select-pfwr-anchor-parent p1 p2)))
    (assert-true (eq anchor p2)
                 "Expected higher-WR parent to be selected as anchor under WR deficit pressure")))

(deftest test-pfwr-mutation-bias-applies-pf-recovery-floor-when-pf-gap-dominates
  "PF-dominant deficit should enforce a minimum RR floor for PF recovery."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-PFWR-PF-FLOOR1"
                                          :profit-factor 0.95 :win-rate 0.56
                                          :sl 40.0 :tp 32.0))
         (p2 (swimmy.school:make-strategy :name "UT-PFWR-PF-FLOOR2"
                                          :profit-factor 0.98 :win-rate 0.55
                                          :sl 38.0 :tp 30.0))
         (orig-enabled swimmy.school::*pfwr-mutation-bias-enabled*)
         (orig-strength swimmy.school::*pfwr-mutation-bias-strength*)
         (orig-target-pf swimmy.school::*pfwr-target-pf*)
         (orig-target-wr swimmy.school::*pfwr-target-wr*))
    (unwind-protect
        (progn
          (setf swimmy.school::*pfwr-mutation-bias-enabled* t)
          (setf swimmy.school::*pfwr-mutation-bias-strength* 1.0)
          (setf swimmy.school::*pfwr-target-pf* 1.30)
          (setf swimmy.school::*pfwr-target-wr* 0.43)
          (multiple-value-bind (sl tp)
              (swimmy.school::apply-pfwr-mutation-bias 40.0 20.0 p1 p2)
            (let ((rr (/ tp sl)))
              (assert-true (>= rr 1.79)
                           (format nil "Expected PF recovery floor RR>=1.8, got ~,3f" rr)))))
      (setf swimmy.school::*pfwr-mutation-bias-enabled* orig-enabled)
      (setf swimmy.school::*pfwr-mutation-bias-strength* orig-strength)
      (setf swimmy.school::*pfwr-target-pf* orig-target-pf)
      (setf swimmy.school::*pfwr-target-wr* orig-target-wr))))

(deftest test-pfwr-mutation-bias-applies-pf-recovery-floor-for-moderate-pf-gap
  "Moderate PF gap with healthy WR should still trigger PF recovery RR floor."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-PFWR-PF-MOD1"
                                          :profit-factor 1.22 :win-rate 0.45
                                          :sl 40.0 :tp 28.0))
         (p2 (swimmy.school:make-strategy :name "UT-PFWR-PF-MOD2"
                                          :profit-factor 1.22 :win-rate 0.46
                                          :sl 38.0 :tp 26.0))
         (orig-enabled swimmy.school::*pfwr-mutation-bias-enabled*)
         (orig-strength swimmy.school::*pfwr-mutation-bias-strength*)
         (orig-target-pf swimmy.school::*pfwr-target-pf*)
         (orig-target-wr swimmy.school::*pfwr-target-wr*))
    (unwind-protect
        (progn
          (setf swimmy.school::*pfwr-mutation-bias-enabled* t)
          (setf swimmy.school::*pfwr-mutation-bias-strength* 1.0)
          (setf swimmy.school::*pfwr-target-pf* 1.30)
          (setf swimmy.school::*pfwr-target-wr* 0.43)
          (multiple-value-bind (sl tp)
              (swimmy.school::apply-pfwr-mutation-bias 40.0 20.0 p1 p2)
            (let ((rr (/ tp sl)))
              (assert-true (>= rr 1.79)
                           (format nil "Expected moderate PF gap to keep RR floor near 1.8, got ~,3f" rr)))))
      (setf swimmy.school::*pfwr-mutation-bias-enabled* orig-enabled)
      (setf swimmy.school::*pfwr-mutation-bias-strength* orig-strength)
      (setf swimmy.school::*pfwr-target-pf* orig-target-pf)
      (setf swimmy.school::*pfwr-target-wr* orig-target-wr))))

(deftest test-pfwr-mutation-bias-increases-scale-when-pf-gap-dominates
  "PF-dominant deficit should allow SL/TP absolute distance expansion (cost dilution)."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-PFWR-SCALE1"
                                          :profit-factor 1.00 :win-rate 0.55
                                          :sl 20.0 :tp 30.0))
         (p2 (swimmy.school:make-strategy :name "UT-PFWR-SCALE2"
                                          :profit-factor 1.05 :win-rate 0.54
                                          :sl 22.0 :tp 33.0))
         (orig-enabled swimmy.school::*pfwr-mutation-bias-enabled*)
         (orig-strength swimmy.school::*pfwr-mutation-bias-strength*)
         (orig-target-pf swimmy.school::*pfwr-target-pf*)
         (orig-target-wr swimmy.school::*pfwr-target-wr*))
    (unwind-protect
        (progn
          (setf swimmy.school::*pfwr-mutation-bias-enabled* t)
          (setf swimmy.school::*pfwr-mutation-bias-strength* 1.0)
          (setf swimmy.school::*pfwr-target-pf* 1.30)
          (setf swimmy.school::*pfwr-target-wr* 0.43)
          (multiple-value-bind (sl tp)
              (swimmy.school::apply-pfwr-mutation-bias 20.0 20.0 p1 p2)
            (assert-true (> (+ sl tp) 40.0)
                         (format nil "Expected scale expansion above 40.0, got ~,3f" (+ sl tp)))))
      (setf swimmy.school::*pfwr-mutation-bias-enabled* orig-enabled)
      (setf swimmy.school::*pfwr-mutation-bias-strength* orig-strength)
      (setf swimmy.school::*pfwr-target-pf* orig-target-pf)
      (setf swimmy.school::*pfwr-target-wr* orig-target-wr))))

(deftest test-pfwr-mutation-bias-expands-scale-for-opposite-complements
  "Opposite complement pair should enforce a larger absolute SL/TP scale floor."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-PFWR-COMP-SCALE-WR"
                                          :profit-factor 1.22 :win-rate 0.49
                                          :sl 20.0 :tp 30.0))
         (p2 (swimmy.school:make-strategy :name "UT-PFWR-COMP-SCALE-PF"
                                          :profit-factor 1.38 :win-rate 0.38
                                          :sl 20.0 :tp 80.0))
         (orig-enabled swimmy.school::*pfwr-mutation-bias-enabled*)
         (orig-strength swimmy.school::*pfwr-mutation-bias-strength*)
         (orig-target-pf swimmy.school::*pfwr-target-pf*)
         (orig-target-wr swimmy.school::*pfwr-target-wr*))
    (unwind-protect
        (progn
          (setf swimmy.school::*pfwr-mutation-bias-enabled* t)
          (setf swimmy.school::*pfwr-mutation-bias-strength* 1.0)
          (setf swimmy.school::*pfwr-target-pf* 1.30)
          (setf swimmy.school::*pfwr-target-wr* 0.43)
          (multiple-value-bind (sl tp)
              (swimmy.school::apply-pfwr-mutation-bias 20.0 20.0 p1 p2)
            (assert-true (>= (+ sl tp) 56.0)
                         (format nil "Expected opposite-complement scale >=56.0, got ~,3f" (+ sl tp)))))
      (setf swimmy.school::*pfwr-mutation-bias-enabled* orig-enabled)
      (setf swimmy.school::*pfwr-mutation-bias-strength* orig-strength)
      (setf swimmy.school::*pfwr-target-pf* orig-target-pf)
      (setf swimmy.school::*pfwr-target-wr* orig-target-wr))))

(deftest test-pfwr-mutation-bias-expands-scale-for-wr-only-pairs
  "WR-only pair should still enforce a minimum PF-recovery scale floor."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-PFWR-WRONLY-1"
                                          :profit-factor 1.22 :win-rate 0.49
                                          :sl 20.0 :tp 30.0))
         (p2 (swimmy.school:make-strategy :name "UT-PFWR-WRONLY-2"
                                          :profit-factor 1.24 :win-rate 0.47
                                          :sl 18.0 :tp 28.0))
         (orig-enabled swimmy.school::*pfwr-mutation-bias-enabled*)
         (orig-strength swimmy.school::*pfwr-mutation-bias-strength*)
         (orig-target-pf swimmy.school::*pfwr-target-pf*)
         (orig-target-wr swimmy.school::*pfwr-target-wr*))
    (unwind-protect
        (progn
          (setf swimmy.school::*pfwr-mutation-bias-enabled* t)
          (setf swimmy.school::*pfwr-mutation-bias-strength* 1.0)
          (setf swimmy.school::*pfwr-target-pf* 1.30)
          (setf swimmy.school::*pfwr-target-wr* 0.43)
          (multiple-value-bind (sl tp)
              (swimmy.school::apply-pfwr-mutation-bias 20.0 20.0 p1 p2)
            (assert-true (>= (+ sl tp) 48.0)
                         (format nil "Expected WR-only PF recovery scale >=48.0, got ~,3f" (+ sl tp)))))
      (setf swimmy.school::*pfwr-mutation-bias-enabled* orig-enabled)
      (setf swimmy.school::*pfwr-mutation-bias-strength* orig-strength)
      (setf swimmy.school::*pfwr-target-pf* orig-target-pf)
      (setf swimmy.school::*pfwr-target-wr* orig-target-wr))))

(deftest test-strategy-breeding-priority-score-prefers-a-base-near-candidate
  "Breeding priority should prefer candidates closer to A-base gates."
  (let* ((near-a (swimmy.school:make-strategy :name "UT-PRIORITY-NEAR"
                                              :rank :B
                                              :generation 4
                                              :sharpe 1.00
                                              :profit-factor 1.28
                                              :win-rate 0.42
                                              :max-dd 0.05))
         (far-a (swimmy.school:make-strategy :name "UT-PRIORITY-FAR"
                                             :rank :B
                                             :generation 4
                                             :sharpe 1.60
                                             :profit-factor 1.05
                                             :win-rate 0.35
                                             :max-dd 0.05))
         (near-score (swimmy.school::strategy-breeding-priority-score near-a))
         (far-score (swimmy.school::strategy-breeding-priority-score far-a)))
    (assert-true (> near-score far-score)
                 (format nil "Expected near-A score (~,3f) > far-A score (~,3f)"
                         near-score far-score))))

(deftest test-pfwr-mutation-bias-raises-rr-when-pf-gap-dominates
  "PF/WR mutation bias should raise RR when PF deficit dominates."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-PFWR-PF1"
                                          :profit-factor 0.90 :win-rate 0.56
                                          :sl 40.0 :tp 32.0))
         (p2 (swimmy.school:make-strategy :name "UT-PFWR-PF2"
                                          :profit-factor 0.95 :win-rate 0.55
                                          :sl 30.0 :tp 24.0))
         (orig-enabled swimmy.school::*pfwr-mutation-bias-enabled*)
         (orig-strength swimmy.school::*pfwr-mutation-bias-strength*)
         (orig-target-pf swimmy.school::*pfwr-target-pf*)
         (orig-target-wr swimmy.school::*pfwr-target-wr*))
    (unwind-protect
        (progn
          (setf swimmy.school::*pfwr-mutation-bias-enabled* t)
          (setf swimmy.school::*pfwr-mutation-bias-strength* 1.0)
          (setf swimmy.school::*pfwr-target-pf* 1.30)
          (setf swimmy.school::*pfwr-target-wr* 0.43)
          (multiple-value-bind (sl tp)
              (swimmy.school::apply-pfwr-mutation-bias 40.0 20.0 p1 p2)
            (let ((rr (/ tp sl)))
              (assert-true (> rr 1.0)
                           "Expected RR to be pushed up when PF deficit dominates"))))
      (setf swimmy.school::*pfwr-mutation-bias-enabled* orig-enabled)
      (setf swimmy.school::*pfwr-mutation-bias-strength* orig-strength)
      (setf swimmy.school::*pfwr-target-pf* orig-target-pf)
      (setf swimmy.school::*pfwr-target-wr* orig-target-wr))))

(deftest test-mutate-sltp-with-pfwr-bias-lowers-rr-when-wr-gap-dominates
  "Evolution mutate helper should lower RR when WR deficit dominates."
  (let* ((s (swimmy.school:make-strategy :name "UT-EVO-WR"
                                         :profit-factor 1.35 :win-rate 0.30
                                         :sl 20.0 :tp 80.0))
         (orig-enabled swimmy.school::*pfwr-mutation-bias-enabled*)
         (orig-strength swimmy.school::*pfwr-mutation-bias-strength*)
         (orig-target-pf swimmy.school::*pfwr-target-pf*)
         (orig-target-wr swimmy.school::*pfwr-target-wr*))
    (unwind-protect
        (progn
          (setf swimmy.school::*pfwr-mutation-bias-enabled* t)
          (setf swimmy.school::*pfwr-mutation-bias-strength* 1.0)
          (setf swimmy.school::*pfwr-target-pf* 1.30)
          (setf swimmy.school::*pfwr-target-wr* 0.43)
          (multiple-value-bind (sl tp)
              (swimmy.school::mutate-sltp-with-pfwr-bias s :factor 0.0)
            (let ((rr (/ tp sl)))
              (assert-true (< rr 3.0)
                           "Expected RR to be pulled down by evolution-side PF/WR bias"))))
      (setf swimmy.school::*pfwr-mutation-bias-enabled* orig-enabled)
      (setf swimmy.school::*pfwr-mutation-bias-strength* orig-strength)
      (setf swimmy.school::*pfwr-target-pf* orig-target-pf)
      (setf swimmy.school::*pfwr-target-wr* orig-target-wr))))

(deftest test-mutate-sltp-with-pfwr-bias-raises-rr-when-pf-gap-dominates
  "Evolution mutate helper should raise RR when PF deficit dominates."
  (let* ((s (swimmy.school:make-strategy :name "UT-EVO-PF"
                                         :profit-factor 0.90 :win-rate 0.56
                                         :sl 40.0 :tp 20.0))
         (orig-enabled swimmy.school::*pfwr-mutation-bias-enabled*)
         (orig-strength swimmy.school::*pfwr-mutation-bias-strength*)
         (orig-target-pf swimmy.school::*pfwr-target-pf*)
         (orig-target-wr swimmy.school::*pfwr-target-wr*))
    (unwind-protect
        (progn
          (setf swimmy.school::*pfwr-mutation-bias-enabled* t)
          (setf swimmy.school::*pfwr-mutation-bias-strength* 1.0)
          (setf swimmy.school::*pfwr-target-pf* 1.30)
          (setf swimmy.school::*pfwr-target-wr* 0.43)
          (multiple-value-bind (sl tp)
              (swimmy.school::mutate-sltp-with-pfwr-bias s :factor 0.0)
            (let ((rr (/ tp sl)))
              (assert-true (> rr 1.0)
                           "Expected RR to be pushed up by evolution-side PF/WR bias"))))
      (setf swimmy.school::*pfwr-mutation-bias-enabled* orig-enabled)
      (setf swimmy.school::*pfwr-mutation-bias-strength* orig-strength)
      (setf swimmy.school::*pfwr-target-pf* orig-target-pf)
      (setf swimmy.school::*pfwr-target-wr* orig-target-wr))))

(deftest test-breed-strategies-reapplies-pfwr-bias-after-q-selection
  "Breeder should re-apply PF/WR bias after Q-selection to avoid extreme RR in WR-deficit regimes."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-Q-OVERRIDE-P1"
                                          :category :trend
                                          :timeframe 3600
                                          :direction :BOTH
                                          :symbol "USDJPY"
                                          :generation 10
                                          :profit-factor 1.38 :win-rate 0.30
                                          :sl 20.0 :tp 60.0
                                          :indicators '((sma 20))
                                          :entry '(> close sma-20)
                                          :exit '(< close sma-20)))
         (p2 (swimmy.school:make-strategy :name "UT-Q-OVERRIDE-P2"
                                          :category :trend
                                          :timeframe 3600
                                          :direction :BOTH
                                          :symbol "USDJPY"
                                          :generation 9
                                          :profit-factor 1.34 :win-rate 0.31
                                          :sl 24.0 :tp 72.0
                                          :indicators '((sma 50))
                                          :entry '(> close sma-50)
                                          :exit '(< close sma-50)))
         (orig-enabled swimmy.school::*pfwr-mutation-bias-enabled*)
         (orig-strength swimmy.school::*pfwr-mutation-bias-strength*)
         (orig-target-pf swimmy.school::*pfwr-target-pf*)
         (orig-target-wr swimmy.school::*pfwr-target-wr*)
         (orig-select (and (fboundp 'swimmy.school::select-sltp-with-q)
                           (symbol-function 'swimmy.school::select-sltp-with-q))))
    (unwind-protect
        (progn
          (setf swimmy.school::*pfwr-mutation-bias-enabled* t)
          (setf swimmy.school::*pfwr-mutation-bias-strength* 1.0)
          (setf swimmy.school::*pfwr-target-pf* 1.30)
          (setf swimmy.school::*pfwr-target-wr* 0.43)
          (when (fboundp 'swimmy.school::select-sltp-with-q)
            ;; Simulate extreme Q-table exploit (RR=10.0).
            (setf (symbol-function 'swimmy.school::select-sltp-with-q)
                  (lambda (_tf _dir _sym _fallback-sl _fallback-tp)
                    (declare (ignore _tf _dir _sym _fallback-sl _fallback-tp))
                    (values 10.0 100.0))))
          (let* ((child (swimmy.school::breed-strategies p1 p2))
                 (rr (/ (swimmy.school:strategy-tp child)
                        (max 0.0001 (swimmy.school:strategy-sl child)))))
            (assert-true (< rr 3.0)
                         (format nil "Expected post-Q PF/WR guard to pull RR below 3.0, got ~,3f" rr))))
      (setf swimmy.school::*pfwr-mutation-bias-enabled* orig-enabled)
      (setf swimmy.school::*pfwr-mutation-bias-strength* orig-strength)
      (setf swimmy.school::*pfwr-target-pf* orig-target-pf)
      (setf swimmy.school::*pfwr-target-wr* orig-target-wr)
      (when orig-select
        (setf (symbol-function 'swimmy.school::select-sltp-with-q) orig-select)))))

(deftest test-find-diverse-breeding-partner-falls-back-past-similar-neighbor
  "Breeder should scan forward for a non-similar partner when immediate neighbor is too similar."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-P1" :sl 20.0 :tp 60.0 :rank :B))
         (p2 (swimmy.school:make-strategy :name "UT-P2" :sl 20.0 :tp 60.0 :rank :B))
         (p3 (swimmy.school:make-strategy :name "UT-P3" :sl 20.0 :tp 60.0 :rank :B))
         (orig-corr (symbol-function 'swimmy.school::strategies-correlation-ok-p)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::strategies-correlation-ok-p)
                (lambda (a b)
                  (declare (ignore a))
                  (not (string= (swimmy.school:strategy-name b) "UT-P2"))))
          (let ((picked (swimmy.school::find-diverse-breeding-partner
                         p1 (list p1 p2 p3) :start-index 1)))
            (assert-true (eq picked p3)
                         (format nil "Expected fallback partner UT-P3, got ~a"
                                 (if picked (swimmy.school:strategy-name picked) :none)))))
      (setf (symbol-function 'swimmy.school::strategies-correlation-ok-p) orig-corr))))

(deftest test-find-diverse-breeding-partner-prefers-pfwr-complement
  "Breeder should prefer a complement partner when parent has WR/PF deficit imbalance."
  (let* ((parent (swimmy.school:make-strategy :name "UT-COMP-PARENT"
                                              :rank :B :sl 20.0 :tp 60.0
                                              :profit-factor 1.40 :win-rate 0.34))
         (cand1 (swimmy.school:make-strategy :name "UT-COMP-CAND1"
                                             :rank :B :sl 20.0 :tp 70.0
                                             :profit-factor 1.45 :win-rate 0.35))
         (cand2 (swimmy.school:make-strategy :name "UT-COMP-CAND2"
                                             :rank :B :sl 20.0 :tp 30.0
                                             :profit-factor 1.22 :win-rate 0.49))
         (orig-corr (symbol-function 'swimmy.school::strategies-correlation-ok-p)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::strategies-correlation-ok-p)
                (lambda (_a _b)
                  (declare (ignore _a _b))
                  t))
          (let ((picked (swimmy.school::find-diverse-breeding-partner
                         parent (list parent cand1 cand2) :start-index 1)))
            (assert-true (eq picked cand2)
                         (format nil "Expected WR-complement partner cand2, got ~a"
                                 (if picked (swimmy.school:strategy-name picked) :none)))))
      (setf (symbol-function 'swimmy.school::strategies-correlation-ok-p) orig-corr))))

(deftest test-find-diverse-breeding-partner-prioritizes-complement-over-base-score
  "When parent lacks WR/PF side, complement partner should win even if base score is lower."
  (let* ((parent (swimmy.school:make-strategy :name "UT-COMP-STRICT-PARENT"
                                              :rank :B :generation 50
                                              :sl 20.0 :tp 60.0
                                              :profit-factor 1.45 :win-rate 0.34
                                              :sharpe 1.1))
         (cand-high-no-complement (swimmy.school:make-strategy :name "UT-COMP-STRICT-HIGH"
                                                               :rank :LEGEND :generation 1200
                                                               :sl 20.0 :tp 70.0
                                                               :profit-factor 1.60 :win-rate 0.35
                                                               :sharpe 2.6))
         (cand-low-complement (swimmy.school:make-strategy :name "UT-COMP-STRICT-LOW"
                                                           :rank :B :generation 5
                                                           :sl 20.0 :tp 30.0
                                                           :profit-factor 1.22 :win-rate 0.49
                                                           :sharpe 0.2))
         (orig-corr (symbol-function 'swimmy.school::strategies-correlation-ok-p)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::strategies-correlation-ok-p)
                (lambda (_a _b)
                  (declare (ignore _a _b))
                  t))
          (let ((picked (swimmy.school::find-diverse-breeding-partner
                         parent (list parent cand-high-no-complement cand-low-complement)
                         :start-index 1)))
            (assert-true (eq picked cand-low-complement)
                         (format nil "Expected complement candidate UT-COMP-STRICT-LOW, got ~a"
                                 (if picked (swimmy.school:strategy-name picked) :none)))))
      (setf (symbol-function 'swimmy.school::strategies-correlation-ok-p) orig-corr))))

(deftest test-find-diverse-breeding-partner-filters-low-quality-complements
  "Complement partner should be filtered out when opposite-side metric is too weak."
  (let* ((parent (swimmy.school:make-strategy :name "UT-COMP-FILTER-PARENT"
                                              :rank :B :generation 90
                                              :sl 20.0 :tp 60.0
                                              :profit-factor 1.45 :win-rate 0.34
                                              :sharpe 1.2))
         (cand-high-but-weak-pf (swimmy.school:make-strategy :name "UT-COMP-FILTER-WEAK"
                                                             :rank :LEGEND :generation 1400
                                                             :sl 20.0 :tp 30.0
                                                             :profit-factor 1.05 :win-rate 0.53
                                                             :sharpe 2.8))
         (cand-lower-but-balanced (swimmy.school:make-strategy :name "UT-COMP-FILTER-GOOD"
                                                               :rank :B :generation 8
                                                               :sl 20.0 :tp 30.0
                                                               :profit-factor 1.23 :win-rate 0.47
                                                               :sharpe 0.3))
         (orig-corr (symbol-function 'swimmy.school::strategies-correlation-ok-p)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::strategies-correlation-ok-p)
                (lambda (_a _b)
                  (declare (ignore _a _b))
                  t))
          (let ((picked (swimmy.school::find-diverse-breeding-partner
                         parent (list parent cand-high-but-weak-pf cand-lower-but-balanced)
                         :start-index 1)))
            (assert-true (eq picked cand-lower-but-balanced)
                         (format nil "Expected balanced complement candidate UT-COMP-FILTER-GOOD, got ~a"
                                 (if picked (swimmy.school:strategy-name picked) :none)))))
      (setf (symbol-function 'swimmy.school::strategies-correlation-ok-p) orig-corr))))

(deftest test-find-diverse-breeding-partner-prefers-near-pf-wr-only-candidate
  "When parent mainly lacks PF, near-threshold PF + solid WR candidate should be prioritized."
  (let* ((parent (swimmy.school:make-strategy :name "UT-NEAR-PF-PARENT"
                                              :rank :B :generation 80
                                              :sl 20.0 :tp 60.0
                                              :profit-factor 1.12 :win-rate 0.48
                                              :sharpe 1.1))
         (cand-high-no-pf (swimmy.school:make-strategy :name "UT-NEAR-PF-HIGH-NO"
                                                       :rank :LEGEND :generation 1200
                                                       :sl 20.0 :tp 70.0
                                                       :profit-factor 1.10 :win-rate 0.62
                                                       :sharpe 2.4))
         (cand-near-pf (swimmy.school:make-strategy :name "UT-NEAR-PF-CAND"
                                                    :rank :B :generation 6
                                                    :sl 20.0 :tp 34.0
                                                    :profit-factor 1.25 :win-rate 0.46
                                                    :sharpe 0.3))
         (orig-corr (symbol-function 'swimmy.school::strategies-correlation-ok-p)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::strategies-correlation-ok-p)
                (lambda (_a _b)
                  (declare (ignore _a _b))
                  t))
          (let ((picked (swimmy.school::find-diverse-breeding-partner
                         parent (list parent cand-high-no-pf cand-near-pf)
                         :start-index 1)))
            (assert-true (eq picked cand-near-pf)
                         (format nil "Expected near-PF candidate UT-NEAR-PF-CAND, got ~a"
                                 (if picked (swimmy.school:strategy-name picked) :none)))))
      (setf (symbol-function 'swimmy.school::strategies-correlation-ok-p) orig-corr))))

(deftest test-select-logic-anchor-parent-prefers-high-wr-under-wr-deficit
  "Logic anchor should prefer higher-WR parent when WR deficit dominates."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-LOGIC-WR-P1"
                                          :profit-factor 1.38 :win-rate 0.30
                                          :entry '(and p1-entry) :exit '(and p1-exit)))
         (p2 (swimmy.school:make-strategy :name "UT-LOGIC-WR-P2"
                                          :profit-factor 1.12 :win-rate 0.49
                                          :entry '(and p2-entry) :exit '(and p2-exit)))
         (picked (swimmy.school::select-logic-anchor-parent p1 p2)))
    (assert-true (eq picked p2)
                 (format nil "Expected higher-WR logic parent p2, got ~a"
                         (if picked (swimmy.school:strategy-name picked) :none)))))

(deftest test-select-logic-anchor-parent-prefers-high-pf-under-pf-deficit
  "Logic anchor should prefer higher-PF parent when PF deficit dominates."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-LOGIC-PF-P1"
                                          :profit-factor 0.92 :win-rate 0.55
                                          :entry '(and p1-entry) :exit '(and p1-exit)))
         (p2 (swimmy.school:make-strategy :name "UT-LOGIC-PF-P2"
                                          :profit-factor 1.42 :win-rate 0.47
                                          :entry '(and p2-entry) :exit '(and p2-exit)))
         (picked (swimmy.school::select-logic-anchor-parent p1 p2)))
    (assert-true (eq picked p2)
                 (format nil "Expected higher-PF logic parent p2, got ~a"
                         (if picked (swimmy.school:strategy-name picked) :none)))))

(deftest test-breed-strategies-combines-high-wr-entry-and-high-pf-exit
  "Breeder should combine entry from higher-WR parent and exit from higher-PF parent."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-LOGIC-COMB-P1"
                                          :category :trend :timeframe 3600 :direction :BOTH :symbol "USDJPY"
                                          :generation 10 :rank :B
                                          :profit-factor 1.08 :win-rate 0.49
                                          :sl 20.0 :tp 36.0
                                          :entry '(and wr-parent-entry)
                                          :exit '(and wr-parent-exit)
                                          :indicators '((sma 20))))
         (p2 (swimmy.school:make-strategy :name "UT-LOGIC-COMB-P2"
                                          :category :trend :timeframe 3600 :direction :BOTH :symbol "USDJPY"
                                          :generation 11 :rank :B
                                          :profit-factor 1.46 :win-rate 0.35
                                          :sl 24.0 :tp 44.0
                                          :entry '(and pf-parent-entry)
                                          :exit '(and pf-parent-exit)
                                          :indicators '((sma 50))))
         (child (swimmy.school::breed-strategies p1 p2)))
    (assert-equal '(and wr-parent-entry) (swimmy.school:strategy-entry child)
                  "Expected child entry to inherit from higher-WR parent")
    (assert-equal '(and pf-parent-exit) (swimmy.school:strategy-exit child)
                  "Expected child exit to inherit from higher-PF parent")))

(deftest test-strategies-correlation-ok-p-respects-configurable-distance-threshold
  "Correlation gate should respect configurable minimum genetic distance."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-CORR-P1"
                                          :entry '(and a) :exit '(and b)
                                          :sl 20.0 :tp 40.0))
         (p2 (swimmy.school:make-strategy :name "UT-CORR-P2"
                                          :entry '(and c) :exit '(and d)
                                          :sl 22.0 :tp 44.0))
         (orig-dist (symbol-function 'swimmy.school::calculate-genetic-distance))
         (orig-genome (symbol-function 'swimmy.school::extract-genome))
         (orig-threshold (and (boundp 'swimmy.school::*breeder-min-genetic-distance*)
                              swimmy.school::*breeder-min-genetic-distance*)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::extract-genome)
                (lambda (_s) (declare (ignore _s)) '(mock-genome)))
          (setf (symbol-function 'swimmy.school::calculate-genetic-distance)
                (lambda (_g1 _g2) (declare (ignore _g1 _g2)) 0.16))
          (setf swimmy.school::*breeder-min-genetic-distance* 0.15)
          (assert-true (swimmy.school::strategies-correlation-ok-p p1 p2)
                       "Expected pair to pass when distance (0.16) is above threshold (0.15)")
          (setf swimmy.school::*breeder-min-genetic-distance* 0.17)
          (assert-false (swimmy.school::strategies-correlation-ok-p p1 p2)
                        "Expected pair to fail when distance (0.16) is below threshold (0.17)"))
      (setf (symbol-function 'swimmy.school::calculate-genetic-distance) orig-dist)
      (setf (symbol-function 'swimmy.school::extract-genome) orig-genome)
      (if orig-threshold
          (setf swimmy.school::*breeder-min-genetic-distance* orig-threshold)
          (makunbound 'swimmy.school::*breeder-min-genetic-distance*)))))

(deftest test-strategies-correlation-ok-p-honors-dynamic-min-distance-override
  "Correlation gate should allow per-pair dynamic min-distance override."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-CORR-DYN-P1"
                                          :entry '(and a) :exit '(and b)
                                          :sl 20.0 :tp 40.0))
         (p2 (swimmy.school:make-strategy :name "UT-CORR-DYN-P2"
                                          :entry '(and c) :exit '(and d)
                                          :sl 22.0 :tp 44.0))
         (orig-dist (symbol-function 'swimmy.school::calculate-genetic-distance))
         (orig-genome (symbol-function 'swimmy.school::extract-genome))
         (orig-threshold (and (boundp 'swimmy.school::*breeder-min-genetic-distance*)
                              swimmy.school::*breeder-min-genetic-distance*)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::extract-genome)
                (lambda (_s) (declare (ignore _s)) '(mock-genome)))
          (setf (symbol-function 'swimmy.school::calculate-genetic-distance)
                (lambda (_g1 _g2) (declare (ignore _g1 _g2)) 0.12))
          (setf swimmy.school::*breeder-min-genetic-distance* 0.15)
          (assert-false (swimmy.school::strategies-correlation-ok-p p1 p2)
                        "Expected fail with default threshold 0.15 and distance 0.12")
          (let ((swimmy.school::*breeder-current-pair-min-distance* 0.10))
            (assert-true (swimmy.school::strategies-correlation-ok-p p1 p2)
                         "Expected pass with dynamic override threshold 0.10 and distance 0.12")))
      (setf (symbol-function 'swimmy.school::calculate-genetic-distance) orig-dist)
      (setf (symbol-function 'swimmy.school::extract-genome) orig-genome)
      (if orig-threshold
          (setf swimmy.school::*breeder-min-genetic-distance* orig-threshold)
          (makunbound 'swimmy.school::*breeder-min-genetic-distance*)))))

(deftest test-find-diverse-breeding-partner-relaxes-distance-for-complement
  "Complement partner should pass correlation gate with relaxed distance threshold."
  (let* ((parent (swimmy.school:make-strategy :name "UT-CORR-COMP-PARENT"
                                              :rank :B :generation 90
                                              :sl 20.0 :tp 60.0
                                              :profit-factor 1.45 :win-rate 0.34
                                              :sharpe 1.2))
         (cand-non-complement (swimmy.school:make-strategy :name "UT-CORR-COMP-NON"
                                                           :rank :LEGEND :generation 1400
                                                           :sl 20.0 :tp 30.0
                                                           :profit-factor 1.55 :win-rate 0.35
                                                           :sharpe 2.8))
         (cand-complement (swimmy.school:make-strategy :name "UT-CORR-COMP-YES"
                                                       :rank :B :generation 8
                                                       :sl 20.0 :tp 30.0
                                                       :profit-factor 1.22 :win-rate 0.49
                                                       :sharpe 0.3))
         (orig-dist (symbol-function 'swimmy.school::calculate-genetic-distance))
         (orig-genome (symbol-function 'swimmy.school::extract-genome))
         (orig-threshold (and (boundp 'swimmy.school::*breeder-min-genetic-distance*)
                              swimmy.school::*breeder-min-genetic-distance*))
         (orig-comp-threshold (and (boundp 'swimmy.school::*breeder-min-genetic-distance-complement*)
                                   swimmy.school::*breeder-min-genetic-distance-complement*)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::extract-genome)
                (lambda (_s) (declare (ignore _s)) '(mock-genome)))
          (setf (symbol-function 'swimmy.school::calculate-genetic-distance)
                (lambda (_g1 _g2) (declare (ignore _g1 _g2)) 0.12))
          (setf swimmy.school::*breeder-min-genetic-distance* 0.15)
          (setf swimmy.school::*breeder-min-genetic-distance-complement* 0.10)
          (let ((picked (swimmy.school::find-diverse-breeding-partner
                         parent (list parent cand-non-complement cand-complement)
                         :start-index 1)))
            (assert-true (eq picked cand-complement)
                         (format nil "Expected relaxed-distance complement partner UT-CORR-COMP-YES, got ~a"
                                 (if picked (swimmy.school:strategy-name picked) :none)))))
      (setf (symbol-function 'swimmy.school::calculate-genetic-distance) orig-dist)
      (setf (symbol-function 'swimmy.school::extract-genome) orig-genome)
      (if orig-threshold
          (setf swimmy.school::*breeder-min-genetic-distance* orig-threshold)
          (makunbound 'swimmy.school::*breeder-min-genetic-distance*))
      (if orig-comp-threshold
          (setf swimmy.school::*breeder-min-genetic-distance-complement* orig-comp-threshold)
          (makunbound 'swimmy.school::*breeder-min-genetic-distance-complement*)))))
