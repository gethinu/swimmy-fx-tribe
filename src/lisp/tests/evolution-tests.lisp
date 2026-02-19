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
         (name-b nil)
         ;; Isolate this test from production graveyard/retired files (can be huge/corrupt).
         (tmp-dir (uiop:temporary-directory))
         (gy-path (merge-pathnames
                   (format nil "swimmy_breed_graveyard_~a.sexp" (get-universal-time))
                   tmp-dir))
         (retired-path (merge-pathnames
                        (format nil "swimmy_breed_retired_~a.sexp" (get-universal-time))
                        tmp-dir)))
    (unwind-protect
        (progn
          (when (probe-file gy-path) (ignore-errors (delete-file gy-path)))
          (when (probe-file retired-path) (ignore-errors (delete-file retired-path)))
          (let ((swimmy.school::*graveyard-file* (namestring gy-path))
                (swimmy.school::*retired-file* (namestring retired-path)))
            (let ((*random-state* (make-random-state seed)))
              (setf name-a (swimmy.school:strategy-name (swimmy.school::breed-strategies p1 p2))))
            (let ((*random-state* (make-random-state seed)))
              (setf name-b (swimmy.school:strategy-name (swimmy.school::breed-strategies p1 p2))))
            (assert-true (string/= name-a name-b)
                         (format nil "Expected unique names, got ~a and ~a" name-a name-b))))
      (when (probe-file gy-path) (ignore-errors (delete-file gy-path)))
      (when (probe-file retired-path) (ignore-errors (delete-file retired-path))))))

(deftest test-breed-strategies-timeframe-crossover-can-use-parent2
  "Breeder should allow TF crossover so child can inherit parent2 timeframe."
  (let* ((p1 (swimmy.school:make-strategy
              :name "UT-TF-XOVER-P1"
              :category :trend
              :timeframe 60
              :direction :BOTH
              :symbol "USDJPY"
              :generation 10
              :sl 0.8
              :tp 1.6
              :indicators '((sma 20))
              :entry '(> close sma-20)
              :exit '(< close sma-20)))
         (p2 (swimmy.school:make-strategy
              :name "UT-TF-XOVER-P2"
              :category :trend
              :timeframe 240
              :direction :BOTH
              :symbol "USDJPY"
              :generation 9
              :sl 0.9
              :tp 1.8
              :indicators '((sma 50))
              :entry '(> close sma-50)
              :exit '(< close sma-50)))
         (crossover-rate-sym 'swimmy.school::*breeder-timeframe-crossover-rate*)
         (mutation-rate-sym 'swimmy.school::*breeder-timeframe-mutation-rate*)
         (has-crossover-rate (boundp crossover-rate-sym))
         (has-mutation-rate (boundp mutation-rate-sym))
         (orig-crossover-rate (and has-crossover-rate (symbol-value crossover-rate-sym)))
         (orig-mutation-rate (and has-mutation-rate (symbol-value mutation-rate-sym)))
         (orig-q (and (fboundp 'swimmy.school::select-sltp-with-q)
                      (symbol-function 'swimmy.school::select-sltp-with-q))))
    (unwind-protect
        (progn
          (when has-crossover-rate
            (setf (symbol-value crossover-rate-sym) 1.0))
          (when has-mutation-rate
            (setf (symbol-value mutation-rate-sym) 0.0))
          (when (fboundp 'swimmy.school::select-sltp-with-q)
            (setf (symbol-function 'swimmy.school::select-sltp-with-q)
                  (lambda (_tf _dir _sym fallback-sl fallback-tp)
                    (declare (ignore _tf _dir _sym))
                    (values fallback-sl fallback-tp))))
          (let ((child (swimmy.school::breed-strategies p1 p2)))
            (assert-equal 240
                          (swimmy.school:strategy-timeframe child)
                          "Expected child timeframe to crossover from parent2")))
      (when has-crossover-rate
        (setf (symbol-value crossover-rate-sym) orig-crossover-rate))
      (when has-mutation-rate
        (setf (symbol-value mutation-rate-sym) orig-mutation-rate))
      (when orig-q
        (setf (symbol-function 'swimmy.school::select-sltp-with-q) orig-q)))))

(deftest test-breed-strategies-timeframe-mutation-diversifies-same-parent-tf
  "Breeder TF mutation should diversify child timeframe even when both parents share same TF."
  (let* ((p1 (swimmy.school:make-strategy
              :name "UT-TF-MUT-P1"
              :category :trend
              :timeframe 60
              :direction :BOTH
              :symbol "USDJPY"
              :generation 10
              :sl 0.8
              :tp 1.6
              :indicators '((sma 20))
              :entry '(> close sma-20)
              :exit '(< close sma-20)))
         (p2 (swimmy.school:make-strategy
              :name "UT-TF-MUT-P2"
              :category :trend
              :timeframe 60
              :direction :BOTH
              :symbol "USDJPY"
              :generation 9
              :sl 0.9
              :tp 1.8
              :indicators '((sma 50))
              :entry '(> close sma-50)
              :exit '(< close sma-50)))
         (crossover-rate-sym 'swimmy.school::*breeder-timeframe-crossover-rate*)
         (mutation-rate-sym 'swimmy.school::*breeder-timeframe-mutation-rate*)
         (has-crossover-rate (boundp crossover-rate-sym))
         (has-mutation-rate (boundp mutation-rate-sym))
         (orig-crossover-rate (and has-crossover-rate (symbol-value crossover-rate-sym)))
         (orig-mutation-rate (and has-mutation-rate (symbol-value mutation-rate-sym)))
         (orig-options (and (fboundp 'swimmy.school::get-tf-mutation-options)
                            (symbol-function 'swimmy.school::get-tf-mutation-options)))
         (orig-q (and (fboundp 'swimmy.school::select-sltp-with-q)
                      (symbol-function 'swimmy.school::select-sltp-with-q))))
    (unwind-protect
        (progn
          (when has-crossover-rate
            (setf (symbol-value crossover-rate-sym) 0.0))
          (when has-mutation-rate
            (setf (symbol-value mutation-rate-sym) 1.0))
          (when (fboundp 'swimmy.school::get-tf-mutation-options)
            (setf (symbol-function 'swimmy.school::get-tf-mutation-options)
                  (lambda () '(60 240))))
          (when (fboundp 'swimmy.school::select-sltp-with-q)
            (setf (symbol-function 'swimmy.school::select-sltp-with-q)
                  (lambda (_tf _dir _sym fallback-sl fallback-tp)
                    (declare (ignore _tf _dir _sym))
                    (values fallback-sl fallback-tp))))
          (let ((child (swimmy.school::breed-strategies p1 p2)))
            (assert-equal 240
                          (swimmy.school:strategy-timeframe child)
                          "Expected TF mutation to move child away from shared parent TF")))
      (when has-crossover-rate
        (setf (symbol-value crossover-rate-sym) orig-crossover-rate))
      (when has-mutation-rate
        (setf (symbol-value mutation-rate-sym) orig-mutation-rate))
      (when orig-options
        (setf (symbol-function 'swimmy.school::get-tf-mutation-options) orig-options))
      (when orig-q
        (setf (symbol-function 'swimmy.school::select-sltp-with-q) orig-q)))))

(deftest test-select-breeder-child-timeframe-boosts-mutation-when-parents-share-tf
  "When parents share TF, breeder should use the dedicated mutation-rate boost for diversification."
  (let* ((p1 (swimmy.school:make-strategy
              :name "UT-TF-BOOST-P1"
              :category :trend
              :timeframe 60
              :direction :BOTH
              :symbol "USDJPY"
              :generation 10
              :sl 0.8
              :tp 1.6
              :indicators '((sma 20))
              :entry '(> close sma-20)
              :exit '(< close sma-20)))
         (p2 (swimmy.school:make-strategy
              :name "UT-TF-BOOST-P2"
              :category :trend
              :timeframe 60
              :direction :BOTH
              :symbol "USDJPY"
              :generation 9
              :sl 0.9
              :tp 1.8
              :indicators '((sma 50))
              :entry '(> close sma-50)
              :exit '(< close sma-50)))
         (mutation-rate-sym 'swimmy.school::*breeder-timeframe-mutation-rate*)
         (same-tf-rate-sym 'swimmy.school::*breeder-timeframe-mutation-same-parent-rate*)
         (has-mutation-rate (boundp mutation-rate-sym))
         (has-same-tf-rate (boundp same-tf-rate-sym))
         (orig-mutation-rate (and has-mutation-rate (symbol-value mutation-rate-sym)))
         (orig-same-tf-rate (and has-same-tf-rate (symbol-value same-tf-rate-sym)))
         (orig-options (and (fboundp 'swimmy.school::get-tf-mutation-options)
                            (symbol-function 'swimmy.school::get-tf-mutation-options))))
    (unwind-protect
        (progn
          (assert-true has-same-tf-rate
                       "Expected dedicated same-parent TF mutation-rate parameter")
          (when has-mutation-rate
            (setf (symbol-value mutation-rate-sym) 0.0))
          (when has-same-tf-rate
            (setf (symbol-value same-tf-rate-sym) 1.0))
          (when (fboundp 'swimmy.school::get-tf-mutation-options)
            (setf (symbol-function 'swimmy.school::get-tf-mutation-options)
                  (lambda () '(60 240))))
          (multiple-value-bind (tf mode _p1 _p2)
              (swimmy.school::select-breeder-child-timeframe p1 p2)
            (declare (ignore _p1 _p2))
            (assert-equal :mutation mode
                          "Expected same-parent TF boost to force mutation path")
            (assert-equal 240 tf
                          "Expected same-parent TF boost to diversify away from shared TF")))
      (when has-mutation-rate
        (setf (symbol-value mutation-rate-sym) orig-mutation-rate))
      (when has-same-tf-rate
        (setf (symbol-value same-tf-rate-sym) orig-same-tf-rate))
      (when orig-options
        (setf (symbol-function 'swimmy.school::get-tf-mutation-options) orig-options)))))

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
         (orig-upside-enabled (and (boundp 'swimmy.school::*pfwr-upside-scale-enabled*)
                                   swimmy.school::*pfwr-upside-scale-enabled*))
         (orig-target-pf swimmy.school::*pfwr-target-pf*)
         (orig-target-wr swimmy.school::*pfwr-target-wr*))
    (unwind-protect
        (progn
          (setf swimmy.school::*pfwr-mutation-bias-enabled* t)
          (when (boundp 'swimmy.school::*pfwr-upside-scale-enabled*)
            (setf swimmy.school::*pfwr-upside-scale-enabled* nil))
          (setf swimmy.school::*pfwr-target-pf* 1.30)
          (setf swimmy.school::*pfwr-target-wr* 0.43)
          (multiple-value-bind (sl tp)
              (swimmy.school::apply-pfwr-mutation-bias 20.0 60.0 p1 p2)
            (assert-true (< (abs (- sl 20.0)) 1.0e-6)
                         "Expected SL unchanged when parents are already healthy")
            (assert-true (< (abs (- tp 60.0)) 1.0e-6)
                         "Expected TP unchanged when parents are already healthy")))
      (setf swimmy.school::*pfwr-mutation-bias-enabled* orig-enabled)
      (when (boundp 'swimmy.school::*pfwr-upside-scale-enabled*)
        (setf swimmy.school::*pfwr-upside-scale-enabled* orig-upside-enabled))
      (setf swimmy.school::*pfwr-target-pf* orig-target-pf)
      (setf swimmy.school::*pfwr-target-wr* orig-target-wr))))

(deftest test-pfwr-mutation-bias-uses-s-target-when-parents-a-or-above
  "PF/WR mutation bias should treat S PF/WR gates as targets when parents are A-rank or above."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-PFWR-S-TARGET-A1"
                                          :rank :A
                                          :profit-factor 1.40 :win-rate 0.45
                                          :sl 30.0 :tp 60.0))
         (p2 (swimmy.school:make-strategy :name "UT-PFWR-S-TARGET-A2"
                                          :rank :A
                                          :profit-factor 1.40 :win-rate 0.45
                                          :sl 30.0 :tp 60.0))
         (orig-enabled swimmy.school::*pfwr-mutation-bias-enabled*)
         (orig-strength swimmy.school::*pfwr-mutation-bias-strength*)
         (orig-target-pf swimmy.school::*pfwr-target-pf*)
         (orig-target-wr swimmy.school::*pfwr-target-wr*)
         (orig-upside-enabled (and (boundp 'swimmy.school::*pfwr-upside-scale-enabled*)
                                   swimmy.school::*pfwr-upside-scale-enabled*)))
    (unwind-protect
        (progn
          (setf swimmy.school::*pfwr-mutation-bias-enabled* t)
          (setf swimmy.school::*pfwr-mutation-bias-strength* 1.0)
          (setf swimmy.school::*pfwr-target-pf* 1.30)
          (setf swimmy.school::*pfwr-target-wr* 0.43)
          (when (boundp 'swimmy.school::*pfwr-upside-scale-enabled*)
            (setf swimmy.school::*pfwr-upside-scale-enabled* nil))
          (multiple-value-bind (sl tp)
              (swimmy.school::apply-pfwr-mutation-bias 40.0 20.0 p1 p2)
            (let ((rr (/ tp sl)))
              (assert-true (>= rr (float swimmy.school::*pfwr-pf-recovery-min-rr*))
                           (format nil "Expected A-rank parents to use S-target PF/WR and enforce PF-recovery RR floor; got rr=~,3f" rr)))))
      (setf swimmy.school::*pfwr-mutation-bias-enabled* orig-enabled)
      (setf swimmy.school::*pfwr-mutation-bias-strength* orig-strength)
      (setf swimmy.school::*pfwr-target-pf* orig-target-pf)
      (setf swimmy.school::*pfwr-target-wr* orig-target-wr)
      (when (boundp 'swimmy.school::*pfwr-upside-scale-enabled*)
        (setf swimmy.school::*pfwr-upside-scale-enabled* orig-upside-enabled)))))

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
              (assert-true (<= rr 1.72)
                           (format nil "Expected complement stabilization RR<=1.72, got ~,3f" rr))
              (assert-true (>= rr 1.55)
                           (format nil "Expected complement stabilization RR>=1.55, got ~,3f" rr)))))
      (setf swimmy.school::*pfwr-mutation-bias-enabled* orig-enabled)
      (setf swimmy.school::*pfwr-mutation-bias-strength* orig-strength)
      (setf swimmy.school::*pfwr-target-pf* orig-target-pf)
      (setf swimmy.school::*pfwr-target-wr* orig-target-wr))))

(deftest test-pfwr-mutation-bias-opposite-complements-shifts-to-pf-recovery-band-when-wr-ready
  "Opposite complements with WR ready but slight PF gap should use a higher RR PF-recovery band."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-PFWR-COMP-PFREC-WR"
                                          :profit-factor 1.20 :win-rate 0.52
                                          :sl 20.0 :tp 30.0))
         (p2 (swimmy.school:make-strategy :name "UT-PFWR-COMP-PFREC-PF"
                                          :profit-factor 1.38 :win-rate 0.36
                                          :sl 20.0 :tp 80.0))
         (orig-enabled swimmy.school::*pfwr-mutation-bias-enabled*)
         (orig-strength swimmy.school::*pfwr-mutation-bias-strength*)
         (orig-target-pf swimmy.school::*pfwr-target-pf*)
         (orig-target-wr swimmy.school::*pfwr-target-wr*)
         (orig-comp-pf-min swimmy.school::*pfwr-complement-pf-recovery-min-rr*)
         (orig-comp-pf-max swimmy.school::*pfwr-complement-pf-recovery-max-rr*))
    (unwind-protect
        (progn
          (setf swimmy.school::*pfwr-mutation-bias-enabled* t)
          (setf swimmy.school::*pfwr-mutation-bias-strength* 1.0)
          (setf swimmy.school::*pfwr-target-pf* 1.30)
          (setf swimmy.school::*pfwr-target-wr* 0.43)
          (setf swimmy.school::*pfwr-complement-pf-recovery-min-rr* 1.75)
          (setf swimmy.school::*pfwr-complement-pf-recovery-max-rr* 2.05)
          (multiple-value-bind (sl tp)
              (swimmy.school::apply-pfwr-mutation-bias 20.0 100.0 p1 p2)
            (let ((rr (/ tp sl)))
              (assert-true (>= rr 1.75)
                           (format nil "Expected PF-recovery complement RR>=1.75, got ~,3f" rr))
              (assert-true (<= rr 2.05)
                           (format nil "Expected PF-recovery complement RR<=2.05, got ~,3f" rr)))))
      (setf swimmy.school::*pfwr-mutation-bias-enabled* orig-enabled)
      (setf swimmy.school::*pfwr-mutation-bias-strength* orig-strength)
      (setf swimmy.school::*pfwr-target-pf* orig-target-pf)
      (setf swimmy.school::*pfwr-target-wr* orig-target-wr)
      (setf swimmy.school::*pfwr-complement-pf-recovery-min-rr* orig-comp-pf-min)
      (setf swimmy.school::*pfwr-complement-pf-recovery-max-rr* orig-comp-pf-max))))

(deftest test-pfwr-mutation-bias-upside-scale-boosts-wr-ready-pf-gap-to-s-target
  "WR-ready parents below S-like PF target should get extra scale boost even when A-target gaps are zero."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-PFWR-UPSIDE-P1"
                                          :profit-factor 1.48 :win-rate 0.52
                                          :sl 20.0 :tp 40.0))
         (p2 (swimmy.school:make-strategy :name "UT-PFWR-UPSIDE-P2"
                                          :profit-factor 1.52 :win-rate 0.50
                                          :sl 22.0 :tp 44.0))
         (orig-enabled swimmy.school::*pfwr-mutation-bias-enabled*)
         (orig-target-pf swimmy.school::*pfwr-target-pf*)
         (orig-target-wr swimmy.school::*pfwr-target-wr*)
         (orig-upside-enabled (and (boundp 'swimmy.school::*pfwr-upside-scale-enabled*)
                                   swimmy.school::*pfwr-upside-scale-enabled*))
         (orig-upside-target (and (boundp 'swimmy.school::*pfwr-upside-target-pf*)
                                  swimmy.school::*pfwr-upside-target-pf*))
         (orig-upside-min-wr (and (boundp 'swimmy.school::*pfwr-upside-min-wr*)
                                  swimmy.school::*pfwr-upside-min-wr*))
         (orig-upside-gain (and (boundp 'swimmy.school::*pfwr-upside-scale-gain*)
                                swimmy.school::*pfwr-upside-scale-gain*))
         (orig-upside-max (and (boundp 'swimmy.school::*pfwr-upside-scale-max*)
                               swimmy.school::*pfwr-upside-scale-max*)))
    (unwind-protect
        (progn
          (setf swimmy.school::*pfwr-mutation-bias-enabled* t)
          (setf swimmy.school::*pfwr-target-pf* 1.30)
          (setf swimmy.school::*pfwr-target-wr* 0.43)
          (setf swimmy.school::*pfwr-upside-scale-enabled* t)
          (setf swimmy.school::*pfwr-upside-target-pf* 1.70)
          (setf swimmy.school::*pfwr-upside-min-wr* 0.47)
          (setf swimmy.school::*pfwr-upside-scale-gain* 0.30)
          (setf swimmy.school::*pfwr-upside-scale-max* 1.40)
          (multiple-value-bind (sl tp)
              (swimmy.school::apply-pfwr-mutation-bias 20.0 40.0 p1 p2)
            (assert-true (> sl 20.5)
                         (format nil "Expected upside mode to scale SL above baseline, got ~,3f" sl))
            (assert-true (> tp 40.5)
                         (format nil "Expected upside mode to scale TP above baseline, got ~,3f" tp))))
      (setf swimmy.school::*pfwr-mutation-bias-enabled* orig-enabled)
      (setf swimmy.school::*pfwr-target-pf* orig-target-pf)
      (setf swimmy.school::*pfwr-target-wr* orig-target-wr)
      (setf swimmy.school::*pfwr-upside-scale-enabled* orig-upside-enabled)
      (setf swimmy.school::*pfwr-upside-target-pf* orig-upside-target)
      (setf swimmy.school::*pfwr-upside-min-wr* orig-upside-min-wr)
      (setf swimmy.school::*pfwr-upside-scale-gain* orig-upside-gain)
      (setf swimmy.school::*pfwr-upside-scale-max* orig-upside-max))))

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
              (assert-true (>= rr 1.89)
                           (format nil "Expected moderate PF gap to keep RR floor near 1.9, got ~,3f" rr)))))
      (setf swimmy.school::*pfwr-mutation-bias-enabled* orig-enabled)
      (setf swimmy.school::*pfwr-mutation-bias-strength* orig-strength)
      (setf swimmy.school::*pfwr-target-pf* orig-target-pf)
      (setf swimmy.school::*pfwr-target-wr* orig-target-wr))))

(deftest test-pfwr-mutation-bias-severe-low-pf-enforces-stronger-floor
  "Severe low-PF with WR-ready parents should trigger stronger RR and scale floors."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-PFWR-SEVERE1"
                                          :profit-factor 1.02 :win-rate 0.55
                                          :sl 40.0 :tp 28.0))
         (p2 (swimmy.school:make-strategy :name "UT-PFWR-SEVERE2"
                                          :profit-factor 1.08 :win-rate 0.54
                                          :sl 38.0 :tp 26.0))
         (orig-enabled swimmy.school::*pfwr-mutation-bias-enabled*)
         (orig-strength swimmy.school::*pfwr-mutation-bias-strength*)
         (orig-target-pf swimmy.school::*pfwr-target-pf*)
         (orig-target-wr swimmy.school::*pfwr-target-wr*)
         (orig-severe-pf swimmy.school::*pfwr-severe-low-pf-threshold*)
         (orig-severe-wr swimmy.school::*pfwr-severe-wr-ready-threshold*)
         (orig-severe-rr swimmy.school::*pfwr-severe-min-rr*)
         (orig-severe-scale swimmy.school::*pfwr-severe-scale-floor*))
    (unwind-protect
        (progn
          (setf swimmy.school::*pfwr-mutation-bias-enabled* t)
          (setf swimmy.school::*pfwr-mutation-bias-strength* 1.0)
          (setf swimmy.school::*pfwr-target-pf* 1.30)
          (setf swimmy.school::*pfwr-target-wr* 0.43)
          (setf swimmy.school::*pfwr-severe-low-pf-threshold* 1.15)
          (setf swimmy.school::*pfwr-severe-wr-ready-threshold* 0.43)
          (setf swimmy.school::*pfwr-severe-min-rr* 2.25)
          (setf swimmy.school::*pfwr-severe-scale-floor* 1.30)
          (multiple-value-bind (sl tp)
              (swimmy.school::apply-pfwr-mutation-bias 40.0 20.0 p1 p2)
            (let ((rr (/ tp sl))
                  (total (+ sl tp)))
              (assert-true (>= rr 2.24)
                           (format nil "Expected severe PF mode RR>=2.25, got ~,3f" rr))
              (assert-true (>= total 77.9)
                           (format nil "Expected severe PF mode scaled risk budget >=78.0, got ~,3f" total)))))
      (setf swimmy.school::*pfwr-mutation-bias-enabled* orig-enabled)
      (setf swimmy.school::*pfwr-mutation-bias-strength* orig-strength)
      (setf swimmy.school::*pfwr-target-pf* orig-target-pf)
      (setf swimmy.school::*pfwr-target-wr* orig-target-wr)
      (setf swimmy.school::*pfwr-severe-low-pf-threshold* orig-severe-pf)
      (setf swimmy.school::*pfwr-severe-wr-ready-threshold* orig-severe-wr)
      (setf swimmy.school::*pfwr-severe-min-rr* orig-severe-rr)
      (setf swimmy.school::*pfwr-severe-scale-floor* orig-severe-scale))))

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
            (assert-true (>= (+ sl tp) 56.0)
                         (format nil "Expected WR-only PF recovery scale >=56.0, got ~,3f" (+ sl tp)))))
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

(deftest test-strategy-breeding-priority-score-prefers-high-cpcv-pass-rate
  "Breeding priority should prefer higher CPCV pass-rate when base metrics tie."
  (let* ((base (list :rank :A
                     :generation 4
                     :sharpe 1.20
                     :profit-factor 1.55
                     :win-rate 0.48
                     :max-dd 0.05))
         (low (apply #'swimmy.school:make-strategy
                     (append (list :name "UT-PRIORITY-CPCV-LOW"
                                   :cpcv-pass-rate 0.20)
                             base)))
         (high (apply #'swimmy.school:make-strategy
                      (append (list :name "UT-PRIORITY-CPCV-HIGH"
                                    :cpcv-pass-rate 0.80)
                              base)))
         (low-score (swimmy.school::strategy-breeding-priority-score low))
         (high-score (swimmy.school::strategy-breeding-priority-score high)))
    (assert-true (> high-score low-score)
                 (format nil "Expected high-CPCV score (~,3f) > low-CPCV score (~,3f)"
                         high-score low-score))))

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

(deftest test-clamp-child-sltp-to-parent-envelope-limits-explosive-values
  "Child SL/TP should be capped to a parent-relative envelope to avoid no-trade explosions."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-SLTP-CAP-P1"
                                          :sl 0.22 :tp 0.31
                                          :profit-factor 1.18 :win-rate 0.48))
         (p2 (swimmy.school:make-strategy :name "UT-SLTP-CAP-P2"
                                          :sl 0.18 :tp 0.29
                                          :profit-factor 1.16 :win-rate 0.47))
         (orig-cap (and (boundp 'swimmy.school::*breeder-sltp-parent-multiplier-cap*)
                        swimmy.school::*breeder-sltp-parent-multiplier-cap*)))
    (unwind-protect
        (progn
          (setf swimmy.school::*breeder-sltp-parent-multiplier-cap* 2.0)
          (multiple-value-bind (sl tp)
              (swimmy.school::clamp-child-sltp-to-parent-envelope 1.80 3.00 p1 p2)
            (assert-true (<= sl 0.441)
                         (format nil "Expected SL cap <=0.44, got ~,3f" sl))
            (assert-true (<= tp 0.621)
                         (format nil "Expected TP cap <=0.62, got ~,3f" tp))
            (assert-true (>= sl 0.1)
                         (format nil "Expected SL floor >=0.1, got ~,3f" sl))
            (assert-true (>= tp 0.1)
                         (format nil "Expected TP floor >=0.1, got ~,3f" tp))))
      (if orig-cap
          (setf swimmy.school::*breeder-sltp-parent-multiplier-cap* orig-cap)
          (makunbound 'swimmy.school::*breeder-sltp-parent-multiplier-cap*)))))

(deftest test-breed-strategies-borrows-logic-when-parents-are-empty
  "Breeder should borrow entry/exit/indicators from same-category donor when parents have NIL logic."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-LOGIC-NIL-P1"
                                          :category :trend :timeframe 300 :symbol "USDJPY"
                                          :entry nil :exit nil :indicators nil
                                          :sl 0.22 :tp 0.31 :rank :B :generation 10))
         (p2 (swimmy.school:make-strategy :name "UT-LOGIC-NIL-P2"
                                          :category :trend :timeframe 300 :symbol "USDJPY"
                                          :entry nil :exit nil :indicators nil
                                          :sl 0.20 :tp 0.29 :rank :B :generation 9))
         (donor (swimmy.school:make-strategy :name "UT-LOGIC-DONOR"
                                             :category :trend :timeframe 300 :symbol "USDJPY"
                                             :entry '(> close sma-20)
                                             :exit '(< close sma-20)
                                             :indicators '((sma 20))
                                             :sl 0.18 :tp 0.27 :rank :legend :generation 100))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-q (and (fboundp 'swimmy.school::select-sltp-with-q)
                      (symbol-function 'swimmy.school::select-sltp-with-q))))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list p1 p2 donor))
          ;; Keep Q-selection deterministic and non-intrusive for this test.
          (when (fboundp 'swimmy.school::select-sltp-with-q)
            (setf (symbol-function 'swimmy.school::select-sltp-with-q)
                  (lambda (_tf _dir _sym fallback-sl fallback-tp)
                    (declare (ignore _tf _dir _sym))
                    (values fallback-sl fallback-tp))))
          (let ((child (swimmy.school::breed-strategies p1 p2)))
            (assert-true (not (null (swimmy.school:strategy-entry child)))
                         "Expected borrowed non-NIL entry logic")
            (assert-true (not (null (swimmy.school:strategy-exit child)))
                         "Expected borrowed non-NIL exit logic")
            (assert-true (not (null (swimmy.school:strategy-indicators child)))
                         "Expected borrowed non-NIL indicators")))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (when orig-q
        (setf (symbol-function 'swimmy.school::select-sltp-with-q) orig-q)))))

(deftest test-breed-strategies-uses-default-logic-when-no-donor
  "Breeder should inject default logic genes when parents are empty and no donor exists."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-LOGIC-DEF-P1"
                                          :category :trend :timeframe 300 :symbol "USDJPY"
                                          :entry nil :exit nil :indicators nil
                                          :sl 0.22 :tp 0.31 :rank :B :generation 10))
         (p2 (swimmy.school:make-strategy :name "UT-LOGIC-DEF-P2"
                                          :category :trend :timeframe 300 :symbol "USDJPY"
                                          :entry nil :exit nil :indicators nil
                                          :sl 0.20 :tp 0.29 :rank :B :generation 9))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-q (and (fboundp 'swimmy.school::select-sltp-with-q)
                      (symbol-function 'swimmy.school::select-sltp-with-q))))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base* (list p1 p2))
          (when (fboundp 'swimmy.school::select-sltp-with-q)
            (setf (symbol-function 'swimmy.school::select-sltp-with-q)
                  (lambda (_tf _dir _sym fallback-sl fallback-tp)
                    (declare (ignore _tf _dir _sym))
                    (values fallback-sl fallback-tp))))
          (let ((child (swimmy.school::breed-strategies p1 p2)))
            (assert-true (not (null (swimmy.school:strategy-entry child)))
                         "Expected default non-NIL entry logic")
            (assert-true (not (null (swimmy.school:strategy-exit child)))
                         "Expected default non-NIL exit logic")
            (assert-true (not (null (swimmy.school:strategy-indicators child)))
                         "Expected default non-NIL indicators")))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (when orig-q
        (setf (symbol-function 'swimmy.school::select-sltp-with-q) orig-q)))))

(deftest test-find-diverse-breeding-partner-falls-back-past-similar-neighbor
  "Breeder should scan forward for a non-similar partner when immediate neighbor is too similar."
  (let* ((p1 (swimmy.school:make-strategy :name "UT-P1" :sl 20.0 :tp 60.0 :rank :B :trades 50))
         (p2 (swimmy.school:make-strategy :name "UT-P2" :sl 20.0 :tp 60.0 :rank :B :trades 50))
         (p3 (swimmy.school:make-strategy :name "UT-P3" :sl 20.0 :tp 60.0 :rank :B :trades 50))
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

(deftest test-find-diverse-breeding-partner-prefers-different-timeframe-when-scores-close
  "When quality is similar, breeder should prioritize partner with different timeframe."
  (let* ((parent (swimmy.school:make-strategy :name "UT-TF-DIV-PARENT"
                                              :rank :B :generation 100
                                              :timeframe 60
                                              :sl 20.0 :tp 60.0
                                              :trades 80
                                              :profit-factor 1.45 :win-rate 0.50
                                              :sharpe 1.20))
         (cand-same (swimmy.school:make-strategy :name "UT-TF-DIV-SAME"
                                                 :rank :B :generation 20
                                                 :timeframe 60
                                                 :sl 20.0 :tp 62.0
                                                 :trades 80
                                                 :profit-factor 1.40 :win-rate 0.48
                                                 :sharpe 1.10))
         (cand-diff (swimmy.school:make-strategy :name "UT-TF-DIV-DIFF"
                                                 :rank :B :generation 20
                                                 :timeframe 240
                                                 :sl 20.0 :tp 62.0
                                                 :trades 80
                                                 :profit-factor 1.40 :win-rate 0.48
                                                 :sharpe 1.10))
         (diversity-bonus-sym 'swimmy.school::*breeder-timeframe-diversity-bonus*)
         (has-diversity-bonus (boundp diversity-bonus-sym))
         (orig-diversity-bonus (and has-diversity-bonus (symbol-value diversity-bonus-sym)))
         (orig-corr (symbol-function 'swimmy.school::strategies-correlation-ok-p)))
    (unwind-protect
        (progn
          (assert-true has-diversity-bonus
                       "Expected breeder timeframe-diversity bonus parameter")
          (when has-diversity-bonus
            (setf (symbol-value diversity-bonus-sym) 1.0))
          (setf (symbol-function 'swimmy.school::strategies-correlation-ok-p)
                (lambda (_a _b)
                  (declare (ignore _a _b))
                  t))
          (let ((picked (swimmy.school::find-diverse-breeding-partner
                         parent (list parent cand-same cand-diff) :start-index 1)))
            (assert-true (eq picked cand-diff)
                         (format nil "Expected different-timeframe partner UT-TF-DIV-DIFF, got ~a"
                                 (if picked (swimmy.school:strategy-name picked) :none)))))
      (when has-diversity-bonus
        (setf (symbol-value diversity-bonus-sym) orig-diversity-bonus))
      (setf (symbol-function 'swimmy.school::strategies-correlation-ok-p) orig-corr))))

(deftest test-can-breed-p-rejects-retired-rank
  "Retired strategies must not be used as breeding parents."
  (let ((s (swimmy.school:make-strategy :name "UT-RETIRED-PARENT"
                                        :rank :retired
                                        :status :active
                                        :sl 0.22 :tp 0.31
                                        :profit-factor 1.20 :win-rate 0.47)))
    (assert-false (swimmy.school::can-breed-p s)
                  "Expected can-breed-p to reject :retired strategy")))

(deftest test-can-breed-p-rejects-incubator-rank
  "Incubator (untested) strategies must not be used as breeding parents."
  (let ((s (swimmy.school:make-strategy :name "UT-INCUBATOR-PARENT"
                                        :rank :incubator
                                        :status :active
                                        :sl 0.22 :tp 0.31
                                        :profit-factor 0.0 :win-rate 0.0
                                        :trades 0)))
    (assert-false (swimmy.school::can-breed-p s)
                  "Expected can-breed-p to reject :incubator strategy")))

(deftest test-can-breed-p-rejects-low-trade-parent
  "Low-trade parents should be excluded from breeding to avoid zero-trade cascades."
  (let* ((low (swimmy.school:make-strategy :name "UT-LOW-TRADES-PARENT"
                                           :rank :B
                                           :status :active
                                           :sl 0.22 :tp 0.31
                                           :trades 0
                                           :profit-factor 1.10 :win-rate 0.46))
         (ok (swimmy.school:make-strategy :name "UT-ENOUGH-TRADES-PARENT"
                                          :rank :B
                                          :status :active
                                          :sl 0.22 :tp 0.31
                                          :trades 50
                                          :profit-factor 1.10 :win-rate 0.46)))
    (assert-false (swimmy.school::can-breed-p low)
                  "Expected can-breed-p to reject low-trade parent")
    (assert-true (swimmy.school::can-breed-p ok)
                 "Expected can-breed-p to allow parent with enough trades")))

(deftest test-find-diverse-breeding-partner-prefers-pfwr-complement
  "Breeder should prefer a complement partner when parent has WR/PF deficit imbalance."
  (let* ((parent (swimmy.school:make-strategy :name "UT-COMP-PARENT"
                                              :rank :B :sl 20.0 :tp 60.0
                                              :trades 50
                                              :profit-factor 1.40 :win-rate 0.34))
         (cand1 (swimmy.school:make-strategy :name "UT-COMP-CAND1"
                                             :rank :B :sl 20.0 :tp 70.0
                                             :trades 50
                                             :profit-factor 1.45 :win-rate 0.35))
         (cand2 (swimmy.school:make-strategy :name "UT-COMP-CAND2"
                                             :rank :B :sl 20.0 :tp 30.0
                                             :trades 50
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
                                              :trades 50
                                              :profit-factor 1.45 :win-rate 0.34
                                              :sharpe 1.1))
         (cand-high-no-complement (swimmy.school:make-strategy :name "UT-COMP-STRICT-HIGH"
                                                               :rank :LEGEND :generation 1200
                                                               :sl 20.0 :tp 70.0
                                                               :trades 50
                                                               :profit-factor 1.60 :win-rate 0.35
                                                               :sharpe 2.6))
         (cand-low-complement (swimmy.school:make-strategy :name "UT-COMP-STRICT-LOW"
                                                           :rank :B :generation 5
                                                           :sl 20.0 :tp 30.0
                                                           :trades 50
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
                                              :trades 50
                                              :profit-factor 1.45 :win-rate 0.34
                                              :sharpe 1.2))
         (cand-high-but-weak-pf (swimmy.school:make-strategy :name "UT-COMP-FILTER-WEAK"
                                                             :rank :LEGEND :generation 1400
                                                             :sl 20.0 :tp 30.0
                                                             :trades 50
                                                             :profit-factor 1.05 :win-rate 0.53
                                                             :sharpe 2.8))
         (cand-lower-but-balanced (swimmy.school:make-strategy :name "UT-COMP-FILTER-GOOD"
                                                               :rank :B :generation 8
                                                               :sl 20.0 :tp 30.0
                                                               :trades 50
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
                                              :trades 50
                                              :profit-factor 1.12 :win-rate 0.48
                                              :sharpe 1.1))
         (cand-high-no-pf (swimmy.school:make-strategy :name "UT-NEAR-PF-HIGH-NO"
                                                       :rank :LEGEND :generation 1200
                                                       :sl 20.0 :tp 70.0
                                                       :trades 50
                                                       :profit-factor 1.10 :win-rate 0.62
                                                       :sharpe 2.4))
         (cand-near-pf (swimmy.school:make-strategy :name "UT-NEAR-PF-CAND"
                                                    :rank :B :generation 6
                                                    :sl 20.0 :tp 34.0
                                                    :trades 50
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

(deftest test-find-diverse-breeding-partner-prefers-wr-only-recovery-over-pf-only
  "For a WR-only parent, prefer WR-only higher-PF partner over PF-only low-WR partner."
  (let* ((parent (swimmy.school:make-strategy :name "UT-WRONLY-PARENT"
                                              :rank :B :generation 60
                                              :sl 20.0 :tp 60.0
                                              :trades 50
                                              :profit-factor 1.18 :win-rate 0.49
                                              :sharpe 1.0))
         (cand-pf-only (swimmy.school:make-strategy :name "UT-WRONLY-PFONLY"
                                                    :rank :LEGEND :generation 1400
                                                    :sl 20.0 :tp 80.0
                                                    :trades 50
                                                    :profit-factor 1.44 :win-rate 0.38
                                                    :sharpe 2.6))
         (cand-wr-only-high-pf (swimmy.school:make-strategy :name "UT-WRONLY-HIGHPF"
                                                            :rank :B :generation 7
                                                            :sl 20.0 :tp 35.0
                                                            :trades 50
                                                            :profit-factor 1.27 :win-rate 0.47
                                                            :sharpe 0.4))
         (orig-corr (symbol-function 'swimmy.school::strategies-correlation-ok-p)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::strategies-correlation-ok-p)
                (lambda (_a _b)
                  (declare (ignore _a _b))
                  t))
          (let ((picked (swimmy.school::find-diverse-breeding-partner
                         parent (list parent cand-pf-only cand-wr-only-high-pf)
                         :start-index 1)))
            (assert-true (eq picked cand-wr-only-high-pf)
                         (format nil "Expected WR-only high-PF candidate UT-WRONLY-HIGHPF, got ~a"
                                 (if picked (swimmy.school:strategy-name picked) :none)))))
      (setf (symbol-function 'swimmy.school::strategies-correlation-ok-p) orig-corr))))

(deftest test-find-diverse-breeding-partner-allows-wr-complement-with-moderate-pf
  "PF-only parent should accept WR complement with moderate PF to recover WR deficit."
  (let* ((parent (swimmy.school:make-strategy :name "UT-PFONLY-PARENT"
                                              :rank :B :generation 55
                                              :sl 20.0 :tp 60.0
                                              :trades 50
                                              :profit-factor 1.45 :win-rate 0.34
                                              :sharpe 1.2))
         (cand-high-no-wr (swimmy.school:make-strategy :name "UT-PFONLY-NONCOMP"
                                                       :rank :LEGEND :generation 1500
                                                       :sl 20.0 :tp 75.0
                                                       :trades 50
                                                       :profit-factor 1.62 :win-rate 0.35
                                                       :sharpe 2.7))
         (cand-wr-moderate-pf (swimmy.school:make-strategy :name "UT-PFONLY-WR-MIDPF"
                                                           :rank :B :generation 9
                                                           :sl 20.0 :tp 33.0
                                                           :trades 50
                                                           :profit-factor 1.18 :win-rate 0.49
                                                           :sharpe 0.5))
         (orig-corr (symbol-function 'swimmy.school::strategies-correlation-ok-p)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::strategies-correlation-ok-p)
                (lambda (_a _b)
                  (declare (ignore _a _b))
                  t))
          (let ((picked (swimmy.school::find-diverse-breeding-partner
                         parent (list parent cand-high-no-wr cand-wr-moderate-pf)
                         :start-index 1)))
            (assert-true (eq picked cand-wr-moderate-pf)
                         (format nil "Expected WR complement UT-PFONLY-WR-MIDPF, got ~a"
                                 (if picked (swimmy.school:strategy-name picked) :none)))))
      (setf (symbol-function 'swimmy.school::strategies-correlation-ok-p) orig-corr))))

(deftest test-find-diverse-breeding-partner-allows-low-pf-high-wr-when-parent-has-pf-surplus
  "When parent PF is far above target, low-PF high-WR partner should still be accepted for WR recovery."
  (let* ((parent (swimmy.school:make-strategy :name "UT-PF-SURPLUS-PARENT"
                                              :rank :B :generation 60
                                              :sl 20.0 :tp 60.0
                                              :trades 50
                                              :profit-factor 1.56 :win-rate 0.34
                                              :sharpe 1.3))
         (cand-high-no-wr (swimmy.school:make-strategy :name "UT-PF-SURPLUS-NONCOMP"
                                                       :rank :LEGEND :generation 1800
                                                       :sl 20.0 :tp 80.0
                                                       :trades 50
                                                       :profit-factor 1.65 :win-rate 0.35
                                                       :sharpe 2.9))
         (cand-low-pf-high-wr (swimmy.school:make-strategy :name "UT-PF-SURPLUS-WR"
                                                           :rank :B :generation 8
                                                           :sl 20.0 :tp 32.0
                                                           :trades 50
                                                           :profit-factor 1.08 :win-rate 0.50
                                                           :sharpe 0.3))
         (orig-corr (symbol-function 'swimmy.school::strategies-correlation-ok-p)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::strategies-correlation-ok-p)
                (lambda (_a _b)
                  (declare (ignore _a _b))
                  t))
          (let ((picked (swimmy.school::find-diverse-breeding-partner
                         parent (list parent cand-high-no-wr cand-low-pf-high-wr)
                         :start-index 1)))
            (assert-true (eq picked cand-low-pf-high-wr)
                         (format nil "Expected PF-surplus WR complement UT-PF-SURPLUS-WR, got ~a"
                                 (if picked (swimmy.school:strategy-name picked) :none)))))
      (setf (symbol-function 'swimmy.school::strategies-correlation-ok-p) orig-corr))))

(deftest test-find-diverse-breeding-partner-prefers-partial-wr-recovery-when-no-full-complement
  "When no full WR complement exists, breeder should prefer best partial WR-recovery candidate."
  (let* ((parent (swimmy.school:make-strategy :name "UT-PARTIAL-WR-PARENT"
                                              :rank :B :generation 65
                                              :sl 20.0 :tp 60.0
                                              :trades 50
                                              :profit-factor 1.42 :win-rate 0.35
                                              :sharpe 1.2))
         (cand-high-no-recovery (swimmy.school:make-strategy :name "UT-PARTIAL-WR-HIGH"
                                                             :rank :LEGEND :generation 1400
                                                             :sl 20.0 :tp 72.0
                                                             :trades 50
                                                             :profit-factor 1.62 :win-rate 0.36
                                                             :sharpe 2.7))
         (cand-partial-wr (swimmy.school:make-strategy :name "UT-PARTIAL-WR-CAND"
                                                       :rank :B :generation 10
                                                       :sl 20.0 :tp 34.0
                                                       :trades 50
                                                       :profit-factor 1.20 :win-rate 0.41
                                                       :sharpe 0.4))
         (orig-corr (symbol-function 'swimmy.school::strategies-correlation-ok-p)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::strategies-correlation-ok-p)
                (lambda (_a _b)
                  (declare (ignore _a _b))
                  t))
          (let ((picked (swimmy.school::find-diverse-breeding-partner
                         parent (list parent cand-high-no-recovery cand-partial-wr)
                         :start-index 1)))
            (assert-true (eq picked cand-partial-wr)
                         (format nil "Expected partial WR-recovery candidate UT-PARTIAL-WR-CAND, got ~a"
                                 (if picked (swimmy.school:strategy-name picked) :none)))))
      (setf (symbol-function 'swimmy.school::strategies-correlation-ok-p) orig-corr))))

(deftest test-breeding-pair-blacklist-blocks-candidate-before-cooldown
  "Pair blacklist should block candidates after reaching failure threshold within cooldown."
  (let* ((parent (swimmy.school:make-strategy :name "UT-BLACKLIST-PARENT"
                                              :rank :B :sl 20.0 :tp 60.0
                                              :trades 50
                                              :profit-factor 1.28 :win-rate 0.42))
         (cand (swimmy.school:make-strategy :name "UT-BLACKLIST-CAND"
                                            :rank :B :sl 20.0 :tp 34.0
                                            :trades 50
                                            :profit-factor 1.20 :win-rate 0.45))
         (orig-enabled swimmy.school::*breeder-pair-blacklist-enabled*)
         (orig-threshold swimmy.school::*breeder-pair-failure-threshold*)
         (orig-cooldown swimmy.school::*breeder-pair-blacklist-cooldown-seconds*)
         (orig-stats swimmy.school::*breeder-pair-failure-stats*))
    (unwind-protect
        (progn
          (setf swimmy.school::*breeder-pair-blacklist-enabled* t)
          (setf swimmy.school::*breeder-pair-failure-threshold* 2)
          (setf swimmy.school::*breeder-pair-blacklist-cooldown-seconds* 600)
          (setf swimmy.school::*breeder-pair-failure-stats* (make-hash-table :test 'equal))
          (swimmy.school::note-breeding-pair-failure parent cand "ut-1")
          (assert-false (swimmy.school::breeding-pair-blacklisted-p parent cand)
                        "Expected pair to remain unblocked before threshold")
          (swimmy.school::note-breeding-pair-failure parent cand "ut-2")
          (assert-true (swimmy.school::breeding-pair-blacklisted-p parent cand)
                       "Expected pair to be blacklisted after threshold"))
      (setf swimmy.school::*breeder-pair-blacklist-enabled* orig-enabled)
      (setf swimmy.school::*breeder-pair-failure-threshold* orig-threshold)
      (setf swimmy.school::*breeder-pair-blacklist-cooldown-seconds* orig-cooldown)
      (setf swimmy.school::*breeder-pair-failure-stats* orig-stats))))

(deftest test-breeding-pair-blacklist-expires-after-cooldown
  "Pair blacklist should expire after cooldown window."
  (let* ((parent (swimmy.school:make-strategy :name "UT-BLACKLIST-EXP-PARENT"
                                              :rank :B :sl 20.0 :tp 60.0
                                              :trades 50
                                              :profit-factor 1.28 :win-rate 0.42))
         (cand (swimmy.school:make-strategy :name "UT-BLACKLIST-EXP-CAND"
                                            :rank :B :sl 20.0 :tp 34.0
                                            :trades 50
                                            :profit-factor 1.20 :win-rate 0.45))
         (orig-enabled swimmy.school::*breeder-pair-blacklist-enabled*)
         (orig-threshold swimmy.school::*breeder-pair-failure-threshold*)
         (orig-cooldown swimmy.school::*breeder-pair-blacklist-cooldown-seconds*)
         (orig-stats swimmy.school::*breeder-pair-failure-stats*))
    (unwind-protect
        (progn
          (setf swimmy.school::*breeder-pair-blacklist-enabled* t)
          (setf swimmy.school::*breeder-pair-failure-threshold* 1)
          (setf swimmy.school::*breeder-pair-blacklist-cooldown-seconds* 10)
          (setf swimmy.school::*breeder-pair-failure-stats* (make-hash-table :test 'equal))
          (let ((key (swimmy.school::breeding-pair-key parent cand)))
            (setf (gethash key swimmy.school::*breeder-pair-failure-stats*)
                  (list :fails 2 :last-fail (- (get-universal-time) 20))))
          (assert-false (swimmy.school::breeding-pair-blacklisted-p parent cand)
                        "Expected blacklist to expire past cooldown"))
      (setf swimmy.school::*breeder-pair-blacklist-enabled* orig-enabled)
      (setf swimmy.school::*breeder-pair-failure-threshold* orig-threshold)
      (setf swimmy.school::*breeder-pair-blacklist-cooldown-seconds* orig-cooldown)
      (setf swimmy.school::*breeder-pair-failure-stats* orig-stats))))

(deftest test-find-diverse-breeding-partner-skips-blacklisted-pair
  "Partner search should skip pair-blacklisted candidate and pick next viable candidate."
  (let* ((parent (swimmy.school:make-strategy :name "UT-BLACKLIST-PICK-PARENT"
                                              :rank :B :generation 70
                                              :sl 20.0 :tp 60.0
                                              :trades 50
                                              :profit-factor 1.45 :win-rate 0.34
                                              :sharpe 1.2))
         (cand-blocked (swimmy.school:make-strategy :name "UT-BLACKLIST-PICK-BLOCKED"
                                                    :rank :B :generation 9
                                                    :sl 20.0 :tp 33.0
                                                    :trades 50
                                                    :profit-factor 1.20 :win-rate 0.49
                                                    :sharpe 0.8))
         (cand-fallback (swimmy.school:make-strategy :name "UT-BLACKLIST-PICK-FALLBACK"
                                                     :rank :B :generation 10
                                                     :sl 20.0 :tp 34.0
                                                     :trades 50
                                                     :profit-factor 1.19 :win-rate 0.46
                                                     :sharpe 0.4))
         (orig-enabled swimmy.school::*breeder-pair-blacklist-enabled*)
         (orig-threshold swimmy.school::*breeder-pair-failure-threshold*)
         (orig-cooldown swimmy.school::*breeder-pair-blacklist-cooldown-seconds*)
         (orig-stats swimmy.school::*breeder-pair-failure-stats*)
         (orig-corr (symbol-function 'swimmy.school::strategies-correlation-ok-p)))
    (unwind-protect
        (progn
          (setf swimmy.school::*breeder-pair-blacklist-enabled* t)
          (setf swimmy.school::*breeder-pair-failure-threshold* 1)
          (setf swimmy.school::*breeder-pair-blacklist-cooldown-seconds* 600)
          (setf swimmy.school::*breeder-pair-failure-stats* (make-hash-table :test 'equal))
          (setf (symbol-function 'swimmy.school::strategies-correlation-ok-p)
                (lambda (_a _b)
                  (declare (ignore _a _b))
                  t))
          (swimmy.school::note-breeding-pair-failure parent cand-blocked "ut-block")
          (let ((picked (swimmy.school::find-diverse-breeding-partner
                         parent (list parent cand-blocked cand-fallback)
                         :start-index 1)))
            (assert-true (eq picked cand-fallback)
                         (format nil "Expected fallback candidate UT-BLACKLIST-PICK-FALLBACK, got ~a"
                                 (if picked (swimmy.school:strategy-name picked) :none)))))
      (setf swimmy.school::*breeder-pair-blacklist-enabled* orig-enabled)
      (setf swimmy.school::*breeder-pair-failure-threshold* orig-threshold)
      (setf swimmy.school::*breeder-pair-blacklist-cooldown-seconds* orig-cooldown)
      (setf swimmy.school::*breeder-pair-failure-stats* orig-stats)
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

(deftest test-breeder-relaxed-distance-defaults-are-stricter-ordering-safe
  "Default relaxed distances should be <= base threshold and complement <= partial."
  (let ((base swimmy.school::*breeder-min-genetic-distance*)
        (complement swimmy.school::*breeder-min-genetic-distance-complement*)
        (partial swimmy.school::*breeder-min-genetic-distance-partial-recovery*))
    (assert-true (<= complement base)
                 "Complement threshold should not exceed base threshold")
    (assert-true (<= partial base)
                 "Partial-recovery threshold should not exceed base threshold")
    (assert-true (<= complement partial)
                 "Complement threshold should be <= partial-recovery threshold")))

(deftest test-find-diverse-breeding-partner-relaxes-distance-for-complement
  "Complement partner should pass correlation gate with relaxed distance threshold."
  (let* ((parent (swimmy.school:make-strategy :name "UT-CORR-COMP-PARENT"
                                              :rank :B :generation 90
                                              :sl 20.0 :tp 60.0
                                              :trades 50
                                              :profit-factor 1.45 :win-rate 0.34
                                              :sharpe 1.2))
         (cand-non-complement (swimmy.school:make-strategy :name "UT-CORR-COMP-NON"
                                                           :rank :LEGEND :generation 1400
                                                           :sl 20.0 :tp 30.0
                                                           :trades 50
                                                           :profit-factor 1.55 :win-rate 0.35
                                                           :sharpe 2.8))
         (cand-complement (swimmy.school:make-strategy :name "UT-CORR-COMP-YES"
                                                       :rank :B :generation 8
                                                       :sl 20.0 :tp 30.0
                                                       :trades 50
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

(deftest test-find-diverse-breeding-partner-relaxes-distance-for-partial-pf-recovery
  "PF-deficit parent should allow distance-relaxed partner that improves PF even below strict near-target threshold."
  (let* ((parent (swimmy.school:make-strategy :name "UT-CORR-PARTIAL-PF-PARENT"
                                              :rank :B :generation 70
                                              :sl 20.0 :tp 60.0
                                              :trades 50
                                              :profit-factor 1.12 :win-rate 0.48
                                              :sharpe 1.0))
         (cand-partial-pf (swimmy.school:make-strategy :name "UT-CORR-PARTIAL-PF-CAND"
                                                       :rank :B :generation 9
                                                       :sl 20.0 :tp 32.0
                                                       :trades 50
                                                       :profit-factor 1.18 :win-rate 0.46
                                                       :sharpe 0.4))
         (orig-dist (symbol-function 'swimmy.school::calculate-genetic-distance))
         (orig-genome (symbol-function 'swimmy.school::extract-genome))
         (orig-threshold (and (boundp 'swimmy.school::*breeder-min-genetic-distance*)
                              swimmy.school::*breeder-min-genetic-distance*))
         (orig-comp-threshold (and (boundp 'swimmy.school::*breeder-min-genetic-distance-complement*)
                                   swimmy.school::*breeder-min-genetic-distance-complement*))
         (orig-near-pf-threshold (and (boundp 'swimmy.school::*breeder-near-pf-threshold*)
                                      swimmy.school::*breeder-near-pf-threshold*)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::extract-genome)
                (lambda (_s) (declare (ignore _s)) '(mock-genome)))
          (setf (symbol-function 'swimmy.school::calculate-genetic-distance)
                (lambda (_g1 _g2) (declare (ignore _g1 _g2)) 0.12))
          (setf swimmy.school::*breeder-min-genetic-distance* 0.15)
          (setf swimmy.school::*breeder-min-genetic-distance-complement* 0.10)
          ;; Keep strict near-PF threshold so this candidate is NOT treated as full complement.
          (setf swimmy.school::*breeder-near-pf-threshold* 1.24)
          (let ((picked (swimmy.school::find-diverse-breeding-partner
                         parent (list parent cand-partial-pf)
                         :start-index 1)))
            (assert-true (eq picked cand-partial-pf)
                         (format nil "Expected partial PF-recovery partner UT-CORR-PARTIAL-PF-CAND, got ~a"
                                 (if picked (swimmy.school:strategy-name picked) :none)))))
      (setf (symbol-function 'swimmy.school::calculate-genetic-distance) orig-dist)
      (setf (symbol-function 'swimmy.school::extract-genome) orig-genome)
      (if orig-threshold
          (setf swimmy.school::*breeder-min-genetic-distance* orig-threshold)
          (makunbound 'swimmy.school::*breeder-min-genetic-distance*))
      (if orig-comp-threshold
          (setf swimmy.school::*breeder-min-genetic-distance-complement* orig-comp-threshold)
          (makunbound 'swimmy.school::*breeder-min-genetic-distance-complement*))
      (if orig-near-pf-threshold
          (setf swimmy.school::*breeder-near-pf-threshold* orig-near-pf-threshold)
          (makunbound 'swimmy.school::*breeder-near-pf-threshold*)))))

(deftest test-find-diverse-breeding-partner-relaxes-distance-for-modest-partial-pf-recovery
  "PF-deficit parent should still allow distance-relaxed partner on modest PF improvement."
  (let* ((parent (swimmy.school:make-strategy :name "UT-CORR-MODEST-PF-PARENT"
                                              :rank :B :generation 75
                                              :sl 20.0 :tp 60.0
                                              :trades 50
                                              :profit-factor 1.20 :win-rate 0.48
                                              :sharpe 1.0))
         (cand-modest-pf (swimmy.school:make-strategy :name "UT-CORR-MODEST-PF-CAND"
                                                      :rank :B :generation 10
                                                      :sl 20.0 :tp 31.0
                                                      :trades 50
                                                      :profit-factor 1.22 :win-rate 0.46
                                                      :sharpe 0.4))
         (orig-dist (symbol-function 'swimmy.school::calculate-genetic-distance))
         (orig-genome (symbol-function 'swimmy.school::extract-genome))
         (orig-threshold (and (boundp 'swimmy.school::*breeder-min-genetic-distance*)
                              swimmy.school::*breeder-min-genetic-distance*))
         (orig-comp-threshold (and (boundp 'swimmy.school::*breeder-min-genetic-distance-complement*)
                                   swimmy.school::*breeder-min-genetic-distance-complement*))
         (orig-near-pf-threshold (and (boundp 'swimmy.school::*breeder-near-pf-threshold*)
                                      swimmy.school::*breeder-near-pf-threshold*)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::extract-genome)
                (lambda (_s) (declare (ignore _s)) '(mock-genome)))
          (setf (symbol-function 'swimmy.school::calculate-genetic-distance)
                (lambda (_g1 _g2) (declare (ignore _g1 _g2)) 0.12))
          (setf swimmy.school::*breeder-min-genetic-distance* 0.15)
          (setf swimmy.school::*breeder-min-genetic-distance-complement* 0.10)
          ;; Keep strict near-PF threshold so this candidate is NOT treated as full complement.
          (setf swimmy.school::*breeder-near-pf-threshold* 1.24)
          (let ((picked (swimmy.school::find-diverse-breeding-partner
                         parent (list parent cand-modest-pf)
                         :start-index 1)))
            (assert-true (eq picked cand-modest-pf)
                         (format nil "Expected modest partial PF-recovery partner UT-CORR-MODEST-PF-CAND, got ~a"
                                 (if picked (swimmy.school:strategy-name picked) :none)))))
      (setf (symbol-function 'swimmy.school::calculate-genetic-distance) orig-dist)
      (setf (symbol-function 'swimmy.school::extract-genome) orig-genome)
      (if orig-threshold
          (setf swimmy.school::*breeder-min-genetic-distance* orig-threshold)
          (makunbound 'swimmy.school::*breeder-min-genetic-distance*))
      (if orig-comp-threshold
          (setf swimmy.school::*breeder-min-genetic-distance-complement* orig-comp-threshold)
          (makunbound 'swimmy.school::*breeder-min-genetic-distance-complement*))
      (if orig-near-pf-threshold
          (setf swimmy.school::*breeder-near-pf-threshold* orig-near-pf-threshold)
          (makunbound 'swimmy.school::*breeder-near-pf-threshold*)))))

(deftest test-find-diverse-breeding-partner-relaxes-distance-to-0p11-for-partial-pf-recovery
  "PF partial-recovery partner should still pass when distance is 0.11."
  (let* ((parent (swimmy.school:make-strategy :name "UT-CORR-PARTIAL011-PARENT"
                                              :rank :B :generation 76
                                              :sl 20.0 :tp 60.0
                                              :trades 50
                                              :profit-factor 1.20 :win-rate 0.48
                                              :sharpe 1.0))
         (cand-partial-pf (swimmy.school:make-strategy :name "UT-CORR-PARTIAL011-CAND"
                                                       :rank :B :generation 11
                                                       :sl 20.0 :tp 31.0
                                                       :trades 50
                                                       :profit-factor 1.22 :win-rate 0.46
                                                       :sharpe 0.4))
         (orig-dist (symbol-function 'swimmy.school::calculate-genetic-distance))
         (orig-genome (symbol-function 'swimmy.school::extract-genome))
         (orig-threshold (and (boundp 'swimmy.school::*breeder-min-genetic-distance*)
                              swimmy.school::*breeder-min-genetic-distance*))
         (orig-comp-threshold (and (boundp 'swimmy.school::*breeder-min-genetic-distance-complement*)
                                   swimmy.school::*breeder-min-genetic-distance-complement*))
         (orig-near-pf-threshold (and (boundp 'swimmy.school::*breeder-near-pf-threshold*)
                                      swimmy.school::*breeder-near-pf-threshold*)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::extract-genome)
                (lambda (_s) (declare (ignore _s)) '(mock-genome)))
          (setf (symbol-function 'swimmy.school::calculate-genetic-distance)
                (lambda (_g1 _g2) (declare (ignore _g1 _g2)) 0.11))
          (setf swimmy.school::*breeder-min-genetic-distance* 0.15)
          (setf swimmy.school::*breeder-min-genetic-distance-complement* 0.10)
          (setf swimmy.school::*breeder-near-pf-threshold* 1.24)
          (let ((picked (swimmy.school::find-diverse-breeding-partner
                         parent (list parent cand-partial-pf)
                         :start-index 1)))
            (assert-true (eq picked cand-partial-pf)
                         (format nil "Expected 0.11 partial PF-recovery partner UT-CORR-PARTIAL011-CAND, got ~a"
                                 (if picked (swimmy.school:strategy-name picked) :none)))))
      (setf (symbol-function 'swimmy.school::calculate-genetic-distance) orig-dist)
      (setf (symbol-function 'swimmy.school::extract-genome) orig-genome)
      (if orig-threshold
          (setf swimmy.school::*breeder-min-genetic-distance* orig-threshold)
          (makunbound 'swimmy.school::*breeder-min-genetic-distance*))
      (if orig-comp-threshold
          (setf swimmy.school::*breeder-min-genetic-distance-complement* orig-comp-threshold)
          (makunbound 'swimmy.school::*breeder-min-genetic-distance-complement*))
      (if orig-near-pf-threshold
          (setf swimmy.school::*breeder-near-pf-threshold* orig-near-pf-threshold)
          (makunbound 'swimmy.school::*breeder-near-pf-threshold*)))))

(deftest test-find-diverse-breeding-partner-relaxes-distance-for-tiny-partial-pf-recovery
  "PF partial-recovery should work with +0.01 PF improvement when distance is 0.11."
  (let* ((parent (swimmy.school:make-strategy :name "UT-CORR-PARTIAL001-PARENT"
                                              :rank :B :generation 77
                                              :sl 20.0 :tp 60.0
                                              :trades 50
                                              :profit-factor 1.20 :win-rate 0.48
                                              :sharpe 1.0))
         (cand-tiny-pf (swimmy.school:make-strategy :name "UT-CORR-PARTIAL001-CAND"
                                                    :rank :B :generation 12
                                                    :sl 20.0 :tp 31.0
                                                    :trades 50
                                                    :profit-factor 1.21 :win-rate 0.46
                                                    :sharpe 0.4))
         (orig-dist (symbol-function 'swimmy.school::calculate-genetic-distance))
         (orig-genome (symbol-function 'swimmy.school::extract-genome))
         (orig-threshold (and (boundp 'swimmy.school::*breeder-min-genetic-distance*)
                              swimmy.school::*breeder-min-genetic-distance*))
         (orig-comp-threshold (and (boundp 'swimmy.school::*breeder-min-genetic-distance-complement*)
                                   swimmy.school::*breeder-min-genetic-distance-complement*))
         (orig-near-pf-threshold (and (boundp 'swimmy.school::*breeder-near-pf-threshold*)
                                      swimmy.school::*breeder-near-pf-threshold*)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::extract-genome)
                (lambda (_s) (declare (ignore _s)) '(mock-genome)))
          (setf (symbol-function 'swimmy.school::calculate-genetic-distance)
                (lambda (_g1 _g2) (declare (ignore _g1 _g2)) 0.11))
          (setf swimmy.school::*breeder-min-genetic-distance* 0.15)
          (setf swimmy.school::*breeder-min-genetic-distance-complement* 0.10)
          (setf swimmy.school::*breeder-near-pf-threshold* 1.24)
          (let ((picked (swimmy.school::find-diverse-breeding-partner
                         parent (list parent cand-tiny-pf)
                         :start-index 1)))
            (assert-true (eq picked cand-tiny-pf)
                         (format nil "Expected tiny partial PF-recovery partner UT-CORR-PARTIAL001-CAND, got ~a"
                                 (if picked (swimmy.school:strategy-name picked) :none)))))
      (setf (symbol-function 'swimmy.school::calculate-genetic-distance) orig-dist)
      (setf (symbol-function 'swimmy.school::extract-genome) orig-genome)
      (if orig-threshold
          (setf swimmy.school::*breeder-min-genetic-distance* orig-threshold)
          (makunbound 'swimmy.school::*breeder-min-genetic-distance*))
      (if orig-comp-threshold
          (setf swimmy.school::*breeder-min-genetic-distance-complement* orig-comp-threshold)
          (makunbound 'swimmy.school::*breeder-min-genetic-distance-complement*))
      (if orig-near-pf-threshold
          (setf swimmy.school::*breeder-near-pf-threshold* orig-near-pf-threshold)
          (makunbound 'swimmy.school::*breeder-near-pf-threshold*)))))
