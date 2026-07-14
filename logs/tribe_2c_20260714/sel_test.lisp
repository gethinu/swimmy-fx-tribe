(load "logs/tribe_2c_20260714/sel_forms.lisp")
(in-package :swimmy.school)
(let ((a (make-strategy :name "A" :symbol "USDJPY" :category :trend :sharpe 1.0d0 :trades 300))
      (clones (loop repeat 5 collect (make-strategy :name "C" :symbol "USDJPY" :category :trend :sharpe 1.0d0 :trades 300)))
      (lone  (make-strategy :name "L" :symbol "EURUSD" :category :reversion :sharpe 1.0d0 :trades 300)))
  ;; OFF: raw sharpe exactly
  (setf *enable-primitive-diversity* nil)
  (assert (= (selection-fitness a) 1.0) () "OFF raw sharpe")
  (assert (= (selection-fitness a clones) 1.0) () "OFF ignores population")
  ;; 2f OFF: always eligible
  (assert (breeding-cpcv-eligible-p (make-strategy :name "x" :cpcv-pass-rate 0.0d0)) () "OFF cpcv gate off")
  ;; ON: niche crowding divides
  (setf *enable-primitive-diversity* t)
  (let ((pop (cons a clones)))                        ; 6 USDJPY/TREND in niche
    (assert (= (fitness-sharing-denominator a pop) 6) () "niche count 6")
    (assert (< (abs (- (selection-fitness a pop) (/ 1.0 6))) 1e-6) () "shared fitness = sharpe/6"))
  (let ((mixed (list a lone)))                        ; lone EURUSD/REVERSION niche of 1
    (assert (= (fitness-sharing-denominator lone mixed) 1) () "lone niche 1")
    (assert (= (selection-fitness lone mixed) 1.0) () "lone keeps full fitness (division by 1)"))
  ;; 2f ON: gated on cpcv pass-rate
  (assert (not (breeding-cpcv-eligible-p (make-strategy :name "lo" :cpcv-pass-rate 0.4d0))) () "ON cpcv 0.4 blocked")
  (assert (breeding-cpcv-eligible-p (make-strategy :name "hi" :cpcv-pass-rate 0.7d0)) () "ON cpcv 0.7 eligible")
  (format t "~&SEL-TEST-PASS: OFF raw sharpe; ON fitness-sharing divides monoculture (sharpe/6) but spares lone niche; 2f gates cpcv<0.6~%"))
(sb-ext:exit :code 0)
