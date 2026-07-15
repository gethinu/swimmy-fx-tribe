(load "logs/tribe_2c_20260714/sel_forms.lisp")
(in-package :swimmy.school)
;; Selection-dynamics demonstration: a 20-strong USDJPY/TREND monoculture (raw sharpe 1.0)
;; vs 2 lone diverse EURUSD/REVERSION individuals (raw sharpe 0.7, i.e. WEAKER on raw sharpe).
(let* ((mono (loop repeat 20 collect (make-strategy :name "T" :symbol "USDJPY" :category :trend :sharpe 1.0d0 :trades 300)))
       (div  (list (make-strategy :name "K1" :symbol "EURUSD" :category :reversion :sharpe 0.7d0 :trades 300)
                   (make-strategy :name "K2" :symbol "EURUSD" :category :reversion :sharpe 0.7d0 :trades 300)))
       (pop (append mono div)))
  (flet ((rank-top (flag)
           (setf *enable-primitive-diversity* flag)
           (let ((sorted (sort (copy-list pop) #'> :key (lambda (s) (selection-fitness s pop)))))
             (strategy-category (first sorted)))))
    (let ((off-top (rank-top nil))
          (on-top  (rank-top t)))
      ;; OFF: raw sharpe => a TREND clone (1.0) tops the ranking => monoculture wins selection.
      (assert (eq off-top :trend) () "OFF top should be TREND monoculture, got ~a" off-top)
      ;; ON: fitness-sharing => TREND clone 1.0/20=0.05, diverse 0.7/1=0.7 => diverse tops.
      (assert (eq on-top :reversion) () "ON top should be diverse REVERSION, got ~a" on-top)
      (setf *enable-primitive-diversity* t)
      (format t "~&DYNAMICS-PASS: OFF selection top = ~a (monoculture wins); ON top = ~a (fitness-sharing lifts the lone diverse niche above the 20-clone monoculture: 1.0/20=0.05 < 0.7/1=0.70)~%"
              off-top on-top))))
(sb-ext:exit :code 0)
