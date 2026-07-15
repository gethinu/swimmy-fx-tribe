(in-package :swimmy.school)
(setf swimmy.core:*enable-primitive-diversity* t)
;; EXPLICIT copy DB path (absolute) so nothing opens the repo's real dev DB.
(let ((swimmy.core::*db-path-default* "/mnt/c/tmp/swimmy_smoke.db"))
  (setf *strategy-knowledge-base* nil)
  (let* ((p1 (make-strategy :name "P1" :symbol "USDJPY" :category :trend :indicators '((sma 30)(sma 60))
                            :sharpe 0.8d0 :profit-factor 1.5d0 :win-rate 0.55d0 :max-dd 0.05d0 :trades 300
                            :rank :B :tier :battlefield :status :active))
         (p2 (make-strategy :name "P2" :symbol "USDJPY" :category :trend :indicators '((ema 20))
                            :sharpe 0.7d0 :profit-factor 1.4d0 :win-rate 0.53d0 :max-dd 0.06d0 :trades 300
                            :rank :B :tier :battlefield :status :active))
         (child (make-diverse-child p1 p2 5 "SMOKE-KELT-1" :both "USDJPY" 0.005d0 0.005d0)))
    (format t "~&CHILD sym=~a cat=~a inds=~a band=~a atr-sl=~a~%"
            (strategy-symbol child) (strategy-category child) (strategy-indicators child)
            (strategy-band-mult child) (strategy-atr-barrier-sl child))
    (format t "~&EMIT type=~a short=~a band_mult=~a~%"
            (cdr (assoc 'indicator_type (strategy-to-alist child)))
            (cdr (assoc 'sma_short (strategy-to-alist child)))
            (cdr (assoc 'band_mult (strategy-to-alist child))))
    ;; ingest (skip BT dispatch to avoid zmq; verify structural acceptance)
    (multiple-value-bind (accepted status)
        (handler-case (add-to-kb child :breeder :require-bt nil :notify nil)
          (error (e) (format t "~&ADDKB-ERROR ~a~%" e) (values nil :error)))
      (format t "~&ADD-TO-KB accepted=~a status=~a in-kb=~a~%"
              accepted status
              (and (member child *strategy-knowledge-base*) t)))))
(sb-ext:exit :code 0)
