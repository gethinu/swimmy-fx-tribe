(in-package :swimmy.school)

(defparameter *pip-size-by-symbol*
  '(("EURUSD" . 1.0d-4)
    ("GBPUSD" . 1.0d-4)
    ("USDJPY" . 1.0d-2))
  "Pip size by symbol for bid/ask to pips conversion.")

(defun get-pip-size (symbol)
  (or (cdr (assoc symbol *pip-size-by-symbol* :test #'string=)) 1.0d-4))

(defun spread-pips-from-bid-ask (symbol bid ask)
  (let ((pip (get-pip-size symbol)))
    (if (and (numberp bid) (numberp ask) (> pip 0))
        (let ((b (float bid 1.0d0))
              (a (float ask 1.0d0))
              (p (float pip 1.0d0)))
          (/ (- a b) p))
        0.0)))

(defun calculate-cost-pips (symbol bid ask &key (slippage-pips 0.0) (commission-pips 0.0) (swap-pips 0.0))
  (+ (spread-pips-from-bid-ask symbol bid ask)
     (float slippage-pips)
     (float commission-pips)
     (float swap-pips)))
