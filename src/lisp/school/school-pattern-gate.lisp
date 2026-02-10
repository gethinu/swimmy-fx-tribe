;;; school-pattern-gate.lisp - Pattern Similarity Soft Gate
(in-package :swimmy.school)

(defun pattern-gate-multiplier (direction p-up p-down p-flat &key (threshold 0.60) (mismatch-multiplier 0.70))
  "Return (values multiplier reason).

Fail-open principle:
- If probabilities are missing/invalid, return 1.0 (no gate).

Soft gate policy:
- If max prob < threshold => mismatch-multiplier
- If FLAT is dominant => mismatch-multiplier (conservative)
- If direction aligns with dominant (UP->BUY, DOWN->SELL) => 1.0
- Otherwise => mismatch-multiplier"
  (labels ((num (x)
             (and (numberp x) (float x))))
    (let* ((pu (num p-up))
           (pd (num p-down))
           (pf (num p-flat))
           (thr (if (numberp threshold) (float threshold) 0.60))
           (mm (if (numberp mismatch-multiplier) (float mismatch-multiplier) 0.70)))
      (if (not (and pu pd pf))
          (values 1.0 "missing_probs")
          (let* ((max-p (max pu pd pf))
                 (winner (cond ((>= pu (max pd pf)) :up)
                               ((>= pd (max pu pf)) :down)
                               (t :flat))))
            (cond
              ((< max-p thr) (values mm "low_confidence"))
              ((eq winner :flat) (values mm "flat_regime"))
              ((and (eq direction :buy) (eq winner :up)) (values 1.0 "aligned"))
              ((and (eq direction :sell) (eq winner :down)) (values 1.0 "aligned"))
              (t (values mm "disagree"))))))))

