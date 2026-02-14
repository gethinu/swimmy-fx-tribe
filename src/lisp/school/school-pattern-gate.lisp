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

(defun apply-pattern-similarity-gate (symbol timeframe-key direction lot history
                                     &key (k 30) (threshold 0.60) (mismatch-multiplier 0.70) query-fn)
  "Apply Pattern Similarity soft gate to LOT.

Returns (values new-lot multiplier reason p-up p-down p-flat).

Fail-open:
- When not applicable TF, insufficient history, missing query-fn, or service error -> multiplier=1.0."
  (let* ((tf (string-upcase (format nil "~a" timeframe-key)))
         (allowed (member tf '("H1" "H4" "D1" "W1" "MN1" "MN") :test #'string=))
         (window (cond ((string= tf "W1") 104)
                       (t 120))))
    (cond
      ((not allowed) (values (float lot) 1.0 "not_applicable" nil nil nil))
      ((or (null history) (< (length history) window)) (values (float lot) 1.0 "insufficient_history" nil nil nil))
      ((null query-fn) (values (float lot) 1.0 "missing_query_fn" nil nil nil))
      (t
       (let* ((candles (subseq history 0 window))
              (resp (handler-case (funcall query-fn symbol tf candles :k k) (error () nil))))
         (if (null resp)
             (values (float lot) 1.0 "query_error" nil nil nil)
             (let ((status (swimmy.core:sexp-alist-get resp 'status)))
               (if (not (and status (string= status "ok")))
                   (values (float lot) 1.0 "service_error" nil nil nil)
                   (let* ((result (swimmy.core:sexp-alist-get resp 'result))
                          (p-up (and result (swimmy.core:sexp-alist-get result 'p_up)))
                          (p-down (and result (swimmy.core:sexp-alist-get result 'p_down)))
                          (p-flat (and result (swimmy.core:sexp-alist-get result 'p_flat))))
                     (multiple-value-bind (mult reason)
                         (pattern-gate-multiplier direction p-up p-down p-flat
                                                  :threshold threshold
                                                  :mismatch-multiplier mismatch-multiplier)
                       (let ((new-lot (max 0.01 (* (float lot) (float mult)))))
                         (values new-lot mult reason p-up p-down p-flat))))))))))))
