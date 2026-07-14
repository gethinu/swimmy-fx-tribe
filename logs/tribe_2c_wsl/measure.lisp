;;; 2c flag-ON breeding measurement (PURE: no add-to-kb / no zmq / no live orders).
;;; Loads the real library population, breeds via the real breed-strategies + pairing,
;;; and measures the diversity of the children + emits diverse children to primitive_scan
;;; manifests for standalone honest scoring. Live swimmy.db untouched (never opened here).
(in-package :swimmy.school)
(defun cat-key (s) (string-downcase (symbol-name (or (strategy-category s) :unknown))))
(defun dist (pop)
  (let ((sym (make-hash-table :test 'equal)) (cat (make-hash-table :test 'equal)))
    (dolist (s pop)
      (incf (gethash (or (strategy-symbol s) "?") sym 0))
      (incf (gethash (cat-key s) cat 0)))
    (list :sym (alexandria-hash-to-alist sym) :cat (alexandria-hash-to-alist cat))))
(defun alexandria-hash-to-alist (h) (let (a) (maphash (lambda (k v) (push (cons k v) a)) h) a))

;; Load population from the library (read-only file reads; no DB).
(handler-case (setf *strategy-knowledge-base* (swimmy.persistence::load-all-strategies))
  (error (e) (format t "~&LOADPOP-WARN: ~a~%" e)))
(let* ((pop (remove-if (lambda (s) (eq (strategy-rank s) :graveyard)) *strategy-knowledge-base*)))
  (format t "~&POP-BEFORE n=~d dist=~s~%" (length pop) (dist pop))

  ;; Flag ON.
  (setf swimmy.core:*enable-primitive-diversity* t)

  ;; Breed like run-breeding-cycle (per category, priority-sorted, diverse partner), but
  ;; PURE: just call breed-strategies and collect children. No persistence, no zmq.
  (let ((children nil) (n-pairs 0))
   (dotimes (rep 8)
    (dolist (cat '(:trend :reversion :breakout :scalp))
      (let* ((warriors (remove-if-not
                        (lambda (s) (eq (or (ignore-errors (strategy-regime-class s))
                                            (strategy-category s)) cat)) pop))
             (sorted (sort (copy-list warriors) #'>
                           :key (lambda (s) (or (ignore-errors (strategy-breeding-priority-score s)) 0))))
             (used (make-hash-table :test 'equal)) (pairs 0))
        (loop for i from 0 below (length sorted) while (< pairs 240)
              for p1 = (nth i sorted)
              do (when (and p1 (null (gethash (strategy-name p1) used))
                            (ignore-errors (can-breed-p p1)))
                   (let ((p2 (ignore-errors (find-diverse-breeding-partner p1 sorted :start-index (1+ i) :used-names used))))
                     (when p2
                       (incf pairs) (incf n-pairs)
                       (setf (gethash (strategy-name p1) used) t (gethash (strategy-name p2) used) t)
                       (let ((c (ignore-errors (breed-strategies p1 p2))))
                         (when c (push c children))))))))) ; end dolist cat
   ) ; end dotimes rep
    (format t "~&BRED n-pairs=~d n-children=~d~%" n-pairs (length children))

    ;; Classify children.
    (let* ((diverse (remove-if-not
                     (lambda (c) (let ((typ (detect-indicator-type-extended (strategy-indicators c))))
                                   (member typ '(keltner bb donchian rsi stoch)))) children))
           (nonusd (remove-if (lambda (c) (equal (strategy-symbol c) "USDJPY")) children))
           (nontrend (remove-if (lambda (c) (eq (strategy-category c) :trend)) children)))
      (format t "~&CHILDREN diverse(prim)=~d non-USDJPY=~d non-TREND=~d~%"
              (length diverse) (length nonusd) (length nontrend))
      (format t "~&CHILD-CAT-DIST=~s~%" (dist children))

      ;; Emit diverse children as primitive_scan manifests, per symbol.
      (dolist (pair '(("EURUSD" . "/mnt/c/tmp/2c_div_EURUSD.json")
                      ("GBPUSD" . "/mnt/c/tmp/2c_div_GBPUSD.json")))
        (let ((rows (remove-if-not (lambda (c) (equal (strategy-symbol c) (car pair)))
                                   (remove-if (lambda (c) (eq (strategy-category c) :trend)) children))))
          (with-open-file (out (cdr pair) :direction :output :if-exists :supersede :if-does-not-exist :create)
            (format out "[~%")
            (loop for c in rows for k from 0
                  for al = (strategy-to-alist c)
                  for prim = (string-downcase (format nil "~a" (or (cdr (assoc 'indicator_type al)) "bb")))
                  for period = (or (cdr (assoc 'sma_short al)) 30)
                  for dev = (or (cdr (assoc 'band_mult al)) 2.0)
                  for atrp = (or (cdr (assoc 'atr_period al)) 14)
                  for atrsl = (or (cdr (assoc 'atr_barrier_sl al)) 0.0)
                  for atrtp = (or (cdr (assoc 'atr_barrier_tp al)) 0.0)
                  for tf = (or (cdr (assoc 'timeframe al)) 240)
                  do (format out "~a {\"name\":\"~a\",\"symbol\":\"~a\",\"regime\":\"~a\",\"prim\":\"~a\",\"period\":~d,\"dev\":~,3f,\"atr_period\":~d,\"tf_seconds\":~d,\"barrier_mode\":\"~a\",\"sl\":~,4f,\"tp\":~,4f,\"max_hold\":0}~%"
                             (if (zerop k) " " ",") (strategy-name c) (car pair) (cat-key c) prim period dev atrp (* tf 60)
                             (if (> atrsl 0.0) "atr" "pip") (if (> atrsl 0.0) atrsl 0.005) (if (> atrtp 0.0) atrtp 0.005)))
            (format out "]~%"))
          (format t "~&EMITTED ~a: ~d diverse children~%" (car pair) (length rows)))))))
(sb-ext:exit :code 0)
