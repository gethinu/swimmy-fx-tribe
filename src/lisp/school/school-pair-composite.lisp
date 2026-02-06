;;; school-pair-composite.lisp - Pair-Composite utilities

(in-package :swimmy.school)

(defparameter *pair-strategy-enabled* nil
  "When true, apply pair-composite overlay logic.")

(defparameter *pair-active-defs* nil
  "Active pair definitions (plist with :pair-id, :a, :b, :weight-a, :weight-b).")

(defparameter *pair-max-lot* 0.05
  "Maximum lot allowed per pair overlay.")

(defun %fnv1a-64 (s)
  "Compute 64-bit FNV-1a hash for string S."
  (let ((hash #xCBF29CE484222325))
    (loop for ch across s
          do (setf hash (logxor hash (char-code ch))
                   hash (ldb (byte 64 0) (* hash #x100000001B3))))
    hash))

(defun pair-id (a b)
  "Return stable, order-independent ID for a pair of strategy names."
  (let* ((names (sort (list (string a) (string b)) #'string<))
         (raw (format nil "~a|~a" (first names) (second names))))
    (format nil "~16,'0x" (%fnv1a-64 raw))))

(defun %pair-def-matches-p (pair-def strategy-name)
  (let ((a (getf pair-def :a))
        (b (getf pair-def :b))
        (name (string strategy-name)))
    (or (and a (string= name (string a)))
        (and b (string= name (string b))))))

(defun apply-pair-overlay (strategy-name direction symbol base-lot)
  "Return (final-lot pair-id) after applying pair overlay (if enabled)."
  (declare (ignore direction symbol))
  (if (or (null strategy-name) (not *pair-strategy-enabled*))
      (list base-lot nil)
      (let* ((pair-def (find-if (lambda (def) (%pair-def-matches-p def strategy-name))
                                *pair-active-defs*))
             (pair-id (and pair-def (getf pair-def :pair-id)))
             (final-lot (if pair-def
                            (min base-lot *pair-max-lot*)
                            base-lot)))
        (list final-lot pair-id))))

(defparameter *pair-candidate-per-group* 50)
(defparameter *pair-eligible-ranks* '(:A :S :LEGEND :legend))

(defun %strategy-prop (strat key)
  (cond
    ((typep strat 'strategy)
     (ecase key
       (:name (strategy-name strat))
       (:symbol (strategy-symbol strat))
       (:timeframe (strategy-timeframe strat))
       (:sharpe (strategy-sharpe strat))
       (:rank (strategy-rank strat))))
    ((and (listp strat) (getf strat key)) (getf strat key))
    ((and (listp strat) (getf strat (intern (string-upcase (symbol-name key)) :keyword)))
     (getf strat (intern (string-upcase (symbol-name key)) :keyword)))
    (t nil)))

(defun %normalize-rank (rank)
  (cond
    ((keywordp rank) rank)
    ((symbolp rank) (intern (string-upcase (symbol-name rank)) :keyword))
    ((stringp rank) (intern (string-upcase rank) :keyword))
    (t nil)))

(defun %pair-eligible-rank-p (rank)
  (member (%normalize-rank rank) *pair-eligible-ranks*))

(defun %strategy-better-p (a b)
  (let ((sa (or (%strategy-prop a :sharpe) 0.0))
        (sb (or (%strategy-prop b :sharpe) 0.0))
        (na (string (%strategy-prop a :name)))
        (nb (string (%strategy-prop b :name))))
    (if (/= sa sb)
        (> sa sb)
        (string< na nb))))

(defun pair-candidate-pool (strategies &key (per-group *pair-candidate-per-group*))
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (s strategies)
      (when (%pair-eligible-rank-p (%strategy-prop s :rank))
        (let* ((symbol (or (%strategy-prop s :symbol) ""))
               (tf (or (%strategy-prop s :timeframe) 0))
               (key (format nil "~a|~a" symbol tf)))
          (push s (gethash key groups)))))
    (let (out)
      (maphash (lambda (key group)
                 (declare (ignore key))
                 (let* ((sorted (sort (copy-list group) #'%strategy-better-p))
                        (top (subseq sorted 0 (min per-group (length sorted)))))
                   (setf out (nconc out top))))
               groups)
      out)))

(defparameter *pair-eval-window-trades* 100)
(defparameter *pair-min-trades* 100)

(defun %trade-entry-val (entry keys)
  (cond
    ((and (listp entry) (keywordp (first entry)))
     (loop for k in keys
           for kw = (cond ((keywordp k) k)
                          ((symbolp k) (intern (string-upcase (symbol-name k)) :keyword))
                          (t nil))
           for val = (and kw (getf entry kw :missing))
           unless (eq val :missing) do (return val)))
    (t
     (loop for k in keys
           for key-name = (cond ((keywordp k) (string-upcase (symbol-name k)))
                                ((symbolp k) (string-upcase (symbol-name k)))
                                ((stringp k) (string-upcase k))
                                (t nil))
           for cell = (and key-name
                           (find key-name entry
                                 :key (lambda (cell)
                                        (let ((ck (car cell)))
                                          (cond ((symbolp ck) (string-upcase (symbol-name ck)))
                                                ((stringp ck) (string-upcase ck))
                                                (t nil))))
                                 :test #'string=))
           when cell do (return (cdr cell))))))

(defun trade-list->series (trade-list &key (max-trades *pair-eval-window-trades*))
  (let ((pairs nil))
    (dolist (entry trade-list)
      (let ((ts (%trade-entry-val entry '(timestamp t)))
            (pnl (%trade-entry-val entry '(pnl))))
        (when (and ts pnl)
          (push (cons ts (float pnl 0.0)) pairs))))
    (setf pairs (sort pairs #'< :key #'car))
    (when (> (length pairs) max-trades)
      (setf pairs (subseq pairs (- (length pairs) max-trades))))
    (let ((acc (make-hash-table :test 'eql))
          (keys nil))
      (dolist (pair pairs)
        (let ((ts (car pair)) (pnl (cdr pair)))
          (unless (gethash ts acc) (push ts keys))
          (incf (gethash ts acc 0.0) pnl)))
      (setf keys (sort keys #'<))
      (mapcar (lambda (ts) (cons ts (gethash ts acc 0.0))) keys))))

(defun align-pnl-series (series-a series-b)
  (let ((map-a (make-hash-table :test 'eql))
        (map-b (make-hash-table :test 'eql))
        (ts nil))
    (dolist (pair series-a)
      (setf (gethash (car pair) map-a) (cdr pair))
      (pushnew (car pair) ts))
    (dolist (pair series-b)
      (setf (gethash (car pair) map-b) (cdr pair))
      (pushnew (car pair) ts))
    (setf ts (sort ts #'<))
    (values (mapcar (lambda (ts-val) (gethash ts-val map-a 0.0)) ts)
            (mapcar (lambda (ts-val) (gethash ts-val map-b 0.0)) ts))))

(defun pearson-correlation (xs ys)
  (let* ((n (length xs)))
    (if (or (zerop n) (/= n (length ys)))
        0.0
        (let* ((mean-x (/ (reduce #'+ xs) n))
               (mean-y (/ (reduce #'+ ys) n))
               (num 0.0)
               (den-x 0.0)
               (den-y 0.0))
          (loop for x in xs
                for y in ys
                do (let ((dx (- x mean-x))
                         (dy (- y mean-y)))
                     (incf num (* dx dy))
                     (incf den-x (* dx dx))
                     (incf den-y (* dy dy))))
          (if (or (zerop den-x) (zerop den-y))
              0.0
              (/ num (sqrt (* den-x den-y))))))))

(defparameter *pair-score-sharpe-weight* 0.7)
(defparameter *pair-score-pf-weight* 0.3)

(defun %stddev (xs)
  (let* ((n (length xs)))
    (if (zerop n)
        0.0
        (let* ((mean (/ (reduce #'+ xs) n))
               (sq (reduce #'+ (mapcar (lambda (x) (expt (- x mean) 2)) xs)))
               (var (/ sq n)))
          (sqrt var)))))

(defun inverse-vol-weights (xs ys)
  (let* ((sx (%stddev xs))
         (sy (%stddev ys))
         (w1 (if (zerop sx) 0.0 (/ 1.0 sx)))
         (w2 (if (zerop sy) 0.0 (/ 1.0 sy)))
         (sum (+ w1 w2)))
    (if (> sum 0.0)
        (values (/ w1 sum) (/ w2 sum))
        (values 0.5 0.5))))

(defun composite-pnls (xs ys w1 w2)
  (mapcar (lambda (x y) (+ (* w1 x) (* w2 y))) xs ys))

(defun profit-factor (pnls)
  (let ((gains 0.0)
        (losses 0.0))
    (dolist (p pnls)
      (if (> p 0)
          (incf gains p)
          (incf losses (abs p))))
    (if (zerop losses)
        0.0
        (/ gains losses))))

(defun sharpe-from-pnls (pnls)
  (let* ((n (length pnls)))
    (if (zerop n)
        0.0
        (let* ((mean (/ (reduce #'+ pnls) n))
               (sd (%stddev pnls)))
          (if (zerop sd) 0.0 (/ mean sd))))))

(defun pair-score-from-pnls (pnls)
  (let* ((sharpe (sharpe-from-pnls pnls))
         (pf (profit-factor pnls))
         (score (+ (* *pair-score-sharpe-weight* sharpe)
                   (* *pair-score-pf-weight* pf))))
    (list :sharpe sharpe :pf pf :score score)))

