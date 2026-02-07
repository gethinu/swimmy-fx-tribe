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

(defun win-rate-from-pnls (pnls)
  (let ((wins 0) (total 0))
    (dolist (p pnls)
      (incf total)
      (when (> p 0) (incf wins)))
    (if (zerop total) 0.0 (/ (float wins) total))))

(defun max-dd-from-pnls (pnls)
  (let ((peak 0.0) (equity 0.0) (maxdd 0.0))
    (dolist (p pnls)
      (incf equity p)
      (setf peak (max peak equity))
      (let ((dd (if (> peak 0) (/ (- peak equity) peak) 0.0)))
        (setf maxdd (max maxdd dd))))
    maxdd))

(defun pair-score-from-pnls (pnls)
  (let* ((sharpe (sharpe-from-pnls pnls))
         (pf (profit-factor pnls))
         (score (+ (* *pair-score-sharpe-weight* sharpe)
                   (* *pair-score-pf-weight* pf))))
    (list :sharpe sharpe :pf pf :score score)))

(defun pair-metrics-from-trades (trades-a trades-b &key (min-trades *pair-min-trades*))
  "Compute composite metrics from trade lists. Returns (values metrics reason)."
  (when (or (< (length trades-a) min-trades)
            (< (length trades-b) min-trades))
    (return-from pair-metrics-from-trades (values nil :insufficient-data)))
  (multiple-value-bind (xs ys)
      (align-pnl-series (trade-list->series trades-a) (trade-list->series trades-b))
    (multiple-value-bind (w1 w2) (inverse-vol-weights xs ys)
      (let* ((pnls (composite-pnls xs ys w1 w2))
             (score (pair-score-from-pnls pnls))
             (wr (win-rate-from-pnls pnls))
             (maxdd (max-dd-from-pnls pnls)))
        (values (append score (list :weight-a w1 :weight-b w2 :win-rate wr :max-dd maxdd)) nil)))))

(defparameter *pair-corr-threshold* 0.2)
(defparameter *pair-rescue-corr-threshold* 0.3)
(defparameter *pair-max-pairs* 5)

(defun %trade-list-from-map (trade-map name)
  (cond
    ((hash-table-p trade-map) (gethash name trade-map))
    ((listp trade-map) (cdr (assoc name trade-map :test #'string=)))
    (t nil)))

(defun %pair-eval (name-a name-b xs ys)
  (multiple-value-bind (w1 w2)
      (inverse-vol-weights xs ys)
    (let* ((pnls (composite-pnls xs ys w1 w2))
           (score (pair-score-from-pnls pnls)))
      (append (list :pair-id (pair-id name-a name-b)
                    :a name-a :b name-b
                    :weight-a w1 :weight-b w2)
              score))))

(defun select-pair-candidates (strategies trade-map
                               &key
                                 (per-group *pair-candidate-per-group*)
                                 (min-trades *pair-min-trades*)
                                 (max-pairs *pair-max-pairs*)
                                 (corr-threshold *pair-corr-threshold*)
                                 (rescue-threshold *pair-rescue-corr-threshold*)
                                 (corr-fn #'pearson-correlation))
  (let* ((pool (pair-candidate-pool strategies :per-group per-group))
         (eligible nil)
         (excluded 0)
         (pairs nil)
         (pair-ids (make-hash-table :test 'equal)))
    (dolist (s pool)
      (let* ((name (string (%strategy-prop s :name)))
             (trades (%trade-list-from-map trade-map name)))
        (if (and trades (>= (length trades) min-trades))
            (push (list :name name :series (trade-list->series trades)) eligible)
            (incf excluded))))
    (setf eligible (nreverse eligible))
    (labels ((try-add (name-a name-b xs ys threshold)
               (let ((corr (funcall corr-fn xs ys)))
                 (when (<= (abs corr) threshold)
                   (let ((pid (pair-id name-a name-b)))
                     (unless (gethash pid pair-ids)
                       (setf (gethash pid pair-ids) t)
                       (let ((entry (%pair-eval name-a name-b xs ys)))
                         (push (append entry (list :corr corr)) pairs)))))))
             (build (threshold)
               (dolist (i eligible)
                 (dolist (j eligible)
                   (let ((name-a (getf i :name))
                         (name-b (getf j :name)))
                     (when (string< name-a name-b)
                       (multiple-value-bind (xs ys)
                           (align-pnl-series (getf i :series) (getf j :series))
                         (try-add name-a name-b xs ys threshold))))))))
      (build corr-threshold)
      (when (and (< (length pairs) max-pairs) rescue-threshold)
        (build rescue-threshold)))
    (setf pairs (sort pairs #'> :key (lambda (p) (getf p :score))))
    (setf pairs (subseq pairs 0 (min max-pairs (length pairs))))
    (values pairs (list :total (length pool)
                        :eligible (length eligible)
                        :excluded excluded))))

(defparameter *pair-slots-per-tf* 1
  "Max pair slots per symbol/timeframe in hybrid competition.")

(defparameter *pair-competition-top-n* 2
  "Top-N combined list to consider when activating pairs.")

(defun %single-score (strat)
  (+ (* *pair-score-sharpe-weight* (or (strategy-sharpe strat) 0.0))
     (* *pair-score-pf-weight* (or (strategy-profit-factor strat) 0.0))))

(defun select-active-pair-defs (pair-rows single-strategies
                                &key (pair-slots-per-tf *pair-slots-per-tf*)
                                     (competition-top-n *pair-competition-top-n*))
  "Select active pair defs using hybrid competition against single strategies."
  (let ((groups (make-hash-table :test 'equal))
        (out nil))
    (dolist (p pair-rows)
      (let* ((symbol (or (getf p :symbol) ""))
             (tf (or (getf p :timeframe) 0))
             (key (format nil "~a|~a" symbol tf)))
        (push (list :type :pair :score (or (getf p :score) 0.0) :pair p) (gethash key groups))))
    (dolist (s single-strategies)
      (let* ((symbol (or (strategy-symbol s) ""))
             (tf (or (strategy-timeframe s) 0))
             (key (format nil "~a|~a" symbol tf)))
        (push (list :type :single :score (%single-score s) :single s) (gethash key groups))))
    (maphash
     (lambda (_key entries)
       (declare (ignore _key))
       (let* ((sorted (sort (copy-list entries) #'> :key (lambda (e) (getf e :score))))
              (top (subseq sorted 0 (min competition-top-n (length sorted))))
              (pairs (remove-if-not (lambda (e) (eq (getf e :type) :pair)) top))
              (pairs (subseq pairs 0 (min pair-slots-per-tf (length pairs)))))
         (dolist (p pairs)
           (let* ((pair (getf p :pair))
                  (a (or (getf pair :a) (getf pair :strategy-a)))
                  (b (or (getf pair :b) (getf pair :strategy-b))))
             (push (list :pair-id (getf pair :pair-id)
                         :a a
                         :b b
                         :weight-a (getf pair :weight-a)
                         :weight-b (getf pair :weight-b))
                   out)))))
     groups)
    (nreverse out)))
