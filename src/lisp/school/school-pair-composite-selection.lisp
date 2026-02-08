;;; school-pair-composite-selection.lisp - Pair-Composite selection/refresh logic

(in-package :swimmy.school)

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

(defun pair-promotable-p (pair &key oos-trades-a oos-trades-b)
  "Return non-nil metrics when pair can be promoted with OOS trades."
  (declare (ignore pair))
  (multiple-value-bind (metrics reason)
      (pair-metrics-from-trades oos-trades-a oos-trades-b :min-trades *pair-min-trades*)
    (declare (ignore reason))
    metrics))

(defun refresh-pair-strategies ()
  "Refresh pair strategy rows using backtest trade history."
  (init-db)
  (let* ((strategies *strategy-knowledge-base*)
         (trade-map (make-hash-table :test 'equal)))
    (dolist (s strategies)
      (let* ((name (strategy-name s))
             (rows (fetch-backtest-trades name)))
        (when rows
          (setf (gethash name trade-map) (backtest-rows->trade-list rows)))))
    (multiple-value-bind (pairs _diag)
        (select-pair-candidates strategies trade-map)
      (declare (ignore _diag))
      (dolist (pair pairs)
        (let* ((a (getf pair :a))
               (b (getf pair :b))
               (oos-a (backtest-rows->trade-list (fetch-backtest-trades a :oos-kind "OOS")))
               (oos-b (backtest-rows->trade-list (fetch-backtest-trades b :oos-kind "OOS")))
               (cpcv-a (backtest-rows->trade-list (fetch-backtest-trades a :oos-kind "CPCV")))
               (cpcv-b (backtest-rows->trade-list (fetch-backtest-trades b :oos-kind "CPCV")))
               (strat-a (find-strategy a))
               (symbol (and strat-a (strategy-symbol strat-a)))
               (timeframe (and strat-a (strategy-timeframe strat-a))))
          (multiple-value-bind (oos-metrics oos-reason)
              (pair-metrics-from-trades oos-a oos-b :min-trades *pair-min-trades*)
            (multiple-value-bind (cpcv-metrics cpcv-reason)
                (pair-metrics-from-trades cpcv-a cpcv-b :min-trades *pair-min-trades*)
              (declare (ignore oos-reason cpcv-reason))
              (let ((rank (cond ((and cpcv-metrics (>= (getf cpcv-metrics :sharpe) 0.5)) :S)
                                ((and oos-metrics (>= (getf oos-metrics :sharpe) 0.3)) :A)
                                (t :B))))
                (upsert-pair-strategy
                 (append pair
                         (list :strategy-a a
                               :strategy-b b
                               :profit-factor (or (getf pair :profit-factor) (getf pair :pf))
                               :symbol symbol
                               :timeframe timeframe
                               :rank rank
                               :oos-sharpe (and oos-metrics (getf oos-metrics :sharpe))
                               :cpcv-median (and cpcv-metrics (getf cpcv-metrics :sharpe))
                               :cpcv-pass-rate (and cpcv-metrics (getf cpcv-metrics :pass-rate)))))))))))))

(defun refresh-pair-active-defs ()
  "Refresh *pair-active-defs* using hybrid competition."
  (init-db)
  (let* ((pairs (fetch-pair-strategies))
         (active (select-active-pair-defs pairs *strategy-knowledge-base*)))
    (setf *pair-active-defs* active)))
