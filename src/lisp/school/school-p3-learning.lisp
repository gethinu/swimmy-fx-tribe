;;; src/lisp/school/school-p3-learning.lisp
;;; ============================================================================
;;; P3: LEARNING ADVANCED (V47.3 - Expert Panel 2026-01-21)
;;; ============================================================================
;;; Components: Graveyard Analysis, RL Priority, Q-Learning, Time Decay, File Rotation
;;; Extracted from school-learning.lisp for SRP compliance
;;; ============================================================================

(in-package :swimmy.school)

;;; ---------------------------------------------------------------------------
;;; RL REWARD SYSTEM (V47.3)
;;; ---------------------------------------------------------------------------

(defparameter *rl-rewards-file* "data/memory/rl_rewards.sexp"
  "File to store S-RANK trade rewards for reinforcement learning.")

(defun record-rl-reward (strategy trade-result)
  "Record S-RANK trade result as RL reward."
  (when (eq (strategy-rank strategy) :S)
    (let ((reward-entry (list :strategy (strategy-name strategy)
                              :timeframe (strategy-timeframe strategy)
                              :direction (strategy-direction strategy)
                              :symbol (strategy-symbol strategy)
                              :sl (strategy-sl strategy)
                              :tp (strategy-tp strategy)
                              :pnl trade-result
                              :reward (if (> trade-result 0) 1.0 -0.5)
                              :timestamp (get-universal-time))))
      (handler-case
          (with-open-file (stream *rl-rewards-file*
                                  :direction :output
                                  :if-exists :append
                                  :if-does-not-exist :create)
            (write reward-entry :stream stream)
            (terpri stream)
            (format t "[RL] 🎯 Reward recorded: ~a → ~,2f~%" 
                    (strategy-name strategy) (getf reward-entry :reward)))
        (error (e)
          (format t "[RL] ⚠️ Failed to record reward: ~a~%" e))))))

(defun load-rl-rewards ()
  "Load all RL rewards from rl_rewards.sexp."
  (handler-case
      (when (probe-file *rl-rewards-file*)
        (with-open-file (stream *rl-rewards-file* :direction :input)
          (loop for reward = (read stream nil :eof)
                until (eq reward :eof)
                collect reward)))
    (error (e)
      (format t "[RL] ⚠️ Failed to load: ~a~%" e)
      nil)))

(defun get-param-priority (timeframe direction symbol)
  "Get exploration priority based on historical rewards."
  (let ((rewards (load-rl-rewards))
        (wins 0) (total 0))
    (dolist (r rewards)
      (when (and (eql (getf r :timeframe) timeframe)
                 (eq (getf r :direction) direction)
                 (string= (getf r :symbol) symbol))
        (incf total)
        (when (> (getf r :pnl 0) 0)
          (incf wins))))
    (if (> total 0)
        (+ 0.5 (* 1.0 (/ wins total)))
        1.0)))

;;; ---------------------------------------------------------------------------
;;; GRAVEYARD ANALYSIS (V47.3)
;;; ---------------------------------------------------------------------------

(defparameter *graveyard-file* "data/memory/graveyard.sexp")
(defparameter *p3-decay-rate* 0.9)
(defparameter *retired-file* "data/memory/retired.sexp")
(defparameter *retired-weight* 0.25)

(defun load-graveyard-patterns ()
  "Load all failure patterns from graveyard.sexp."
  (handler-case
      (when (probe-file *graveyard-file*)
        (with-open-file (stream *graveyard-file* :direction :input)
          (loop for pattern = (read stream nil :eof)
                until (eq pattern :eof)
                collect pattern)))
    (error (e)
      (format t "[GRAVEYARD] ⚠️ Failed: ~a~%" e)
      nil)))

(defun load-retired-patterns ()
  "Load retired patterns from retired.sexp."
  (handler-case
      (when (probe-file *retired-file*)
        (with-open-file (stream *retired-file* :direction :input)
          (loop for pattern = (read stream nil :eof)
                until (eq pattern :eof)
                collect pattern)))
    (error (e)
      (format t "[RETIRED] ⚠️ Failed: ~a~%" e)
      nil)))

(defun apply-p3-time-decay (patterns)
  "Apply time decay to patterns."
  (let ((now (get-universal-time))
        (month-seconds (* 30 24 60 60)))
    (mapcar (lambda (p)
              (let* ((timestamp (getf p :timestamp now))
                     (age-months (/ (- now timestamp) month-seconds))
                     (weight (expt *p3-decay-rate* age-months)))
                (append p (list :weight weight))))
            patterns)))

(defun apply-retired-weight (patterns)
  (mapcar (lambda (p)
            (let ((weight (getf p :weight 1.0)))
              (append p (list :weight (* weight *retired-weight*) :retired t))))
          patterns))

(defun analyze-graveyard-for-avoidance ()
  "Analyze graveyard.sexp for SL/TP regions to avoid.
   Stream patterns to avoid loading huge files into memory."
  (let* ((now (get-universal-time))
         (month-seconds (* 30 24 60 60))
         (clusters (make-hash-table :test 'equal)))
    (labels ((ensure-cluster (key)
               (or (gethash key clusters)
                   (setf (gethash key clusters)
                         (list :weight-sum 0.0
                               :count 0
                               :sl-min nil :sl-max nil
                               :tp-min nil :tp-max nil))))
             (update-min-max (entry sl tp)
               (when (numberp sl)
                 (let ((cur-min (getf entry :sl-min))
                       (cur-max (getf entry :sl-max)))
                   (setf (getf entry :sl-min) (if (or (null cur-min) (< sl cur-min)) sl cur-min))
                   (setf (getf entry :sl-max) (if (or (null cur-max) (> sl cur-max)) sl cur-max))))
               (when (numberp tp)
                 (let ((cur-min (getf entry :tp-min))
                       (cur-max (getf entry :tp-max)))
                   (setf (getf entry :tp-min) (if (or (null cur-min) (< tp cur-min)) tp cur-min))
                   (setf (getf entry :tp-max) (if (or (null cur-max) (> tp cur-max)) tp cur-max)))))
             (process-pattern (pattern weight-multiplier)
               (let* ((timestamp (getf pattern :timestamp now))
                      (age-months (/ (- now timestamp) month-seconds))
                      (weight (* (expt *p3-decay-rate* age-months) weight-multiplier))
                      (key (list (getf pattern :timeframe)
                                 (getf pattern :direction)
                                 (getf pattern :symbol)))
                      (entry (ensure-cluster key)))
                 (incf (getf entry :weight-sum) weight)
                 (incf (getf entry :count))
                 (update-min-max entry (getf pattern :sl) (getf pattern :tp))))
             (scan-pattern-file (path weight-multiplier label)
               (handler-case
                   (when (probe-file path)
                     (with-open-file (stream path :direction :input)
                       (loop for pattern = (read stream nil :eof)
                             until (eq pattern :eof)
                             do (when (listp pattern)
                                  (process-pattern pattern weight-multiplier)))))
                 (error (e)
                   (format t "[~a] ⚠️ Failed: ~a~%" label e)
                   nil))))
      (scan-pattern-file *graveyard-file* 1.0 "GRAVEYARD")
      (scan-pattern-file *retired-file* *retired-weight* "RETIRED")
      (let ((avoid-regions nil))
        (maphash (lambda (key entry)
                   (let ((effective-count (getf entry :weight-sum))
                         (sl-min (getf entry :sl-min))
                         (sl-max (getf entry :sl-max))
                         (tp-min (getf entry :tp-min))
                         (tp-max (getf entry :tp-max)))
                     (when (and (>= effective-count 5.0)
                                (numberp sl-min) (numberp sl-max)
                                (numberp tp-min) (numberp tp-max))
                       (push (list :tf (first key) :dir (second key) :sym (third key)
                                   :sl-min sl-min :sl-max sl-max
                                   :tp-min tp-min :tp-max tp-max
                                   :failure-count (getf entry :count))
                             avoid-regions))))
                 clusters)
        (format t "[GRAVEYARD] 📊 Found ~d avoid regions~%" (length avoid-regions))
        avoid-regions))))

(defun should-avoid-params-p (sl tp avoid-regions)
  "Check if SL/TP falls in an avoid region."
  (dolist (region avoid-regions)
    (when (and (>= sl (getf region :sl-min 0)) (<= sl (getf region :sl-max 1000))
               (>= tp (getf region :tp-min 0)) (<= tp (getf region :tp-max 1000)))
      (return-from should-avoid-params-p t)))
  nil)

;;; ---------------------------------------------------------------------------
;;; Q-LEARNING (V47.3 + V47.5 Persistence)
;;; ---------------------------------------------------------------------------

(defparameter *q-table* (make-hash-table :test 'equal))
(defparameter *q-table-file* "data/memory/q_table.sexp")
(defparameter *q-alpha* 0.1)
(defparameter *q-epsilon* 0.2)

(defun q-key (tf dir sym sl tp)
  (list tf dir sym (round sl 10) (round tp 10)))

(defun get-q-value (tf dir sym sl tp)
  (gethash (q-key tf dir sym sl tp) *q-table* 0.0))

(defun update-q-value (tf dir sym sl tp reward)
  "Update Q-value: Q = Q + α(r - Q)"
  (let* ((key (q-key tf dir sym sl tp))
         (old-q (gethash key *q-table* 0.0))
         (new-q (+ old-q (* *q-alpha* (- reward old-q)))))
    (setf (gethash key *q-table*) new-q)
    new-q))

(defun explore-or-exploit-p ()
  "ε-greedy: 20% explore, 80% exploit."
  (< (random 1.0) *q-epsilon*))

;;; V47.7: Q-Value Strategy Selection (Expert Panel Approved)
;;; Musk Conditions: Breeder only, 80/20 rule, daily decay

(defun get-top-q-sltps (tf dir sym n)
  "Get top N SL/TP combinations by Q-value for given TF/Dir/Symbol."
  (let ((candidates nil))
    (maphash (lambda (k v) 
               (when (and (= (first k) tf)
                          (eq (second k) dir)
                          (string= (third k) sym)
                          (> v 0))  ; Only positive Q-values
                 (push (list :sl (fourth k) :tp (fifth k) :q v) candidates)))
             *q-table*)
    (let ((sorted (sort candidates #'> :key (lambda (c) (getf c :q)))))
      (subseq sorted 0 (min n (length sorted))))))

(defun select-sltp-with-q (tf dir sym fallback-sl fallback-tp)
  "Select SL/TP using Q-table, with fallback values.
   80% use fallback (explore), 20% use Q-value (exploit)."
  (if (not (explore-or-exploit-p))
      (values fallback-sl fallback-tp)  ; 80% Explore
      ;; 20% Exploit: Use Q-value
      (let ((top-sltps (get-top-q-sltps tf dir sym 5)))
        (if (and top-sltps (> (length top-sltps) 0))
            (let ((chosen (nth (random (length top-sltps)) top-sltps)))
              (format t "[Q-VALUE] 🎯 Using learned SL=~d TP=~d (Q=~,2f)~%"
                      (getf chosen :sl) (getf chosen :tp) (getf chosen :q))
              (values (getf chosen :sl) (getf chosen :tp)))
            (values fallback-sl fallback-tp)))))

(defun decay-q-table ()
  "Apply daily time decay to Q-values (1% per day).
   López de Prado: Prevents overfitting to old market conditions."
  (let ((decay-rate 0.99)
        (count 0))
    (maphash (lambda (k v)
               (let ((new-v (* v decay-rate)))
                 (if (< (abs new-v) 0.01)
                     (remhash k *q-table*)  ; Remove negligible entries
                     (setf (gethash k *q-table*) new-v))
                 (incf count)))
             *q-table*)
    (format t "[Q-TABLE] ⏳ Decayed ~d entries by ~,1f%~%" count (* (- 1 decay-rate) 100))
    (save-q-table)))  ; Persist after decay


;;; V47.5: Q-table Persistence
(defun save-q-table ()
  "Save Q-table to disk for persistence across restarts."
  (handler-case
      (progn
        (ensure-directories-exist *q-table-file*)
        (with-open-file (stream *q-table-file*
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
          (let ((entries nil))
            (maphash (lambda (k v) (push (cons k v) entries)) *q-table*)
            (write entries :stream stream))
          (format t "[Q-TABLE] 💾 Saved ~d entries~%" (hash-table-count *q-table*))))
    (error (e)
      (format t "[Q-TABLE] ⚠️ Failed to save: ~a~%" e))))

(defun load-q-table ()
  "Load Q-table from disk."
  (handler-case
      (when (probe-file *q-table-file*)
        (with-open-file (stream *q-table-file* :direction :input)
          (let ((entries (read stream nil nil)))
            (clrhash *q-table*)
            (dolist (entry entries)
              (setf (gethash (car entry) *q-table*) (cdr entry)))
            (format t "[Q-TABLE] 📂 Loaded ~d entries~%" (hash-table-count *q-table*)))))
    (error (e)
      (format t "[Q-TABLE] ⚠️ Failed to load: ~a~%" e))))

;;; ---------------------------------------------------------------------------
;;; FILE ROTATION (V47.3)
;;; ---------------------------------------------------------------------------

(defparameter *max-file-size* (* 10 1024 1024))

(defun check-file-rotation-needed (filepath)
  (handler-case
      (when (probe-file filepath)
        (with-open-file (s filepath :direction :input :element-type '(unsigned-byte 8))
          (> (file-length s) *max-file-size*)))
    (error () nil)))

(defun rotate-file-if-needed (filepath)
  "Rotate file if >10MB."
  (when (check-file-rotation-needed filepath)
    (let ((backup (format nil "~a.~a.sexp" 
                          (pathname-name filepath)
                          (multiple-value-bind (s m h d mo y) 
                              (decode-universal-time (get-universal-time))
                            (declare (ignore s m h d mo))
                            y))))
      (rename-file filepath backup)
      (format t "[ROTATE] 📦 Rotated ~a -> ~a~%" filepath backup)
      t)))

;; Auto-load Q-table on file load
(load-q-table)

(format t "[P3] 🧠 V47.5 Learning Advanced Loaded (Q-table persistent)~%")
