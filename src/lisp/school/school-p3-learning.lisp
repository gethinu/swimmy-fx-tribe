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

(defun analyze-graveyard-for-avoidance ()
  "Analyze graveyard.sexp for SL/TP regions to avoid."
  (let ((patterns (apply-p3-time-decay (load-graveyard-patterns)))
        (clusters (make-hash-table :test 'equal)))
    (dolist (p patterns)
      (let ((key (list (getf p :timeframe) (getf p :direction) (getf p :symbol))))
        (push p (gethash key clusters nil))))
    (let ((avoid-regions nil))
      (maphash (lambda (key failures)
                 (when (>= (length failures) 5)
                   ;; V47.5: Filter nil values to prevent min/max errors
                   (let ((sls (remove nil (mapcar (lambda (f) (getf f :sl)) failures)))
                         (tps (remove nil (mapcar (lambda (f) (getf f :tp)) failures))))
                     (when (and sls tps)  ;; Only if non-empty after filtering
                       (push (list :tf (first key) :dir (second key) :sym (third key)
                                   :sl-min (apply #'min sls) :sl-max (apply #'max sls)
                                   :tp-min (apply #'min tps) :tp-max (apply #'max tps)
                                   :failure-count (length failures))
                             avoid-regions)))))
               clusters)
      (format t "[GRAVEYARD] 📊 Found ~d avoid regions~%" (length avoid-regions))
      avoid-regions)))

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

