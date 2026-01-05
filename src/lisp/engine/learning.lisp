;;; ============================================================================
;;; engine/learning.lisp - Pattern Learning System
;;; ============================================================================
;;; Elder lessons, failure analysis, pattern recognition
;;; Part of "The Efficient Gardener" refactoring
;;;
;;; NOTE: Hall of Fame _ceremony_ display (with emoji) is in shell/narrative.lisp
;;; ============================================================================

(in-package :swimmy.engine)

;;; ==========================================
;;; ELDER STRUCTURES
;;; ==========================================

(defparameter *hall-of-fame* nil)
(defparameter *hall-of-fame-path* "/home/swimmy/swimmy/.opus/hall_of_fame.lisp")

(defstruct elder
  name              ; Strategy name
  peak-pnl          ; Highest profit achieved
  era               ; When they were active
  speciality        ; What they were good at
  wisdom            ; Lessons learned
  vote-weight)      ; How much their vote counts

;;; ==========================================
;;; FAILURE LEARNING
;;; ==========================================

;; Pattern structure: (count total-pnl avg-pnl last-seen)
(defparameter *elder-lessons* (make-hash-table :test 'equal))
(defparameter *failure-history* nil)
(defparameter *max-failure-history* 100)
(defparameter *success-count* 0)
(defparameter *win-rate-history* nil)
(defparameter *max-win-rate-history* 7)

(defstruct failure-record
  timestamp
  ;; 6-dimension feature vector
  regime          ; :trending / :ranging
  volatility      ; :high / :normal / :low
  session         ; :tokyo / :london / :ny / :off
  hour            ; 0-23
  rsi-zone        ; :oversold / :neutral / :overbought
  price-position  ; :above-ma / :below-ma / :at-ma
  ;; Outcome
  pnl
  symbol
  direction)

(defun rsi-to-zone (rsi)
  "Convert RSI value to zone."
  (cond
    ((null rsi) :unknown)
    ((< rsi 30) :oversold)
    ((> rsi 70) :overbought)
    (t :neutral)))

(defun get-price-position (history)
  "Get price position relative to SMA50."
  (when (and history (> (length history) 50))
    (let* ((close (candle-close (first history)))
           (sma50 (/ (reduce #'+ (mapcar #'candle-close (subseq history 0 50))) 50))
           (diff-pct (/ (- close sma50) sma50)))
      (cond
        ((> diff-pct 0.005) :above-ma)
        ((< diff-pct -0.005) :below-ma)
        (t :at-ma)))))

(defun learn-from-failure (context pnl)
  "6-dimension feature learning from failed trades.
   Context should contain :regime, :volatility-state, :session, :rsi-value, :price-position, :symbol, :direction"
  (let* ((regime (or (getf context :regime) :unknown))
         (volatility (or (getf context :volatility-state) :normal))
         (session (or (getf context :session) :unknown))
         (rsi (getf context :rsi-value))
         (hour (mod (floor (get-universal-time) 3600) 24))
         (rsi-zone (rsi-to-zone rsi))
         (price-pos (or (getf context :price-position) :unknown))
         (pattern-key (format nil "~a|~a|~a|~d|~a|~a"
                              regime volatility session hour rsi-zone price-pos)))
    
    ;; Store structured failure record
    (push (make-failure-record
           :timestamp (get-universal-time)
           :regime regime
           :volatility volatility
           :session session
           :hour hour
           :rsi-zone rsi-zone
           :price-position price-pos
           :pnl pnl
           :symbol (getf context :symbol)
           :direction (getf context :direction))
          *failure-history*)
    
    ;; Trim history
    (when (> (length *failure-history*) *max-failure-history*)
      (setf *failure-history* (subseq *failure-history* 0 *max-failure-history*)))
    
    ;; Update pattern statistics
    (let ((existing (gethash pattern-key *elder-lessons*)))
      (if existing
          (let* ((count (1+ (first existing)))
                 (total-pnl (+ (second existing) pnl))
                 (avg-pnl (/ total-pnl count)))
            (setf (gethash pattern-key *elder-lessons*)
                  (list count total-pnl avg-pnl (get-universal-time)))
            (when (>= count 5)
              (format t "[ENGINE] Pattern learned: ~a -> ~d occurrences, avg loss ~,0f~%"
                      pattern-key count avg-pnl)))
          (setf (gethash pattern-key *elder-lessons*)
                (list 1 pnl pnl (get-universal-time)))))
    
    ;; Update dimension counters for quick lookup
    (incf (gethash (format nil "dim:regime:~a" regime) *elder-lessons* 0))
    (incf (gethash (format nil "dim:vol:~a" volatility) *elder-lessons* 0))
    (incf (gethash (format nil "dim:session:~a" session) *elder-lessons* 0))
    (incf (gethash (format nil "dim:hour:~d" hour) *elder-lessons* 0))
    (incf (gethash (format nil "dim:rsi:~a" rsi-zone) *elder-lessons* 0))
    (incf (gethash (format nil "dim:price:~a" price-pos) *elder-lessons* 0))))

;;; ==========================================
;;; LESSON DECAY
;;; ==========================================

(defparameter *elder-lessons-last-decay* (get-universal-time))
(defparameter *elder-decay-interval* 86400)  ; 24 hours
(defparameter *elder-decay-rate* 0.9)        ; Reduce by 10% per day

(defun decay-elder-lessons ()
  "Apply decay to elder lessons - old lessons fade as market changes."
  (let ((now (get-universal-time)))
    (when (> (- now *elder-lessons-last-decay*) *elder-decay-interval*)
      (setf *elder-lessons-last-decay* now)
      (maphash (lambda (key value)
                 (let ((new-val (* value *elder-decay-rate*)))
                   (if (< new-val 0.5)
                       (remhash key *elder-lessons*)
                       (setf (gethash key *elder-lessons*) new-val))))
               *elder-lessons*)
      (format t "[ENGINE] Elder lessons decayed~%"))))

(defun elder-learned-lesson-p (lesson-key threshold)
  "Check if elders have learned a specific lesson."
  (decay-elder-lessons)
  (let ((lesson (gethash lesson-key *elder-lessons* nil)))
    (cond
      ((and (listp lesson) (>= (length lesson) 1))
       (>= (first lesson) threshold))
      ((numberp lesson)
       (>= lesson threshold))
      (t nil))))

;;; ==========================================
;;; HALL OF FAME MANAGEMENT
;;; ==========================================

(defun induct-to-hall-of-fame (strategy-name peak-pnl speciality wisdom)
  "Induct a legendary strategy into the Hall of Fame.
   Returns the created elder object."
  (let ((elder (make-elder
                :name strategy-name
                :peak-pnl peak-pnl
                :era (multiple-value-bind (s m h day month year)
                         (decode-universal-time (get-universal-time))
                       (declare (ignore s m h))
                       (format nil "~d-~2,'0d-~2,'0d" year month day))
                :speciality speciality
                :wisdom wisdom
                :vote-weight (min 3 (/ peak-pnl 1000)))))
    (push elder *hall-of-fame*)
    (format t "[ENGINE] Elder inducted: ~a~%" strategy-name)
    elder))

(defun elder-vote (proposal context)
  "Ask elders to vote on a proposal. Returns :approve, :caution, or :reject"
  (let ((approve-votes 0)
        (reject-votes 0)
        (total-weight 0))
    
    (dolist (elder *hall-of-fame*)
      (let ((weight (elder-vote-weight elder)))
        (incf total-weight weight)
        
        (cond
          ((and (search "volatility" (string-downcase (elder-wisdom elder)))
                (eq (getf context :volatility-state) :extreme))
           (incf reject-votes weight))
          
          ((and (search "patience" (string-downcase (elder-wisdom elder)))
                (eq (getf context :regime) :ranging))
           (incf reject-votes (* 0.5 weight)))
          
          (t (incf approve-votes weight)))))
    
    ;; Dynamic lessons from failure analysis
    (when (elder-learned-lesson-p "extreme-volatility" 3)
      (when (eq (getf context :volatility-state) :extreme)
        (incf reject-votes 2)))
    
    (when (elder-learned-lesson-p "ranging-losses" 3)
      (when (eq (getf context :regime) :ranging)
        (incf reject-votes 1)))
    
    ;; Decision
    (cond
      ((> reject-votes (* 0.6 total-weight)) :reject)
      ((> reject-votes (* 0.3 total-weight)) :caution)
      (t :approve))))

(defun save-hall-of-fame ()
  "Save Hall of Fame to file."
  (handler-case
      (progn
        (ensure-directories-exist *hall-of-fame-path*)
        (with-open-file (out *hall-of-fame-path* :direction :output :if-exists :supersede)
          (write *hall-of-fame* :stream out :pretty t)))
    (error (e)
      (format t "[ENGINE] Failed to save Hall of Fame: ~a~%" e))))

(defun load-hall-of-fame ()
  "Load Hall of Fame from file."
  (handler-case
      (with-open-file (in *hall-of-fame-path* :direction :input :if-does-not-exist nil)
        (when in
          (setf *hall-of-fame* (read in))
          (format t "[ENGINE] Loaded ~d elders~%" (length *hall-of-fame*))))
    (error (e)
      (format t "[ENGINE] Failed to load Hall of Fame: ~a~%" e))))

(format t "[ENGINE] learning.lisp loaded~%")
