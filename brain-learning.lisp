;; brain-learning.lisp - Swimmy V4.0 Learning Module
;; Contains: Elder Lessons, Hall of Fame, Meta-Learning
;; Loaded by: brain-core.lisp

;;; NOTE: This file depends on the following from brain-core.lisp:
;;; - *current-regime*, *volatility-regime* (global state)
;;; - candle-close (from school.lisp, loaded before this)

;;; ═══════════════════════════════════════════════════════════════════
;;; ELDER LESSONS V3.0 - ML Engineer Feedback Implementation
;;; 6-Dimension Feature Engineering + Structured Pattern Learning
;;; ═══════════════════════════════════════════════════════════════════

;; Pattern structure: (count total-pnl avg-pnl last-seen)
(defparameter *elder-lessons* (make-hash-table :test 'equal))

;; Failure history for proper ML analysis
(defparameter *failure-history* nil)
(defparameter *max-failure-history* 100)

;; V3.0: Success counter for win rate (PM feedback)
(defparameter *success-count* 0)

;; V3.0: Win rate trend history (PM feedback - show trend)
(defparameter *win-rate-history* nil)  ; List of (timestamp win-rate)
(defparameter *max-win-rate-history* 7)  ; Track 7 days

(defstruct failure-record
  timestamp
  ;; 6-dimension feature vector (ML Engineer recommendation)
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
  "Convert RSI value to zone"
  (cond
    ((null rsi) :unknown)
    ((< rsi 30) :oversold)
    ((> rsi 70) :overbought)
    (t :neutral)))

(defun get-price-position (history)
  "Get price position relative to SMA50"
  (when (and history (> (length history) 50))
    (let* ((close (candle-close (first history)))
           (sma50 (/ (reduce #'+ (mapcar #'candle-close (subseq history 0 50))) 50))
           (diff-pct (/ (- close sma50) sma50)))
      (cond
        ((> diff-pct 0.005) :above-ma)
        ((< diff-pct -0.005) :below-ma)
        (t :at-ma)))))

(defun learn-from-failure (context pnl)
  "V3.0: 6-dimension feature learning from failed trades"
  (let* ((regime (or (getf context :regime) :unknown))
         (volatility (or (getf context :volatility-state) :normal))
         (session (or (getf context :session) :unknown))
         (rsi (getf context :rsi-value))
         (hour (mod (floor (get-universal-time) 3600) 24))
         (rsi-zone (rsi-to-zone rsi))
         (price-pos (or (getf context :price-position) :unknown))
         ;; Create 6-dimension pattern key
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
          ;; Update existing: (count total-pnl avg-pnl last-seen)
          (let* ((count (1+ (first existing)))
                 (total-pnl (+ (second existing) pnl))
                 (avg-pnl (/ total-pnl count)))
            (setf (gethash pattern-key *elder-lessons*)
                  (list count total-pnl avg-pnl (get-universal-time)))
            (when (>= count 5)  ;; V3.0: Increased from 3 to 5 (Professor feedback: n=3 not significant)
              (format t "[L] 👴 パターン学習: ~a → ~d回, 平均損失 ¥~,0f~%" 
                      pattern-key count avg-pnl)))
          ;; New pattern
          (setf (gethash pattern-key *elder-lessons*)
                (list 1 pnl pnl (get-universal-time)))))
    
    ;; Also update simple dimension counters for quick lookup
    (incf (gethash (format nil "dim:regime:~a" regime) *elder-lessons* 0))
    (incf (gethash (format nil "dim:vol:~a" volatility) *elder-lessons* 0))
    (incf (gethash (format nil "dim:session:~a" session) *elder-lessons* 0))
    (incf (gethash (format nil "dim:hour:~d" hour) *elder-lessons* 0))
    (incf (gethash (format nil "dim:rsi:~a" rsi-zone) *elder-lessons* 0))
    (incf (gethash (format nil "dim:price:~a" price-pos) *elder-lessons* 0))))

(defparameter *elder-lessons-last-decay* (get-universal-time))
(defparameter *elder-decay-interval* 86400)  ; 24 hours in seconds
(defparameter *elder-decay-rate* 0.9)        ; Reduce by 10% per day

(defun decay-elder-lessons ()
  "Apply decay to elder lessons - old lessons fade as market changes"
  (let ((now (get-universal-time)))
    (when (> (- now *elder-lessons-last-decay*) *elder-decay-interval*)
      (setf *elder-lessons-last-decay* now)
      (maphash (lambda (key value)
                 (let ((new-val (* value *elder-decay-rate*)))
                   (if (< new-val 0.5)
                       (remhash key *elder-lessons*)  ; Remove if too small
                       (setf (gethash key *elder-lessons*) new-val))))
               *elder-lessons*)
      (format t "[L] 👴 長老の記憶が薄れる...（減衰適用）~%"))))

(defun elder-learned-lesson-p (lesson-key threshold)
  "Check if elders have learned a specific lesson (V3.0: handles structured patterns)"
  (decay-elder-lessons)  ; Apply decay before checking
  (let ((lesson (gethash lesson-key *elder-lessons* nil)))
    (cond
      ;; New structured pattern: (count total-pnl avg-pnl last-seen)
      ((and (listp lesson) (>= (length lesson) 1))
       (>= (first lesson) threshold))
      ;; Old simple counter format (for dim: keys)
      ((numberp lesson)
       (>= lesson threshold))
      (t nil))))

(defun elder-vote (proposal context)
  "Ask elders to vote on a proposal. Returns :approve, :caution, or :reject"
  (let ((approve-votes 0)
        (reject-votes 0)
        (total-weight 0))
    
    (dolist (elder *hall-of-fame*)
      (let ((weight (elder-vote-weight elder)))
        (incf total-weight weight)
        
        ;; Elder logic based on their wisdom
        (cond
          ;; Elder who learned about volatility warns during high vol
          ((and (search "volatility" (string-downcase (elder-wisdom elder)))
                (eq (getf context :volatility-state) :extreme))
           (incf reject-votes weight)
           (format t "[L] 👴 Elder ~a: 「ボラが高すぎる。わしの時代もそうだった。」~%"
                   (elder-name elder)))
          
          ;; Elder who learned about patience during ranging markets
          ((and (search "patience" (string-downcase (elder-wisdom elder)))
                (eq (getf context :regime) :ranging))
           (incf reject-votes (* 0.5 weight))
           (format t "[L] 👴 Elder ~a: 「待て。レンジでは焦るな。」~%"
                   (elder-name elder)))
          
          ;; Otherwise, approve
          (t
           (incf approve-votes weight)))))
    
    ;; ═══════════════════════════════════════
    ;; DYNAMIC LESSONS from failure analysis
    ;; ═══════════════════════════════════════
    (when (elder-learned-lesson-p "extreme-volatility" 3)
      (when (eq (getf context :volatility-state) :extreme)
        (incf reject-votes 2)
        (format t "[L] 👴 集合知: 「過去の失敗から学んだ。極端なボラは危険だ。」~%")))
    
    (when (elder-learned-lesson-p "ranging-losses" 3)
      (when (eq (getf context :regime) :ranging)
        (incf reject-votes 1)
        (format t "[L] 👴 集合知: 「レンジ相場での失敗を覚えている。」~%")))
    
    ;; Decision
    (cond
      ((> reject-votes (* 0.6 total-weight)) :reject)
      ((> reject-votes (* 0.3 total-weight)) :caution)
      (t :approve))))

(defun save-hall-of-fame ()
  "Save Hall of Fame to file"
  (handler-case
      (with-open-file (out *hall-of-fame-path* :direction :output :if-exists :supersede)
        (write *hall-of-fame* :stream out :pretty t))
    (error (e) nil)))


;;; ==========================================
;;; META-LEARNING v2.0 (最高品質メタ学習)
;;; ==========================================
;;; Features:
;;; - Sharpe ratio tracking per regime
;;; - Risk-adjusted lot sizing (Kelly criterion inspired)
;;; - Adaptive learning rate based on confidence
;;; - Historical performance decay
;;; - Multi-factor performance optimization

(defstruct performance-stats
  wins
  losses
  total-pnl
  pnl-squared-sum    ; For variance calculation
  max-drawdown
  win-streak
  lose-streak
  current-streak
  last-updated)

(defparameter *regime-performance* (make-hash-table :test 'equal))
(defparameter *learning-rate* 0.1)        ; How fast to adapt
(defparameter *performance-decay* 0.99)   ; Decay old data slightly
(defparameter *min-trades-for-adjust* 10) ; Minimum trades before adjusting

(defun get-or-create-stats (key)
  "Get or initialize performance stats for a key"
  (or (gethash key *regime-performance*)
      (setf (gethash key *regime-performance*)
            (make-performance-stats 
             :wins 0 :losses 0 :total-pnl 0.0
             :pnl-squared-sum 0.0 :max-drawdown 0.0
             :win-streak 0 :lose-streak 0 :current-streak 0
             :last-updated (get-universal-time)))))

(defun record-regime-performance (won pnl)
  "Track comprehensive performance in current market conditions"
  (let* ((key (format nil "~a:~a" *current-regime* *volatility-regime*))
         (stats (get-or-create-stats key)))
    ;; Update basic stats
    (if won
        (progn
          (incf (performance-stats-wins stats))
          (if (> (performance-stats-current-streak stats) 0)
              (incf (performance-stats-current-streak stats))
              (setf (performance-stats-current-streak stats) 1))
          (setf (performance-stats-win-streak stats)
                (max (performance-stats-win-streak stats)
                     (performance-stats-current-streak stats))))
        (progn
          (incf (performance-stats-losses stats))
          (if (< (performance-stats-current-streak stats) 0)
              (decf (performance-stats-current-streak stats))
              (setf (performance-stats-current-streak stats) -1))
          (setf (performance-stats-lose-streak stats)
                (max (performance-stats-lose-streak stats)
                     (abs (performance-stats-current-streak stats))))))
    
    ;; Update PnL stats for Sharpe calculation
    (incf (performance-stats-total-pnl stats) pnl)
    (incf (performance-stats-pnl-squared-sum stats) (* pnl pnl))
    
    ;; Track drawdown
    (when (< pnl 0)
      (setf (performance-stats-max-drawdown stats)
            (min (performance-stats-max-drawdown stats) pnl)))
    
    (setf (performance-stats-last-updated stats) (get-universal-time))
    (setf (gethash key *regime-performance*) stats)))

(defun calculate-regime-sharpe (regime volatility)
  "Calculate Sharpe ratio for a specific regime/volatility combo"
  (let* ((key (format nil "~a:~a" regime volatility))
         (stats (gethash key *regime-performance*)))
    (if (and stats (> (+ (performance-stats-wins stats) 
                         (performance-stats-losses stats)) 5))
        (let* ((n (+ (performance-stats-wins stats) (performance-stats-losses stats)))
               (mean-pnl (/ (performance-stats-total-pnl stats) n))
               (variance (- (/ (performance-stats-pnl-squared-sum stats) n)
                           (* mean-pnl mean-pnl)))
               (std-dev (if (> variance 0) (sqrt variance) 1.0)))
          (/ mean-pnl (max std-dev 0.001)))
        0.0)))

(defun calculate-kelly-fraction (regime volatility)
  "Calculate Kelly criterion inspired position sizing"
  (let* ((key (format nil "~a:~a" regime volatility))
         (stats (gethash key *regime-performance*)))
    (if (and stats (> (+ (performance-stats-wins stats) 
                         (performance-stats-losses stats)) 10))
        (let* ((wins (performance-stats-wins stats))
               (losses (performance-stats-losses stats))
               (total (+ wins losses))
               (win-rate (/ wins total))
               (avg-win (if (> wins 0) 
                           (/ (max 0 (performance-stats-total-pnl stats)) wins)
                           0.01))
               (avg-loss (if (> losses 0)
                            (/ (abs (min 0 (performance-stats-total-pnl stats))) losses)
                            0.01))
               (win-loss-ratio (/ avg-win (max avg-loss 0.001)))
               ;; Kelly formula: f = (bp - q) / b where b=win/loss ratio, p=win rate, q=1-p
               (kelly (/ (- (* win-loss-ratio win-rate) (- 1 win-rate)) 
                        (max win-loss-ratio 0.001))))
          ;; Half-Kelly for safety, clamped to reasonable range
          (max 0.25 (min 1.5 (* kelly 0.5))))
        1.0)))  ; Default to full size if not enough data

(defun get-regime-win-rate (regime volatility)
  "Get win rate for specific regime/volatility combo"
  (let* ((key (format nil "~a:~a" regime volatility))
         (stats (gethash key *regime-performance*)))
    (if (and stats (> (+ (performance-stats-wins stats) 
                         (performance-stats-losses stats)) 0))
        (let ((total (+ (performance-stats-wins stats) 
                        (performance-stats-losses stats))))
          (/ (performance-stats-wins stats) total))
        0.5)))

(defun meta-adjust-lot (base-lot)
  "Advanced lot size adjustment based on meta-learned performance"
  (let* ((sharpe (calculate-regime-sharpe *current-regime* *volatility-regime*))
         (kelly (calculate-kelly-fraction *current-regime* *volatility-regime*))
         (win-rate (get-regime-win-rate *current-regime* *volatility-regime*))
         (key (format nil "~a:~a" *current-regime* *volatility-regime*))
         (stats (gethash key *regime-performance*))
         (streak (if stats (performance-stats-current-streak stats) 0)))
    
    ;; Multi-factor adjustment
    (let ((multiplier 1.0))
      ;; Sharpe-based adjustment
      (cond
        ((> sharpe 1.0) (setf multiplier (* multiplier 1.3)))   ; Great Sharpe
        ((> sharpe 0.5) (setf multiplier (* multiplier 1.1)))   ; Good Sharpe
        ((< sharpe -0.5) (setf multiplier (* multiplier 0.6)))  ; Bad Sharpe
        ((< sharpe 0) (setf multiplier (* multiplier 0.8))))    ; Negative Sharpe
      
      ;; Kelly adjustment (blend with 1.0)
      (setf multiplier (* multiplier (+ (* 0.5 kelly) 0.5)))
      
      ;; Win rate adjustment
      (cond
        ((> win-rate 0.65) (setf multiplier (* multiplier 1.15)))
        ((< win-rate 0.35) (setf multiplier (* multiplier 0.7))))
      
      ;; Streak adjustment (reduce after losses, increase after wins)
      (cond
        ((< streak -3) (setf multiplier (* multiplier 0.5)))   ; Losing streak
        ((> streak 3) (setf multiplier (* multiplier 1.2))))   ; Winning streak
      
      ;; Log significant adjustments
      (when (or (> multiplier 1.15) (< multiplier 0.85))
        (format t "[L] 🧠 META-LOT: ~,2fx (Sharpe:~,2f Kelly:~,2f WR:~,0f% Streak:~d)~%"
                multiplier sharpe kelly (* 100 win-rate) streak))
      
      (* base-lot (max 0.3 (min 2.0 multiplier))))))

(defun decay-old-performance ()
  "Apply decay to old performance data to adapt to changing markets"
  (maphash (lambda (key stats)
             (declare (ignore key))
             ;; Slightly decay old stats
             (setf (performance-stats-wins stats) 
                   (floor (* (performance-stats-wins stats) *performance-decay*)))
             (setf (performance-stats-losses stats)
                   (floor (* (performance-stats-losses stats) *performance-decay*)))
             (setf (performance-stats-total-pnl stats)
                   (* (performance-stats-total-pnl stats) *performance-decay*))
             (setf (performance-stats-pnl-squared-sum stats)
                   (* (performance-stats-pnl-squared-sum stats) *performance-decay*)))
           *regime-performance*))

(defun update-nn-threshold (won)
  "Update NN threshold based on comprehensive performance analysis"
  (if won (incf *nn-wins*) (incf *nn-losses*))
  (let ((total (+ *nn-wins* *nn-losses*)))
    (when (> total *min-trades-for-adjust*)
      (let* ((win-rate (/ *nn-wins* total))
             (sharpe (calculate-regime-sharpe *current-regime* *volatility-regime*))
             ;; Adjust threshold based on both win rate and Sharpe
             (adjustment (* *learning-rate* 
                           (+ (* 0.5 (- 0.5 win-rate))  ; Win rate component
                              (* 0.5 (- 0 sharpe))))))  ; Sharpe component
        (cond
          ((> win-rate 0.6)
           (setf *nn-threshold* (max 0.30 (- *nn-threshold* (* adjustment 2))))
           (format t "[L] 📈 WR:~,0f% Sharpe:~,2f → threshold:~,2f~%" 
                   (* win-rate 100) sharpe *nn-threshold*))
          ((< win-rate 0.4)
           (setf *nn-threshold* (min 0.80 (+ *nn-threshold* (* (abs adjustment) 2))))
           (format t "[L] 📉 WR:~,0f% Sharpe:~,2f → threshold:~,2f~%" 
                   (* win-rate 100) sharpe *nn-threshold*))))))
  ;; Record for meta-learning
  (record-regime-performance won 0))

(defun get-meta-learning-summary ()
  "Get summary of meta-learning state"
  (let ((summaries nil))
    (maphash (lambda (key stats)
               (let ((total (+ (performance-stats-wins stats) 
                              (performance-stats-losses stats))))
                 (when (> total 5)
                   (push (format nil "~a: WR=~,0f% Sharpe=~,2f"
                                 key
                                 (* 100 (/ (performance-stats-wins stats) total))
                                 (calculate-regime-sharpe 
                                  (intern (subseq key 0 (position #\: key)) :keyword)
                                  (intern (subseq key (1+ (position #\: key))) :keyword)))
                         summaries))))
             *regime-performance*)
    (or summaries (list "No data yet"))))
