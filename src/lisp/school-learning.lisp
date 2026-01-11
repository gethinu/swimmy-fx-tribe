;;; ============================================================================
;;; school-learning.lisp - Failure Learning System (Uncle Bob Split)

(in-package :swimmy.school)
;;; ============================================================================
;;; Extracted from school.lisp for Single Responsibility Principle
;;; Contains: Trade recording, failure;;; Extracted from school.lisp (V6.14) to reduce complexity
;;; ============================================================================

;;; ==========================================
;;; LEARNING PARAMETERS
;;; ==========================================

(defparameter *failure-log* nil           "List of failure records")
(defparameter *success-log* nil           "List of success records for comparison")
(defparameter *max-log-size* 500          "Keep more history")
(defparameter *decay-half-life* 3600      "1 hour in seconds")
(defparameter *min-samples-for-block* 5   "Need 5+ samples before blocking")

;;; ==========================================
;;; TRADE RECORD STRUCTURE
;;; ==========================================

(defstruct trade-record
  timestamp
  symbol
  direction           ; :buy or :sell
  category            ; :trend, :reversion, :breakout, :scalp
  strategy-name
  ;; Market Context (15+ variables)
  regime              ; :trending, :ranging
  volatility          ; :high, :normal, :low
  sma-position        ; :above, :below, :crossing
  rsi-value           ; actual RSI value 0-100
  rsi-zone            ; :overbought, :neutral, :oversold
  momentum            ; :accelerating, :decelerating, :flat
  spread-condition    ; :tight, :normal, :wide
  session             ; :tokyo, :london, :newyork, :overlap, :off
  day-of-week         ; 0-6
  hour-of-day         ; 0-23
  price-vs-high       ; distance from recent high (%)
  price-vs-low        ; distance from recent low (%)
  consecutive-candles ; number of same-direction candles
  volume-condition    ; :high, :normal, :low
  atr-percentile      ; current ATR vs historical
  ;; Outcome
  pnl
  hold-time           ; how long position was held (seconds)
  max-drawdown        ; worst point during trade
  hit-sl-or-tp)       ; :sl, :tp, :manual, :signal

;;; ==========================================
;;; MIGRATED: HALL OF FAME (From engine/learning)
;;; ==========================================

(defparameter *hall-of-fame* nil "List of legendary strategies")
(defparameter *hall-of-fame-path* "/home/swimmy/swimmy/.opus/hall_of_fame.lisp")

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
    (format t "[SCHOOL] Elder inducted into HoF: ~a~%" strategy-name)
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
      (format t "[SCHOOL] Failed to save Hall of Fame: ~a~%" e))))

(defun load-hall-of-fame ()
  "Load Hall of Fame from file."
  (handler-case
      (with-open-file (in *hall-of-fame-path* :direction :input :if-does-not-exist nil)
        (when in
          (setf *hall-of-fame* (read in))
          (format t "[SCHOOL] Loaded ~d elders from Hall of Fame~%" (length *hall-of-fame*))))
    (error (e)
      (format t "[SCHOOL] Failed to load Hall of Fame: ~a~%" e))))


;;; ==========================================
;;; SESSION & MOMENTUM DETECTION
;;; ==========================================

(defun get-current-session ()
  "Determine current trading session based on UTC time"
  (multiple-value-bind (sec min hour) (get-decoded-time)
    (declare (ignore sec min))
    (cond
      ((and (>= hour 0) (< hour 7)) :tokyo)
      ((and (>= hour 7) (< hour 8)) :overlap)
      ((and (>= hour 8) (< hour 12)) :london)
      ((and (>= hour 12) (< hour 14)) :overlap)
      ((and (>= hour 14) (< hour 21)) :newyork)
      (t :off))))

(defun calculate-momentum (history n)
  "Calculate price momentum over n candles"
  (when (> (length history) (+ n 1))
    (let* ((recent (mapcar #'candle-close (subseq history 0 n)))
           (older (mapcar #'candle-close (subseq history n (* 2 n))))
           (recent-change (- (first recent) (car (last recent))))
           (older-change (- (first older) (car (last older)))))
      (cond
        ((> recent-change (* older-change 1.5)) :accelerating)
        ((< recent-change (* older-change 0.5)) :decelerating)
        (t :flat)))))

(defun count-consecutive-candles (history direction)
  "Count consecutive candles in same direction"
  (let ((count 0))
    (dolist (c history)
      (let ((bullish (> (candle-close c) (candle-open c))))
        (if (eq (if bullish :up :down) direction)
            (incf count)
            (return count))))
    count))

;;; ==========================================
;;; MARKET CONTEXT CAPTURE
;;; ==========================================

(defun get-rich-market-context (symbol)
  "Capture comprehensive market context (15+ variables)"
  (let* ((history (or (gethash symbol *candle-histories*) *candle-history*))
         (close (and history (candle-close (first history))))
         (open (and history (candle-open (first history))))
         (sma20 (and history (> (length history) 20)
                     (/ (reduce #'+ (mapcar #'candle-close (subseq history 0 20))) 20)))
         (rsi (and history (> (length history) 14) 
                   (when (fboundp 'ind-rsi) (ind-rsi 14 history))))
         (atr (and history (> (length history) 14) 
                   (when (fboundp 'ind-atr) (ind-atr 14 history))))
         (recent-high (and history (> (length history) 20)
                           (reduce #'max (mapcar #'candle-high (subseq history 0 20)))))
         (recent-low (and history (> (length history) 20)
                          (reduce #'min (mapcar #'candle-low (subseq history 0 20)))))
         (now (multiple-value-list (get-decoded-time))))
    (list
     :regime (if (boundp '*current-regime*) *current-regime* :unknown)
     :volatility (if (boundp '*volatility-regime*) *volatility-regime* :normal)
     :sma-position (cond
                     ((null sma20) :unknown)
                     ((and close (> (abs (- close sma20)) (* close 0.0001)))
                      (if (> close sma20) :above :below))
                     (t :crossing))
     :rsi-value (or rsi 50)
     :rsi-zone (cond ((null rsi) :neutral)
                     ((> rsi 70) :overbought)
                     ((< rsi 30) :oversold)
                     (t :neutral))
     :momentum (calculate-momentum history 5)
     :spread-condition :normal
     :session (get-current-session)
     :day-of-week (nth 6 now)
     :hour-of-day (nth 2 now)
     :price-vs-high (if (and close recent-high (> recent-high 0))
                        (* 100 (/ (- recent-high close) recent-high)) 0)
     :price-vs-low (if (and close recent-low (> recent-low 0))
                       (* 100 (/ (- close recent-low) recent-low)) 0)
     :consecutive-candles (if (and history close open)
                              (count-consecutive-candles history (if (> close open) :up :down))
                              0)
     :atr-percentile (if atr (min 100 (* atr 1000)) 50))))

;;; ==========================================
;;; PATTERN SIMILARITY & DECAY
;;; ==========================================

(defun time-decay-weight (timestamp)
  "Calculate exponential decay weight based on age"
  (let* ((age (- (get-universal-time) timestamp))
         (half-lives (/ age *decay-half-life*)))
    (expt 0.5 half-lives)))

(defun pattern-similarity (ctx1 ctx2)
  "Calculate similarity between two contexts (0.0 to 1.0)"
  (let ((score 0) (total 0))
    ;; Exact matches for categorical variables (weight: 2)
    (dolist (key '(:regime :volatility :sma-position :rsi-zone :momentum :session))
      (incf total 2)
      (when (eq (getf ctx1 key) (getf ctx2 key))
        (incf score 2)))
    ;; Fuzzy matches for numerical variables (weight: 1)
    (let ((rsi1 (getf ctx1 :rsi-value))
          (rsi2 (getf ctx2 :rsi-value)))
      (incf total 1)
      (when (and rsi1 rsi2 (< (abs (- rsi1 rsi2)) 15))
        (incf score 1)))
    (let ((hour1 (getf ctx1 :hour-of-day))
          (hour2 (getf ctx2 :hour-of-day)))
      (incf total 1)
      (when (and hour1 hour2 (< (abs (- hour1 hour2)) 3))
        (incf score 1)))
    ;; Day of week match
    (incf total 1)
    (when (eql (getf ctx1 :day-of-week) (getf ctx2 :day-of-week))
      (incf score 1))
    (/ score (max 1 total))))

;;; ==========================================
;;; FAILURE CONFIDENCE CALCULATION
;;; ==========================================

(defun calculate-failure-confidence (symbol direction category)
  "Calculate confidence that this trade will fail (0.0 to 1.0)"
  (let* ((current-ctx (get-rich-market-context symbol))
         (weighted-failures 0.0)
         (weighted-successes 0.0)
         (similarity-sum 0.0))
    ;; Analyze failure log with time decay and similarity
    (dolist (record *failure-log*)
      (when (and (eq (trade-record-direction record) direction)
                 (eq (trade-record-category record) category))
        (let* ((record-ctx (list :regime (trade-record-regime record)
                                 :volatility (trade-record-volatility record)
                                 :sma-position (trade-record-sma-position record)
                                 :rsi-zone (trade-record-rsi-zone record)
                                 :rsi-value (trade-record-rsi-value record)
                                 :momentum (trade-record-momentum record)
                                 :session (trade-record-session record)
                                 :day-of-week (trade-record-day-of-week record)
                                 :hour-of-day (trade-record-hour-of-day record)))
               (similarity (pattern-similarity current-ctx record-ctx))
               (decay (time-decay-weight (trade-record-timestamp record)))
               (weight (* similarity decay)))
          (when (> similarity 0.5)
            (incf weighted-failures weight)
            (incf similarity-sum weight)))))
    ;; Analyze success log similarly
    (dolist (record *success-log*)
      (when (and (eq (trade-record-direction record) direction)
                 (eq (trade-record-category record) category))
        (let* ((record-ctx (list :regime (trade-record-regime record)
                                 :volatility (trade-record-volatility record)
                                 :sma-position (trade-record-sma-position record)
                                 :rsi-zone (trade-record-rsi-zone record)
                                 :rsi-value (trade-record-rsi-value record)
                                 :momentum (trade-record-momentum record)
                                 :session (trade-record-session record)
                                 :day-of-week (trade-record-day-of-week record)
                                 :hour-of-day (trade-record-hour-of-day record)))
               (similarity (pattern-similarity current-ctx record-ctx))
               (decay (time-decay-weight (trade-record-timestamp record)))
               (weight (* similarity decay)))
          (when (> similarity 0.5)
            (incf weighted-successes weight)
            (incf similarity-sum weight)))))
    ;; Calculate failure probability
    (if (< similarity-sum 0.5)
        0.0  ; Not enough similar data
        (/ weighted-failures (+ weighted-failures weighted-successes 0.001)))))

(defun get-failure-penalty (symbol direction category)
  "Get lot size multiplier based on failure probability (0.3 to 1.0)"
  (let ((failure-prob (calculate-failure-confidence symbol direction category)))
    (cond
      ((> failure-prob 0.8) 
       (format t "[L] 🛑 HIGH FAILURE RISK (~,1f%) - blocking~%" (* failure-prob 100))
       0.0)
      ((> failure-prob 0.6)
       (format t "[L] ⚠️ ELEVATED RISK (~,1f%) - reducing to 50%~%" (* failure-prob 100))
       0.5)
      ((> failure-prob 0.4)
       (format t "[L] 📊 MODERATE RISK (~,1f%) - reducing to 70%~%" (* failure-prob 100))
       0.7)
      (t 1.0))))

(defun should-block-trade-p (symbol direction category)
  "Check if this trade should be blocked based on failure analysis"
  (let ((penalty (get-failure-penalty symbol direction category)))
    (zerop penalty)))

;;; ==========================================
;;; TRADE OUTCOME RECORDING
;;; ==========================================

;;; ==========================================
;;; STRATEGY HISTORY & METRICS (V6.9 Fix)
;;; ==========================================

(defun update-strategy-metrics (strategy pnl)
  "Update strategy history and recalculate Sharpe Ratio."
  (when strategy
    ;; 1. Check if pnl-history slot exists (it should, we added it to struct)
    (when (slot-exists-p strategy 'pnl-history)
      (push pnl (strategy-pnl-history strategy))
      
      ;; Limit history size
      (when (> (length (strategy-pnl-history strategy)) 500)
        (setf (strategy-pnl-history strategy) (subseq (strategy-pnl-history strategy) 0 500)))

      ;; 2. Recalculate Sharpe
      (let* ((history (strategy-pnl-history strategy))
             (count (length history)))
        (when (> count 5)
          (let* ((mean (/ (reduce #'+ history) count))
                 (sq-diffs (mapcar (lambda (x) (expt (- x mean) 2)) history))
                 (variance (/ (reduce #'+ sq-diffs) count))
                 (std-dev (sqrt (max 0.000001 variance))))
            (setf (strategy-sharpe strategy) 
                  (if (> std-dev 0) (/ mean std-dev) 0.0))))))))

(defun record-trade-outcome (symbol direction category strategy-name pnl &key (hit :unknown) (hold-time 0))
  "Record trade outcome for learning (both wins and losses)"
  (let* ((ctx (get-rich-market-context symbol))
         (record (make-trade-record
                  :timestamp (get-universal-time)
                  :symbol symbol
                  :direction direction
                  :category category
                  :strategy-name strategy-name
                  :regime (getf ctx :regime)
                  :volatility (getf ctx :volatility)
                  :sma-position (getf ctx :sma-position)
                  :rsi-value (getf ctx :rsi-value)
                  :rsi-zone (getf ctx :rsi-zone)
                  :momentum (getf ctx :momentum)
                  :spread-condition (getf ctx :spread-condition)
                  :session (getf ctx :session)
                  :day-of-week (getf ctx :day-of-week)
                  :hour-of-day (getf ctx :hour-of-day)
                  :price-vs-high (getf ctx :price-vs-high)
                  :price-vs-low (getf ctx :price-vs-low)
                  :consecutive-candles (getf ctx :consecutive-candles)
                  :atr-percentile (getf ctx :atr-percentile)
                  :pnl pnl
                  :hold-time hold-time
                  :hit-sl-or-tp hit))
         (regime (getf ctx :regime))
         (won-p (> pnl 0)))
    (if (< pnl 0)
        (progn
          (push record *failure-log*)
          (when (> (length *failure-log*) *max-log-size*)
            (setf *failure-log* (subseq *failure-log* 0 *max-log-size*)))
          (format t "[L] 📉 FAILURE: ~a | ~a ~a | RSI:~,0f | Session:~a~%"
                  strategy-name direction (getf ctx :regime) 
                  (getf ctx :rsi-value) (getf ctx :session)))
        (progn
          (push record *success-log*)
          (when (> (length *success-log*) *max-log-size*)
            (setf *success-log* (subseq *success-log* 0 *max-log-size*)))
          (format t "[L] ✅ SUCCESS: ~a | ~a ~a | RSI:~,0f | Session:~a~%"
                  strategy-name direction (getf ctx :regime)
                  (getf ctx :rsi-value) (getf ctx :session))))
    
    ;; Update Strategy Specific Metrics (V6.9 Fix: The -0.19 Bug)
    (when strategy-name
      (let ((strat (or (find strategy-name *evolved-strategies* :key #'strategy-name :test #'string=)
                       (find strategy-name *strategy-knowledge-base* :key #'strategy-name :test #'string=))))
        (when strat
          (update-strategy-metrics strat pnl))))

    ;; Homework integration hooks
    (handler-case
        (when (fboundp 'on-trade-close-meta)
          (on-trade-close-meta regime strategy-name won-p pnl))
      (error () nil))
    (handler-case
        (when (fboundp 'record-proof-trade)
          (record-proof-trade pnl))
      (error () nil))
    (handler-case
        (when (and (fboundp 'record-template-result) category)
          (record-template-result category won-p 0.0))
      (error () nil))

    ;; P1 LEARNING FEEDBACK LOOP
    ;; Link actual outcome back to prediction history
    (when (boundp '*prediction-history*)
      (let ((pred (find-if (lambda (p) 
                             (and (string= (trade-prediction-symbol p) symbol)
                                  (eq (trade-prediction-direction p) direction)
                                  (null (trade-prediction-actual-outcome p))))
                           *prediction-history*)))
        (when pred
          (setf (trade-prediction-actual-outcome pred) (if won-p :win :loss))
          (format t "[L] 🧠 FEEDBACK: Updated prediction for ~a ~a -> ~a~%" 
                  symbol direction (if won-p :win :loss)))))))

;; Legacy compatibility
(defun record-failure (symbol direction category strategy-name pnl)
  (record-trade-outcome symbol direction category strategy-name pnl))

;;; ==========================================
;;; FAILURE ANALYSIS
;;; ==========================================

(defun analyze-failure-patterns ()
  "Statistical analysis of failure patterns"
  (let ((pattern-stats (make-hash-table :test 'equal)))
    (dolist (record *failure-log*)
      (let ((key (format nil "~a|~a|~a" 
                         (trade-record-direction record)
                         (trade-record-regime record)
                         (trade-record-session record))))
        (incf (gethash key pattern-stats 0))))
    (let ((sorted nil))
      (maphash (lambda (k v) (push (cons k v) sorted)) pattern-stats)
      (sort sorted #'> :key #'cdr))))

(defun get-failure-summary ()
  "Get detailed failure summary"
  (let* ((patterns (analyze-failure-patterns))
         (total-failures (length *failure-log*))
         (total-successes (length *success-log*))
         (overall-rate (if (> (+ total-failures total-successes) 0)
                           (* 100 (/ total-successes (+ total-failures total-successes)))
                           50)))
    (format nil "Overall win rate: ~,1f% (~d wins, ~d losses)"
            overall-rate total-successes total-failures)))


;;; ==========================================
;;; MEMORY SYSTEM (記憶と想起) - Merged from school.lisp
;;; ==========================================
;;; Features:
;;; - Store market patterns with outcomes
;;; - Similarity-based pattern retrieval
;;; - Experience-based decision making
;;; - Episodic and semantic memory

(defparameter *episodic-memory* nil)          ; Specific trade memories
(defparameter *semantic-memory* nil)          ; Generalized patterns
(defparameter *max-episodic-memory* 1000)
(defparameter *max-semantic-memory* 100)
(defparameter *memory-similarity-threshold* 0.7)

(defstruct memory-episode
  timestamp
  symbol
  ;; Pattern features
  pattern-hash          ; Quick lookup key
  regime
  volatility
  rsi-value
  momentum-direction
  sma-position
  hour-of-day
  day-of-week
  consecutive-candles
  price-range           ; High-low range as %
  ;; What happened
  trade-direction
  outcome               ; :win :loss :breakeven
  pnl
  hold-time)

(defstruct semantic-pattern
  key                   ; Pattern identifier
  occurrences           ; How many times seen
  win-count
  loss-count
  total-pnl
  avg-hold-time
  best-direction        ; Most successful direction
  last-seen)

(defun create-pattern-hash (regime volatility rsi-zone momentum sma-pos hour)
  "Create a hash key for pattern lookup"
  (format nil "~a|~a|~a|~a|~a|~a" 
          regime volatility rsi-zone momentum sma-pos 
          (cond ((< hour 8) :asia)
                ((< hour 16) :europe)
                (t :america))))

(defun capture-current-pattern (symbol)
  "Capture current market pattern for memory"
  (let* ((ctx (get-rich-market-context symbol))
         (history (or (gethash symbol *candle-histories*) *candle-history*))
         (range (if (and history (> (length history) 10))
                    (let* ((highs (mapcar #'candle-high (subseq history 0 10)))
                           (lows (mapcar #'candle-low (subseq history 0 10)))
                           (max-h (reduce #'max highs))
                           (min-l (reduce #'min lows)))
                      (if (> min-l 0) (* 100 (/ (- max-h min-l) min-l)) 0))
                    0))
         (rsi-zone (getf ctx :rsi-zone))
         (hour (getf ctx :hour-of-day)))
    (list
     :hash (create-pattern-hash (getf ctx :regime)
                                (getf ctx :volatility)
                                rsi-zone
                                (getf ctx :momentum)
                                (getf ctx :sma-position)
                                (getf ctx :hour-of-day)) ; Use extracted hour
     :regime (getf ctx :regime)
     :volatility (getf ctx :volatility)
     :rsi-value (getf ctx :rsi-value)
     :momentum (getf ctx :momentum)
     :sma-position (getf ctx :sma-position)
     :hour hour
     :day (getf ctx :day-of-week)
     :consecutive (getf ctx :consecutive-candles)
     :range range)))

(defun store-memory (symbol direction outcome pnl hold-time)
  "Store a trade experience in episodic memory"
  (let* ((pattern (capture-current-pattern symbol))
         (episode (make-memory-episode
                   :timestamp (get-universal-time)
                   :symbol symbol
                   :pattern-hash (getf pattern :hash)
                   :regime (getf pattern :regime)
                   :volatility (getf pattern :volatility)
                   :rsi-value (getf pattern :rsi-value)
                   :momentum-direction (getf pattern :momentum)
                   :sma-position (getf pattern :sma-position)
                   :hour-of-day (getf pattern :hour)
                   :day-of-week (getf pattern :day)
                   :consecutive-candles (getf pattern :consecutive)
                   :price-range (getf pattern :range)
                   :trade-direction direction
                   :outcome outcome
                   :pnl pnl
                   :hold-time hold-time)))
    ;; Add to episodic memory
    (push episode *episodic-memory*)
    (when (> (length *episodic-memory*) *max-episodic-memory*)
      (setf *episodic-memory* (subseq *episodic-memory* 0 *max-episodic-memory*)))
    
    ;; Update semantic memory (generalized patterns)
    (update-semantic-memory episode)))

(defun update-semantic-memory (episode)
  "Update generalized pattern knowledge from episode"
  (let* ((key (memory-episode-pattern-hash episode))
         (existing (find key *semantic-memory* :key #'semantic-pattern-key :test #'string=)))
    (if existing
        (progn
          (incf (semantic-pattern-occurrences existing))
          (if (eq (memory-episode-outcome episode) :win)
              (incf (semantic-pattern-win-count existing))
              (incf (semantic-pattern-loss-count existing)))
          (incf (semantic-pattern-total-pnl existing) (memory-episode-pnl episode))
          (setf (semantic-pattern-last-seen existing) (get-universal-time)))
        ;; Create new semantic pattern
        (let ((new-pattern (make-semantic-pattern
                            :key key
                            :occurrences 1
                            :win-count (if (eq (memory-episode-outcome episode) :win) 1 0)
                            :loss-count (if (eq (memory-episode-outcome episode) :loss) 1 0)
                            :total-pnl (memory-episode-pnl episode)
                            :avg-hold-time (memory-episode-hold-time episode)
                            :best-direction (memory-episode-trade-direction episode)
                            :last-seen (get-universal-time))))
          (push new-pattern *semantic-memory*)
          (when (> (length *semantic-memory*) *max-semantic-memory*)
            (setf *semantic-memory* 
                  (subseq (sort *semantic-memory* #'> :key #'semantic-pattern-occurrences)
                          0 *max-semantic-memory*)))))))

(defun calculate-pattern-similarity (pattern1 pattern2)
  "Calculate similarity between two patterns (0.0 to 1.0)"
  (let ((score 0) (total 8))
    (when (eq (getf pattern1 :regime) (memory-episode-regime pattern2))
      (incf score 2))
    (when (eq (getf pattern1 :volatility) (memory-episode-volatility pattern2))
      (incf score 2))
    (when (eq (getf pattern1 :sma-position) (memory-episode-sma-position pattern2))
      (incf score 1))
    (when (eq (getf pattern1 :momentum) (memory-episode-momentum-direction pattern2))
      (incf score 1))
    (when (< (abs (- (or (getf pattern1 :rsi-value) 50) 
                     (or (memory-episode-rsi-value pattern2) 50))) 15)
      (incf score 1))
    (when (< (abs (- (or (getf pattern1 :hour) 12) 
                     (or (memory-episode-hour-of-day pattern2) 12))) 4)
      (incf score 1))
    (/ score total)))

(defun recall-similar-experiences (symbol &optional (limit 10))
  "Retrieve similar past experiences from memory"
  (let* ((current (capture-current-pattern symbol))
         (matches nil))
    ;; Search episodic memory for similar patterns
    (dolist (episode *episodic-memory*)
      (let ((sim (calculate-pattern-similarity current episode)))
        (when (> sim *memory-similarity-threshold*)
          (push (cons sim episode) matches))))
    ;; Sort by similarity and return top matches
    (mapcar #'cdr 
            (subseq (sort matches #'> :key #'car)
                    0 (min limit (length matches))))))

(defun memory-suggests-direction (symbol)
  "Use memory to suggest trading direction"
  (let* ((similar (recall-similar-experiences symbol 20))
         (buy-wins 0) (buy-losses 0)
         (sell-wins 0) (sell-losses 0))
    ;; Analyze outcomes of similar situations
    (dolist (episode similar)
      (case (memory-episode-trade-direction episode)
        (:buy (if (eq (memory-episode-outcome episode) :win)
                  (incf buy-wins)
                  (incf buy-losses)))
        (:sell (if (eq (memory-episode-outcome episode) :win)
                   (incf sell-wins)
                   (incf sell-losses)))))
    
    (let* ((buy-total (+ buy-wins buy-losses))
           (sell-total (+ sell-wins sell-losses))
           (buy-rate (if (> buy-total 0) (/ buy-wins buy-total) 0.5))
           (sell-rate (if (> sell-total 0) (/ sell-wins sell-total) 0.5)))
      
      (when (> (+ buy-total sell-total) 5)
        (format t "[L] 🧠 MEMORY: ~d similar patterns | BUY:~,0f% SELL:~,0f%~%"
                (length similar) (* 100 buy-rate) (* 100 sell-rate)))
      
      (cond
        ((and (> buy-rate 0.6) (> buy-total 3)) :buy)
        ((and (> sell-rate 0.6) (> sell-total 3)) :sell)
        (t nil)))))

(defun get-memory-confidence (symbol direction)
  "Get confidence level based on memory of similar situations"
  (let* ((similar (recall-similar-experiences symbol 20))
         (matching 0)
         (winning 0))
    (dolist (episode similar)
      (when (eq (memory-episode-trade-direction episode) direction)
        (incf matching)
        (when (eq (memory-episode-outcome episode) :win)
          (incf winning))))
    (if (> matching 0)
        (/ winning matching)
        0.5)))

(defun semantic-pattern-win-rate (key)
  "Get win rate for a semantic pattern"
  (let ((pattern (find key *semantic-memory* :key #'semantic-pattern-key :test #'string=)))
    (if (and pattern (> (semantic-pattern-occurrences pattern) 0))
        (/ (semantic-pattern-win-count pattern) (semantic-pattern-occurrences pattern))
        0.5)))

;;; ==========================================
;;; ECOSYSTEM DYNAMICS (生態系ダイナミクス) - Merged from school.lisp
;;; ==========================================
;;; Features:
;;; - Strategy diversity maintenance
;;; - Niche management (category balance)
;;; - Natural selection pressure
;;; - Population health monitoring
;;; - Symbiotic relationships

(defparameter *min-diversity-score* 0.3)    ; Minimum acceptable diversity
(defparameter *max-species-per-niche* 10)   ; Max strategies per category
(defparameter *extinction-threshold* -0.5)  ; Sharpe below this = extinction risk
(defparameter *reproduction-threshold* 0.5) ; Sharpe above this = reproduction chance

(defstruct ecosystem-state
  total-population
  diversity-score
  niche-balance
  health-score
  dominant-species
  endangered-species
  timestamp)

(defun calculate-diversity-score ()
  "Calculate Shannon diversity index of strategy population"
  (let* ((categories '(:trend :reversion :breakout :scalp))
         (counts (mapcar (lambda (cat) 
                          (length (gethash cat *category-pools*)))
                        categories))
         (total (reduce #'+ counts)))
    (if (> total 0)
        (let ((proportions (mapcar (lambda (c) (if (> c 0) (/ c total) 0)) counts)))
          ;; Shannon diversity: -sum(p * ln(p))
          (- (reduce #'+ (mapcar (lambda (p) 
                                  (if (> p 0) (* p (log p)) 0))
                                proportions))))
        0)))

(defun calculate-niche-balance ()
  "Calculate how balanced the niches (categories) are"
  (let* ((categories '(:trend :reversion :breakout :scalp))
         (counts (mapcar (lambda (cat) 
                          (length (gethash cat *category-pools*)))
                        categories))
         (total (max 1 (reduce #'+ counts)))
         (ideal (/ total 4.0))
         (deviations (mapcar (lambda (c) (abs (- c ideal))) counts))
         (avg-deviation (/ (reduce #'+ deviations) 4)))
    ;; Return 0-1 where 1 is perfect balance
    (max 0 (- 1 (/ avg-deviation ideal)))))

(defun identify-endangered-species ()
  "Find strategies at risk of extinction"
  (let ((endangered nil))
    (dolist (strat *evolved-strategies*)
      (when (and (strategy-sharpe strat)
                 (< (strategy-sharpe strat) *extinction-threshold*))
        (push (strategy-name strat) endangered)))
    endangered))

(defun identify-dominant-species ()
  "Find most successful strategies"
  (let ((dominant nil))
    (dolist (strat *evolved-strategies*)
      (when (and (strategy-sharpe strat)
                 (> (strategy-sharpe strat) *reproduction-threshold*))
        (push (cons (strategy-name strat) (strategy-sharpe strat)) dominant)))
    (sort dominant #'> :key #'cdr)))

(defun calculate-ecosystem-health ()
  "Calculate overall ecosystem health score"
  (let* ((diversity (calculate-diversity-score))
         (balance (calculate-niche-balance))
         (endangered (length (identify-endangered-species)))
         (dominant (length (identify-dominant-species)))
         (total-pop (length *evolved-strategies*)))
    ;; Health = diversity + balance - endangered ratio + dominant bonus
    (let ((health (+ (* diversity 0.3)
                     (* balance 0.3)
                     (if (> total-pop 0) (* 0.2 (- 1 (/ endangered total-pop))) 0.2)
                     (if (> total-pop 0) (* 0.2 (min 1 (/ dominant total-pop 0.5))) 0))))
      (max 0 (min 1 health)))))

;; Alias for brain.lisp compatibility
(defun get-population-health ()
  "Get overall ecosystem health score (alias)"
  (calculate-ecosystem-health))

(defun get-ecosystem-state ()
  "Capture current ecosystem state"
  (make-ecosystem-state
   :total-population (length *evolved-strategies*)
   :diversity-score (calculate-diversity-score)
   :niche-balance (calculate-niche-balance)
   :health-score (calculate-ecosystem-health)
   :dominant-species (mapcar #'car (identify-dominant-species))
   :endangered-species (identify-endangered-species)
   :timestamp (get-universal-time)))

(defun ecosystem-needs-diversity-p ()
  "Check if ecosystem needs more diversity"
  (< (calculate-diversity-score) *min-diversity-score*))

(defun get-underpopulated-niche ()
  "Find the category with fewest strategies"
  (let* ((categories '(:trend :reversion :breakout :scalp))
         (counts (mapcar (lambda (cat) 
                          (cons cat (length (gethash cat *category-pools*))))
                        categories))
         (sorted (sort counts #'< :key #'cdr)))
    (car (first sorted))))

(defun apply-natural-selection ()
  "Remove poor performers and allow good ones to reproduce"
  (let ((endangered (identify-endangered-species))
        (removed 0)
        (reproduced 0))
    
    ;; Remove endangered species (poor performers) WITH FUNERAL RITES
    (when endangered
      (dolist (name endangered)
        (let ((strat (find name *evolved-strategies* :key #'strategy-name :test #'string=)))
          (when (and strat (> (random 1.0) 0.3))  ; 70% chance of extinction
            (let ((final-pnl (or (strategy-sharpe strat) 0))
                  (lessons (format nil "Strategy ~a failed in ~a regime" 
                                   name (symbol-name (or *current-regime* :unknown)))))
              ;; Hold funeral ceremony
              (hold-funeral name final-pnl lessons)))))
      (setf *evolved-strategies* 
            (remove-if (lambda (s) 
                        (and (member (strategy-name s) endangered :test #'string=)
                             (> (random 1.0) 0.3)))
                      *evolved-strategies*))
      (setf removed (length endangered)))
    
    ;; Allow dominant species to reproduce (mutate)
    (let ((dominant (identify-dominant-species)))
      (when (and dominant (< (length *evolved-strategies*) 50))
        (dolist (d (subseq dominant 0 (min 2 (length dominant))))
          (let ((parent (find (car d) *evolved-strategies* 
                             :key #'strategy-name :test #'string=)))
            (when parent
              ;; Mutation with small changes
              (let ((child (mutate-strategy parent 0.2)))
                (push child *evolved-strategies*)
                (incf reproduced)))))))

    ;; V8.8: DIVERSITY INJECTION (Affirmative Action for Minorities)
    (when (ecosystem-needs-diversity-p)
      (let* ((weak-niche (get-underpopulated-niche))
             ;; Find potential parents in this niche (even if not dominant)
             (candidates (remove-if-not (lambda (s) (eq (strategy-category s) weak-niche))
                                      *evolved-strategies*)))
        (when candidates
          ;; Pick the best of the minority
          (let ((parent (first (sort candidates #'> :key (lambda (s) (or (strategy-sharpe s) -999))))))
            (format t "[L] 🧬 DIVERSITY INJECTION: Breeding ~a to fill ~a niche~%" (strategy-name parent) weak-niche)
            (let ((child (mutate-strategy parent 0.3))) ; Higher mutation rate
               (push child *evolved-strategies*)
               (incf reproduced))))))
    
    ;; Log ecosystem changes
    (when (or (> removed 0) (> reproduced 0))
      (format t "[L] 🌿 ECOSYSTEM: ~d extinct, ~d born | Health: ~,0f%~%"
              removed reproduced (* 100 (calculate-ecosystem-health))))))

(defun maintain-ecosystem-balance ()
  "Periodic ecosystem maintenance"
  ;; Apply natural selection pressure
  (apply-natural-selection)
  
  ;; Check diversity and suggest focus for new strategies
  (when (ecosystem-needs-diversity-p)
    (let ((weak-niche (get-underpopulated-niche)))
      (format t "[L] 🌱 Ecosystem needs ~a strategies for diversity~%" weak-niche)))
  
  ;; Report ecosystem state every N calls
  (let ((state (get-ecosystem-state)))
    (format t "[L] 🏞️ Population: ~d | Diversity: ~,2f | Balance: ~,0f% | Health: ~,0f%~%"
            (ecosystem-state-total-population state)
            (ecosystem-state-diversity-score state)
            (* 100 (ecosystem-state-niche-balance state))
            (* 100 (ecosystem-state-health-score state)))))

(defun get-ecosystem-recommendation ()
  "Get recommendation for new strategy generation"
  (let ((weak-niche (get-underpopulated-niche))
        (health (calculate-ecosystem-health)))
    (cond
      ((< health 0.3)
       (list :action :diversify
             :focus weak-niche
             :message "Ecosystem unhealthy - need diverse strategies"))
      ((ecosystem-needs-diversity-p)
       (list :action :specialize
             :focus weak-niche
             :message (format nil "Focus on ~a category" weak-niche)))
      (t
       (list :action :evolve
             :focus nil
             :message "Ecosystem healthy - continue evolution")))))

(format t "[SCHOOL] school-learning.lisp loaded - Failure Learning + Memory + Ecosystem~%")
