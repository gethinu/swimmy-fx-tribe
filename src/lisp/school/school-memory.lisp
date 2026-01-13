;;; school-memory.lisp - Memory System (記憶と想起)
;;; Part of the Swimmy School System
;;; Extracted from school-learning.lisp (Expert Panel 2026-01-13)

(in-package :swimmy.school)

;;; ==========================================
;;; MEMORY SYSTEM (記憶と想起)
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
