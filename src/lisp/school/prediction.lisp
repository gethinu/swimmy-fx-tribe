;;; ============================================================================
;;; school-research.lisp - Research Paper Implementations (Uncle Bob Split)

(in-package :swimmy.school)
;;; ============================================================================
;;; Extracted from school.lisp for Single Responsibility Principle
;;; Contains: Trade prediction, self-explanation, risk parity
;;; ============================================================================

;;; ==========================================
;;; TRADE PREDICTION SYSTEM
;;; ==========================================
;;; Inspired by: Go AI endgame prediction (KataGo score estimation)

(defparameter *prediction-history* nil     "Track predictions vs actuals")
(defparameter *prediction-accuracy* 0.0    "Running accuracy score")

(defstruct trade-prediction
  timestamp
  symbol
  direction          ; :buy or :sell
  predicted-outcome  ; :win or :loss
  confidence         ; 0.0-1.0
  factors            ; Factors that influenced prediction
  actual-outcome)    ; Filled in after trade closes

(defun predict-trade-outcome (symbol direction)
  "Predict whether a trade will be profitable"
  (let* ((history (or (gethash symbol *candle-histories*) *candle-history*))
         (factors nil)
         (win-score 0.0)
         (total-weight 0.0))
    
    (when (and history (> (length history) 50))
      ;; Factor 1: Trend alignment (weight: 3)
      (let* ((sma20 (/ (reduce #'+ (mapcar #'candle-close (subseq history 0 20))) 20))
             (sma50 (/ (reduce #'+ (mapcar #'candle-close (subseq history 0 50))) 50))
             (price (candle-close (first history)))
             (trend-up (and (> price sma20) (> sma20 sma50)))
             (trend-down (and (< price sma20) (< sma20 sma50)))
             (aligned (or (and trend-up (eq direction :buy))
                          (and trend-down (eq direction :sell)))))
        (incf total-weight 3.0)
        (when aligned (incf win-score 3.0))
        (push (list :trend-aligned aligned) factors))
      
      ;; Factor 2: Volatility state (weight: 2)
      (let ((vol-ok (not (eq *current-volatility-state* :extreme))))
        (incf total-weight 2.0)
        (when vol-ok (incf win-score 2.0))
        (push (list :volatility-ok vol-ok) factors))
      
      ;; Factor 3: Leader agreement (weight: 2)
      (when (and (boundp '*current-leader*) *current-leader* (fboundp 'get-leader-direction))
        (let* ((leader-dir (get-leader-direction history))
               (agrees (eq leader-dir direction)))
          (incf total-weight 2.0)
          (when agrees (incf win-score 2.0))
          (push (list :leader-agrees agrees) factors)))
      
      ;; Factor 4: Consecutive candles (weight: 1.5)
      (when (fboundp 'count-consecutive-candles)
        (let* ((up-count (count-consecutive-candles history :up))
               (down-count (count-consecutive-candles history :down))
               (momentum-aligned (or (and (> up-count 2) (eq direction :buy))
                                     (and (> down-count 2) (eq direction :sell)))))
          (incf total-weight 1.5)
          (when momentum-aligned (incf win-score 1.5))
          (push (list :momentum-aligned momentum-aligned) factors)))
      
      ;; Factor 5: Session favorability (weight: 1)
      (when (fboundp 'get-current-session)
        (let* ((session (get-current-session))
               (active-session (member session '(:tokyo :london :newyork :overlap))))
          (incf total-weight 1.0)
          (when active-session (incf win-score 1.0))
          (push (list :active-session (not (null active-session))) factors))))
    
    ;; Calculate prediction
    (let* ((confidence (if (> total-weight 0) (/ win-score total-weight) 0.5))
           (predicted-outcome (if (> confidence 0.5) :win :loss))
           (prediction (make-trade-prediction
                        :timestamp (get-universal-time)
                        :symbol symbol
                        :direction direction
                        :predicted-outcome predicted-outcome
                        :confidence confidence
                        :factors factors
                        :actual-outcome nil)))
      
      ;; Log prediction
      (format t "[L] 🔮 PREDICTION: ~a ~a → ~a (~,0f% confidence)~%"
              direction symbol predicted-outcome (* 100 confidence))
      
      ;; Store for later validation
      (push prediction *prediction-history*)
      (when (> (length *prediction-history*) 100)
        (setf *prediction-history* (subseq *prediction-history* 0 100)))
      
      prediction)))

(defun get-predicted-win-rate ()
  "Get recent prediction win rate"
  (let ((validated (remove-if-not #'trade-prediction-actual-outcome *prediction-history*))
        (correct 0))
    (dolist (p validated)
      (when (eq (trade-prediction-predicted-outcome p)
                (trade-prediction-actual-outcome p))
        (incf correct)))
    (if (> (length validated) 0)
        (/ correct (length validated))
        0.5)))

(defun should-take-trade-p (prediction)
  "Decide whether to take trade based on prediction - P0 HARDENED VERSION"
  (let ((conf (trade-prediction-confidence prediction))
        (outcome (trade-prediction-predicted-outcome prediction)))
    ;; P0: LOSS predictions are NEVER traded
    (when (eq outcome :loss)
      (format t "[L] 🚫 LOSS PREDICTION: Trade blocked (conf ~,0f%)~%" (* 100 conf))
      (return-from should-take-trade-p nil))
    ;; P0: Require 60%+ confidence (raised from 35%)
    (cond
      ((< conf 0.60)
       (format t "[L] ❌ CONFIDENCE TOO LOW (~,0f%): Skipping trade~%" (* 100 conf))
       nil)
      (t 
       (format t "[L] ✅ APPROVED: WIN prediction (~,0f% confidence)~%" (* 100 conf))
       t))))

;;; ==========================================
;;; RISK PARITY SYSTEM
;;; ==========================================
;;; Inspired by: Modern portfolio theory + All Weather Fund

(defparameter *category-volatilities* (make-hash-table :test 'equal))
(defparameter *target-total-risk* 0.02  "2% of portfolio per trade round")
(defparameter *trade-history* (make-hash-table :test 'equal))

(defun calculate-category-volatility (category)
  "Calculate historical volatility for a category's trades"
  (let ((trades (gethash category *trade-history*)))
    (if (and trades (> (length trades) 5))
        (let* ((pnls (mapcar #'trade-record-pnl trades))
               (mean (/ (reduce #'+ pnls) (length pnls)))
               (sq-diffs (mapcar (lambda (p) (expt (- p mean) 2)) pnls)))
          (sqrt (/ (reduce #'+ sq-diffs) (length sq-diffs))))
        0.01)))

(defun calculate-risk-parity-lots ()
  "Calculate lot sizes for equal risk contribution"
  (let ((volatilities nil)
        (total-inv-vol 0.0)
        (lots (make-hash-table :test 'equal)))
    
    ;; Calculate each category's volatility
    (dolist (cat '(:trend :reversion :breakout :scalp))
      (let ((vol (max 0.001 (calculate-category-volatility cat))))
        (setf (gethash cat *category-volatilities*) vol)
        (push (cons cat vol) volatilities)
        (incf total-inv-vol (/ 1.0 vol))))
    
    ;; Allocate lots inversely proportional to volatility
    (dolist (cat-vol volatilities)
      (let* ((cat (car cat-vol))
             (vol (cdr cat-vol))
             (weight (/ (/ 1.0 vol) total-inv-vol))
             (lot (* *target-total-risk* weight 10)))
        (setf (gethash cat lots) (max 0.01 (min 0.1 lot)))))
    
    lots))

(defun get-risk-parity-lot (category)
  "Get risk-parity adjusted lot for a category"
  (let ((lots (calculate-risk-parity-lots)))
    (or (gethash category lots) 0.01)))

;;; ==========================================
;;; SELF-EXPLANATION SYSTEM
;;; ==========================================
;;; Inspired by: LLM Chain-of-Thought, XAI (Explainable AI)

(defparameter *trade-explanations* nil)
(defparameter *max-explanations* 50)

(defstruct trade-explanation
  timestamp
  symbol
  direction
  action          ; :execute, :skip, :reduce
  reasoning       ; List of reasons
  summary         ; One-line summary
  confidence)

(defun explain-trade-decision (symbol direction action factors)
  "Generate human-readable explanation for trade decision"
  (let* ((reasons nil)
         (summary ""))
    
    ;; Build reasoning from factors
    (dolist (f factors)
      (let ((name (first f))
            (value (second f)))
        (push (case name
                (:trend-aligned 
                 (if value "✓ Trend aligned with direction" "✗ Against trend"))
                (:volatility-ok
                 (if value "✓ Volatility acceptable" "✗ High volatility risk"))
                (:leader-agrees
                 (if value "✓ Leader confirms direction" "✗ Leader disagrees"))
                (:momentum-aligned
                 (if value "✓ Momentum supports entry" "✗ Weak momentum"))
                (:active-session
                 (if value "✓ Active trading session" "✗ Off-hours"))
                (otherwise (format nil "~a: ~a" name value)))
              reasons)))
    
    ;; Generate summary
    (setf summary
          (case action
            (:execute (format nil "~a ~a: Confidence high, executing" direction symbol))
            (:skip (format nil "SKIP ~a ~a: Conditions unfavorable" direction symbol))
            (:reduce (format nil "~a ~a: Reduced size due to mixed signals" direction symbol))))
    
    ;; Create explanation
    (let ((explanation (make-trade-explanation
                        :timestamp (get-universal-time)
                        :symbol symbol
                        :direction direction
                        :action action
                        :reasoning (nreverse reasons)
                        :summary summary
                        :confidence (or (getf factors :confidence) 0.5))))
      
      ;; Store and log
      (push explanation *trade-explanations*)
      (when (> (length *trade-explanations*) *max-explanations*)
        (setf *trade-explanations* (subseq *trade-explanations* 0 *max-explanations*)))
      
      ;; Output explanation
      (format t "[L] 📝 ~a~%" summary)
      (dolist (r reasons)
        (format t "[L]    ~a~%" r))
      
      explanation)))

(defun get-recent-trade-summary ()
  "Get summary of recent trades with explanations"
  (let ((recent (subseq *trade-explanations* 0 (min 5 (length *trade-explanations*)))))
    (format nil "Recent decisions:~%~{  - ~a~%~}"
            (mapcar #'trade-explanation-summary recent))))

(format t "[SCHOOL] school-research.lisp loaded - Research Implementations~%")
