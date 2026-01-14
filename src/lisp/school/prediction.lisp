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

;;; V17: Configurable weights (Issue 1 fix - no more hardcoded values)
(defvar *prediction-weight-trend* 3.0       "Weight for trend alignment factor")
(defvar *prediction-weight-volatility* 2.0  "Weight for volatility factor")
(defvar *prediction-weight-leader* 2.0      "Weight for leader agreement factor")
(defvar *prediction-weight-momentum* 1.5    "Weight for momentum factor")
(defvar *prediction-weight-session* 1.0     "Weight for session factor")
(defvar *prediction-min-confidence* 0.50    "Minimum confidence to approve trade")
(defvar *prediction-block-threshold* 0.30   "Block LOSS predictions below this")

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

;;; V18: Factor Calculation Functions (Fowler: extract to separate functions)
;;; Each returns (values valid-p weight-contribution factor-info)

(defun calculate-trend-factor (history direction)
  "Factor 1: Trend alignment - V18 extracted function"
  (when (> (length history) 50)
    (let* ((sma20 (/ (reduce #'+ (mapcar #'candle-close (subseq history 0 20))) 20))
           (sma50 (/ (reduce #'+ (mapcar #'candle-close (subseq history 0 50))) 50))
           (price (candle-close (first history)))
           (trend-up (and (> price sma20) (> sma20 sma50)))
           (trend-down (and (< price sma20) (< sma20 sma50)))
           (range-market (not (or trend-up trend-down)))
           (aligned (or (and trend-up (eq direction :buy))
                        (and trend-down (eq direction :sell))
                        range-market)))
      (values aligned *prediction-weight-trend*
              (list :trend-aligned aligned :range-market range-market 
                    :trend-up trend-up :trend-down trend-down)))))

(defun calculate-volatility-factor ()
  "Factor 2: Volatility state"
  (let ((vol-ok (not (eq *current-volatility-state* :extreme))))
    (values vol-ok *prediction-weight-volatility*
            (list :volatility-ok vol-ok))))

(defun calculate-leader-factor (history direction)
  "Factor 3: Leader agreement"
  (when (and (boundp '*current-leader*) *current-leader* (fboundp 'get-leader-direction))
    (let* ((leader-dir (get-leader-direction history))
           (agrees (eq leader-dir direction)))
      (values agrees *prediction-weight-leader*
              (list :leader-agrees agrees)))))

(defun calculate-momentum-factor (history direction trend-info)
  "Factor 4: Momentum - V18: Improved reversion with RSI (Graham)"
  (when (fboundp 'count-consecutive-candles)
    (let* ((up-count (count-consecutive-candles history :up))
           (down-count (count-consecutive-candles history :down))
           ;; V18 Graham: Use RSI for better reversion detection
           (rsi (when (fboundp 'ind-rsi) (ind-rsi 14 history)))
           (oversold (and rsi (< rsi 30)))
           (overbought (and rsi (> rsi 70)))
           ;; Trend-following
           (trend-momentum (or (and (> up-count 2) (eq direction :buy))
                               (and (> down-count 2) (eq direction :sell))))
           ;; V18: Enhanced reversion with RSI confirmation (Graham)
           (reversion-momentum (or (and (> down-count 2) (eq direction :buy) (or oversold (not rsi)))
                                   (and (> up-count 2) (eq direction :sell) (or overbought (not rsi)))))
           (momentum-valid (or trend-momentum reversion-momentum))
           ;; V18 López de Prado: Dynamic correlation factor
           (trend-aligned-p (getf trend-info :trend-aligned))
           (correlation (calculate-factor-correlation trend-aligned-p trend-momentum reversion-momentum))
           (adjusted-weight (* *prediction-weight-momentum* (- 1.0 (* 0.5 correlation)))))
      (values momentum-valid adjusted-weight
              (list :momentum-valid momentum-valid
                    :trend-momentum trend-momentum
                    :reversion-momentum reversion-momentum
                    :rsi rsi :oversold oversold :overbought overbought
                    :correlation correlation)))))

(defun calculate-factor-correlation (trend-aligned trend-momentum reversion-momentum)
  "V18 López de Prado: Calculate correlation between factors (0.0-1.0)"
  ;; When trend and momentum agree, they're correlated
  ;; When reversion is active, they're anti-correlated (0 correlation)
  (cond
    ((and trend-aligned trend-momentum (not reversion-momentum)) 0.8)  ; High correlation
    ((and (not trend-aligned) (not trend-momentum)) 0.3)  ; Both negative = some correlation
    (reversion-momentum 0.0)  ; Reversion = independent signal
    (t 0.2)))  ; Default low correlation

(defun calculate-session-factor ()
  "Factor 5: Session favorability"
  (when (fboundp 'get-current-session)
    (let* ((session (get-current-session))
           (active-session (member session '(:tokyo :london :newyork :overlap))))
      (values (not (null active-session)) *prediction-weight-session*
              (list :active-session (not (null active-session)) :session session)))))

;;; V18: Simplified main prediction function (Hickey: reduce complexity)
(defun predict-trade-outcome (symbol direction)
  "Predict whether a trade will be profitable - V18: Refactored"
  (let* ((history (or (gethash symbol *candle-histories*) *candle-history*))
         (factors nil)
         (win-score 0.0)
         (total-weight 0.0))
    
    (when (and history (> (length history) 50))
      ;; Factor 1: Trend
      (multiple-value-bind (valid weight info) (calculate-trend-factor history direction)
        (incf total-weight weight)
        (when valid (incf win-score weight))
        (push info factors))
      
      ;; Factor 2: Volatility
      (multiple-value-bind (valid weight info) (calculate-volatility-factor)
        (incf total-weight weight)
        (when valid (incf win-score weight))
        (push info factors))
      
      ;; Factor 3: Leader (optional)
      (multiple-value-bind (valid weight info) (calculate-leader-factor history direction)
        (when weight
          (incf total-weight weight)
          (when valid (incf win-score weight))
          (push info factors)))
      
      ;; Factor 4: Momentum (with correlation adjustment)
      (let ((trend-info (first factors)))  ; Get trend info for correlation
        (multiple-value-bind (valid weight info) (calculate-momentum-factor history direction trend-info)
          (when weight
            (incf total-weight weight)
            (when valid (incf win-score weight))
            (push info factors))))
      
      ;; Factor 5: Session (optional)
      (multiple-value-bind (valid weight info) (calculate-session-factor)
        (when weight
          (incf total-weight weight)
          (when valid (incf win-score weight))
          (push info factors))))
    
    ;; Calculate and return prediction
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
      
      (format t "[L] 🔮 PREDICTION: ~a ~a → ~a (~,0f% confidence)~%"
              direction symbol predicted-outcome (* 100 confidence))
      
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

;;; V17: Feedback Loop Implementation (Issue 2 Fix)
(defun find-prediction-by-symbol-direction (symbol direction &optional (max-age-seconds 3600))
  "Find the most recent prediction for symbol/direction within max-age"
  (let ((now (get-universal-time))
        (cutoff (- (get-universal-time) max-age-seconds)))
    (find-if (lambda (p)
               (and (equal (trade-prediction-symbol p) symbol)
                    (eq (trade-prediction-direction p) direction)
                    (> (trade-prediction-timestamp p) cutoff)
                    (null (trade-prediction-actual-outcome p))))  ; Not yet recorded
             *prediction-history*)))

(defun record-prediction-outcome (symbol direction outcome)
  "Record actual trade outcome (:win or :loss) for feedback learning"
  (let ((prediction (find-prediction-by-symbol-direction symbol direction)))
    (when prediction
      (setf (trade-prediction-actual-outcome prediction) outcome)
      (update-prediction-accuracy)
      (format t "[PREDICT] 📊 Feedback recorded: ~a ~a → ~a (predicted: ~a, ~a)~%"
              direction symbol outcome
              (trade-prediction-predicted-outcome prediction)
              (if (eq outcome (trade-prediction-predicted-outcome prediction))
                  "✅ CORRECT" "❌ WRONG"))
      t)))

(defun update-prediction-accuracy ()
  "V18: Update accuracy and adjust weights based on performance (Naval/Ng learning cycle)"
  (let* ((validated (remove-if-not #'trade-prediction-actual-outcome *prediction-history*))
         (count (length validated))
         (accuracy (get-predicted-win-rate)))
    (setf *prediction-accuracy* accuracy)
    
    ;; V18: Adaptive weight adjustment based on accuracy (Naval/Ng)
    (when (>= count 20)  ; Need minimum sample size
      (cond
        ;; Accuracy too low - tighten threshold (be more conservative)
        ((< accuracy 0.45)
         (let ((new-threshold (min 0.65 (+ *prediction-min-confidence* 0.05))))
           (format t "[LEARN] 📉 Accuracy ~,1f% too low - tightening threshold: ~,0f% → ~,0f%~%"
                   (* 100 accuracy) (* 100 *prediction-min-confidence*) (* 100 new-threshold))
           (setf *prediction-min-confidence* new-threshold)))
        ;; Accuracy high - relax threshold (capture more opportunities)
        ((> accuracy 0.70)
         (let ((new-threshold (max 0.40 (- *prediction-min-confidence* 0.03))))
           (format t "[LEARN] 📈 Accuracy ~,1f% strong - relaxing threshold: ~,0f% → ~,0f%~%"
                   (* 100 accuracy) (* 100 *prediction-min-confidence*) (* 100 new-threshold))
           (setf *prediction-min-confidence* new-threshold)))
        ;; Accuracy acceptable - no change
        (t nil)))
    
    ;; Log accuracy for monitoring
    (when (> count 10)
      (format t "[PREDICT] 📈 Prediction accuracy: ~,1f% (~d validated, threshold: ~,0f%)~%"
              (* 100 accuracy) count (* 100 *prediction-min-confidence*)))))

(defun should-take-trade-p (prediction)
  "Decide whether to take trade based on prediction - V17: Configurable thresholds"
  (let ((conf (trade-prediction-confidence prediction))
        (outcome (trade-prediction-predicted-outcome prediction)))
    ;; V17: Use configurable block threshold (default 0.30)
    (when (and (eq outcome :loss) (< conf *prediction-block-threshold*))
      (format t "[L] 🚫 HIGH-RISK LOSS: Trade blocked (conf ~,0f% < ~,0f%)~%" 
              (* 100 conf) (* 100 *prediction-block-threshold*))
      (return-from should-take-trade-p nil))
    ;; V17: Use configurable minimum confidence (default 0.50)
    (cond
      ((< conf *prediction-min-confidence*)
       (format t "[L] ⚠️ LOW CONFIDENCE (~,0f% < ~,0f%): Skipping trade~%" 
               (* 100 conf) (* 100 *prediction-min-confidence*))
       nil)
      (t 
       (format t "[L] ✅ APPROVED: ~a prediction (~,0f% confidence)~%" outcome (* 100 conf))
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

;;; V17: SELF-EXPLANATION system removed (Issue 4 - was unused dead code)
;;; If needed in future, implement a simpler version that actually gets called

(format t "[SCHOOL] school-research.lisp loaded - Research Implementations~%")
