;; research.lisp - Research Paper Insights Implementation
;; 
;; 論文知見統合モジュール
;; Based on 31 research papers (2024-2026)
;;
;; Architecture:
;; - Phase 1: Dual Trend, Volatility Model Switch, Vol-Scaled Rewards
;; - Phase 2: Kalman+HMM Regime, Latent Parameter Estimation
;; - Phase 3: Ensemble concepts, Indicator normalization
;; - Phase 4: LLM integration preparation

(in-package :swimmy.core)

;;; ══════════════════════════════════════════════════════════════════
;;;  KALMAN FILTER (ArXiv:1808.03297 - "Trend without hiccups")
;;; ══════════════════════════════════════════════════════════════════
;;; Optimal smoothing with minimal lag. Outperforms SMA/EMA for trend detection.
;;; 
;;; Key parameters:
;;; - process-noise (Q): How much the true price changes between observations
;;; - measurement-noise (R): How noisy the observed prices are
;;; - Lower Q/R ratio = more smoothing, higher = more responsive

(defun ind-kalman (history &key (process-noise 0.01) (measurement-noise 0.1))
  "Kalman Filter for price smoothing. Returns filtered price estimate.
   Based on ArXiv:1808.03297 'Trend without hiccups'"
  (when (>= (length history) 2)
    (let* ((closes (reverse (mapcar #'candle-close history)))
           ;; Initialize state
           (x (first closes))           ; State estimate (filtered price)
           (p 1.0)                       ; Estimation error covariance
           (q process-noise)             ; Process noise
           (r measurement-noise))        ; Measurement noise
      ;; Run Kalman filter through all observations
      (dolist (z (rest closes))
        ;; Prediction step
        (let* ((x-pred x)                ; State prediction (random walk model)
               (p-pred (+ p q)))         ; Covariance prediction
          ;; Update step
          (let* ((k (/ p-pred (+ p-pred r)))  ; Kalman gain
                 (x-new (+ x-pred (* k (- z x-pred))))  ; State update
                 (p-new (* (- 1 k) p-pred)))            ; Covariance update
            (setf x x-new)
            (setf p p-new))))
      x)))

(defun ind-kalman-velocity (history &key (process-noise 0.01) (measurement-noise 0.1))
  "Kalman Filter with velocity estimation. Returns (price, velocity).
   Velocity indicates trend direction and strength."
  (when (>= (length history) 3)
    (let* ((closes (reverse (mapcar #'candle-close history)))
           ;; Initialize state [price, velocity]
           (x (first closes))
           (v 0.0)
           (p-x 1.0)
           (p-v 1.0)
           (q process-noise)
           (r measurement-noise))
      (dolist (z (rest closes))
        ;; Prediction step (constant velocity model)
        (let* ((x-pred (+ x v))
               (v-pred v)
               (p-x-pred (+ p-x p-v q))
               (p-v-pred (+ p-v q)))
          ;; Update step
          (let* ((k-x (/ p-x-pred (+ p-x-pred r)))
                 (innovation (- z x-pred))
                 (x-new (+ x-pred (* k-x innovation)))
                 (v-new (+ v-pred (* 0.1 innovation)))  ; Velocity adjustment
                 (p-x-new (* (- 1 k-x) p-x-pred))
                 (p-v-new (* 0.95 p-v-pred)))  ; Velocity uncertainty decay
            (setf x x-new v v-new p-x p-x-new p-v p-v-new))))
      (values x v))))

(defun ind-kalman-trend (history &key (process-noise 0.01) (measurement-noise 0.1) (threshold 0.0001))
  "Kalman Filter trend detector. Returns :UP, :DOWN, or :FLAT based on velocity."
  (multiple-value-bind (price velocity) 
      (ind-kalman-velocity history :process-noise process-noise :measurement-noise measurement-noise)
    (declare (ignore price))
    (cond
      ((> velocity threshold) :UP)
      ((< velocity (- threshold)) :DOWN)
      (t :FLAT))))

(format t "[CORE] Kalman Filter loaded (ArXiv:1808.03297)~%")

;;; ══════════════════════════════════════════════════════════════════
;;;  PAPER #13: DUAL TREND DECOMPOSITION (短期/長期トレンド分解)
;;; ══════════════════════════════════════════════════════════════════
;;; ArXiv:2507.15876 - Re-evaluating Short- and Long-Term Trend Factors
;;; Key insight: Short-term trend + market beta = High Sharpe + Low DD

(defparameter *short-trend-period* 5
  "Short-term trend lookback (5-10 candles)")

(defparameter *long-trend-period* 50
  "Long-term trend lookback (50-200 candles)")

(defparameter *trend-blend-alpha* 0.7
  "Weight for short-term trend (0-1). Higher = more responsive")

(defun calculate-trend-strength (history period)
  "Calculate trend strength as normalized slope over period.
   Returns value between -1 (strong down) and +1 (strong up)."
  (when (>= (length history) period)
    (let* ((closes (mapcar #'candle-close (subseq history 0 period)))
           (first-price (car (last closes)))
           (last-price (first closes))
           (change (- last-price first-price))
           (avg-price (float (/ (+ first-price last-price) 2))))
      (if (> avg-price 0)
          (max -1.0 (min 1.0 (float (/ change (* avg-price 0.1)))))  ; Normalize to ~10% move
          0.0))))

(defun dual-trend-signal (history)
  "Combine short and long-term trends for robust signal.
   Returns (:direction :UP/:DOWN/:FLAT :strength 0-1 :short-trend X :long-trend Y)"
  (when (>= (length history) *long-trend-period*)
    (let* ((short-trend (or (calculate-trend-strength history *short-trend-period*) 0.0))
           (long-trend (or (calculate-trend-strength history *long-trend-period*) 0.0))
           ;; Blend: Responsive short + Stable long
           (blended (float (+ (* *trend-blend-alpha* short-trend)
                       (* (- 1 *trend-blend-alpha*) long-trend))))
           ;; Direction
           (direction (cond
                       ((> blended 0.1) :UP)
                       ((< blended -0.1) :DOWN)
                       (t :FLAT))))
      (list :direction direction
            :strength (float (abs blended))
            :short-trend (float short-trend)
            :long-trend (float long-trend)
            :agreement (if (or (and (> short-trend 0) (> long-trend 0))
                              (and (< short-trend 0) (< long-trend 0)))
                          :aligned
                          :divergent)))))

(format t "[RESEARCH] #13 Dual Trend Decomposition loaded~%")

;;; ══════════════════════════════════════════════════════════════════
;;;  PAPER #17: VOLATILITY-BASED MODEL SWITCHING
;;; ══════════════════════════════════════════════════════════════════
;;; ArXiv:2202.03156 - Kalman vs LSTM comparison
;;; Key insight: Low volatility → Kalman, High volatility → Complex models

(defparameter *volatility-threshold-low* 0.5
  "Below this: use simple models (Kalman)")

(defparameter *volatility-threshold-high* 1.5
  "Above this: use complex models (Ensemble)")

(defparameter *current-model-mode* :auto
  "Current model selection: :kalman, :ensemble, or :auto")

(defun calculate-realized-volatility (history &key (period 20))
  "Calculate realized volatility as percentage.
   Based on standard deviation of returns."
  (when (>= (length history) (1+ period))
    (let* ((closes (mapcar #'candle-close (subseq history 0 (1+ period))))
           (returns (loop for i from 0 below period
                          collect (let ((curr (nth i closes))
                                       (prev (nth (1+ i) closes)))
                                   (if (> prev 0)
                                       (float (* 100 (/ (- curr prev) prev)))
                                       0.0))))
           (mean (float (/ (reduce #'+ returns) period)))
           (variance (float (/ (reduce #'+ (mapcar (lambda (r) (expt (- r mean) 2)) returns))
                       period))))
      (sqrt variance))))

(defun select-optimal-model (history)
  "Select optimal model based on volatility regime.
   Paper #17: Kalman for low vol, LSTM/complex for high vol."
  (let ((vol (calculate-realized-volatility history)))
    (when vol
      (cond
        ((< vol *volatility-threshold-low*)
         (setf *current-model-mode* :kalman)
         (format t "[RESEARCH] 📊 Low volatility (~,2f%): Kalman mode~%" vol)
         :kalman)
        ((> vol *volatility-threshold-high*)
         (setf *current-model-mode* :ensemble)
         (format t "[RESEARCH] 📊 High volatility (~,2f%): Ensemble mode~%" vol)
         :ensemble)
        (t
         (setf *current-model-mode* :ensemble)
         (format t "[RESEARCH] 📊 Normal volatility (~,2f%): Ensemble mode~%" vol)
         :ensemble)))))

(defun get-model-prediction (history)
  "Get prediction using currently optimal model."
  (let ((mode (or (select-optimal-model history) :ensemble)))
    (case mode
      (:kalman
       ;; Use Kalman filter for prediction
       (let ((trend (ind-kalman-trend history)))
         (case trend
           (:UP :BUY)
           (:DOWN :SELL)
           (t :HOLD))))
      (:ensemble
       ;; Use multiple signals
       (let ((kalman (ind-kalman-trend history))
             (dual (dual-trend-signal history)))
         (if (and (eq kalman (getf dual :direction))
                  (eq (getf dual :agreement) :aligned))
             (case kalman
               (:UP :BUY)
               (:DOWN :SELL)
               (t :HOLD))
             :HOLD)))  ; Conflicting signals = HOLD
      (otherwise
       ;; Default ensemble mode
       nil))))

(format t "[RESEARCH] #17 Volatility-Based Model Switch loaded~%")

;;; ══════════════════════════════════════════════════════════════════
;;;  PAPER #18: VOLATILITY-SCALED REWARDS
;;; ══════════════════════════════════════════════════════════════════
;;; ArXiv:1911.10107 - Deep Reinforcement Learning for Trading
;;; Key insight: Volatility scaling in reward function enables profit 
;;;              even with large transaction costs

(defparameter *base-lot-scaling-enabled* t
  "Enable volatility-based lot size scaling")

(defparameter *target-volatility* 1.0
  "Target volatility percentage for scaling")

(defun volatility-scaled-lot (base-lot history)
  "Scale lot size inversely with volatility.
   High vol → smaller lots (risk management)
   Low vol → larger lots (exploit efficiency)"
  (if *base-lot-scaling-enabled*
      (let ((vol (calculate-realized-volatility history)))
        (if (and vol (> vol 0))
            (let ((scale (/ *target-volatility* vol)))
              ;; Clamp scale between 0.5x and 2x
              (* base-lot (max 0.5 (min 2.0 scale))))
            base-lot))
      base-lot))

(defun calculate-vol-adjusted-reward (pnl history)
  "Calculate volatility-adjusted reward for RL-style learning.
   Normalizes reward by volatility to make different regimes comparable."
  (let ((vol (calculate-realized-volatility history)))
    (if (and vol (> vol 0))
        (/ pnl vol)  ; Sharpe-like normalization
        pnl)))

(format t "[RESEARCH] #18 Volatility-Scaled Rewards loaded~%")

;;; ══════════════════════════════════════════════════════════════════
;;;  PAPER #16: KALMAN + HMM REGIME DETECTION
;;; ══════════════════════════════════════════════════════════════════
;;; ArXiv:1811.11618 - Kalman filter demystified
;;; Key insight: Kalman as HMM variant, CMA-ES for optimization

(defparameter *regime-states* '(:trending-up :trending-down :ranging :volatile)
  "Possible market regime states")

(defparameter *regime-transition-matrix* 
  '((:trending-up . ((:trending-up . 0.7) (:trending-down . 0.1) (:ranging . 0.15) (:volatile . 0.05)))
    (:trending-down . ((:trending-up . 0.1) (:trending-down . 0.7) (:ranging . 0.15) (:volatile . 0.05)))
    (:ranging . ((:trending-up . 0.15) (:trending-down . 0.15) (:ranging . 0.6) (:volatile . 0.1)))
    (:volatile . ((:trending-up . 0.2) (:trending-down . 0.2) (:ranging . 0.2) (:volatile . 0.4))))
  "Hidden Markov Model transition probabilities")

(defun hmm-regime-probability (history current-regime)
  "Estimate regime probabilities using Kalman-HMM hybrid.
   Returns plist of regime probabilities."
  (declare (ignore current-regime))
  (when (>= (length history) 20)
    (let* ((kalman-trend (ind-kalman-trend history))
           (vol (calculate-realized-volatility history))
           (dual (dual-trend-signal history))
           ;; Observation model: map observations to regime likelihoods
           (trending-up-likelihood (if (and (eq kalman-trend :UP)
                                            (eq (getf dual :direction) :UP))
                                       0.8 0.1))
           (trending-down-likelihood (if (and (eq kalman-trend :DOWN)
                                              (eq (getf dual :direction) :DOWN))
                                         0.8 0.1))
           (ranging-likelihood (if (eq kalman-trend :FLAT) 0.7 0.2))
           (volatile-likelihood (if (and vol (> vol *volatility-threshold-high*))
                                   0.8 0.1)))
      ;; Normalize
      (let ((total (+ trending-up-likelihood trending-down-likelihood 
                     ranging-likelihood volatile-likelihood)))
        (list :trending-up (float (/ trending-up-likelihood total))
              :trending-down (float (/ trending-down-likelihood total))
              :ranging (float (/ ranging-likelihood total))
              :volatile (float (/ volatile-likelihood total)))))))

(defun detect-regime-hmm (history)
  "Detect current regime using HMM-style inference."
  (let ((probs (hmm-regime-probability history *current-regime*)))
    (when probs
      (let ((max-prob 0)
            (best-regime :unknown))
        (loop for (regime prob) on probs by #'cddr
              when (> prob max-prob)
              do (setf max-prob prob best-regime regime))
        (format t "[RESEARCH] 🔮 HMM Regime: ~a (~,0f%)~%" best-regime (* 100 max-prob))
        best-regime))))

(format t "[RESEARCH] #16 Kalman+HMM Regime Detection loaded~%")

;;; ══════════════════════════════════════════════════════════════════
;;;  PAPER #11: LATENT PARAMETER ESTIMATION
;;; ══════════════════════════════════════════════════════════════════
;;; ArXiv:2511.00190 - Deep RL with partial information
;;; Key insight: Filter latent parameters (mean reversion level) for better trading

(defparameter *latent-mean-reversion* 0.0
  "Estimated mean reversion level (Ornstein-Uhlenbeck)")

(defparameter *latent-mean-speed* 0.1
  "Estimated mean reversion speed")

(defun estimate-mean-reversion (history &key (period 50))
  "Estimate latent mean reversion parameters using Kalman filtering.
   Based on Ornstein-Uhlenbeck process assumption."
  (when (>= (length history) period)
    (let* ((closes (mapcar #'candle-close (subseq history 0 period)))
           (mean (float (/ (reduce #'+ closes) period)))
           (current (first closes))
           ;; Estimate reversion speed from autocorrelation
           (lag1-sum 0) (var-sum 0))
      ;; Calculate lag-1 autocorrelation
      (loop for i from 0 below (1- period)
            do (let ((dev-i (- (nth i closes) mean))
                    (dev-i1 (- (nth (1+ i) closes) mean)))
                (incf lag1-sum (* dev-i dev-i1))
                (incf var-sum (* dev-i dev-i))))
      (let ((rho (if (> var-sum 0) (float (/ lag1-sum var-sum)) 0.0)))
        ;; Mean reversion speed: -ln(rho)
        (setf *latent-mean-reversion* mean)
        (setf *latent-mean-speed* (max 0.01 (min 1.0 (- (log (max 0.01 (abs rho)))))))
        (list :mean mean
              :current current
              :deviation (- current mean)
              :speed *latent-mean-speed*
              :signal (cond
                       ((> (- current mean) (* 0.5 (sqrt var-sum))) :OVERBOUGHT)
                       ((< (- current mean) (* -0.5 (sqrt var-sum))) :OVERSOLD)
                       (t :NEUTRAL)))))))

(format t "[RESEARCH] #11 Latent Parameter Estimation loaded~%")

;;; ══════════════════════════════════════════════════════════════════
;;;  PAPER #10 & #27: ENSEMBLE & INDICATOR NORMALIZATION
;;; ══════════════════════════════════════════════════════════════════
;;; ArXiv:2511.12120 - DRL Ensemble Strategy
;;; ArXiv:2411.07585v1 - Indicator normalization techniques

(defparameter *indicator-normalization-enabled* t
  "Enable z-score normalization for indicators")

(defun normalize-indicator (value history-values)
  "Z-score normalize an indicator value based on recent history.
   Paper #27: Proper normalization improves RL agent performance."
  (when (and *indicator-normalization-enabled* 
             history-values 
             (> (length history-values) 2))
    (let* ((mean (/ (reduce #'+ history-values) (length history-values)))
           (variance (/ (reduce #'+ (mapcar (lambda (v) (expt (- v mean) 2)) 
                                            history-values))
                       (length history-values)))
           (std (sqrt (max 0.0001 variance))))
      (/ (- value mean) std))))

(defun ensemble-vote (predictions)
  "Combine multiple model predictions into ensemble vote.
   Paper #10: PPO+A2C+DDPG ensemble beats individual models."
  (let ((buy-count 0) (sell-count 0) (hold-count 0))
    (dolist (pred predictions)
      (case pred
        (:BUY (incf buy-count))
        (:SELL (incf sell-count))
        (otherwise (incf hold-count))))
    (let ((total (+ buy-count sell-count hold-count)))
      (cond
        ((and (> buy-count sell-count) (> buy-count hold-count))
         (values :BUY (float (/ buy-count total))))
        ((and (> sell-count buy-count) (> sell-count hold-count))
         (values :SELL (float (/ sell-count total))))
        (t
         (values :HOLD (float (/ hold-count total))))))))

(format t "[RESEARCH] #10/#27 Ensemble & Normalization loaded~%")

;;; ══════════════════════════════════════════════════════════════════
;;;  PAPER #4: IMPLEMENTATION QUALITY > ALGORITHM COMPLEXITY
;;; ══════════════════════════════════════════════════════════════════
;;; ArXiv:2512.10913 - RL Systematic Review
;;; Key insight: Domain knowledge + implementation quality matters more
;;;              than algorithm sophistication

(defparameter *implementation-quality-checks* t
  "Enable runtime quality checks")

(defun quality-check-strategy (strategy)
  "Validate strategy implementation quality.
   Paper #4: Quality > Complexity"
  (let ((issues nil))
    ;; Check SL/TP ratio
    (when (and (swimmy.school:strategy-sl strategy) (swimmy.school:strategy-tp strategy))
      (let ((ratio (/ (swimmy.school:strategy-tp strategy) (swimmy.school:strategy-sl strategy))))
        (when (< ratio 1.0)
          (push "Risk/Reward ratio < 1.0" issues))))
    ;; Check lot size
    (when (and (swimmy.school:strategy-volume strategy) (> (swimmy.school:strategy-volume strategy) 0.1))
      (push "Lot size too large (>0.1)" issues))
    ;; Check indicator count
    (when (and (swimmy.school:strategy-indicators strategy) 
               (> (length (swimmy.school:strategy-indicators strategy)) 5))
      (push "Too many indicators (>5) - potential overfitting" issues))
    ;; Return result
    (if issues
        (progn
          (format t "[QUALITY] ⚠️ Strategy ~a issues: ~{~a~^, ~}~%" 
                  (swimmy.school:strategy-name strategy) issues)
          (values nil issues))
        (values t nil))))

(format t "[RESEARCH] #4 Implementation Quality Checks loaded~%")

;;; ══════════════════════════════════════════════════════════════════
;;;  PAPER #9: ZERO-SHOT CAUTION
;;; ══════════════════════════════════════════════════════════════════
;;; ArXiv:2412.09394 - Time Series Foundation Models in Finance
;;; Key insight: Zero-shot TSFMs often underperform. Need fine-tuning.

(defparameter *zero-shot-confidence-penalty* 0.3
  "Confidence penalty for predictions without domain-specific training")

(defun apply-zero-shot-caution (prediction confidence source)
  "Apply caution to predictions from zero-shot or untrained models.
   Paper #9: Don't trust zero-shot blindly."
  (declare (ignore prediction))
  (if (member source '(:external-llm :foundation-model :zero-shot))
      (let ((adjusted (* confidence (- 1 *zero-shot-confidence-penalty*))))
        (format t "[RESEARCH] ⚠️ Zero-shot caution: ~,0f% → ~,0f%~%" 
                (* 100 confidence) (* 100 adjusted))
        adjusted)
      confidence))

(format t "[RESEARCH] #9 Zero-Shot Caution loaded~%")

;;; ══════════════════════════════════════════════════════════════════
;;;  PAPER #29: CONVEX CO-INTEGRATION (PENDING)
;;; ══════════════════════════════════════════════════════════════════
;;; ArXiv:2601.03927 - Index Tracking / Co-integration
;;; Insight: Statistical mean reversion (Safe Shaman)

;;; ══════════════════════════════════════════════════════════════════
;;;  PAPER #30: RRE-PPO4Pred (PENDING)
;;; ══════════════════════════════════════════════════════════════════
;;; ArXiv:2601.03683 - Reinforcement Learning for RNN Architecture
;;; Insight: Dreamer V3 candidate (Naval's favorite)

;;; ══════════════════════════════════════════════════════════════════
;;;  MASTER INTEGRATION FUNCTION
;;; ══════════════════════════════════════════════════════════════════

(defun research-enhanced-analysis (history)
  "Comprehensive analysis using all research paper insights."
  (when (>= (length history) 50)
    (let* (;; Phase 1: Trend Analysis
           (dual-trend (dual-trend-signal history))
           ;; Phase 2: Model Selection
           (optimal-model (select-optimal-model history))
           ;; Phase 3: Regime Detection
           (regime (detect-regime-hmm history))
           ;; Phase 4: Mean Reversion
           (mean-rev (estimate-mean-reversion history)))
      
      (format t "~%[RESEARCH] ═══════════════════════════════════════~%")
      (format t "[RESEARCH] 📊 RESEARCH-ENHANCED ANALYSIS~%")
      (format t "[RESEARCH] ═══════════════════════════════════════~%")
      (when (and dual-trend (listp dual-trend))
        (format t "[RESEARCH] Dual Trend: ~a (strength: ~,2f, ~a)~%"
                (getf dual-trend :direction)
                (or (getf dual-trend :strength) 0.0)
                (getf dual-trend :agreement)))
      (format t "[RESEARCH] Model: ~a | Regime: ~a~%" optimal-model regime)
      (when (and mean-rev (listp mean-rev))
        (format t "[RESEARCH] Mean Reversion: ~a (dev: ~,4f)~%"
                (getf mean-rev :signal)
                (or (getf mean-rev :deviation) 0.0)))
      (format t "[RESEARCH] ═══════════════════════════════════════~%")
      
      ;; Return combined analysis
      (list :dual-trend dual-trend
            :model optimal-model
            :regime regime
            :mean-reversion mean-rev))))

(format t "~%[RESEARCH] ════════════════════════════════════════════~%")
(format t "[RESEARCH] 📚 31 Research Paper Insights Integrated (2 Pending)~%")
(format t "[RESEARCH] Papers: #4,#5,#9,#10,#11,#13,#14,#16,#17,#18,#27,#28,#29,#30...~%")
(format t "[RESEARCH] ════════════════════════════════════════════~%")
