;; mixseek.lisp - MixSeek-Style Competitive Multi-Agent Strategy Generation

(in-package :swimmy.school)
;; Based on: https://note.com/drillan/n/n7379c02632c9
;; 
;; "人の褌で相撲を取る" - Standing on the shoulders of giants
;;
;; Architecture:
;; - 4 Clans (Teams) compete to generate the best strategies
;; - Elder Council (Evaluator) scores each strategy
;; - Rounds of improvement with feedback
;; - Comomentum indicator to detect market crowding

(in-package :common-lisp-user)

;;; ══════════════════════════════════════════════════════════════════
;;;  COMOMENTUM INDICATOR (混雑度)
;;; ══════════════════════════════════════════════════════════════════
;;; Measures how "crowded" a trade is by detecting correlation clustering
;;; High Comomentum = Many traders doing the same thing = Danger

(defparameter *comomentum-threshold-warning* 0.15
  "Comomentum level that triggers caution")

(defparameter *comomentum-threshold-danger* 0.20
  "Comomentum level that triggers exposure reduction")

(defun calculate-comomentum (history &key (lookback 20))
  "Calculate Comomentum (co-movement momentum) as proxy for market crowding.
   Returns value between 0 (uncrowded) and 1 (extremely crowded).
   Based on correlation of price changes across time windows."
  (when (>= (length history) (* lookback 2))
    (let* ((closes (mapcar #'candle-close (subseq history 0 (* lookback 2))))
           (returns (loop for i from 0 below (1- (length closes))
                          collect (/ (- (nth i closes) (nth (1+ i) closes))
                                    (max 0.0001 (nth (1+ i) closes)))))
           ;; Split into two windows
           (recent (subseq returns 0 lookback))
           (prior (subseq returns lookback))
           ;; Calculate correlation
           (mean-r (/ (reduce #'+ recent) lookback))
           (mean-p (/ (reduce #'+ prior) lookback))
           (cov 0.0) (var-r 0.0) (var-p 0.0))
      (loop for r in recent
            for p in prior
            do (incf cov (* (- r mean-r) (- p mean-p)))
               (incf var-r (expt (- r mean-r) 2))
               (incf var-p (expt (- p mean-p) 2)))
      ;; Normalize correlation to [0, 1]
      (let ((denom (sqrt (* var-r var-p))))
        (if (< denom 0.0001)
            0.0
            (abs (/ cov denom)))))))

(defun comomentum-risk-level (comomentum)
  "Categorize Comomentum into risk levels"
  (cond
    ((null comomentum) :unknown)
    ((< comomentum *comomentum-threshold-warning*) :safe)
    ((< comomentum *comomentum-threshold-danger*) :caution)
    (t :danger)))

(defun comomentum-exposure-multiplier (comomentum)
  "Return position size multiplier based on crowding.
   Reduces exposure when market is crowded."
  (cond
    ((null comomentum) 1.0)
    ((< comomentum *comomentum-threshold-warning*) 1.0)  ; Full size
    ((< comomentum *comomentum-threshold-danger*) 0.7)   ; 30% reduction
    (t 0.5)))  ; 50% reduction in danger zone

;;; ══════════════════════════════════════════════════════════════════
;;;  TEAM COMPETITION FRAMEWORK
;;; ══════════════════════════════════════════════════════════════════
;;; Each clan generates strategies, Elder Council evaluates

(defstruct team-submission
  clan-id           ; Which clan submitted
  strategy          ; The strategy struct
  round             ; Which round of competition
  reasoning         ; Why this strategy fits current conditions
  confidence)       ; Self-assessed confidence (0-1)

(defstruct evaluation-result
  submission        ; The team-submission
  scores            ; Plist of scores by category
  total-score       ; Final score (0-100)
  feedback          ; Text feedback from evaluator
  rank)             ; Rank among submissions this round

(defparameter *competition-history* nil
  "History of all rounds of competition")

(defparameter *current-round* 0
  "Current competition round")

;;; ─────────────────────────────────────────
;;; EVALUATION CRITERIA (from MixSeek)
;;; ─────────────────────────────────────────
;;; 1. 実行可能性 (Executability) - 30 points
;;; 2. リスク調整後リターン (Risk-adjusted Return) - 30 points  
;;; 3. 頑健性 (Robustness) - 20 points
;;; 4. 明確さ (Clarity) - 20 points

(defun evaluate-executability (strategy)
  "Score executability: execution cost, liquidity, implementation difficulty.
   Max 30 points."
  (let ((score 30))
    ;; Penalize extreme SL (hard to execute)
    (when (< (strategy-sl strategy) 0.1)
      (decf score 10))
    ;; Penalize extreme volume
    (when (> (strategy-volume strategy) 0.1)
      (decf score 5))
    ;; Bonus for simple indicators
    (when (<= (length (strategy-indicators strategy)) 3)
      (incf score 5))
    (min 30 (max 0 score))))

(defun evaluate-risk-return (strategy history)
  "Score risk-adjusted return potential.
   Max 30 points."
  (let ((score 15)
        (sharpe (strategy-sharpe strategy)))
    ;; Sharpe ratio bonus
    (cond
      ((> sharpe 2.0) (incf score 15))
      ((> sharpe 1.5) (incf score 10))
      ((> sharpe 1.0) (incf score 5))
      ((> sharpe 0.5) (incf score 2)))
    ;; Risk/reward ratio
    (let ((rr (/ (strategy-tp strategy) (max 0.01 (strategy-sl strategy)))))
      (when (> rr 2.0) (incf score 5))
      (when (< rr 1.0) (decf score 10)))
    (min 30 (max 0 score))))

(defun evaluate-robustness (strategy comomentum)
  "Score robustness: overfit resistance, regime adaptability.
   Max 20 points."
  (let ((score 10))
    ;; Comomentum awareness bonus
    (when (and comomentum (< comomentum *comomentum-threshold-warning*))
      (incf score 5))
    ;; Indicator diversity bonus
    (let* ((inds (strategy-indicators strategy))
           (types (remove-duplicates (mapcar #'car inds))))
      (when (> (length types) 1) (incf score 5)))
    (min 20 (max 0 score))))

(defun evaluate-clarity (strategy)
  "Score clarity: logic clarity, reproducibility.
   Max 20 points."
  (let ((score 10))
    ;; Named strategy bonus
    (when (strategy-name strategy) (incf score 5))
    ;; Has entry/exit logic
    (when (and (strategy-entry strategy) (strategy-exit strategy))
      (incf score 5))
    (min 20 (max 0 score))))

(defun elder-evaluate (submission history)
  "Elder Council evaluates a team submission.
   Returns evaluation-result with scores and feedback."
  (let* ((strategy (team-submission-strategy submission))
         (comomentum (calculate-comomentum history))
         (exec-score (evaluate-executability strategy))
         (risk-score (evaluate-risk-return strategy history))
         (robust-score (evaluate-robustness strategy comomentum))
         (clarity-score (evaluate-clarity strategy))
         (total (+ exec-score risk-score robust-score clarity-score))
         (feedback (elder-generate-feedback total (team-submission-clan-id submission))))
    (make-evaluation-result
     :submission submission
     :scores (list :executability exec-score
                   :risk-return risk-score
                   :robustness robust-score
                   :clarity clarity-score)
     :total-score total
     :feedback feedback
     :rank 0)))  ; Rank set later

(defun elder-generate-feedback (score clan-id)
  "Generate Elder feedback based on score and clan."
  (let ((clan-name (let ((c (get-clan clan-id))) (if c (clan-name c) "Unknown"))))
    (cond
      ((>= score 90)
       (format nil "👴 Elders praise ~a: 「見事な戦略だ。部族の誇りとなるであろう。」" clan-name))
      ((>= score 70)
       (format nil "👴 Elders approve ~a: 「良い戦略だ。さらに磨けばより強くなる。」" clan-name))
      ((>= score 50)
       (format nil "👴 Elders note ~a: 「まだ荒削りだ。経験を積め。」" clan-name))
      (t
       (format nil "👴 Elders warn ~a: 「この戦略は危険だ。再考せよ。」" clan-name)))))

;;; ─────────────────────────────────────────
;;; TEAM STRATEGY GENERATION
;;; ─────────────────────────────────────────

(defun generate-team-strategy (clan-id context)
  "Each clan generates a strategy based on their philosophy and current context."
  (let* ((clan (get-clan clan-id))
         (regime (getf context :regime))
         (comomentum (getf context :comomentum))
         (volatility (getf context :volatility)))
    (case clan-id
      ;; Hunters (Trend) - Patient, follow the wind
      (:trend
       (make-strategy
        :name (format nil "Hunter-Wind-~d" (random 1000))
        :indicators '((kalman) (sma 50) (atr 14))
        :entry '(and (eq (ind-kalman-trend history) :UP)
                     (> (ind-sma 20 history) (ind-sma 50 history)))
        :exit '(eq (ind-kalman-trend history) :DOWN)
        :sl (if (eq volatility :high) 0.8 0.5)
        :tp (if (eq volatility :high) 1.6 1.0)
        :volume (* 0.01 (comomentum-exposure-multiplier comomentum))))
      
      ;; Shamans (Reversion) - Counter the crowd
      (:reversion
       (make-strategy
        :name (format nil "Shaman-Reversion-~d" (random 1000))
        :indicators '((rsi 14) (bb 20 2))
        :entry '(and (< (ind-rsi 14 history) 30)
                     (< close bb-lower))
        :exit '(or (> (ind-rsi 14 history) 70)
                   (> close bb-upper))
        :sl 0.4
        :tp 0.8
        :volume (* 0.01 (comomentum-exposure-multiplier comomentum))))
      
      ;; Breakers (Breakout) - Explosive power
      (:breakout
       (make-strategy
        :name (format nil "Breaker-Storm-~d" (random 1000))
        :indicators '((atr 14) (bb 20 2))
        :entry '(and (> (ind-atr 14 history) (* 1.5 (ind-atr 50 history)))
                     (> close bb-upper))
        :exit '(< (ind-atr 14 history) (ind-atr 50 history))
        :sl 0.6
        :tp 1.5
        :volume (* 0.01 (comomentum-exposure-multiplier comomentum))))
      
      ;; Raiders (Scalp) - Quick and nimble
      (:scalp
       (make-strategy
        :name (format nil "Raider-Strike-~d" (random 1000))
        :indicators '((ema 5) (ema 13) (stoch 5 3))
        :entry '(and (cross-above ema-5 ema-13)
                     (< (ind-stoch 5 3 history) 20))
        :exit '(or (cross-below ema-5 ema-13)
                   (> (ind-stoch 5 3 history) 80))
        :sl 0.2
        :tp 0.3
        :volume (* 0.02 (comomentum-exposure-multiplier comomentum))))
      
      (otherwise nil))))

;;; ─────────────────────────────────────────
;;; COMPETITION ROUNDS
;;; ─────────────────────────────────────────

(defun run-competition-round (history)
  "Run one round of competition among all clans.
   Returns ranked list of evaluation results."
  (incf *current-round*)
  (format t "~%[MIXSEEK] ═══════════════════════════════════════~%")
  (format t "[MIXSEEK] 🏆 COMPETITION ROUND ~d~%" *current-round*)
  (format t "[MIXSEEK] ═══════════════════════════════════════~%")
  
  (let* ((comomentum (calculate-comomentum history))
         (context (list :regime *current-regime*
                        :comomentum comomentum
                        :volatility *volatility-regime*))
         (submissions nil)
         (results nil))
    
    ;; Report market crowding
    (format t "[MIXSEEK] 📊 Comomentum (混雑度): ~,3f [~a]~%"
            (or comomentum 0.0)
            (comomentum-risk-level comomentum))
    
    ;; Each clan generates a submission
    (dolist (clan-id '(:trend :reversion :breakout :scalp))
      (let* ((clan (get-clan clan-id))
             (strategy (generate-team-strategy clan-id context)))
        (when strategy
          (let ((submission (make-team-submission
                            :clan-id clan-id
                            :strategy strategy
                            :round *current-round*
                            :confidence (+ 0.5 (random 0.5)))))
            (format t "[MIXSEEK] ~a ~a submitted: ~a~%"
                    (clan-emoji clan) (clan-name clan) (strategy-name strategy))
            (push submission submissions)))))
    
    ;; Elder Council evaluates
    (format t "~%[MIXSEEK] 👴 Elder Council convenes...~%")
    (dolist (sub submissions)
      (let ((result (elder-evaluate sub history)))
        (push result results)))
    
    ;; Rank by score
    (setf results (sort results #'> :key #'evaluation-result-total-score))
    (loop for result in results
          for rank from 1
          do (setf (evaluation-result-rank result) rank))
    
    ;; Announce results
    (format t "~%[MIXSEEK] 🏆 RESULTS:~%")
    (dolist (result results)
      (let* ((sub (evaluation-result-submission result))
             (clan (get-clan (team-submission-clan-id sub)))
             (medal (case (evaluation-result-rank result)
                     (1 "🥇") (2 "🥈") (3 "🥉") (otherwise "  "))))
        (format t "~a ~a ~a: ~d points~%"
                medal
                (clan-emoji clan)
                (strategy-name (team-submission-strategy sub))
                (evaluation-result-total-score result))
        (format t "   ~a~%" (evaluation-result-feedback result))))
    
    ;; Store history
    (push (list :round *current-round*
                :results results
                :comomentum comomentum)
          *competition-history*)
    
    ;; Return winner
    (first results)))

(defun get-best-strategy-from-competition ()
  "Get the best strategy from the latest competition round."
  (when *competition-history*
    (let* ((latest (first *competition-history*))
           (results (getf latest :results)))
      (when results
        (team-submission-strategy 
         (evaluation-result-submission (first results)))))))

(format t "[MIXSEEK] Competition System loaded~%")
(format t "[MIXSEEK] 4 Clans ready to compete: Hunters, Shamans, Breakers, Raiders~%")
