;; llm-integration.lisp - LLM Integration Preparation
;;
;; 論文知見: Phase 4 LLM統合準備
;; Based on Papers: #5 Trading-R1, #14 PrimoGPT, #28 FinLlama
;;
;; Note: These are preparation structures. Actual LLM calls use existing
;; Gemini API infrastructure in brain.lisp

(in-package :common-lisp-user)

;;; ══════════════════════════════════════════════════════════════════
;;;  PAPER #5: TRADING-R1 REASONING STRUCTURE
;;; ══════════════════════════════════════════════════════════════════
;;; ArXiv:2509.11420 - Trading with LLM Reasoning via RL
;;; Key insight: Structured reasoning → Investment thesis → Better decisions

(defparameter *reasoning-enabled* t
  "Enable structured reasoning for trade decisions")

(defstruct investment-thesis
  timestamp
  symbol
  direction           ; :BUY, :SELL, :HOLD
  confidence          ; 0.0-1.0
  thesis-summary      ; One-line summary
  supporting-factors  ; List of supporting evidence
  risk-factors        ; List of risks
  entry-conditions    ; What must be true to enter
  exit-conditions     ; What triggers exit
  time-horizon)       ; Expected holding period

(defun generate-investment-thesis (symbol direction analysis)
  "Generate structured investment thesis (Paper #5 style).
   This creates the reasoning structure that LLM would fill."
  (make-investment-thesis
   :timestamp (get-universal-time)
   :symbol symbol
   :direction direction
   :confidence (or (getf analysis :strength) 0.5)
   :thesis-summary (format nil "~a ~a based on ~a regime with ~a trend"
                          direction symbol
                          (or (getf analysis :regime) "unknown")
                          (getf (getf analysis :dual-trend) :agreement))
   :supporting-factors 
   (let ((factors nil))
     (when (eq (getf (getf analysis :dual-trend) :agreement) :aligned)
       (push "Dual trend aligned" factors))
     (when (and (getf analysis :mean-reversion)
                (member (getf (getf analysis :mean-reversion) :signal) '(:OVERSOLD :OVERBOUGHT)))
       (push (format nil "Mean reversion signal: ~a" 
                    (getf (getf analysis :mean-reversion) :signal)) factors))
     factors)
   :risk-factors
   (let ((risks nil))
     (when (eq (getf analysis :regime) :volatile)
       (push "High volatility regime" risks))
     (when (eq (getf (getf analysis :dual-trend) :agreement) :divergent)
       (push "Short/Long trend divergence" risks))
     risks)
   :entry-conditions (list "Swarm consensus > 60%"
                           "Constitution approval"
                           "Within daily loss limit")
   :exit-conditions (list "TP hit" "SL hit" "Trend reversal" "Time-based exit")
   :time-horizon :intraday))

(defun thesis-to-prompt (thesis)
  "Convert investment thesis to LLM prompt format.
   For future Gemini API integration."
  (format nil "
INVESTMENT THESIS REVIEW

Symbol: ~a
Proposed Action: ~a
Confidence: ~,0f%

THESIS: ~a

SUPPORTING FACTORS:
~{- ~a~%~}

RISK FACTORS:
~{- ~a~%~}

ENTRY CONDITIONS:
~{- ~a~%~}

EXIT CONDITIONS:
~{- ~a~%~}

TIME HORIZON: ~a

Please evaluate this thesis and provide:
1. Approval (YES/NO)
2. Confidence adjustment (-20% to +20%)
3. Additional risks to consider
4. Recommended modifications
"
          (investment-thesis-symbol thesis)
          (investment-thesis-direction thesis)
          (* 100 (investment-thesis-confidence thesis))
          (investment-thesis-thesis-summary thesis)
          (or (investment-thesis-supporting-factors thesis) '("None identified"))
          (or (investment-thesis-risk-factors thesis) '("None identified"))
          (investment-thesis-entry-conditions thesis)
          (investment-thesis-exit-conditions thesis)
          (investment-thesis-time-horizon thesis)))

(format t "[LLM] #5 Trading-R1 Reasoning Structure loaded~%")

;;; ══════════════════════════════════════════════════════════════════
;;;  PAPER #14: TEXT FEATURE EXTRACTION (PrimoGPT Style)
;;; ══════════════════════════════════════════════════════════════════
;;; BDCC Journal - LLM-Driven Features for DRL
;;; Key insight: Extract trading-relevant features from financial text

(defparameter *text-feature-categories*
  '(:sentiment :volatility-expectation :trend-indication 
    :risk-level :event-type :time-sensitivity)
  "Categories of features to extract from financial text")

(defstruct text-features
  timestamp
  source          ; :news :twitter :report
  raw-text        ; Original text
  sentiment       ; -1.0 to +1.0
  volatility-exp  ; :low :normal :high :extreme
  trend-ind       ; :bullish :bearish :neutral
  risk-level      ; 0.0 to 1.0
  event-type      ; :earnings :fed :geopolitical :technical :none
  time-sensitive  ; T or NIL
  confidence)     ; How confident in extraction

(defun create-text-feature-prompt (text source)
  "Create prompt for LLM to extract trading features from text.
   Based on Paper #14 PrimoGPT approach."
  (format nil "
FINANCIAL TEXT FEATURE EXTRACTION

Source: ~a
Text: \"~a\"

Extract the following features in JSON format:

{
  \"sentiment\": <float from -1.0 (very bearish) to +1.0 (very bullish)>,
  \"volatility_expectation\": <\"low\"|\"normal\"|\"high\"|\"extreme\">,
  \"trend_indication\": <\"bullish\"|\"bearish\"|\"neutral\">,
  \"risk_level\": <float from 0.0 (safe) to 1.0 (dangerous)>,
  \"event_type\": <\"earnings\"|\"fed\"|\"geopolitical\"|\"technical\"|\"none\">,
  \"time_sensitive\": <true|false>,
  \"confidence\": <float from 0.0 to 1.0>
}

Respond with JSON only, no explanation.
"
          source text))

(defun mock-text-features (text source)
  "Create mock text features (when LLM not available).
   Simple keyword-based sentiment analysis."
  (let* ((lower-text (string-downcase text))
         (bullish-words '("up" "bullish" "buy" "strong" "growth" "positive"))
         (bearish-words '("down" "bearish" "sell" "weak" "decline" "negative"))
         (bull-count (count-if (lambda (w) (search w lower-text)) bullish-words))
         (bear-count (count-if (lambda (w) (search w lower-text)) bearish-words))
         (sentiment (cond
                     ((> bull-count bear-count) (/ bull-count 3.0))
                     ((> bear-count bull-count) (- (/ bear-count 3.0)))
                     (t 0.0))))
    (make-text-features
     :timestamp (get-universal-time)
     :source source
     :raw-text text
     :sentiment (max -1.0 (min 1.0 sentiment))
     :volatility-exp :normal
     :trend-ind (cond ((> sentiment 0.3) :bullish)
                     ((< sentiment -0.3) :bearish)
                     (t :neutral))
     :risk-level 0.5
     :event-type :none
     :time-sensitive nil
     :confidence 0.3)))  ; Low confidence for mock

(format t "[LLM] #14 Text Feature Extraction loaded~%")

;;; ══════════════════════════════════════════════════════════════════
;;;  PAPER #28: FINLLAMA SENTIMENT INTEGRATION
;;; ══════════════════════════════════════════════════════════════════
;;; ArXiv:2403.12285 - Financial Sentiment for Algo Trading
;;; Key insight: Integrate sentiment directly into portfolio construction

(defparameter *sentiment-weight* 0.2
  "Weight of sentiment in final signal (0-1)")

(defparameter *sentiment-history* nil
  "Recent sentiment readings for smoothing")

(defparameter *sentiment-history-max* 10
  "Maximum sentiment readings to keep")

(defun integrate-sentiment (base-signal sentiment-score)
  "Integrate sentiment score into trading signal.
   Paper #28: Direct sentiment → portfolio integration."
  (let* ((smoothed-sentiment 
          (if *sentiment-history*
              (/ (+ sentiment-score (reduce #'+ *sentiment-history*))
                 (1+ (length *sentiment-history*)))
              sentiment-score))
         (sentiment-bias (cond
                          ((> smoothed-sentiment 0.5) :boost-long)
                          ((< smoothed-sentiment -0.5) :boost-short)
                          (t :neutral))))
    ;; Update history
    (push sentiment-score *sentiment-history*)
    (when (> (length *sentiment-history*) *sentiment-history-max*)
      (setf *sentiment-history* (butlast *sentiment-history*)))
    
    ;; Apply integration
    (case base-signal
      (:BUY 
       (if (eq sentiment-bias :boost-long)
           (values :STRONG-BUY (+ 1.0 *sentiment-weight*))
           (values :BUY 1.0)))
      (:SELL
       (if (eq sentiment-bias :boost-short)
           (values :STRONG-SELL (+ 1.0 *sentiment-weight*))
           (values :SELL 1.0)))
      (otherwise
       (cond
        ((eq sentiment-bias :boost-long) (values :WEAK-BUY *sentiment-weight*))
        ((eq sentiment-bias :boost-short) (values :WEAK-SELL *sentiment-weight*))
        (t (values :HOLD 0.0)))))))

(format t "[LLM] #28 FinLlama Sentiment Integration loaded~%")

;;; ══════════════════════════════════════════════════════════════════
;;;  PAPER #24: LOGIC-GUIDED DSL EXTENSION
;;; ══════════════════════════════════════════════════════════════════
;;; ArXiv:2310.05551v1 - SYENS: Logic-guided DRL
;;; Key insight: Embed human expertise as logical "sketches"

(defparameter *expert-sketches*
  '(;; Trend-following sketch
    (:trend-follow
     :precondition (and (eq regime :trending) 
                        (eq dual-trend-agreement :aligned))
     :action-sketch "Follow the prevailing trend direction"
     :constraints ((> confidence 0.6)))
    
    ;; Mean reversion sketch
    (:mean-revert
     :precondition (and (eq regime :ranging)
                        (member mean-signal '(:OVERSOLD :OVERBOUGHT)))
     :action-sketch "Counter the extreme move"
     :constraints ((< volatility 1.5)))
    
    ;; Breakout sketch
    (:breakout
     :precondition (and (eq regime :volatile)
                        (> atr-ratio 1.5))
     :action-sketch "Trade the breakout direction"
     :constraints ((> volume-spike 2.0)))
    
    ;; Safe-haven sketch
    (:safe-haven
     :precondition (or (> danger-level 2)
                      (< daily-pnl -300))
     :action-sketch "Reduce exposure or exit"
     :constraints ()))
  "Expert knowledge encoded as logical sketches (Paper #24)")

(defun match-expert-sketch (context)
  "Find matching expert sketch for current context."
  (dolist (sketch *expert-sketches*)
    (let* ((name (first sketch))
           (props (rest sketch))
           (precondition (getf props :precondition)))
      ;; Simple pattern matching (in real impl, would eval precondition)
      (when (case name
              (:trend-follow (eq (getf context :regime) :trending))
              (:mean-revert (eq (getf context :regime) :ranging))
              (:breakout (eq (getf context :regime) :volatile))
              (:safe-haven (or (> (or (getf context :danger-level) 0) 2)
                              (< (or (getf context :daily-pnl) 0) -300)))
              (otherwise nil))
        (format t "[LOGIC] 📐 Matched sketch: ~a~%" name)
        (return (list :sketch name
                     :action (getf props :action-sketch)
                     :constraints (getf props :constraints))))))
  nil)

(format t "[LLM] #24 Logic-Guided DSL Extension loaded~%")

;;; ══════════════════════════════════════════════════════════════════
;;;  INTEGRATED LLM DECISION SUPPORT
;;; ══════════════════════════════════════════════════════════════════

(defun llm-enhanced-decision (symbol analysis context)
  "Generate LLM-enhanced trading decision.
   Combines Papers #5, #14, #28, #24."
  (let* (;; Match expert sketch
         (sketch (match-expert-sketch context))
         ;; Determine base direction from analysis
         (dual-trend (getf analysis :dual-trend))
         (base-direction (case (getf dual-trend :direction)
                          (:UP :BUY)
                          (:DOWN :SELL)
                          (t :HOLD)))
         ;; Generate thesis
         (thesis (when (member base-direction '(:BUY :SELL))
                  (generate-investment-thesis symbol base-direction analysis))))
    
    (format t "~%[LLM] ═══════════════════════════════════════~%")
    (format t "[LLM] 🧠 LLM-ENHANCED DECISION SUPPORT~%")
    (format t "[LLM] ═══════════════════════════════════════~%")
    (when sketch
      (format t "[LLM] Expert Sketch: ~a~%" (getf sketch :sketch))
      (format t "[LLM] Guidance: ~a~%" (getf sketch :action)))
    (when thesis
      (format t "[LLM] Thesis: ~a~%" (investment-thesis-thesis-summary thesis))
      (format t "[LLM] Confidence: ~,0f%%~%" (* 100 (investment-thesis-confidence thesis))))
    (format t "[LLM] ═══════════════════════════════════════~%")
    
    (list :base-direction base-direction
          :sketch sketch
          :thesis thesis)))

(format t "~%[LLM] ════════════════════════════════════════════~%")
(format t "[LLM] 🧠 LLM Integration Preparation Complete~%")
(format t "[LLM] Papers: #5 Trading-R1, #14 PrimoGPT, #28 FinLlama, #24 SYENS~%")
(format t "[LLM] ════════════════════════════════════════════~%")
