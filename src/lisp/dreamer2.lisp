;; dreamer2.lisp - Code Generator (Genesis v2) + Backtester Integration
;; 変数を関数の前に定義して警告を防ぐ
(defparameter *evolved-strategies* nil)

;; Forward declarations for variables defined in school.lisp
(defvar *failure-log* nil)
(defvar *success-log* nil)
(defvar *regime-performance* nil)
(defvar *current-regime* :unknown)
(defvar *volatility-regime* :normal)
(defvar *candle-histories* nil)
(defvar *category-pools* nil)  ; Used by seed-evolution-from-knowledge-base

;; Forward declarations for struct accessors
(declaim (ftype (function (t) t) trade-record-category))
(declaim (ftype (function (t) t) trade-record-pnl))
(declaim (ftype (function (t) t) trade-record-session))
(declaim (ftype (function (t) t) trade-record-regime))

;;; ==========================================
;;; SELF-ANALYSIS v2.0 (構造化自己分析)
;;; ==========================================
;;; Features:
;;; - Structured performance metrics by category/session/regime
;;; - Strategy performance ranking
;;; - Actionable insights generation
;;; - JSON-formatted feedback for Gemini

(defun analyze-by-category ()
  "Analyze performance by trading category"
  (let ((stats (make-hash-table :test 'eq)))
    ;; Initialize categories
    (dolist (cat '(:trend :reversion :breakout :scalp))
      (setf (gethash cat stats) (list 0 0 0.0)))  ; wins losses pnl
    ;; Aggregate from logs
    (when (boundp '*success-log*)
      (dolist (record *success-log*)
        (let* ((cat (trade-record-category record))
               (s (gethash cat stats (list 0 0 0.0))))
          (incf (first s))
          (incf (third s) (or (trade-record-pnl record) 0))
          (setf (gethash cat stats) s))))
    (when (boundp '*failure-log*)
      (dolist (record *failure-log*)
        (let* ((cat (trade-record-category record))
               (s (gethash cat stats (list 0 0 0.0))))
          (incf (second s))
          (incf (third s) (or (trade-record-pnl record) 0))
          (setf (gethash cat stats) s))))
    stats))

(defun analyze-by-session ()
  "Analyze performance by trading session"
  (let ((stats (make-hash-table :test 'eq)))
    (dolist (session '(:tokyo :london :newyork :overlap :off))
      (setf (gethash session stats) (list 0 0 0.0)))
    (when (boundp '*success-log*)
      (dolist (record *success-log*)
        (let* ((sess (trade-record-session record))
               (s (gethash sess stats (list 0 0 0.0))))
          (when s
            (incf (first s))
            (incf (third s) (or (trade-record-pnl record) 0))
            (setf (gethash sess stats) s)))))
    (when (boundp '*failure-log*)
      (dolist (record *failure-log*)
        (let* ((sess (trade-record-session record))
               (s (gethash sess stats (list 0 0 0.0))))
          (when s
            (incf (second s))
            (incf (third s) (or (trade-record-pnl record) 0))
            (setf (gethash sess stats) s)))))
    stats))

(defun analyze-by-regime ()
  "Analyze performance by market regime"
  (let ((stats (make-hash-table :test 'eq)))
    (dolist (regime '(:trending :ranging :unknown))
      (setf (gethash regime stats) (list 0 0 0.0)))
    (when (boundp '*success-log*)
      (dolist (record *success-log*)
        (let* ((reg (or (trade-record-regime record) :unknown))
               (s (gethash reg stats (list 0 0 0.0))))
          (incf (first s))
          (incf (third s) (or (trade-record-pnl record) 0))
          (setf (gethash reg stats) s))))
    (when (boundp '*failure-log*)
      (dolist (record *failure-log*)
        (let* ((reg (or (trade-record-regime record) :unknown))
               (s (gethash reg stats (list 0 0 0.0))))
          (incf (second s))
          (incf (third s) (or (trade-record-pnl record) 0))
          (setf (gethash reg stats) s))))
    stats))

(defun format-stats-summary (stats-hash)
  "Format hash table stats into readable string"
  (let ((result nil))
    (maphash (lambda (key val)
               (let* ((wins (first val))
                      (losses (second val))
                      (pnl (third val))
                      (total (+ wins losses))
                      (win-rate (if (> total 0) (* 100 (/ wins total)) 0)))
                 (when (> total 0)
                   (push (format nil "~a: ~,0f% win (~d trades, ~,2f PnL)" 
                                 key win-rate total pnl) result))))
             stats-hash)
    (format nil "~{~a~^; ~}" (reverse result))))

(defun get-best-and-worst-conditions ()
  "Identify best and worst trading conditions"
  (let ((conditions nil))
    ;; Analyze by session
    (let ((session-stats (analyze-by-session)))
      (maphash (lambda (sess val)
                 (let* ((wins (first val)) (losses (second val))
                        (total (+ wins losses)))
                   (when (> total 3)
                     (push (cons sess (/ wins (max 1 total))) conditions))))
               session-stats))
    ;; Sort by win rate
    (let ((sorted (sort conditions #'> :key #'cdr)))
      (list :best (car (first sorted))
            :best-rate (cdr (first sorted))
            :worst (car (car (last sorted)))
            :worst-rate (cdr (car (last sorted)))))))

(defun generate-actionable-insights ()
  "Generate specific actionable insights from analysis"
  (let ((insights nil)
        (conditions (get-best-and-worst-conditions)))
    ;; Session insights
    (when (and (getf conditions :best) (> (getf conditions :best-rate) 0.6))
      (push (format nil "FOCUS on ~a session (~,0f% win rate)" 
                    (getf conditions :best) (* 100 (getf conditions :best-rate))) insights))
    (when (and (getf conditions :worst) (< (getf conditions :worst-rate) 0.4))
      (push (format nil "AVOID ~a session (~,0f% win rate)" 
                    (getf conditions :worst) (* 100 (getf conditions :worst-rate))) insights))
    ;; Regime insights from regime-performance if available
    (when (boundp '*regime-performance*)
      (maphash (lambda (key val)
                 (let* ((wins (first val)) (losses (second val))
                        (total (+ wins losses)))
                   (when (and (> total 5) (< (/ wins (max 1 total)) 0.35))
                     (push (format nil "REDUCE trading in ~a conditions" key) insights))))
               *regime-performance*))
    ;; Return as formatted string
    (if insights
        (format nil "~{- ~a~^~%~}" insights)
        "No strong patterns detected yet. Continue gathering data.")))

(defun get-top-strategies (n)
  "Get top N performing strategies by Sharpe ratio"
  (when *evolved-strategies*
    (let ((sorted (sort (copy-list *evolved-strategies*) #'> 
                        :key (lambda (s) (or (strategy-sharpe s) -999)))))
      (subseq sorted 0 (min n (length sorted))))))

(defun get-structured-self-analysis ()
  "Generate comprehensive structured analysis for Gemini"
  (let* ((category-stats (analyze-by-category))
         (session-stats (analyze-by-session))
         (regime-stats (analyze-by-regime))
         (top-strats (get-top-strategies 3))
         (insights (generate-actionable-insights)))
    (format nil "
=== SELF-ANALYSIS REPORT ===

PERFORMANCE BY CATEGORY:
~a

PERFORMANCE BY SESSION:
~a

PERFORMANCE BY REGIME:
~a

TOP STRATEGIES:
~{~a (Sharpe: ~,2f)~^, ~}

ACTIONABLE INSIGHTS:
~a

CURRENT MARKET: Regime=~a, Volatility=~a
"
            (format-stats-summary category-stats)
            (format-stats-summary session-stats)
            (format-stats-summary regime-stats)
            (if top-strats
                (loop for s in top-strats 
                      collect (strategy-name s)
                      collect (or (strategy-sharpe s) 0))
                (list "None" 0))
            insights
            *current-regime*
            *volatility-regime*)))

(defun find-balanced-end (str start)
  "Find the end of a balanced s-expression starting at position start"
  (let ((depth 0) (in-string nil))
    (loop for i from start below (length str)
          for c = (char str i)
          do (cond
               ((and (char= c #\\) (not in-string) (< (1+ i) (length str)))
                (incf i))  ; skip escaped char
               ((char= c #\") (setf in-string (not in-string)))
               ((and (not in-string) (char= c #\()) (incf depth))
               ((and (not in-string) (char= c #\)))
                (decf depth)
                (when (zerop depth) (return (1+ i)))))
          finally (return nil))))

;; Extract SMA parameters from indicator list
(defun extract-sma-params (indicators)
  "Extract short and long SMA periods from indicators list"
  (let ((smas (remove-if-not (lambda (ind) (eq (car ind) 'sma)) indicators)))
    (when (>= (length smas) 2)
      (let ((periods (sort (mapcar #'cadr smas) #'<)))
        (values (first periods) (second periods))))))

;; Convert strategy to JSON for Rust backtester
(defun strategy-to-json (strat)
  "Convert strategy struct to JSON for backtest request"
  (multiple-value-bind (sma-short sma-long) 
      (extract-sma-params (strategy-indicators strat))
    (jsown:new-js
      ("name" (strategy-name strat))
      ("sma_short" (or sma-short 5))
      ("sma_long" (or sma-long 20))
      ("sl" (strategy-sl strat))
      ("tp" (strategy-tp strat))
      ("volume" (strategy-volume strat)))))

;; Convert candle history to JSON
(defun candles-to-json (history)
  "Convert candle list to JSON array"
  (mapcar (lambda (c)
            (let ((close (candle-close c)))
              (jsown:new-js
                ("t" (or (candle-timestamp c) 0))
                ("o" (or (candle-open c) close))
                ("h" (or (candle-high c) close))
                ("l" (or (candle-low c) close))
                ("c" close))))
          (reverse (subseq history 0 (min 500 (length history))))))

;; Request backtest from Rust
(defun request-backtest (strat)
  "Send strategy to Rust for high-speed backtesting"
  (format t "[L] 📊 Requesting backtest for ~a...~%" (strategy-name strat))
  (let ((msg (jsown:to-json 
               (jsown:new-js 
                 ("action" "BACKTEST")
                 ("strategy" (strategy-to-json strat))
                 ("candles" (candles-to-json *candle-history*))))))
    (pzmq:send *cmd-publisher* msg)
    (format t "[L] 📤 Sent ~d bytes to Rust~%" (length msg))))

;; Clone check threshold (0.85 = 85% similar = reject, more diversity)
(defparameter *clone-threshold* 0.85)

;; Convert existing evolved strategies to JSON for clone check
(defun evolved-strategies-to-json ()
  "Convert evolved strategies list to JSON for clone check"
  (mapcar #'strategy-to-json *evolved-strategies*))

;; Request clone check from Rust
(defun request-clone-check (new-strat callback)
  "Check if new strategy is a clone of existing ones"
  (when (and *evolved-strategies* (> (length *evolved-strategies*) 0))
    (format t "[L] 🧬 Checking for clones...~%")
    (let ((msg (jsown:to-json 
                 (jsown:new-js 
                   ("action" "CHECK_CLONE")
                   ("new_strategy" (strategy-to-json new-strat))
                   ("existing_strategies" (evolved-strategies-to-json))
                   ("threshold" *clone-threshold*)))))
      (pzmq:send *cmd-publisher* msg))))

(defun dream-code ()
  "Generate strategy CODE with comprehensive self-analysis feedback"
  (format t "~%[L] ☁️ Dreaming CODE with Self-Analysis v2.0... ")
  (let* ((random-seed (random 1000))
         (self-analysis (get-structured-self-analysis))
         (failure-summary (get-failure-summary))
         (prompt (format nil "You are an AI trading strategy designer. Generate a NEW strategy based on this performance analysis.

~a

FAILURE SUMMARY: ~a

=== STRATEGY GENERATION RULES ===
1. AVOID patterns that have been failing
2. FOCUS on conditions that work well (high win rate)
3. Use creative parameter combinations NOT in top strategies
4. Adapt to current market conditions

Generate a strategy with:
- Unique creative name (3-8 letters)
- SMA short: 3-15 (pick based on volatility)
- SMA long: 20-100 (must be > short)
- SL: 0.05-0.5 (tighter in ranging, wider in trending)
- TP: 0.1-1.0 (adjust to regime)

Random seed for variety: ~d

Format EXACTLY:
(defstrategy \"NAME\" :indicators ((sma SHORT) (sma LONG)) :entry (cross-above sma-SHORT sma-LONG) :exit (cross-below sma-SHORT sma-LONG) :sl SL :tp TP :volume 0.01)

Output ONLY the s-expression. No markdown or explanation."
                         self-analysis failure-summary random-seed))
         (resp (call-gemini prompt))
         (start (when resp (search "(defstrategy" resp)))
         (end (when start (find-balanced-end resp start))))
    ;; 括弧バランスチェック
    (when (and start end)
      (let* ((code (subseq resp start end))
             (opens (count #\( code))
             (closes (count #\) code)))
        (if (/= opens closes)
            (format t "~%[L] ⚠️ Unbalanced parens: ~d/~d~%" opens closes)
            (progn
              (format t "~%[L] 📝 Generated: ~a~%" (subseq code 0 (min 80 (length code))))
              (multiple-value-bind (result valid) (safe-eval-strategy code)
                (if valid
                    (progn
                      (format t "[L] ✅ Valid strategy!~%")
                      ;; Check for clones if we have existing strategies
                      (if (and *evolved-strategies* (> (length *evolved-strategies*) 0))
                          (progn
                            (setf *pending-strategy* result)  ; Save for clone check result
                            (request-clone-check result nil))
                          ;; No existing strategies, just add it
                          (progn
                            (push result *evolved-strategies*)
                            (when (and *candle-history* (> (length *candle-history*) 50))
                              (request-backtest result))
                            (notify-discord (format nil "🧬 CODE: ~a" (subseq code 0 (min 100 (length code)))) :color 65535)))))
                    (format t "[L] ❌ Invalid code~%"))))))))


;; Store pending strategy for clone check result
(defparameter *pending-strategy* nil)

;; ===== GENETIC PROGRAMMING =====

;; Mutation: randomly adjust parameters
(defun mutate-value (val min-v max-v mutation-rate)
  "Mutate a numeric value with given probability"
  (if (< (random 1.0) mutation-rate)
      (let ((delta (* (- (random 2.0) 1.0) (/ (- max-v min-v) 4))))
        (max min-v (min max-v (+ val delta))))
      val))

(defun mutate-strategy (strat &optional (rate 0.3))
  "Mutate strategy parameters"
  (multiple-value-bind (sma-s sma-l) (extract-sma-params (strategy-indicators strat))
    (let* ((new-sma-s (floor (mutate-value (or sma-s 5) 3 15 rate)))
           (new-sma-l (floor (mutate-value (or sma-l 20) 20 100 rate)))
           (new-sl (mutate-value (strategy-sl strat) 0.05 0.5 rate))
           (new-tp (mutate-value (strategy-tp strat) 0.1 1.0 rate))
           (new-name (format nil "~a-M~d" (strategy-name strat) (random 100))))
      ;; Ensure sma-l > sma-s
      (when (<= new-sma-l new-sma-s) (setf new-sma-l (+ new-sma-s 10)))
      (make-strategy :name new-name
                     :indicators (list (list 'sma new-sma-s) (list 'sma new-sma-l))
                     :entry (strategy-entry strat)
                     :exit (strategy-exit strat)
                     :sl new-sl :tp new-tp :volume (strategy-volume strat)))))

;; Crossover: combine two strategies (IMPROVED - preserves strategy diversity)
(defun crossover-strategies (parent1 parent2)
  "Create child strategy by combining two parents - preserves strategy structure"
  ;; Randomly pick which parent's indicators/entry/exit to use (preserves strategy type)
  (let* ((indicator-parent (if (zerop (random 2)) parent1 parent2))
         (logic-parent (if (zerop (random 2)) parent1 parent2))
         ;; Blend SL/TP from both parents
         (child-sl (/ (+ (strategy-sl parent1) (strategy-sl parent2)) 2))
         (child-tp (/ (+ (strategy-tp parent1) (strategy-tp parent2)) 2))
         (child-name (format nil "~a×~a" 
                             (subseq (strategy-name parent1) 0 (min 4 (length (strategy-name parent1))))
                             (subseq (strategy-name parent2) 0 (min 4 (length (strategy-name parent2)))))))
    ;; Add slight mutation to SL/TP (10% variance)
    (when (< (random 1.0) 0.3)
      (setf child-sl (* child-sl (+ 0.9 (random 0.2)))))
    (when (< (random 1.0) 0.3)
      (setf child-tp (* child-tp (+ 0.9 (random 0.2)))))
    (make-strategy :name child-name
                   :indicators (strategy-indicators indicator-parent)  ; Preserve parent's indicators
                   :entry (strategy-entry logic-parent)
                   :exit (strategy-exit logic-parent)
                   :sl (max 0.05 (min 0.5 child-sl))  ; Clamp to valid range
                   :tp (max 0.1 (min 1.0 child-tp))
                   :volume 0.01)))

;; Seed evolution pool from knowledge base (ALL 61 strategies!)
(defun seed-evolution-from-knowledge-base ()
  "Seed *evolved-strategies* with top strategies from each category"
  (when (boundp '*category-pools*)
    (let ((categories '(:trend :reversion :breakout :scalp))
          (count 0))
      (setf *evolved-strategies* nil)
      ;; Add top 3 from each category (12 total)
      (dolist (cat categories)
        (let ((pool (gethash cat *category-pools*)))
          (when pool
            (dolist (strat (subseq pool 0 (min 3 (length pool))))
              (push strat *evolved-strategies*)
              (incf count)))))
      (when (> count 0)
        (format t "[L] 🧬 EVOLUTION SEEDED with ~d strategies:~%" count)
        (dolist (s (subseq *evolved-strategies* 0 (min 5 (length *evolved-strategies*))))
          (format t "[L]    • ~a~%" (strategy-name s)))))))

;; Evolve population using genetic operations (with auto-seeding)
(defun evolve-population ()
  "Apply genetic operations - auto-seeds from 61 strategies if pool is empty"
  ;; Seed from knowledge base if pool is empty
  (when (< (length *evolved-strategies*) 2)
    (seed-evolution-from-knowledge-base))
  
  (when (>= (length *evolved-strategies*) 2)
    (format t "[L] 🧬 Evolving (~d strategies in pool)~%" (length *evolved-strategies*))
    (let* ((sorted (sort (copy-list *evolved-strategies*) #'> :key #'strategy-sharpe))
           (parent1 (first sorted))
           (parent2 (second sorted)))
      ;; Log which strategies are being crossed (VERBOSE)
      (format t "[L] 🧬 Parents: ~a × ~a~%" (strategy-name parent1) (strategy-name parent2))
      (format t "[L]    P1 indicators: ~a~%" (strategy-indicators parent1))
      (format t "[L]    P2 indicators: ~a~%" (strategy-indicators parent2))
      ;; Crossover
      (let ((child (crossover-strategies parent1 parent2)))
        (format t "[L] 🧬 Child: ~a (inherits: ~a)~%" 
                (strategy-name child) (strategy-indicators child))
        ;; Mutate the child
        (let ((mutant (mutate-strategy child 0.3)))
          (format t "[L] 🧬 Mutant: ~a~%" (strategy-name mutant))
          (setf *pending-strategy* mutant)
          (request-clone-check mutant nil))))))

(format t "[DREAMER2] Loaded (with Backtester + Clone Detection + Genetic Programming)~%")

;;; ══════════════════════════════════════════════════════════════════
;;;  VERBALIZED SAMPLING (ArXiv:2510.01171)
;;; ══════════════════════════════════════════════════════════════════
;;; Mitigate mode collapse and unlock diversity in strategy generation.
;;; Instead of generating one strategy, generate multiple with probabilities.
;;;
;;; Key insight: Post-training alignment causes "typicality bias" where 
;;; models favor familiar outputs. VS prompts model to verbalize probability
;;; distribution over responses, increasing diversity 1.6-2.1x.

(defparameter *vs-candidate-count* 5
  "Number of strategy candidates to generate with Verbalized Sampling")

(defparameter *vs-temperature-boost* 0.3
  "Temperature boost for diversity when using VS")

(defun generate-vs-prompt (strategy-type context)
  "Generate a Verbalized Sampling prompt for diverse strategy generation.
   Based on ArXiv:2510.01171 methodology."
  (format nil "You are an expert algorithmic trading strategist.

TASK: Generate ~d distinct trading strategies of type '~a' with their confidence probabilities.

CONTEXT:
- Current regime: ~a
- Recent performance: ~a
- Market volatility: ~a

OUTPUT FORMAT (JSON):
{
  \"strategies\": [
    {
      \"name\": \"Strategy-Name-1\",
      \"probability\": 0.35,
      \"indicators\": [[\"sma\", 5], [\"sma\", 20]],
      \"entry\": \"cross-above sma-5 sma-20\",
      \"exit\": \"cross-below sma-5 sma-20\",
      \"sl\": 0.50,
      \"tp\": 1.00,
      \"reasoning\": \"Why this strategy fits the context\"
    },
    ...
  ]
}

RULES:
1. Each strategy MUST be meaningfully different (not just parameter variations)
2. Probabilities MUST sum to 1.0
3. Higher probability = more confident this fits current conditions
4. Include at least one contrarian/unconventional approach (low probability but valuable diversity)
5. DO NOT repeat the same structure with minor changes

Generate ~d strategies now:"
          *vs-candidate-count*
          strategy-type
          (or (getf context :regime) "unknown")
          (or (getf context :performance) "neutral")
          (or (getf context :volatility) "normal")
          *vs-candidate-count*))

(defun parse-vs-response (response-json)
  "Parse Verbalized Sampling response into list of (strategy . probability) pairs"
  (handler-case
      (let* ((parsed (jsown:parse response-json))
             (strategies (jsown:val parsed "strategies")))
        (mapcar (lambda (s)
                  (cons (make-strategy 
                         :name (jsown:val s "name")
                         :indicators (mapcar (lambda (ind) 
                                              (list (intern (string-upcase (first ind)) :keyword)
                                                    (second ind)))
                                            (jsown:val s "indicators"))
                         :entry (jsown:val s "entry")
                         :exit (jsown:val s "exit")
                         :sl (jsown:val s "sl")
                         :tp (jsown:val s "tp")
                         :volume 0.01)
                        (jsown:val s "probability")))
                strategies))
    (error (e)
      (format t "[VS] Parse error: ~a~%" e)
      nil)))

(defun select-from-vs-distribution (candidates)
  "Select a strategy from VS candidates using weighted random sampling.
   This maintains diversity while respecting confidence scores."
  (when candidates
    (let* ((total-prob (reduce #'+ candidates :key #'cdr))
           (roll (random total-prob))
           (cumulative 0.0))
      (dolist (pair candidates)
        (incf cumulative (cdr pair))
        (when (>= cumulative roll)
          (return-from select-from-vs-distribution (car pair))))
      ;; Fallback to highest probability
      (car (first (sort (copy-list candidates) #'> :key #'cdr))))))

(defun vs-explore-diverse (candidates)
  "Deliberately select a LOW probability candidate for exploration.
   This combats mode collapse by occasionally trying unconventional strategies."
  (when (and candidates (> (length candidates) 2))
    (let ((sorted (sort (copy-list candidates) #'< :key #'cdr)))
      ;; Return bottom 20% candidate
      (car (nth (min 1 (1- (length sorted))) sorted)))))

(defun dream-with-vs (strategy-type &key (explore-p nil))
  "Generate strategy using Verbalized Sampling for diversity.
   If explore-p is T, deliberately choose unconventional strategy."
  (format t "[VS] 🎲 Verbalized Sampling for ~a (explore=~a)~%" strategy-type explore-p)
  (let* ((context (list :regime *current-regime*
                        :volatility *volatility-regime*
                        :performance (if *evolved-strategies*
                                        (format nil "top sharpe: ~,2f" 
                                                (strategy-sharpe (first *evolved-strategies*)))
                                        "no data")))
         (prompt (generate-vs-prompt strategy-type context)))
    ;; Note: Actual API call would go here. For now, using genetic fallback.
    (format t "[VS] 📝 Prompt generated (~d chars)~%" (length prompt))
    ;; Return prompt for external use or fallback to genetic
    prompt))

(defun vs-diversity-score (strategies)
  "Calculate diversity score for a set of strategies.
   Higher = more diverse (better for avoiding mode collapse)."
  (if (< (length strategies) 2)
      0.0
      (let* ((n (length strategies))
             (pairs (/ (* n (1- n)) 2))
             (total-diff 0.0))
        (loop for i from 0 below (1- n)
              do (loop for j from (1+ i) below n
                       do (let* ((s1 (nth i strategies))
                                 (s2 (nth j strategies))
                                 (sl-diff (abs (- (strategy-sl s1) (strategy-sl s2))))
                                 (tp-diff (abs (- (strategy-tp s1) (strategy-tp s2)))))
                            (incf total-diff (+ sl-diff tp-diff)))))
        (/ total-diff pairs))))

(format t "[VS] Verbalized Sampling loaded (ArXiv:2510.01171)~%")







