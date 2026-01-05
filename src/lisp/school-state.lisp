;; school-state.lisp - Shared State for Swimmy Trading System
;; V6.13: Extracted from school.lisp for modular architecture
;; All shared state variables are defined here and exported

;;; ==========================================
;;; FORWARD DECLARATIONS
;;; ==========================================
(defvar *has-resigned-today* nil)
(defvar *current-leader* nil)
(defvar *trade-history* (make-hash-table :test 'eq))
(defvar *category-entries* (make-hash-table :test 'eq))
(defvar *last-swarm-consensus* 0)
(defvar *category-positions* nil)
(defvar *daily-pnl* 0)
(defvar *accumulated-pnl* 0)
(defvar *category-trades* 0)

;; Warrior System (school-danger.lisp)
(defparameter *warrior-allocation* (make-hash-table :test 'equal) 
  "Tracks which warrior is assigned to which trade/symbol")

;; Ritual History (rituals.lisp)
(defparameter *win-rate-history* nil "Historical win rates")
(defparameter *max-win-rate-history* 100 "Max size of win rate history")

;; Tribe signal integration
(defvar *tribe-direction* :hold "Current tribe consensus direction")
(defvar *tribe-consensus* 0.0 "Current tribe consensus strength")

;;; ==========================================
;;; CORRELATION & EXPOSURE MANAGEMENT
;;; ==========================================
(defparameter *pair-correlations*
  '(("USDJPY" . (("EURJPY" . 0.85) ("GBPJPY" . 0.80) ("EURUSD" . -0.60) ("GBPUSD" . -0.50)))
    ("EURUSD" . (("GBPUSD" . 0.90) ("USDJPY" . -0.60) ("EURJPY" . 0.30)))
    ("GBPUSD" . (("EURUSD" . 0.90) ("USDJPY" . -0.50) ("GBPJPY" . 0.40)))))

(defparameter *symbol-exposure* (make-hash-table :test 'equal))
(defparameter *max-symbol-exposure* 0.15)
(defparameter *max-total-exposure* 0.30)

;;; ==========================================
;;; DYNAMIC CORRELATION (Taleb Homework #3)
;;; ==========================================
;;; Calculate rolling correlations from live price data
;;; instead of relying on static historical averages

(defparameter *dynamic-correlations* (make-hash-table :test 'equal)
  "Hash: (sym1 . sym2) -> correlation value from rolling calculation")
(defparameter *correlation-window* 100
  "Number of candles for correlation calculation")
(defparameter *correlation-alert-threshold* 0.8
  "Alert when correlation exceeds this (correlation breakdown risk)")
(defparameter *last-correlation-update* 0)
(defparameter *correlation-update-interval* 300)  ;; Update every 5 minutes

(defun get-price-returns (history n)
  "Calculate price returns from candle history"
  (when (and history (> (length history) n))
    (loop for i from 0 below n
          for c1 = (nth i history)
          for c2 = (nth (1+ i) history)
          when (and c1 c2 (candle-close c1) (candle-close c2) (> (candle-close c2) 0))
          collect (/ (- (candle-close c1) (candle-close c2)) (candle-close c2)))))

(defun calculate-dynamic-correlation (sym1 sym2)
  "Calculate rolling correlation between two symbols using price history"
  (let ((history1 (gethash sym1 *candle-histories*))
        (history2 (gethash sym2 *candle-histories*)))
    (when (and history1 history2
               (> (length history1) *correlation-window*)
               (> (length history2) *correlation-window*))
      (let* ((returns1 (get-price-returns history1 *correlation-window*))
             (returns2 (get-price-returns history2 *correlation-window*))
             (n (min (length returns1) (length returns2))))
        (when (> n 10)
          (let* ((r1 (subseq returns1 0 n))
                 (r2 (subseq returns2 0 n))
                 (mean1 (/ (reduce #'+ r1) n))
                 (mean2 (/ (reduce #'+ r2) n))
                 (cov (/ (reduce #'+ (mapcar (lambda (a b) (* (- a mean1) (- b mean2))) r1 r2)) n))
                 (var1 (/ (reduce #'+ (mapcar (lambda (a) (expt (- a mean1) 2)) r1)) n))
                 (var2 (/ (reduce #'+ (mapcar (lambda (a) (expt (- a mean2) 2)) r2)) n))
                 (std1 (sqrt var1))
                 (std2 (sqrt var2)))
            (if (and (> std1 0) (> std2 0))
                (/ cov (* std1 std2))
                0.0)))))))

(defun update-all-correlations ()
  "Update all dynamic correlations between supported symbols"
  (let ((symbols (if (boundp '*supported-symbols*) *supported-symbols* '("USDJPY" "EURUSD" "GBPUSD")))
        (updated 0))
    (dolist (sym1 symbols)
      (dolist (sym2 symbols)
        (unless (string= sym1 sym2)
          (let ((corr (calculate-dynamic-correlation sym1 sym2)))
            (when corr
              (setf (gethash (cons sym1 sym2) *dynamic-correlations*) corr)
              (incf updated))))))
    (setf *last-correlation-update* (get-universal-time))
    (format t "[C] 📊 Updated ~d dynamic correlations~%" updated)))

(defun maybe-update-correlations ()
  "Update correlations if interval has passed"
  (when (> (- (get-universal-time) *last-correlation-update*) *correlation-update-interval*)
    (update-all-correlations)))

(defun get-dynamic-correlation (sym1 sym2)
  "Get dynamic correlation, falling back to static if not available"
  (or (gethash (cons sym1 sym2) *dynamic-correlations*)
      (gethash (cons sym2 sym1) *dynamic-correlations*)
      ;; Fall back to static
      (let ((pairs (cdr (assoc sym1 *pair-correlations* :test #'string=))))
        (if pairs (or (cdr (assoc sym2 pairs :test #'string=)) 0.0) 0.0))))

(defun check-correlation-breakdown ()
  "Detect correlation regime change (correlations moving toward 1.0)"
  (let ((alerts nil))
    (maphash
     (lambda (pair corr)
       (let* ((sym1 (car pair))
              (sym2 (cdr pair))
              (static (let ((pairs (cdr (assoc sym1 *pair-correlations* :test #'string=))))
                        (if pairs (or (cdr (assoc sym2 pairs :test #'string=)) 0.0) 0.0)))
              (abs-corr (abs corr))
              (abs-static (abs static))
              (drift (- abs-corr abs-static)))
         ;; Alert if correlation significantly increased
         (when (and (> abs-corr *correlation-alert-threshold*)
                    (> drift 0.2))
           (push (list :pair pair :dynamic corr :static static :drift drift) alerts))))
     *dynamic-correlations*)
    (when alerts
      (format t "[C] ⚠️ CORRELATION BREAKDOWN DETECTED!~%")
      (dolist (a alerts)
        (format t "[C]    ~a-~a: ~,2f (was ~,2f, drift +~,2f)~%"
                (car (getf a :pair)) (cdr (getf a :pair))
                (getf a :dynamic) (getf a :static) (getf a :drift)))
      (when (fboundp 'notify-discord-alert)
        (notify-discord-alert
         (format nil "⚠️ CORRELATION BREAKDOWN~%~{~a~%~}"
                 (mapcar (lambda (a)
                           (format nil "~a-~a: ~,2f → ~,2f"
                                   (car (getf a :pair)) (cdr (getf a :pair))
                                   (getf a :static) (getf a :dynamic)))
                         alerts)))))
    alerts))

(defun get-portfolio-correlation-risk ()
  "Calculate overall portfolio correlation risk score (0-1)"
  (maybe-update-correlations)
  (let ((total-risk 0.0)
        (count 0))
    (maphash
     (lambda (pair corr)
       (declare (ignore pair))
       (incf total-risk (abs corr))
       (incf count))
     *dynamic-correlations*)
    (if (> count 0) (/ total-risk count) 0.0)))

(defun check-correlation-risk (symbol direction)
  "Check correlation risk for a new trade (used by risk-manager.lisp)"
  (maybe-update-correlations)
  (let ((max-corr 0.0))
    (maphash
     (lambda (sym exp)
       (when (and (> exp 0) (not (string= sym symbol)))
         (let ((corr (abs (get-dynamic-correlation symbol sym))))
           (when (> corr max-corr)
             (setf max-corr corr)))))
     *symbol-exposure*)
    max-corr))


;;; ==========================================
;;; DANGER AVOIDANCE SYSTEM
;;; ==========================================
(defparameter *consecutive-losses* 0)
(defparameter *consecutive-wins* 0)
(defparameter *last-trade-result* nil)
(defparameter *danger-cooldown-until* 0)
(defparameter *danger-level* 0)

(defparameter *cooldown-durations*
  '((1 . 0)
    (2 . 120)
    (3 . 300)
    (4 . 600)
    (5 . 1800)))

;;; ==========================================
;;; RESIGNATION JUDGMENT
;;; ==========================================
(defparameter *resignation-threshold* -5000)
(defparameter *resignation-loss-count* 7)

;;; ==========================================
;;; FAILURE ANALYSIS
;;; ==========================================
(defparameter *failure-log* nil)
(defparameter *success-log* nil)
(defparameter *max-log-size* 500)
(defparameter *decay-half-life* 3600)
(defparameter *min-samples-for-block* 5)

;;; ==========================================
;;; VOLATILITY TRACKING
;;; ==========================================
(defparameter *volatility-history* nil)
(defparameter *volatility-history-size* 20)
(defparameter *volatility-shift-threshold* 2.0)
(defparameter *current-volatility-state* :normal)
(defparameter *last-shift-time* 0)

;;; ==========================================
;;; CURRENCY RISK
;;; ==========================================
(defparameter *currency-risk-cache* (make-hash-table :test 'equal))
(defparameter *currency-risk-cache-time* (make-hash-table :test 'equal))

;;; ==========================================
;;; PREDICTION TRACKING
;;; ==========================================
(defparameter *prediction-history* nil)
(defparameter *prediction-accuracy* 0.0)

;;; ==========================================
;;; CATEGORY & RISK MANAGEMENT
;;; ==========================================
(defparameter *category-volatilities* (make-hash-table :test 'equal))
(defparameter *target-total-risk* 0.02)
(defparameter *trade-explanations* nil)
(defparameter *max-explanations* 50)

;;; ==========================================
;;; CLAN SYSTEM
;;; ==========================================
(defparameter *strategy-ranks* (make-hash-table :test 'equal))
(defparameter *clan-treasury* (make-hash-table :test 'eq))
(defparameter *mutual-aid-history* nil)

;;; ==========================================
;;; SWARM CONSENSUS
;;; ==========================================
(defparameter *swarm-consensus-threshold* 0.6)
(defparameter *swarm-vote-log* nil)
(defparameter *max-vote-log* 100)

;;; ==========================================
;;; LEADER SYSTEM
;;; ==========================================
(defparameter *leader-tenure* 0)
(defparameter *min-leader-tenure* 10)
(defparameter *leader-bonus-weight* 2.0)
(defparameter *leader-history* nil)

;;; ==========================================
;;; MEMORY SYSTEM
;;; ==========================================
(defparameter *episodic-memory* nil)
(defparameter *semantic-memory* nil)
(defparameter *max-episodic-memory* 1000)

;;; ==========================================
;;; V7.8+++: STRATEGY TTL & AUTO-PRUNING (Naval Critique #4)
;;; Strategies have a lifespan and get pruned if underperforming
;;; ==========================================
(defparameter *strategy-registry* (make-hash-table :test 'equal)
  "Hash table: strategy-id -> (birth-time last-used-time trades wins losses win-rate)")
(defparameter *strategy-max-ttl* 604800   ;; 7 days in seconds
  "Maximum time a strategy can live without being used")
(defparameter *strategy-min-win-rate* 0.35
  "Minimum win rate to avoid auto-pruning")
(defparameter *strategy-min-trades* 10
  "Minimum trades before win-rate is enforced")

(defun register-strategy (strategy-id)
  "Register a new strategy with TTL tracking"
  (let ((now (get-universal-time)))
    (setf (gethash strategy-id *strategy-registry*)
          (list now now 0 0 0 0.5))  ;; (birth last-used trades wins losses win-rate)
    (format t "[S] 🆕 Strategy registered: ~a (TTL: ~a days)~%"
            strategy-id (/ *strategy-max-ttl* 86400))))

(defun update-strategy-result (strategy-id won-p)
  "Update strategy win/loss tracking after a trade"
  (let ((entry (gethash strategy-id *strategy-registry*)))
    (if entry
        (let* ((birth (first entry))
               (now (get-universal-time))
               (trades (+ 1 (third entry)))
               (wins (+ (if won-p 1 0) (fourth entry)))
               (losses (+ (if won-p 0 1) (fifth entry)))
               (win-rate (if (> trades 0) (/ wins trades) 0.5)))
          (setf (gethash strategy-id *strategy-registry*)
                (list birth now trades wins losses win-rate))
          (format t "[S] 📊 Strategy ~a: ~a/~a trades, ~,1f% WR~%"
                  strategy-id wins trades (* 100 win-rate)))
        (register-strategy strategy-id))))

(defun prune-underperforming-strategies ()
  "Auto-prune strategies that are expired or underperforming. Returns count pruned."
  (let ((now (get-universal-time))
        (pruned 0))
    (maphash
     (lambda (id entry)
       (let ((birth (first entry))
             (last-used (second entry))
             (trades (third entry))
             (win-rate (sixth entry)))
         ;; Prune if: TTL expired OR (enough trades AND low win-rate)
         (when (or (> (- now last-used) *strategy-max-ttl*)
                   (and (>= trades *strategy-min-trades*)
                        (< win-rate *strategy-min-win-rate*)))
           (remhash id *strategy-registry*)
           (incf pruned)
           (format t "[S] 🗑️ Pruned strategy ~a: WR=~,1f% trades=~a age=~a days~%"
                   id (* 100 win-rate) trades 
                   (floor (- now birth) 86400)))))
     *strategy-registry*)
    (when (> pruned 0)
      (format t "[S] 🧹 Naval Critique #4: Auto-pruned ~a underperforming strategies~%" pruned))
    pruned))

(format t "[L] 📦 school-state.lisp loaded - Shared state initialized~%")
(format t "[L] 🕐 Strategy TTL system enabled (max TTL: ~a days, min WR: ~a%)~%" 
        (floor *strategy-max-ttl* 86400) (* 100 *strategy-min-win-rate*))

;;; ==========================================
;;; V7.9++: CONTEXT COMPRESSION (Graham Critique #3 - Agent Skills)
;;; Implementing techniques from .agent/workflows/context-compression.md
;;; ==========================================

(defparameter *context-max-history* 50
  "Maximum items to keep in rolling context window")

(defun compress-strategy-list (strategies)
  "Schema-based compression: Keep only essential fields from strategies.
   Reduces token count for LLM processing."
  (mapcar (lambda (s)
            (list :id (getf s :id)
                  :wr (getf s :win-rate)
                  :n (getf s :trade-count)))
          strategies))

(defun compress-trade-log (log &optional (max-entries *context-max-history*))
  "Rolling window compression: Keep only recent N trades.
   Implements age-based expiration from Agent Skills."
  (let ((sorted (sort (copy-list log) #'> :key (lambda (x) (getf x :time)))))
    (subseq sorted 0 (min max-entries (length sorted)))))

(defun summarize-market-context (ticks)
  "Hierarchical summarization: Aggregate tick data into summary stats.
   Reduces 1000s of ticks to a handful of metrics."
  (when (and ticks (> (length ticks) 0))
    (let* ((prices (mapcar (lambda (tick) (getf tick :close)) ticks))
           (high (apply #'max prices))
           (low (apply #'min prices))
           (avg (/ (apply #'+ prices) (length prices)))
           (last-price (car (last prices)))
           (volatility (/ (- high low) avg)))
      (list :high high :low low :avg avg :last last-price :vol volatility :n (length ticks)))))

(defun compress-for-llm (data data-type)
  "Entry point for context compression. Dispatches by data type.
   Implements Agent Skills integration (Graham Critique #3)."
  (case data-type
    (:strategies (compress-strategy-list data))
    (:trades (compress-trade-log data))
    (:ticks (summarize-market-context data))
    (otherwise data)))

;;; ==========================================
;;; V7.9++: MEMORY SYSTEMS (Graham Critique #3 - Agent Skills)
;;; Implementing techniques from .agent/workflows/memory-systems.md
;;; ==========================================

(defparameter *episodic-buffer* nil "Short-term episodic memory buffer")
(defparameter *semantic-rules* (make-hash-table :test 'equal) "Long-term semantic rules")
(defparameter *episodic-buffer-max* 100 "Max episodic buffer size")

(defun store-episodic (event-type data)
  "Store event in episodic memory with timestamp."
  (push (list :time (get-universal-time) :type event-type :data data) *episodic-buffer*)
  (when (> (length *episodic-buffer*) *episodic-buffer-max*)
    (setf *episodic-buffer* (butlast *episodic-buffer*))))

(defun consolidate-to-semantic (event-type threshold)
  "Consolidate repeated episodic patterns into semantic rules."
  (let* ((relevant (remove-if-not (lambda (e) (eq (getf e :type) event-type)) *episodic-buffer*))
         (count (length relevant)))
    (when (>= count threshold)
      (setf (gethash event-type *semantic-rules*) 
            (list :pattern event-type :confidence (/ count (float *episodic-buffer-max*)) :n count))
      (format t "[M] 📚 Semantic rule consolidated: ~a (confidence: ~,1f%)~%" 
              event-type (* 100 (/ count (float *episodic-buffer-max*)))))))

(defun recall-semantic (event-type)
  "Recall semantic rule if exists."
  (gethash event-type *semantic-rules*))

;;; ==========================================
;;; V7.9++: MULTI-AGENT PATTERNS (Graham Critique #3 - Agent Skills)
;;; Implementing techniques from .agent/workflows/multi-agent-patterns.md
;;; ==========================================

(defparameter *agent-blackboard* (make-hash-table :test 'equal) "Shared blackboard for agents")
(defparameter *agent-votes* nil "Voting log for consensus")

(defun agent-post (agent-id topic data)
  "Post to shared blackboard (publish-subscribe pattern)."
  (setf (gethash (cons agent-id topic) *agent-blackboard*) 
        (list :agent agent-id :topic topic :data data :time (get-universal-time))))

(defun agent-read (topic)
  "Read all posts on a topic from blackboard."
  (let ((posts nil))
    (maphash (lambda (k v) 
               (when (equal (cdr k) topic)
                 (push v posts)))
             *agent-blackboard*)
    posts))

(defun agent-vote (agent-id decision confidence)
  "Cast a vote with confidence weighting."
  (push (list :agent agent-id :decision decision :confidence confidence) *agent-votes*))

(defun tally-votes ()
  "Tally votes using confidence-weighted consensus."
  (let ((tallies (make-hash-table :test 'equal)))
    (dolist (vote *agent-votes*)
      (let ((decision (getf vote :decision))
            (conf (getf vote :confidence)))
        (incf (gethash decision tallies 0) conf)))
    (let ((winner nil) (max-score 0))
      (maphash (lambda (k v) (when (> v max-score) (setf winner k max-score v))) tallies)
      (setf *agent-votes* nil)
      (values winner max-score))))

;;; ==========================================
;;; V7.9++: AGENT SKILLS TESTS (Graham: Test everything)
;;; ==========================================

(defun test-context-compression ()
  "Unit tests for context compression functions."
  (format t "~%🧪 Running Context Compression Tests...~%")
  (let ((passed 0) (failed 0))
    ;; Test compress-strategy-list
    (let* ((strats (list (list :id "S1" :win-rate 0.6 :trade-count 10 :extra "ignored")))
           (result (compress-strategy-list strats)))
      (if (and result (= (length result) 1) (getf (car result) :id))
          (progn (incf passed) (format t "  ✅ compress-strategy-list works~%"))
          (progn (incf failed) (format t "  ❌ compress-strategy-list failed~%"))))
    ;; Test summarize-market-context
    (let* ((ticks (list (list :close 100) (list :close 110) (list :close 105)))
           (result (summarize-market-context ticks)))
      (if (and result (= (getf result :high) 110) (= (getf result :low) 100))
          (progn (incf passed) (format t "  ✅ summarize-market-context works~%"))
          (progn (incf failed) (format t "  ❌ summarize-market-context failed~%"))))
    (format t "📊 Compression Tests: ~a passed, ~a failed~%" passed failed)
    (= failed 0)))

(format t "[L] 🗜️ Context Compression enabled (Graham Agent Skills)~%")
(format t "[L] 🧠 Memory Systems enabled (Episodic + Semantic)~%")
(format t "[L] 🤝 Multi-Agent Patterns enabled (Blackboard + Voting)~%")
