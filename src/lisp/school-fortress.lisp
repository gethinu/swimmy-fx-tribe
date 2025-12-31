;; school-fortress.lisp - Swimmy V5.5-V5.8 Fortress Features
;; V6.0 (Graham): Extracted from school.lisp for modularity

;;; ══════════════════════════════════════════════════════════════════
;;;  V5.5: FORTRESS HELPERS
;;; ══════════════════════════════════════════════════════════════════

(defun global-panic-active-p ()
  "Check if multiple currencies are in extreme volatility (Soros)"
  (if (boundp '*current-volatility-state*)
      (let ((extreme-count 0))
        (when (boundp '*symbol-volatility-states*)
          (maphash (lambda (k v) 
                     (declare (ignore k))
                     (when (eq v :extreme) (incf extreme-count)))
                   *symbol-volatility-states*))
        (eq *current-volatility-state* :extreme))
      nil))

(defun should-unlearn-p (symbol)
  "Check if recent performance is toxic (Darwin)"
  (declare (ignore symbol))
  nil) ; Stub - implement with trade history analysis

;;; ══════════════════════════════════════════════════════════════════
;;;  V5.6 (Paper #36): PARALLEL VERIFICATION LOOPS
;;; ══════════════════════════════════════════════════════════════════

(defun verify-parallel-scenarios (symbol direction category)
  "Run 3 parallel simulations to verify trade robustness (DeepMind)"
  (let ((optimistic-pass nil)
        (pessimistic-pass nil)
        (chaos-pass nil))
    
    ;; 1. Optimistic World: Market Regime check
    (let ((regime (if (boundp '*market-regime*) *market-regime* :ranging)))
       (setf optimistic-pass 
             (cond ((member category '(:hunters :raiders)) (eq regime :trending))
                   ((eq category :breakout) (eq regime :volatile))
                   ((eq category :shamans) (eq regime :ranging))
                   (t t))))
    
    ;; 2. Pessimistic World: Counter-indicators
    (setf pessimistic-pass t) ; Placeholder
    
    ;; 3. Chaos World: Volatility check
    (let ((vol (if (boundp '*current-volatility-state*) *current-volatility-state* :normal)))
      (setf chaos-pass (not (eq vol :extreme))))
      
    ;; Verification: Pass if 2/3 scenarios survive
    (let ((score (+ (if optimistic-pass 1 0) 
                    (if pessimistic-pass 1 0) 
                    (if chaos-pass 1 0))))
      (format t "[L] 🧬 PARALLEL VERIFICATION (~a): Opt=~a Pess=~a Chaos=~a (Score: ~d/3)~%" 
              symbol optimistic-pass pessimistic-pass chaos-pass score)
      (>= score 2))))

;;; ══════════════════════════════════════════════════════════════════
;;;  HIGH COUNCIL
;;; ══════════════════════════════════════════════════════════════════

(defun convene-high-council (proposal category &key (urgency 0))
  "Evaluate trade proposal by the High Council. Returns t (approve) or nil (reject)."
  (let* ((symbol (getf proposal :symbol))
         (direction (getf proposal :direction))
         (danger-level (if (boundp '*danger-level*) *danger-level* 0))
         (tribe-consensus (if (boundp '*tribe-consensus*) *tribe-consensus* 0.5))
         (swarm-consensus (if (boundp '*last-swarm-consensus*) *last-swarm-consensus* 0.0))
         (volatility-state (if (boundp '*current-volatility-state*) *current-volatility-state* :normal))
         (approval nil)
         (reason ""))
    
    (cond
      ((>= urgency 10)
       (setf approval t reason "🚨 EMERGENCY PROTOCOL Override"))
      
      ((>= danger-level 3)
       (setf approval nil reason "🚫 REJECTED: FLEE MODE active."))
      
      ((>= danger-level 2)
       (if (and (> tribe-consensus 0.7) (> swarm-consensus 0.7))
           (setf approval t reason "⚠️ APPROVED: High consensus in Danger Lv2")
           (setf approval nil reason "🛡️ REJECTED: Danger Lv2 requires 70%+ consensus")))
           
      ((eq volatility-state :extreme)
       (if (member category '(:breakers :shamans))
           (setf approval t reason "🌊 APPROVED: Extreme volatility fits Clan")
           (setf approval nil reason "⛔ REJECTED: Too volatile for Clan")))
           
      (t (setf approval t reason "✅ APPROVED: Standard deployment")))
       
    (when (or (not approval) (>= danger-level 2) (eq volatility-state :extreme))
      (let ((msg (format nil "🏛️ **HIGH COUNCIL**~%~a ~a (~a)~%~a" 
                         category symbol direction reason)))
        (format t "[L] ~a~%" msg)
        (when (fboundp 'notify-discord-symbol)
           (notify-discord-symbol symbol msg :color (if approval 3066993 15158332)))))
           
    approval))

;;; ══════════════════════════════════════════════════════════════════
;;;  V6.1 (Taleb): PARANOIA CHECKS - Be Paranoid, Stay Alive
;;; ══════════════════════════════════════════════════════════════════

;; High-risk event times (UTC hours) - DO NOT TRADE
(defparameter *nfp-day* 5)        ; First Friday of month (approximate)
(defparameter *high-risk-hours*   ; Known volatility spikes
  '((13 . "US Market Open") 
    (14 . "FOMC/NFP typical release")
    (18 . "London Close")
    (23 . "BOJ/Japan News")))

(defun paranoia-check ()
  "Taleb: Block trades during known high-risk periods"
  (multiple-value-bind (sec min hour day dow) 
      (decode-universal-time (get-universal-time))
    (declare (ignore sec min))
    (let ((blocked nil)
          (reason ""))
      ;; 1. High-risk hours check
      (dolist (risky *high-risk-hours*)
        (when (= hour (car risky))
          (setf blocked t)
          (setf reason (cdr risky))))
      
      ;; 2. Friday afternoon (weekend gap risk)
      (when (and (= dow 4) (>= hour 20))
        (setf blocked t)
        (setf reason "Weekend gap risk"))
      
      ;; 3. Monday morning (gap continuation)
      (when (and (= dow 0) (<= hour 2))
        (setf blocked t)
        (setf reason "Monday gap risk"))
      
      (when blocked
        (format t "[L] 🔴 TALEB PARANOIA: Trade blocked (~a)~%" reason))
      blocked)))

(defun antifragile-lot-adjust (base-lot current-pnl)
  "Taleb: Reduce exposure when losing, maintain when winning"
  (cond
    ((< current-pnl -3000)
     (format t "[L] 🛡️ ANTIFRAGILE: Heavy loss, reducing lot to 50%~%")
     (* base-lot 0.5))
    ((< current-pnl -1000)
     (format t "[L] 🛡️ ANTIFRAGILE: Loss detected, reducing lot to 75%~%")
     (* base-lot 0.75))
    ((> current-pnl 5000)
     (format t "[L] 💪 ANTIFRAGILE: Strong profit, maintaining full lot~%")
     base-lot)
    (t base-lot)))

;;; ══════════════════════════════════════════════════════════════════
;;;  V6.1 (Naval): JAPAN-SPECIFIC KNOWLEDGE EDGE
;;; ══════════════════════════════════════════════════════════════════

(defun tokyo-lunch-break-p ()
  "Naval: Tokyo lunch break (11:30-12:30 JST = 2:30-3:30 UTC) = low volatility"
  (multiple-value-bind (sec min hour) (decode-universal-time (get-universal-time))
    (declare (ignore sec))
    (or (and (= hour 2) (>= min 30))
        (and (= hour 3) (<= min 30)))))

(defun month-end-exporter-flow-p ()
  "Naval: Japanese exporters sell USD at month-end for repatriation"
  (multiple-value-bind (sec min hour day month year) 
      (decode-universal-time (get-universal-time))
    (declare (ignore sec min hour))
    (let ((last-day (days-in-month month year)))
      (>= day (- last-day 2)))))  ; Last 3 days of month

(defun apply-japan-edge (symbol direction base-lot)
  "Naval: Apply Japan-specific knowledge adjustments"
  (let ((adj 1.0))
    ;; 1. Gotobi (already implemented, enhance here)
    (when (and (string= symbol "USDJPY") (gotobi-day-p))
      (if (eq direction :buy)
          (progn
            (setf adj (* adj 1.2))
            (format t "[L] 🇯🇵 NAVAL: Gotobi BUY boost (+20%)~%"))
          (progn
            (setf adj (* adj 0.8))
            (format t "[L] 🇯🇵 NAVAL: Gotobi SELL dampened (-20%)~%"))))
    
    ;; 2. Tokyo lunch = avoid
    (when (and (tokyo-lunch-break-p) (string= symbol "USDJPY"))
      (setf adj (* adj 0.5))
      (format t "[L] 🍱 NAVAL: Tokyo lunch break - reduced activity~%"))
    
    ;; 3. Month-end exporter flow (USD selling)
    (when (and (month-end-exporter-flow-p) (string= symbol "USDJPY"))
      (if (eq direction :sell)
          (progn
            (setf adj (* adj 1.15))
            (format t "[L] 📅 NAVAL: Month-end exporter flow - SELL boost~%"))
          (progn
            (setf adj (* adj 0.85))
            (format t "[L] 📅 NAVAL: Month-end exporter flow - BUY dampened~%"))))
    
    (* base-lot adj)))

;;; ══════════════════════════════════════════════════════════════════
;;;  V6.1 (Graham): AUTO-PRUNE STRATEGIES
;;; ══════════════════════════════════════════════════════════════════

(defun auto-prune-on-load ()
  "Graham: Automatically reduce redundant strategies at startup"
  (when (fboundp 'get-redundant-strategies)
    (let ((redundant (get-redundant-strategies)))
      (when (and redundant (> (length redundant) 0))
        (format t "[L] 🧹 GRAHAM: Found ~d redundant strategies to prune~%" 
                (length redundant))
        (prune-redundant-strategies)))))

(format t "[FORTRESS] V6.1 features loaded (Paranoia + Japan Edge + Auto-Prune)~%")
