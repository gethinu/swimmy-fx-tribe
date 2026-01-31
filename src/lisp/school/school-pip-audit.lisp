;;; src/lisp/school/school-pip-audit.lisp
;;; ============================================================================
;;; P4/P5: PIP DESIGN AUDIT + LIVE TRADE MONITOR (V47.5)
;;; ============================================================================
;;; Implements pip-based strategy health checks per GPT recommendations.
;;; V47.5: Added live trade monitoring with S-RANK demotion logic.
;;;
;;; Components:
;;; 1. Average Pips Calculation - Core health metric
;;; 2. SL/TP Ratio Analysis - Expected value calculation
;;; 3. GPT Filter Warnings - Quality alerts
;;; 4. Live Trade Monitor - Periodic audit with demotion
;;; ============================================================================

(in-package :swimmy.school)

;;; ---------------------------------------------------------------------------
;;; CONSTANTS (GPT Criteria)
;;; ---------------------------------------------------------------------------

(defparameter *min-avg-pips* 10.0
  "Minimum average profit in pips. Below this = no edge (GPT criterion).")

(defparameter *min-pf-threshold* 1.2
  "Minimum Profit Factor for serious consideration.")

(defparameter *min-sharpe-threshold* 0.3
  "Minimum Sharpe to avoid noise territory.")

(defparameter *live-audit-interval* 20
  "Number of trades before live audit is triggered.")

;;; V47.5: Currency-specific pip values (per 1 standard lot)
(defparameter *pip-values-by-symbol*
  '(("EURUSD" . 10.0)
    ("GBPUSD" . 10.0)
    ("USDJPY" . 9.1)     ; Approx at 110 rate
    ("AUDUSD" . 10.0)
    ("USDCHF" . 10.8)
    ("USDCAD" . 7.5)
    ("NZDUSD" . 10.0)
    ("XAUUSD" . 1.0))    ; Gold: 1 pip = $1 per 0.01 lot
  "Pip value per 1 standard lot by symbol.")

(defun get-pip-value (symbol)
  "Get pip value for a symbol. V47.5: Currency-specific."
  (or (cdr (assoc symbol *pip-values-by-symbol* :test #'string=))
      10.0))  ; Default

;;; ---------------------------------------------------------------------------
;;; AVERAGE PIPS CALCULATION
;;; ---------------------------------------------------------------------------

(defun calculate-avg-pips (strategy)
  "Calculate average profit in pips from strategy SL/TP and win rate.
   V47.4: Core pip design health metric."
  (let* ((sl (or (strategy-sl strategy) 50))
         (tp (or (strategy-tp strategy) 50))
         (wr (or (strategy-win-rate strategy) 0.5)))
    ;; Expected pips per trade = (WR × TP) - ((1 - WR) × SL)
    (- (* wr tp) (* (- 1 wr) sl))))

(defun calculate-avg-pips-from-history (pnl-history lot-size symbol)
  "Calculate average pips from actual PNL history.
   V47.5: Uses currency-specific pip value."
  (when (and pnl-history (> (length pnl-history) 0) (> lot-size 0))
    (let* ((avg-pnl (/ (reduce #'+ pnl-history) (length pnl-history)))
           (pip-value (* (get-pip-value symbol) lot-size)))
      (if (> pip-value 0)
          (/ avg-pnl pip-value)
          0.0))))

;;; ---------------------------------------------------------------------------
;;; SL/TP RATIO ANALYSIS
;;; ---------------------------------------------------------------------------

(defun analyze-sl-tp-ratio (strategy)
  "Analyze SL/TP ratio and expected value.
   Returns plist with :ratio :expected-pips :verdict"
  (let* ((sl (or (strategy-sl strategy) 50))
         (tp (or (strategy-tp strategy) 50))
         (wr (or (strategy-win-rate strategy) 0.5))
         (ratio (if (> sl 0) (/ tp sl) 0))
         (expected-pips (calculate-avg-pips strategy))
         (verdict (cond
                    ((< expected-pips 0) :negative-edge)
                    ((< expected-pips 5) :marginal)
                    ((< expected-pips 10) :weak)
                    ((< expected-pips 20) :acceptable)
                    (t :strong))))
    (list :sl sl
          :tp tp
          :wr wr
          :ratio ratio
          :expected-pips expected-pips
          :verdict verdict)))

(defun calculate-required-wr (sl tp)
  "Calculate minimum win rate needed for break-even.
   WR_min = SL / (SL + TP)"
  (if (> (+ sl tp) 0)
      (/ sl (+ sl tp))
      0.5))

;;; ---------------------------------------------------------------------------
;;; GPT FILTER WARNINGS
;;; ---------------------------------------------------------------------------

(defun check-pip-design-health (strategy)
  "Check strategy against GPT pip design criteria.
   Returns list of warnings."
  (let ((warnings nil)
        (analysis (analyze-sl-tp-ratio strategy))
        (sharpe (or (strategy-sharpe strategy) 0))
        (pf (or (strategy-profit-factor strategy) 0)))
    
    ;; Check expected pips
    (when (< (getf analysis :expected-pips) *min-avg-pips*)
      (push (format nil "⚠️ Expected pips ~,1f < ~,0f (GPT minimum)"
                    (getf analysis :expected-pips) *min-avg-pips*)
            warnings))
    
    ;; Check PF
    (when (and (> pf 0) (< pf *min-pf-threshold*))
      (push (format nil "⚠️ PF ~,2f < ~,1f (marginal edge)"
                    pf *min-pf-threshold*)
            warnings))
    
    ;; Check Sharpe (noise threshold)
    (when (< sharpe *min-sharpe-threshold*)
      (push (format nil "⚠️ Sharpe ~,2f < ~,1f (noise territory)"
                    sharpe *min-sharpe-threshold*)
            warnings))
    
    ;; Check TP/SL ratio vs win rate
    (let* ((required-wr (calculate-required-wr 
                         (getf analysis :sl) 
                         (getf analysis :tp)))
           (actual-wr (getf analysis :wr)))
      (when (< actual-wr required-wr)
        (push (format nil "⚠️ WR ~,1f% < required ~,1f% for break-even"
                      (* 100 actual-wr) (* 100 required-wr))
              warnings)))
    
    warnings))

(defun audit-strategy-pip-design (strategy)
  "Full pip design audit for a strategy. Prints report."
  (let* ((name (strategy-name strategy))
         (analysis (analyze-sl-tp-ratio strategy))
         (warnings (check-pip-design-health strategy)))
    (format t "~%═══════════════════════════════════════~%")
    (format t "📊 PIP AUDIT: ~a~%" name)
    (format t "═══════════════════════════════════════~%")
    (format t "  SL: ~d pips | TP: ~d pips | Ratio: ~,2f~%"
            (getf analysis :sl) (getf analysis :tp) (getf analysis :ratio))
    (format t "  WR: ~,1f% | Expected: ~,1f pips/trade~%"
            (* 100 (getf analysis :wr)) (getf analysis :expected-pips))
    (format t "  Verdict: ~a~%" (getf analysis :verdict))
    
    (if warnings
        (progn
          (format t "~%  ⚠️ WARNINGS:~%")
          (dolist (w warnings)
            (format t "    ~a~%" w)))
        (format t "~%  ✅ No warnings~%"))
    
    (format t "═══════════════════════════════════════~%")
    (values analysis warnings)))

(defun audit-all-strategies ()
  "Audit all strategies in knowledge base for pip design health."
  (let ((total 0) (healthy 0) (warned 0))
    (dolist (strategy *strategy-knowledge-base*)
      (incf total)
      (let ((warnings (check-pip-design-health strategy)))
        (if warnings
            (incf warned)
            (incf healthy))))
    (format t "~%📊 PIP AUDIT SUMMARY~%")
    (format t "  Total: ~d | Healthy: ~d | Warned: ~d~%"
            total healthy warned)
    (format t "  Health Rate: ~,1f%~%" 
            (if (> total 0) (* 100 (/ healthy total)) 0))
    (list :total total :healthy healthy :warned warned)))

;;; ---------------------------------------------------------------------------
;;; V47.5: LIVE TRADE MONITORING (S-RANK Demotion)
;;; ---------------------------------------------------------------------------

(defparameter *strategy-trade-counts* (make-hash-table :test 'equal)
  "Track trade counts per strategy for live audit intervals.")

(defun get-strategy-trade-count (strategy-name)
  "Get cumulative trade count for a strategy."
  (gethash strategy-name *strategy-trade-counts* 0))

(defun increment-strategy-trade-count (strategy-name)
  "Increment trade count for a strategy."
  (incf (gethash strategy-name *strategy-trade-counts* 0)))

(defun live-trade-audit-needed-p (strategy)
  "Check if strategy needs live audit based on trade count."
  (let ((count (get-strategy-trade-count (strategy-name strategy))))
    (and (> count 0)
         (zerop (mod count *live-audit-interval*)))))

(defun calculate-live-metrics (strategy)
  "Calculate current live metrics for a strategy.
   Returns plist with :sharpe :pf :avg-pips :wr"
  (let* ((history (strategy-pnl-history strategy))
         (count (length history)))
    (if (< count 5)
        (list :sharpe 0 :pf 0 :avg-pips 0 :wr 0)
        (let* ((wins (count-if #'plusp history))
               (wr (/ wins (max 1 count)))
               (avg-pnl (/ (reduce #'+ history) count))
               (mean avg-pnl)
               (sq-diffs (mapcar (lambda (x) (expt (- x mean) 2)) history))
               (variance (/ (reduce #'+ sq-diffs) count))
               (std-dev (sqrt (max 0.000001 variance)))
               (sharpe (if (> std-dev 0) (/ mean std-dev) 0))
               (total-wins (reduce #'+ (remove-if-not #'plusp history)))
               (total-losses (abs (reduce #'+ (remove-if-not #'minusp history))))
               (pf (if (> total-losses 0) (/ total-wins total-losses) 0))
               (avg-pips (calculate-avg-pips-from-history 
                          history 0.01 (or (strategy-symbol strategy) "EURUSD"))))
          (list :sharpe sharpe :pf pf :avg-pips (or avg-pips 0) :wr wr)))))

(defun determine-live-audit-action (strategy)
  "Determine action for S-RANK strategy based on live performance.
   Returns :keep, :demote-to-a, :demote-to-breeding, :demote-to-graveyard"
  (let* ((metrics (calculate-live-metrics strategy))
         (sharpe (getf metrics :sharpe))
         (pf (getf metrics :pf))
         (avg-pips (getf metrics :avg-pips))
         (violations 0))
    
    ;; Count GPT criteria violations
    (when (< avg-pips *min-avg-pips*) (incf violations))
    (when (< pf *min-pf-threshold*) (incf violations))
    (when (< sharpe *min-sharpe-threshold*) (incf violations))
    
    (cond
      ;; All criteria met - keep S-RANK
      ((zerop violations) :keep)
      ;; 1 violation - demote to A-RANK (probation)
      ((= violations 1) :demote-to-a)
      ;; 2 violations - downgrade to breeding stock (B-RANK)
      ((= violations 2) :demote-to-breeding)
      ;; 3 violations - all criteria failed - graveyard
      (t :demote-to-graveyard))))

(defun run-live-trade-audit (strategy trade-result)
  "Run after each S-RANK trade. Check if demotion needed.
   V47.5: Owner's Vision - S-RANK strategies are not permanent."
  (declare (ignore trade-result))
  (when (eq (strategy-rank strategy) :S)
    (increment-strategy-trade-count (strategy-name strategy))
    
    (when (live-trade-audit-needed-p strategy)
      (let* ((action (determine-live-audit-action strategy))
             (metrics (calculate-live-metrics strategy)))
        (format t "~%🔍 LIVE AUDIT: ~a (after ~d trades)~%"
                (strategy-name strategy)
                (get-strategy-trade-count (strategy-name strategy)))
        (format t "   Sharpe: ~,2f | PF: ~,2f | AvgPips: ~,1f~%"
                (getf metrics :sharpe) (getf metrics :pf) (getf metrics :avg-pips))
        
        (case action
          (:keep
           (format t "   ✅ KEEP S-RANK - All criteria met~%"))
          
          (:demote-to-a
           (ensure-rank strategy :A "Live Audit: 1 violation")
           (format t "   ⚠️ DEMOTED TO A-RANK - 1 violation (probation)~%"))
          
          (:demote-to-breeding
           (ensure-rank strategy :B "Live Audit: 2 violations")
           (format t "   ⚠️ DEMOTED TO B-RANK - 2 violations (breeding stock)~%"))
          
          (:demote-to-graveyard
           (ensure-rank strategy :graveyard "Live Audit: Critical failure")
           (format t "   💀 DEMOTED TO GRAVEYARD - All criteria failed~%")
           (save-failure-pattern strategy)))
        
        action))))

(format t "[P4/P5] 📊 V47.5 Pip Audit + Live Monitor Loaded~%")
