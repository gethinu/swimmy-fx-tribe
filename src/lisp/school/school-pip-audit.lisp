;;; src/lisp/school/school-pip-audit.lisp
;;; ============================================================================
;;; P4: PIP DESIGN AUDIT (V47.4 - GPT/Expert Panel 2026-01-21)
;;; ============================================================================
;;; Implements pip-based strategy health checks per GPT recommendations.
;;;
;;; Components:
;;; 1. Average Pips Calculation - Core health metric
;;; 2. SL/TP Ratio Analysis - Expected value calculation
;;; 3. GPT Filter Warnings - Quality alerts
;;; ============================================================================

(in-package :swimmy.school)

;;; ---------------------------------------------------------------------------
;;; CONSTANTS (GPT Criteria)
;;; ---------------------------------------------------------------------------

(defparameter *min-avg-pips* 10.0
  "Minimum average profit in pips. Below this = no edge (GPT criterion).")

(defparameter *min-pf-threshold* 1.2
  "Minimum Profit Factor for serious consideration.")

(defparameter *pip-value-per-lot* 10.0
  "Approximate pip value per standard lot in USD.")

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

(defun calculate-avg-pips-from-history (pnl-history lot-size)
  "Calculate average pips from actual PNL history.
   Assumes pip value = $10 per lot."
  (when (and pnl-history (> (length pnl-history) 0) (> lot-size 0))
    (let* ((avg-pnl (/ (reduce #'+ pnl-history) (length pnl-history)))
           (pip-value (* *pip-value-per-lot* lot-size)))
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
    (when (< sharpe 0.3)
      (push (format nil "⚠️ Sharpe ~,2f < 0.3 (noise territory)"
                    sharpe)
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

(format t "[P4] 📊 V47.4 Pip Design Audit Loaded~%")
