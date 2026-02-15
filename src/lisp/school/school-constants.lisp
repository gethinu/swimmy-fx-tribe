(in-package :swimmy.school)

;;; ==========================================
;;; SCHOOL CONSTANTS & PARAMETERS (V50.2)
;;; ==========================================

;;; 1. UNIVERSE DEFINITION (Simons Scope)
(defparameter *timeframes* '("M5" "M15" "M30" "H1" "H4" "D1" "W1" "MN") "Multi-Timeframe Scope")
(defparameter *symbols* '("USDJPY" "EURUSD" "GBPUSD") "Primary Currency Pairs")

;;; 2. BACKTEST RANGE (Screening)
;;; Phase 1 (Screening): Learn from history (2011-2020)
(defparameter *backtest-range-1* '(:start "2011.01.01" :end "2020.12.31") "Screening Range (10y)")

;;; 3. EVOLUTION PARAMETERS (Musk Optimization)
(defparameter *b-rank-pool-size* 20 "Max strategies per pool. Speed is survival.")
(defparameter *s-rank-slots* 3 "Elite slots per category (Best of Best)")

;;; 3.1 SELECTION SCORING (Sharpe + PF + WR + MaxDD)
(defparameter *selection-score-sharpe-weight* 0.4)
(defparameter *selection-score-pf-weight* 0.25)
(defparameter *selection-score-wr-weight* 0.2)
(defparameter *selection-score-maxdd-weight* 0.15)

(defun strategy-selection-score (s)
  "Composite score for selection/voting: Sharpe + PF + WR + (1-MaxDD)."
  (let* ((sh (or (strategy-sharpe s) 0.0))
         (pf (or (strategy-profit-factor s) 0.0))
         (wr (or (strategy-win-rate s) 0.0))
         (dd (or (strategy-max-dd s) 1.0)))
    (+ (* *selection-score-sharpe-weight* sh)
       (* *selection-score-pf-weight* pf)
       (* *selection-score-wr-weight* wr)
       (* *selection-score-maxdd-weight* (- 1.0 dd)))))

;;; 4. EXECUTION PARAMETERS
(defparameter *default-sl-pips* 0.15 "Default SL (15 pips)")
(defparameter *default-tp-pips* 0.40 "Default TP (40 pips)")
(defparameter *max-spread-pips* 3.0 "Max Spread (pips)")

;;; 4.1 EXECUTION COST HELPERS
(defparameter *pip-size-by-symbol*
  '(("USDJPY" . 0.01)
    ("EURUSD" . 0.0001)
    ("GBPUSD" . 0.0001))
  "Pip size by symbol (price units).")

(defun get-pip-size (symbol)
  "Return pip size for a symbol. Defaults to JPY=0.01, others=0.0001."
  (let* ((sym (if symbol (string-upcase symbol) ""))
         (mapped (cdr (assoc sym *pip-size-by-symbol* :test #'string=))))
    (or mapped (if (search "JPY" sym) 0.01 0.0001))))

(defun spread-pips-from-bid-ask (symbol bid ask)
  "Convert bid/ask spread into pips for a symbol."
  (let ((pip-size (get-pip-size symbol)))
    (if (and (numberp bid) (numberp ask) (> pip-size 0))
        (/ (- ask bid) pip-size)
        0.0)))

(defun slippage-pips-from-fill (symbol direction expected-bid expected-ask fill-price)
  "Calculate signed slippage in pips (positive = worse fill)."
  (let* ((pip-size (get-pip-size symbol))
         (dir (cond ((keywordp direction) direction)
                    ((symbolp direction)
                     (intern (string-upcase (symbol-name direction)) :keyword))
                    ((stringp direction)
                     (cond ((search "BUY" (string-upcase direction)) :buy)
                           ((search "SELL" (string-upcase direction)) :sell)
                           (t :unknown)))
                    (t :unknown)))
         (expected (cond ((eq dir :buy) expected-ask)
                         ((eq dir :sell) expected-bid)
                         (t nil)))
         (sign (if (eq dir :sell) -1 1)))
    (when (and (numberp expected) (numberp fill-price) (> pip-size 0))
      (/ (* sign (- fill-price expected)) pip-size))))

(defun calculate-cost-pips (symbol bid ask &key (slippage-pips 0.0) (commission-pips 0.0) (swap-pips 0.0))
  "Return total execution cost in pips: spread + slippage + commission + swap."
  (+ (float (spread-pips-from-bid-ask symbol bid ask) 1.0)
     (if (numberp slippage-pips) (float slippage-pips 1.0) 0.0)
     (if (numberp commission-pips) (float commission-pips 1.0) 0.0)
     (if (numberp swap-pips) (float swap-pips 1.0) 0.0)))

;;; 5. SCREENING THRESHOLDS
(defparameter *phase1-min-sharpe* 0.1 "Minimum Sharpe for Phase 1 (Screening)")
