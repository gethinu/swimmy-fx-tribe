(in-package :swimmy.school)

;;; ============================================================================
;;; Legend 61 Revival (Commit 3f58795 V3.0 "61-STRATEGY SIGNAL SYSTEM")
;;; - Source definitions live in ./strategies_v3.lisp
;;; - We re-load only the 61 canonical strategies and re-register them as LEGEND.
;;; - Child/offspring are intentionally NOT restored (user intent).
;;; ============================================================================

(defparameter *legend-61-names*
  '("Golden-Cross-50-200"
    "Death-Cross-50-200"
    "Fast-EMA-Cross-9-21"
    "Medium-EMA-Cross-20-50"
    "Silver-Cross-20-100"
    "Scalp-Cross-5-12"
    "TF-5-20"
    "TF-10-50"
    "Triple-EMA-Trend-Follow"
    "Perfect-Order-SMA"
    "Fibonacci-EMA-Scalp"
    "Trend-Pullback-Entry"
    "Conservative-Trend"
    "MACD-Zero-Cross-Long"
    "MACD-Signal-Cross"
    "MACD-Above-Zero-Cross"
    "MACD-Expansion"
    "RSI-Momentum-Break"
    "RSI-Bull-Zone"
    "RSI-Fast-Break"
    "RSI-Trend-Shift"
    "MACD-RSI-Confluence"
    "Elder-Impulse-Simulated"
    "Momentum-Burst"
    "RSI-Oversold-Reversal"
    "RSI-Overbought-Reversal"
    "RSI-Short-Reversion"
    "RSI-2-Period-Connors"
    "BB-Lower-Bounce"
    "BB-Upper-Rejection"
    "BB-RSI-Reversion-Combo"
    "Extreme-Reversion-BB"
    "Stoch-Oversold-Entry"
    "Stoch-Overbought-Entry"
    "Stoch-Extreme-Dip"
    "BB-Breakout-Upper"
    "BB-Breakout-Lower"
    "BB-Squeeze-Expansion"
    "Bollinger-Band-Walk"
    "Volatility-Trend-Follow"
    "ATR-Confirmed-Breakout"
    "Session-Breakout-Proxy"
    "Low-Vol-Breakout"
    "Puria-Method-Proxy"
    "Holy-Grail-Proxy"
    "Triple-Screen-Proxy"
    "Trend-Scalp-1M"
    "MACD-Zero-Reject"
    "Crossover-Plus-MACD"
    "Simple-Momentum-Sync"
    "RSI-Stoch-Reversal"
    "Aggressive-Reversal"
    "Double-Bollinger-Trend"
    "MA-Ribbon-Scalp"
    "Stoch-Momentum-Cross"
    "Stoch-Pop"
    "RSI-Volatility-Break"
    "Pullback-Breakout"
    "Bladerunner"
    "Sweet-Chariot-SMA-40"
    "CCI-Trend-Breakout"))

(defun %walk-and-collect (form names)
  "Recursively collect (make-strategy ...) forms inside FORM that match NAMES."
  (cond
    ((and (listp form) (eq (first form) 'make-strategy))
     (let* ((plist form)
            (name (second (member :name plist :test #'eq))))
       (when (and name (member name names :test #'string=))
         (list (eval form)))))
    ((consp form)
     (mapcan (lambda (x) (%walk-and-collect x names)) form))
    (t nil)))

(defun %read-make-strategy-forms (path names)
  "Parse make-strategy forms (including nested) from PATH and return matches."
  (let ((results '()))
    (with-open-file (s path)
      (loop for form = (ignore-errors (read s nil :eof))
            until (eq form :eof) do
              (setf results (nconc results (%walk-and-collect form names)))))
    results))

(defun restore-legend-61 ()
  "Re-register the 61 signal-system strategies as LEGEND (immutables).
   - Uses definitions from strategies_v3.lisp
   - Protects existing LEGENDs from demotion
   - Does not restore offspring (Bred-Legen-*)"
  (format t "[LEGENDS-61] 🔍 Loading definitions from strategies_v3.lisp...~%")
  (let* ((path (swimmy.core::swimmy-path "strategies_v3.lisp"))
         (defs (%read-make-strategy-forms path *legend-61-names*))
         (count-added 0)
         (count-updated 0)
         (*startup-mode* t)) ;; bypass graveyard pattern gate for restoration
    (dolist (s defs)
      (setf (strategy-rank s) :legend)
      (when (slot-exists-p s 'generation) (setf (strategy-generation s) 0))
      (when (slot-exists-p s 'immortal) (setf (strategy-immortal s) t))
      (let* ((name (strategy-name s))
             (existing (find name *strategy-knowledge-base* :key #'strategy-name :test #'string=)))
        (if existing
            (progn
              (ensure-rank existing :legend "Legend-61 restore")
              (ignore-errors (swimmy.persistence:move-strategy existing :legend :force t))
              (incf count-updated))
            (when (add-to-kb s :founder :require-bt nil :notify nil)
              (ensure-rank s :legend "Legend-61 restore")
              (ignore-errors (swimmy.persistence:save-strategy s))
              (incf count-added)))))
    (format t "[LEGENDS-61] ✅ Restoration complete. Added: ~d | Updated: ~d~%"
            count-added count-updated)))
