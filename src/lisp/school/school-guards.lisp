;;; school-guards.lisp - Trade Guards & Safety Checks
;;; Extracted from school-execution.lisp for SRP compliance
;;; Expert Panel 2026-01-15: P12.5 Symbol Mismatch Protection

(in-package :swimmy.school)

;;; ==========================================
;;; SYMBOL MISMATCH GUARD (P12.5)
;;; ==========================================

(defun check-symbol-mismatch (strategy-name symbol)
  "V44.4: Prevent GBPUSD strategies from trading USDJPY etc.
   Returns T if there is a mismatch (should block)."
  (let ((name (string-upcase strategy-name))
        (sym (string-upcase symbol)))
    (cond
      ((search sym name) nil)  ; Name contains current symbol - OK
      ((search "USDJPY" name) (not (equal sym "USDJPY")))
      ((search "EURUSD" name) (not (equal sym "EURUSD")))
      ((search "GBPUSD" name) (not (equal sym "GBPUSD")))
      ((search "EURJPY" name) (not (equal sym "EURJPY")))
      ((search "GBPJPY" name) (not (equal sym "GBPJPY")))
      ((search "AUDUSD" name) (not (equal sym "AUDUSD")))
      (t nil))))  ; No pair in name - generic, allow

;;; ==========================================
;;; TRADING TIME SAFETY GUARD
;;; ==========================================
;; NOTE: is-safe-trading-time-p is defined in school-execution.lisp

;;; ==========================================
;;; STRATEGY COOLDOWN GUARD
;;; ==========================================

(defun check-strategy-cooldown (strategy-name symbol)
  "V44.4: Check if strategy is on cooldown for this symbol.
   Returns NIL if clear to trade, cooldown-remaining-seconds if blocked."
  (let* ((key (format nil "~a-~a" strategy-name symbol))
         (last-trade (gethash key *strategy-cooldowns*)))
    (when last-trade
      (let ((elapsed (- (get-universal-time) last-trade)))
        (when (< elapsed *strategy-symbol-cooldown-seconds*)
          (- *strategy-symbol-cooldown-seconds* elapsed))))))

(defun activate-strategy-cooldown (strategy-name symbol)
  "Activate cooldown for strategy-symbol pair after trade execution."
  (let ((key (format nil "~a-~a" strategy-name symbol)))
    (setf (gethash key *strategy-cooldowns*) (get-universal-time))))

(format t "[GUARDS] school-guards.lisp loaded - P12.5 Safety Guards Active~%")
