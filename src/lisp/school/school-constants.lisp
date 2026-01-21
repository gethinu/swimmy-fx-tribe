(in-package :swimmy.school)

;;; ==========================================
;;; SCHOOL CONSTANTS & PARAMETERS
;;; ==========================================

;; Execution Parameters
(defparameter *default-sl-pips* 0.15 "Default Stop Loss in pips (Scalping Standard)")
(defparameter *default-tp-pips* 0.40 "Default Take Profit in pips (Scalping Standard)")

;; Risk Parameters
(defparameter *max-spread-pips* 0.03 "Maximum allowed spread for execution")
(defparameter *min-consensus-score* 0.25 "Minimum swarm consensus to consider trading")
