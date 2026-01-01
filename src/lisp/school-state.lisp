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
(defvar *monthly-goal* 100000)
(defvar *category-trades* 0)

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

(format t "[L] 📦 school-state.lisp loaded - Shared state initialized~%")
