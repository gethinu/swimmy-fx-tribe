;;; ============================================================================
;;; core/declarations.lisp - Forward Declarations (Fowler's Advice)
;;; ============================================================================
;;; "Declare all global variables before they're used to eliminate 
;;; undefined variable warnings at load time." - Fowler
;;; ============================================================================

(in-package :swimmy.core)

;;; ==========================================
;;; STRATEGY STATE
;;; ==========================================

(defvar *evolved-strategies* nil
  "List of evolved/generated strategies.")

(defvar *strategy-knowledge-base* nil
  "Master knowledge base of all strategies.")

(defvar *active-team* (make-hash-table :test 'equal)
  "Currently active trading team by category.")

;;; ==========================================
;;; MARKET STATE
;;; ==========================================

(defvar *current-regime* :unknown
  "Current market regime (trending-up, trending-down, ranging, volatile).")

(defvar *volatility-regime* :normal
  "Current volatility regime (low, normal, high, extreme).")

(defvar *candle-history* nil
  "Price history for primary symbol.")

(defvar *candle-histories* (make-hash-table :test 'equal)
  "Per-symbol candle histories.")

;;; ==========================================
;;; TRADING STATE
;;; ==========================================

(defvar *trading-enabled* t
  "Master switch for trading (failsafe can disable).")

(defvar *daily-trade-count* 0
  "Number of trades executed today.")

(defvar *accumulated-pnl* 0
  "Accumulated profit/loss for the session.")

(defvar *locked-treasury* 0
  "Locked profits (Graham's Margin of Safety).")

(defvar *last-regime* nil
  "Previous market regime for change detection.")

;;; ==========================================
;;; NOTIFICATION STATE
;;; ==========================================

(defvar *discord-webhook-url* nil
  "Main Discord webhook URL.")

(defvar *last-heartbeat-sent* 0
  "Timestamp of last heartbeat sent.")

;;; ==========================================
;;; RISK STATE
;;; ==========================================

(defvar *danger-level* 0
  "Current danger level (0-10).")

(defvar *consecutive-losses* 0
  "Number of consecutive losing trades.")

(defvar *risk-tolerance* :balanced
  "Risk tolerance mode (:conservative, :balanced, :aggressive).")

(defvar *daily-loss-limit* -5000
  "Maximum allowed loss for the day.")

(defvar *daily-pnl* 0.0
  "Profit/Loss for the current day.")

(defvar *current-equity* 100000.0
  "Current account equity.")

(defvar *peak-equity* 100000.0
  "Peak equity for drawdown calculation.")

(defvar *max-drawdown* 0.0
  "Maximum drawdown experienced (absolute).")

(defvar *max-dd-percent* 5.0
  "Maximum drawdown tolerance in percent.")

(defvar *base-lot-size* 0.01
  "Base lot size for trades.")

(defvar *success-count* 0
  "Total number of winning trades.")

(defvar *total-trades* 0
  "Total number of trades executed.")

;;; ==========================================
;;; AI/ML STATE
;;; ==========================================

(defvar *last-prediction* nil
  "Last prediction from neural network.")

(defvar *last-confidence* 0.5
  "Confidence of last prediction.")

(defvar *last-swarm-consensus* 0.5
  "Last swarm intelligence consensus.")

(defvar *tribe-direction* nil
  "Current tribe consensus direction.")

(defvar *tribe-consensus* 0.0
  "Strength of tribe consensus.")

;;; ==========================================
;;; COMMUNICATION STATE
;;; ==========================================

(defvar *publisher* nil
  "ZMQ publisher socket.")

(defvar *subscriber* nil
  "ZMQ subscriber socket.")

(defvar *cmd-publisher* nil
  "ZMQ command publisher socket.")

;;; ==========================================
;;; LEARNING STATE
;;; ==========================================

(defvar *learned-patterns* nil
  "Patterns learned from trading history.")

(defvar *improvement-requests* nil
  "Queue of improvement requests.")

(format t "[DECLARATIONS] Forward declarations loaded~%")
