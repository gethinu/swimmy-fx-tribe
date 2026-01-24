;;; school-swarm-core.lisp - Project Haystack Core (Jim Simons Adaptation)
;;; ============================================================================
;;; Architecture: Swarm as a Strategy
;;; ============================================================================

(in-package :swimmy.school)

;;; ----------------------------------------------------------------------------
;;; 1. THE ATOM: PREDICTOR
;;; ----------------------------------------------------------------------------
;;; A predictor is a single, atomic signal generator (usually a small S-expression).
;;; It doesn't know about SL/TP or Lot size. It only outputs -1, 0, or 1.

(defstruct predictor
  (id (symbol-name (gensym "P")))
  logic              ; S-expression e.g. (> (ind-rsi 14 history) 70)
  (weight 1.0)       ; Vote weight (Sharpe-based or correlation-adjusted)
  (sharpe 0.0)       ; Performance metric
  (trades 0)         ; Count of signals generated
  (last-signal 0)    ; Last signal (-1: SELL, 0: HOLD, 1: BUY)
  (creation-time (get-universal-time))
  (metadata nil))

;;; ----------------------------------------------------------------------------
;;; 2. THE CONTAINER: SWARM-STRATEGY
;;; ----------------------------------------------------------------------------
;;; SwarmStrategy is a specialized Strategy that delegates decision-making
;;; to an internal ensemble of thousands of Predictors.

(defstruct (swarm-strategy (:include strategy))
  (predictors nil)           ; List of predictor structs
  (consensus-threshold 0.5)   ; Minimum normalized signal to act
  (voting-method :weighted)  ; :simple or :weighted
  (last-consensus 0.0))      ; For tracking/debugging

;;; ----------------------------------------------------------------------------
;;; 3. VOTING ENGINE
;;; ----------------------------------------------------------------------------

(defun evaluate-predictor (predictor history)
  "Evaluate a single predictor atom. Returns -1, 0, or 1."
  (let ((logic (predictor-logic predictor)))
    (handler-case
        (let ((result (eval `(let ((history ',history)) ,logic))))
          (cond
            ((eq result t) 1)
            ((eq result nil) 0)
            ((numberp result) (signum result))
            (t 0)))
      (error (e)
        (declare (ignore e))
        0))))

(defun convene-swarm-voting (swarm history)
  "Aggregate votes from all predictors in the swarm.
   Returns (values net-signal consensus-strength)."
  (let ((predictors (swarm-strategy-predictors swarm))
        (sum-signals 0.0)
        (sum-weights 0.0))
    (unless predictors
      (return-from convene-swarm-voting (values 0 0.0)))
    
    (dolist (p predictors)
      (let ((sig (evaluate-predictor p history))
            (w (predictor-weight p)))
        (incf sum-signals (* sig w))
        (incf sum-weights w)))
    
    (let* ((net-signal (if (> sum-weights 0) (/ sum-signals sum-weights) 0.0))
           (strength (abs net-signal)))
      (setf (swarm-strategy-last-consensus swarm) net-signal)
      (values (cond ((> net-signal (swarm-strategy-consensus-threshold swarm)) 1)
                    ((< net-signal (- (swarm-strategy-consensus-threshold swarm))) -1)
                    (t 0))
              strength))))

;;; ----------------------------------------------------------------------------
;;; 4. CLAN MAPPING
;;; ----------------------------------------------------------------------------
;;; The Swarm can be specialized into 'Trend Swarm', 'Reversion Swarm', etc.

(defun make-swarm (name category &key predictors threshold symbol)
  "Factory function to create a named Swarm strategy."
  (make-swarm-strategy 
   :name name
   :category category
   :symbol (or symbol "USDJPY")
   :indicators '(swarm-ensemble)
   :entry '(swarm-voting)
   :exit '(swarm-exit)
   :indicators nil ; Swarm doesn't need standard indicators
   :indicator-type "swarm"
   :predictors (or predictors nil)
   :consensus-threshold (or threshold 0.5)
   :rank :S ; Swarm starts as S-Rank because it's its own ecosystem
   :tier :battlefield))

;;; ----------------------------------------------------------------------------
;;; 5. ORCHESTRATION
;;; ----------------------------------------------------------------------------

(defun initialize-project-haystack (&key (force nil))
  "Deploy the Swarm Ensembles for all supported symbols.
   If force is T, regenerates predictors even if Swarm exists."
  (let ((symbols (or *supported-symbols* '("USDJPY" "EURUSD" "GBPUSD"))))
    (dolist (sym symbols)
      (let ((name (format nil "Swarm-~a" sym)))
        (if (and (not force) (find name *strategy-knowledge-base* :key #'strategy-name :test #'string=))
            (format t "[SWARM] ℹ️ ~a already active. Skipping.~%" name)
            (progn
              (format t "[SWARM] 🚀 Deploying Haystack Swarm for ~a...~%" sym)
              (let ((swarm (create-symbol-swarm sym)))
                ;; Remove old one if it exists
                (setf *strategy-knowledge-base* 
                      (remove name *strategy-knowledge-base* :key #'strategy-name :test #'string=))
                ;; Add new one
                (push swarm *strategy-knowledge-base*)
                (format t "[SWARM] ✅ ~a (with ~d predictors) is now live in the KB.~%" 
                        name (length (swarm-strategy-predictors swarm))))))))))

(format t "[SWARM] Core loaded - Project Haystack Orchestrator Active.~%")

