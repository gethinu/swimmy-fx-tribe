;;; src/lisp/core/evaluator.lisp
;;; EVALUATOR AI (Multi-Agent Debate)
;;; ==========================================
;;; A second "AI" that reviews proposals before execution
;;; Inspired by: 2025 Multi-Agent systems

(in-package :swimmy.school)

(defparameter *evaluator-enabled* t)
(defparameter *debate-log* nil)

(defstruct debate-entry
  timestamp
  proposal-type  ; :trade, :parameter-change, :strategy-switch
  proposer       ; Who proposed (e.g., "Leader-Fish", "Swarm")
  proposal       ; The actual proposal
  evaluator-verdict  ; :approve, :reject, :modify
  evaluator-reason
  final-action)

(defun evaluator-check-trade (direction symbol confidence)
  "Evaluator AI reviews trade proposal"
  (when *evaluator-enabled*
    (let ((issues nil)
          (verdict :approve)
          (reason "All checks passed"))
      
      ;; Check 1: Volatility
      (when (eq *current-volatility-state* :extreme)
        (push "⚠️ EXTREME volatility - high risk" issues)
        (setf verdict :reject))
      
      ;; Check 2: Danger level
      (when (>= *danger-level* 3)
        (push "⚠️ High danger level - avoid new trades" issues)
        (setf verdict :reject))
      
      ;; Check 3: Already resigned
      (when (and (fboundp 'has-resigned-p) (has-resigned-p))
        (push "⛔ Already resigned today" issues)
        (setf verdict :reject))
      
      ;; Check 4: Confidence too low
      (when (and confidence (< confidence 0.4))
        (push "⚠️ Low confidence prediction" issues)
        (setf verdict :modify))
      
      ;; Check 5: Against leader opinion
      (when (and *current-leader* 
                 (fboundp 'get-leader-direction)
                 (not (eq (get-leader-direction *candle-history*) direction)))
        (push "📢 Leader disagrees with this direction" issues))
      
      ;; Check 6: Daily loss already significant
      (when (< *daily-pnl* -2000)
        (push "⚠️ Daily loss already significant" issues)
        (setf verdict :modify))
      
      ;; Build reason
      (when issues
        (setf reason (format nil "~{~a~^; ~}" issues)))
      
      ;; Log debate
      (let ((entry (make-debate-entry
                    :timestamp (get-universal-time)
                    :proposal-type :trade
                    :proposer "Swarm"
                    :proposal (format nil "~a ~a (conf: ~,0f%)" direction symbol (* 100 (or confidence 0)))
                    :evaluator-verdict verdict
                    :evaluator-reason reason
                    :final-action nil)))
        (push entry *debate-log*)
        
        ;; Output debate result
        (format t "[L] 🔍 EVALUATOR REVIEW:~%")
        (format t "[L]    Proposal: ~a ~a~%" direction symbol)
        (format t "[L]    Verdict: ~a~%" verdict)
        (when issues
          (format t "[L]    Issues: ~{~a~^, ~}~%" issues))
        
        (list :verdict verdict :issues issues :entry entry)))))

(defun evaluator-check-parameter-change (param old-value new-value)
  "Evaluator reviews parameter change proposals"
  (when *evaluator-enabled*
    (let ((change-pct (if (and old-value (> (abs old-value) 0.001))
                          (* 100 (/ (abs (- new-value old-value)) (abs old-value)))
                          100))
          (verdict :approve)
          (issues nil))
      
      ;; Check for extreme changes
      (when (> change-pct 50)
        (push "⚠️ Change > 50% - too aggressive" issues)
        (setf verdict :modify))
      
      ;; Log
      (format t "[L] 🔍 EVALUATOR: ~a change ~,2f → ~,2f (~,0f%%) → ~a~%"
              param old-value new-value change-pct verdict)
      
      (list :verdict verdict :change-pct change-pct))))

(defun get-debate-summary ()
  "Get summary of recent debates"
  (let* ((recent (subseq *debate-log* 0 (min 10 (length *debate-log*))))
         (approved (count :approve recent :key #'debate-entry-evaluator-verdict))
         (rejected (count :reject recent :key #'debate-entry-evaluator-verdict)))
    (format nil "Recent debates: ~d approved, ~d rejected out of ~d"
            approved rejected (length recent))))
