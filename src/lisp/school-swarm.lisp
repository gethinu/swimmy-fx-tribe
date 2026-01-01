;; school-swarm.lisp - Swarm Intelligence System
;; V6.21: Extracted from school.lisp for modular architecture
;; Features: Strategy voting with weighted votes, Consensus threshold, Minority reports

;;; Variables defined in school-state.lisp:
;;; *swarm-consensus-threshold*, *swarm-vote-log*, *max-vote-log*

(defstruct strategy-vote
  strategy-name
  direction       ; :buy :sell :hold
  confidence      ; 0.0-1.0
  weight          ; based on sharpe/win-rate
  timestamp
  category)

(defstruct swarm-decision
  timestamp
  direction          ; :buy :sell :hold
  consensus-strength ; 0.0-1.0
  votes-for
  votes-against
  votes-hold
  minority-report    ; dissenting opinions
  confidence)

(defun calculate-strategy-weight (strat)
  "Calculate voting weight based on strategy performance"
  (let* ((sharpe (or (strategy-sharpe strat) 0))
         (base-weight 1.0))
    (cond
      ((> sharpe 1.0) (* base-weight 2.0))
      ((> sharpe 0.5) (* base-weight 1.5))
      ((> sharpe 0) (* base-weight 1.2))
      ((< sharpe -0.5) (* base-weight 0.5))
      (t base-weight))))

(defun collect-strategy-votes (symbol history)
  "Collect votes from all active strategies"
  (let ((votes nil))
    (maphash 
     (lambda (category strategies)
       (dolist (strat strategies)
         (let* ((signal (evaluate-strategy-signal strat history))
                (weight (calculate-strategy-weight strat))
                (vote (make-strategy-vote
                       :strategy-name (strategy-name strat)
                       :direction signal
                       :confidence (if (eq signal :hold) 0.3 0.7)
                       :weight weight
                       :timestamp (get-universal-time)
                       :category category)))
           (push vote votes))))
     *active-team*)
    votes))

(defun aggregate-swarm-votes (votes)
  "Aggregate votes into a collective decision"
  (let ((buy-weight 0.0)
        (sell-weight 0.0)
        (hold-weight 0.0)
        (total-weight 0.0)
        (buy-votes 0)
        (sell-votes 0)
        (hold-votes 0)
        (minority nil))
    
    (dolist (vote votes)
      (let ((w (strategy-vote-weight vote))
            (dir (strategy-vote-direction vote)))
        (incf total-weight w)
        (case dir
          (:buy (incf buy-weight w) (incf buy-votes))
          (:sell (incf sell-weight w) (incf sell-votes))
          (otherwise (incf hold-weight w) (incf hold-votes)))))
    
    (let* ((max-weight (max buy-weight sell-weight hold-weight))
           (winner (cond
                     ((= max-weight buy-weight) :buy)
                     ((= max-weight sell-weight) :sell)
                     (t :hold)))
           (consensus (if (> total-weight 0) (/ max-weight total-weight) 0)))
      
      (dolist (vote votes)
        (when (and (not (eq (strategy-vote-direction vote) winner))
                   (> (strategy-vote-weight vote) 1.2))
          (push (format nil "~a votes ~a" 
                        (strategy-vote-strategy-name vote)
                        (strategy-vote-direction vote))
                minority)))
      
      (make-swarm-decision
       :timestamp (get-universal-time)
       :direction winner
       :consensus-strength consensus
       :votes-for (case winner (:buy buy-votes) (:sell sell-votes) (t hold-votes))
       :votes-against (case winner (:buy (+ sell-votes hold-votes)) 
                                   (:sell (+ buy-votes hold-votes))
                                   (t (+ buy-votes sell-votes)))
       :votes-hold hold-votes
       :minority-report minority
       :confidence (* consensus 0.8)))))

(defun swarm-trade-decision (symbol history)
  "Get swarm's collective trading decision"
  (let* ((votes (collect-strategy-votes symbol history))
         (decision (aggregate-swarm-votes votes)))
    
    (when (> (swarm-decision-consensus-strength decision) 0.5)
      (format t "[L] 🐟 SWARM: ~a (~,0f% consensus, ~d for/~d against)~%"
              (swarm-decision-direction decision)
              (* 100 (swarm-decision-consensus-strength decision))
              (swarm-decision-votes-for decision)
              (swarm-decision-votes-against decision)))
    
    (when (swarm-decision-minority-report decision)
      (format t "[L] 📢 Minority: ~{~a~^, ~}~%" 
              (subseq (swarm-decision-minority-report decision) 
                      0 (min 3 (length (swarm-decision-minority-report decision))))))
    
    (push decision *swarm-vote-log*)
    (when (> (length *swarm-vote-log*) *max-vote-log*)
      (setf *swarm-vote-log* (subseq *swarm-vote-log* 0 *max-vote-log*)))
    
    decision))

(defun swarm-should-trade-p (decision)
  "Check if swarm consensus is strong enough to trade"
  (and (not (eq (swarm-decision-direction decision) :hold))
       (>= (swarm-decision-consensus-strength decision) *swarm-consensus-threshold*)))

(defun get-swarm-confidence (decision)
  "Get confidence level from swarm decision"
  (swarm-decision-confidence decision))

(format t "[L] 🐟 school-swarm.lisp loaded - Swarm Intelligence active~%")
