;; school-voting.lisp - Swimmy School: Consensus & Voting Systems
;; Extracted from school.lisp (V6.14) and school-fortress.lisp (V6.0)

(in-package :swimmy.school)

;;; ==========================================
;;; SWARM INTELLIGENCE (群れの知恵)
;;; ==========================================
;;; Features:
;;; - Strategy voting with weighted votes
;;; - Consensus threshold for trade execution
;;; - Minority report tracking
;;; - Confidence aggregation

(defparameter *swarm-consensus-threshold* 0.6)  ; 60% agreement needed
(defparameter *swarm-vote-log* nil)             ; Track voting history
(defparameter *max-vote-log* 100)

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
  (let* ((score (strategy-selection-score strat))
         (base-weight 1.0))
    ;; Weight based on composite score (Sharpe+PF+WR+MaxDD)
    (let* ((weight (+ base-weight (* 0.6 score))))
      (min 2.0 (max 0.3 weight)))))

(defun strategy-allowed-by-volatility-p (strat)
  "Check if strategy is allowed in current volatility regime"
  (let ((regime (if (boundp '*volatility-regime*) *volatility-regime* :normal))
        (name (strategy-name strat)))
    (cond
      ;; Gotobi is exempt (Time-based structural edge)
      ((search "Gotobi" name) t)
      
      ;; Extreme volatility: Only allow Breakout or Special strategies
      ((eq regime :extreme)
       (or (search "Breakout" name :test #'string=) 
           (search "Volatility" name :test #'string=)))
      
      ;; Low volatility: Disallow generic Cross-Over (avoid chop)
      ((eq regime :low)
       (not (search "Cross" name :test #'string=)))
       
      (t t))))

(defun collect-strategy-votes (symbol history &key timeframe-map)
  "Collect votes from all active strategies using their specific timeframes"
  (declare (ignore symbol))
  (let ((votes nil))
    (maphash 
     (lambda (category strategies)
       (dolist (strat strategies)
         (when (strategy-allowed-by-volatility-p strat)
           (let* ((tf (if (slot-exists-p strat 'timeframe) (strategy-timeframe strat) 1))
                  (target-history (if timeframe-map 
                                      (or (cdr (assoc tf timeframe-map)) history)
                                      history))
                  (signal (evaluate-strategy-signal strat target-history))
                  (weight (calculate-strategy-weight strat))
                  (vote (make-strategy-vote
                         :strategy-name (strategy-name strat)
                         :direction signal
                         :confidence (if (eq signal :hold) 0.3 0.7)
                         :weight weight
                         :timestamp (get-universal-time)
                         :category category)))
             (push vote votes)))))
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
    
    ;; Sum weighted votes
    (dolist (vote votes)
      (let ((w (strategy-vote-weight vote))
            (dir (strategy-vote-direction vote)))
        (incf total-weight w)
        (case dir
          (:buy (incf buy-weight w) (incf buy-votes))
          (:sell (incf sell-weight w) (incf sell-votes))
          (otherwise (incf hold-weight w) (incf hold-votes)))))
    
    ;; Determine winning direction
    (let* ((max-weight (max buy-weight sell-weight hold-weight))
           (winner (cond
                     ((= max-weight buy-weight) :buy)
                     ((= max-weight sell-weight) :sell)
                     (t :hold)))
           (consensus (if (> total-weight 0) (/ max-weight total-weight) 0)))
      
      ;; Identify minority reports (strong dissenting votes)
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
       :confidence (* consensus 0.8)))))  ; Scale confidence by consensus

(defun swarm-trade-decision (symbol history)
  "Get swarm's collective trading decision (Multi-Timeframe V8.0)"
  ;; V8.0: Prepare Multi-Timeframe Data (M1, M5, M15, H1)
  ;; Priority: 1. *candle-histories-tf* (Pre-loaded CSV) 2. Resample from M1
  (let* ((tf-hash (gethash symbol *candle-histories-tf*))
         
         ;; Helper to get or resample
         (get-tf-data (lambda (tf-name factor min-len)
                        (or (and tf-hash (gethash tf-name tf-hash))
                            (if (> (length history) min-len) 
                                (resample-candles history factor) 
                                nil))))
         
         (h-m5 (funcall get-tf-data "M5" 5 20))
         (h-m15 (funcall get-tf-data "M15" 15 60))
         (h-h1 (funcall get-tf-data "H1" 60 240))
         
         ;; Fallback for base history if nil (shouldn't happen but safe)
         (h-m5 (or h-m5 history))
         (h-m15 (or h-m15 history))
         (h-h1 (or h-h1 history))

         (tf-map (list (cons 1 history) 
                       (cons 5 h-m5) 
                       (cons 15 h-m15) 
                       (cons 60 h-h1)))
         (votes (collect-strategy-votes symbol history :timeframe-map tf-map))
         (decision (aggregate-swarm-votes votes)))
    
    ;; Log significant decisions
    (when (> (swarm-decision-consensus-strength decision) 0.5)
      (format t "[L] 🐟 SWARM: ~a (~,0f% consensus, ~d for/~d against)~%"
              (swarm-decision-direction decision)
              (* 100 (swarm-decision-consensus-strength decision))
              (swarm-decision-votes-for decision)
              (swarm-decision-votes-against decision)))
    
    ;; Log minority reports
    (when (swarm-decision-minority-report decision)
      (format t "[L] 📢 Minority: ~{~a~^, ~}~%" 
              (subseq (swarm-decision-minority-report decision) 
                      0 (min 3 (length (swarm-decision-minority-report decision))))))
    
    ;; Record for analysis
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

;;; ==========================================
;;; LEADER FISH SYSTEM (リーダーフィッシュ)
;;; ==========================================
;;; Inspired by: Ensemble Meta-Learning + Natural flocking behavior
;;; The best performing strategy leads the school

(defparameter *current-leader* nil)           ; Current leader strategy
(defparameter *leader-tenure* 0)              ; How long current leader has led
(defparameter *min-leader-tenure* 10)         ; Minimum tenure before leader change
(defparameter *leader-bonus-weight* 2.0)      ; Extra voting weight for leader
(defparameter *leader-history* nil)           ; Track leader performance

(defstruct leader-info
  strategy-name
  sharpe
  win-rate
  tenure-start
  trades-as-leader
  pnl-as-leader)

(defun elect-leader ()
  "Elect the best performing strategy as leader"
  (let* ((all-strategies (append *strategy-knowledge-base* *evolved-strategies*))
         (candidates (remove-if-not (lambda (s) 
                                      (and (strategy-sharpe s)
                                           (> (strategy-sharpe s) 0)))
                                    all-strategies))
         (sorted (sort (copy-list candidates) #'> :key #'strategy-sharpe))
         (best (first sorted)))
    (when best
      (let ((new-leader-name (strategy-name best)))
        ;; Only change leader if tenure exceeded or no current leader
        (when (or (null *current-leader*)
                  (> *leader-tenure* *min-leader-tenure*))
          (unless (and *current-leader* 
                       (string= new-leader-name 
                                (leader-info-strategy-name *current-leader*)))
            ;; New leader elected!
            (when *current-leader*
              (push *current-leader* *leader-history*))
            (setf *current-leader*
                  (make-leader-info
                   :strategy-name new-leader-name
                   :sharpe (strategy-sharpe best)
                   :win-rate 0.0
                   :tenure-start (get-universal-time)
                   :trades-as-leader 0
                   :pnl-as-leader 0.0))
            (setf *leader-tenure* 0)
            (format t "[L] 👑 NEW LEADER: ~a (Sharpe: ~,2f)~%" 
                    new-leader-name (strategy-sharpe best)))))))
  *current-leader*)

(defun get-leader-direction (history)
  "Get the leader's trading signal"
  (when *current-leader*
    (let* ((leader-name (leader-info-strategy-name *current-leader*))
           (leader-strat (or (find leader-name *strategy-knowledge-base* 
                                   :key #'strategy-name :test #'string=)
                             (find leader-name *evolved-strategies*
                                   :key #'strategy-name :test #'string=))))
      (when leader-strat
        (evaluate-strategy-signal leader-strat history)))))

(defun leader-agrees-p (decision)
  "Check if leader agrees with swarm decision"
  (when (and *current-leader* *candle-history*)
    (let ((leader-signal (get-leader-direction *candle-history*)))
      (eq leader-signal (swarm-decision-direction decision)))))

(defun get-leader-boosted-decision (decision)
  "Boost swarm decision if leader agrees, or flag caution if not"
  (if (leader-agrees-p decision)
      (progn
        (format t "[L] 👑 LEADER CONFIRMS: ~a~%" (swarm-decision-direction decision))
        ;; Boost confidence when leader agrees
        (setf (swarm-decision-confidence decision)
              (min 1.0 (* (swarm-decision-confidence decision) 1.3)))
        decision)
      (progn
        (when *current-leader*
          (format t "[L] ⚠️ LEADER DISAGREES: ~a says ~a~%" 
                  (leader-info-strategy-name *current-leader*)
                  (get-leader-direction *candle-history*)))
        ;; Reduce confidence when leader disagrees
        (setf (swarm-decision-confidence decision)
              (* (swarm-decision-confidence decision) 0.7))
        decision)))

(defun update-leader-stats (pnl)
  "Update leader's performance statistics"
  (when *current-leader*
    (incf (leader-info-trades-as-leader *current-leader*))
    (incf (leader-info-pnl-as-leader *current-leader*) pnl)
    (incf *leader-tenure*)))

(defun analyze-swarm-accuracy ()
  "Analyze historical accuracy of swarm decisions"
  (let ((correct 0) (total 0))
    ;; Compare swarm decisions with actual outcomes
    ;; This would need to be correlated with trade results
    (dolist (decision *swarm-vote-log*)
      (when (> (swarm-decision-consensus-strength decision) 0.6)
        (incf total)))
    (if (> total 0)
        (/ correct total)
        0.5)))

;;; ══════════════════════════════════════════════════════════════════
;;;  HIGH COUNCIL (Merged from school-fortress.lisp)
;;; ══════════════════════════════════════════════════════════════════

(defun convene-high-council (proposal category &key (urgency 0))
  "Evaluate trade proposal by the High Council. Returns t (approve) or nil (reject)."
  (let* ((symbol (getf proposal :symbol))
         (direction (getf proposal :direction))
         (danger-level (if (boundp '*danger-level*) *danger-level* 0))
         (swarm-consensus (if (boundp '*last-swarm-consensus*) *last-swarm-consensus* 0.0))
         (volatility-state (if (boundp '*current-volatility-state*) *current-volatility-state* :normal))
         (approval nil)
         (reason ""))
    (format t "[HC] INPUT sym=~a sym-type=~a dir=~a dir-type=~a cat=~a danger=~a swarm=~a vol=~a urgency=~a~%"
            symbol (type-of symbol) direction (type-of direction)
            category danger-level swarm-consensus volatility-state urgency)
    
    (cond
      ((>= urgency 10)
       (setf approval t reason "🚨 EMERGENCY PROTOCOL Override"))
      
      ((>= danger-level 3)
       (setf approval nil reason "🚫 REJECTED: FLEE MODE active."))
      
      ((>= danger-level 2)
       (if (> swarm-consensus 0.7)
           (setf approval t reason "⚠️ APPROVED: Swarm consensus in Danger Lv2")
           (setf approval nil reason "🛡️ REJECTED: Danger Lv2 requires 70%+ swarm consensus")))
           
      ((eq volatility-state :extreme)
       (if (member category '(:breakout :reversion))
           (setf approval t reason "🌊 APPROVED: Extreme volatility fits Category")
           (setf approval nil reason "⛔ REJECTED: Too volatile for Category")))
           
      (t (setf approval t reason "✅ APPROVED: Standard deployment")))
       
    (format t "[HC] RESULT approval=~a reason=~a~%" approval reason)
    (when (or (not approval) (>= danger-level 2) (eq volatility-state :extreme))
      (let ((msg (format nil "🏛️ **HIGH COUNCIL**~%~a ~a (~a)~%~a" 
                         category symbol direction reason)))
        (format t "[L] ~a~%" msg)
        (when (fboundp 'notify-discord-status)
          (notify-discord-status msg :color (if approval 3066993 15158332)))))
           
    approval))
