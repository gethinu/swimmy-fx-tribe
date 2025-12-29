;; evolution.lisp - Continuous Evolution Loop
;; AlphaZero-style self-improvement for Swimmy

;; ===== EVOLUTION CONTROL =====

(defparameter *evolution-interval* 1800)  ; Every 30 minutes
(defparameter *last-evolution-time* 0)
(defparameter *evolution-generation* 0)
(defparameter *top-strategies* nil)

(defun request-evolution (rounds)
  "Request evolution cycle from Rust tournament system"
  (when (and *candle-history* (> (length *candle-history*) 500))
    (format t "[L] 🧬 Requesting evolution (Gen ~d, ~d rounds)...~%" 
            *evolution-generation* rounds)
    (let ((msg (jsown:to-json 
                 (jsown:new-js 
                   ("action" "EVOLVE")
                   ("candles" (candles-to-json (subseq *candle-history* 0 (min 500 (length *candle-history*)))))
                   ("rounds" rounds)))))
      (pzmq:send *cmd-publisher* msg))))

(defun process-evolution-result (result)
  "Process evolution result from Rust"
  (let ((pop-size (jsown:val result "population_size"))
        (top (jsown:val result "top_strategies")))
    (incf *evolution-generation*)
    (setf *top-strategies* top)
    (format t "~%[L] 🏆 Generation ~d Complete!~%" *evolution-generation*)
    (format t "[L] 📊 Population: ~d strategies~%" pop-size)
    (format t "[L] 🥇 Top Strategies:~%")
    (loop for i from 0 below (min 5 (length top))
          for strat = (nth i top)
          do (format t "    ~d. ~a (Elo: ~,0f, Wins: ~d)~%"
                     (1+ i)
                     (jsown:val strat "name")
                     (jsown:val strat "elo")
                     (jsown:val strat "wins")))
    ;; Discord notification
    (notify-discord 
      (format nil "🧬 Gen ~d | Top: ~a (Elo ~,0f)"
              *evolution-generation*
              (jsown:val (first top) "name")
              (jsown:val (first top) "elo"))
      :color 10181046)))

(defun check-evolution ()
  "Check if it's time for evolution cycle"
  (let ((now (get-universal-time)))
    (when (> (- now *last-evolution-time*) *evolution-interval*)
      (setf *last-evolution-time* now)
      (request-evolution 20))))

;; ===== CONTINUOUS LEARNING LOOP =====

(defparameter *learning-cycle* 0)

(defun continuous-learning-step ()
  "One step of continuous learning"
  (incf *learning-cycle*)
  (cond
    ;; Every 10 cycles: Backtest new/evolved strategies
    ((zerop (mod *learning-cycle* 10))
     (when (and *candle-history* (> (length *candle-history*) 200))
       (batch-backtest-knowledge)))
    ;; Every 30 cycles: Evolution tournament
    ((zerop (mod *learning-cycle* 30))
     (check-evolution))
    ;; V4.0: Every 100 cycles: Dream new strategy via Gemini
    ((zerop (mod *learning-cycle* 100))
     (handler-case (dream-code) (error (e) nil)))
    ;; Every 5 cycles: Update school team
    ((zerop (mod *learning-cycle* 5))
     (assemble-team))
    ;; Every cycle: Update NN threshold
    (t nil)))

;; ===== FITNESS FUNCTIONS =====

(defun calculate-fitness (sharpe win-rate max-dd trades)
  "Multi-objective fitness function"
  (let ((sharpe-score (* sharpe 0.4))
        (win-score (* (- win-rate 0.3) 0.3))  ; Penalize below 30%
        (dd-penalty (* max-dd -0.2))          ; Penalize drawdown
        (trade-bonus (if (> trades 10) 0.1 0)))
    (+ sharpe-score win-score dd-penalty trade-bonus)))

(defun rank-strategies-by-fitness ()
  "Rank all strategies by multi-objective fitness"
  (let ((ranked (sort (copy-list *strategy-knowledge-base*) #'>
                      :key (lambda (s) 
                             (calculate-fitness 
                               (or (strategy-sharpe s) 0)
                               (or (strategy-win-rate s) 0)
                               (or (strategy-max-dd s) 0)
                               (or (strategy-trades s) 0))))))
    (format t "[L] 📊 Top 5 by Fitness:~%")
    (loop for i from 0 below (min 5 (length ranked))
          for s = (nth i ranked)
          do (format t "    ~d. ~a (Sharpe: ~,2f)~%" 
                     (1+ i) (strategy-name s) (or (strategy-sharpe s) 0)))
    ranked))

(format t "[EVOLUTION] Continuous evolution system loaded~%")
