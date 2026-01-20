;; evolution.lisp - Continuous Evolution Loop

(in-package :swimmy.school)
;; AlphaZero-style self-improvement for Swimmy

;; ===== EVOLUTION CONTROL =====

(defparameter *evolution-interval* 300)  ; AGGRESSIVE: Every 5 minutes (was 1800)
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
                   ("candles" (swimmy.school:candles-to-json (subseq *candle-history* 0 (min 500 (length *candle-history*)))))
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
(defparameter *last-regime* :unknown "Last known regime for change detection")
(defparameter *meta-learning-interval* 120)  ; Every 2 minutes

(defun continuous-learning-step ()
  "One step of continuous learning - Naval enhanced with meta-learning hooks"
  (incf *learning-cycle*)
  (cond
    ;; Every 300 cycles (~5 min): Backtest new/evolved strategies
    ((zerop (mod *learning-cycle* 300))
     (when (and *candle-history* (> (length *candle-history*) 200))
       (batch-backtest-knowledge)))
    ;; Every 300 cycles (~5 min): Evolution tournament (AGGRESSIVE)
    ((zerop (mod *learning-cycle* 300))
     (check-evolution))
    ;; V4.0: Every 300 cycles (~5 min): Dream new strategy via Gemini
    ((zerop (mod *learning-cycle* 300))
     (handler-case (dream-code) (error (e) nil)))
    
    ;; V7.1: Every 100 cycles (~1.5 min): Evolve from Wisdom (Transfer Learning)
    ((zerop (mod *learning-cycle* 100))
     (handler-case (evolve-from-wisdom) (error (e) (format t "[E] Wisdom evolution failed: ~a~%" e))))

    ;; Every 60 cycles (~1 min): Update school team
    ((zerop (mod *learning-cycle* 60))
     (assemble-team))
    
    ;; NAVAL IMPROVEMENT: Meta-learning integration
    ;; Every 120 cycles (~2 min): Check and update meta-learning
    ((zerop (mod *learning-cycle* *meta-learning-interval*))
     (naval-meta-learning-step))
    
    ;; Every cycle: nothing special
    (t nil)))

(defun naval-meta-learning-step ()
  "Naval improvement: Integrated meta-learning step with regime change detection"
  (handler-case
      (progn
        ;; 1. Update correlations for transfer learning
        (when (fboundp 'maybe-update-correlations)
          (maybe-update-correlations))
        
        ;; 2. Check for regime change
        (let ((current-regime (if (boundp '*current-regime*) *current-regime* :unknown)))
          (when (and (not (eq current-regime *last-regime*))
                     (not (eq current-regime :unknown)))
            (on-regime-change *last-regime* current-regime))
          (setf *last-regime* current-regime))
        
        ;; 3. Periodically run meta-learning update
        (when (and (fboundp 'update-best-strategy-for-regime)
                   (boundp '*current-regime*))
          (update-best-strategy-for-regime *current-regime*))
        
        ;; 4. REVIEW FIX: Periodic save of meta-learning state (every 30 min = 1800 cycles)
        (when (and (zerop (mod *learning-cycle* 1800))
                   (fboundp 'save-meta-learning))
          (save-meta-learning)))
    (error (e)
      (format t "[E] Meta-learning step error: ~a~%" e))))

(defun on-regime-change (old-regime new-regime)
  "Naval improvement: Handle regime change with strategy auto-switch"
  (format t "~%[M] 🔄 REGIME CHANGE: ~a → ~a~%" old-regime new-regime)
  
  ;; 1. Get best strategy for new regime
  (let ((best-strategy (when (fboundp 'get-best-strategy-for-regime)
                         (get-best-strategy-for-regime new-regime))))
    (when best-strategy
      (format t "[M] ✨ Auto-switching to: ~a~%" best-strategy)
      
      ;; 2. Generate new regime-appropriate strategy
      (when (fboundp 'auto-generate-strategy-for-regime)
        (let ((new-strat (auto-generate-strategy-for-regime)))
          (when new-strat
            (format t "[M] 🎯 Generated backup: ~a~%" (strategy-name new-strat))
            (when (and (boundp '*evolved-strategies*) (listp *evolved-strategies*))
              (push new-strat *evolved-strategies*)))))
      
      ;; 3. Discord notification
      (when (fboundp 'notify-discord)
        (notify-discord
         (format nil "🔄 **Regime Change**~%~a → ~a~%Best: ~a"
                 old-regime new-regime best-strategy)
         :color 16776960))))  ; Yellow
  
  ;; 4. Trigger transfer learning update
  (when (fboundp 'extract-learned-patterns)
    (dolist (symbol '("USDJPY" "EURUSD" "GBPUSD"))
      (handler-case
          (extract-learned-patterns symbol)
        (error () nil)))))


;; ===== WISDOM-BASED EVOLUTION (Connecting to Memory) =====

(defun evolve-from-wisdom ()
  "Generate a new strategy using historical wisdom (Transfer Learning) as a seed.
   Instead of random generation, we mutate a successful historical pattern."
  (when (and (boundp '*learned-patterns*) *learned-patterns*)
    ;; Pick a random symbol's patterns
    (let* ((symbols (loop for k being the hash-keys of *learned-patterns* collect k))
           (target-symbol (nth (random (length symbols)) symbols))
           (patterns (gethash target-symbol *learned-patterns*)))
      
      (when patterns
        ;; Pick a random pattern
        (let* ((pattern (nth (random (length patterns)) patterns))
               ;; Convert to strategy
               (seed-strategy (if (fboundp 'swimmy.school::pattern-to-strategy-template)
                                  (swimmy.school::pattern-to-strategy-template pattern "Wisdom")
                                  nil)))
          
          (when seed-strategy
            (format t "[EVOLUTION] 🧠 Evolving from wisdom: ~a (Source: ~a)~%" 
                    (learned-pattern-pattern-type pattern)
                    (learned-pattern-source-symbol pattern))
            
            ;; Apply mutation immediately to make it unique
            (if (fboundp 'mutate-strategy)
                (let ((mutated (mutate-strategy seed-strategy 0.5)))
                   (format t "[EVOLUTION] 🧬 Mutated seed: ~a (Orig: ~a)~%" 
                           (strategy-name mutated) (strategy-name seed-strategy))
                   (push mutated *evolved-strategies*)
                   mutated)
                (progn
                   (push seed-strategy *evolved-strategies*)
                   seed-strategy))))))))

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
