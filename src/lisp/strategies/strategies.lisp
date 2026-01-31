;; strategies.lisp - Historical Strategy Knowledge Base
;; 歴史的に有効な戦略パターンのナレッジベース（100+種類）
;; Refactored (Expert Panel 2026-01-13) - Strategies split into subfiles

(in-package :swimmy.school)

;; Ensure *strategy-knowledge-base* is imported from swimmy.globals

;; Ensure persistence functions are available
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :swimmy.persistence)
    (load "src/lisp/core/persistence.lisp")))

(defun init-knowledge-base ()
  "Initialize with strategies from The Great Library + SQL database."
  (let ((file-strats (swimmy.persistence:load-all-strategies))
        (db-strats (fetch-all-strategies-from-db)))
    
    ;; Merge lists, prioritizing DB records if duplicates exist (Name based)
    (let ((kb (copy-list db-strats)))
      (dolist (fs file-strats)
        (unless (find (strategy-name fs) kb :key #'strategy-name :test #'string=)
          (push fs kb)))
      (setf *strategy-knowledge-base* kb)))
  
  ;; P8: P7 Recruit Strategies Injection DELETED - use add-to-kb

  (format t "[L] 📚 Knowledge base loaded: ~d strategies from Library~%" 
          (length *strategy-knowledge-base*))

  ;; Phase 10: Unlock the Alpha (Legacy Activation)
  (let ((strats *strategy-knowledge-base*))
    (dolist (s strats)
      (unless (strategy-generation s) (setf (strategy-generation s) 0))
      (unless (strategy-rank s)
        (let ((sharpe (or (strategy-sharpe s) 0.0))
              (trades (or (strategy-trades s) 0)))
          ;; Only rank/purge strategies that have actually traded.
          (when (> trades 0)
            (cond
              ((>= sharpe 0.5) (setf (strategy-rank s) :S))
              ((>= sharpe 0.3) (setf (strategy-rank s) :A))
              ((>= sharpe 0.1) (setf (strategy-rank s) :B))
              (t (setf (strategy-rank s) :graveyard))))
      
      ;; V50.2: Auto-Immortalize Legends (Expert Panel Audit)
      (when (eq (strategy-rank s) :legend)
        (setf (strategy-immortal s) t)
        ;; (format t "[LEGENDS] 🛡️ Restored Immortality to ~a~%" (strategy-name s)) ; Optional log
        ))
      ))
    
    ;; Physically remove graveyard ones once after processing
    (setf *strategy-knowledge-base* 
          (remove-if (lambda (s) (eq (strategy-rank s) :graveyard)) strats))
    
    (format t "[L] 🔓 Alpha Unlocked: ~d strategies survivors (~d purged from memory)~%" 
            (length *strategy-knowledge-base*)
            (- (length strats) (length *strategy-knowledge-base*)))))

;; ===== Sharpe フィルター;; Thresholds
(defparameter *min-sharpe-threshold* 0.0 "Minimum Sharpe to be adopted/kept (Taleb's Rule: Block negative EV)")
(defparameter *min-win-rate-threshold* 0.4 "Minimum Win Rate")
(defparameter *approved-strategies* nil)

(defun filter-by-sharpe (strategies)
  "Filter strategies with Sharpe > threshold"
  (let ((approved (remove-if-not 
                    (lambda (s) (and (strategy-sharpe s) (> (strategy-sharpe s) *min-sharpe-threshold*)))
                    strategies)))
    (format t "[L] 🔍 Filtered: ~d/~d passed Sharpe > ~,1f~%"
            (length approved) (length strategies) *min-sharpe-threshold*)
    approved))

;; V19.2: Round-Robin Backtest Cursor (Musk's Decision)
(defparameter *backtest-cursor* 0 "Cursor for round-robin backtesting")
(defparameter *cycle-start-kb-size* 0 "V48.5: KB size at the start of a BT cycle")
(defparameter *last-cycle-notify-time* 0 "V48.5: Throttling for Cycle Complete alerts")
(defconstant +cycle-notify-interval+ (* 6 3600) "6 Hour Alert Interval")


(defun batch-backtest-knowledge ()
  "V48.0: Phase 1 BT with per-strategy symbol support + larger batch size.
   Backtests each strategy using its native symbol's candle data."
  (load-backtest-cache) 
  (setf swimmy.globals:*rr-backtest-results-buffer* nil)
  (setf swimmy.globals:*rr-backtest-start-time* (get-universal-time))
  
  (let* ((max-batch-size 1000) 
         (total (length *strategy-knowledge-base*))
         (old-cursor *backtest-cursor*)
         (start-idx (mod *backtest-cursor* total))
         (end-idx (min total (+ start-idx max-batch-size))))
    
    (when (or (zerop old-cursor) (>= old-cursor total))
      (setf *cycle-start-kb-size* total))
    
    (let* ((batch-strategies (subseq *strategy-knowledge-base* start-idx end-idx))
           ;; Handle wrap-around if needed
           (wrap-strategies (if (< (- end-idx start-idx) max-batch-size)
                                (subseq *strategy-knowledge-base* 0 (min total (- max-batch-size (- end-idx start-idx))))
                                nil))
           (final-batch (append batch-strategies wrap-strategies))
           (cached-count 0)
           (requested-count 0)
           (skipped-count 0))
      
      ;; Update cursor for next time
      (setf *backtest-cursor* (mod (+ start-idx (length final-batch)) total))
      
      ;; V48.0: Detect cycle completion (cursor wrapped around)
      ;; V50.9 Fix: Handle single-batch completion (old=0 -> new=0)
      (let ((cycle-completed (or (and (> old-cursor 0) (< *backtest-cursor* old-cursor))
                                 (and (= old-cursor 0) (= *backtest-cursor* 0) (> total 0)))))
        (format t "[L] 🧪 Batch testing ~d strategies (Round-Robin: ~d -> ~d)...~%" 
                (length final-batch) start-idx *backtest-cursor*)
        
        ;; V48.0: Use strategy's native symbol for candle data
        (dolist (strat final-batch)
          (let* ((sym (or (strategy-symbol strat) "USDJPY"))
                 (snapshot (or (gethash sym *candle-histories*)
                               *candle-history*
                               (gethash "USDJPY" *candle-histories*))))
            (if (and snapshot (> (length snapshot) 100))
                (progn
                  ;; Apply cached metrics if available, but still request fresh BT
                  (let ((cached (get-cached-backtest (strategy-name strat))))
                    (when cached
                      (incf cached-count)
                      ;; V48.5: Apply cached metrics to strategy and prune if weak
                      (let ((sharpe (float (getf cached :sharpe 0.0)))
                            (pf (float (getf cached :profit-factor 0.0)))
                            (wr (float (getf cached :win-rate 0.0)))
                            (trades (getf cached :trades 0)))
                        (setf (strategy-sharpe strat) sharpe
                              (strategy-profit-factor strat) pf
                              (strategy-win-rate strat) wr
                              (strategy-trades strat) trades)
                        (when (< sharpe 0.1)
                          (prune-to-graveyard strat "Cached Sharpe < 0.1")))))
                  (request-backtest strat :candles snapshot :symbol sym :suffix "-RR")
                  (incf requested-count))
                (progn
                  (incf skipped-count)
                  (format t "[L] ⚠️ Skipping BT (no candles) for ~a (~a)~%"
                          (strategy-name strat) sym))))))
        
        ;; Expected count should reflect actual enqueued requests
        (setf swimmy.globals:*rr-expected-backtest-count* requested-count)
                     
        (format t "[L] 🏁 Batch Request Complete. Cached: ~d, Queued: ~d, Skipped: ~d (Cursor: ~d)~%" 
                cached-count requested-count skipped-count *backtest-cursor*)
        
        ;; Notify Discord only if actual requests were made
        (when (> requested-count 0)
          (notify-discord-alert 
            (format nil "🧪 **Phase 1 BT Batch (RR)**~%- Requested: ~d / ~d~%- Cursor: ~d / ~d~a" 
                    requested-count total *backtest-cursor* total
                    (if cycle-completed "~%✅ **Cycle Complete!**" "")) 
            :color 3066993))
        
        ;; V48.5: Throttled summary on cycle completion (Every 6 hours)
        ;; V49.5: Decoupled and Triggered at 90% (expert Panel)
        (when cycle-completed
          (format t "[L] 🔄 KB Backtest Cycle Complete! Sending throttled summary...~%")
          (setf *last-cycle-notify-time* (get-universal-time))
          (notify-backtest-summary :rr)))))

(defun adopt-proven-strategies ()
  "Adopt only strategies that passed Sharpe filter"
  (setf *approved-strategies* (filter-by-sharpe *strategy-knowledge-base*))
  (when *approved-strategies*
    (format t "[L] ✅ Adopted ~d proven strategies~%" (length *approved-strategies*))
    (dolist (s *approved-strategies*)
      (unless (find (strategy-name s) *evolved-strategies* :key #'strategy-name :test #'string=)
        (push s *evolved-strategies*))))
  (when *evolved-strategies*
    (setf *evolved-strategies* 
          (sort *evolved-strategies* #'> 
                :key (lambda (s) (or (strategy-sharpe s) 0))))))

;; Auto-initialize
;; (init-knowledge-base)

;;; ==========================================
;;; V7.9++: INDICATOR TYPE INFERENCE (Sharpe=-3.75 Bug Fix)
;;; Infer indicator_type from strategy indicators for correct backtesting
;;; ==========================================

(defun infer-indicator-type (strategy)
  "Infer the primary indicator type from strategy indicators.
   This fixes the Sharpe=-3.75 bug where all strategies defaulted to SMA."
  (let* ((indicators (strategy-indicators strategy))
         (first-indicator (first indicators))
         (indicator-name (cond
                           ((stringp first-indicator) (string-downcase first-indicator))
                           ((and first-indicator (listp first-indicator)) (string-downcase (symbol-name (first first-indicator))))
                           (t nil))))
    (cond
      ;; MACD strategies
      ((and indicator-name (search "macd" indicator-name)) "macd")
      ;; RSI strategies (including elder, momentum)
      ((and indicator-name (search "rsi" indicator-name)) "rsi")
      ;; Stochastic strategies
      ((and indicator-name (search "stoch" indicator-name)) "stoch")
      ;; Bollinger Band strategies
      ((and indicator-name (search "boland" indicator-name)) "bb") ; Fixed "boland" for BB
      ((and indicator-name (search "bb" indicator-name)) "bb")
      ;; EMA strategies
      ((and indicator-name (search "ema" indicator-name)) "ema")
      ;; Default to SMA
      (t "sma"))))

(defun apply-indicator-types ()
  "Apply inferred indicator types to all strategies in knowledge base."
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (strat *strategy-knowledge-base*)
      (let ((ind-type (infer-indicator-type strat)))
        (setf (strategy-indicator-type strat) ind-type)
        (incf (gethash ind-type counts 0))))
    ;; Log the distribution
    (format t "[STRATEGIES] Indicator types assigned:~%")
    (maphash (lambda (k v) (format t "  ~a: ~a strategies~%" k v)) counts)))

;; Apply indicator types after initialization
(apply-indicator-types)

(format t "[STRATEGIES] ~d strategies loaded from Knowledge Base~%" 
        (length *strategy-knowledge-base*))

;;; ==========================================
;;; BENCH & KILL SYSTEM (V46.0 Expert Panel 2026-01-16)
;;; P0: 3-strikes permanent elimination
;;; P1: Similarity-based pruning (min 3 per category)
;;; ==========================================

;; *benched-strategies* REMOVED (Expert Panel Cleanup - Hickey)
;; We now use strategy-status slot in the strategy struct.

(defparameter *kill-counter* (make-hash-table :test 'equal) "P0: Track how many times each strategy was benched")
(defparameter *max-bench-strikes* 1 "P0: Kill IMMEDIATELY after 1 bench (Expert Panel 2026-01-16)")
(defparameter *min-strategies-per-category* 3 "P1: Minimum strategies to keep per category")
(defparameter *similarity-threshold* 0.10 "P1: Parameter distance below this = similar")


(defun find-strategy-object (name)
  "Find a strategy object by name in the KB"
  (find name *strategy-knowledge-base*
        :key #'strategy-name
        :test #'string-equal))

(defun find-strategy (name)
  "Alias for find-strategy-object (Generative AI Compatibility)"
  (or (find-strategy-object name)
      (find name swimmy.globals:*evolved-strategies* :key #'strategy-name :test #'string=)))

(defun strategy-benched-p (name)
  "Check if strategy is benched (check status slot)"
  (let ((s (find-strategy-object name)))
    (if (and s (eq (strategy-status s) :benched))
        (strategy-status-reason s)
        nil)))

(defun get-kill-count (name)
  "Get how many times a strategy has been benched"
  (or (gethash name *kill-counter*) 0))

(defun kill-strategy (name reason)
  "P0: Soft Kill - Bench indefinitely instead of permanent deletion (Expert Panel 2026-01-16)"
  (let ((s (find-strategy-object name)))
    (when s
      (format t "~%[L] 🛡️ SOFT KILL: ~a (~a) -> Benched indefinitely~%" name reason)
      (setf (strategy-status s) :killed)
      (setf (strategy-status-reason s) (format nil "SOFT_KILL: ~a" reason))
      ;; Notify
      (when (fboundp 'notify-discord-alert)
        (notify-discord-alert 
          (format nil "🛡️ **Strategy Soft-Killed (Cooldown)**~%Name: ~a~%Reason: ~a~%Action: Shelved for future review" name reason)
          :color 15158332)))))

(defun bench-strategy (name reason)
  "Bench a strategy with reason. P0: 3rd bench = permanent kill"
  (let ((strikes (1+ (get-kill-count name)))
        (s (find-strategy-object name)))
    (setf (gethash name *kill-counter*) strikes)
    (if (>= strikes *max-bench-strikes*)
        ;; 3 strikes - you're out!
        (kill-strategy name (format nil "3-Strikes Rule: ~a" reason))
        ;; Just bench for now
        (when s
          (setf (strategy-status s) :benched)
          (setf (strategy-status-reason s) reason)
          (format t "[L] 🚫 BENCHED (~d/~d): ~a (~a)~%" 
                  strikes *max-bench-strikes* name reason)))))

(defun unbench-all ()
  "Unbench all strategies (but keep kill counters)"
  ;; Iterate both lists
  (dolist (s *strategy-knowledge-base*)
    (when (or (eq (strategy-status s) :benched) (eq (strategy-status s) :killed))
      (setf (strategy-status s) :active)
      (setf (strategy-status-reason s) "")))
  (dolist (s swimmy.globals:*evolved-strategies*)
    (when (or (eq (strategy-status s) :benched) (eq (strategy-status s) :killed))
      (setf (strategy-status s) :active)
      (setf (strategy-status-reason s) "")))
  (format t "[L] ♻️  All strategies status reset to ACTIVE (kill counters preserved)~%"))

(defun reset-all-kill-counters ()
  "Reset all kill counters (use sparingly - monthly?)"
  (clrhash *kill-counter*)
  (format t "[L] 🔄 All kill counters reset~%"))

(defun should-weekly-unbench-p ()
  "Check if it is Monday morning (before 9am) to reset bench"
  (multiple-value-bind (s m h d mo y dow) (decode-universal-time (get-universal-time))
    (declare (ignore s m d mo y))
    (and (= dow 0) (< h 9))))

(defun weekly-unbench-all ()
  "Weekly unbench wrapper - DISABLED for Kodoku System (Strict Survival)"
  ;; (unbench-all)
  (format t "[L] 🔒 Weekly unbench skipped (Kodoku Rules in effect)~%"))

;;; ==========================================
;;; P1: SIMILARITY CHECK & PRUNING
;;; ==========================================

(defun strategy-distance-legacy (strat-a strat-b)
  "Calculate normalized parameter distance between two strategies.
   Returns 0.0 for identical, 1.0 for completely different.
   Compares: SL, TP, timeframe, indicator parameters."
  (let* ((sl-a (or (strategy-sl strat-a) 0.01))
         (sl-b (or (strategy-sl strat-b) 0.01))
         (tp-a (or (strategy-tp strat-a) 0.02))
         (tp-b (or (strategy-tp strat-b) 0.02))
         (tf-a (or (strategy-timeframe strat-a) 60))
         (tf-b (or (strategy-timeframe strat-b) 60))
         (ind-a (strategy-indicators strat-a))
         (ind-b (strategy-indicators strat-b))
         ;; Calculate relative differences
         (sl-diff (/ (abs (- sl-a sl-b)) (max sl-a sl-b 0.01)))
         (tp-diff (/ (abs (- tp-a tp-b)) (max tp-a tp-b 0.01)))
         (tf-diff (if (and (numberp tf-a) (numberp tf-b))
                      (/ (abs (- tf-a tf-b)) (max tf-a tf-b 1))
                      0.5))
         ;; Indicator difference (compare first indicator params)
         (ind-diff (if (and ind-a ind-b 
                            (listp (first ind-a)) (listp (first ind-b))
                            (eq (car (first ind-a)) (car (first ind-b))))
                       (let ((param-a (or (second (first ind-a)) 0))
                             (param-b (or (second (first ind-b)) 0)))
                         (if (and (numberp param-a) (numberp param-b) (> (max param-a param-b) 0))
                             (/ (abs (- param-a param-b)) (max param-a param-b))
                             0.5))
                       0.5)))
    ;; Weighted average distance
    (/ (+ (* 0.2 sl-diff) (* 0.2 tp-diff) (* 0.2 tf-diff) (* 0.4 ind-diff)) 1.0)))

(defun strategies-similar-p (strat-a strat-b)
  "Check if two strategies are too similar (P1)"
  (and (eq (strategy-category strat-a) (strategy-category strat-b))
       (< (strategy-distance-legacy strat-a strat-b) *similarity-threshold*)))

(defun prune-similar-strategies-legacy (strategies &optional (min-per-category *min-strategies-per-category*))
  "P1: Remove near-duplicate strategies, keeping the stronger one.
   Maintains minimum MIN-PER-CATEGORY strategies per category."
  (let ((category-counts (make-hash-table))
        (kept nil)
        (pruned-count 0))
    ;; Sort by sharpe desc so we keep stronger ones first
    (setf strategies (sort (copy-list strategies) #'> 
                           :key (lambda (s) (or (strategy-sharpe s) -999))))
    (dolist (strat strategies)
      (let* ((cat (or (strategy-category strat) :unknown))
             (cat-count (gethash cat category-counts 0))
             (dominated-p nil))
        ;; Check if similar to any already-kept strategy
        (when (>= cat-count min-per-category)
          (dolist (k kept)
            (when (and (eq (strategy-category k) cat)
                       (strategies-similar-p strat k))
              (setf dominated-p t)
              (return))))
        (if dominated-p
            ;; Skip this one (dominated by a stronger similar strategy)
            (progn
              (incf pruned-count)
              (format t "[L] ✂️ Pruned similar: ~a (dominated)~%" (strategy-name strat)))
            ;; Keep it
            (progn
              (push strat kept)
              (incf (gethash cat category-counts 0))))))
    (format t "[L] 🧹 Similarity pruning: ~d removed, ~d kept (min ~d per category)~%" 
            pruned-count (length kept) min-per-category)
    (nreverse kept)))

;;; ==========================================
;;; INTRA-CATEGORY TOURNAMENT (Expert Panel 2026-01-16)
;;; New strategies must compete against same-category rivals
;;; ==========================================

(defun compete-for-slot (new-strat)
  "Make new strategy compete against existing same-category strategies.
   Returns T if strategy earned a slot, NIL if rejected.
   Untested strategies (Sharpe nil/0) bypass tournament to be backtested first."
  (let* ((cat (or (strategy-category new-strat) :unknown))
         (raw-sharpe (strategy-sharpe new-strat))
         (new-sharpe (or raw-sharpe 0.0))
         (rivals (remove-if-not 
                   (lambda (s) (eq (strategy-category s) cat))
                   *strategy-knowledge-base*))
         (rival-count (length rivals)))
    
    ;; BYPASS: Untested strategies enter automatically to get backtested
    (when (or (null raw-sharpe) (= new-sharpe 0.0))
      (format t "[TOURNAMENT] 🆕 ~a enters for evaluation (Sharpe not yet tested)~%" 
              (strategy-name new-strat))
      (return-from compete-for-slot t))
    
    ;; If below minimum, allow entry without competition
    (when (< rival-count *min-strategies-per-category*)
      (format t "[TOURNAMENT] 🏆 ~a enters (Category ~a has ~d < ~d min)~%" 
              (strategy-name new-strat) cat rival-count *min-strategies-per-category*)
      (return-from compete-for-slot t))
    
    ;; Find the weakest rival (only compare TESTED rivals with Sharpe > 0)
    (let* ((tested-rivals (remove-if (lambda (s) 
                                        (let ((sh (strategy-sharpe s)))
                                          (or (null sh) (= sh 0.0))))
                                      rivals))
           (weakest (first (sort (copy-list tested-rivals) #'<
                                :key (lambda (s) (or (strategy-sharpe s) -999.0))))))
      (cond
        ;; No tested rivals = automatic entry
        ((null weakest)
         (format t "[TOURNAMENT] 🏆 ~a enters (no tested rivals in ~a)~%" 
                 (strategy-name new-strat) cat)
         t)
        ;; Beat the weakest? Replace it
        ((> new-sharpe (or (strategy-sharpe weakest) -999.0))
         (format t "[TOURNAMENT] ⚔️ ~a defeats ~a (Sharpe: ~,2f > ~,2f)~%"
                 (strategy-name new-strat) (strategy-name weakest)
                 new-sharpe (or (strategy-sharpe weakest) -999.0))
         ;; Kill the defeated
         (kill-strategy (strategy-name weakest) 
                        (format nil "Lost tournament to ~a" (strategy-name new-strat)))
         t)
        ;; Lost to all
        (t
         (format t "[TOURNAMENT] 💀 ~a rejected (Sharpe ~,2f too weak for ~a)~%"
                 (strategy-name new-strat) new-sharpe cat)
         nil)))))

(defun current-trading-session ()
  "Determine current market session (JST based approximation)"
  (let ((h (nth 2 (multiple-value-list (decode-universal-time (get-universal-time))))))
    (cond 
      ((and (>= h 9) (< h 15)) :asian)
      ((and (>= h 16) (< h 21)) :london)
      ((or (>= h 22) (< h 6)) :ny)
      (t :mixed))))

;;; ==========================================
;;; STRATEGY PERFORMANCE EVALUATION
;;; ==========================================

(defun evaluate-strategy-performance (strat sharpe trades win-rate &optional (profit-factor 0.0))
  "Adjust strategy parameters based on backtest performance (Kodoku Standard)"
  (let ((name (strategy-name strat)))
      ;; THE PROVING GROUNDS (Tribal Selection 2026-01-16)
      
      ;; TIER 1: DEATH (Sharpe < 0) - Soft Kill / Bench
      (when (< sharpe 0)
        (bench-strategy name (format nil "Grading [D]: Negative Sharpe ~,2f. Needs optimization." sharpe))
        (return-from evaluate-strategy-performance))

      ;; TIER 2: REJECT (0 <= Sharpe < 0.6) - Noise
      (when (< sharpe 0.6)
        (bench-strategy name (format nil "Grading [C]: Weak Sharpe ~,2f (< 0.6)" sharpe))
        (return-from evaluate-strategy-performance))

      ;; TIER 3: CONDITIONAL (0.6 <= Sharpe < 1.0) - Bench for Review
      ;; Note: We bench these so they don't consume resources unless explicitly promoted
      (when (< sharpe 1.0)
        (bench-strategy name (format nil "Grading [B]: Med Sharpe ~,2f (< 1.0)" sharpe))
        (return-from evaluate-strategy-performance))

      ;; TIER 4: SURVIVE (Sharpe >= 1.0)
      
      ;; QUALITY CHECKS (Rule 2 & 3)
      (when (> trades 20) ; Only apply with sufficient sample
        ;; Rule 3: PF Floor
        (when (< profit-factor 1.2)
          (bench-strategy name (format nil "Grading [D]: Low PF ~,2f (< 1.2)" profit-factor))
          (return-from evaluate-strategy-performance))
          
        ;; Rule 3: Fake PF Detection (Martingale/Grid signature)
        (when (and (> profit-factor 2.0) (< sharpe 1.0))
          (bench-strategy name (format nil "Grading [D]: Fake PF Trap (PF ~,2f but Sharpe ~,2f)" profit-factor sharpe))
          (return-from evaluate-strategy-performance))
          
        ;; Rule 2: High Win-Rate Trap
        (when (and (> win-rate 70) (< profit-factor 1.3))
          (bench-strategy name (format nil "Grading [D]: High WR Trap (WR ~,1f% but PF ~,2f)" win-rate profit-factor))
          (return-from evaluate-strategy-performance)))
          
      ;; TIER 4: SURVIVE -> GRADED [S] or [A]
      (let ((grade (if (and (>= sharpe 1.2) (>= profit-factor 1.5)) "S" "A")))
          (format t "[L] 🎖️ Grading [~a]: ~a Survived! (S=~,2f PF=~,2f)~%" grade name sharpe profit-factor))

      ;; Proceeds to parameter tuning below...
      
      (cond
        ;; Poor performance: tighten SL, reduce volume
        ((or (< sharpe 0) (< win-rate 40))
         (when (strategy-sl strat)
           (setf (strategy-sl strat) (* 0.9 (strategy-sl strat))))
         (when (strategy-volume strat)
           (setf (strategy-volume strat) (max 0.01 (* 0.8 (strategy-volume strat)))))
         (format t "[L] ⚙️ 📉 ~a: Tightening params (poor perf)~%" name))
        ;; Good performance: widen TP, increase volume (Aggressive V7.0)
        ((and (> sharpe 1.0) (> win-rate 55))
         (when (strategy-tp strat)
           (setf (strategy-tp strat) (* 1.1 (strategy-tp strat))))
         (when (strategy-volume strat)
           ;; Scale faster: 1.2x -> 1.5x, Cap 0.1 -> 0.3
           (setf (strategy-volume strat) (min 0.3 (* 1.5 (strategy-volume strat)))))
         (format t "[L] ⚙️ 📈 🚀 ~a: Aggressive Expansion (Good Perf)~%" name))
        ;; Average: adjust SL/TP ratio for better risk/reward
        ((and (> sharpe 0.5) (> win-rate 45))
         (when (and (strategy-sl strat) (strategy-tp strat))
           (let ((rr (/ (strategy-tp strat) (max 0.01 (strategy-sl strat)))))
             (when (< rr 2.0)
               (setf (strategy-tp strat) (* 1.05 (strategy-tp strat)))
               (format t "[L] ⚙️ 🎯 ~a: Improving R:R ratio~%" name))))))))
