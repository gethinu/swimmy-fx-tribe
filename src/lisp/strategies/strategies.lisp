;; strategies.lisp - Historical Strategy Knowledge Base
;; 歴史的に有効な戦略パターンのナレッジベース（100+種類）
;; Refactored (Expert Panel 2026-01-13) - Strategies split into subfiles

(in-package :swimmy.school)

;; Ensure *strategy-knowledge-base* is imported from swimmy.globals

;; Ensure persistence functions are available
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :swimmy.persistence)
    (load "src/lisp/core/persistence.lisp")))

(defun %normalize-timeframe-minutes (tf)
  "Normalize timeframe input to internal minutes(int)."
  (labels ((all-digits-p (s)
             (and (stringp s)
                  (> (length s) 0)
                  (loop for ch across s always (digit-char-p ch))))
           (parse-int (s default)
             (handler-case (parse-integer s) (error () default))))
    (cond
      ((numberp tf) (max 1 (round tf)))
      ((stringp tf)
       (let* ((up (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return) tf))))
         (cond
           ((or (string= up "MN") (string= up "MN1")) 43200)
           ((and (>= (length up) 2)
                 (char= (char up 0) #\M)
                 (all-digits-p (subseq up 1)))
            (max 1 (parse-int (subseq up 1) 1)))
           ((and (>= (length up) 2)
                 (char= (char up 0) #\H)
                 (all-digits-p (subseq up 1)))
            (* 60 (max 1 (parse-int (subseq up 1) 1))))
           ((and (>= (length up) 2)
                 (char= (char up 0) #\D)
                 (all-digits-p (subseq up 1)))
            (* 1440 (max 1 (parse-int (subseq up 1) 1))))
           ((and (>= (length up) 2)
                 (char= (char up 0) #\W)
                 (all-digits-p (subseq up 1)))
            (* 10080 (max 1 (parse-int (subseq up 1) 1))))
           (t 1))))
      (t 1))))

(defun %normalize-strategy-timeframe! (strat)
  (when (and strat (strategy-p strat))
    (setf (strategy-timeframe strat)
          (%normalize-timeframe-minutes (strategy-timeframe strat))))
  strat)

(defvar *allow-archived-rank-resurrection-write* nil
  "When T, upsert can intentionally revive archived DB rows to active ranks.")
(defparameter *kb-init-max-archived-duplicate-revives* 256
  "Maximum archived DB duplicate revives during one KB init pass. NIL means unlimited.")
(defparameter *kb-init-library-dirs* '("B" "A" "S" "LEGEND")
  "Library directories loaded during startup KB init.")
(defparameter *kb-init-revive-source-dirs* '("B" "A" "S" "LEGEND")
  "Library directories considered authoritative sources for archived->active revive.")
(defparameter *kb-init-revive-require-active-library-file* t
  "When T, archived->active revive requires the strategy file to exist under active source dirs.")

(defun %normalize-rank-token-for-kb-init (rank)
  "Normalize rank token into uppercase string without leading colon."
  (let* ((raw (cond
                ((null rank) "NIL")
                ((symbolp rank) (symbol-name rank))
                (t (format nil "~a" rank))))
         (trimmed (string-upcase (string-trim '(#\Space #\Tab #\Newline) raw))))
    (if (and (> (length trimmed) 0)
             (char= (char trimmed 0) #\:))
        (subseq trimmed 1)
        trimmed)))

(defun %archived-rank-for-kb-init-p (rank)
  "Return T when rank is archived and should not shadow active file strategy."
  (member (%normalize-rank-token-for-kb-init rank)
          '("GRAVEYARD" "RETIRED" "ARCHIVE" "ARCHIVED" "LEGEND-ARCHIVE")
          :test #'string=))

(defun %active-rank-for-kb-init-p (rank)
  "Return T when rank is explicitly active (B/A/S/LEGEND)."
  (let ((token (%normalize-rank-token-for-kb-init rank)))
    (member token '("B" "A" "S" "LEGEND") :test #'string=)))

(defun %unevaluated-rank-for-kb-init-p (rank)
  "Return T when rank should be treated as unevaluated at KB init."
  (member (%normalize-rank-token-for-kb-init rank)
          '("NIL" "INCUBATOR" "SCOUT" "")
          :test #'string=))

(defun %purge-revived-strategy-archive-files (name
                                              &optional (root swimmy.persistence::*library-path*))
  "Delete stale GRAVEYARD/RETIRED files for a strategy revived to active rank."
  (let* ((safe-name (swimmy.persistence::sanitize-filename name))
         (removed 0))
    (dolist (dir '("GRAVEYARD" "RETIRED"))
      (let ((path (merge-pathnames (format nil "~a/~a.lisp" dir safe-name) root)))
        (when (probe-file path)
          (ignore-errors
            (delete-file path)
            (incf removed)))))
    removed))

(defun %basename-without-lisp (filename)
  "Return FILENAME without trailing .lisp."
  (let* ((name (or filename ""))
         (len (length name)))
    (if (and (> len 5)
             (string-equal ".lisp" name :start1 0 :end1 5 :start2 (- len 5) :end2 len))
        (subseq name 0 (- len 5))
        name)))

(defun %build-kb-init-revive-source-index (&optional (root swimmy.persistence::*library-path*))
  "Build hash-table of strategy names that physically exist under active source dirs."
  (let ((idx (make-hash-table :test 'equal)))
    (dolist (dir *kb-init-revive-source-dirs*)
      (let ((pattern (merge-pathnames (format nil "~a/*.lisp" dir) root)))
        (dolist (path (ignore-errors (directory pattern)))
          (let ((name (%basename-without-lisp (file-namestring path))))
            (when (> (length name) 0)
              (setf (gethash name idx) t))))))
    idx))

(defun init-knowledge-base ()
  "Initialize with strategies from The Great Library + SQL database."
  (let* ((raw-file-strats (or (swimmy.persistence:load-all-strategies
                               :dirs *kb-init-library-dirs*) '()))
         ;; Startup performance: avoid deserializing archived DB rows (can be 400k+).
         (raw-db-strats (or (fetch-all-strategies-from-db :ranks '(":B" ":A" ":S" ":LEGEND")) '()))
         (file-strats (remove-if-not #'strategy-p raw-file-strats))
         (db-strats (remove-if-not #'strategy-p raw-db-strats))
         (dropped (+ (- (length raw-file-strats) (length file-strats))
                     (- (length raw-db-strats) (length db-strats)))))
    (format t "[KB] 🗂️ Startup library dirs: ~{~a~^,~}.~%" *kb-init-library-dirs*)
    (format t "[KB] 🧭 Startup DB restore scope: B/A/S/LEGEND only (~d rows).~%"
            (length db-strats))
    (when (> dropped 0)
      (format t "[KB] ⚠️ Dropped ~d invalid strategies during init.~%" dropped))
    ;; Canonicalize loaded TFs to internal minutes(int).
    (dolist (s file-strats) (%normalize-strategy-timeframe! s))
    (dolist (s db-strats) (%normalize-strategy-timeframe! s))

    ;; Merge lists, prioritizing DB records except archived DB collisions
    ;; where file has active rank (explicit revive path).
    (let ((kb (copy-list db-strats))
          (kb-index (make-hash-table :test 'equal))
          (revived-db-entries (make-hash-table :test 'eq))
          (revive-source-index (and *kb-init-revive-require-active-library-file*
                                    (%build-kb-init-revive-source-index)))
          (revive-limit *kb-init-max-archived-duplicate-revives*)
          (revive-targets '())
          (replaced-archived-duplicates 0)
          (revive-skipped-by-cap 0)
          (revive-lookups 0)
          (revive-lookup-hits 0)
          (purged-archive-files 0))
      (dolist (dbs kb)
        (let ((name (and (strategy-p dbs) (strategy-name dbs))))
          (when name
            (setf (gethash name kb-index) dbs))))
      (dolist (fs file-strats)
        (let* ((name (and (strategy-p fs) (strategy-name fs)))
               (existing (and name (gethash name kb-index))))
          (cond
            ((null existing)
             (push fs kb)
             (when name
               (setf (gethash name kb-index) fs))
             ;; When DB restore is active-only, archived collisions won't be deserialized.
             ;; Probe rank by name for ACTIVE file strategies and queue revive if archived.
             (when (and name
                        (%active-rank-for-kb-init-p (strategy-rank fs)))
               (incf revive-lookups)
               (let ((db-rank (ignore-errors
                                (execute-single "SELECT rank FROM strategies WHERE name = ?"
                                                name))))
                 (when (and db-rank
                            (%archived-rank-for-kb-init-p db-rank)
                            (or (not *kb-init-revive-require-active-library-file*)
                                (gethash name revive-source-index)))
                   (incf revive-lookup-hits)
                   (if (or (null revive-limit)
                           (< replaced-archived-duplicates revive-limit))
                       (progn
                         (unless (find name revive-targets
                                       :key #'strategy-name :test #'string=)
                           (push fs revive-targets))
                         (incf replaced-archived-duplicates))
                       (incf revive-skipped-by-cap))))))
            ((and (%archived-rank-for-kb-init-p (strategy-rank existing))
                  (%active-rank-for-kb-init-p (strategy-rank fs))
                  (or (not *kb-init-revive-require-active-library-file*)
                      (and name (gethash name revive-source-index))))
             (if (or (null revive-limit)
                     (< replaced-archived-duplicates revive-limit))
                 (progn
                   (setf (gethash existing revived-db-entries) t)
                   (push fs kb)
                   (setf (gethash name kb-index) fs)
                   (unless (find name revive-targets
                                 :key #'strategy-name :test #'string=)
                     (push fs revive-targets))
                   (incf replaced-archived-duplicates))
                  (incf revive-skipped-by-cap))))))
      (when (> (hash-table-count revived-db-entries) 0)
        (setf kb (remove-if (lambda (s) (gethash s revived-db-entries)) kb)))
      (setf *strategy-knowledge-base* kb)
      (when (> revive-lookups 0)
        (format t "[KB] 🔎 Archived rank lookups for active file strategies: ~d (hits: ~d).~%"
                revive-lookups revive-lookup-hits))
      (when (> replaced-archived-duplicates 0)
        (format t "[KB] ♻️ Revived ~d archived DB duplicates from active Library entries.~%"
                replaced-archived-duplicates))
      (when (> revive-skipped-by-cap 0)
        (format t "[KB] ⏳ Deferred ~d archived duplicate revives (cap=~a).~%"
                revive-skipped-by-cap
                (or revive-limit "NIL")))
      (dolist (target revive-targets)
        (handler-case
            (let ((*allow-archived-rank-resurrection-write* t))
              (upsert-strategy target)
              (incf purged-archive-files
                    (%purge-revived-strategy-archive-files (strategy-name target))))
          (error (e)
            (format t "[KB] ⚠️ Failed to persist revived strategy ~a: ~a~%"
                    (strategy-name target) e))))
      (when (> purged-archive-files 0)
        (format t "[KB] 🧹 Purged ~d stale archive files for revived strategies.~%"
                purged-archive-files))))

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

    ;; V50.6 hardening: never keep unevaluated entries in KB population.
    (let* ((before (length *strategy-knowledge-base*)))
      (setf *strategy-knowledge-base*
            (remove-if (lambda (s)
                         (%unevaluated-rank-for-kb-init-p (strategy-rank s)))
                       *strategy-knowledge-base*))
      (let ((pruned (- before (length *strategy-knowledge-base*))))
        (when (> pruned 0)
          (format t "[KB] 🧹 Pruned ~d unevaluated strategies (NIL/INCUBATOR/SCOUT) during init.~%"
                  pruned))))
    
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
(defvar *rr-db-archive-cache* (make-hash-table :test 'equal)
  "Memoized flag: strategy name is archived in DB (:RETIRED/:GRAVEYARD).")

(defun active-strategy-p (strategy)
  (let ((rank (strategy-rank strategy)))
    (member rank '(:B :A :S :LEGEND) :test #'eq)))

(defun %normalize-rr-db-rank-token (rank)
  "Normalize DB rank token to uppercase without leading colon."
  (let* ((raw (cond
                ((null rank) "")
                ((symbolp rank) (symbol-name rank))
                (t (format nil "~a" rank))))
         (trimmed (string-upcase (string-trim '(#\Space #\Tab #\Newline) raw))))
    (if (and (> (length trimmed) 0)
             (char= (char trimmed 0) #\:))
        (subseq trimmed 1)
        trimmed)))

(defun %rr-db-archived-rank-p (rank)
  "Return T when DB rank is archived."
  (member (%normalize-rr-db-rank-token rank)
          '("RETIRED" "GRAVEYARD")
          :test #'string=))

(defun %rr-name-archived-in-db-p (name)
  "Return T when strategy NAME is archived in DB. Uses memoized cache."
  (multiple-value-bind (cached foundp) (gethash name *rr-db-archive-cache*)
    (if foundp
        cached
        (let* ((db-rank (ignore-errors
                          (execute-single "SELECT rank FROM strategies WHERE name = ?"
                                          name)))
               (archived (%rr-db-archived-rank-p db-rank)))
          (setf (gethash name *rr-db-archive-cache*) archived)
          archived))))

(defun format-phase1-bt-batch-message (requested total cursor &key cycle-completed)
  (format nil "🧪 **Phase 1 BT Batch (RR)**~%- Requested: ~d / ~d~%- Cursor: ~d / ~d~a"
          requested total cursor total
          (if cycle-completed "~%✅ **Phase1 BT Cycle Complete (RR)**" "")))

(defun rr-backtest-pending-count ()
  "Best-effort pending count for RR batch sizing."
  (if (fboundp 'backtest-pending-count)
      (backtest-pending-count)
      (let ((recv (if (boundp 'swimmy.main::*backtest-recv-count*)
                      swimmy.main::*backtest-recv-count*
                      0)))
        (max 0 (- swimmy.globals::*backtest-submit-count* recv)))))

(defun rr-batch-cap ()
  "Compute per-run RR dispatch cap from pending headroom and rate limits."
  (let* ((max-pending (max 1 (if (boundp 'swimmy.globals::*backtest-max-pending*)
                                 swimmy.globals::*backtest-max-pending*
                                 1000)))
         (pending-now (rr-backtest-pending-count))
         (available-slots (max 0 (- max-pending pending-now)))
         (rate-limit (if (boundp 'swimmy.globals::*backtest-rate-limit-per-sec*)
                         swimmy.globals::*backtest-rate-limit-per-sec*
                         0))
         (rate-cap (if (and (numberp rate-limit) (> rate-limit 0))
                       (max 1 (truncate rate-limit))
                       max-pending)))
    (max 0 (min max-pending available-slots rate-cap))))

(defun batch-backtest-knowledge ()
  "V48.0: Phase 1 BT with per-strategy symbol support + larger batch size.
   Backtests each strategy using its native symbol's candle data."
  (load-backtest-cache) 
  (setf swimmy.globals:*rr-backtest-results-buffer* nil)
  (setf swimmy.globals:*rr-backtest-start-time* (get-universal-time))
  
  (let* ((max-batch-size (rr-batch-cap))
         (active-strategies (remove-if-not #'active-strategy-p *strategy-knowledge-base*))
         (total (length active-strategies)))
    (when (zerop total)
      (setf swimmy.globals:*rr-expected-backtest-count* 0)
      (format t "[L] ⚠️ No active strategies for Phase1 BT. Skipping batch.~%")
      (return-from batch-backtest-knowledge nil))
    (when (<= max-batch-size 0)
      (setf swimmy.globals:*rr-expected-backtest-count* 0)
      (format t "[L] ⏳ RR batch paused (pending=~d max=~d rate=~d/s)~%"
              (rr-backtest-pending-count)
              (if (boundp 'swimmy.globals::*backtest-max-pending*)
                  swimmy.globals::*backtest-max-pending*
                  0)
              (if (boundp 'swimmy.globals::*backtest-rate-limit-per-sec*)
                  swimmy.globals::*backtest-rate-limit-per-sec*
                  0))
      (return-from batch-backtest-knowledge nil))
    (let* ((old-cursor *backtest-cursor*)
           (start-idx (mod *backtest-cursor* total))
           (batch-size (min total max-batch-size))
           (end-idx (min total (+ start-idx batch-size))))
    
    (when (or (zerop old-cursor) (>= old-cursor total))
      (setf *cycle-start-kb-size* total))
    
    (let* ((batch-strategies (subseq active-strategies start-idx end-idx))
           ;; Handle wrap-around without duplication
           (remaining (- batch-size (- end-idx start-idx)))
           (wrap-strategies (if (> remaining 0)
                                (subseq active-strategies 0 remaining)
                                nil))
           (final-batch (append batch-strategies wrap-strategies))
           (cached-count 0)
           (requested-count 0)
           (skipped-count 0))
      
      ;; Update cursor for next time
      (setf *backtest-cursor*
            (if (>= batch-size total)
                0
                (mod (+ start-idx batch-size) total)))
      
      ;; V48.0: Detect cycle completion (cursor wrapped around)
      ;; V50.9 Fix: Handle single-batch completion (old=0 -> new=0)
      (let ((cycle-completed (or (and (> old-cursor 0) (< *backtest-cursor* old-cursor))
                                 (and (= old-cursor 0) (= *backtest-cursor* 0) (> total 0)))))
        (format t "[L] 🧪 Batch testing ~d strategies (Round-Robin: ~d -> ~d)...~%" 
                (length final-batch) start-idx *backtest-cursor*)
        
        ;; V48.0: Use strategy's native symbol for candle data
        (dolist (strat final-batch)
          (let ((name (strategy-name strat)))
            (if (%rr-name-archived-in-db-p name)
                (incf skipped-count)
                (let* ((sym (or (strategy-symbol strat) "USDJPY"))
                       (snapshot (or (gethash sym *candle-histories*)
                                     *candle-history*
                                     (gethash "USDJPY" *candle-histories*))))
                  (if (and snapshot (> (length snapshot) 100))
                      (progn
                        ;; Apply cached metrics if available, but still request fresh BT
                        (let ((cached (get-cached-backtest name)))
                          (when cached
                            (incf cached-count)
                            ;; V48.5: Apply cached metrics to strategy and prune if weak
                            (let ((sharpe (float (getf cached :sharpe 0.0)))
                                  (pf (float (getf cached :profit-factor 0.0)))
                                  (wr (float (getf cached :win-rate 0.0)))
                                  (trades (getf cached :trades 0))
                                  (max-dd (float (getf cached :max-dd 0.0))))
                              (setf (strategy-sharpe strat) sharpe
                                    (strategy-profit-factor strat) pf
                                    (strategy-win-rate strat) wr
                                    (strategy-trades strat) trades)
                              (let ((founder-recovery-pass
                                      (and (fboundp 'founder-phase1-recovery-passed-p)
                                           (ignore-errors
                                             (founder-phase1-recovery-passed-p
                                              strat sharpe pf trades max-dd)))))
                                ;; Founder recovery survivors use a softer floor than generic RR prune.
                                (when (and (< sharpe 0.1)
                                           (not founder-recovery-pass))
                                  (prune-to-graveyard strat "Cached Sharpe < 0.1"))))))
                        (let ((dispatch-state (request-backtest strat :candles snapshot :symbol sym :suffix "-RR")))
                          (if (backtest-dispatch-accepted-p dispatch-state)
                              (progn
                                (incf requested-count)
                                (when (fboundp 'maybe-pause-after-backtest-dispatch)
                                  (maybe-pause-after-backtest-dispatch)))
                              (format t "[L] ⚠️ RR dispatch rejected: ~a (state=~a)~%"
                                      name dispatch-state))))
                      (progn
                        (incf skipped-count)
                        (format t "[L] ⚠️ Skipping BT (no candles) for ~a (~a)~%"
                                name sym))))))))
        
        ;; Expected count should reflect actual enqueued requests
        (setf swimmy.globals:*rr-expected-backtest-count* requested-count)
                     
        (format t "[L] 🏁 Batch Request Complete. Cached: ~d, Queued: ~d, Skipped: ~d (Cursor: ~d)~%" 
                cached-count requested-count skipped-count *backtest-cursor*)
        
        ;; Notify Discord only if actual requests were made
        (when (> requested-count 0)
          (notify-discord-alert
            (format-phase1-bt-batch-message requested-count total *backtest-cursor*
                                            :cycle-completed cycle-completed)
            :color 3066993))
        
        ;; V48.5: Throttled summary on cycle completion (Every 6 hours)
        ;; V49.5: Decoupled and Triggered at 90% (expert Panel)
        (when cycle-completed
          (format t "[L] 🔄 KB Backtest Cycle Complete! Sending throttled summary...~%")
          (setf *last-cycle-notify-time* (get-universal-time))
          (notify-backtest-summary :rr))))))

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
      ;; VWAP volume ratio strategies (orange threshold style)
      ((and indicator-name
            (or (search "vwapvr" indicator-name)
                (search "vwap-volume-ratio" indicator-name)))
       "vwapvr")
      ;; Volume profile / volume expansion families
      ((and indicator-name (search "vpoc" indicator-name)) "vpoc")
      ((and indicator-name (search "volsma" indicator-name)) "volsma")
      ((and indicator-name (search "vwap" indicator-name)) "vwap")
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
;;; KILL SYSTEM (V46.0 Expert Panel 2026-01-16)
;;; P1: Similarity-based pruning (min 3 per category)
;;; ==========================================

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

(defun %normalize-kill-reason-code (reason reason-code)
  (or reason-code
      (when (stringp reason)
        (cond
          ((search "Max Age Retirement" reason :test #'char-equal) :max-age-retirement)
          ((search "Stagnant C-Rank" reason :test #'char-equal) :stagnant-crank)
          (t nil)))))

(defun kill-strategy (name reason &key reason-code)
  "P0: Soft Kill - Shelve indefinitely instead of permanent deletion (Expert Panel 2026-01-16)"
  (let ((s (find-strategy-object name))
        (code (%normalize-kill-reason-code reason reason-code)))
    (when s
      (format t "~%[L] 🛡️ SOFT KILL: ~a (~a) -> Shelved indefinitely~%" name reason)
      (setf (strategy-status s) :killed)
      (setf (strategy-status-reason s) (format nil "SOFT_KILL: ~a" reason))
      ;; Persist status to DB so restarts don't re-trigger kill/alerts
      (ignore-errors (upsert-strategy s))
      ;; Notify
      (when (fboundp 'notify-discord-alert)
        (cond
          ((eq code :max-age-retirement)
           (swimmy.core::queue-max-age-retire name))
          ((eq code :stagnant-crank)
           (swimmy.core::queue-stagnant-crank-retire name))
          (t
           (notify-discord-alert 
            (format nil "🛡️ **Strategy Soft-Killed (Cooldown)**~%Name: ~a~%Reason: ~a~%Action: Shelved for future review" name reason)
            :color 15158332)))))))

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
        (send-to-graveyard strat (format nil "Grading [D]: Negative Sharpe ~,2f. Needs optimization." sharpe))
        (return-from evaluate-strategy-performance))

      ;; TIER 2: REJECT (0 <= Sharpe < 0.6) - Noise
      (when (< sharpe 0.6)
        (send-to-graveyard strat (format nil "Grading [C]: Weak Sharpe ~,2f (< 0.6)" sharpe))
        (return-from evaluate-strategy-performance))

      ;; TIER 3: CONDITIONAL (0.6 <= Sharpe < 1.0) - Retire for Review
      ;; Note: We retire these so they don't consume resources unless explicitly promoted
      (when (< sharpe 1.0)
        (send-to-graveyard strat (format nil "Grading [B]: Med Sharpe ~,2f (< 1.0)" sharpe))
        (return-from evaluate-strategy-performance))

      ;; TIER 4: SURVIVE (Sharpe >= 1.0)
      
      ;; QUALITY CHECKS (Rule 2 & 3)
      (when (> trades 20) ; Only apply with sufficient sample
        ;; Rule 3: PF Floor
        (when (< profit-factor 1.2)
          (send-to-graveyard strat (format nil "Grading [D]: Low PF ~,2f (< 1.2)" profit-factor))
          (return-from evaluate-strategy-performance))
          
        ;; Rule 3: Fake PF Detection (Martingale/Grid signature)
        (when (and (> profit-factor 2.0) (< sharpe 1.0))
          (send-to-graveyard strat (format nil "Grading [D]: Fake PF Trap (PF ~,2f but Sharpe ~,2f)" profit-factor sharpe))
          (return-from evaluate-strategy-performance))
          
        ;; Rule 2: High Win-Rate Trap
        (when (and (> win-rate 70) (< profit-factor 1.3))
          (send-to-graveyard strat (format nil "Grading [D]: High WR Trap (WR ~,1f% but PF ~,2f)" win-rate profit-factor))
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
