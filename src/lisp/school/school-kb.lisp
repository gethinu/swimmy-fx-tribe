;;; school-kb.lisp - Knowledge Base Single Entry Point
;;; P8: Strategy Pipeline Redesign
;;; Expert Panel Approved (2026-01-21)
;;;
;;; This file provides THE ONLY entry point for adding strategies to KB.
;;; All sources (Founder, Breeder) must go through add-to-kb.

(in-package :swimmy.school)

;;; ============================================================================
;;; STATE
;;; ============================================================================

(defvar *startup-mode* t
  "When T, suppress Discord notifications for strategy additions.
   Set to NIL after initial load completes.")

(defparameter *breeder-graveyard-bypass-for-phase1-enabled* t
  "When T, breeder entries requiring Phase1 can bypass graveyard pattern rejection once.")

;; *kb-lock* moved to school-state.lisp for early initialization

;;; ============================================================================
;;; GRAVEYARD AVOIDANCE (Taleb)
;;; ============================================================================

(defvar *graveyard-cache* nil)
(defvar *last-graveyard-load* 0)

(defun normalize-graveyard-entry (entry)
  "Normalize graveyard entry into a plist with :sl/:tp/:symbol keys."
  (cond
    ((null entry) nil)
    ;; Already a plist pattern
    ((and (listp entry) (or (getf entry :sl) (getf entry :tp)))
     entry)
    ;; Strategy struct stored in SQL data_sexp
    ((and (fboundp 'strategy-p) (strategy-p entry))
     (list :name (strategy-name entry)
           :timeframe (strategy-timeframe entry)
           :direction (strategy-direction entry)
           :symbol (or (strategy-symbol entry) "USDJPY")
           :sl (strategy-sl entry)
           :tp (strategy-tp entry)
           :sharpe (strategy-sharpe entry)
           :profit-factor (strategy-profit-factor entry)
           :win-rate (strategy-win-rate entry)
           :max-dd (strategy-max-dd entry)))
    (t nil)))

(defun load-graveyard-cache ()
  "Load failure patterns from SQL (V49.8) or graveyard.sexp."
  (let ((now (get-universal-time)))
    (if (and *graveyard-cache* (< (- now *last-graveyard-load*) 300))
        *graveyard-cache*
        (progn
          (setf *last-graveyard-load* now)
          (setf *graveyard-cache*
                (or (handler-case
                        (progn
                          (init-db)
                          (let ((rows (execute-to-list
                                       "SELECT data_sexp FROM strategies WHERE rank = ':GRAVEYARD'")))
                            (remove-if #'null
                                       (mapcar (lambda (row)
                                                 (let ((sexp-str (first row)))
                                                   (let ((entry (swimmy.core:safe-read-sexp sexp-str :package :swimmy.school)))
                                                     (when entry
                                                       (normalize-graveyard-entry entry)))))
                                               rows))))
                      (error () nil))
                    (when (probe-file "data/memory/graveyard.sexp")
                      (with-open-file (stream "data/memory/graveyard.sexp" :direction :input :if-does-not-exist nil)
                        (let ((*package* (find-package :swimmy.school)))
                          (remove-if #'null
                                     (loop for data = (handler-case (read stream nil nil) (error () nil))
                                           while data collect (normalize-graveyard-entry data))))))))))))

(defun is-graveyard-pattern-p (strategy)
  "Check if strategy matches a known failure pattern."
  (let ((patterns (load-graveyard-cache))
        (s-sl (strategy-sl strategy))
        (s-tp (strategy-tp strategy))
        (s-sym (or (strategy-symbol strategy) "USDJPY")))
    (block match-loop
      (dolist (p patterns)
        (let ((g-sl (getf p :sl))
              (g-tp (getf p :tp))
              (g-sym (getf p :symbol)))
          ;; Match Criteria:
          ;; 1. Same Symbol
          ;; 2. SL and TP within 5% tolerance
          (when (and g-sym (string= s-sym g-sym)
                     (numberp g-sl) (numberp s-sl)
                     (numberp g-tp) (numberp s-tp)
                     (< (abs (- s-sl g-sl)) (* 0.05 g-sl))
                     (< (abs (- s-tp g-tp)) (* 0.05 g-tp))
                     ;; V49.1: EXEMPTION for A-Rank Rescues (Sharpe >= 0.3)
                     (not (>= (or (strategy-sharpe strategy) 0) 0.3)))
            (return-from match-loop t))))
      nil)))

;;; ============================================================================
;;; LOGIC INTEGRITY & SYMBOLIC HASHING (Phase 24 - Graham/Ng)
;;; ============================================================================

;; ----------------------------------------------------------------------------
;; Logic Integrity check moved to school-ast.lisp (canonicalize-logic)

(defun compute-strategy-hash (strategy)
  "Compute a structural hash of the strategy logic (indicators + entry/exit).
   This creates a canonical representation of 'what the strategy does'
   ignoring names, whitespace, or operator order (commutative)."
  (let* ((entry (canonicalize-logic (strategy-entry strategy)))
         (exit (canonicalize-logic (strategy-exit strategy)))
         (indicators (canonicalize-logic (strategy-indicators strategy)))
         (sym (or (strategy-symbol strategy) "USDJPY"))
         (tf (or (strategy-timeframe strategy) 1))
         ;; We still use prin1-to-string on the CANONICALIZED list to get a stable hash
         (canonical (prin1-to-string (list sym tf entry exit indicators))))
    (sxhash canonical)))

(defun is-logic-duplicate-p (strategy kb)
  "Check if logic already exists in KB (under different name)."
  (let ((target-hash (compute-strategy-hash strategy)))
    (dolist (s kb)
      (when (= (compute-strategy-hash s) target-hash)
        (return-from is-logic-duplicate-p s)))
    nil))

(defparameter *breeder-variant-min-relative-diff* 0.15
  "Minimum relative SL/TP delta to treat breeder child as a meaningful logic variant.")

(defun %relative-diff (a b)
  (let* ((a (float a 1.0))
         (b (float b 1.0))
         (den (max 1.0e-6 (abs b))))
    (/ (abs (- a b)) den)))

(defun breeder-parameter-variant-p (candidate incumbent)
  "Return T if breeder candidate differs enough in SL/TP from incumbent."
  (let ((c-sl (strategy-sl candidate))
        (c-tp (strategy-tp candidate))
        (i-sl (strategy-sl incumbent))
        (i-tp (strategy-tp incumbent)))
    (and (numberp c-sl) (numberp c-tp)
         (numberp i-sl) (numberp i-tp)
         (or (>= (%relative-diff c-sl i-sl) *breeder-variant-min-relative-diff*)
             (>= (%relative-diff c-tp i-tp) *breeder-variant-min-relative-diff*)))))

(defun extract-logic-atoms (sexp)
  "Recursively extract all atoms (indicators, ops, numbers) from logic tree.
   Returns a flat list of symbols/numbers."
  (cond 
    ((null sexp) nil)
    ((atom sexp) (list sexp))
    (t (append (extract-logic-atoms (car sexp))
               (extract-logic-atoms (cdr sexp))))))

(defun calculate-jaccard-similarity (s1 s2)
  "Calculate Jaccard Similarity of logic atoms between two strategies.
   Correlation = |Intersection| / |Union|.
   Returns float 0.0 to 1.0."
  (let* ((atoms1 (remove-duplicates (append (extract-logic-atoms (strategy-entry s1))
                                            (extract-logic-atoms (strategy-exit s1)))
                                    :test #'equal))
         (atoms2 (remove-duplicates (append (extract-logic-atoms (strategy-entry s2))
                                            (extract-logic-atoms (strategy-exit s2)))
                                    :test #'equal))
         (intersection (intersection atoms1 atoms2 :test #'equal))
         (union (union atoms1 atoms2 :test #'equal)))
    (if (null union) 0.0
        (/ (length intersection) (float (length union))))))

(defun find-correlated-strategy (strategy kb &optional (threshold 0.9))
  "Find structurally correlated strategies (Simons Challenge).
   Returns (values match score)."
  (dolist (s kb)
    (unless (string= (strategy-name s) (strategy-name strategy))
      (let ((score (calculate-jaccard-similarity strategy s)))
        (when (> score threshold)
          (return-from find-correlated-strategy (values s score))))))
  (values nil 0.0))

;;; ============================================================================
;;; SINGLE ENTRY POINT
;;; ============================================================================

(defun add-to-kb (strategy source &key (notify t) (require-bt t))
  "THE ONLY FUNCTION to add strategies to Knowledge Base.
   
   P8 Expert Panel Conditions:
   - Graham: Breeder生成物は必ずBT通過
   - López de Prado: Founderも最低限BT検証
   
   V49.0 Expert Panel: Graveyard Check (Taleb)
   
   Arguments:
     STRATEGY - Strategy struct to add
     SOURCE   - Keyword: :founder or :breeder
     :NOTIFY  - Send Discord notification (default T)
     :REQUIRE-BT - Require Sharpe >= 0.1 (default T, for B-RANK gate)
   
   Returns:
     T if added, NIL if rejected"
  
  (when (null strategy)
    (format t "[KB] ❌ Cannot add NIL strategy~%")
    (return-from add-to-kb nil))
  
  (bt:with-lock-held (*kb-lock*)
    (let ((name (strategy-name strategy))
          (breeder-variant-approved nil))
      
      ;; 1. Name Duplicate Check
      (when (find name *strategy-knowledge-base* :key #'strategy-name :test #'string=)
        (format t "[KB] ⚠️ Duplicate (Name): ~a already exists~%" name)
        (return-from add-to-kb nil))
      
      ;; 1.2 Logic Duplicate Check (Symbolic Hashing)
      (let ((dupe (is-logic-duplicate-p strategy *strategy-knowledge-base*)))
        (when dupe
          (if (and (eq source :breeder)
                   (breeder-parameter-variant-p strategy dupe))
              (progn
                (setf breeder-variant-approved t)
                (format t "[KB] ♻️ Breeder logic variant accepted: ~a vs ~a (SL/TP differs).~%"
                        name (strategy-name dupe)))
              (progn
                (format t "[KB] ⚠️ Duplicate (Logic): ~a is logically identical to ~a. Rejected.~%"
                        name (strategy-name dupe))
                (return-from add-to-kb nil)))))
      
      ;; 1.3 Logic Correlation Check (Simons Challenge / Musk Highlander Rule)
      (multiple-value-bind (match score) (find-correlated-strategy strategy *strategy-knowledge-base* 0.95)
        (when match
           (let ((new-sharpe (or (strategy-sharpe strategy) 0.0))
                 (old-sharpe (or (strategy-sharpe match) 0.0)))
             (cond
               ;; Case 0: Breeder variant with meaningful SL/TP delta is allowed.
               ((and (eq source :breeder)
                     (breeder-parameter-variant-p strategy match))
                (setf breeder-variant-approved t)
                (format t "[KB] ♻️ Breeder correlation variant accepted: ~a vs ~a (~d% similar).~%"
                        name (strategy-name match) (round (* 100 score))))
               ;; Case A: New is significantly better (Highlander Rule)
               ((> new-sharpe (* old-sharpe 1.05))
                (format t "[KB] ⚔️ HIGHLANDER: ~a (S=~,2f) defeats redundant ~a (S=~,2f). Replacing!~%"
                        name new-sharpe (strategy-name match) old-sharpe)
                ;; Demote old to Graveyard (or Archive)
                (setf (strategy-rank match) :graveyard)
                (setf *strategy-knowledge-base* (delete match *strategy-knowledge-base*))
                ;; Proceed to add New (fall through)
                )
               ;; Case B: New is worse or similar -> Reject
               (t
                (format t "[KB] 🛡️ Correlation: ~a is ~d% similar to ~a. Incumbent holds (S=~,2f vs ~,2f). Rejected.~%" 
                        name (round (* 100 score)) (strategy-name match) old-sharpe new-sharpe)
                (return-from add-to-kb nil))))))
        
      ;; 1.5 Graveyard Check (V49.0 Taleb)
      (when (and (not *startup-mode*) (is-graveyard-pattern-p strategy))
        (cond
          ((and (eq source :breeder) breeder-variant-approved)
           (format t "[KB] ♻️ Breeder variant bypassed graveyard pattern once: ~a~%" name))
          ((and (eq source :breeder)
                require-bt
                *breeder-graveyard-bypass-for-phase1-enabled*)
           (format t "[KB] ♻️ Breeder Phase1 bypassed graveyard pattern once: ~a~%" name))
          (t
           (format t "[KB] 🪦 Rejected: ~a matches GRAVEYARD pattern!~%" name)
           (return-from add-to-kb nil))))
      
      ;; 2. BT Validation (Phase 1 Screening Gate)
      (when require-bt
        (cond
          ;; V50.6: Breeder products must always pass Phase 1 before B-rank.
          ;; During startup we skip mandatory queueing to avoid replay storms.
          ((and (eq source :breeder) (not *startup-mode*))
           (format t "[KB] 🧪 Breeder candidate: ~a. Queueing mandatory Phase 1 Screening...~%" name)
           (setf (strategy-rank strategy) :incubator)
           (run-phase-1-screening strategy))
          (t
           (let ((sharpe (or (strategy-sharpe strategy) 0.0)))
             (when (< sharpe *phase1-min-sharpe*)
               ;; V50.2: Automatic Screening Injection
               (format t "[KB] 🐣 Newborn: ~a (Sharpe ~a). Queueing for Phase 1 Screening...~%" name sharpe)
               (setf (strategy-rank strategy) :incubator)
               (run-phase-1-screening strategy)
               ;; We continue to add it as :incubator.
               ;; NOTE: It will fall through to Step 3.
               )))))
      
      ;; 3. Add to KB
      (push strategy *strategy-knowledge-base*)
      ;; V50.4: Only assign :B rank if it actually meets the criteria.
      ;; This prevents unvalidated junk from diluting the base.
      (when (and (not (strategy-rank strategy))
                 (check-rank-criteria strategy :B))
        (%ensure-rank-no-lock strategy :B "New Strategy Induction (Validation Passed)"))
      (format t "[KB] ✅ Added: ~a (Source: ~a, Rank: ~s)~%" name source (strategy-rank strategy))
      
      ;; 4. Add to category pool
      (let ((cat (categorize-strategy strategy)))
        (when (boundp '*category-pools*)
          (push strategy (gethash cat *category-pools*))))
      
      ;; 5. Persist to SQL (V49.8)
      (upsert-strategy strategy)
      
      ;; 6. Notification (suppress during startup)
      (when (and notify (not *startup-mode*))
        (notify-recruit-unified strategy source))
      
      t)))

(defun notify-recruit-unified (strategy source)
  "Send unified recruitment notification to Discord.
   P8: Single notification format for all sources."
  (let ((name (strategy-name strategy))
        (cat (categorize-strategy strategy))
        (sharpe (or (strategy-sharpe strategy) 0.0)))
    (swimmy.core:notify-discord-recruit
     (format nil "📣 **Strategy Recruitment**
━━━━━━━━━━━━━━━━━━━━━━
🏷️ Source: ~a
📊 Name: `~a`
🏷️ Category: ~a
📈 Sharpe: ~,2f"
             source name cat sharpe)
     :color (if (eq source :founder) 3447003 #x9B59B6))))

(defun end-startup-mode ()
  "Call this after initial system load to enable notifications."
  (setf *startup-mode* nil)
  (format t "[KB] 🔔 Startup mode ended. Notifications enabled.~%"))

;;; ============================================================================
;;; VALIDATION UTILITIES
;;; ============================================================================

(defun validate-strategy-for-kb (strategy)
  "Validate strategy meets B-RANK criteria.
   Returns (values ok-p reason)"
  (cond
    ((null strategy)
     (values nil "Strategy is NIL"))
    ((null (strategy-name strategy))
     (values nil "Strategy has no name"))
    ((< (or (strategy-sharpe strategy) 0) 0.1)
     (values nil (format nil "Sharpe ~,2f < 0.1" (or (strategy-sharpe strategy) 0))))
    (t
     (values t "OK"))))

(format t "[L] 📦 school-kb.lisp loaded - Single Entry Point for KB~%")
