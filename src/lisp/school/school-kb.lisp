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
(defparameter *founder-graveyard-bypass-enabled* nil
  "When T, founder entries can bypass graveyard pattern rejection once (manual re-evaluation).")

(defparameter *phase1-pending-candidates* (make-hash-table :test 'equal)
  "Candidates queued for Phase1 screening but not yet admitted to KB.
Keyed by strategy name.")

(defun take-phase1-pending-candidate (name)
  "Pop pending Phase1 candidate by NAME. Returns candidate or NIL."
  (when (and (stringp name) (> (length name) 0))
    (bt:with-lock-held (*kb-lock*)
      (let ((candidate (gethash name *phase1-pending-candidates*)))
        (when candidate
          (remhash name *phase1-pending-candidates*))
        candidate))))

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
          (flet ((load-from-structured-sql ()
                   (let ((rows (execute-to-list
                                "SELECT sl, tp, symbol
                                   FROM strategies
                                  WHERE rank = ':GRAVEYARD'
                                    AND sl IS NOT NULL
                                    AND tp IS NOT NULL")))
                     (remove-if #'null
                                (mapcar (lambda (row)
                                          (let ((sl (first row))
                                                (tp (second row))
                                                (symbol (third row)))
                                            (when (and (numberp sl) (numberp tp))
                                              (normalize-graveyard-entry
                                               (list :sl (float sl 0.0)
                                                     :tp (float tp 0.0)
                                                     :symbol (if (and (stringp symbol)
                                                                      (> (length symbol) 0))
                                                                 symbol
                                                                 "USDJPY"))))))
                                        rows))))
                 (load-from-sexp-sql ()
                   (let ((rows (execute-to-list
                                "SELECT data_sexp FROM strategies WHERE rank = ':GRAVEYARD'")))
                     (remove-if #'null
                                (mapcar (lambda (row)
                                          (let ((sexp-str (first row)))
                                            (let ((entry (swimmy.core:safe-read-sexp sexp-str :package :swimmy.school)))
                                              (when entry
                                                (normalize-graveyard-entry entry)))))
                                        rows))))
                 (load-from-file ()
                   (when (probe-file "data/memory/graveyard.sexp")
                     (with-open-file (stream "data/memory/graveyard.sexp" :direction :input :if-does-not-exist nil)
                       (let ((*package* (find-package :swimmy.school)))
                         (remove-if #'null
                                    (loop for data = (handler-case (read stream nil nil) (error () nil))
                                          while data collect (normalize-graveyard-entry data))))))))
            (setf *graveyard-cache*
                  (or (handler-case
                          (progn
                            (init-db)
                            (or (load-from-structured-sql)
                                (load-from-sexp-sql)))
                        (error () nil))
                      (load-from-file))))))))

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

(defun normalize-rank-token-for-competition (rank)
  "Normalize rank token into a keyword used by competition filters."
  (cond
    ((keywordp rank) rank)
    ((symbolp rank) (intern (string-upcase (symbol-name rank)) :keyword))
    ((stringp rank)
     (let* ((trimmed (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return) rank)))
            (plain (if (and (> (length trimmed) 0)
                            (char= (char trimmed 0) #\:))
                       (subseq trimmed 1)
                       trimmed)))
       (and (> (length plain) 0)
            (intern plain :keyword))))
    (t nil)))

(defun archived-rank-for-competition-p (rank)
  "Return T when rank is archived and should not compete in duplicate/correlation checks."
  (member (normalize-rank-token-for-competition rank)
          '(:GRAVEYARD :RETIRED)
          :test #'eq))

(defun strategy-competition-eligible-p (strategy)
  "Archived strategies should not block fresh admissions."
  (and strategy
       (not (archived-rank-for-competition-p (strategy-rank strategy)))))

(defun is-logic-duplicate-p (strategy kb)
  "Check if logic already exists in KB (under different name)."
  (let ((target-hash (compute-strategy-hash strategy)))
    (dolist (s kb)
      (when (and (strategy-competition-eligible-p s)
                 (= (compute-strategy-hash s) target-hash))
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

(defun strategy-correlation-scope-key (strategy)
  "Return a normalized correlation scope key for STRATEGY.

Correlation screening (Jaccard/Highlander) is only meaningful within the same
trading universe. For multi-symbol evolution, we scope correlation to:
  (timeframe × direction × symbol)
so identical logic across different symbols is allowed."
  (let* ((tf-raw (and (fboundp 'strategy-timeframe) (strategy-timeframe strategy)))
         ;; Scope TF is bucketed to keep correlation keys finite under arbitrary TF exploration.
         (tf (cond
               ((fboundp 'get-tf-bucket-minutes)
                (get-tf-bucket-minutes (or tf-raw 1)))
               ((fboundp 'get-tf-minutes)
                (get-tf-minutes (or tf-raw 1)))
               (t (or tf-raw 1))))
         (dir (and (fboundp 'strategy-direction) (strategy-direction strategy)))
         (sym (and (fboundp 'strategy-symbol) (strategy-symbol strategy))))
    (list (or tf 1)
          (or dir :BOTH)
          (string-upcase (or sym "USDJPY")))))

(defun find-correlated-strategy (strategy kb &optional (threshold 0.9))
  "Find structurally correlated strategies (Simons Challenge).
   Returns (values match score).

V50.6: Correlation is scoped to the same (timeframe × direction × symbol)."
  (let ((target-scope (strategy-correlation-scope-key strategy)))
    (dolist (s kb)
      (when (and (strategy-competition-eligible-p s)
                 (not (string= (strategy-name s) (strategy-name strategy)))
                 (equal target-scope (strategy-correlation-scope-key s)))
        (let ((score (calculate-jaccard-similarity strategy s)))
          (when (> score threshold)
            (return-from find-correlated-strategy (values s score)))))))
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

  ;; Canonicalize timeframe to internal minutes(int) at the KB ingress.
  (when (and (fboundp '%normalize-timeframe-minutes)
             (fboundp 'strategy-timeframe))
    (setf (strategy-timeframe strategy)
          (%normalize-timeframe-minutes (strategy-timeframe strategy))))
  
  (bt:with-lock-held (*kb-lock*)
      (let ((name (strategy-name strategy))
            (breeder-variant-approved nil)
            (defer-phase1-admission nil))
      
      ;; 1. Name Duplicate Check
      ;; NOTE: Archived name collisions should not block founder re-entry.
      ;; Avoid removing archived rows from in-memory KB here because repeated
      ;; large-list copies can trigger severe GC pressure during bulk recruit.
      (let ((active-name-duplicate
              (and (stringp name)
                   (find-if (lambda (existing)
                              (let ((existing-name (and existing (strategy-name existing))))
                                (and (stringp existing-name)
                                     (string= existing-name name)
                                     (strategy-competition-eligible-p existing))))
                            *strategy-knowledge-base*))))
        (when active-name-duplicate
          (format t "[KB] ⚠️ Duplicate (Name): ~a already exists~%" name)
          (return-from add-to-kb nil))
        (when (and (stringp name)
                   (find-if (lambda (existing)
                              (let ((existing-name (and existing (strategy-name existing))))
                                (and (stringp existing-name)
                                     (string= existing-name name)
                                     (not (strategy-competition-eligible-p existing)))))
                            *strategy-knowledge-base*))
          (format t "[KB] ♻️ Archived duplicate ignored: ~a~%" name)))
      
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
      ;; Performance note: founder bypass is an explicit operator override.
      ;; Skip expensive graveyard-pattern evaluation entirely in this branch.
      (unless *startup-mode*
        (if (and (eq source :founder) *founder-graveyard-bypass-enabled*)
            (format t "[KB] ♻️ Founder bypassed graveyard pattern once: ~a~%" name)
            (when (is-graveyard-pattern-p strategy)
              (cond
                ((and (eq source :breeder) breeder-variant-approved)
                 (format t "[KB] ♻️ Breeder variant bypassed graveyard pattern once: ~a~%" name))
                ((and (eq source :breeder)
                      require-bt
                      *breeder-graveyard-bypass-for-phase1-enabled*)
                 (format t "[KB] ♻️ Breeder Phase1 bypassed graveyard pattern once: ~a~%" name))
                (t
                 (format t "[KB] 🪦 Rejected: ~a matches GRAVEYARD pattern!~%" name)
                 (return-from add-to-kb nil))))))
      
      ;; 2. BT Validation (Phase 1 Screening Gate)
      (when require-bt
        (cond
          ;; V50.6: Breeder products must always pass Phase 1 before B-rank.
          ;; During startup we skip mandatory queueing to avoid replay storms.
          ((and (eq source :breeder) (not *startup-mode*))
           (format t "[KB] 🧪 Breeder candidate: ~a. Queueing mandatory Phase 1 Screening...~%" name)
           (setf defer-phase1-admission t))
          (t
           (let ((sharpe (or (strategy-sharpe strategy) 0.0)))
             (when (< sharpe *phase1-min-sharpe*)
               ;; V50.2: Automatic Screening Injection
               (format t "[KB] 🐣 Newborn: ~a (Sharpe ~a). Queueing for Phase 1 Screening...~%" name sharpe)
               (setf defer-phase1-admission t))))))

      ;; Defer admission until Phase1 result arrives.
      (when defer-phase1-admission
        (setf (strategy-rank strategy) nil)
        (setf (gethash name *phase1-pending-candidates*) strategy)
        ;; Persist pending candidate so Phase1 result fallback can recover after restart.
        (handler-case
            (upsert-strategy strategy)
          (error (e)
            (format t "[KB] ⚠️ Failed to persist Phase1 pending candidate ~a: ~a~%" name e)))
        (run-phase-1-screening strategy)
        (format t "[KB] ⏸️ Deferred admission until Phase1 result: ~a~%" name)
        (return-from add-to-kb (values t :queued-phase1)))

      ;; 3. Add to KB
      (push strategy *strategy-knowledge-base*)
      ;; V50.4: Only assign :B rank if it actually meets the criteria.
      ;; This prevents unvalidated junk from diluting the base.
      (when (and (not (strategy-rank strategy))
                 (check-rank-criteria strategy :B))
        (%ensure-rank-no-lock strategy :B "New Strategy Induction (Validation Passed)"))
      (format t "[KB] ✅ Added: ~a (Source: ~a, Rank: ~s)~%" name source (strategy-rank strategy))
      
      ;; 4. Add to active pools.
      (if (fboundp 'add-strategy-to-active-pools)
          (add-strategy-to-active-pools strategy)
          (let ((cat (categorize-strategy strategy)))
            (when (boundp '*category-pools*)
              (pushnew strategy (gethash cat *category-pools* nil) :test #'eq))
            (when (boundp '*regime-pools*)
              (let ((regime-class (strategy-regime-class strategy)))
                (pushnew strategy (gethash regime-class *regime-pools* nil) :test #'eq)))))
      
      ;; 5. Persist to SQL (V49.8)
      (upsert-strategy strategy)
      
      ;; 6. Notification (suppress during startup)
      (when (and notify (not *startup-mode*))
        (notify-recruit-unified strategy source))
      
      (values t :added))))

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
