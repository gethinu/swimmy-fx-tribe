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

;;; ============================================================================
;;; SINGLE ENTRY POINT
;;; ============================================================================

(defun add-to-kb (strategy source &key (notify t) (require-bt t))
  "THE ONLY FUNCTION to add strategies to Knowledge Base.
   
   P8 Expert Panel Conditions:
   - Graham: Breeder生成物は必ずBT通過
   - López de Prado: Founderも最低限BT検証
   
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
  
  (let ((name (strategy-name strategy)))
    
    ;; 1. Duplicate Check
    (when (find name *strategy-knowledge-base* :key #'strategy-name :test #'string=)
      (format t "[KB] ⚠️ Duplicate: ~a already exists~%" name)
      (return-from add-to-kb nil))
    
    ;; 2. BT Validation (B-RANK gate: Sharpe >= 0.1)
    (when require-bt
      (let ((sharpe (or (strategy-sharpe strategy) 0.0)))
        (when (< sharpe 0.1)
          (format t "[KB] 🚫 Rejected: ~a (Sharpe ~,2f < 0.1)~%" name sharpe)
          (return-from add-to-kb nil))))
    
    ;; 3. Add to KB
    (push strategy *strategy-knowledge-base*)
    ;; V48.0: Initialize rank to B (passed Phase 1 BT to get here)
    (unless (strategy-rank strategy)
      (setf (strategy-rank strategy) :B))
    (format t "[KB] ✅ Added: ~a (Source: ~a, Rank: ~a)~%" name source (strategy-rank strategy))
    
    ;; 4. Add to category pool
    (let ((cat (categorize-strategy strategy)))
      (when (boundp '*category-pools*)
        (push strategy (gethash cat *category-pools*))))
    
    ;; 5. Notification (suppress during startup)
    (when (and notify (not *startup-mode*))
      (notify-recruit-unified strategy source))
    
    t))

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
🏛️ Clan: ~a
📈 Sharpe: ~,2f"
             source name cat sharpe)
     :color (if (eq source :founder) 3447003 9b59b6))))

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
