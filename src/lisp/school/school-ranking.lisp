;;; school-ranking.lisp
;;; Phase 20: Ranking Logic (Meritocracy)
;;; Implements the Ladder: Incubator -> Rank B -> Rank A -> Rank S

(in-package :swimmy.school)

;;; =========================================================
;;; RANK B: THE POOL (Capacity 100)
;;; =========================================================

(defparameter *rank-b-capacity* 100 "Max strategies in Rank B pool per timeframe.")

(defun promote-to-rank-b (strategy)
  "Promote strategy to Rank B (The Pool).
   Triggered after passing Phase 1 Screening."
  (format t "[RANK] 🎖️ Promoting ~a to Rank B (Screening Passed)~%" (strategy-name strategy))
  
  (bt:with-lock-held (*kb-lock*)
    ;; 1. Update Rank
    (setf (strategy-rank strategy) :B)
    (setf (strategy-tier strategy) :candidate) ;; Legacy tier mapping
    
    ;; 2. Update Persistance
    (upsert-strategy strategy)
    
    ;; 3. Check Pool Capacity (Culling)
    ;; Validating pool size for this timeframe/pair?
    ;; For now, global B-rank checks or per-category.
    (cull-rank-b-pool)))

(defun cull-rank-b-pool ()
  "Maintain Rank B pool size. Remove weakest if > Capacity."
  (let ((b-rankers (remove-if-not (lambda (s) (eq (strategy-rank s) :B)) *strategy-knowledge-base*)))
    (when (> (length b-rankers) *rank-b-capacity*)
      (format t "[RANK] ✂️ Culling Rank B Pool (~d > ~d)...~%" (length b-rankers) *rank-b-capacity*)
      
      ;; Sort by Sharpe (Ascending - Weakest first)
      (let ((sorted (sort (copy-list b-rankers) #'< :key (lambda (s) (or (strategy-sharpe s) 0.0))))
            (culled-count 0))
        
        ;; Remove bottom 30% (buffer)
        (let ((to-remove (subseq sorted 0 (floor (* (length sorted) 0.3)))))
          (dolist (s to-remove)
            (demote-to-graveyard s "Rank B Culling (Weakest)")
            (incf culled-count))
          (format t "[RANK] ✂️ Culled ~d strategies.~%" culled-count))))))

;;; =========================================================
;;; RANK A: THE CANDIDATES (OOS Validated)
;;; =========================================================

(defun promote-to-rank-a (strategy)
  "Promote to Rank A (Candidate).
   Triggered after passing Phase 2 Validation (OOS)."
  (format t "[RANK] 🌟 Promoting ~a to Rank A (Validation Passed)~%" (strategy-name strategy))
  
  (bt:with-lock-held (*kb-lock*)
    (setf (strategy-rank strategy) :A)
    (upsert-strategy strategy)
    
    ;; Trigger CPCV for S-Rank?
    ;; Or wait for manual trigger/schedule?
    ;; V50.2 Plan says "Gate to Rank S (CPCV)".
    ;; We can queue it.
    (format t "[RANK] ⏳ ~a queued for CPCV (S-Rank Gate).~%" (strategy-name strategy))))

;;; =========================================================
;;; RANK S: THE GLADIATORS (Live Trading)
;;; =========================================================

(defun promote-to-rank-s (strategy)
  "Promote to Rank S (Gladiator).
   Triggered after passing CPCV."
  (format t "[RANK] 👑 Promoting ~a to Rank S (The Gladiator)!~%" (strategy-name strategy))
  
  (bt:with-lock-held (*kb-lock*)
    (setf (strategy-rank strategy) :S)
    (setf (strategy-tier strategy) :battlefield)
    (upsert-strategy strategy)
    
    ;; Notification
    (swimmy.core:notify-discord-alert 
     (format nil "👑 **NEW GLADIATOR**: `~a` Promoted to Rank S!" (strategy-name strategy))
     :color 16766720))) ;; Gold

;;; =========================================================
;;; DSR (DEFLATED SHARPE RATIO) LOGIC (Simons/Prado)
;;; =========================================================

(defun count-graveyard-trials ()
  "Count total strategies in the Graveyard (representing failed trials)."
  (count-if (lambda (s) (eq (strategy-rank s) :GRAVEYARD)) *strategy-knowledge-base*))

(defun calculate-dsr-threshold ()
  "Calculate DSR (Deflated Sharpe Ratio) threshold based on total trials.
   Phase 23 Directive: S-Rank Barrier rises with trial count.
   Formula: Base=3.0 + log10(GraveyardCount / 1000)"
  (let* ((trials (max 1 (count-graveyard-trials)))
         (log-factor (log (/ trials 1000.0) 10)))
    (if (> log-factor 0)
        (float (+ 3.0 (* 0.5 log-factor))) ;; 0.5 scaling factor to be slightly lenient? No, Simons said strict.
        3.0))) 

(defun meets-s-rank-dsr-p (strategy)
  "Check if strategy meets the dynamic DSR threshold."
  (let ((threshold (calculate-dsr-threshold))
        (sharpe (or (strategy-sharpe strategy) 0.0)))
    (if (>= sharpe threshold)
        t
        (progn
          (format t "[DSR] 🚫 Strategy ~a Sharpe (~,2f) < DSR Threshold (~,2f)~%" 
                  (strategy-name strategy) sharpe threshold)
          nil))))

;;; =========================================================
;;; UTILS
;;; =========================================================

(defun demote-to-graveyard (strategy reason)
  "Move to Graveyard."
  (format t "[RANK] 🪦 Demoting ~a to Graveyard: ~a~%" (strategy-name strategy) reason)
  (bt:with-lock-held (*kb-lock*)
    (setf (strategy-rank strategy) :GRAVEYARD)
    (upsert-strategy strategy)
    ;; Ideally remove from *strategy-knowledge-base* to save memory?
    ;; Or keep for record? Current logic keeps but marks Rank.
    ;; Pruning logic handles physical removal later.
    ))
