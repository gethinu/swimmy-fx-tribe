
(in-package :swimmy.school)

(defun rescue-a-rank-strategies ()
  "Rescue strategies from GRAVEYARD that have Sharpe > 0.3 (A-Rank qual)."
  (let ((graveyard-path (merge-pathnames "GRAVEYARD/*.lisp" swimmy.persistence:*library-path*))
        (rescued-count 0))
    (format t "[RESCUE] 🚑 Scanning GRAVEYARD for wrongfully accused A-Ranks...~%")
    (dolist (file (directory graveyard-path))
      (let ((strat (swimmy.persistence:load-strategy file)))
        (when strat
          (let ((sharpe (or (strategy-sharpe strat) 0.0))
                (rank (strategy-rank strat)))
            (when (>= sharpe 0.3)
              (format t "[RESCUE] 🌟 Rescuing ~a (Sharpe=~,2f, Rank=~a)~%" 
                      (strategy-name strat) sharpe rank)
              
              ;; 1. Update Rank
              (setf (strategy-rank strat) :A)
              (setf (strategy-status strat) :active)
              (setf (strategy-tier strat) :training) ;; Sync legacy tier
              
              ;; 2. Move File (Physical Rescue)
              ;; Note: move-strategy also saves the file
              (swimmy.persistence:move-strategy strat :training)
              
              ;; 3. Add to KB
              (add-to-kb strat :rescue :notify nil)
              
              (incf rescued-count))))))
    
    (format t "[RESCUE] ✅ Operation Complete. ~d strategies restored to A-Rank.~%" rescued-count)
    rescued-count))

(rescue-a-rank-strategies)
