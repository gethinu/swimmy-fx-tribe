;;; school-cemetery.lisp - Pre-emptive Graveyard Avoidance
;;; V49.8: Leveraging SQL Graveyard Data for Performance
(in-package :swimmy.school)

(defun get-graveyard-hashes ()
  "Fetch all hashes of strategies marked as :GRAVEYARD from DB."
  (handler-case
      (let ((rows (execute-to-list "SELECT hash FROM strategies WHERE rank = ':GRAVEYARD' AND hash IS NOT NULL")))
        (mapcar #'first rows))
    (error (e) 
      (format t "[CEMETERY] ⚠️ Error fetching graveyard hashes: ~a~%" e)
      nil)))

(defun prune-strategies-by-graveyard-hash ()
  "Identify and mark strategies that match known graveyard patterns."
  (let ((gy-hashes (get-graveyard-hashes))
        (banned-count 0))
    (unless gy-hashes
      (format t "[CEMETERY] 🪦 No graveyard hashes found. Skipping prune.~%")
      (return-from prune-strategies-by-graveyard-hash 0))
      
    (format t "[CEMETERY] 🪦 Checking against ~d graveyard patterns...~%" (length gy-hashes))
    
    ;; P8: Atomic KB Operations - should ideally use lock if running in parallel
    (dolist (strat (append *strategy-knowledge-base* *evolved-strategies*))
      (unless (strategy-hash strat)
        (setf (strategy-hash strat) (calculate-strategy-hash strat)))
      
      (when (member (strategy-hash strat) gy-hashes :test #'string=)
        ;; Don't re-ban if already graveyard
        (unless (eq (strategy-rank strat) :GRAVEYARD)
          (format t "[CEMETERY] 🚫 BANNED: ~a matches graveyard pattern!~%" (strategy-name strat))
          (setf (strategy-rank strat) :GRAVEYARD)
          (incf banned-count))))
          
    (when (> banned-count 0)
      (format t "[CEMETERY] 🧹 Musk: Pruned ~d strategies matching CEMETERY patterns.~%" banned-count))
    banned-count))

(defun cemetery-audit-db (&optional (batch-size 5000))
  "Verify graveyard entries in DB have hashes and update in batches."
  (let ((total-processed 0))
    (loop
       (let ((rows (execute-to-list "SELECT name, data_sexp FROM strategies WHERE hash IS NULL LIMIT ?" batch-size)))
         (unless rows (return))
         (format t "[CEMETERY] 🔍 Auditing batch of ~d records...~%" (length rows))
         (handler-case
             (with-transaction
               (dolist (row rows)
                 (let* ((name (first row))
                        ;; V49.8: Data S-Exp might be large, handler-case for safety
                        (strat (handler-case (read-from-string (second row)) (error () nil))))
                   (if (and strat (strategy-p strat))
                       (let ((hash (calculate-strategy-hash strat)))
                         (execute-non-query "UPDATE strategies SET hash = ? WHERE name = ?" hash name))
                       (execute-non-query "UPDATE strategies SET hash = ':INVALID' WHERE name = ?" name)))))
           (error (e) 
             (format t "[CEMETERY] ⚠️ Batch failed: ~a~%" e)
             (return)))
         (incf total-processed (length rows))
         ;; Manual GC hint?
         #+sbcl (sb-ext:gc :full t)))
    (format t "[CEMETERY] ✅ Audit complete. Total processed: ~d~%" total-processed)
    total-processed))

(defun record-graveyard-pattern (strat)
  "Explicitly add a strategy to the graveyard and save its hash."
  (setf (strategy-rank strat) :GRAVEYARD)
  (unless (strategy-hash strat)
    (setf (strategy-hash strat) (calculate-strategy-hash strat)))
  (upsert-strategy strat)
  (format t "[CEMETERY] 🪦 Recorded pattern ~a to SQL Graveyard.~%" (strategy-name strat)))
