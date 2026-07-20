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
          (upsert-strategy strat) ;; V50.5.1 Fix
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
                        ;; V49.8: Data S-Exp might be large, safe-read for safety
                        (strat (swimmy.core:safe-read-sexp (second row) :package :swimmy.school)))
                   (if (and strat (strategy-p strat))
                       (let ((hash (calculate-strategy-hash strat)))
                         (execute-non-query "UPDATE strategies SET hash = ? WHERE name = ?" hash name))
                       ;; P5 (Thread B): the blob did not parse to a strategy =
                       ;; corruption. QUARANTINE (rank=:CORRUPT), never delete, so
                       ;; the row survives for forensics and is excluded from every
                       ;; active/breeding allow-list (which enumerate B/A/S/LEGEND).
                       (progn
                         (format t "[CEMETERY] ☣️ QUARANTINE: ~a data_sexp unreadable -> rank=:CORRUPT~%" name)
                         (execute-non-query
                          "UPDATE strategies SET hash = ':INVALID', rank = ':CORRUPT' WHERE name = ?"
                          name))))))
           (error (e) 
             (format t "[CEMETERY] ⚠️ Batch failed: ~a~%" e)
             (return)))
         (incf total-processed (length rows))
         ;; Manual GC hint?
         #+sbcl (sb-ext:gc :full t)))
    (format t "[CEMETERY] ✅ Audit complete. Total processed: ~d~%" total-processed)
    total-processed))

(defun record-graveyard-pattern (strat)
  "Explicitly add a strategy to the graveyard and save its hash.
   P5: the rank flip + upsert run under BEGIN IMMEDIATE so the graveyard write
   is atomic and never loses to a concurrent brain/school writer (SQLITE_BUSY).
   The graveyard is the append-only failure record whose monotonic growth the
   persistence audit relies on, so a dropped write here is exactly what we must
   prevent."
  (setf (strategy-rank strat) :GRAVEYARD)
  (unless (strategy-hash strat)
    (setf (strategy-hash strat) (calculate-strategy-hash strat)))
  (handler-case
      (with-immediate-transaction
        (upsert-strategy strat))
    (error (e)
      (format t "[CEMETERY] ⚠️ Graveyard write failed for ~a: ~a~%"
              (strategy-name strat) e)
      (return-from record-graveyard-pattern nil)))
  (format t "[CEMETERY] 🪦 Recorded pattern ~a to SQL Graveyard.~%" (strategy-name strat)))
