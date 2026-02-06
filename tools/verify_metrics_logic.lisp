

(require :asdf)
(load "swimmy.asd")
(asdf:load-system :swimmy)

(in-package :swimmy.school)

(defun verify-reporting-metrics ()
  (format t "📊 Verifying Reporting Metrics...~%")
  
  ;; Initialize DB if not already
  (unless *db-conn* (init-db))
  
  ;; 1. Active Knowledge Base
  (let ((kb-count (length *strategy-knowledge-base*))
        (db-count (handler-case 
                      (second (first (execute-to-list "SELECT count(*) FROM strategies WHERE rank NOT IN (':GRAVEYARD', ':RETIRED')")))
                    (error (e) 
                      (format t "DB Error: ~a~%" e)
                      -1))))
    (format t "[METRIC 1] Active KB: Memory=~d, DB(non-graveyard/non-retired)=~d~%" kb-count db-count)
    (when (not (= kb-count db-count))
      (format t "⚠️ Discrepancy in Active KB count!~%")))
      
  ;; 2. New Recruits (24h)
  (let* ((one-day-ago (- (get-universal-time) 86400))
         (recruits-mem (count-if (lambda (s) (and (strategy-creation-time s) (> (strategy-creation-time s) one-day-ago))) *strategy-knowledge-base*))
         ;; Assuming we can query by timestamp if stored, otherwise approximate
         ;; Just checking logic here
         )
    (format t "[METRIC 5] New Recruits (24h): ~d~%" recruits-mem))

  ;; 3. Rank Counts
  (let ((s-rank (count-if (lambda (s) (eq (strategy-rank s) :S)) *strategy-knowledge-base*))
        (a-rank (count-if (lambda (s) (eq (strategy-rank s) :A)) *strategy-knowledge-base*))
        (b-rank (count-if (lambda (s) (eq (strategy-rank s) :B)) *strategy-knowledge-base*)))
    (format t "[METRIC 2-4] Rank Counts: S=~d, A=~d, B=~d~%" s-rank a-rank b-rank))
    
  ;; 4. Graveyard
  (let* ((gy-files (length (directory (merge-pathnames "GRAVEYARD/*.lisp" swimmy.persistence:*library-path*))))
         (gy-db (handler-case (second (first (sqlite:execute-to-list (or *db-conn* (init-db)) "SELECT count(*) FROM strategies WHERE rank = ':GRAVEYARD'")))
                  (error () -1))))
    (format t "[METRIC 6] Graveyard: Files=~d, DB=~d~%" gy-files gy-db))

  ;; 5. Retired
  (let* ((ret-files (length (directory (merge-pathnames "RETIRED/*.lisp" swimmy.persistence:*library-path*))))
         (ret-db (handler-case (second (first (sqlite:execute-to-list (or *db-conn* (init-db)) "SELECT count(*) FROM strategies WHERE rank = ':RETIRED'")))
                  (error () -1))))
    (format t "[METRIC 7] Retired: Files=~d, DB=~d~%" ret-files ret-db))
    
  (format t "✅ Verification Complete.~%"))

(verify-reporting-metrics)
