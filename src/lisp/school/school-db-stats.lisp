;;; school-db-stats.lisp - DB/library stats and drift checks

(in-package :swimmy.school)

(defun get-db-stats ()
  "Return summary of DB contents."
  (let ((strat-count (execute-single "SELECT count(*) FROM strategies"))
        (trade-count (execute-single "SELECT count(*) FROM trade_logs")))
    (list :strategies strat-count :trades trade-count)))

(defun %normalize-db-rank-key (rank)
  (labels ((normalize (value)
             (let* ((trimmed (string-upcase (string-trim '(#\Space #\Newline #\Tab) value))))
               (if (and (> (length trimmed) 0) (char= (char trimmed 0) #\:))
                   (subseq trimmed 1)
                   trimmed))))
    (cond
      ((null rank) "NIL")
      ((stringp rank) (normalize rank))
      ((symbolp rank) (normalize (symbol-name rank)))
      (t (normalize (format nil "~a" rank))))))

(defun get-db-rank-counts ()
  "Return plist of rank counts from DB: :total :active :s :a :b :legend :graveyard :incubator :unranked."
  (let* ((rows (execute-to-list "SELECT rank, count(*) FROM strategies GROUP BY rank"))
         (counts (make-hash-table :test 'equal))
         (total 0))
    (dolist (row rows)
      (destructuring-bind (rank count) row
        (let ((key (%normalize-db-rank-key rank)))
          (setf (gethash key counts) count)
          (incf total count))))
    (labels ((count-rank (key) (or (gethash key counts) 0)))
      (let* ((s (count-rank "S"))
             (a (count-rank "A"))
             (b (count-rank "B"))
             (legend (count-rank "LEGEND"))
             (graveyard (count-rank "GRAVEYARD"))
             (incubator (count-rank "INCUBATOR"))
             (unranked (count-rank "NIL"))
             (active (- total graveyard)))
        (list :total total
              :active active
              :s s
              :a a
              :b b
              :legend legend
              :graveyard graveyard
              :incubator incubator
              :unranked unranked)))))

(defun get-library-rank-counts (&optional (root swimmy.persistence:*library-path*))
  "Return plist of library counts by rank dir."
  (labels ((count-dir (dir)
             (length (directory (merge-pathnames (format nil "~a/*.lisp" dir) root)))))
    (list :s (count-dir "S")
          :a (count-dir "A")
          :b (count-dir "B")
          :incubator (count-dir "INCUBATOR")
          :legend (count-dir "LEGEND")
          :graveyard (count-dir "GRAVEYARD"))))

(defun report-source-drift ()
  "Return list of warning strings when DB/KB/Library counts drift."
  (let* ((db (get-db-rank-counts))
         (lib (get-library-rank-counts))
         (kb-active (length *strategy-knowledge-base*))
         (db-active (getf db :active 0))
         (db-grave (getf db :graveyard 0))
         (lib-grave (getf lib :graveyard 0))
         (warnings nil))
    (when (/= db-active kb-active)
      (push (format nil "KB active mismatch (DB=~d KB=~d)" db-active kb-active) warnings))
    (when (/= db-grave lib-grave)
      (push (format nil "Graveyard mismatch (DB=~d Library=~d)" db-grave lib-grave) warnings))
    (nreverse warnings)))
