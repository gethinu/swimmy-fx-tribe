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
  "Return plist of rank counts from DB: :total :active :s :a :b :legend :graveyard :retired :incubator :unranked."
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
             (retired (count-rank "RETIRED"))
             (incubator (count-rank "INCUBATOR"))
             (unranked (count-rank "NIL"))
             (active (- total graveyard retired)))
        (list :total total
              :active active
              :s s
              :a a
              :b b
              :legend legend
              :graveyard graveyard
              :retired retired
              :incubator incubator
              :unranked unranked)))))

(defun get-library-rank-counts (&optional (root swimmy.persistence:*library-path*))
  "Return plist of library counts by rank dir."
  ;; NOTE: Do not use CL:DIRECTORY here for huge dirs (e.g., GRAVEYARD) because it
  ;; materializes a full pathname list and can exhaust memory. We count via `find`
  ;; and measure output length (1 dot per file).
  (labels ((count-dir (dir)
             (let* ((dirpath (merge-pathnames (format nil "~a/" dir) root))
                    (dirstr (namestring dirpath)))
               (if (and (fboundp 'uiop:directory-exists-p)
                        (not (uiop:directory-exists-p dirpath)))
                   0
                   (handler-case
                       (length
                        (or (uiop:run-program
                             (list "find" dirstr
                                   "-maxdepth" "1"
                                   "-type" "f"
                                   "-name" "*.lisp"
                                   "-printf" ".")
                             :output :string
                             :ignore-error-status t)
                            ""))
                     (error () 0))))))
    (list :s (count-dir "S")
          :a (count-dir "A")
          :b (count-dir "B")
          :incubator (count-dir "INCUBATOR")
          :legend (count-dir "LEGEND")
          :graveyard (count-dir "GRAVEYARD")
          :retired (count-dir "RETIRED"))))

(defun %shell-quote-single (s)
  "Return shell-safe single-quoted string."
  (format nil "'~a'"
          (with-output-to-string (out)
            (loop for ch across (or s "")
                  do (if (char= ch #\')
                         (write-string "'\"'\"'" out)
                         (write-char ch out))))))

(defun get-library-archive-canonical-count (&optional (root swimmy.persistence:*library-path*))
  "Return unique strategy-name count across Library GRAVEYARD+RETIRED dirs."
  (labels ((path-str (dir)
             (namestring (merge-pathnames (format nil "~a/" dir) root))))
    (let* ((grave (%shell-quote-single (path-str "GRAVEYARD")))
           (retired (%shell-quote-single (path-str "RETIRED")))
           (cmd (format nil
                        "{ find ~a -maxdepth 1 -type f -name '*.lisp' -printf '%f\\n' 2>/dev/null; find ~a -maxdepth 1 -type f -name '*.lisp' -printf '%f\\n' 2>/dev/null; } | sed 's/\\.lisp$//' | LC_ALL=C sort -u | wc -l"
                        grave retired))
           (raw (ignore-errors (uiop:run-program cmd :output :string :ignore-error-status t :force-shell t)))
           (trimmed (and raw (string-trim '(#\Space #\Tab #\Newline #\Return) raw)))
           (n (and trimmed (> (length trimmed) 0) (parse-integer trimmed :junk-allowed t))))
      (or n 0))))

(defun report-source-drift ()
  "Return list of warning strings when DB/KB/Library counts drift."
  (let* ((db (get-db-rank-counts))
         (lib (get-library-rank-counts))
         (lib-canonical (get-library-archive-canonical-count))
         (kb-active (length *strategy-knowledge-base*))
         (db-active (getf db :active 0))
         (db-grave (getf db :graveyard 0))
         (lib-grave (getf lib :graveyard 0))
         (delta-grave (- db-grave lib-grave))
         (db-retired (getf db :retired 0))
         (lib-retired (getf lib :retired 0))
         (delta-retired (- db-retired lib-retired))
         (db-archive-total (+ db-grave db-retired))
         (delta-archive (- db-archive-total lib-canonical))
         (warnings nil))
    (when (/= db-active kb-active)
      (push (format nil "KB active mismatch (DB=~d KB=~d)" db-active kb-active) warnings))
    (when (/= db-grave lib-grave)
      (push (format nil "Graveyard mismatch (DB=~d Library=~d delta(DB-Library)=~@d)"
                    db-grave lib-grave delta-grave)
            warnings))
    (when (/= db-retired lib-retired)
      (push (format nil "Retired mismatch (DB=~d Library=~d delta(DB-Library)=~@d)"
                    db-retired lib-retired delta-retired)
            warnings))
    (when (or (/= db-grave lib-grave)
              (/= db-retired lib-retired)
              (/= db-archive-total lib-canonical))
      (push (format nil "Archive canonical mismatch (DB archive=~d Library canonical=~d delta(DB-LibraryCanonical)=~@d)"
                    db-archive-total lib-canonical delta-archive)
            warnings))
    (nreverse warnings)))
