;; tools/deep_audit.lisp
(in-package :cl-user)
(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init) (load quicklisp-init)))

(handler-bind ((style-warning #'muffle-warning)
               (warning #'muffle-warning))
  (asdf:load-system :swimmy))

(in-package :swimmy.school)

(format t "[AUDIT] Initializing DB + Knowledge Base...~%")
(init-db)
(init-knowledge-base)

(format t "════════════════════════════════════════════~%")
(format t "🔍 DEEP DATA AUDIT: In-Memory vs SQL~%")
(format t "════════════════════════════════════════════~%")

(let* ((kb-size (length *strategy-knowledge-base*))
       (sql-count (swimmy.core:execute-single "SELECT count(*) FROM strategies WHERE rank != ':GRAVEYARD'"))
       (sql-total (swimmy.core:execute-single "SELECT count(*) FROM strategies"))
       (sql-s-count (swimmy.core:execute-single "SELECT count(*) FROM strategies WHERE rank = ':S'"))
       (sql-a-count (swimmy.core:execute-single "SELECT count(*) FROM strategies WHERE rank = ':A'"))
       (kb-s-count (count-if (lambda (s) (eq (strategy-rank s) :S)) *strategy-knowledge-base*))
       (kb-a-count (count-if (lambda (s) (eq (strategy-rank s) :A)) *strategy-knowledge-base*)))

  (format t "--- Population Check ---~%")
  (format t "KB (In-Memory) Size: ~d~%" kb-size)
  (format t "SQL (Non-Graveyard): ~d~%" sql-count)
  (format t "Total SQL Entries  : ~d~%" sql-total)
  
  (format t "~%--- Rank Sync Check ---~%")
  (format t "S-Rank: KB=~d | SQL=~d~%" kb-s-count sql-s-count)
  (format t "A-Rank: KB=~d | SQL=~d~%" kb-a-count sql-a-count)
  
  (when (/= kb-s-count sql-s-count)
    (format t "⚠️ S-Rank MISMATCH! Forcing sync for all KB strategies...~%")
    (dolist (s *strategy-knowledge-base*)
      (when (eq (strategy-rank s) :S)
        (upsert-strategy s))))

  (format t "~%--- Specific Target Check ---~%")
  (let ((target "RECRUIT-RND-1768778854-7"))
    (let ((in-kb (find target *strategy-knowledge-base* :key #'strategy-name :test #'string=))
          (in-sql (swimmy.core:execute-single "SELECT rank FROM strategies WHERE name = ?" target)))
      (format t "Target: ~a~%" target)
      (format t "  In KB : ~a~%" (if in-kb (strategy-rank in-kb) "NOT FOUND"))
      (format t "  In SQL: ~a~%" (if in-sql in-sql "NOT FOUND"))))

  (format t "~%--- NULL/Malformed Audit ---~%")
  (let ((null-names (swimmy.core:execute-single "SELECT count(*) FROM strategies WHERE name IS NULL"))
        (empty-hashes (swimmy.core:execute-single "SELECT count(*) FROM strategies WHERE hash IS NULL OR hash = ''")))
    (format t "  NULL Names : ~d~%" null-names)
    (format t "  Empty Hashes: ~d~%" empty-hashes)))

(format t "════════════════════════════════════════════~%")
(sb-ext:exit :code 0)
