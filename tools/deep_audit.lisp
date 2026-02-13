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

(format t "[AUDIT] Initializing DB (streaming; no full KB materialization)...~%")
(init-db)

(format t "════════════════════════════════════════════~%")
(format t "🔍 DEEP DATA AUDIT: DB vs Library (Streaming)~%")
(format t "════════════════════════════════════════════~%")

(let* ((db (get-db-rank-counts))
       (lib (get-library-rank-counts))
       (sql-total (swimmy.core:execute-single "SELECT count(*) FROM strategies"))
       (null-names (swimmy.core:execute-single "SELECT count(*) FROM strategies WHERE name IS NULL"))
       (empty-hashes (swimmy.core:execute-single "SELECT count(*) FROM strategies WHERE hash IS NULL OR hash = ''"))
       (db-grave (getf db :graveyard 0))
       (db-retired (getf db :retired 0))
       (db-active (getf db :active 0))
       (lib-grave (getf lib :graveyard 0))
       (lib-retired (getf lib :retired 0))
       (lib-active (+ (getf lib :s 0) (getf lib :a 0) (getf lib :b 0)
                      (getf lib :incubator 0) (getf lib :legend 0))))

  (format t "--- Population Check ---~%")
  (format t "DB Total Entries   : ~d~%" sql-total)
  (format t "DB Active (Total-Archive): ~d~%" db-active)
  (format t "DB Archive         : ~d (GRAVEYARD=~d RETIRED=~d)~%"
          (+ db-grave db-retired) db-grave db-retired)
  (format t "Library Active Dirs: ~d (S+A+B+INCUBATOR+LEGEND)~%" lib-active)
  (format t "Library Archive    : ~d (GRAVEYARD=~d RETIRED=~d)~%"
          (+ lib-grave lib-retired) lib-grave lib-retired)
  
  (format t "~%--- Rank Sync Check ---~%")
  (format t "DB S-Rank: ~d | Library S dir: ~d~%" (getf db :s 0) (getf lib :s 0))
  (format t "DB A-Rank: ~d | Library A dir: ~d~%" (getf db :a 0) (getf lib :a 0))
  (format t "DB B-Rank: ~d | Library B dir: ~d~%" (getf db :b 0) (getf lib :b 0))
  (format t "DB LEGEND: ~d | Library LEGEND dir: ~d~%" (getf db :legend 0) (getf lib :legend 0))
  (format t "DB INCUBATOR: ~d | Library INCUBATOR dir: ~d~%" (getf db :incubator 0) (getf lib :incubator 0))
  (format t "DB UNRANKED(NIL): ~d~%" (getf db :unranked 0))

  (format t "~%--- Drift Check ---~%")
  (when (/= db-grave lib-grave)
    (format t "⚠️ Graveyard mismatch: DB=~d Library=~d delta(DB-Library)=~@d~%"
            db-grave lib-grave (- db-grave lib-grave)))
  (when (/= db-retired lib-retired)
    (format t "⚠️ Retired mismatch: DB=~d Library=~d delta(DB-Library)=~@d~%"
            db-retired lib-retired (- db-retired lib-retired)))

  (format t "~%--- NULL/Malformed Audit ---~%")
  (format t "  NULL Names : ~d~%" null-names)
  (format t "  Empty Hashes: ~d~%" empty-hashes)

  (format t "~%--- SEXP Parse Smoke Test (Sample) ---~%")
  (let* ((limit-str (or (uiop:getenv "SWIMMY_DEEP_AUDIT_PARSE_LIMIT") "5000"))
         (limit (or (ignore-errors (parse-integer limit-str :junk-allowed t)) 5000))
         (batch-size 500)
         (offset 0)
         (ok 0)
         (bad 0))
    (loop
      (when (>= (+ ok bad) limit)
        (return))
      (let ((rows (execute-to-list "SELECT data_sexp FROM strategies LIMIT ? OFFSET ?" batch-size offset)))
        (when (null rows)
          (return))
        (dolist (row rows)
          (when (>= (+ ok bad) limit)
            (return))
          (let* ((sexp-str (first row))
                 (obj (ignore-errors (swimmy.core:safe-read-sexp sexp-str :package :swimmy.school))))
            (if (and obj (strategy-p obj))
                (incf ok)
                (incf bad))))
        (incf offset batch-size)))
    (format t "  Parsed sample: ok=~d bad=~d limit=~d~%" ok bad limit)))

(format t "════════════════════════════════════════════~%")
(sb-ext:exit :code 0)
