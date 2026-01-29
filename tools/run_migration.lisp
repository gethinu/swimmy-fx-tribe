;; tools/run_migration.lisp
;; One-off script to migrate legacy flat-file data to SQLite (V49.8)

(format t "--- 🚜 STARTING MIGRATION (V49.8) ---~%")

;; Load System
(handler-case
    (progn
      (format t "[MIGRATE] ⏳ Loading Swimmy System...~%")
      ;; Use ql:quickload instead of direct ASD loading for safety
      (load "swimmy.asd") 
      (ql:quickload :swimmy :silent t)
      (format t "[MIGRATE] ✅ System Loaded.~%"))
  (error (e)
    (format t "[MIGRATE] ❌ Load Error: ~a~%" e)
    (sb-ext:exit :code 1)))

;; Verify Function Exists
(unless (fboundp 'swimmy.school::migrate-existing-data)
  (format t "[MIGRATE] ❌ Function swimmy.school::migrate-existing-data NOT FOUND!~%")
  (sb-ext:exit :code 1))

;; Execute Migration
(handler-case
    (progn
      (format t "[MIGRATE] 🚀 Executing Migration...~%")
      (let ((count (swimmy.school::migrate-existing-data)))
        (format t "[MIGRATE] 🏁 Migration Complete. Strategies: ~d~%" count)))
  (error (e)
    (format t "[MIGRATE] ❌ Execution Error: ~a~%" e)
    (sb-ext:exit :code 1)))

;; Verify Counts
(format t "[MIGRATE] 🔍 Verifying DB Counts...~%")
(let ((stats (swimmy.school::get-db-stats)))
  (format t "[MIGRATE] 📊 DB Stats: Strategies=~d, Trades=~d~%" 
          (getf stats :strategies) 
          (getf stats :trades)))

(format t "--- ✅ MIGRATION FINISHED ---~%")
(sb-ext:exit :code 0)
