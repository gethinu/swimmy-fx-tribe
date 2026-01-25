;; tools/sync_to_sql.lisp
(in-package :cl-user)
(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init) (load quicklisp-init)))

(handler-bind ((style-warning #'muffle-warning)
               (warning #'muffle-warning))
  (asdf:load-system :swimmy))

(in-package :swimmy.school)

(format t "════════════════════════════════════════════~%")
(format t "🚜 SQL MIGRATION & SYNC INITIALIZED~%")
(format t "════════════════════════════════════════════~%")

(let ((count (migrate-existing-data)))
  (format t "Successfully synced ~d strategies from KB to SQL.~%" count))

;; Verify sync for the previously missing target
(let ((target "RECRUIT-RND-1768783472-13"))
  (let ((exists (swimmy.core:execute-single "SELECT count(*) FROM strategies WHERE name = ?" target)))
    (if (> exists 0)
        (format t "✅ Target ~a is now in SQL.~%" target)
        (format t "❌ Target ~a STILL MISSING!~%" target))))

(format t "════════════════════════════════════════════~%")
(sb-ext:exit :code 0)
