;; tools/audit_db.lisp
(in-package :cl-user)
(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init) (load quicklisp-init)))

(handler-bind ((style-warning #'muffle-warning)
               (warning #'muffle-warning))
  (asdf:load-system :swimmy))

(in-package :swimmy.core)

(format t "--- SQL Database Audit ---~%")
(handler-case
    (let ((conn (get-db-connection)))
      (format t "Tables in database:~%")
      (dolist (table (sqlite:execute-to-list conn "SELECT name FROM sqlite_master WHERE type='table';"))
        (format t "  - ~a~%" (car table)))
      
      (format t "~%Rank Distribution (strategies table):~%")
      (let ((ranks (sqlite:execute-to-list conn "SELECT rank, count(*) FROM strategies GROUP BY rank;")))
        (dolist (row ranks)
          (format t "  ~a: ~d~%" (first row) (second row))))
      
      (format t "~%Integrity Check: ~a~%" 
              (sqlite:execute-single conn "PRAGMA integrity_check;"))
      
      (format t "~%Recent S-Rank Additions (Last 5):~%")
      (let ((recent-s (sqlite:execute-to-list conn "SELECT name, sharpe FROM strategies WHERE rank = 'S' ORDER BY rowid DESC LIMIT 5;")))
        (if recent-s
            (dolist (row recent-s)
              (format t "  - ~a (Sharpe: ~,2f)~%" (first row) (second row)))
            (format t "  (No S-Rank strategies found in DB)~%"))))
  (error (e)
    (format t "❌ SQL Error: ~a~%" e)))

(sb-ext:exit :code 0)
