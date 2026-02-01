(require :asdf)

(defun load-quicklisp ()
  (let ((ql-setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file ql-setup)
      (load ql-setup))))

(defun resolve-root ()
  (let ((env (uiop:getenv "SWIMMY_HOME")))
    (uiop:ensure-directory-pathname
     (if (and env (> (length env) 0))
         env
         (uiop:getcwd)))))

(load-quicklisp)

(let* ((root (resolve-root))
       (asd (merge-pathnames "swimmy.asd" root)))
  (asdf:load-asd asd)
  (asdf:load-system :swimmy))

(in-package :swimmy.school)

(format t "[OPS] Loading knowledge base...~%")
(init-db)
(init-knowledge-base)

(format t "[OPS] Refreshing metrics from DB...~%")
(when (fboundp 'refresh-strategy-metrics-from-db)
  (refresh-strategy-metrics-from-db :force t))

(format t "[OPS] Running rank evaluation...~%")
(when (fboundp 'run-rank-evaluation)
  (run-rank-evaluation))

(format t "[OPS] Generating evolution report...~%")
(when (fboundp 'notify-evolution-report)
  (notify-evolution-report))
