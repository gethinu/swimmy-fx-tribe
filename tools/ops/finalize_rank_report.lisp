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

(defun env-truthy-p (key &optional (default nil))
  (let* ((raw (uiop:getenv key))
         (val (and raw (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) raw)))))
    (cond
      ((or (null val) (string= val "")) default)
      ((member val '("1" "true" "yes" "on") :test #'string=) t)
      ((member val '("0" "false" "no" "off") :test #'string=) nil)
      (t default))))

(load-quicklisp)

(let* ((root (resolve-root))
       (asd (merge-pathnames "swimmy.asd" root)))
  (asdf:load-asd asd)
  (asdf:load-system :swimmy))

(in-package :swimmy.school)

(let ((run-rank-eval (cl-user::env-truthy-p "SWIMMY_FINALIZE_REPORT_RUN_RANK_EVAL" nil)))
  (init-db)
  (if run-rank-eval
      (progn
        (format t "[OPS] Loading knowledge base (rank-eval mode)...~%")
        (init-knowledge-base)
        (format t "[OPS] Refreshing metrics from DB...~%")
        (when (fboundp 'refresh-strategy-metrics-from-db)
          (refresh-strategy-metrics-from-db :force t))
        (format t "[OPS] Running rank evaluation...~%")
        (when (fboundp 'run-rank-evaluation)
          (run-rank-evaluation)))
      (format t "[OPS] Snapshot mode: skipping knowledge-base hydration and rank evaluation.~%")))

(format t "[OPS] Generating evolution report...~%")
(when (fboundp 'notify-evolution-report)
  (notify-evolution-report))
