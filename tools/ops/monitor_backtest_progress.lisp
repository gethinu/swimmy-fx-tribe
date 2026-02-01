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

(defun read-start-time (path)
  (when (probe-file path)
    (with-open-file (in path :direction :input)
      (let ((line (read-line in nil nil)))
        (when line
          (parse-integer line :junk-allowed t))))))

(let* ((start-path (swimmy.core::swimmy-path "data/reports/backtest_all_start.txt"))
       (start-time (read-start-time start-path)))
  (unless start-time
    (format t "[OPS] ❌ Start time not found. Expected file: ~a~%" start-path)
    (sb-ext:exit :code 1))

  (init-db)

  (let* ((total (or (execute-single "SELECT count(*) FROM strategies") 0))
         (completed (or (execute-single "SELECT count(*) FROM strategies WHERE last_bt_time >= ?" start-time) 0))
         (pct (if (> total 0) (* 100.0 (/ completed total)) 0.0)))
    (format t "[OPS] Backtest progress since ~d~%" start-time)
    (format t "[OPS] Completed: ~d / ~d (~,2f%%)~%" completed total pct)))
