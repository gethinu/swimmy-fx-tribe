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

(defun getenv-int (name)
  (let ((val (uiop:getenv name)))
    (when (and val (> (length val) 0))
      (parse-integer val :junk-allowed t))))

(defun write-lines (path lines)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (line lines)
      (write-line line out))))

(let* ((limit (getenv-int "SWIMMY_BACKTEST_LIMIT"))
       (dry-run (uiop:getenv "SWIMMY_BACKTEST_DRY_RUN"))
       (start-time (get-universal-time))
       (kb-size 0)
       (processed 0)
       (queued 0)
       (skipped 0)
       (missing (make-hash-table :test 'equal)))
  (format t "[OPS] Using SWIMMY_HOME: ~a~%" swimmy.core::*swimmy-home*)

  (init-db)
  (setf swimmy.core:*backtest-service-enabled* t)
  (init-backtest-zmq)

  (when limit
    (format t "[OPS] Limit enabled: ~d strategies~%" limit))
  (when dry-run
    (format t "[OPS] Dry run enabled: no backtest requests will be sent.~%"))

  (let ((seen (make-hash-table :test 'equal)))
    (flet ((handle-strategy (strat)
             (let ((name (strategy-name strat)))
               (unless (gethash name seen)
                 (setf (gethash name seen) t)
                 (incf processed)
                 (let* ((raw-sym (or (strategy-symbol strat) "USDJPY"))
                        (sym (string-upcase raw-sym))
                        (path (swimmy.core::swimmy-path (format nil "data/historical/~a_M1.csv" sym))))
                   (if (probe-file path)
                       (progn
                         (unless dry-run
                           (request-backtest strat :candles nil :symbol sym :suffix "-FULL")
                           (sleep 0.01))
                         (incf queued))
                       (progn
                         (incf skipped)
                         (setf (gethash sym missing) t))))
                 t))))
      (setf kb-size (or (execute-single "SELECT count(*) FROM strategies") 0))
      (map-strategies-from-db #'handle-strategy :batch-size 1000 :limit limit)
      (let ((file-strats (ignore-errors (swimmy.persistence:load-all-strategies))))
        (dolist (fs (or file-strats '()))
          (when (and limit (>= processed limit))
            (return))
          (when (handle-strategy fs)
            (incf kb-size))))))

  (write-lines (swimmy.core::swimmy-path "data/reports/backtest_all_start.txt")
               (list (format nil "~d" start-time)))

  (let ((missing-list nil))
    (maphash (lambda (k v) (declare (ignore v)) (push k missing-list)) missing)
    (setf missing-list (sort missing-list #'string<))
    (write-lines (swimmy.core::swimmy-path "data/reports/backtest_all_missing_symbols.txt")
                 (if missing-list missing-list (list "(none)"))))

  (format t "[OPS] KB size: ~d~%" kb-size)
  (format t "[OPS] Processed: ~d~%" processed)
  (format t "[OPS] Queued: ~d~%" queued)
  (format t "[OPS] Skipped (missing CSV): ~d~%" skipped)
  (format t "[OPS] Start time (unix): ~d~%" start-time))
