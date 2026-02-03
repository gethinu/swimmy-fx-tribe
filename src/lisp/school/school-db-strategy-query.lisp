;;; school-db-strategy-query.lisp - Strategy retrieval helpers

(in-package :swimmy.school)

(defun fetch-candidate-strategies (&key (min-sharpe 0.1) (ranks '(":B" ":A" ":S")))
  "Fetch strategies from DB matching criteria for the global draft."
  (let ((query (format nil "SELECT data_sexp FROM strategies WHERE sharpe >= ? AND rank IN (~{~a~^,~})"
                       (mapcar (lambda (r) (format nil "'~a'" r)) ranks))))
    (let ((rows (execute-to-list query min-sharpe)))
      (remove-if #'null
                 (mapcar (lambda (row)
                           (let ((sexp-str (first row)))
                             (let ((obj (swimmy.core:safe-read-sexp sexp-str :package :swimmy.school)))
                               (if (strategy-p obj)
                                   obj
                                   (progn
                                     (when (and sexp-str (> (length sexp-str) 0))
                                       (format t "[DB] 💥 Corrupted Strategy SEXP (safe-read): ~a...~%"
                                               (subseq sexp-str 0 (min 30 (length sexp-str)))))
                                     nil)))))
                         rows)))))

(defun fetch-all-strategies-from-db ()
  "Fetch EVERY strategy from the DB, including unranked and legends."
  (let ((rows (execute-to-list "SELECT data_sexp FROM strategies")))
    (remove-if #'null
               (mapcar (lambda (row)
                         (let ((sexp-str (first row)))
                           (let ((obj (swimmy.core:safe-read-sexp sexp-str :package :swimmy.school)))
                             (if (strategy-p obj)
                                 obj
                                 (progn
                                   (when (and sexp-str (> (length sexp-str) 0))
                                     (format t "[DB] 💥 Corrupted Strategy SEXP (safe-read): ~a...~%"
                                             (subseq sexp-str 0 (min 30 (length sexp-str)))))
                                   nil)))))
                       rows))))

(defun collect-all-strategies-unpruned ()
  "Return all strategies from DB + Library without pruning."
  (let* ((db-strats (fetch-all-strategies-from-db))
         (file-strats (ignore-errors (swimmy.persistence:load-all-strategies)))
         (all (copy-list db-strats)))
    (dolist (fs (or file-strats '()))
      (unless (find (strategy-name fs) all :key #'strategy-name :test #'string=)
        (push fs all)))
    all))

(defun map-strategies-from-db (fn &key (batch-size 1000) limit)
  "Call FN for each strategy in DB, processing in batches. Returns count."
  (block done
    (let ((offset 0)
          (processed 0))
      (loop
        (when (and limit (>= processed limit))
          (return-from done processed))
        (let ((rows (execute-to-list "SELECT data_sexp FROM strategies LIMIT ? OFFSET ?" batch-size offset)))
          (when (null rows)
            (return-from done processed))
          (dolist (row rows)
            (when (and limit (>= processed limit))
              (return-from done processed))
            (let ((sexp-str (first row)))
              (handler-case
                  (let ((*package* (find-package :swimmy.school)))
                    (let ((obj (read-from-string sexp-str)))
                      (when (strategy-p obj)
                        (funcall fn obj)
                        (incf processed))))
                (error (e)
                  (format t "[DB] 💥 Corrupted Strategy SEXP (pkg: ~a): ~a... Error: ~a~%"
                          *package*
                          (subseq sexp-str 0 (min 30 (length sexp-str))) e))))))
          (incf offset batch-size)))))
