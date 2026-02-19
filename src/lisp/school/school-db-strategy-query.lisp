;;; school-db-strategy-query.lisp - Strategy retrieval helpers

(in-package :swimmy.school)

(defun %db-rank->keyword (rank)
  "Normalize DB rank cell into keyword rank (or NIL)."
  (let* ((raw (cond
                ((null rank) nil)
                ((stringp rank) rank)
                ((symbolp rank) (symbol-name rank))
                (t (format nil "~a" rank))))
         (token (and raw
                     (string-upcase
                      (string-trim '(#\Space #\Tab #\Newline #\Return) raw)))))
    (cond
      ((or (null token) (string= token "") (string= token "NIL")) nil)
      (t (let ((name (if (char= (char token 0) #\:)
                         (subseq token 1)
                         token)))
           (intern name :keyword))))))

(defun %deserialize-strategy-row (row)
  "Decode DB row containing data_sexp and optional rank."
  (let* ((sexp-str (first row))
         (has-rank-col (and (consp row) (consp (cdr row))))
         (db-rank (when has-rank-col (second row)))
         (obj (swimmy.core:safe-read-sexp sexp-str :package :swimmy.school)))
    (if (strategy-p obj)
        (progn
          ;; rank column is authoritative when present (data_sexp can be stale).
          (when has-rank-col
            (setf (strategy-rank obj) (%db-rank->keyword db-rank)))
          obj)
        (progn
          (when (and sexp-str (> (length sexp-str) 0))
            (format t "[DB] 💥 Corrupted Strategy SEXP (safe-read): ~a...~%"
                    (subseq sexp-str 0 (min 30 (length sexp-str)))))
          nil))))

(defun fetch-candidate-strategies (&key (min-sharpe 0.1) (ranks '(":B" ":A" ":S")))
  "Fetch strategies from DB matching criteria for the global draft."
  (let ((query (format nil "SELECT data_sexp, rank FROM strategies WHERE sharpe >= ? AND rank IN (~{~a~^,~})"
                       (mapcar (lambda (r) (format nil "'~a'" r)) ranks))))
    (let ((rows (execute-to-list query min-sharpe)))
      (remove-if #'null
                 (mapcar #'%deserialize-strategy-row rows)))))

(defun fetch-all-strategies-from-db ()
  "Fetch EVERY strategy from the DB, including unranked and legends."
  (let ((rows (execute-to-list "SELECT data_sexp, rank FROM strategies")))
    (remove-if #'null
               (mapcar #'%deserialize-strategy-row rows))))

(defun collect-all-strategies-unpruned ()
  "Return all strategies from DB + Library without pruning."
  (let* ((db-strats (fetch-all-strategies-from-db))
         (file-strats (ignore-errors (swimmy.persistence:load-all-strategies)))
         (all (copy-list db-strats))
         (seen (make-hash-table :test #'equal)))
    (dolist (dbs db-strats)
      (let ((name (and dbs (strategy-name dbs))))
        (when name
          (setf (gethash name seen) t))))
    (dolist (fs (or file-strats '()))
      (let ((name (and fs (strategy-name fs))))
        (if name
            (unless (gethash name seen)
              (setf (gethash name seen) t)
              (push fs all))
            (push fs all))))
    all))

(defun map-strategies-from-db (fn &key (batch-size 1000) limit)
  "Call FN for each strategy in DB, processing in batches. Returns count."
  (block done
    (let ((offset 0)
          (processed 0))
      (loop
        (when (and limit (>= processed limit))
          (return-from done processed))
        (let ((rows (execute-to-list "SELECT data_sexp, rank FROM strategies LIMIT ? OFFSET ?"
                                     batch-size offset)))
          (when (null rows)
            (return-from done processed))
          (dolist (row rows)
            (when (and limit (>= processed limit))
              (return-from done processed))
            (let ((sexp-str (first row)))
              (handler-case
                  (let ((obj (%deserialize-strategy-row row)))
                    (when obj
                      (funcall fn obj)
                      (incf processed)))
                (error (e)
                  (format t "[DB] 💥 Corrupted Strategy SEXP: ~a... Error: ~a~%"
                          (subseq sexp-str 0 (min 30 (length sexp-str))) e))))))
          (incf offset batch-size)))))
