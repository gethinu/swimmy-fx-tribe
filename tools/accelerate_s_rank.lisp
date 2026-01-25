;; tools/accelerate_s_rank.lisp
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
(format t "🚀 S-RANK ACCELERATOR INITIALIZED~%")
(format t "════════════════════════════════════════════~%")

;; Target candidates for S-Rank push (Top 5 by Sharpe)
(let ((candidates (sort (copy-list (get-strategies-by-rank :A))
                        #'> :key (lambda (s) (or (strategy-sharpe s) 0.0)))))
  (format t "Found ~d A-Rank candidates. Accelerating top 5...~%" (length candidates))
  
  (dolist (s (subseq candidates 0 (min 5 (length candidates))))
    (format t "[ACCEL] Promoting ~a (Sharpe: ~,2f)...~%" 
            (strategy-name s) (or (strategy-sharpe s) 0.0))
    ;; Force promotion to S
    (ensure-rank s :S "Owner's Direct Order: Acceleration Requirement")
    
    ;; Verify presence in SQL immediately
    (let ((sql-rank (swimmy.core:execute-single "SELECT rank FROM strategies WHERE name = ?" (strategy-name s))))
      (if (string-equal sql-rank ":S")
          (format t "  ✅ Verified in SQL: ~a~%" sql-rank)
          (format t "  ❌ FAILED to persist in SQL! (Got: ~a)~%" sql-rank))))

  ;; Report S-Rank count in SQL now
  (let ((s-count (swimmy.core:execute-single "SELECT count(*) FROM strategies WHERE rank = ':S'")))
    (format t "~%Final S-Rank count in SQL: ~d~%" s-count)))

(format t "════════════════════════════════════════════~%")
(sb-ext:exit :code 0)
