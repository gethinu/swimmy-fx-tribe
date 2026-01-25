;; tools/audit_a_rank.lisp
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
(format t "👑 A-RANK ELITE AUDIT (Candidate for S-Rank)~%")
(format t "════════════════════════════════════════════~%")

(let ((a-ranks (sort (copy-list (get-strategies-by-rank :A))
                     #'> :key (lambda (s) (or (strategy-sharpe s) 0.0)))))
  (format t "Found ~d candidates.~%~%" (length a-ranks))
  (format t "~30a | ~6a | ~5a | ~5a | ~5a~%" "Name" "Sharpe" "PF" "WR%" "DD%")
  (format t "------------------------------------------------------------~%")
  (dolist (s a-ranks)
    (format t "~30a | ~,2f   | ~,2f | ~,1f | ~,1f~%"
            (subseq (strategy-name s) 0 (min 30 (length (strategy-name s))))
            (or (strategy-sharpe s) 0.0)
            (or (strategy-profit-factor s) 0.0)
            (* 100 (or (strategy-win-rate s) 0.0))
            (* 100 (or (strategy-max-dd s) 0.0))))
  
  (format t "~%--- S-Rank Thresholds ---~%")
  (format t "Sharpe >= 0.50 | PF >= 1.50 | WR >= 45.0% | DD < 15.0%~%"))

(format t "════════════════════════════════════════════~%")
(sb-ext:exit :code 0)
