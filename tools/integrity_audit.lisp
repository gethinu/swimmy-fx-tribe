;; tools/integrity_audit.lisp
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
(format t "⚖️ RANK INTEGRITY AUDIT: CRITERIA CHECK~%")
(format t "════════════════════════════════════════════~%")

(let ((s-ranks (swimmy.core:execute-to-list "SELECT name, sharpe, profit_factor, win_rate, max_dd FROM strategies WHERE rank = ':S'")))
  (format t "Strict S-Rank Thresholds:~%")
  (format t "  Sharpe >= 0.50 | PF >= 1.50 | WR >= 45.0% | DD < 15.0%~%~%")
  
  (format t "~30a | ~6a | ~6a | ~6a | ~6a | ~10a~%" "Name" "Sharpe" "PF" "WR%" "DD%" "Status")
  (format t "-------------------------------------------------------------------------~%")
  
  (dolist (row s-ranks)
    (let* ((name (first row))
           (sharpe (or (second row) 0.0))
           (pf (or (third row) 0.0))
           (wr (* 100 (or (fourth row) 0.0)))
           (dd (* 100 (or (fifth row) 0.0)))
           (pass-s (and (>= sharpe 0.5) (>= pf 1.5) (>= wr 45.0) (< dd 15.0))))
      (format t "~30a | ~,2f   | ~,2f   | ~,1f   | ~,1f   | ~a~%"
              (subseq name 0 (min 30 (length name)))
              sharpe pf wr dd
              (if pass-s "✅ LEGIT" "❌ DEVIATION")))))

(format t "════════════════════════════════════════════~%")
(sb-ext:exit :code 0)
