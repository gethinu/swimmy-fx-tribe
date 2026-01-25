;; tools/correct_ranks.lisp
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
(format t "🛠️ BAS-RANK CORRECTION: Restoration of Gate Integrity~%")
(format t "════════════════════════════════════════════~%")

(let ((s-ranks (get-strategies-by-rank :S)))
  (format t "Demoting ~d unauthorized S-Rank strategies...~%" (length s-ranks))
  
  (dolist (s s-ranks)
    (format t "[CORRECT] Demoting ~a back to A-Rank (Integrity Failure)~%" (strategy-name s))
    (ensure-rank s :A "BAS-Rank Integrity Policy: Manual Acceleration Reversed"))

  ;; Final Audit in SQL
  (let ((s-count (swimmy.core:execute-single "SELECT count(*) FROM strategies WHERE rank = ':S'")))
    (format t "~%Final S-Rank count in SQL: ~d~%" s-count)
    (if (= s-count 0)
        (format t "✅ LEGIT: System integrity restored to BAS-Rank standards.~%")
        (format t "❌ WARNING: S-Rank count is still ~d!~%" s-count))))

(format t "════════════════════════════════════════════~%")
(sb-ext:exit :code 0)
