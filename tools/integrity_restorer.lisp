;; tools/integrity_restorer.lisp
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
(format t "🛡️ BAS-RANK INTEGRITY RESTORER~%")
(format t "════════════════════════════════════════════~%")

(let ((s-ranks (swimmy.core:execute-to-list "SELECT name, sharpe, profit_factor, win_rate, max_dd FROM strategies WHERE rank = ':S'")))
  (format t "Auditing ~d current S-Rank strategies...~%~%" (length s-ranks))
  
  (dolist (row s-ranks)
    (let* ((name (first row))
           (sharpe (or (second row) 0.0))
           (pf (or (third row) 0.0))
           (wr (* 100 (or (fourth row) 0.0)))
           (dd (* 100 (or (fifth row) 0.0)))
           ;; Strict Criteria Check
           (pass-s (and (>= sharpe 0.5) 
                        (>= pf 1.5) 
                        (>= wr 45.0) 
                        (< dd 15.0))))
      
      (unless pass-s
        (format t "[RESTORE] Demoting ~30a | S=~,2f PF=~,2f WR=~,1f% DD=~,1f% -> A-Rank (Integrity Violation)~%"
                name sharpe pf wr dd)
        ;; Demote directly in SQL to ensure persistence if KB is out of sync
        (swimmy.core:execute-non-query "UPDATE strategies SET rank = ':A' WHERE name = ?" name))))

  (let ((final-count (swimmy.core:execute-single "SELECT count(*) FROM strategies WHERE rank = ':S'")))
    (format t "~%Final Elite Count (S-Rank): ~d~%" final-count)
    (if (= final-count 0)
        (format t "✅ System integrity restored. Zero unauthorized S-Ranks remain.~%")
        (format t "🎖️ ~d legitimate S-Ranks remain in the elite pool.~%" final-count))))

(format t "════════════════════════════════════════════~%")
(sb-ext:exit :code 0)
