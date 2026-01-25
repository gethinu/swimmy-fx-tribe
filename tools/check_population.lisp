;; tools/check_population.lisp
(in-package :cl-user)
(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init) (load quicklisp-init)))

(handler-bind ((style-warning #'muffle-warning)
               (warning #'muffle-warning))
  (asdf:load-system :swimmy))

(in-package :swimmy.school)

(defun audit-population ()
  (let* ((b-ranks (get-strategies-by-rank :B))
         (a-ranks (get-strategies-by-rank :A))
         (s-ranks (get-strategies-by-rank :S))
         (elite-b (remove-if-not (lambda (s) (and (strategy-sharpe s) (>= (strategy-sharpe s) 0.3))) b-ranks))
         (elite-a (remove-if-not (lambda (s) (and (strategy-sharpe s) (>= (strategy-sharpe s) 0.5))) a-ranks)))
    
    (format t "════════════════════════════════════════════~%")
    (format t "📊 POPULATION AUDIT (V50.4)~%")
    (format t "════════════════════════════════════════════~%")
    (format t "Total Strategies:~%")
    (format t "  B-Rank: ~d (~d candidates for OOS)~%" (length b-ranks) (length elite-b))
    (format t "  A-Rank: ~d (~d candidates for CPCV)~%" (length a-ranks) (length elite-a))
    (format t "  S-Rank: ~d~%" (length s-ranks))
    (format t "════════════════════════════════════════════~%")
    
    (when (and (null s-ranks) elite-a)
      (format t "🎯 ACTION: ~d A-Rank elites found. Dispatching CPCV batch...~%" (length elite-a))
      (run-a-rank-cpcv-batch))))

(audit-population)
(sb-ext:exit)
