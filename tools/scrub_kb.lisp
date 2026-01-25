;; tools/scrub_kb.lisp
(in-package :cl-user)
(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init) (load quicklisp-init)))

(handler-bind ((style-warning #'muffle-warning)
               (warning #'muffle-warning))
  (asdf:load-system :swimmy))

(in-package :swimmy.school)

(defun scrub-kb ()
  (let ((b-ranks (get-strategies-by-rank :B))
        (scrubbed 0))
    (format t "[SCRUB] 🧹 Starting scrub of ~d B-Rank strategies...~%" (length b-ranks))
    (dolist (s b-ranks)
      (when (or (null (strategy-sharpe s)) (<= (strategy-sharpe s) 0.0))
        (setf (strategy-rank s) nil)
        (upsert-strategy s)
        (incf scrubbed)))
    (format t "[SCRUB] ✅ Scrubbed ~d unvalidated strategies.~%" scrubbed)))

(scrub-kb)
(sb-ext:exit)
