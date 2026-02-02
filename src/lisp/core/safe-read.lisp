;;; safe-read.lisp - Safe S-expression reader

(in-package :swimmy.core)

(defun safe-read-sexp (input &key (package :swimmy.main))
  "Safely read a single S-expression string. Returns form or NIL on error."
  (when (and input (stringp input))
    (handler-case
        (let ((*read-eval* nil)
              (*package* (find-package package)))
          (multiple-value-bind (form _) (read-from-string input nil nil)
            (declare (ignore _))
            form))
      (error () nil))))
