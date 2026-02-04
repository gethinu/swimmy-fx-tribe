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

(defun safe-parse-number (input)
  "Safely parse a numeric string. Returns number or NIL."
  (when (and input (stringp input))
    (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) input)))
      (when (> (length trimmed) 0)
        (when (and (some #'digit-char-p trimmed)
                   (every (lambda (ch)
                            (or (digit-char-p ch)
                                (member ch '(#\. #\+ #\- #\e #\E))))
                          trimmed))
          (let ((form (safe-read-sexp trimmed :package :swimmy.main)))
            (when (numberp form) form)))))))
