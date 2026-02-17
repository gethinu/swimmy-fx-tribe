;;; safe-read.lisp - Safe S-expression reader

(in-package :swimmy.core)

(defun %sexp-token-boundary-char-p (ch)
  "Return T when CH separates S-expression tokens."
  (or (null ch)
      (member ch '(#\Space #\Tab #\Newline #\Return
                   #\( #\) #\[ #\] #\{ #\}
                   #\" #\' #\` #\, #\;) :test #'char=)))

(defun %normalize-sharp-bool-tokens (input)
  "Normalize #t/#f tokens to t/nil outside quoted strings.
This keeps Lisp reader compatibility with serde_lexpr boolean literals."
  (let ((len (length input))
        (idx 0)
        (in-string nil)
        (escaped nil))
    (with-output-to-string (out)
      (loop while (< idx len) do
        (let ((ch (char input idx)))
          (cond
            (in-string
             (write-char ch out)
             (cond
               (escaped
                (setf escaped nil))
               ((char= ch #\\)
                (setf escaped t))
               ((char= ch #\")
                (setf in-string nil)))
             (incf idx))
            ((char= ch #\")
             (write-char ch out)
             (setf in-string t)
             (incf idx))
            ((and (char= ch #\#)
                  (< (1+ idx) len)
                  (let* ((next (char-downcase (char input (1+ idx))))
                         (prev (when (> idx 0) (char input (1- idx))))
                         (after-pos (+ idx 2))
                         (after (when (< after-pos len) (char input after-pos))))
                    (and (member next '(#\t #\f) :test #'char=)
                         (%sexp-token-boundary-char-p prev)
                         (%sexp-token-boundary-char-p after))))
             (if (char= (char-downcase (char input (1+ idx))) #\t)
                 (write-char #\t out)
                 (write-string "nil" out))
             (incf idx 2))
            (t
             (write-char ch out)
             (incf idx))))))))

(defun safe-read-sexp (input &key (package :swimmy.main))
  "Safely read a single S-expression string. Returns form or NIL on error."
  (when (and input (stringp input))
    (handler-case
        (let* ((normalized (%normalize-sharp-bool-tokens input))
               (*read-eval* nil)
              (*package* (find-package package)))
          (multiple-value-bind (form _) (read-from-string normalized nil nil)
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
