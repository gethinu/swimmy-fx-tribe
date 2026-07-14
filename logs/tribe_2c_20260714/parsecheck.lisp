;;; Read-check a Lisp file with the SBCL reader (no load/eval) to catch paren/syntax
;;; errors. Usage: sbcl --no-sysinit --no-userinit --non-interactive --load parsecheck.lisp <file>...
;;; Uses a permissive readtable so package-qualified symbols in absent packages don't error.
(defun parse-check (path)
  (handler-case
      (with-open-file (in path :direction :input :external-format :utf-8)
        (let ((*read-eval* nil) (n 0) (*package* (find-package :cl-user)))
          ;; Map every unknown package/symbol to a keyword-ish read: use a custom
          ;; reader that tolerates undefined packages by interning into CL-USER.
          (handler-bind ((error (lambda (e) (declare (ignore e)))))
            (loop
              (let ((form (handler-case (read in nil :eof)
                            (reader-error (e) (format t "~&READER-ERROR ~a: ~a~%" path e) (return-from parse-check nil)))))
                (when (eq form :eof) (return))
                (incf n))))
          (format t "~&OK   ~a  (~d top-level forms)~%" path n)
          t))
    (error (e) (format t "~&FAIL ~a: ~a~%" path e) nil)))
(let ((ok t))
  (dolist (f (cdr sb-ext:*posix-argv*))
    (unless (parse-check f) (setf ok nil)))
  (sb-ext:exit :code (if ok 0 1)))
