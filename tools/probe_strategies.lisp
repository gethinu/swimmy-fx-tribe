(in-package :cl-user)
(load "src/lisp/packages.lisp")
(load "src/lisp/packages-school.lisp")
(load "src/lisp/core/persistence.lisp")

(defun probe-incubator ()
  (let ((files (directory "data/library/INCUBATOR/*.lisp")))
    (format t "Probing ~d files...~%" (length files))
    (loop for f in (subseq files 0 (min 5 (length files))) do
      (let ((s (swimmy.persistence:load-strategy f)))
        (format t "Strategy: ~a~%  Indicators: ~a (Type: ~a)~%" 
                (swimmy.school:strategy-name s)
                (swimmy.school:strategy-indicators s)
                (type-of (swimmy.school:strategy-indicators s)))))))

(probe-incubator)
(sb-ext:exit)
