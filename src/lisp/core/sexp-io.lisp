;;; sexp-io.lisp - Atomic S-expression read/write helpers

(in-package :swimmy.core)

(defun write-sexp-atomic (path form)
  "Write FORM to PATH atomically using temp file + rename."
  (let* ((base-defaults *default-pathname-defaults*)
         (target (merge-pathnames path base-defaults))
         (tmp (make-pathname :name (format nil "~a.tmp" (or (pathname-name target) "sexp"))
                             :type (pathname-type target)
                             :defaults target)))
    (ensure-directories-exist target)
    (with-open-file (out tmp :direction :output :if-exists :supersede)
      (with-standard-io-syntax
        (let ((*print-readably* t)
              (*print-pretty* nil)
              (*print-case* :downcase))
          (write form :stream out))))
    (rename-file tmp target)))

(defun read-sexp-file (path &key (package :swimmy.main))
  "Read a single S-expression from PATH using safe reader."
  (when (probe-file path)
    (let ((content (uiop:read-file-string path)))
      (safe-read-sexp content :package package))))
