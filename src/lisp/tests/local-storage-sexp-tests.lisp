;;; local-storage-sexp-tests.lisp - S-expression IO tests

(in-package :swimmy.tests)

(deftest test-sexp-io-roundtrip
  (let* ((tmp (merge-pathnames "sexp_io_test.sexp" #P"/tmp/"))
         (payload '((schema_version . 1) (name . "X") (value . 42))))
    (unwind-protect
        (progn
          (swimmy.core:write-sexp-atomic tmp payload)
          (let ((read (swimmy.core:read-sexp-file tmp :package :swimmy.school)))
            (assert-equal payload read)))
      (when (probe-file tmp) (delete-file tmp)))))
