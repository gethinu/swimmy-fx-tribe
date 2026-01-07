;;; test_runner.lisp
;;; V41.5: ASDF Integration + Proper Exit Codes

(in-package :cl-user)

(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(format t "[TEST RUNNER] Loading Swimmy via ASDF...~%")
(handler-case
    (asdf:load-system :swimmy)
  (error (e)
    (format t "[FATAL] ASDF Load Error: ~a~%" e)
    (sb-ext:exit :code 1)))

(format t "[TEST RUNNER] Executing Test Suite...~%")

(unless (find-package :swimmy.tests)
  (format t "[FATAL] SWIMMY.TESTS package not found!~%")
  (sb-ext:exit :code 1))

;; Run Tests and capture result
(multiple-value-bind (passed failed)
    (uiop:symbol-call :swimmy.tests :run-all-tests)
  (declare (ignore passed))
  (format t "[TEST RUNNER] Passed: ~d, Failed: ~d~%" passed failed)
  (sb-ext:exit :code (if (> failed 0) 1 0)))
