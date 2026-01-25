;; verify_fix.lisp
(in-package :cl-user)
(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init) (load quicklisp-init)))

(asdf:load-system :swimmy)

(in-package :swimmy.school)

(defun test-fix ()
  (let* ((pkg (find-package :swimmy.school))
         (test-expr '(cross-above (sma 5) (sma 20)))
         (expected (list 'cross-above 
                         (intern "SMA-5" pkg) 
                         (intern "SMA-20" pkg) 
                         (intern "SMA-5-PREV" pkg) 
                         (intern "SMA-20-PREV" pkg)))
         (result (transform-cross-calls-helper test-expr pkg)))
    (format t "Test expression: ~A~%" test-expr)
    (format t "Expected:        ~A~%" expected)
    (format t "Result:          ~A~%" result)
    (if (equal result expected)
        (format t "✅ Internal Transformation Test Passed!~%")
        (format t "❌ Internal Transformation Test Failed!~%")))

  (let* ((pkg (find-package :swimmy.school))
         (test-expr '(and (> CLOSE SMA-5) (cross-below (rsi 14) 70)))
         (result (transform-cross-calls-helper test-expr pkg)))
    (format t "Complex expression result: ~A~%" result)))

(test-fix)
(sb-ext:exit :code 0)
