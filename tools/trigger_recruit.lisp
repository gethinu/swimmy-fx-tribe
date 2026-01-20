(in-package :cl-user)

(require 'asdf)

;; Add current directory to ASDF registry
(push (uiop:getcwd) asdf:*central-registry*)

(handler-case
    (progn
      (format t "Loading Swimmy System...~%")
      (ql:quickload :swimmy :silent t)
      
      (format t "~%[TEST] 🧪 Triggering Phase 8 Recruitment...~%")
      
      ;; Access symbols via package qualification since we are in CL-USER
      (let ((func (find-symbol "RECRUIT-SCOUT" :swimmy.school))
            (var  (find-symbol "*SCOUT-ATTEMPTS*" :swimmy.school)))
        
        (if (and func var)
            (progv (list var) (list 5) ;; Dynamic binding of *scout-attempts* to 5
              (funcall func))
            (format t "❌ Could not find symbols in SWIMMY.SCHOOL~%")))
      
      (format t "[TEST] ✅ Done.~%"))
  (error (e)
    (format t "[TEST] ❌ Error: ~a~%" e)
    (uiop:quit 1)))

(uiop:quit 0)
