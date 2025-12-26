;; dreamer-test.lisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init) (load quicklisp-init)))

(ql:quickload :jsown)
(ql:quickload :dexador)

(defparameter *gemini-api-key* (uiop:getenv "SWIMMY_GEMINI_API_KEY"))

(defun call-test ()
  (format t "Testing Gemini API connection...~%")
  (if *gemini-api-key*
      (handler-case
          (let* ((url (format nil "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash-exp:generateContent?key=~a" *gemini-api-key*))
                 (payload (jsown:to-json
                           (jsown:new-js
                             ("contents" (list (jsown:new-js
                                                ("parts" (list (jsown:new-js ("text" "Say 'Hello Swimmy'")))))))
                             ("generationConfig" (jsown:new-js ("temperature" 0.9))))))
                 (response (dex:post url :content payload :headers '(("Content-Type" . "application/json")) :read-timeout 10)))
            (format t "Response: ~a~%" response))
        (error (e) (format t "API Error: ~a~%" e)))
      (format t "No API Key found!~%")))

(call-test)
