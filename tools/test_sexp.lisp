#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(require :asdf)
(let* ((script (or *load-pathname* *compile-file-pathname*))
       (script-dir (when script (uiop:pathname-directory-pathname script)))
       (root (or (uiop:getenv "SWIMMY_HOME")
                 (when script-dir
                   (uiop:pathname-directory-pathname
                    (merge-pathnames "../" script-dir)))
                 (truename "."))))
  (push (probe-file (uiop:ensure-directory-pathname root))
        asdf:*central-registry*))
(ql:quickload :swimmy)

(in-package :swimmy.school)

(let* ((strat (make-strategy :name "TestStrategy"
                             :indicators '((:rsi 14))
                             :sl 0.003
                             :tp 0.006
                             :volume 0.01
                             :timeframe 1
                             :symbol "USDJPY"))
       (alist (strategy-to-alist strat))
       (sexp-str (prin1-to-string alist)))
  (format t "--- S-Expression Strategy ---~%")
  (format t "~a~%~%" sexp-str)
  
  (let ((ind-type (detect-indicator-type (strategy-indicators strat))))
    (format t "Detected Indicator Type: ~a~%" ind-type)
    (if (string= ind-type "rsi")
        (format t "✅ Indicator Detection Fixed!~%")
        (format t "❌ Indicator Detection Still Broken!~%"))))

(sb-ext:exit)
