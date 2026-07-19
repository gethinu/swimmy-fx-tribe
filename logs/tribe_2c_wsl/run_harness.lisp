(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(push #p"/mnt/c/Repos/swimmy-fx-tribe/" asdf:*central-registry*)
(handler-case
    (handler-bind ((warning #'muffle-warning))
      (ql:quickload "swimmy" :silent t))
  (error (c) (format t "~&[LOAD-FAIL] ~a~%" c) (sb-ext:exit :code 3)))
(format t "~&==== swimmy loaded; running daemon-verify harness ====~%")
(load (merge-pathnames "daemon_verify.lisp" (user-homedir-pathname)))
