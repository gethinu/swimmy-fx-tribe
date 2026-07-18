(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(push #p"/mnt/c/Repos/swimmy-fx-tribe/" asdf:*central-registry*)
(handler-case
    (handler-bind ((warning #'muffle-warning))
      (ql:quickload "swimmy" :silent t))
  (error (c) (format t "~&[LOAD-FAIL] ~a~%" c) (sb-ext:exit :code 3)))
(format t "~&==== swimmy loaded; running BUNDLE-FOUNDER harness ====~%")
(load "/mnt/c/Repos/swimmy-fx-tribe/logs/tribe_2d_bundle/bundle_founder_verify.lisp")
