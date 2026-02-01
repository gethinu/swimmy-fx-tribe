#!/usr/bin/env sbcl --script
;; Legend sync checker: DB vs filesystem + revalidation pending count

(require 'asdf)
(let ((ql (or (uiop:getenv "QUICKLISP_SETUP")
              (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))))
  (when (and ql (probe-file ql))
    (load ql)))

(load "swimmy.asd")
(asdf:load-system :swimmy)

(in-package :swimmy.school)

(defun list-files (dir)
  (let ((path (merge-pathnames (format nil "~a/*.lisp" dir)
                               swimmy.persistence:*library-path*)))
    (mapcar #'pathname-name (directory path))))

(defun db-names (rank)
  (mapcar #'car
          (execute-to-list "SELECT name FROM strategies WHERE rank=?" (format nil "~s" rank))))

(defun summary ()
  (let* ((fs-legend (list-files "LEGEND"))
         (fs-archive (list-files "LEGEND-ARCHIVE"))
         (db-legend (db-names :legend))
         (db-archive (db-names :legend-archive))
         (pending (remove-if-not #'strategy-revalidation-pending *strategy-knowledge-base*)))
    (format t "LEGEND sync check~%")
    (format t "FS Legend: ~d, DB Legend: ~d~%" (length fs-legend) (length db-legend))
    (format t "FS Archive: ~d, DB Archive: ~d~%" (length fs-archive) (length db-archive))
    (format t "Revalidation pending (memory): ~d~%" (length pending))
    (let ((missing-in-db (set-difference fs-legend db-legend :test #'string=))
          (missing-in-fs (set-difference db-legend fs-legend :test #'string=)))
      (when missing-in-db
        (format t "⚠️ FS→DB差分 (DBに無い): ~a~%" missing-in-db))
      (when missing-in-fs
        (format t "⚠️ DB→FS差分 (FSに無い): ~a~%" missing-in-fs)))
    (sb-ext:quit :unix-status 0)))

(summary)
