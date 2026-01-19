
;; tools/shard_legends.lisp
(require :sb-posix)

;; 1. Load Dependencies
(require :sb-posix)
(unless (find-package :ql)
  (let ((ql-setup "~/quicklisp/setup.lisp"))
    (if (probe-file ql-setup)
        (load ql-setup)
        (format t "⚠️ Quicklisp not found at ~a~%" ql-setup))))

(ql:quickload :cl-json)
(ql:quickload :cffi)
(ql:quickload :pzmq)
(ql:quickload :jsown)

(load "src/lisp/packages.lisp")
(load "src/lisp/packages-school.lisp")
(load "src/lisp/core/globals.lisp")
(load "src/lisp/dsl.lisp")
(load "src/lisp/core/persistence.lisp")
(load "src/lisp/school/school-state.lisp")
(load "src/lisp/school/school-strategy.lisp")

;; 2. Load Legendary Strategies (Code-as-Data)
(format t "[LEGEND] 🏰 Loading Legendary Strategies...~%")
(load "src/lisp/strategies/strategies-legendary.lisp")

;; 3. Shard Them (The list is now in *strategy-knowledge-base*)
(format t "[LEGEND] 💾 Sharding Legends...~%")
(dolist (s swimmy.school::*strategy-knowledge-base*)
  ;; We assume *strategy-knowledge-base* ONLY contains legends now 
  ;; (since we didn't load the full library or other files)
  ;; However, to be safe, we only save if it's not already in the library? 
  ;; No, overwrite is fine.
  (swimmy.persistence:save-strategy s))

(format t "[LEGEND] ✅ Sharding Complete!~%")
(sb-ext:exit)
