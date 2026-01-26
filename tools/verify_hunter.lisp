;;; tools/verify_hunter.lisp
;;; ============================================================================
;;; HUNTER INTEGRITY CHECKER (Quality Gate)
;;; ============================================================================
;;; This script verifies that the dynamically generated `school-hunter.lisp`
;;; is syntactically correct and can be loaded by the main system.
;;; It prevents "silent failures" where strategies are generated but not loaded.
;;; ============================================================================

(require :asdf)
(in-package :cl-user)

(defun exit-error (msg)
  (format t "~%[❌ HUNTER VERIFICATION FAILED] ~a~%" msg)
  (sb-ext:exit :code 1))

(defun exit-success ()
  (format t "~%[✅ HUNTER VERIFICATION PASSED] Strategies are loadable.~%")
  (sb-ext:exit :code 0))

(format t "[VERIFY] Loading Swimmy System Environment...~%")

;; 1. Load Core System
(handler-case
    (progn
      (push (uiop:getcwd) asdf:*central-registry*)
      (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
        (when (probe-file quicklisp-init)
          (load quicklisp-init)))
      (handler-bind ((warning (lambda (w) (declare (ignore w)) (muffle-warning w))))
        (asdf:load-system :swimmy)))
  (error (e)
    (exit-error (format nil "Failed to load base system: ~a" e))))

;; 2. Verify Hunter File Exists
(defparameter *hunter-file* (merge-pathnames "src/lisp/school/school-hunter.lisp" (uiop:getcwd)))
(unless (probe-file *hunter-file*)
  (format t "[WARN] Hunter file not found at ~a. Skipping check (Clean install?).~%" *hunter-file*)
  (exit-success))

;; 3. Attempt Load
(format t "[VERIFY] Attempting to load ~a...~%" *hunter-file*)
(handler-case
    (load *hunter-file*)
  (error (e)
    (exit-error (format nil "Syntax Error in Hunter file: ~a" e))))

;; 4. Verify Registry Population
(if (and (boundp 'swimmy.school::*founder-registry*)
         (> (hash-table-count swimmy.school::*founder-registry*) 0))
    (format t "[VERIFY] Founder Registry contains ~d strategies.~%" 
            (hash-table-count swimmy.school::*founder-registry*))
    (format t "[WARN] Founder Registry is empty. (Maybe normal if no strategies yet)~%"))

(exit-success)
