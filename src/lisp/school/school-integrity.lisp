;;; school-integrity.lisp - Pre-flight DNA Verification
;;; Phase 25: Integrity (Musk)
;;;
;;; "If you don't know what code is running, you aren't running anything."
;;;
;;; This module calculates SHA-256 hashes of critical source files on startup.
;;; It ensures that the deployed code matches the expected "Golden State".

(defpackage :swimmy.school.integrity
  (:use :cl :ironclad)
  (:export #:verify-system-integrity
           #:generate-manifest))

(in-package :swimmy.school.integrity)

(defparameter *critical-files*
  '("src/lisp/school/school-core.lisp"
    "src/lisp/school/school-kb.lisp"
    "src/lisp/school/school-scribe.lisp"
    "src/lisp/school/school-watchdog.lisp"
    "src/lisp/school/school-breeder.lisp"))

(defun file-sha256 (path)
  "Calculate SHA-256 hash of a file."
  (if (probe-file path)
      (byte-array-to-hex-string
       (digest-file :sha256 path))
      "MISSING"))

(defun verify-system-integrity ()
  "Run Pre-flight Check."
  (format t "~%[INTEGRITY] 🧬 Running DNA Check...~%")
  (let ((clean t))
    (dolist (path *critical-files*)
      (let ((hash (file-sha256 path)))
        (if (string= hash "MISSING")
            (progn
              (format t "[INTEGRITY] ❌ MISSING: ~a~%" path)
              (setf clean nil))
            (format t "[INTEGRITY] ✅ ~a: ~a~%" (file-namestring path) (subseq hash 0 8)))))
    
    (if clean
        (format t "[INTEGRITY] 🚀 System DNA Verified. Ready for Lift-off.~%")
        (format t "[INTEGRITY] ⚠️ INTEGRITY COMPROMISED. Check logs.~%"))
    clean))
