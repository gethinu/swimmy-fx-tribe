;;; src/lisp/core/profiling.lisp
;;; Simple profiling macro for measuring execution time of code blocks
(in-package :swimmy.main)

(defmacro with-profiling (section-name &body body)
  "Execute BODY and log the elapsed time with SECTION-NAME.
   The log is printed to the console; in production you may redirect to a file.
   Example usage: (with-profiling \"maintenance-section-1\" (do-something))" 
  `(let ((start (get-internal-real-time)))
     (unwind-protect
         (progn ,@body)
       (let* ((end (get-internal-real-time))
              (seconds (float (/ (- end start) internal-time-units-per-second))))
         (when (> seconds 0.05)
           (format t "[PROFILING] ~a took ~,3f seconds~%" ,section-name seconds))))))
