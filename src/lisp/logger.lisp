;; logger.lisp - Structured Logging System (Graham Advisor)
;; Provides unified logging interface with JSON support
;; Phase 2 Step 2: Structured Logging Implementation

(in-package :swimmy.core)

(defparameter *log-file-path* (merge-pathnames "swimmy/logs/swimmy.json.log" (user-homedir-pathname)))
(defparameter *enable-json-log* t)
(defparameter *log-level* :info) ; :debug, :info, :warn, :error

(defun get-iso-8601-time ()
  "Get current time in ISO 8601 format"
  (multiple-value-bind (s m h d mo y) (decode-universal-time (get-universal-time))
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d" y mo d h m s)))

(defun log-to-file (level message data)
  "Append log entry to JSON log file"
  (when *enable-json-log*
    (handler-case
        (with-open-file (out *log-file-path* 
                             :direction :output 
                             :if-exists :append 
                             :if-does-not-exist :create)
          (let ((entry (jsown:new-js
                         ("timestamp" (get-iso-8601-time))
                         ("level" (string-downcase level))
                         ("message" message))))
            (when data
              ;; Handle specific data types for better serialization
              (cond 
                ((stringp data) (setf (jsown:val entry "data") data))
                ((numberp data) (setf (jsown:val entry "data") data))
                ;; Assumption: if it's a list/alist meant for JSON, it's structured properly or is jsown obj
                (t (setf (jsown:val entry "data") data))))
            (write-line (jsown:to-json entry) out)))
      (error (e) (format t "[LOG_ERROR] Failed to write log: ~a~%" e)))))

(defun safe-format-t (control-string &rest args)
  "Safely write to *standard-output*, ignoring broken pipe errors"
  (handler-case
      (apply #'format t control-string args)
    (sb-int:broken-pipe () nil)
    (stream-error () nil)))

(defun log-info (message &rest args)
  "Log INFO level message. Use :data keyword arg for structured data."
  (let ((data (getf args :data)))
    (log-to-file :info message data)
    (safe-format-t "[L] [INFO] ~a~%" message)))

(defun log-warn (message &rest args)
  "Log WARN level message. Use :data keyword arg for structured data."
  (let ((data (getf args :data)))
    (log-to-file :warn message data)
    (safe-format-t "[L] [WARN] ⚠️ ~a~%" message)))

(defun log-error (message &rest args)
  "Log ERROR level message. Use :data keyword arg for structured data."
  (let ((data (getf args :data)))
    (log-to-file :error message data)
    (safe-format-t "[L] [ERROR] 🚨 ~a~%" message)))

(defun log-debug (message &rest args)
  "Log DEBUG level message (only if log level is debug)"
  (when (eq *log-level* :debug)
    (let ((data (getf args :data)))
      (log-to-file :debug message data)
      (safe-format-t "[L] [DEBUG] ~a~%" message))))

(format t "[L] 🪵 logger.lisp loaded - Structured logging active~%")
