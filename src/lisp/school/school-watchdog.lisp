;;; school-watchdog.lisp - Latency Guardian
;;; Phase 25: Watchdog (Hamilton)
;;;
;;; "The software must not stop. It must allow for restart." - Margaret Hamilton
;;;
;;; This module spawns a high-priority thread that monitors the Main Loop's "Pulse".
;;; If the Pulse is not updated within 100ms, it declares a BROKEN ARROW event.

(defpackage :swimmy.school.watchdog
  (:use :cl :bordeaux-threads)
  (:export #:start-watchdog
           #:stop-watchdog
           #:watchdog-pulse
           #:watchdog-status))

(in-package :swimmy.school.watchdog)

;;; ============================================================================
;;; STATE
;;; ============================================================================

(defparameter *watchdog-thread* nil)
(defparameter *watchdog-running* nil)
(defparameter *last-pulse* 0)         ; Timestamp (internal-real-time)
(defparameter *pulse-lock* (make-lock "WatchdogPulse"))

(defparameter *latency-threshold-ms* 100) ; 100ms (Broken Arrow Limit)
(defparameter *broken-arrow-count* 0)

;;; ============================================================================
;;; THE GUARDIAN
;;; ============================================================================

(defun start-watchdog ()
  "Start the Latency Guardian."
  (when *watchdog-running*
    (format t "[WATCHDOG] ⚠️ Already running.~%")
    (return-from start-watchdog))
  
  (setf *last-pulse* (get-internal-real-time))
  (setf *watchdog-running* t)
  (setf *broken-arrow-count* 0)
  
  (setf *watchdog-thread*
        (make-thread #'watchdog-loop :name "WatchdogGuardian"))
  
  (format t "[WATCHDOG] 🐕 Guardian Active. Threshold: ~dms~%" *latency-threshold-ms*))

(defun stop-watchdog ()
  (setf *watchdog-running* nil)
  (when *watchdog-thread*
    (join-thread *watchdog-thread*)
    (setf *watchdog-thread* nil))
  (format t "[WATCHDOG] 💤 Guardian Sleeping.~%"))

(defun watchdog-pulse ()
  "Called by Main Loop to prove implementation aliveness."
  (setf *last-pulse* (get-internal-real-time)))

(defun watchdog-loop ()
  "High-frequency check loop."
  (format t "[WATCHDOG] 👁️ Monitoring...~%")
  (loop while *watchdog-running* do
    (sleep 0.05) ; Check every 50ms
    
    (let* ((now (get-internal-real-time))
           (diff (- now *last-pulse*))
           (diff-ms (* (/ diff internal-time-units-per-second) 1000.0)))
      
      (when (> diff-ms *latency-threshold-ms*)
        (incf *broken-arrow-count*)
        (format *error-output* "~%[WATCHDOG] 🚨 BROKEN ARROW! Main Loop Lag: ~,2f ms (> ~d ms)~%" 
                diff-ms *latency-threshold-ms*)
        ;; In a real production system, we might trigger a failsafe or restart here.
        ;; For now, we scream to the logs.
        ))))

(defun watchdog-status ()
  (format nil "Running: ~a | Broken Arrows: ~d" *watchdog-running* *broken-arrow-count*))
