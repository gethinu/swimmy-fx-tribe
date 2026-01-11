;;; school-telemetry.lisp - Swimmy System Telemetry & Observability
;;; Implements Gene Kim's "Feedback Loops" (DevOps Handbook)
;;; Provides visibility into internal system state (Heap, Counts, etc.)

(in-package :swimmy.school)

(defparameter *telemetry-file* "data/system_metrics.json")
(defparameter *memory-warning-threshold* (* 1024 1024 1024)) ; 1GB in bytes
(defparameter *last-telemetry-time* 0)

(defun get-heap-usage ()
  "Returns current dynamic space usage in bytes (SBCL specific)"
  #+sbcl (sb-kernel:dynamic-usage)
  #-sbcl 0)

(defun get-strategy-stats ()
  "Returns alist of strategy counts (Total, Active, Pending)"
  (list :total (length *strategy-knowledge-base*)
        :active (length *active-team*)
        :pending 0)) ; Pending is managed by Python service, Lisp considers them external for now

(defun save-telemetry-json (metrics)
  "Writes metrics to JSON file for external dashboards"
  (with-open-file (stream *telemetry-file* 
                          :direction :output 
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "{~%")
    (format stream "  \"timestamp\": ~d,~%" (get-universal-time))
    (format stream "  \"heap_used_bytes\": ~d,~%" (getf metrics :heap))
    (format stream "  \"heap_used_mb\": ~,2f,~%" (/ (getf metrics :heap) 1024.0 1024.0))
    (format stream "  \"strategy_count\": ~d,~%" (getf metrics :strategy-count))
    (format stream "  \"uptime_seconds\": ~d~%" (- (get-universal-time) swimmy.globals::*system-start-time*))
    (format stream "}~%")))


(defun check-memory-health (heap-bytes)
  "Checks if memory usage is critical. Returns alert string or NIL."
  (when (> heap-bytes *memory-warning-threshold*)
    (format t "[TELEMETRY] 🚨 High Memory Usage: ~,2f MB~%" (/ heap-bytes 1024.0 1024.0))
    (format nil "⚠️ **Memory Warning**~%Heap Usage: ~,2f MB~%Threshold: 1024 MB" 
             (/ heap-bytes 1024.0 1024.0))))

(defun collect-system-metrics ()
  "Collects, logs, and saves system telemetry. Returns (values metrics alert-message)."
  (format t "~%[TELEMETRY] 📡 Collecting System Metrics...~%")
  (let* ((heap (get-heap-usage))
         (strat-count (length *strategy-knowledge-base*))
         (metrics (list :heap heap :strategy-count strat-count))
         (alert (check-memory-health heap)))
    
    ;; 1. Console Log
    (format t "[TELEMETRY] Heap: ~,2f MB | Strategies: ~d~%" 
            (/ heap 1024.0 1024.0) strat-count)
    
    ;; 2. JSON Dump (Observability)
    (save-telemetry-json metrics)
    
    (values metrics alert)))
