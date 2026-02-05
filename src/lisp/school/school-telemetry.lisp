;;; school-telemetry.lisp - Swimmy System Telemetry & Observability
;;; Implements Gene Kim's "Feedback Loops" (DevOps Handbook)
;;; Provides visibility into internal system state (Heap, Counts, etc.)

(in-package :swimmy.school)

(defparameter *telemetry-file* "data/system_metrics.sexp")
(defparameter *notifier-metrics-file* "data/notifier_metrics.sexp")
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

(defun read-notifier-metrics ()
  "Best-effort read of Notifier health metrics (alist) from *notifier-metrics-file*."
  (handler-case
      (swimmy.core:read-sexp-file *notifier-metrics-file* :package :swimmy.school)
    (error (_e)
      nil)))

(defun save-telemetry-sexp (metrics)
  "Writes metrics to S-expression file for external dashboards."
  (let* ((notifier (read-notifier-metrics))
         (extra nil)
         (payload `((schema_version . 1)
                    (timestamp . ,(get-universal-time))
                    (heap_used_bytes . ,(getf metrics :heap))
                    (heap_used_mb . ,(/ (getf metrics :heap) 1024.0 1024.0))
                    (strategy_count . ,(getf metrics :strategy-count))
                    (uptime_seconds . ,(- (get-universal-time) swimmy.globals::*system-start-time*)))))
    (when notifier
      (let ((q (assoc 'queue_len notifier))
            (d (assoc 'drops notifier))
            (e (assoc 'errors notifier)))
        (when q (push `(notifier_queue_len . ,(cdr q)) extra))
        (when d (push `(notifier_drops . ,(cdr d)) extra))
        (when e (push `(notifier_errors . ,(cdr e)) extra))))
    (when extra
      (setf payload (append payload (nreverse extra))))
    (swimmy.core:write-sexp-atomic *telemetry-file* payload)))


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
    
    ;; 2. S-expression Dump (Observability)
    (save-telemetry-sexp metrics)
    (swimmy.core::emit-telemetry-event "metrics.snapshot"
      :service "school"
      :severity "info"
      :correlation-id (format nil "~a" (get-universal-time))
      :data (jsown:new-js
              ("heap_used_bytes" heap)
              ("heap_used_mb" (/ heap 1024.0 1024.0))
              ("strategy_count" strat-count)
              ("uptime_seconds" (- (get-universal-time) swimmy.globals::*system-start-time*))))
    
    (values metrics alert)))
