(in-package :swimmy.core)

(defparameter *telemetry-fail-count* 0)

(defun emit-telemetry-event (event-type &key service severity correlation-id data)
  (when (and (boundp '*telemetry-enabled*) *telemetry-enabled*)
    (let ((ok (log-telemetry event-type
                              :service service
                              :severity severity
                              :correlation-id correlation-id
                              :data data)))
      (unless ok
        (incf *telemetry-fail-count*)
        (safe-format-t "[TELEMETRY] ⚠️ emit failed for ~a~%" event-type)))))
