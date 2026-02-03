(in-package :swimmy.tests)

(deftest test-telemetry-event-schema
  (let* ((tmp "data/memory/telemetry-test.jsonl")
         (orig swimmy.core::*log-file-path*))
    (unwind-protect
        (progn
          (ensure-directories-exist tmp)
          (setf swimmy.core::*log-file-path* tmp)
          (swimmy.core::emit-telemetry-event "test.event"
            :service "school" :severity "info" :correlation-id "CID-1"
            :data (list :foo 1))
          (with-open-file (in tmp)
            (let* ((line (read-line in nil nil))
                   (obj (jsown:parse line)))
              (assert-true (jsown:val obj "schema_version"))
              (assert-true (jsown:val obj "timestamp"))
              (assert-equal "telemetry" (jsown:val obj "log_type"))
              (assert-equal "test.event" (jsown:val obj "event_type"))
              (assert-equal "school" (jsown:val obj "service"))
              (assert-equal "info" (jsown:val obj "severity"))
              (assert-equal "CID-1" (jsown:val obj "correlation_id")))))
      (setf swimmy.core::*log-file-path* orig))))
