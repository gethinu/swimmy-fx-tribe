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

(deftest test-oos-telemetry-requested
  (let ((events nil)
        (orig-emit (symbol-function 'swimmy.core::emit-telemetry-event))
        (orig-init (symbol-function 'swimmy.school::init-db))
        (orig-lookup (symbol-function 'swimmy.school::lookup-oos-request))
        (orig-enqueue (symbol-function 'swimmy.school::enqueue-oos-request))
        (orig-request (symbol-function 'swimmy.school::request-backtest))
        (orig-write (and (fboundp 'swimmy.school::write-oos-status-file)
                         (symbol-function 'swimmy.school::write-oos-status-file))))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.core::emit-telemetry-event)
                (lambda (event-type &key data &allow-other-keys)
                  (push (list event-type data) events)))
          (setf (symbol-function 'swimmy.school::init-db)
                (lambda () nil))
          (setf (symbol-function 'swimmy.school::lookup-oos-request)
                (lambda (name) (declare (ignore name)) (values nil nil nil)))
          (setf (symbol-function 'swimmy.school::enqueue-oos-request)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::request-backtest)
                (lambda (&rest args) (declare (ignore args)) nil))
          (when orig-write
            (setf (symbol-function 'swimmy.school::write-oos-status-file)
                  (lambda (&rest args) (declare (ignore args)) nil)))
          (swimmy.school::maybe-request-oos-backtest
           (swimmy.school:make-strategy :name "UT-OOS" :symbol "USDJPY"))
          (assert-true (find "oos.requested" events :key #'first :test #'string=)))
      (setf (symbol-function 'swimmy.core::emit-telemetry-event) orig-emit)
      (setf (symbol-function 'swimmy.school::init-db) orig-init)
      (setf (symbol-function 'swimmy.school::lookup-oos-request) orig-lookup)
      (setf (symbol-function 'swimmy.school::enqueue-oos-request) orig-enqueue)
      (setf (symbol-function 'swimmy.school::request-backtest) orig-request)
      (when orig-write
        (setf (symbol-function 'swimmy.school::write-oos-status-file) orig-write)))))
