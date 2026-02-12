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
                (lambda (&rest args) (declare (ignore args)) t))
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

(deftest test-oos-telemetry-dispatch-failed
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
           (swimmy.school:make-strategy :name "UT-OOS-FAIL" :symbol "USDJPY"))
          (assert-true (find "oos.dispatch_failed" events :key #'first :test #'string=)))
      (setf (symbol-function 'swimmy.core::emit-telemetry-event) orig-emit)
      (setf (symbol-function 'swimmy.school::init-db) orig-init)
      (setf (symbol-function 'swimmy.school::lookup-oos-request) orig-lookup)
      (setf (symbol-function 'swimmy.school::enqueue-oos-request) orig-enqueue)
      (setf (symbol-function 'swimmy.school::request-backtest) orig-request)
      (when orig-write
        (setf (symbol-function 'swimmy.school::write-oos-status-file) orig-write)))))

(deftest test-stagnant-crank-telemetry-buffer
  "Queueing stagnant C-Rank should emit buffer length + oldest age telemetry"
  (let ((events nil)
        (orig-emit (symbol-function 'swimmy.core::emit-telemetry-event)))
    (unwind-protect
        (progn
          (setf swimmy.core::*stagnant-crank-retire-buffer* nil)
          (setf swimmy.core::*stagnant-crank-retire-first-seen* 0)
          (setf (symbol-function 'swimmy.core::emit-telemetry-event)
                (lambda (event-type &key data &allow-other-keys)
                  (push (list event-type data) events)))
          (swimmy.core::queue-stagnant-crank-retire "STRAT-A" :now 100)
          (let* ((ev (find "stagnant_crank.buffer" events :key #'first :test #'string=))
                 (data (second ev)))
            (assert-true ev "Expected telemetry event")
            (assert-true (jsown:val data "buffer_len"))
            (assert-true (jsown:val data "oldest_age_seconds"))))
      (setf (symbol-function 'swimmy.core::emit-telemetry-event) orig-emit))))

(deftest test-wfv-telemetry-result-emitted-on-complete
  (let ((events nil)
        (orig-emit (symbol-function 'swimmy.core::emit-telemetry-event))
        (orig-move (symbol-function 'swimmy.persistence:move-strategy))
        (orig-notify (symbol-function 'swimmy.school::notify-discord-alert)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.core::emit-telemetry-event)
                (lambda (event-type &key data &allow-other-keys)
                  (push (list event-type data) events)))
          (setf (symbol-function 'swimmy.persistence:move-strategy)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf (symbol-function 'swimmy.school::notify-discord-alert)
                (lambda (&rest args) (declare (ignore args)) nil))
          (let ((swimmy.school::*wfv-pending-strategies* (make-hash-table :test 'equal)))
            (setf (gethash "WFV" swimmy.school::*wfv-pending-strategies*)
                  (list :is-result nil :oos-result nil
                        :strategy (swimmy.school:make-strategy :name "WFV" :generation 2)
                        :wfv-id "WFV-1" :split-ratio 0.2))
            (swimmy.school::process-wfv-result "WFV_IS" (list :sharpe 1.2))
            (swimmy.school::process-wfv-result "WFV_OOS" (list :sharpe 0.8)))
          (let* ((ev (find "wfv.result" events :key #'first :test #'string=))
                 (data (second ev)))
            (assert-true (jsown:val data "wfv_id"))
            (assert-true (jsown:val data "split_ratio"))
            (assert-true (jsown:val data "generation"))
            (assert-true (jsown:val data "required_oos"))
            (assert-true (jsown:val data "degradation"))
            (assert-true (jsown:val data "decision"))))
      (setf (symbol-function 'swimmy.core::emit-telemetry-event) orig-emit)
      (setf (symbol-function 'swimmy.persistence:move-strategy) orig-move)
      (setf (symbol-function 'swimmy.school::notify-discord-alert) orig-notify))))

(deftest test-heartbeat-emits-telemetry
  (let ((events nil)
        (orig-emit (symbol-function 'swimmy.core::emit-telemetry-event))
        (orig-make (symbol-function 'swimmy.core::make-heartbeat-message)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.core::emit-telemetry-event)
                (lambda (event-type &key data &allow-other-keys)
                  (declare (ignore data))
                  (push event-type events)))
          (setf (symbol-function 'swimmy.core::make-heartbeat-message)
                (lambda (&optional status)
                  (declare (ignore status))
                  `((type . ,swimmy.core:+MSG-HEARTBEAT+)
                    (id . "HB-1"))))
          (let ((swimmy.executor::*last-heartbeat-sent* 0)
                (swimmy.executor::*cmd-publisher* nil))
            (swimmy.executor::send-heartbeat))
          (assert-true (find "heartbeat.sent" events :test #'string=)))
      (setf (symbol-function 'swimmy.core::emit-telemetry-event) orig-emit)
      (setf (symbol-function 'swimmy.core::make-heartbeat-message) orig-make))))

(deftest test-heartbeat-throttle-allows-10s
  "Brain heartbeat should be frequent enough to avoid watchdog false positives."
  (let ((events nil)
        (orig-emit (symbol-function 'swimmy.core::emit-telemetry-event))
        (orig-make (symbol-function 'swimmy.core::make-heartbeat-message)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.core::emit-telemetry-event)
                (lambda (event-type &key data &allow-other-keys)
                  (declare (ignore data))
                  (push event-type events)))
          (setf (symbol-function 'swimmy.core::make-heartbeat-message)
                (lambda (&optional status)
                  (declare (ignore status))
                  `((type . ,swimmy.core:+MSG-HEARTBEAT+)
                    (id . "HB-10S"))))
          (let* ((now (get-universal-time))
                 (swimmy.executor::*last-heartbeat-sent* (- now 11))
                 (swimmy.executor::*cmd-publisher* nil))
            (swimmy.executor::send-heartbeat))
          (assert-true (find "heartbeat.sent" events :test #'string=)))
      (setf (symbol-function 'swimmy.core::emit-telemetry-event) orig-emit)
      (setf (symbol-function 'swimmy.core::make-heartbeat-message) orig-make))))

(deftest test-atomic-write-json
  (let ((path "data/memory/atomic-test.json"))
    (swimmy.core::atomic-write-text path "{\"ok\":true}")
    (with-open-file (in path)
      (assert-equal "{\"ok\":true}" (read-line in nil nil)))))

(deftest test-telemetry-rotation
  (let* ((path "data/memory/telemetry-rotate.jsonl")
         (orig swimmy.core::*log-file-path*)
         (orig-max swimmy.core::*telemetry-max-bytes*))
    (unwind-protect
        (progn
          (setf swimmy.core::*log-file-path* path)
          (setf swimmy.core::*telemetry-max-bytes* 10)
          (swimmy.core::log-telemetry "rotate.test"
            :service "core" :severity "info" :correlation-id "CID" :data (list :x 1))
          (assert-true (probe-file (format nil "~a.1" path))))
      (setf swimmy.core::*log-file-path* orig)
      (setf swimmy.core::*telemetry-max-bytes* orig-max))))

(deftest test-telemetry-fallback-when-primary-unwritable
  (let* ((primary "/proc/swimmy-telemetry-test.jsonl")
         (fallback "data/memory/telemetry-fallback-test.jsonl")
         (orig-log swimmy.core::*log-file-path*)
         (orig-fallback (and (boundp 'swimmy.core::*telemetry-fallback-log-path*)
                             swimmy.core::*telemetry-fallback-log-path*)))
    (unwind-protect
        (progn
          (ignore-errors (delete-file fallback))
          (setf swimmy.core::*log-file-path* primary)
          (setf swimmy.core::*telemetry-fallback-log-path* fallback)
          (assert-true
           (swimmy.core::log-telemetry "fallback.test"
             :service "core" :severity "warn" :correlation-id "CID-FB"
             :data (list :x 1))
           "Expected telemetry write to succeed via fallback path")
          (assert-true (probe-file fallback)
                       "Expected fallback telemetry file to be created"))
      (setf swimmy.core::*log-file-path* orig-log)
      (setf swimmy.core::*telemetry-fallback-log-path* orig-fallback)
      (ignore-errors (delete-file fallback)))))
