;;; integration-tests.lisp - Integration Tests for Naval Modularization
;;; Tests Brain <-> Backtest Service <-> Guardian roundtrip

(in-package :swimmy.tests)

(defparameter *integration-test-passed* 0)
(defparameter *integration-test-failed* 0)

;;; ==========================================
;;; BACKTEST SERVICE INTEGRATION
;;; ==========================================

(deftest test-backtest-standalone ()
  "Test: Guardian --backtest-only mode via subprocess"
  (let* ((strategy-json "{\"name\":\"INTEGRATION-TEST\",\"sma_short\":10,\"sma_long\":50,\"sl\":0.3,\"tp\":0.6,\"volume\":0.01}")
         (candles-json "[{\"t\":1,\"o\":150.0,\"h\":150.5,\"l\":149.5,\"c\":150.2},{\"t\":2,\"o\":150.2,\"h\":150.8,\"l\":150.0,\"c\":150.6}]")
         (input (format nil "{\"strategy\":~a,\"candles\":~a}" strategy-json candles-json))
         (guardian-path (merge-pathnames "guardian/target/release/guardian" (uiop:getcwd))))
    ;; Run guardian --backtest-only with input
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program (list (namestring guardian-path) "--backtest-only")
                          :input (make-string-input-stream input)
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (declare (ignore error-output))
      (if (zerop exit-code)
          (progn
            (let* ((result (jsown:parse output))
                   (result-type (jsown:val result "type")))
              (if (string= result-type "BACKTEST_RESULT")
                  (progn
                    (format t "~%✅ test-backtest-standalone PASSED~%")
                    (format t "   Result: ~a~%" output)
                    (incf *integration-test-passed*))
                  (progn
                    (format t "~%❌ test-backtest-standalone FAILED: Wrong type ~a~%" result-type)
                    (incf *integration-test-failed*)))))
          (progn
            (format t "~%❌ test-backtest-standalone FAILED: Exit code ~a~%" exit-code)
            (incf *integration-test-failed*))))))

(deftest test-discord-notification-queued ()
  "Test: Discord notification can be queued (ZMQ to notifier.py)"
  (let ((*discord-webhook-url* "https://discord.com/api/webhooks/dummy"))
    (handler-case
        (let ((result (notify-discord "[INTEGRATION-TEST] Testing notification queue" :color 3066993)))
          (if result
              (progn
                (format t "~%✅ test-discord-notification-queued PASSED~%")
                (incf *integration-test-passed*))
              (progn
                (format t "~%❌ test-discord-notification-queued FAILED: nil result~%")
                (incf *integration-test-failed*))))
      (error (e)
        (format t "~%❌ test-discord-notification-queued FAILED: ~a~%" e)
        (incf *integration-test-failed*)))))

(deftest test-backtest-service-health ()
  "Test: Backtest Service responds to health check (if running)"
  ;; This test is optional - service may not be running
  (format t "~%⏭️ test-backtest-service-health SKIPPED (manual service start required)~%")
  t)

;;; ==========================================
;;; RUN ALL INTEGRATION TESTS
;;; ==========================================

(defun run-integration-tests ()
  "Run all integration tests"
  (format t "~%═══════════════════════════════════════════════════~%")
  (format t "🔗 Running Integration Tests (Naval Modularization)~%")
  (format t "═══════════════════════════════════════════════════~%")
  
  (setf *integration-test-passed* 0)
  (setf *integration-test-failed* 0)
  
  (test-backtest-standalone)
  (test-discord-notification-queued)
  (test-backtest-service-health)
  
  (format t "~%═══════════════════════════════════════════════════~%")
  (format t "📊 Integration Test Results: ~d passed, ~d failed~%" 
          *integration-test-passed* *integration-test-failed*)
  (format t "═══════════════════════════════════════════════════~%")
  
  (zerop *integration-test-failed*))

(format t "[TESTS] Integration tests loaded (Naval Modularization)~%")
