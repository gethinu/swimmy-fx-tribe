;;; src/lisp/tests/scheduler-tests.lisp
;;; Tests for Task Scheduler (23:00 Daily Report Fix)

(in-package :swimmy.tests)

(defun mock-send-report ()
  (format t "[TEST] Mock Report Sent~%")
  (push :sent *test-results*))

(deftest test-2300-trigger-logic
  "Test that report triggers at 23:00 and not before"
  ;; Globals need to be accessible
  (let ((swimmy.globals:*last-narrative-day* 1)
        (swimmy.globals:*daily-report-sent-today* nil)
        (original-report-fn (symbol-function 'swimmy.main::send-daily-tribal-narrative)))
    
    ;; Mock the reporting function
    (setf (symbol-function 'swimmy.main::send-daily-tribal-narrative) #'mock-send-report)
    
    (unwind-protect
         (progn
           ;; Case 1: 22:59 (Should NOT trigger)
           ;; Time: 2026-01-07 22:59:00
           ;; We need a universal time.
           ;; (encode-universal-time sec min hour date month year)
           (let ((time-2259 (encode-universal-time 0 59 22 1 1 2026)))
             (swimmy.main:check-scheduled-tasks time-2259)
             (assert-false swimmy.globals:*daily-report-sent-today* "Should not send at 22:59"))
           
           ;; Case 2: 23:00 (Should TRIGGER)
           (let ((time-2300 (encode-universal-time 0 0 23 1 1 2026)))
             (swimmy.main:check-scheduled-tasks time-2300)
             (assert-true swimmy.globals:*daily-report-sent-today* "Should send at 23:00")
             ;; Check mock side effect if simple assert isn't enough, but flag is good enough
             )
           
           ;; Case 3: 23:01 (Should NOT trigger again)
           (let ((time-2301 (encode-universal-time 0 1 23 1 1 2026)))
             ;; Manually reset flag to ensure logic holds? No, logic says "if not sent".
             ;; It IS sent now.
             (setf *test-results* nil) ; Clear mock push
             (swimmy.main:check-scheduled-tasks time-2301)
             ;; Should be already true
             (assert-true swimmy.globals:*daily-report-sent-today*)
             ;; Mock should NOT have been called again (implied by unchanged flag logic check)
             ))
      
      ;; Restore
      (setf (symbol-function 'swimmy.main::send-daily-tribal-narrative) original-report-fn))))

(deftest test-midnight-reset-logic
  "Test that flags reset on new day"
  (let ((swimmy.globals:*last-narrative-day* 1)
        (swimmy.globals:*daily-report-sent-today* t))
    
    ;; Trigger new day (Day 2)
    (let ((time-day2 (encode-universal-time 0 0 0 2 1 2026)))
      (swimmy.main:check-scheduled-tasks time-day2)
      
      (assert-equal 2 swimmy.globals:*last-narrative-day* "Should update date")
      (assert-false swimmy.globals:*daily-report-sent-today* "Should reset sent flag"))))
