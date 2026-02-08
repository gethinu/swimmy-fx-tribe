;;; src/lisp/tests/scheduler-tests.lisp
;;; Tests for Task Scheduler (23:00 Daily Report Fix)

(in-package :swimmy.tests)

(defun mock-send-report ()
  (format t "[TEST] Mock Report Sent~%")
  (push :sent *test-results*))

(deftest test-2300-trigger-logic
  "Test that report triggers at 23:00 and not before"
  ;; Globals need to be accessible
  (let* ((time-2259 (encode-universal-time 0 59 22 1 1 2026))
         (time-2300 (encode-universal-time 0 0 23 1 1 2026))
         (time-2301 (encode-universal-time 0 1 23 1 1 2026))
         (day-key (multiple-value-bind (s m h date month year)
                      (decode-universal-time time-2259)
                    (declare (ignore s m h))
                    (+ (* year 10000) (* month 100) date)))
         (swimmy.globals:*last-new-day* day-key)
         (swimmy.globals:*daily-report-sent-today* nil)
         (swimmy.main::*advisor-report-sent-today* t)
         (original-report-fn (symbol-function 'swimmy.main::send-daily-status-report)))
    
    ;; Mock the reporting function
    (setf (symbol-function 'swimmy.main::send-daily-status-report) #'mock-send-report)
    
    (unwind-protect
         (progn
           ;; Case 1: 22:59 (Should NOT trigger)
           ;; Time: 2026-01-07 22:59:00
           ;; We need a universal time.
           ;; (encode-universal-time sec min hour date month year)
           (swimmy.main:check-scheduled-tasks time-2259)
           (assert-false swimmy.globals:*daily-report-sent-today* "Should not send at 22:59")
           
           ;; Case 2: 23:00 (Should TRIGGER)
           (swimmy.main:check-scheduled-tasks time-2300)
           (assert-true swimmy.globals:*daily-report-sent-today* "Should send at 23:00")
           
           ;; Case 3: 23:01 (Should NOT trigger again)
           ;; Manually reset flag to ensure logic holds? No, logic says "if not sent".
           ;; It IS sent now.
           (setf *test-results* nil) ; Clear mock push
           (swimmy.main:check-scheduled-tasks time-2301)
           ;; Should be already true
           (assert-true swimmy.globals:*daily-report-sent-today*)
           ;; Mock should NOT have been called again (implied by unchanged flag logic check)
           )
      
      ;; Restore
      (setf (symbol-function 'swimmy.main::send-daily-status-report) original-report-fn)))
  )

(deftest test-midnight-reset-logic
  "Test that flags reset on new day"
  (let* ((time-day1 (encode-universal-time 0 0 0 1 2 2026))
         (time-day2 (encode-universal-time 0 0 0 2 2 2026))
         (day1-key (multiple-value-bind (s m h date month year)
                       (decode-universal-time time-day1)
                     (declare (ignore s m h))
                     (+ (* year 10000) (* month 100) date)))
         (day2-key (multiple-value-bind (s m h date month year)
                       (decode-universal-time time-day2)
                     (declare (ignore s m h))
                     (+ (* year 10000) (* month 100) date)))
         (swimmy.globals:*last-new-day* day1-key)
         (swimmy.globals:*daily-report-sent-today* t))
    
    ;; Trigger new day (Day 2)
    (swimmy.main:check-scheduled-tasks time-day2)
    
    (assert-equal day2-key swimmy.globals:*last-new-day* "Should update date")
    (assert-false swimmy.globals:*daily-report-sent-today* "Should reset sent flag")))

(deftest test-daily-report-no-duplicate-after-flag-reset
  "Daily report should not send twice if the sent flag is reset mid-day"
  (let* ((time-2300 (encode-universal-time 0 0 23 7 2 2026))
         (time-2318 (encode-universal-time 0 18 23 7 2 2026))
         (day-key (multiple-value-bind (s m h date month year)
                      (decode-universal-time time-2300)
                    (declare (ignore s m h))
                    (+ (* year 10000) (* month 100) date)))
         (swimmy.globals:*last-new-day* day-key)
         (swimmy.globals:*daily-report-sent-today* nil)
         (swimmy.main::*advisor-report-sent-today* t)
         (original-report-fn (symbol-function 'swimmy.main::send-daily-status-report)))
    (setf (symbol-function 'swimmy.main::send-daily-status-report) #'mock-send-report)
    (unwind-protect
         (progn
           (setf *test-results* nil)
           ;; First send at 23:00
           (swimmy.main:check-scheduled-tasks time-2300)
           (assert-equal 1 (count :sent *test-results*) "Should send once at 23:00")
           ;; Simulate restart: flag reset
           (setf swimmy.globals:*daily-report-sent-today* nil)
           ;; Same day, later time - should NOT send again
           (swimmy.main:check-scheduled-tasks time-2318)
           (assert-equal 1 (count :sent *test-results*) "Should not send twice on same day"))
      (setf (symbol-function 'swimmy.main::send-daily-status-report) original-report-fn))))

(deftest test-weekly-summary-dedup
  "Weekly summary should not generate more than once within 7 days"
  (let* ((orig-last swimmy.shell::*last-weekly-summary*)
         (orig-notify (and (fboundp 'swimmy.core:notify-discord)
                           (symbol-function 'swimmy.core:notify-discord)))
         (called 0)
         (now (get-universal-time)))
    (unwind-protect
        (progn
          (setf swimmy.shell::*last-weekly-summary* now)
          (when orig-notify
            (setf (symbol-function 'swimmy.core:notify-discord)
                  (lambda (&rest _args)
                    (declare (ignore _args))
                    (incf called))))
          (let ((result (swimmy.shell::generate-weekly-summary)))
            (assert-true (null result) "Should skip weekly summary if generated recently")
            (assert-equal now swimmy.shell::*last-weekly-summary*
                          "Should not update last weekly summary when skipping")
            (assert-equal 0 called "Should not notify when skipping")))
      (setf swimmy.shell::*last-weekly-summary* orig-last)
      (when orig-notify
        (setf (symbol-function 'swimmy.core:notify-discord) orig-notify)))))

(deftest test-evolution-report-throttle-uses-last-write
  "Evolution report should only send when last write exceeds interval"
  (let* ((orig (symbol-function 'swimmy.school::notify-evolution-report))
         (called 0)
         (now 100000)
         (interval 3600))
    (declare (special swimmy.school::*evolution-report-interval*))
    (setf swimmy.school::*evolution-report-interval* interval)
    (setf (symbol-function 'swimmy.school::notify-evolution-report)
          (lambda () (incf called)))
    (unwind-protect
        (progn
          (swimmy.school::maybe-send-evolution-report :now now :last-write now)
          (assert-equal 0 called "Should not send when fresh")
          (swimmy.school::maybe-send-evolution-report :now now :last-write (- now interval 1))
          (assert-equal 1 called "Should send when stale"))
      (setf (symbol-function 'swimmy.school::notify-evolution-report) orig))))

(deftest test-evolution-report-staleness-alert-throttles
  "Staleness alerts should fire once per cooldown window"
  (let* ((orig-alert (symbol-function 'swimmy.core:notify-discord-alert))
         (orig-emit (and (fboundp 'swimmy.core::emit-telemetry-event)
                         (symbol-function 'swimmy.core::emit-telemetry-event)))
         (called 0)
         (now 200000)
         (threshold 7200)
         (cooldown 3600))
    (declare (special swimmy.school::*evolution-report-stale-threshold*
                      swimmy.school::*evolution-report-alert-interval*
                      swimmy.school::*last-evolution-report-alert-time*))
    (setf swimmy.school::*evolution-report-stale-threshold* threshold)
    (setf swimmy.school::*evolution-report-alert-interval* cooldown)
    (setf swimmy.school::*last-evolution-report-alert-time* 0)
    (setf (symbol-function 'swimmy.core:notify-discord-alert)
          (lambda (&rest _args) (declare (ignore _args)) (incf called)))
    (when orig-emit
      (setf (symbol-function 'swimmy.core::emit-telemetry-event)
            (lambda (&rest _args) (declare (ignore _args)) t)))
    (unwind-protect
        (progn
          (swimmy.school::maybe-alert-evolution-report-staleness
           :now now :last-report now :last-heartbeat now)
          (assert-equal 0 called "Should not alert when fresh")
          (swimmy.school::maybe-alert-evolution-report-staleness
           :now now :last-report (- now threshold 10) :last-heartbeat (- now threshold 10))
          (assert-equal 1 called "Should alert when stale")
          (swimmy.school::maybe-alert-evolution-report-staleness
           :now (+ now 10) :last-report (- now threshold 10) :last-heartbeat (- now threshold 10))
          (assert-equal 1 called "Should respect cooldown"))
      (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-alert)
      (when orig-emit
        (setf (symbol-function 'swimmy.core::emit-telemetry-event) orig-emit)))))
