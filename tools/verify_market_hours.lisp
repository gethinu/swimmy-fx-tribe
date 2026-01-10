
;; Simplified test script for market-open-p

(defun get-current-hour ()
  "Get current hour (JST)"
  (nth-value 2 (decode-universal-time (get-universal-time) -9)))

(defun get-current-day-of-week ()
  "Get current day of week (0=Mon, ... 6=Sun)"
  (nth-value 6 (decode-universal-time (get-universal-time) -9)))

(defun market-open-p ()
  "Check if Forex market is open (JST).
   Open: Monday 06:00 JST to Saturday 06:00 JST."
  (let ((dow (get-current-day-of-week))
        (hour (get-current-hour)))
    (format t "DEBUG: DOW=~d Hour=~d~%" dow hour)
    (cond
      ;; Sunday (6) is always Closed
      ((= dow 6) nil)
      ;; Saturday (5) is Closed after 06:00
      ((and (= dow 5) (>= hour 6)) nil)
      ;; Monday (0) is Closed before 06:00
      ((and (= dow 0) (< hour 6)) nil)
      ;; Otherwise Open
      (t t))))

(format t "Testing market-open-p with current time...~%")
(if (market-open-p)
    (format t "RESULT: OPEN (T)~%")
    (format t "RESULT: CLOSED (NIL)~%"))

;; Current anticipated result for Saturday 16:30 is CLOSED (NIL)
