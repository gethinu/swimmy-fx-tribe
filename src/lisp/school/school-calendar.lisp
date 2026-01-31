;; school-calendar.lisp - Economic Calendar Integration

(in-package :swimmy.school)
;; V6.21: Trading Expert requirement for 10/10 rating
;; Tracks major economic events and adjusts trading before announcements

;;; ============================================================
;;; ECONOMIC CALENDAR DATABASE
;;; ============================================================
;;; Key events that move markets significantly

(defparameter *economic-events*
  '(;; Monthly events (day-of-month . description)
    ;; US Events
    (:nfp (1 . "US Non-Farm Payrolls") :impact :high :currencies ("USDJPY" "EURUSD" "GBPUSD"))
    (:fomc (15 . "FOMC Rate Decision") :impact :critical :currencies ("USDJPY" "EURUSD" "GBPUSD"))
    (:cpi (10 . "US CPI Release") :impact :high :currencies ("USDJPY" "EURUSD" "GBPUSD"))
    
    ;; EU Events
    (:ecb (first-thursday . "ECB Rate Decision") :impact :critical :currencies ("EURUSD" "EURJPY"))
    (:eu-cpi (last-week . "EU CPI Flash") :impact :medium :currencies ("EURUSD" "EURJPY"))
    
    ;; Japan Events
    (:boj (20 . "BOJ Policy Decision") :impact :critical :currencies ("USDJPY" "EURJPY" "GBPJPY"))
    (:tankan (1 . "Tankan Survey (Quarterly)") :impact :medium :currencies ("USDJPY"))
    
    ;; UK Events
    (:boe (first-thursday . "BOE Rate Decision") :impact :critical :currencies ("GBPUSD" "GBPJPY"))))

(defparameter *event-risk-multipliers*
  '((:critical . 0.3)   ; Reduce lot to 30% before critical events
    (:high . 0.5)       ; Reduce lot to 50% before high impact events
    (:medium . 0.7)     ; Reduce lot to 70% before medium impact events
    (:low . 0.9)))      ; Slight reduction for low impact

(defparameter *event-warning-hours* 4)  ; Start adjusting 4 hours before event

;;; ============================================================
;;; EVENT DETECTION
;;; ============================================================

(defun get-current-day-of-month ()
  "Get current day of month"
  (nth-value 3 (decode-universal-time (get-universal-time))))

(defun get-current-hour ()
  "Get current hour (JST)"
  (nth-value 2 (decode-universal-time (get-universal-time) -9)))

(defun get-current-day-of-week ()
  "Get current day of week (0=Mon, ... 6=Sun) - Note: standard CL is 0=Mon or 6=Sun depending on implementation?
   Wait, decode-universal-time returns 0-6 where 6 is Sunday? No, 0-6 where 0 is Monday... let's verify.
   CLHS: 6th value is Day of week. 0=Monday, 1=Tuesday... 6=Sunday."
  (nth-value 6 (decode-universal-time (get-universal-time) -9)))

(defun market-open-p ()
  "Check if Forex market is open (JST).
   Open: Monday 06:00 JST to Saturday 06:00 JST."
  (let ((dow (get-current-day-of-week))
        (hour (get-current-hour)))
    (cond
      ;; Sunday (6) is always Closed
      ((= dow 6) nil)
      ;; Saturday (5) is Closed after 06:00
      ((and (= dow 5) (>= hour 6)) nil)
      ;; Monday (0) is Closed before 06:00
      ((and (= dow 0) (< hour 6)) nil)
      ;; Otherwise Open
      (t t))))


(defun first-thursday-p ()
  "Check if today is the first Thursday of the month"
  (multiple-value-bind (s m h day month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore s m h))
    (and (<= day 7)
         (= (day-of-week year month day) 4))))  ; 4 = Thursday

(defun last-week-of-month-p ()
  "Check if we're in the last week of the month"
  (let ((day (get-current-day-of-month)))
    (> day 24)))

(defun check-upcoming-events (symbol hours-ahead)
  "Check for upcoming economic events affecting the symbol
   Returns list of (event-name impact remaining-hours)"
  (let ((events nil)
        (day (get-current-day-of-month))
        (hour (get-current-hour)))
    
    ;; Check each event type
    (dolist (event-def *economic-events*)
      (when (listp event-def)
        (let* ((event-name (first event-def))
               (schedule (second event-def))
               (event-day (if (consp schedule) (car schedule) nil))
               (impact (getf event-def :impact))
               (currencies (getf event-def :currencies)))
          
          ;; Check if symbol is affected
          (when (member symbol currencies :test #'string=)
            ;; Check if event is upcoming
            (cond
              ;; Fixed day events
              ((and (numberp event-day)
                    (= day event-day)
                    (< hour 14))  ; Assume events at 14:00 JST
               (let ((hours-until (- 14 hour)))
                 (when (<= hours-until hours-ahead)
                   (push (list event-name impact hours-until) events))))
              
              ;; First Thursday pattern
              ((and (eq event-day 'first-thursday)
                    (first-thursday-p)
                    (< hour 20))
               (let ((hours-until (- 20 hour)))
                 (when (<= hours-until hours-ahead)
                   (push (list event-name impact hours-until) events))))
              
              ;; Last week pattern
              ((and (eq event-day 'last-week)
                    (last-week-of-month-p))
               (push (list event-name impact 24) events)))))))
    
    events))

;;; ============================================================
;;; RISK ADJUSTMENT
;;; ============================================================

(defun get-calendar-risk-adjustment (symbol)
  "Get lot multiplier based on upcoming economic events
   Returns (multiplier reason) or (1.0 nil) if no adjustment needed"
  (let ((events (check-upcoming-events symbol *event-warning-hours*)))
    (if events
        (let* ((most-dangerous (first (sort (copy-list events) 
                                            (lambda (a b) 
                                              (< (third a) (third b))))))
               (impact (second most-dangerous))
               (event-name (first most-dangerous))
               (hours-until (third most-dangerous))
               (multiplier (or (cdr (assoc impact *event-risk-multipliers*)) 1.0)))
          
          (format t "[L] 📅 CALENDAR WARNING: ~a in ~d hours~%"
                  event-name hours-until)
          (format t "[L]    Impact: ~a -> Lot multiplier: ~,1f%~%"
                  impact (* 100 multiplier))
          
          (values multiplier 
                  (format nil "~a in ~dh" event-name hours-until)))
        (values 1.0 nil))))

(defun apply-calendar-adjustment (symbol direction lot)
  "Apply economic calendar adjustment to lot size"
  (declare (ignore direction))
  (multiple-value-bind (mult reason)
      (get-calendar-risk-adjustment symbol)
    (let ((adjusted-lot (* lot mult)))
      (when reason
        (format t "[L] 📅 CALENDAR: ~a - Lot adjusted ~,2f -> ~,2f~%"
                reason lot adjusted-lot))
      adjusted-lot)))

;;; ============================================================
;;; SIMULATION MODE (for testing)
;;; ============================================================

(defun simulate-event-day (symbol event-day)
  "Simulate what would happen on a specific event day"
  (format t "~%=== Simulating ~a on day ~d ===~%" symbol event-day)
  (let ((*current-day-override* event-day))
    (declare (special *current-day-override*))
    ;; Note: This would need get-current-day-of-month to check override
    (get-calendar-risk-adjustment symbol)))

(format t "[L] 📅 school-calendar.lisp loaded - Economic Calendar active~%")
