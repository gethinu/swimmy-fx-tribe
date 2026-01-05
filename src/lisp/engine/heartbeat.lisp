;;; ============================================================================
;;; engine/heartbeat.lisp - Mobile Monitoring (Naval's Feedback)
;;; ============================================================================
;;; "If you can't see it from your phone, it doesn't exist." - Naval
;;; Sends periodic heartbeat to Discord so owner knows system is alive.

(in-package :swimmy.engine)
;;; ============================================================================

;;; ==========================================
;;; HEARTBEAT PARAMETERS
;;; ==========================================

(defparameter *heartbeat-interval* 3600
  "Seconds between heartbeat notifications (default: 1 hour).")

(defparameter *last-heartbeat-sent* 0
  "Timestamp of last heartbeat sent to Discord.")

(defparameter *heartbeat-enabled* t
  "Enable/disable heartbeat notifications.")

(defparameter *heartbeat-webhook-url* 
  "https://discord.com/api/webhooks/1413195529680191538/OLcthUXpQr6fM32o8Vx-zlEJfgDTXfq14RPPSJdEKBJJZUUVBWJ9Hwq7ZPNFOMDkmQSW"
  "Dedicated webhook URL for heartbeat notifications.")

;;; ==========================================
;;; HEARTBEAT LOGIC
;;; ==========================================

(defun get-system-summary ()
  "Create a brief system status summary."
  (format nil "~a~%~a~%~a~%~a"
          (format nil "📊 PnL: ¥~:d" (if (boundp '*accumulated-pnl*) *accumulated-pnl* 0))
          (format nil "🏦 Treasury: ¥~:d" (if (boundp '*locked-treasury*) *locked-treasury* 0))
          (format nil "📈 Trades today: ~d" (if (boundp '*daily-trade-count*) *daily-trade-count* 0))
          (format nil "⏰ Uptime: ~d min" (floor (/ (- (get-universal-time) *last-heartbeat-sent*) 60)))))

(defun send-discord-heartbeat ()
  "Send heartbeat notification to dedicated Discord channel."
  (when *heartbeat-enabled*
    (let* ((hour (nth 2 (multiple-value-list (get-decoded-time))))
           (emoji (cond
                   ((< hour 6) "🌙")
                   ((< hour 12) "🌅")
                   ((< hour 18) "☀️")
                   (t "🌆")))
           (message (format nil "~a **Swimmy Heartbeat** ~a~%~%~a~%~%_System is running normally._"
                           emoji
                           (get-time-string-full)
                           (get-system-summary)))
           ;; Escape quotes for JSON (Naval: no cl-ppcre needed)
           (safe-message (substitute #\' #\" message)))
      (handler-case
          (progn
            ;; Use dedicated heartbeat webhook
            (uiop:run-program 
              (list "curl" "-s" "-X" "POST" 
                    "-H" "Content-Type: application/json"
                    "-d" (format nil "{\"content\": \"~a\"}" safe-message)
                    *heartbeat-webhook-url*)
              :ignore-error-status t)
            (setf *last-heartbeat-sent* (get-universal-time))
            (format t "[HEARTBEAT] 💓 Sent to Discord~%"))
        (error (e)
          (format t "[HEARTBEAT] ❌ Failed: ~a~%" e))))))

(defun get-time-string-full ()
  "Get full timestamp string."
  (multiple-value-bind (sec min hour day month year)
      (get-decoded-time)
    (declare (ignore sec))
    (format nil "~d/~2,'0d/~2,'0d ~2,'0d:~2,'0d"
            year month day hour min)))

(defun check-discord-heartbeat ()
  "Check if it's time to send a heartbeat."
  (when *heartbeat-enabled*
    (let ((elapsed (- (get-universal-time) *last-heartbeat-sent*)))
      (when (>= elapsed *heartbeat-interval*)
        (send-discord-heartbeat)))))

;;; ==========================================
;;; IMMEDIATE HEARTBEAT (For testing)
;;; ==========================================

(defun heartbeat-now ()
  "Send heartbeat immediately (manual trigger)."
  (format t "[HEARTBEAT] 💓 Sending immediate heartbeat...~%")
  (send-discord-heartbeat))

(format t "[ENGINE] heartbeat.lisp loaded - Mobile monitoring active~%")

;; Initialize timestamp (first heartbeat will be sent after interval)
(setf *last-heartbeat-sent* (get-universal-time))
