;;; discord-async.lisp - Async Discord Notification Queue
;;; ═══════════════════════════════════════════════════════
;;; V41.3: Non-blocking Discord notifications
;;; Purpose: Prevent tick processing delays from HTTP calls

;;; Queue storage
(defparameter *discord-queue* nil
  "Queue of pending Discord notifications")

(defparameter *discord-queue-lock* (bt:make-lock "discord-queue")
  "Lock for thread-safe queue access")

(defparameter *discord-flush-interval* 3
  "Seconds between queue flushes (batch notifications)")

(defparameter *last-discord-flush* 0
  "Last time the queue was flushed")

;;; Queue management
(defun queue-discord-notification (webhook msg &key (color 3447003) (title "🐟 Swimmy"))
  "Add a notification to the async queue (non-blocking)"
  (bt:with-lock-held (*discord-queue-lock*)
    (push (list :webhook webhook
                :msg msg
                :color color
                :title title
                :timestamp (get-universal-time))
          *discord-queue*))
  ;; Return immediately - no HTTP wait
  t)

(defun flush-discord-queue ()
  "Process queued notifications (call periodically from main loop)"
  (let ((now (get-universal-time))
        (items nil))
    ;; Only flush every N seconds
    (when (> (- now *last-discord-flush*) *discord-flush-interval*)
      (setf *last-discord-flush* now)
      ;; Atomically grab all items
      (bt:with-lock-held (*discord-queue-lock*)
        (setf items (reverse *discord-queue*))
        (setf *discord-queue* nil))
      ;; Send each notification
      (dolist (item items)
        (let ((webhook (getf item :webhook))
              (msg (getf item :msg))
              (color (getf item :color))
              (title (getf item :title)))
          (when (and webhook msg (not (equal msg "NIL")))
            (handler-case
                (dex:post webhook
                          :content (jsown:to-json 
                                    (jsown:new-js 
                                     ("embeds" (list (jsown:new-js 
                                                      ("title" title)
                                                      ("description" (format nil "~a" msg))
                                                      ("color" color))))))
                          :headers '(("Content-Type" . "application/json"))
                          :read-timeout 3)
              (error (e) 
                (format t "[L] ⚠️ Discord post failed: ~a~%" e))))))
      ;; Log flush result
      (when items
        (format t "[L] 📤 Flushed ~d Discord notifications~%" (length items))))))

(defun discord-queue-length ()
  "Get current queue length (for monitoring)"
  (bt:with-lock-held (*discord-queue-lock*)
    (length *discord-queue*)))

;;; Async wrapper functions - drop-in replacements
(defun async-notify-discord (msg &key (color 3447003))
  "Async version of notify-discord"
  (when (and *discord-webhook-url* msg)
    (queue-discord-notification *discord-webhook-url* msg 
                                :color color 
                                :title "🐟 Apex")))

(defun async-notify-discord-symbol (symbol msg &key (color 3447003))
  "Async version of notify-discord-symbol"
  (let ((webhook (or (gethash symbol *symbol-webhooks*) *discord-webhook-url*)))
    (when (and webhook msg)
      (queue-discord-notification webhook msg 
                                  :color color 
                                  :title (format nil "🐟 ~a" symbol)))))

(defun async-notify-discord-alert (msg &key (color 15158332))
  "Async version of notify-discord-alert"
  (when (and *alerts-webhook-url* msg)
    (queue-discord-notification *alerts-webhook-url* msg 
                                :color color 
                                :title "🚨 ALERT")))

(defun async-notify-discord-status (msg &key (color 3066993))
  "Async version of notify-discord-status"
  (when (and *status-webhook-url* msg)
    (queue-discord-notification *status-webhook-url* msg 
                                :color color 
                                :title "📊 Status")))

(defun async-notify-discord-backtest (msg &key (color 3447003))
  "Async version of notify-discord-backtest"
  (when (and *backtest-webhook-url* msg)
    (queue-discord-notification *backtest-webhook-url* msg 
                                :color color 
                                :title "📈 Backtest")))

(defun async-notify-discord-emergency (msg)
  "Async version but sends immediately for emergencies"
  (let ((webhook (or *discord-emergency-webhook* *discord-webhook-url*)))
    (when webhook
      ;; Emergency: send immediately, don't queue
      (handler-case
          (dex:post webhook
                    :content (jsown:to-json 
                              (jsown:new-js 
                               ("embeds" (list (jsown:new-js 
                                                ("title" "🚨 EMERGENCY 🚨")
                                                ("description" (format nil "~a" msg))
                                                ("color" 15158332))))))
                    :headers '(("Content-Type" . "application/json"))
                    :read-timeout 3)
        (error (e) nil)))))

(defun async-notify-discord-daily (msg &key (color 3447003))
  "Async version of notify-discord-daily"
  (let ((webhook (or *discord-daily-webhook* *discord-webhook-url*)))
    (when (and webhook msg)
      (queue-discord-notification webhook msg 
                                  :color color 
                                  :title "📊 Daily Report"))))

(defun async-notify-discord-weekly (msg)
  "Async version of notify-discord-weekly"
  (let ((webhook (or *discord-weekly-webhook* *discord-webhook-url*)))
    (when (and webhook msg)
      (queue-discord-notification webhook msg 
                                  :color 10181046 
                                  :title "📈 Weekly Summary"))))

(format t "[L] ✅ Discord Async Queue loaded (V41.3)~%")
