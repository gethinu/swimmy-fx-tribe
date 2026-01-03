;;; discord-async.lisp - Async Discord Notification Queue
;;; ═══════════════════════════════════════════════════════
;;; V41.3: Non-blocking Discord notifications
;;; Purpose: Prevent tick processing delays from HTTP calls
;;; Note: Swimmy is single-threaded, no locking needed

;;; Queue storage
(defparameter *discord-queue* nil
  "Queue of pending Discord notifications")

(defparameter *discord-flush-interval* 3
  "Seconds between queue flushes (batch notifications)")

(defparameter *last-discord-flush* 0
  "Last time the queue was flushed")

;;; Queue management
(defun queue-discord-notification (webhook msg &key (color 3447003) (title "🐟 Swimmy"))
  "Add a notification to the async queue (non-blocking)"
  (push (list :webhook webhook
              :msg msg
              :color color
              :title title
              :timestamp (get-universal-time))
        *discord-queue*)
  ;; Return immediately - no HTTP wait
  t)

(defun flush-discord-queue ()
  "Process queued notifications (call periodically from main loop)"
  (let ((now (get-universal-time))
        (items nil))
    ;; Only flush every N seconds
    (when (> (- now *last-discord-flush*) *discord-flush-interval*)
      (setf *last-discord-flush* now)
      ;; Grab all items and clear queue
      (setf items (reverse *discord-queue*))
      (setf *discord-queue* nil)
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
  (length *discord-queue*))

(format t "[L] ✅ Discord Async Queue loaded (V41.3)~%")
