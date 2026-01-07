;;; discord-async.lisp - Async Discord Notification Queue
;;; ═══════════════════════════════════════════════════════

(in-package :swimmy.core)
;;; V41.3: Non-blocking Discord notifications
;;; Purpose: Prevent tick processing delays from HTTP calls
;;; Note: Swimmy is single-threaded, no locking needed

;;; Queue storage
(defvar *discord-queue* nil
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
  "Process queued notifications in a background thread to prevent blocking"
  (let ((now (get-universal-time)))
    ;; Only flush every N seconds
    (when (> (- now *last-discord-flush*) *discord-flush-interval*)
      (setf *last-discord-flush* now)
      ;; Grab all items and clear queue ATOMICALLY (in terms of main thread)
      (let ((items (reverse *discord-queue*)))
        (setf *discord-queue* nil)
        
        (when items
          ;; Spawn a worker thread for this batch
          ;; Note: SBCL threads are efficient enough for this frequency (every 3s)
          (sb-thread:make-thread
           (lambda ()
             (handler-case
                 (progn
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
                                       :read-timeout 10) ; Allowed to take longer in bg thread
                           (error (e) 
                             (format t "[Thread] ⚠️ Discord post failed: ~a~%" e))))))
                   (format t "[Thread] 📤 Flushed ~d items~%" (length items))
                   )
               (error (e) (format t "[Thread] Critical Error: ~a~%" e))))
           :name "Discord-Worker"))))))

(defun discord-queue-length ()
  "Get current queue length (for monitoring)"
  (length *discord-queue*))

(format t "[L] ✅ Discord Async Queue loaded (V41.3)~%")
