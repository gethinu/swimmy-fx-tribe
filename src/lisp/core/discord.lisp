(in-package :swimmy.core)

;; core/discord.lisp - Async Discord Notification Functions
;; V42.0: FULL SEPARATION (Phase 2 - Notifier Service)
;; Replaces Dexador/HTTP with ZMQ PUSH to tools/notifier.py (Port 5562)

;;; ==========================================
;;; MIGRATION NOTE
;;; ==========================================
;;; OLD: Lisp -> Dexador -> Discord API (Sync/Blocking or Threaded queue)
;;; NEW: Lisp -> ZMQ PUSH -> Notifier.py -> Discord API (Truly Async)

(defparameter *notifier-endpoint* "tcp://localhost:5562")
(defparameter *notifier-socket* nil)
(defparameter *notifier-context* nil)

(defun ensure-notifier-connection ()
  "Ensure ZMQ connection to Notifier is active"
  (unless *notifier-context*
    (setf *notifier-context* (pzmq:ctx-new)))
  (unless *notifier-socket*
    (setf *notifier-socket* (pzmq:socket *notifier-context* :push))
    (pzmq:connect *notifier-socket* *notifier-endpoint*)
    (format t "[DISCORD] Connected to Notifier service at ~a~%" *notifier-endpoint*)))

(defun queue-discord-notification (webhook msg &key (color 3447003) (title "Swimmy Notification"))
  "Send notification to Notifier service via ZMQ"
  (when (and webhook msg (not (equal msg "NIL")))
    (ensure-notifier-connection)
    (handler-case
        (let ((payload (jsown:new-js 
                        ("webhook" webhook)
                        ("data" (jsown:new-js
                                  ("embeds" (list (jsown:new-js
                                                    ("title" title)
                                                    ("description" (format nil "~a" msg))
                                                    ("color" color)))))))))
          (pzmq:send *notifier-socket* (jsown:to-json payload))
          t)
      (error (e)
        (format t "[DISCORD] Failed to send ZMQ msg: ~a~%" e)
        nil))))

(defun queue-raw-discord-message (webhook payload)
  "Send raw JSON payload to Notifier"
  (when (and webhook payload)
    (ensure-notifier-connection)
    (handler-case
        (let ((msg (jsown:new-js 
                     ("webhook" webhook)
                     ("data" payload))))
          (pzmq:send *notifier-socket* (jsown:to-json msg))
          t)
      (error (e)
        (format t "[DISCORD] Failed to send raw ZMQ msg: ~a~%" e)
        nil))))

;;; ==========================================
;;; PUBLIC API (Backward Compatible)
;;; ==========================================

(defun notify-discord (msg &key (color 3447003))
  "Main notification"
  (when *discord-webhook-url*
    (queue-discord-notification *discord-webhook-url* msg :color color :title "🐟 Apex")))

(defun notify-discord-symbol (symbol msg &key (color 3447003))
  "Symbol-specific channel"
  (let* ((looked-up (gethash symbol *symbol-webhooks*))
         (webhook (or looked-up *discord-webhook-url*)))
    (when webhook
      (queue-discord-notification webhook msg :color color :title (format nil "🐟 ~a" symbol)))))

(defun notify-discord-alert (msg &key (color 15158332))
  "Alerts channel"
  (when *alerts-webhook-url*
    (queue-discord-notification *alerts-webhook-url* msg :color color :title "🚨 ALERT")))

(defun notify-discord-status (msg &key (color 3066993))
  "Status channel"
  (when *status-webhook-url*
    (queue-discord-notification *status-webhook-url* msg :color color :title "📊 Status")))

(defun notify-discord-daily (msg &key (color 3447003))
  "Daily report channel"
  (let ((webhook (or *discord-daily-webhook* *discord-webhook-url*)))
    (when webhook
      (queue-discord-notification webhook msg :color color :title "📊 Daily Report"))))

(defun notify-discord-weekly (msg)
  "Weekly summary channel"
  (let ((webhook (or *discord-weekly-webhook* *discord-webhook-url*)))
    (when webhook
      (queue-discord-notification webhook msg :color 10181046 :title "📈 Weekly Summary"))))

(defun notify-discord-recruit (msg &key (color 3066993))
  "Recruitment channel"
  (let ((webhook (or *discord-recruit-webhook* *discord-webhook-url*)))
    (when webhook
      (queue-discord-notification webhook msg :color color :title "🐟 Apex"))))

(defun notify-discord-emergency (msg)
  "EMERGENCY - Now also async via Notifier (since Notifier is reliable)"
  (let ((webhook (or *discord-emergency-url* *discord-webhook-url*)))
    (when webhook
      (queue-discord-notification webhook msg :color 15158332 :title "🚨 EMERGENCY 🚨"))))

(defun notify-apex (msg &key (color 3066993))
  "System status to Apex channel (Online, Recovery, Critical alerts)"
  (when *apex-webhook-url*
    (queue-discord-notification *apex-webhook-url* msg :color color :title "🐟 Apex")))

(defun notify-discord-backtest (msg &key (color 3447003))
  "Backtest results"
  (when *backtest-webhook-url*
     (queue-discord-notification *backtest-webhook-url* msg :color color :title "📊 Backtest Result")))

;;; ==========================================
;;; UTILITIES
;;; ==========================================

(defun flush-discord-queue ()
  "No-op in ZMQ architecture (Notifier handles queue)"
  nil)

(defun get-jst-timestamp ()
  "Return current JST time as MM/DD HH:MM string"
  (multiple-value-bind (sec min hour date month) (decode-universal-time (get-universal-time))
    (declare (ignore sec))
    (format nil "~2,'0d/~2,'0d ~2,'0d:~2,'0d JST" month date hour min)))

;;; ==========================================
;;; LOGIC FUNCTIONS
;;; ==========================================

(defun categorize-strategy-name (name)
  "Categorize strategy based on name keywords"
  (let ((n (string-upcase name)))
    (cond
      ((or (search "RSI" n) (search "STOCH" n) (search "REVERS" n) (search "BOUNCE" n) (search "DIP" n) (search "FLIP" n))
       :reversion)
      ((or (search "BREAK" n) (search "ATR" n) (search "VOL" n) (search "SQUEEZE" n))
       :breakout)
      ((or (search "SCALP" n) (search "1M" n) (search "SECONDS" n))
       :scalp)
      (t :trend))))

(defun notify-backtest-summary ()
  "Compile and send categorized backtest summary to Discord"
  (let ((results (copy-list *backtest-results-buffer*))
        (categories '(:trend :reversion :breakout :scalp))
        (summary-msg (format nil "📊 **Backtest Summary (V7.0/Async)**~%")))
    
    (dolist (cat categories)
      (let* ((cat-results (remove-if-not 
                            (lambda (res) (eq (categorize-strategy-name (car res)) cat))
                            results))
             (total (length cat-results))
             (passed (count-if (lambda (res) (> (getf (cdr res) :sharpe) 0)) cat-results))
             (avg-sharpe (if (> total 0)
                             (/ (loop for r in cat-results sum (getf (cdr r) :sharpe)) total)
                             0.0))
             (sorted (sort (copy-list cat-results) #'> :key (lambda (x) (getf (cdr x) :sharpe)))))
        
        (when (> total 0)
          (setf summary-msg (concatenate 'string summary-msg 
                                         (format nil "~%**~a** (Total: ~d | Pass: ~d | Avg Sharpe: ~,2f)~%" 
                                                 cat total passed avg-sharpe)))
          
          (setf summary-msg (concatenate 'string summary-msg (format nil "  Top 5:~%")))
          (loop for i from 0 below (min 5 (length sorted))
                for res = (nth i sorted)
                do (setf summary-msg (concatenate 'string summary-msg 
                                                  (format nil "    ~d. ~a (S: ~,2f)~%" (1+ i) (car res) (getf (cdr res) :sharpe)))))
          
          (when (> total 5)
            (setf summary-msg (concatenate 'string summary-msg (format nil "  Worst 5:~%")))
            (let ((worst (reverse sorted)))
              (loop for i from 0 below (min 5 (length worst))
                    for res = (nth i worst)
                    do (setf summary-msg (concatenate 'string summary-msg 
                                                      (format nil "    ~d. ~a (S: ~,2f)~%" (1+ i) (car res) (getf (cdr res) :sharpe))))))))))
    
    (setf summary-msg (concatenate 'string summary-msg (format nil "~%🔍 Async Delivery via Notifier Service.")))
    (notify-discord-backtest summary-msg :color 5763719)
    
    ;; Clear buffer
    (setf *backtest-results-buffer* nil)
    (setf *expected-backtest-count* 0)))

(defun send-periodic-status-report (symbol bid)
  "Send a periodic status report to Discord if interval continues"
  (let* ((now (get-universal-time))
         (last-time (gethash symbol *last-status-notification-time* 0)))
    (when (> (- now last-time) *status-notification-interval*)
      (let ((tribe-dir (if (boundp '*tribe-direction*) *tribe-direction* "N/A"))
            (tribe-con (if (and (boundp '*tribe-consensus*) *tribe-consensus*) *tribe-consensus* 0.0))
            (swarm-con (if (and (boundp '*last-swarm-consensus*) *last-swarm-consensus*) *last-swarm-consensus* 0.0))
            (pred (if (boundp '*last-prediction*) *last-prediction* "N/A"))
            (conf (if (and (boundp '*last-confidence*) *last-confidence*) *last-confidence* 0.0))
            (danger (if (boundp '*danger-level*) *danger-level* 0))
            (active-warriors (if (boundp '*warrior-allocation*) 
                                 (hash-table-count *warrior-allocation*) 0)))
        (notify-discord-symbol symbol 
          (format nil "🕒 STATUS REPORT~%Price: ~,3f~%~%🧠 AI: ~a (~,1f%)~%🏛️ Tribes: ~a (~,0f%)~%🐟 Swarm: ~,0f%~%~%⚔️ Warriors: ~d~%⚠️ Danger: Lv~d"
                  bid pred (* 100 conf) tribe-dir (* 100 tribe-con) (* 100 swarm-con) active-warriors danger)
          :color 10070709) ; Dark Grey for status
        (setf (gethash symbol *last-status-notification-time*) now)))))

(format t "[L] 💬 core/discord.lisp loaded - V42.0 (Phase 2 Notifier)~%")
