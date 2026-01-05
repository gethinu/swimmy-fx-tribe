(in-package :swimmy.core)

;; core/discord.lisp - Discord Notification Functions
;; V41.4: Extracted from brain.lisp (Naval's Strangler Fig)
;; Dependencies: core/config.lisp (for variables), discord-async.lisp (for queue)

;;; ==========================================
;;; NOTIFICATION FUNCTIONS (Async via discord-async.lisp)
;;; ==========================================

(defun notify-discord (msg &key (color 3447003))
  "Main notification - uses async queue"
  (when (and *discord-webhook-url* msg (not (equal msg "NIL")))
    (queue-discord-notification *discord-webhook-url* msg :color color :title "🐟 Apex")))

(defun notify-discord-symbol (symbol msg &key (color 3447003))
  "Symbol-specific channel - async"
  (let* ((looked-up (gethash symbol *symbol-webhooks*))
         (webhook (or looked-up *discord-webhook-url*)))
    (when (and webhook msg (not (equal msg "NIL")))
      (queue-discord-notification webhook msg :color color :title (format nil "🐟 ~a" symbol)))))

(defun notify-discord-alert (msg &key (color 15158332))
  "Alerts channel - async"
  (when (and *alerts-webhook-url* msg)
    (queue-discord-notification *alerts-webhook-url* msg :color color :title "🚨 ALERT")))

(defun notify-discord-status (msg &key (color 3066993))
  "Status channel - async"
  (when (and *status-webhook-url* msg)
    (queue-discord-notification *status-webhook-url* msg :color color :title "📊 Status")))

(defun notify-discord-daily (msg &key (color 3447003))
  "Daily report channel - async"
  (let ((webhook (or *discord-daily-webhook* *discord-webhook-url*)))
    (when (and webhook msg)
      (queue-discord-notification webhook msg :color color :title "📊 Daily Report"))))

(defun notify-discord-weekly (msg)
  "Weekly summary channel - async"
  (let ((webhook (or *discord-weekly-webhook* *discord-webhook-url*)))
    (when (and webhook msg)
      (queue-discord-notification webhook msg :color 10181046 :title "📈 Weekly Summary"))))

;;; ==========================================
;;; SYNC FUNCTIONS (for urgent messages)
;;; ==========================================

(defun notify-discord-emergency (msg)
  "EMERGENCY - synchronous for immediate delivery"
  (let ((webhook (or *discord-emergency-url* *discord-webhook-url*)))
    (when webhook
      (handler-case
          (dex:post webhook
                    :content (jsown:to-json (jsown:new-js 
                              ("embeds" (list (jsown:new-js 
                                ("title" "🚨 EMERGENCY 🚨") 
                                ("description" (format nil "~a" msg)) 
                                ("color" 15158332))))))
                    :headers '(("Content-Type" . "application/json")) :read-timeout 3)
        (error (e) nil)))))

(defun notify-discord-backtest (msg &key (color 3447003))
  "Backtest results - synchronous, silent channel"
  (when (and *backtest-webhook-url* msg)
    (handler-case
        (dex:post *backtest-webhook-url*
                  :content (jsown:to-json (jsown:new-js 
                              ("embeds" (list (jsown:new-js ("description" (format nil "~a" msg)) ("color" color))))
                              ("flags" 4096)))  ;; SUPPRESS_NOTIFICATIONS
                  :headers '(("Content-Type" . "application/json")) :read-timeout 3)
      (error (e) nil))))

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
        (summary-msg (format nil "📊 **Backtest Summary (V6.9)**~%")))
    
    (dolist (cat categories)
      (let* ((cat-results (remove-if-not 
                            (lambda (res) (eq (categorize-strategy-name (car res)) cat))
                            results))
             (total (length cat-results))
             (passed (count-if (lambda (res) (> (getf (cdr res) :sharpe) 0)) cat-results))
             (avg-sharpe (if (> total 0)
                             (/ (loop for r in cat-results sum (getf (cdr r) :sharpe)) total)
                             0.0))
             ;; Sort by Sharpe descending
             (sorted (sort (copy-list cat-results) #'> :key (lambda (x) (getf (cdr x) :sharpe)))))
        
        (when (> total 0)
          (setf summary-msg (concatenate 'string summary-msg 
                                         (format nil "~%**~a** (Total: ~d | Pass: ~d | Avg Sharpe: ~,2f)~%" 
                                                 cat total passed avg-sharpe)))
          
          ;; Top 5
          (setf summary-msg (concatenate 'string summary-msg (format nil "  Top 5:~%")))
          (loop for i from 0 below (min 5 (length sorted))
                for res = (nth i sorted)
                do (setf summary-msg (concatenate 'string summary-msg 
                                                  (format nil "    ~d. ~a (S: ~,2f)~%" (1+ i) (car res) (getf (cdr res) :sharpe)))))
          
          ;; Worst 5 (if enough results)
          (when (> total 5)
            (setf summary-msg (concatenate 'string summary-msg (format nil "  Worst 5:~%")))
            (let ((worst (reverse sorted)))
              (loop for i from 0 below (min 5 (length worst))
                    for res = (nth i worst)
                    do (setf summary-msg (concatenate 'string summary-msg 
                                                      (format nil "    ~d. ~a (S: ~,2f)~%" (1+ i) (car res) (getf (cdr res) :sharpe))))))))))
    
    (setf summary-msg (concatenate 'string summary-msg (format nil "~%🔍 Only showing Top/Worst to reduce spam.")))
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

(format t "[L] 💬 core/discord.lisp loaded - Discord notifications consolidated~%")
