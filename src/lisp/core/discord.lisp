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
          (setf *last-zmq-success-time* (get-universal-time))
          (setf *last-discord-notification-time* (get-universal-time))
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
          (setf *last-zmq-success-time* (get-universal-time))
          (setf *last-discord-notification-time* (get-universal-time))
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
  "Symbol-specific channel (with Fallback for errors)"
  (let* ((looked-up (gethash symbol *symbol-webhooks*))
         ;; Priority: Specific -> Fallback (STOP there)
         (webhook (or looked-up *discord-fallback-webhook*))
         (is-fallback (and (null looked-up) webhook))
         (final-msg (if is-fallback (format nil "⚠️ [FALLBACK] ~a" msg) msg))
         (final-title (if is-fallback 
                          (format nil "⚠️ ~a [UNKNOWN]" symbol) 
                          (format nil "🐟 ~a" symbol))))
    (when webhook
      (queue-discord-notification webhook final-msg :color color :title final-title))))

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
  (multiple-value-bind (sec min hour date month) (decode-universal-time (get-universal-time) -9)
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
  "V48.4: Detailed Category Trend Report (54 Categories).
   Groups results by Symbol x Direction x Timeframe. No narrative flair."
  (let* ((results (copy-list *backtest-results-buffer*))
         (category-map (make-hash-table :test 'equal))
         (report-msg (format nil "📊 **Backtest Category Trends**~%⏰ ~a~%~%" (swimmy.core:get-jst-timestamp))))
    
    ;; 1. Group results by category
    (dolist (res results)
      (let* ((name (car res))
             (metrics (cdr res))
             (sharpe (getf metrics :sharpe))
             ;; Lookup strategy properties
             (strat (or (find name swimmy.globals:*strategy-knowledge-base* :key #'swimmy.school:strategy-name :test #'string=)
                        (find name swimmy.globals:*evolved-strategies* :key #'swimmy.school:strategy-name :test #'string=))))
        (when strat
          (let* ((sym (swimmy.school:strategy-symbol strat))
                 (dir (swimmy.school:strategy-direction strat))
                 (tf (swimmy.school:strategy-timeframe strat))
                 (cat-key (format nil "~a/~a/M~d" sym dir tf)))
            (push sharpe (gethash cat-key category-map))))))
    
    ;; 2. Calculate category stats
    (let ((cat-stats nil))
      (maphash (lambda (k v)
                 (let ((avg (/ (reduce #'+ v) (length v))))
                   (push (list :cat k :avg avg :count (length v)) cat-stats)))
               category-map)
      
      (setf cat-stats (sort cat-stats #'> :key (lambda (x) (getf x :avg))))
      
      ;; 3. Build the report
      (when cat-stats
        (setf report-msg (concatenate 'string report-msg "🚀 **Strongest Categories:**~%"))
        (loop for i from 0 below (min 5 (length cat-stats))
              for stat = (nth i cat-stats)
              do (setf report-msg (concatenate 'string report-msg 
                                                (format nil "  • ~a: ~,2f Sharpe (~d strats)~%" 
                                                        (getf stat :cat) (getf stat :avg) (getf stat :count)))))
        
        (when (> (length cat-stats) 5)
          (setf report-msg (concatenate 'string report-msg "~%📉 **Weakest Categories:**~%"))
          (let ((weakest (reverse (last cat-stats (min 5 (length cat-stats))))))
            (dolist (stat weakest)
              (setf report-msg (concatenate 'string report-msg 
                                              (format nil "  • ~a: ~,2f Sharpe (~d strats)~%" 
                                                      (getf stat :cat) (getf stat :avg) (getf stat :count)))))))
        
        ;; 4. Top 3 Individual Strategies
        (let ((top-strats (subseq (sort (copy-list results) #'> :key (lambda (x) (getf (cdr x) :sharpe)))
                                  0 (min 3 (length results)))))
          (setf report-msg (concatenate 'string report-msg "~%🌟 **Top 3 Individual Results:**~%"))
          (dolist (s top-strats)
            (setf report-msg (concatenate 'string report-msg 
                                            (format nil "  • ~a (S: ~,2f)~%" (car s) (getf (cdr s) :sharpe)))))))

      (notify-discord-backtest report-msg :color 5763719))
    
    ;; Clear buffer
    (setf *backtest-results-buffer* nil)
    (setf *expected-backtest-count* 0)))

(defun notify-cpcv-result (data)
  "V48.5: Notify CPCV Validation Result to Discord Alerts."
  (let* ((name (or (getf data :strategy-name) (getf data :name) "Unknown"))
         (median-sharpe (float (or (getf data :median-sharpe) 0.0)))
         (paths (or (getf data :path-count) 0))
         (passed (or (getf data :passed-count) 0))
         (failed (or (getf data :failed-count) 0))
         (pass-rate (float (or (getf data :pass-rate) 0.0)))
         (is-passed (getf data :is-passed))
         (status-emoji (if is-passed "✅" "❌"))
         (status-text (if is-passed "PASSED" "FAILED"))
         (color (if is-passed 3066993 15158332)) ; Green if passed, Red if failed
         (msg (format nil "~a **CPCV Validation: ~a**~%Strategy: `~a`~%~%**Metrics:**~%• Median Sharpe: ~,2f~%• Paths: ~d~%• Result: ~d Passed / ~d Failed (~,1f%)~%~%**Outcome:** ~a"
                      status-emoji status-text name median-sharpe paths passed failed (* 100 pass-rate) status-text)))
    (notify-discord-alert msg :color color)))

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
