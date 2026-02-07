(in-package :swimmy.core)

;; core/discord.lisp - Async Discord Notification Functions
;; V42.0: FULL SEPARATION (Phase 2 - Notifier Service)
;; Replaces Dexador/HTTP with ZMQ PUSH to tools/notifier.py (Port 5562)

;;; ==========================================
;;; MIGRATION NOTE
;;; ==========================================
;;; OLD: Lisp -> Dexador -> Discord API (Sync/Blocking or Threaded queue)
;;; NEW: Lisp -> ZMQ PUSH -> Notifier.py -> Discord API (Truly Async)

(defparameter *notifier-endpoint* (zmq-connect-endpoint *port-notifier*))
(defparameter *notifier-socket* nil)
(defparameter *notifier-context* nil)

;;; ==========================================
;;; NOTIFICATION CONSTANTS (Expert Panel P2)
;;; ==========================================
(defconstant +color-recruit+ 9132483 "Green/Success")
(defconstant +color-alert+   15158332 "Red/Warning")
(defconstant +color-backtest+ 3447003 "Blue/Process")
(defconstant +color-success+  3066993 "Bright Green")
(defconstant +color-info+     5763719 "Gray/Info")
(defconstant +color-emergency+ 16711680 "Critical Red")
(defconstant +color-weekly+    10181046 "Purple/Weekly")
(defconstant +color-status+    10070709 "Dark Gray")

;;; ==========================================
;;; BATCHED ALERTS (Max Age Retirement)
;;; ==========================================
(defparameter *max-age-retire-batch-interval* 3600 "Seconds to batch Max Age Retirement alerts")
(defvar *max-age-retire-buffer* nil "Buffered strategy names for Max Age Retirement alerts")
(defvar *max-age-retire-first-seen* 0 "Timestamp when current Max Age Retirement batch started")

(defun queue-max-age-retire (strategy-name &key (now (get-universal-time)))
  "Queue a Max Age Retirement alert for hourly batching."
  (unless (and (stringp strategy-name) (> (length strategy-name) 0))
    (return-from queue-max-age-retire nil))
  (push strategy-name *max-age-retire-buffer*)
  (when (<= *max-age-retire-first-seen* 0)
    (setf *max-age-retire-first-seen* now))
  t)

(defun maybe-flush-max-age-retire (&optional (now (get-universal-time)))
  "Flush Max Age Retirement summary if the batch interval has elapsed."
  (when (and *max-age-retire-buffer*
             (> (- now *max-age-retire-first-seen*) *max-age-retire-batch-interval*))
    (let* ((total (length *max-age-retire-buffer*))
           (ordered (reverse *max-age-retire-buffer*))
           (top (subseq ordered 0 (min 5 (length ordered))))
           (msg (format nil "🧊 **Max Age Retirement Summary (Last 1h)**~%Total: ~d~%Top: ~{`~a`~^, ~}~%Action: Individual alerts suppressed."
                        total top)))
      (setf *max-age-retire-buffer* nil)
      (setf *max-age-retire-first-seen* 0)
      (notify-discord-alert msg :color +color-alert+))))



(defun ensure-notifier-connection ()
  "Ensure ZMQ connection to Notifier is active"
  (unless *notifier-context*
    (setf *notifier-context* (pzmq:ctx-new)))
  (unless *notifier-socket*
    (setf *notifier-socket* (pzmq:socket *notifier-context* :push))
    (pzmq:connect *notifier-socket* *notifier-endpoint*)
    (format t "[DISCORD] Connected to Notifier service at ~a~%" *notifier-endpoint*)))

(defun queue-discord-notification (webhook msg &key (color +color-backtest+) (title "Swimmy Notification"))
  "Send notification to Notifier service via ZMQ"
  (when (and webhook msg (not (equal msg "NIL")))
    (ensure-notifier-connection)
    (handler-case
        (let* ((payload (jsown:new-js
                          ("embeds" (list (jsown:new-js
                                            ("title" title)
                                            ("description" (format nil "~a" msg))
                                            ("color" color))))))
               (payload-json (jsown:to-json payload))
               (msg-sexp `((type . "NOTIFIER")
                           (schema_version . 1)
                           (action . "SEND")
                           (webhook . ,webhook)
                           (payload_json . ,payload-json))))
          (pzmq:send *notifier-socket* (encode-sexp msg-sexp))
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
        (let* ((payload-json (if (stringp payload) payload (jsown:to-json payload)))
               (msg-sexp `((type . "NOTIFIER")
                           (schema_version . 1)
                           (action . "SEND")
                           (webhook . ,webhook)
                           (payload_json . ,payload-json))))
          (pzmq:send *notifier-socket* (encode-sexp msg-sexp))
          (setf *last-zmq-success-time* (get-universal-time))
          (setf *last-discord-notification-time* (get-universal-time))
          t)
      (error (e)
        (format t "[DISCORD] Failed to send raw ZMQ msg: ~a~%" e)
        nil))))

;;; ==========================================
;;; PUBLIC API (Backward Compatible)
;;; ==========================================

(defun notify-discord (msg &key (color +color-backtest+))
  "Main notification"
  (when *discord-webhook-url*
    (queue-discord-notification *discord-webhook-url* msg :color color :title "🐟 Apex")))

(defun notify-discord-symbol (symbol msg &key (color +color-backtest+))
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

(defun notify-discord-alert (msg &key (color +color-alert+))
  "Alerts channel"
  (when *alerts-webhook-url*
    (queue-discord-notification *alerts-webhook-url* msg :color color :title "🚨 ALERT")))

(defun notify-discord-status (msg &key (color +color-success+))
  "Status channel"
  (when *status-webhook-url*
    (queue-discord-notification *status-webhook-url* msg :color color :title "📊 Status")))

(defun notify-discord-daily (msg &key (color +color-backtest+))
  "Daily report channel"
  (let ((webhook (or *discord-daily-webhook* *discord-webhook-url*)))
    (when webhook
      (queue-discord-notification webhook msg :color color :title "📊 Daily Report"))))

(defun notify-discord-weekly (msg)
  "Weekly summary channel"
  (let ((webhook (or *discord-weekly-webhook* *discord-webhook-url*)))
    (when webhook
      (queue-discord-notification webhook msg :color +color-weekly+ :title "📈 Weekly Summary"))))

(defun notify-discord-recruit (msg &key (color +color-success+))
  "Recruitment channel"
  (let ((webhook (or *discord-recruit-webhook* *discord-webhook-url*)))
    (when webhook
      (queue-discord-notification webhook msg :color color :title "🐟 Apex"))))

(defun notify-discord-emergency (msg)
  "EMERGENCY - Now also async via Notifier (since Notifier is reliable)"
  (let ((webhook (or *discord-emergency-url* *discord-webhook-url*)))
    (when webhook
      (queue-discord-notification webhook msg :color +color-emergency+ :title "🚨 EMERGENCY 🚨"))))

(defun notify-apex (msg &key (color +color-success+))
  "System status to Apex channel (Online, Recovery, Critical alerts)"
  (when *apex-webhook-url*
    (queue-discord-notification *apex-webhook-url* msg :color color :title "🐟 Apex")))

(defun notify-discord-backtest (msg &key (color +color-backtest+))
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

(defun notify-backtest-summary (&optional (type :rr))
   "V49.6: Enhanced summary. Restores Rank Distribution and ensures content visibility."
  (let* ((buffer-sym (case type
                       (:qual 'swimmy.globals:*qual-backtest-results-buffer*)
                       (:rr 'swimmy.globals:*rr-backtest-results-buffer*)
                       (t 'swimmy.globals:*backtest-results-buffer*)))
         (expected-sym (case type
                         (:qual 'swimmy.globals:*qual-expected-backtest-count*)
                         (:rr 'swimmy.globals:*rr-expected-backtest-count*)
                         (t 'swimmy.globals:*expected-backtest-count*)))
         ;; 1. Capture current state
         (results (copy-list (symbol-value buffer-sym)))
         (expected (symbol-value expected-sym)))
    
    (unless (or results (> expected 0))
      (return-from notify-backtest-summary nil))

    ;; 2. CLEAR IMMEDIATELY to prevent double-processing (Race Condition Fix)
    (setf (symbol-value buffer-sym) nil)
    (setf (symbol-value expected-sym) 0)

    (let* ((category-map (make-hash-table :test 'equal))
           (title (if (eq type :qual) "📊 **Qualification Batch (Incubator)**" "📊 **Round-Robin KB Batch**"))
           (report-msg (format nil "~a~%⏰ ~a~%~%Progress: ~d/~d results received.~%~%" 
                               title (get-jst-timestamp) (length results) expected)))
    
      ;; 0. Rank Distribution (Only for RR cycles or if KB is relevant)
      (when (or (eq type :rr) (null type))
        (let* ((counts (swimmy.school:get-db-rank-counts))
               (s-count (getf counts :s 0))
               (a-count (getf counts :a 0))
               (b-count (getf counts :b 0))
               (grave-count (getf counts :graveyard 0)))
          (setf report-msg (concatenate 'string report-msg
                                        (format nil "**Current Rank Distribution:**~%🏆 S-Rank: ~d | 🎯 A-Rank: ~d | 📋 B-Rank: ~d~%⚰️ Graveyard/Pending: ~d~%~%"
                                                s-count a-count b-count grave-count)))))

      ;; 1. Group results by category
      (dolist (res results)
        (let* ((name (car res))
               (metrics (cdr res))
               (sharpe (getf metrics :sharpe))
               (strat (or (find name swimmy.globals:*strategy-knowledge-base* :key #'swimmy.school:strategy-name :test #'string=)
                          (find name swimmy.globals:*evolved-strategies* :key #'swimmy.school:strategy-name :test #'string=))))
          (if strat
              (let* ((sym (swimmy.school:strategy-symbol strat))
                     (dir (swimmy.school:strategy-direction strat))
                     (tf (swimmy.school:strategy-timeframe strat))
                     (cat-key (format nil "~a/~a/M~d" sym dir tf)))
                (push sharpe (gethash cat-key category-map)))
              (push sharpe (gethash "New/Testing" category-map)))))
      
      ;; 2. Calculate category stats
      (let ((cat-stats nil))
        (maphash (lambda (k v)
                   (let ((avg (/ (reduce #'+ v) (length v))))
                     (push (list :cat k :avg avg :count (length v)) cat-stats)))
                 category-map)
        
        (setf cat-stats (sort cat-stats #'> :key (lambda (x) (getf x :avg))))
        
        ;; 3. Build the report
        (when cat-stats
          (setf report-msg (concatenate 'string report-msg "🚀 **Strongest Categories (Current Batch):**~%"))
          (loop for i from 0 below (min 5 (length cat-stats))
                for stat = (nth i cat-stats)
                do (setf report-msg (concatenate 'string report-msg 
                                                  (format nil "  • ~a: ~,2f Sharpe (~d strats)~%" 
                                                          (getf stat :cat) (getf stat :avg) (getf stat :count)))))
          
          (when (> (length cat-stats) 5)
            (setf report-msg (concatenate 'string report-msg "~%📉 **Weakest Categories:**~%"))
            (let ((weakest (reverse (last cat-stats (min 3 (length cat-stats))))))
              (dolist (stat weakest)
                (setf report-msg (concatenate 'string report-msg 
                                                (format nil "  • ~a: ~,2f Sharpe (~d strats)~%" 
                                                        (getf stat :cat) (getf stat :avg) (getf stat :count))))))))

        ;; 4. Individual Results Listing
        (when results
          (let ((sorted (sort (copy-list results) #'> :key (lambda (x) (getf (cdr x) :sharpe))))
                (top-count (min 10 (length results))))
            (setf report-msg (concatenate 'string report-msg "~%🌟 **Top Strategy Results (Latest):**~%"))
            (loop for i from 0 below top-count
                  for s = (nth i sorted)
                  do (setf report-msg (concatenate 'string report-msg 
                                                  (format nil "  • `~a`: ~,2f Sharpe (~d trades)~%" 
                                                          (car s) (getf (cdr s) :sharpe) (getf (cdr s) :trades 0)))))))

        (notify-discord-backtest report-msg :color (if (eq type :qual) 3447003 5763719))))))

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
         (color (if is-passed +color-success+ +color-alert+)) ; Green if passed, Red if failed
         (msg (format nil "~a **CPCV Validation: ~a**~%Strategy: `~a`~%~%**Metrics:**~%• Median Sharpe: ~,2f~%• Paths: ~d~%• Result: ~d Passed / ~d Failed (~,1f%)~%~%**Outcome:** ~a"
                      status-emoji status-text name median-sharpe paths passed failed (* 100 pass-rate) status-text)))
    (notify-discord-alert msg :color color)
    ;; V48.5: Performance Persistence (Expert Panel P3)
    (log-cpcv-to-file data)))

(defun log-cpcv-to-file (data)
  "Save CPCV result to a local CSV for future training/audit."
  (let ((path "data/logs/cpcv_history.csv")
        (header "timestamp,strategy,median_sharpe,paths,pass_rate,outcome"))
    (ensure-directories-exist path)
    (unless (probe-file path)
      (with-open-file (s path :direction :output)
        (format s "~a~%" header)))
    (with-open-file (s path :direction :output :if-exists :append)
      (format s "~d,~a,~,4f,~d,~,4f,~a~%"
              (get-universal-time)
              (or (getf data :strategy-name) "unknown")
              (getf data :median-sharpe 0.0)
              (getf data :path-count 0)
              (getf data :pass-rate 0.0)
              (if (getf data :is-passed) "PASS" "FAIL")))))

(defun notify-cpcv-summary ()
  "V49.5: Send a summary of the CPCV batch to Discord."
  (let* ((results (copy-list *cpcv-results-buffer*))
         (expected *expected-cpcv-count*))
    
    (unless (or results (> expected 0))
      (return-from notify-cpcv-summary nil))

    ;; CLEAR IMMEDIATELY (Race Condition Fix)
    (setf *cpcv-results-buffer* nil)
    (setf *expected-cpcv-count* 0)

    (let* ((promoted 0)
           (failed 0)
           (msg (format nil "🔬 **CPCV Batch Summary**~%⏰ ~a JST~%~%Progress: ~d/~d results.~%~%" 
                            (swimmy.core:get-jst-timestamp) (length results) expected)))

      (dolist (res results)
        (if (getf res :is-passed)
            (incf promoted)
            (incf failed)))
      
      (setf msg (concatenate 'string msg 
                             (format nil "A-RANK Tested: ~d~%" (length results))
                             (format nil "🏆 S-RANK Promoted: ~d~%" promoted)
                             (format nil "❌ CPCV Failed: ~d~%~%" failed)))
      
      (when (> promoted 0)
        (setf msg (concatenate 'string msg "🌟 **New Promotions:**~%"))
        (dolist (res (subseq results 0 (min 5 (length results))))
          (when (getf res :is-passed)
            (setf msg (concatenate 'string msg (format nil "  • `~a` (S: ~,2f)~%" 
                                                   (getf res :strategy-name) 
                                                   (getf res :median-sharpe)))))))
      
      (notify-discord-alert msg :color (if (> promoted 0) 5763719 15158332)))))

(defun check-timeout-flushes ()
  "V49.5 Expert Panel P1: Flush stagnant buffers after 5 minutes."
  (let ((now (get-universal-time))
        (timeout (* 5 60)))
    ;; 1. Round-Robin KB
    (when (and (> *rr-expected-backtest-count* 0)
               (> (length *rr-backtest-results-buffer*) 0)
               (> (- now *rr-backtest-start-time*) timeout))
      (format t "[DISCORD] ⏳ Flushing RR-Batch due to timeout.~%")
      (notify-backtest-summary :rr))
    
    ;; 2. Qualification
    (when (and (> *qual-expected-backtest-count* 0)
               (> (length *qual-backtest-results-buffer*) 0)
               (> (- now *qual-backtest-start-time*) timeout))
      (format t "[DISCORD] ⏳ Flushing Qual-Batch due to timeout.~%")
      (notify-backtest-summary :qual))
      
    ;; 3. CPCV
    (when (and (> *expected-cpcv-count* 0)
               (> (length *cpcv-results-buffer*) 0)
               (> (- now *cpcv-start-time*) timeout))
      (format t "[DISCORD] ⏳ Flushing CPCV-Batch due to timeout.~%")
      (notify-cpcv-summary))

    ;; 4. Max Age Retirement (Hourly batch)
    (maybe-flush-max-age-retire now)))




(format t "[L] 💬 core/discord.lisp loaded - V42.0 (Phase 2 Notifier)~%")
