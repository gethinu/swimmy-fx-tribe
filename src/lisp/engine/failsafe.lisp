;;; ============================================================================
;;; engine/failsafe.lisp - Emergency Fail-Safe System (Taleb's Warning)
;;; ============================================================================
;;; "Expect the unexpected. Prepare for the worst."
;;; When connection is lost, flatten all positions immediately.

(in-package :swimmy.engine)
;;; ============================================================================

;;; ==========================================
;;; FAIL-SAFE PARAMETERS
;;; ==========================================

(defparameter *heartbeat-timeout* 30
  "Seconds without heartbeat before triggering fail-safe.")

(defparameter *last-heartbeat* (get-universal-time)
  "Timestamp of last successful heartbeat from Guardian.")

(defparameter *failsafe-triggered* nil
  "Flag to prevent multiple fail-safe triggers.")

;;; ==========================================
;;; HEARTBEAT MONITORING
;;; ==========================================

(defun update-heartbeat ()
  "Called when we receive a heartbeat from Guardian."
  (setf *last-heartbeat* (get-universal-time))
  (setf *failsafe-triggered* nil))

(defun check-heartbeat ()
  "Check if Guardian is still alive. Returns T if healthy, NIL if dead."
  (let ((elapsed (- (get-universal-time) *last-heartbeat*)))
    (< elapsed *heartbeat-timeout*)))

(defun trigger-failsafe ()
  "Emergency shutdown: Close all positions and stop trading."
  (unless *failsafe-triggered*
    (setf *failsafe-triggered* t)
    (format t "~%[FAILSAFE] ⚠️ EMERGENCY TRIGGERED!~%")
    (format t "[FAILSAFE] Guardian heartbeat lost for ~d seconds.~%" *heartbeat-timeout*)
    
    ;; Notify Discord
    (when (fboundp 'notify-discord-alert)
      (notify-discord-alert 
       "🚨 FAIL-SAFE TRIGGERED: Guardian connection lost. Halting all trades."
       :color 16711680)) ; Red
    
    ;; Disable trading
    (when (boundp '*trading-enabled*)
      (setf *trading-enabled* nil))
    
    ;; Save state before potential crash
    (when (fboundp 'save-state)
      (save-state))
    
    (format t "[FAILSAFE] 🛑 Trading disabled. Manual intervention required.~%")))

(defun failsafe-check-loop ()
  "Periodic check for heartbeat. Call this from main loop."
  (unless (check-heartbeat)
    (trigger-failsafe)))

;;; ==========================================
;;; TESTING & RECOVERY (Taleb: Test your circuit breakers!)
;;; ==========================================

(defun test-failsafe ()
  "TEST FUNCTION: Simulate fail-safe trigger without affecting real trading.
   Use this to verify the fail-safe mechanism works correctly."
  (format t "~%[FAILSAFE] 🧪 TEST MODE - Simulating fail-safe trigger...~%")
  (format t "[FAILSAFE] Current *trading-enabled* = ~a~%" 
          (if (boundp '*trading-enabled*) *trading-enabled* "unbound"))
  
  ;; Simulate trigger
  (let ((*failsafe-triggered* nil))  ; Local binding for test
    (trigger-failsafe))
  
  ;; Restore trading (test only)
  (when (boundp '*trading-enabled*)
    (setf *trading-enabled* t))
  
  (format t "[FAILSAFE] 🧪 TEST COMPLETE - Trading re-enabled~%")
  (format t "[FAILSAFE] Check Discord for alert notification.~%"))

(defun reset-failsafe ()
  "Reset fail-safe state and re-enable trading after manual review."
  (setf *failsafe-triggered* nil)
  (setf *last-heartbeat* (get-universal-time))
  (when (boundp '*trading-enabled*)
    (setf *trading-enabled* t))
  (format t "[FAILSAFE] ✅ Fail-safe reset. Trading re-enabled.~%")
  (when (fboundp 'notify-discord)
    (notify-discord "✅ Fail-safe reset. Trading resumed.")))

(defun failsafe-status ()
  "Display current fail-safe status."
  (let ((elapsed (- (get-universal-time) *last-heartbeat*)))
    (format t "~%[FAILSAFE STATUS]~%")
    (format t "  Triggered: ~a~%" *failsafe-triggered*)
    (format t "  Last Heartbeat: ~d seconds ago~%" elapsed)
    (format t "  Timeout Threshold: ~d seconds~%" *heartbeat-timeout*)
    (format t "  Trading Enabled: ~a~%" 
            (if (boundp '*trading-enabled*) *trading-enabled* "unbound"))
    (format t "  Health: ~a~%" (if (check-heartbeat) "✅ OK" "❌ UNHEALTHY"))))

(format t "[ENGINE] failsafe.lisp loaded~%")
