;; school-danger.lisp - Danger Avoidance System
;; V6.15: Extracted from school.lisp for modular architecture

(in-package :swimmy.school)
;; Inspired by: AlphaGo's resignation logic + RLHF safety
;; Purpose: Protect the school from predators (consecutive losses)

;;; Variables defined in school-state.lisp:
;;; *consecutive-losses*, *consecutive-wins*, *last-trade-result*
;;; *danger-cooldown-until*, *danger-level*, *cooldown-durations*

(defun check-dynamic-circuit-breaker ()
  "Check if dynamic circuit breaker should trip (3 losses in 5 mins)"
  (let ((now (get-universal-time)))
    ;; 1. Prune old losses
    (setf *recent-losses* 
          (remove-if (lambda (ts) (> (- now ts) *max-loss-window-seconds*))
                     *recent-losses*))
    
    ;; 2. Check threshold
    (when (>= (length *recent-losses*) *consecutive-loss-threshold*)
      (setf *circuit-breaker-active* t)
      (setf *breaker-cooldown-end* (+ now *breaker-cooldown-seconds*))
      
      (format t "~%[L] ⚡⚡ CIRCUIT BREAKER TRIPPED! ⚡⚡~%")
      (format t "[L] 🚫 ~d losses in last ~ds. HALTING for ~ds.~%" 
              (length *recent-losses*) *max-loss-window-seconds* *breaker-cooldown-seconds*)
      
      (when (fboundp 'swimmy.core:notify-discord-alert)
        (swimmy.core:notify-discord-alert 
         (format nil "⚡ **CIRCUIT BREAKER TRIPPED**~%~d losses in 5min.~%Halting trading for 15min."
                 (length *recent-losses*))))
      
      (execute-tactical-retreat))))

(defun record-trade-result (result)
  "V44.0: Record trade result and escalate/reset tier"
  (setf *last-trade-result* result)
  (if (eq result :loss)
      (progn
        (incf *consecutive-losses*)
        (setf *consecutive-wins* 0)
        ;; V44.0: Unified Tiered Cooldown - escalate on every loss
        (activate-danger-cooldown))
      (progn
        (incf *consecutive-wins*)
        (setf *consecutive-losses* 0)
        ;; V44.0: Reset tier on WIN (recovery)
        (when (> *cooldown-tier* 0)
          (setf *cooldown-tier* (max 0 (1- *cooldown-tier*)))
          (format t "[L] 🩹 RECOVERY: Cooldown tier decreased to ~d/~d~%" 
                  *cooldown-tier* (1- (length *cooldown-durations*)))))))

(defun get-current-price (symbol type)
  "Get current price from candle history or brain state"
  (let ((candles (gethash symbol *candle-histories*)))
    (if (and candles (first candles))
        (if (eq type :bid) 
            (candle-close (first candles))
            (candle-close (first candles)))
        nil)))

(defun execute-tactical-retreat ()
  "Close all losing positions immediately to stop bleeding (Sun Tzu)"
  (format t "[L] ⚔️ TACTICAL RETREAT INITIATED! Purging weak positions...~%")
  (let ((closed-count 0))
    (maphash 
     (lambda (key warrior)
       (let* ((symbol (getf warrior :symbol))
              (entry (getf warrior :entry))
              (direction (getf warrior :direction))
              (magic (getf warrior :magic))
              (current-bid (get-current-price symbol :bid))
              (current-ask (get-current-price symbol :ask))
              (pnl 0))
         (cond
           ((and (eq direction :long) current-bid)
            (setf pnl (- current-bid entry)))
           ((and (eq direction :short) current-ask)
            (setf pnl (- entry current-ask))))
         (when (< pnl 0)
           (pzmq:send *cmd-publisher* (jsown:to-json (jsown:new-js ("action" "CLOSE") ("symbol" symbol) ("magic" magic))))
           (remhash key *warrior-allocation*)
           (update-symbol-exposure symbol (or (getf warrior :lot) 0.01) :close)
           (incf closed-count)
           (format t "[L] 🍂 Abandoning position: ~a (PnL: ~5f)~%" symbol pnl))))
     *warrior-allocation*)
    (when (> closed-count 0)
      (notify-discord-alert (format nil "⚔️ TACTICAL RETREAT: Closed ~d losing positions." closed-count)))))

(defun format-duration (seconds)
  "Format seconds as human-readable duration"
  (cond
    ((< seconds 60) (format nil "~ds" seconds))
    ((< seconds 3600) (format nil "~dm" (floor seconds 60)))
    (t (format nil "~dh" (floor seconds 3600)))))

(defun activate-danger-cooldown ()
  "V44.0: Unified Tiered Cooldown System (Expert Panel Approved)
   Tiers: 3m→5m→10m→15m→30m→45m→1h→2h→3h→4h→EOD"
  (let* ((max-tier (1- (length *cooldown-durations*)))
         (new-tier (min max-tier (1+ *cooldown-tier*)))
         (duration-entry (nth new-tier *cooldown-durations*)))
    (setf *cooldown-tier* new-tier)
    
    ;; Handle :eod (End of Day) as special case
    (if (eq duration-entry :eod)
        (progn
          ;; Set cooldown to end of day (23:59:59)
          (setf *has-resigned-today* t)
          (setf *danger-cooldown-until* (+ (get-universal-time) 86400)) ; effectively EOD
          (format t "~%[L] 🏳️ RESIGNATION: Trading ended for today (Tier ~d/~d reached)~%" new-tier max-tier)
          (when (fboundp 'notify-discord-alert)
            (notify-discord-alert 
             (format nil "🏳️ **TRADING ENDED FOR TODAY**~%~d consecutive cooldowns exhausted.~%Final tier: ~d/~d"
                     new-tier new-tier max-tier))))
        (progn
          ;; Normal tier cooldown
          (setf *danger-cooldown-until* (+ (get-universal-time) duration-entry))
          (format t "~%[L] ⏸️ COOLDOWN TIER ~d/~d: Pausing for ~a~%" new-tier max-tier (format-duration duration-entry))
          (when (fboundp 'notify-discord-alert)
            (notify-discord-alert 
             (format nil "⏸️ **COOLDOWN TIER ~d/~d**~%Pausing trading for ~a.~%Next tier: ~a"
                     new-tier max-tier 
                     (format-duration duration-entry)
                     (if (= new-tier max-tier) "EOD" (format-duration (nth (1+ new-tier) *cooldown-durations*))))))))))

(defun danger-cooldown-active-p ()
  "Check if we're in danger cooldown mode"
  (> *danger-cooldown-until* (get-universal-time)))

(defun get-cooldown-remaining ()
  "Get remaining cooldown time in seconds"
  (max 0 (- *danger-cooldown-until* (get-universal-time))))

(defun reset-danger-state ()
  "Reset danger state (e.g., at start of new day)"
  (setf *consecutive-losses* 0)
  (setf *consecutive-wins* 0)
  (setf *danger-level* 0)
  (setf *danger-cooldown-until* 0)
  (setf *has-resigned-today* nil)
  (format t "[L] 🌅 Danger state reset - new day, fresh start~%"))

(format t "[L] 🦈 school-danger.lisp loaded - Danger Avoidance System active~%")
