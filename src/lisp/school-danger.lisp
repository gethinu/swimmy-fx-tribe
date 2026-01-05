;; school-danger.lisp - Danger Avoidance System
;; V6.15: Extracted from school.lisp for modular architecture

(in-package :swimmy.school)
;; Inspired by: AlphaGo's resignation logic + RLHF safety
;; Purpose: Protect the school from predators (consecutive losses)

;;; Variables defined in school-state.lisp:
;;; *consecutive-losses*, *consecutive-wins*, *last-trade-result*
;;; *danger-cooldown-until*, *danger-level*, *cooldown-durations*

(defun record-trade-result (result)
  "Record trade result and update danger level"
  (setf *last-trade-result* result)
  (if (eq result :loss)
      (progn
        (incf *consecutive-losses*)
        (setf *consecutive-wins* 0)
        ;; Trigger cooldown on consecutive losses
        (when (>= *consecutive-losses* 2)
          (activate-danger-cooldown)))
      (progn
        (incf *consecutive-wins*)
        (setf *consecutive-losses* 0)
        ;; Recovery from danger
        (when (> *danger-level* 0)
          (decf *danger-level*)
          (format t "[L] 🩹 RECOVERY: Danger level decreased to ~d~%" *danger-level*)))))

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

(defun activate-danger-cooldown ()
  "Activate cooldown based on consecutive losses"
  (let* ((losses *consecutive-losses*)
         (cooldown-entry (or (assoc losses *cooldown-durations* :test #'<=)
                             (cons 5 1800)))
         (duration (cdr cooldown-entry)))
    (setf *danger-level* (min 3 (- losses 1)))
    (setf *danger-cooldown-until* (+ (get-universal-time) duration))
    (format t "~%[L] 🦈🦈🦈 DANGER DETECTED! ~d consecutive losses~%" losses)
    (format t "[L] 🏃 FLEE MODE: Trading suspended for ~d seconds~%" duration)
    (format t "[L] 🐟 School retreating to safety...~%~%")
    (when (fboundp 'notify-discord-alert)
      (notify-discord-alert (format nil "🦈 DANGER: ~d consecutive losses. FLEE MODE activated for ~ds." losses duration)))
    (execute-tactical-retreat)))

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
