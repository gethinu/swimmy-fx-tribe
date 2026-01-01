;; school-resignation.lisp - Resignation Judgment System
;; V6.15: Extracted from school.lisp for modular architecture
;; Inspired by: Shogi/Chess AI resignation logic
;; "Knowing when to stop is as important as knowing when to trade"

;;; Variables defined in school-state.lisp:
;;; *has-resigned-today*, *resignation-threshold*, *resignation-loss-count*

(defun check-resignation ()
  "Check if today's trading should be abandoned"
  (when *has-resigned-today*
    (return-from check-resignation t))
  
  (let ((should-resign nil)
        (reason nil))
    
    ;; Condition 1: Daily loss exceeds threshold
    (when (< *daily-pnl* *resignation-threshold*)
      (setf should-resign t)
      (setf reason (format nil "Daily loss ¥~:d exceeds limit ¥~:d" 
                           (round *daily-pnl*) *resignation-threshold*)))
    
    ;; Condition 2: Too many consecutive losses
    (when (>= *consecutive-losses* *resignation-loss-count*)
      (setf should-resign t)
      (setf reason (format nil "~d consecutive losses - strategy mismatch" 
                           *consecutive-losses*)))
    
    ;; Condition 3: Daily goal already met (positive resignation)
    (when (and (> *daily-pnl* 0) 
               (> *daily-pnl* (* 1.5 (get-daily-target))))
      (setf should-resign t)
      (setf reason (format nil "Daily goal 150%% achieved: ¥~:d" (round *daily-pnl*))))
    
    (when should-resign
      (setf *has-resigned-today* t)
      (format t "~%[L] 🏳️ ═══════════════════════════════════════~%")
      (format t "[L] 🏳️  RESIGNATION: Today's trading ends~%")
      (format t "[L] 🏳️  Reason: ~a~%" reason)
      (format t "[L] 🏳️  Tomorrow is another day.~%")
      (format t "[L] 🏳️ ═══════════════════════════════════════~%~%")
      (when (fboundp 'notify-discord-alert)
        (notify-discord-alert (format nil "🏳️ RESIGNATION: Trading ended for today.~%Reason: ~a" reason))))
    
    should-resign))

(defun has-resigned-p ()
  "Check if we've resigned today"
  *has-resigned-today*)

(defun is-safe-to-trade-p ()
  "Master safety check - combines all safety conditions including resignation"
  (cond
    ;; Already resigned - no more trading today
    ((has-resigned-p)
     (format t "[L] 🏳️ RESIGNED: No trading until tomorrow~%")
     nil)
    ;; Check if we should resign
    ((check-resignation)
     nil)
    ;; In cooldown - definitely not safe
    ((danger-cooldown-active-p)
     (format t "[L] ⏸️ COOLDOWN: ~d seconds remaining~%" (get-cooldown-remaining))
     nil)
    ;; High danger level - extra caution
    ((>= *danger-level* 2)
     (format t "[L] ⚠️ HIGH DANGER: Level ~d - trading cautiously~%" *danger-level*)
     t)
    ;; Safe to trade
    (t t)))

(format t "[L] 🏳️ school-resignation.lisp loaded - Resignation Judgment active~%")
