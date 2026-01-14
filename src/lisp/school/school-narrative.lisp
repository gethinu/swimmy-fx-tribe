;;; school-narrative.lisp - Narrative Generation & Discord Messaging
;;; Part of the Swimmy School System
;;; Extracted from school.lisp to comply with SRP (Expert Panel 2026-01-13)

(in-package :swimmy.school)

;; Generate dynamic narrative with actual values
(defun generate-dynamic-narrative (strat-signal symbol price)
  "Generate natural language explanation"
  (let* ((name (getf strat-signal :strategy-name))
         (direction (getf strat-signal :direction))
         (category (getf strat-signal :category))
         (ind-vals (getf strat-signal :indicator-values))
         ;; V5.1: Default SL/TP when strategy has nil
         (sl (or (getf strat-signal :sl) 0.15))  ; Default 15 pips
         (tp (or (getf strat-signal :tp) 0.40))  ; Default 40 pips
         (clan (get-clan category)))
    (format nil "
═══════════════════════════════
~a 【~a】が戦場に立つ！
═══════════════════════════════

📊 発動戦略: ~a

~{~a~^~%~}

📍 ~a @ ~,3f (🕐 ~a)
~a

🎯 利確: +~d pips | 🛡️ 損切: -~d pips

💪 この条件で行く。
═══════════════════════════════~a"
            (if clan (clan-emoji clan) "🏛️") 
            (if clan (clan-name clan) "Unknown")
            name
            (mapcar (lambda (iv) (format nil "• ~a = ~,2f" (first iv) (second iv))) ind-vals)
            symbol price 
             (swimmy.core:get-jst-timestamp)
             (if (eq direction :buy) "🟢 BUY - 上昇を狙う" "🔴 SELL - 下落を狙う")
             (round (* 100 tp)) (round (* 100 sl))
             (get-clan-positions-summary))))

(defun get-clan-positions-summary ()
  "Generate a compact summary of active positions for all clans"
  (if (hash-table-p *warrior-allocation*)
      (let ((hunters nil) (breakers nil) (raiders nil) (shamans nil)
            (summary ""))
        
        ;; Aggregate positions
        (maphash (lambda (k v)
                   (declare (ignore k))
                   (when v
                     (let ((sym (getf v :symbol))
                           (cat (getf v :category)))
                       (case cat
                         (:trend (pushnew sym hunters :test #'string=))
                         (:breakout (pushnew sym breakers :test #'string=))
                         (:scalp (pushnew sym raiders :test #'string=))
                         (:reversion (pushnew sym shamans :test #'string=))
                         (:hunters (pushnew sym hunters :test #'string=))     ; Alias
                         (:breakers (pushnew sym breakers :test #'string=))   ; Alias
                         (:raiders (pushnew sym raiders :test #'string=))     ; Alias
                         (:shamans (pushnew sym shamans :test #'string=)))))) ; Alias
                 *warrior-allocation*)
        
        ;; Format Text
        (format nil "
🏰 **Active Battlefields**:
🏹 Hunters : ~a
⚔️ Breakers: ~a
🗡️ Raiders : ~a
🔮 Shamans : ~a"
                (if hunters (format nil "~{~a~^, ~}" hunters) "-")
                (if breakers (format nil "~{~a~^, ~}" breakers) "-")
                (if raiders (format nil "~{~a~^, ~}" raiders) "-")
                (if shamans (format nil "~{~a~^, ~}" shamans) "-")))
      ""))
