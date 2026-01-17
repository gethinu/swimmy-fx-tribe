(in-package :swimmy.main)

;;; ==========================================
;;; NARRATIVE - Extracted from tick-handler.lisp (SRP Refactor)
;;; ==========================================
;;; Handles:
;;; - Daily tribal narrative generation
;;; - Flood/Risk status metaphors
;;; - Discord message formatting

(defun get-flood-status ()
  "Convert Danger Level and Drawdown into a Flood Metaphor"
  (let ((danger (if (boundp '*danger-level*) *danger-level* 0))
        (dd (if (boundp '*max-drawdown*) *max-drawdown* 0.0)))
    (cond
      ((>= danger 4) "🌊 **TSUNAMI** (Evacuate!)")
      ((>= danger 3) "💀 **Neck Deep** (Can't Breathe)")
      ((>= danger 2) "🩳 **Waist Deep** (Hard to Move)")
      ((>= danger 1) "👢 **Ankle Deep** (Wet Socks)")
      ((> dd 5.0)    "🌧️ **Heavy Rain** (Puddles Forming)")
      (t             "🏜️ **Dry Land** (Safe)"))))

(defun send-daily-tribal-narrative ()
  "Send a daily summary of tribal sentiments and results in Japanese with dynamic storytelling"
  (let* ((pnl *daily-pnl*)
         (wins *consecutive-wins*)
         (losses *consecutive-losses*)
         (tribe-dir (if (boundp '*tribe-direction*) *tribe-direction* "N/A"))
         ;; Generate dynamic quotes based on situation (AGGRESSIVE V7.0)
         (shaman-quote (cond ((< pnl -1000) "「嵐の中に宝がある。恐れるな、進め。」")
                             ((< pnl -300) "「血が流れている...だが傷は癒える。」")
                             ((< pnl 0) "「小さな痛みは、大きな勝利の前触れだ。」")
                             ((> pnl 3000) "「神々が味方した！もっと貪欲に！」")
                             ((> pnl 1000) "「星々が並んだ。これが運命（さだめ）だ。」")
                             (t "「10年の歴史が見える...今が決戦の時だ。」")))
         (chief-quote (cond ((> pnl 0) "「見事だ。だが満足するな。全てを奪え。」")
                            ((< pnl 0) "「後退ではない。助走だ。死ぬ気で取り返せ。」")
                            (t "「地下壕から出ろ。世界を我らの色に染める時が来た。」")))
         ;; V5.6: Flood Status
         (flood-status (get-flood-status)))
    
    (notify-discord-daily (format nil "
📜 **日刊・部族クロニクル (ATTACK MODE)**
━━━━━━━━━━━━━━━━━━━━━━━━━━
💰 昨日の戦果: ¥~,0f
🔥 現在の戦況: ~d 連勝中 | ~d 連敗中
🌊 **洪水警報 (Risk Level)**:
~a

⚔️ **部族の方向**: ~a
━━━━━━━━━━━━━━━━━━━━━━━━━━
💬 **シャーマンの言葉**:
~a

👑 **チーフからの檄**:
~a
━━━━━━━━━━━━━━━━━━━━━━━━━━
" pnl wins losses flood-status tribe-dir shaman-quote chief-quote)
     :color (cond ((>= (if (boundp '*danger-level*) *danger-level* 0) 3) 15158332) ; Red
                  ((>= (if (boundp '*danger-level*) *danger-level* 0) 1) 16776960) ; Yellow
                  (t 3447003))))) ; Blue/Green
