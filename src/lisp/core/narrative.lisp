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

(defun safe-symbol-value (sym)
  "Return symbol value if bound, otherwise NIL."
  (when (boundp sym)
    (symbol-value sym)))

(defun format-value (value formatter &optional (fallback "N/A"))
  "Format VALUE if present, otherwise return FALLBACK."
  (cond
    ((null value) fallback)
    ((numberp value)
     (let ((fmt (string-downcase formatter)))
       (if (search "~d" fmt)
           (format nil formatter (round value))
           (format nil formatter value))))
    ((symbolp value) (format nil formatter (string-upcase (string value))))
    (t (format nil formatter value))))

(defun format-percent (value &optional (fallback "N/A"))
  "Format VALUE as a percentage (0-1 range)."
  (if (numberp value)
      (format nil "~d%" (round (* 100 value)))
      fallback))

(defun send-daily-status-report ()
  "Send a daily summary of system status without narrative storytelling."
  (let* ((pnl (safe-symbol-value 'swimmy.globals::*daily-pnl*))
         (wins (safe-symbol-value 'swimmy.globals::*consecutive-wins*))
         (losses (safe-symbol-value 'swimmy.globals::*consecutive-losses*))
         (trades (safe-symbol-value 'swimmy.globals::*daily-trade-count*))
         (danger-level (safe-symbol-value 'swimmy.globals::*danger-level*))
         (max-dd (safe-symbol-value 'swimmy.globals::*max-drawdown*))
         (monitor-dd (safe-symbol-value 'swimmy.globals::*monitoring-drawdown*))
         (equity (safe-symbol-value 'swimmy.globals::*current-equity*))
         (peak-equity (safe-symbol-value 'swimmy.globals::*peak-equity*))
         (system-state (safe-symbol-value 'swimmy.globals::*system-state*))
         (trading-enabled (safe-symbol-value 'swimmy.globals::*trading-enabled*))
         (current-regime (safe-symbol-value 'swimmy.globals::*current-regime*))
         (volatility-regime (safe-symbol-value 'swimmy.globals::*volatility-regime*))
         (last-prediction (safe-symbol-value 'swimmy.globals::*last-prediction*))
         (last-confidence (safe-symbol-value 'swimmy.globals::*last-confidence*))
         (swarm-consensus (safe-symbol-value 'swimmy.globals::*last-swarm-consensus*))
         (direction (or last-prediction :hold))
         (flood-status (get-flood-status)))

    (notify-discord-daily (format nil "
📜 **日刊・システムレポート**
━━━━━━━━━━━━━━━━━━━━━━━━━━
💰 日次PnL: ¥~a
📈 連勝/連敗: ~a / ~a
🧾 取引数: ~a
⚙️ システム状態: ~a (Trading: ~a)
🧠 最終シグナル: ~a (信頼度 ~a)
🧭 レジーム: ~a / ~a
📊 合意率: Swarm ~a

🌊 **洪水警報 (Risk Level)**:
~a
🛡️ Risk: Danger ~a / MaxDD ~a / DynDD ~a
💹 Equity: ¥~a (Peak ¥~a)
━━━━━━━━━━━━━━━━━━━━━━━━━━
" (format-value pnl "~,0f")
  (format-value wins "~d")
  (format-value losses "~d")
  (format-value trades "~d")
  (format-value system-state "~a")
  (format-value trading-enabled "~a")
  (format-value direction "~a")
  (format-percent last-confidence)
  (format-value current-regime "~a")
  (format-value volatility-regime "~a")
  (format-percent swarm-consensus)
  flood-status
  (format-value danger-level "~d")
  (format-value max-dd "~,1f%")
  (format-value monitor-dd "~,1f%")
  (format-value equity "~,0f")
  (format-value peak-equity "~,0f"))
     :color (cond ((>= (if (boundp '*danger-level*) *danger-level* 0) 3) 15158332) ; Red
                  ((>= (if (boundp '*danger-level*) *danger-level* 0) 1) 16776960) ; Yellow
                  (t 3447003))))) ; Blue/Green
