(in-package :swimmy.school)

;;; ==========================================
;;; LEGENDARY WARRIORS (External Strategy Import)
;;; ==========================================
;;; P7: Inject proven strategies to seed the tribe.

(defun create-legend-golden-cross ()
  "Create the Classic Golden Cross Strategy (Trend)"
  (make-strategy
    :name "Legend-Golden-Cross-Classic"
    ;; :symbol "USDJPY" ;; Removed: Not in struct
    :timeframe 3600 ; H1
    :category :trend
    :indicators '((:sma 50) (:sma 200))
    :entry '((:cross-over :sma 50 :sma 200)) ; 50 crosses above 200
    :exit '((:cross-under :sma 50 :sma 200)) ; 50 crosses below 200
    :sl 1.0 ; Wide stop
    :tp 2.0 ; Trend following target
    :volume 0.1
    :generation 0
    :immortal t))

;; [DELETED] London Breakout (Fragile Time-Logic)

(defun create-legend-rsi-reversion ()
  "Create the RSI Reversion Strategy (Raider)"
  (make-strategy
    :name "Legend-RSI-Reversion-V1"
    ;; :symbol "USDJPY"
    :timeframe 300 ; M5 Scalp
    :category :reversion ;; Fixed category enum check if needed
    :indicators '((:rsi 2)) ; Connor's RSI 2
    :entry '((:rsi-below 10)) ; Buy dip
    :exit '((:rsi-above 90))  ; Sell rip
    :sl 0.10 ; 10 pips
    :tp 0.10 ; 10 pips
    :volume 0.05
    :generation 0
    :immortal t))

(defun summon-legends ()
  "Inject legends into the Knowledge Base"
  (format t "[LEGENDS] ⚡ Summoning Legendary Warriors...~%")
  (let ((legends (list (create-legend-golden-cross)
                       ;; (create-legend-london-breakout) ; Pruned
                       (create-legend-rsi-reversion))))
    (dolist (l legends)
      ;; Add to KB if not exists
      (unless (find (strategy-name l) *strategy-knowledge-base* :key #'strategy-name :test #'string=)
        ;; V48.7: Set explicit legend rank
        (setf (strategy-rank l) :legend)
        (push l *strategy-knowledge-base*)
        (format t "[LEGENDS] 🆕 Summoned: ~a (Rank=~a)~%" 
                (strategy-name l) (strategy-rank l))))
    ;; Force save
    (when (fboundp 'save-knowledge-base)
      (save-knowledge-base))
    (format t "[LEGENDS] ✅ Summoning Complete. ~d legends active.~%" (length legends))))

;; Auto-summon on load
(summon-legends)
