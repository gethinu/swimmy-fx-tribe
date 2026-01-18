(in-package :swimmy.school)

(defun get-breakout-strategies ()
  (list
      ;; ===== 4. BREAKOUT (ブレイクアウト) =====
      
      ;; 4.1 Bollinger Breakout
      (make-strategy :name "Bollinger-Band-Walk"
        :indicators '((bb 20 1))
        :entry '(> close bb-upper)
        :exit '(< close bb-middle)
        :sl 0.30 :tp 1.00 :volume 0.01
        :timeframe 10080))) ; M15 Walk
