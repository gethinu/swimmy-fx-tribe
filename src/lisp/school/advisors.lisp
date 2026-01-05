;;; src/lisp/school/advisors.lisp
;;; =======================================================
;;; THE ADVISOR COUNCIL (Unified Module)
;;; =======================================================
;;; Taleb (Risk), Naval (Simplicity), Graham (Value)
;;; Consolidated per Musk's Order (2026-01-04)


;;; -------------------------------------------------------
;;; 1. NASSIM TALEB (Risk & Anti-Fragility)
;;; -------------------------------------------------------

(defun generate-taleb-report ()
  "Generate Taleb's Risk Analysis Report and enforce Circuit Breakers."
  (let* ((exposure (if (boundp '*max-total-exposure*) *max-total-exposure* 0))
         (danger (if (boundp '*danger-level*) *danger-level* 0))
         (loss-streak (if (boundp '*consecutive-losses*) *consecutive-losses* 0))
         ;; Fragility Score
         (fragility-score (+ (* exposure 2) (* danger 10) (* loss-streak 5)))
         (status (cond ((< fragility-score 30) "Robust (Anti-Fragile)")
                       ((< fragility-score 60) "Fragile")
                       (t "🚨 CRITICAL ( Turkey? )"))))
    
    ;; CIRCUIT BREAKER (Musk/Taleb Request)
    (when (>= fragility-score 60)
      (format t "[TALEB] 🛑 CIRCUIT BREAKER TRIGGERED! Fragility: ~d. Disabling Trading.~%" fragility-score)
      (when (boundp '*trading-enabled*)
        (setf *trading-enabled* nil)))
    
    (format nil "🛡️ **Taleb's Risk Report**~%~
                 - Condition: ~a~%~
                 - Fragility Score: ~d~%~
                 - Danger Level: ~d/10~%~
                 - Loss Streak: ~d~%~
                 - Circuit Breaker: ~a~%~
                 - Message: 'The market is a theater of destruction. Respect the tails.'"
            status fragility-score danger loss-streak
            (if (and (boundp '*trading-enabled*) (not *trading-enabled*)) "ACTIVE (Trading Halted)" "Inactive"))))

;;; -------------------------------------------------------
;;; 2. NAVAL RAVIKANT (Simplicity & Subtraction)
;;; -------------------------------------------------------

(defun generate-naval-report ()
  "Generate Naval Ravikant's Simplicity Report."
  (let ((total-strats (if (boundp '*evolved-strategies*) (length *evolved-strategies*) 0))
        (zombies 0)
        (candidates nil)
        (now (get-universal-time)))
    
    ;; Analyze Zombies
    (when (boundp '*evolved-strategies*)
      (dolist (strat *evolved-strategies*)
        (let* ((name (strategy-name strat))
               (stats (if (boundp '*strategy-usage-stats*) (gethash name *strategy-usage-stats*) nil))
               (last-trade (if stats (getf stats :last-trade) 0)))
          (when (or (zerop last-trade) (> (- now last-trade) 604800)) ; 7 days
            (incf zombies)
            (push name candidates)))))
    
    (setf candidates (sort candidates #'string<))
    
    (format nil "🧘 **Naval's Simplicity Report**~%~
                 - Total Strategies: ~d~%~
                 - 🧟 Zombies (>7d inactive): ~d~%~
                 - ✂️ Recommended Deletions: ~{~a~^, ~}~%~
                 - Message: 'Wealth is what you don't spend. Code is what you don't write.'"
            total-strats zombies (if candidates (subseq candidates 0 (min 5 (length candidates))) '("None")))))

;;; -------------------------------------------------------
;;; 3. BENJAMIN GRAHAM (Value & Safety)
;;; -------------------------------------------------------

(defun generate-graham-report ()
  "Generate Graham's Value Analysis Report."
  (let* ((equity (if (boundp '*current-equity*) *current-equity* 100000.0))
         (locked (if (boundp '*locked-treasury*) *locked-treasury* 0.0))
         (pnl (if (boundp '*daily-pnl*) *daily-pnl* 0.0))
         (margin (if (> equity 0) (/ locked equity) 0.0))
         (safety-status (cond ((> margin 0.5) "Fortress")
                              ((> margin 0.2) "Secure")
                              ((> margin 0.05) "Building...")
                              (t "Vulnerable"))))
    
    (format nil "💰 **Graham's Value Report**~%~
                 - Status: ~a~%~
                 - Current Equity: ¥~,0f~%~
                 - Locked Treasury: ¥~,0f~%~
                 - Margin of Safety: ~,1f%~%~
                 - Today's PnL: ¥~,0f~%~
                 - Message: 'Rule No.1: Never lose money. Rule No.2: Never forget Rule No.1.'"
            safety-status equity locked (* margin 100) pnl)))

;;; -------------------------------------------------------
;;; 4. CORE ORCHESTRATOR
;;; -------------------------------------------------------

(defun generate-advisor-reports ()
  "Generate and publish all advisor reports."
  (let ((taleb (if (fboundp 'generate-taleb-report) (generate-taleb-report) "Taleb: Absent"))
        (naval (if (fboundp 'generate-naval-report) (generate-naval-report) "Naval: Absent"))
        (graham (if (fboundp 'generate-graham-report) (generate-graham-report) "Graham: Absent")))
    
    (format nil "~a~%~%~a~%~%~a" taleb naval graham)))

(defun run-advisor-council ()
  "Run the council and notify Discord."
  (let ((report (generate-advisor-reports)))
    (if (fboundp 'notify-discord-daily)
        (funcall 'notify-discord-daily report :title "🧐 Advisor Council Report")
        (format t "DEBUG OUTPUT:~%~a~%" report))
    (format t "[ADVISORS] Council report generated and sent.~%")))

(format t "[SCHOOL] 🏛️ Advisor Council (Unified) loaded~%")
