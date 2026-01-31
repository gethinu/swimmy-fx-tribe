;;; src/lisp/school/advisors.lisp
;;; =======================================================
;;; THE ADVISOR COUNCIL (Unified Module)
;;; =======================================================
;;; Taleb (Risk), Naval (Simplicity), Paul Graham (Startup), Jim Simons (Quant)
;;; Consolidated per Musk's Order (2026-01-04)

(in-package :swimmy.school)

;; Local defaults for advisor report inputs (override elsewhere as needed)
(defvar *active-users* 1)
(defvar *manual-interventions* 0)
(defvar *signal-count* 0)
(defvar *current-alpha* 0.0)
(defvar *global-win-rate* 0.0)

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
    ;; V44.0: DEPRECATED - Unified into school-danger.lisp (Tiered Cooldown)
    ;; (when (>= fragility-score 60)
    ;;   (format t "[TALEB] 🛑 CIRCUIT BREAKER TRIGGERED! Fragility: ~d. Disabling Trading.~%" fragility-score)
    ;;   (when (boundp '*trading-enabled*)
    ;;     (setf *trading-enabled* nil)))
    
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
;;; 3. PAUL GRAHAM (Startup & Do Things That Don't Scale)
;;; -------------------------------------------------------

(defun generate-pg-report ()
  "Generate Paul Graham's Startup Report."
  (let* ((manual-trades (if (boundp '*manual-interventions*) *manual-interventions* 0))
         (users (if (boundp '*active-users*) *active-users* 1)) ; Always at least 1 (You)
         (message (if (zerop manual-trades)
                      "Automated. Are you talking to users?"
                      "Good. You are doing things that don't scale.")))
    
    (format nil "🦄 **Paul Graham's Startup Report**~%~
                 - Stage: Early Stage~%~
                 - Users: ~d~%~
                 - Manual Ops: ~d~%~
                 - Message: '~a'"
            users manual-trades message)))

;;; -------------------------------------------------------
;;; 4. JIM SIMONS (Quant & Patterns)
;;; -------------------------------------------------------

(defun generate-simons-report ()
  "Generate Jim Simons' Quant Report."
  (let* ((signal-count (if (boundp '*signal-count*) *signal-count* 0))
         (alpha (if (boundp '*current-alpha*) *current-alpha* 0.0))
         (win-rate (if (boundp '*global-win-rate*) *global-win-rate* 0.0))
         (status (cond ((> win-rate 0.55) "Renaissance")
                       ((> win-rate 0.51) "Mediocre")
                       (t "Random Walk"))))
    
    (format nil "📐 **Jim Simons' Quant Report**~%~
                 - Status: ~a~%~
                 - Signal Count: ~d~%~
                 - Win Rate: ~5f%~%~
                 - Alpha: ~5f~%~
                 - Message: 'Patterns exist, but they are faint. Don't override the models.'"
            status signal-count (* win-rate 100) alpha)))

;;; -------------------------------------------------------
;;; 5. CORE ORCHESTRATOR
;;; -------------------------------------------------------

(defun generate-advisor-reports ()
  "Generate and publish all advisor reports."
  (let ((taleb (if (fboundp 'generate-taleb-report) (generate-taleb-report) "Taleb: Absent"))
        (naval (if (fboundp 'generate-naval-report) (generate-naval-report) "Naval: Absent"))
        (pg    (if (fboundp 'generate-pg-report) (generate-pg-report) "PG: Absent"))
        (simons (if (fboundp 'generate-simons-report) (generate-simons-report) "Simons: Absent")))
    
    (format nil "~a~%~%~a~%~%~a~%~%~a" taleb naval pg simons)))

(defun run-advisor-council ()
  "Run the council and notify Discord."
  (let ((report (generate-advisor-reports)))
    (if (fboundp 'notify-discord-daily)
        (funcall 'notify-discord-daily report :title "🧐 Advisor Council Report")
        (format t "DEBUG OUTPUT:~%~a~%" report))
    (format t "[ADVISORS] Council report generated and sent.~%")))

(format t "[SCHOOL] 🏛️ Advisor Council (Unified) loaded~%")
