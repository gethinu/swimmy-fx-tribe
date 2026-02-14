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
;;; 0. AUTO MAINTENANCE (Naval: Subtraction -> Action)
;;; -------------------------------------------------------

(defparameter *advisor-maintenance-enabled* t
  "When T, Advisor Council will run daily auto-maintenance before sending the report.")

(defparameter *advisor-maintenance-dry-run* nil
  "When T, do not mutate state; only report what would be done.")

(defparameter *advisor-zombie-threshold-seconds* 604800
  "Seconds since last trade/update to consider a strategy a zombie (default: 7d).")

(defparameter *advisor-zombie-min-trades* 6
  "Minimum trade evidence required before a strategy can be retired as a zombie.")

(defparameter *advisor-zombie-max-actions* 25
  "Safety cap for zombie retire actions per day.")

(defparameter *advisor-zombie-protected-ranks* '(:S :A :legend)
  "Ranks protected from zombie retirement.")

(defvar *advisor-maintenance-last-day* nil
  "YYYYMMDD of last executed advisor maintenance sweep (localtime).")

(defun %advisor-day-key (&optional (now (get-universal-time)))
  (multiple-value-bind (s m h date month year) (decode-universal-time now)
    (declare (ignore s m h))
    (+ (* year 10000) (* month 100) date)))

(defun advisor-zombie-candidates (&key (now (get-universal-time))
                                       (limit *advisor-zombie-max-actions*))
  "Return zombie strategy objects (housekeeping candidates) based on last trade/update age.

LIMIT controls the maximum number of returned strategies. When NIL, returns all zombies (uncapped)."
  (let ((candidates nil))
    (dolist (strat (copy-list *strategy-knowledge-base*))
      (when strat
        (let* ((rank (strategy-rank strat))
               (trades (or (strategy-trades strat) 0))
               (last (or (strategy-last-update strat) 0))
               (age (and (numberp last) (> last 0) (- now last))))
          (when (and (eq (strategy-status strat) :active)
                     (not (strategy-immortal strat))
                     (not (member rank *advisor-zombie-protected-ranks* :test #'eq))
                     (>= trades *advisor-zombie-min-trades*)
                     age
                     (> age *advisor-zombie-threshold-seconds*))
            (push strat candidates)))))
    ;; Oldest first (most stale)
    (setf candidates (sort candidates #'>
                           :key (lambda (s)
                                  (- now (or (strategy-last-update s) 0)))))
    (if (and (numberp limit) (> limit 0))
        (subseq candidates 0 (min (length candidates) limit))
        candidates)))

(defun advisor-low-sharpe-candidate-p (strat)
  "Return T when STRAT would be graveyarded by prune-low-sharpe-strategies."
  (let ((sharpe (or (strategy-sharpe strat) 0.0))
        (rank (strategy-rank strat)))
    (and (not (newborn-protected-p strat))
         (< sharpe *prune-sharpe-threshold*)
         (not (member rank *prune-protected-ranks*))
         t)))

(defun run-advisor-auto-maintenance (&key (now (get-universal-time)))
  "Execute (or report) daily auto-maintenance. Returns a human-readable summary string."
  (unless *advisor-maintenance-enabled*
    (return-from run-advisor-auto-maintenance nil))

  (let ((day-key (%advisor-day-key now)))
    (when (and (numberp *advisor-maintenance-last-day*)
               (= *advisor-maintenance-last-day* day-key))
      (return-from run-advisor-auto-maintenance nil))
    (setf *advisor-maintenance-last-day* day-key))

  (let* ((zombies (advisor-zombie-candidates :now now))
         (zombie-names (mapcar #'strategy-name zombies))
         (low-sharpe-cnt (count-if #'advisor-low-sharpe-candidate-p *strategy-knowledge-base*))
         (retired 0)
         (graveyarded 0))

    (unless *advisor-maintenance-dry-run*
      ;; Housekeeping: retire zombies (inactive)
      (dolist (s zombies)
        (ignore-errors
          (send-to-retired s "Auto Maintenance: Zombie (>7d inactive)"))
        (incf retired))

      ;; Objective failure: graveyard low-sharpe laggards
      (setf graveyarded (or (ignore-errors (prune-low-sharpe-strategies)) 0)))

    (format nil "🧹 **Auto Maintenance**~%~
- Mode: ~a~%~
- Housekeeping (Zombies -> Retired): ~d~%~
- Objective Failure (Low Sharpe -> Graveyard): ~d~@[ (would ~d)~]~%~
- Sample Zombies: ~{`~a`~^, ~}"
            (if *advisor-maintenance-dry-run* "DRY-RUN" "EXECUTE")
            (if *advisor-maintenance-dry-run* (length zombies) retired)
            graveyarded
            (and *advisor-maintenance-dry-run* low-sharpe-cnt)
            (if zombie-names
                (subseq zombie-names 0 (min 5 (length zombie-names)))
                '("None")))))

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
  (let* ((now (get-universal-time))
         (total-strats (length *strategy-knowledge-base*))
         ;; Reporting should show the full zombie count; action caps are applied only during execution.
         (zombie-strats (advisor-zombie-candidates :now now :limit nil))
         (zombies (length zombie-strats))
         (candidates (sort (mapcar #'strategy-name zombie-strats) #'string<)))
    
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
  (let* ((maintenance (ignore-errors (run-advisor-auto-maintenance)))
         (report (generate-advisor-reports))
         (final (if (and maintenance (> (length maintenance) 0))
                    (format nil "~a~%~%~a" report maintenance)
                    report)))
    (if (fboundp 'notify-discord-daily)
        (funcall 'notify-discord-daily final :title "🧐 Advisor Council Report")
        (format t "DEBUG OUTPUT:~%~a~%" final))
    (format t "[ADVISORS] Council report generated and sent.~%")))

(format t "[SCHOOL] 🏛️ Advisor Council (Unified) loaded~%")
