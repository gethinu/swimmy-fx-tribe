;;; src/lisp/school/school-connector.lisp
;;; ============================================================================
;;; THE CONNECTOR (Phase 12)
;;; ============================================================================
;;; Replaces `run_evolution_v2.py` with native Lisp orchestration.
;;; Implements the main evolution loop as a persistent service.

(in-package :swimmy.school)

(defun run-command (cmd)
  "Run a shell command synchronously."
  (format t "~%[CONNECTOR] 🐚 Executing: ~{~a~^ ~}~%" cmd)
  (let ((exit-code (sb-ext:process-exit-code
                    (sb-ext:run-program (first cmd) (rest cmd)
                                        :search t
                                        :output *standard-output*
                                        :error *error-output*
                                        :wait t))))
    (if (zerop exit-code)
        (progn (format t "[CONNECTOR] ✅ Command succesful.~%") t)
        (progn (format t "[CONNECTOR] ❌ Command failed (RC=~a).~%" exit-code) nil))))

(defun phase-1-validation ()
  "Phase 1: Validation Batch (Formerly RR/Optimize)
   Running native Lisp qualification cycle for scout/incubator strategies."
  (format t "[CONNECTOR] [Phase 1] Validation Batch (Lisp Native)...~%")
  (when (fboundp 'run-qualification-cycle)
    (run-qualification-cycle)))

;; Phase 2 (Legacy Python Patch) is deprecated and removed.
;; Logic is now handled dynamically by the evolution loop.


(defparameter *last-wfv-qualify-time* 0)

(defun maybe-run-wfv-qualification ()
  (let ((now (get-universal-time)))
    (unless (and (boundp 'swimmy.core::*wfv-enabled*) swimmy.core::*wfv-enabled*)
      (format t "[CONNECTOR] [Phase 3] WFV Qualification disabled.~%")
      (return-from maybe-run-wfv-qualification nil))
    (when (< (- now *last-wfv-qualify-time*) swimmy.core::*wfv-interval-sec*)
      (return-from maybe-run-wfv-qualification nil))
    (multiple-value-bind (pending-count _oldest-age) (wfv-pending-stats :now now)
      (declare (ignore _oldest-age))
      (when (>= pending-count swimmy.core::*wfv-max-pending*)
        (return-from maybe-run-wfv-qualification nil))
      (let* ((max-per-run (or swimmy.core::*wfv-max-per-run* 0))
             (candidates (and (fboundp 'get-top-strategies)
                              (get-top-strategies max-per-run)))
             (started 0))
        (when (<= max-per-run 0)
          (return-from maybe-run-wfv-qualification nil))
        (dolist (strat candidates)
          (when (and strat (strategy-name strat)
                     (not (gethash (strategy-name strat) *wfv-pending-strategies*)))
            (start-walk-forward-validation strat)
            (incf started)
            (when (>= started max-per-run)
              (return))))
        (when (> started 0)
          (setf *last-wfv-qualify-time* now))
        started))))

(defun phase-3-qualify ()
  "Run WFV Qualification (Native Lisp Loop)"
  (maybe-run-wfv-qualification))

(defun phase-4-purge ()
  "Run The Selector (Native Lisp Battle Royale)
   V49.0: Switch to Rank System (run-rank-evaluation).
   Legacy 'execute-proving-grounds' replaced."
  (format t "[CONNECTOR] [Phase 4] The Proving Grounds (Rank System V49)...~%")
  ;; V49.0: Fix A-Rank 0 issue by running the correct evaluation logic
  (when (fboundp 'run-rank-evaluation)
    (run-rank-evaluation)))

;; P8: phase-5-recruit (Scout) DELETED

(defun phase-6-breeding ()
  "Evolution (Breeding & Selection)"
  (format t "~%[CONNECTOR] [Phase 6] Evolution (Native Breeding)...~%")
  ;; V24: Directly call the native breeder
  (run-breeding-cycle)
  ;; V48.7: Integrate Legend Breeding (Owner's Request)
  (when (fboundp 'run-legend-breeding)
    (run-legend-breeding)))

(defun phase-3-5-cpcv-validate ()
  "V48.0: CPCV Validation Phase - Promote A-RANK → S-RANK via CPCV"
  (format t "~%[CONNECTOR] [Phase 3.5] CPCV Validation...~%")
  (handler-case
      (run-a-rank-cpcv-batch)
    (error (e)
      (format t "[CONNECTOR] ⚠️ CPCV error: ~a~%" e))))

(defun phase-7-wisdom-update ()
  "Wisdom Update (Civilization Handover)"
  ;; V24: Native Lisp Wisdom Extraction
  (format t "~%[CONNECTOR] [Phase 7] Wisdom Update (Native)...~%")
  (analyze-veterans))

;; V48: Throttled Reporting (Prevents Spam)
(defparameter *last-report-time* 0)
(defconstant +report-interval+ (* 60 60)) ; 1 Hour (V48.5: Increased to reduce noise)

(defun phase-7-report ()
  "Send report if interval passed"
  (let ((now (get-universal-time)))
    (if (> (- now *last-report-time*) +report-interval+)
        (progn
          (setf *last-report-time* now) ;; Claim execution first
          (format t "[CONNECTOR] 📨 Sending Scheduled Evolution Report...~%")
          (swimmy.school::notify-evolution-report)
          (when (fboundp 'swimmy.school::write-oos-status-file)
            (ignore-errors (swimmy.school::write-oos-status-file :reason "scheduled"))))
        (when (= (mod now 60) 0) ;; Log every minute only
          (format t "[CONNECTOR] ⏳ Report cooldown: ~ds remaining~%" 
                  (- +report-interval+ (- now *last-report-time*)))))))

;; P11: KB Pruning every 6 hours (V48.0: Changed from weekly)
(defparameter *last-prune-time* 0)
(defconstant +prune-interval+ (* 1 3600)) ;; 1 Hour (V49.6: Aggressive Acceleration)

(defun phase-8-weekly-prune ()
  "P11: Run KB pruning weekly to maintain optimal KB size.
   Target: 18,349 → 5,000 strategies"
  (let ((now (get-universal-time)))
    (when (> (- now *last-prune-time*) +prune-interval+)
      (setf *last-prune-time* now) ;; Claim execution first
      (format t "[CONNECTOR] 🗑️ Running Weekly KB Pruning...~%")
      (handler-case
          (let ((removed (run-kb-pruning)))
            (format t "[CONNECTOR] ✅ Pruning complete: ~d strategies removed~%" removed))
        (error (e)
          (format t "[CONNECTOR] ❌ Pruning error: ~a~%" e))))))

(defun check-notifier-health ()
  "V48.2 Expert Panel (Taleb/Kim): Watchdog for the Discord Notifier.
   If no message has been successful for 15 minutes, log a critical local warning."
  (let* ((now (get-universal-time))
         (elapsed (- now *last-discord-notification-time*))
         (threshold (* 15 60))) ;; 15 Minutes (Musk Order)
    (when (and (> elapsed threshold) (> *last-discord-notification-time* 0))
      (format t "~%[WATCHDOG] 🚨 CRITICAL: Discord Notifier SILENT for ~d minutes!~%" 
              (floor elapsed 60))
      (format t "[WATCHDOG] 🛡️ Possible service crash or network failure. Verifying systemd...~%")
      ;; We don't auto-restart yet (Taleb: "Complexity is death"), but we alert in logs.
      )))



(defun start-evolution-service ()
  "Main Loop: The Connector"
  (format t "♾️  STARTING EVOLUTION SERVICE (Lisp-Native Orchestration) ♾️~%")
  (loop
    (format t "~%--- 🕰️ Cycle Start [Time: ~a] ---~%" (swimmy.core:get-time-string))

    ;; Sync metrics/ranks from DB for multi-process coherence
    (when (fboundp 'refresh-strategy-metrics-from-db)
      (refresh-strategy-metrics-from-db))
    
    ;; 1. Validation Batch (Formerly RR/Optimize)
    (phase-1-validation)
    
    ;; 2. [REMOVED] Legacy Python Phase 2
    ;; Logic absorbed into Evolution Engine

    
    ;; 3. Qualify
    (phase-3-qualify)
    
    ;; 3.5 V48.0: CPCV Validation (A-RANK → S-RANK)
    (phase-3-5-cpcv-validate)
    
    ;; 4. Purge
    (phase-4-purge)
    
    ;; 5. Recruit
    ;; (phase-5-recruit)
    
    ;; 6. Breeding
    (phase-6-breeding)
    
    ;; 7. Wisdom & Reporting
    (phase-7-wisdom-update)
    (phase-7-report)
    
    ;; 8. Weekly KB Pruning (P11)
    (phase-8-weekly-prune)
    
    ;; 9. V48.2 Expert Panel: Heartbeat 2.0 (Watcher)
    (check-notifier-health)
    
    ;; V50.3 Expert Panel 2: Honest Heartbeat (File Touch)
    (update-heartbeat-file)

    ;; V50.4: User Request "Cycle Complete" Notification
    (notify-cycle-complete)
    
    ;; V49.5: Flush stagnant notification buffers (Expert Panel)
    (swimmy.core:check-timeout-flushes)

    ;; V50.10: Periodic flush for backtest queue (requester may connect mid-cycle)
    (when (fboundp 'swimmy.school::maybe-flush-backtest-send-queue)
      (swimmy.school::maybe-flush-backtest-send-queue))
    
    (format t "~%--- ✅ Cycle Complete ---~%")
    ;; Simple sleep to prevent CPU burn if loop is too fast (though backtests take time)
    (sleep 1)))

(defun update-heartbeat-file ()
  "Touch the heartbeat file to prove the loop is spinning."
  (let ((path "data/heartbeat/school.tick"))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out "~d" (get-universal-time)))))

(defparameter *last-cycle-notify-time* 0)
(defconstant +cycle-notify-interval* 300) ; 5 Minutes (Prevent Spam)

(defun notify-cycle-complete ()
  "Notify Discord of Cycle Completion (Throttled)"
  (let ((now (get-universal-time)))
    (when (> (- now *last-cycle-notify-time*) +cycle-notify-interval*)
      (setf *last-cycle-notify-time* now)
      (swimmy.core:queue-discord-notification 
       swimmy.globals:*status-webhook-url*
       "🫀 **System Pulse (5m)**\nHeartbeat only. Backtests may still be running."
       :color 10070709
       :title "♻️ System Pulse"))))
