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
   Running native Lisp qualification cycle for incubator candidates."
  (format t "[CONNECTOR] [Phase 1] Validation Batch (Lisp Native)...~%")
  (when (fboundp 'run-qualification-cycle)
    (run-qualification-cycle)))

;; Phase 2 (Legacy Python Patch) is deprecated and removed.
;; Logic is now handled dynamically by the evolution loop.


(defparameter *last-wfv-qualify-time* 0)

(defvar *last-rank-eval-time* 0
  "Unix timestamp of the last rank evaluation run.")

(defvar *last-breeding-cycle-time* 0
  "Unix timestamp of the last breeding phase run.")

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

(defun phase-4-purge (&key (now (get-universal-time)))
  "Run The Selector (Native Lisp Battle Royale)
   V49.0: Switch to Rank System (run-rank-evaluation).
   Legacy 'execute-proving-grounds' replaced."
  ;; Run expensive A/B/S conformance on interval instead of every 1s cycle.
  (when (and (fboundp 'run-rank-evaluation)
             (should-run-rank-evaluation-p now))
    (format t "[CONNECTOR] [Phase 4] The Proving Grounds (Rank System V49)...~%")
    (setf *last-rank-eval-time* now)
    ;; V49.0: Fix A-Rank 0 issue by running the correct evaluation logic
    (run-rank-evaluation)))

;; P8: phase-5-recruit DELETED

(defun phase-6-breeding (&key (now (get-universal-time)))
  "Evolution (Breeding & Selection)"
  (when (should-run-breeding-cycle-p now)
    (setf *last-breeding-cycle-time* now)
    (format t "~%[CONNECTOR] [Phase 6] Evolution (Native Breeding)...~%")
    ;; V24: Directly call the native breeder
    (run-breeding-cycle)
    ;; V48.7: Integrate Legend Breeding (Owner's Request)
    (when (fboundp 'run-legend-breeding)
      (run-legend-breeding))))

(defun phase-3-5-cpcv-validate ()
  "V48.0: CPCV Validation Phase - Promote A-RANK → S-RANK via CPCV"
  (format t "~%[CONNECTOR] [Phase 3.5] CPCV Validation...~%")
  (handler-case
      (run-a-rank-cpcv-batch)
    (error (e)
      (format t "[CONNECTOR] ⚠️ CPCV error: ~a~%" e))))

(defun %connector-env-int-or (key default)
  "Resolve integer env var with DEFAULT fallback."
  (let ((raw (ignore-errors (uiop:getenv key))))
    (if (and (stringp raw) (> (length raw) 0))
        (let ((parsed (ignore-errors (parse-integer raw :junk-allowed nil))))
          (if (and (integerp parsed) (> parsed 0))
              parsed
              default))
        default)))

(defparameter *rank-eval-interval-sec*
  (%connector-env-int-or "SWIMMY_RANK_EVAL_INTERVAL_SEC" 60)
  "Minimum seconds between rank evaluation runs.")

(defun should-run-rank-evaluation-p (&optional (now (get-universal-time)))
  "Return T when rank evaluation interval has elapsed."
  (let ((interval (max 1 (or *rank-eval-interval-sec* 60))))
    (or (<= *last-rank-eval-time* 0)
        (>= (- now *last-rank-eval-time*) interval))))

(defparameter *breeding-cycle-interval-sec*
  (%connector-env-int-or "SWIMMY_BREEDING_CYCLE_INTERVAL_SEC" 60)
  "Minimum seconds between breeding phase runs.")

(defun should-run-breeding-cycle-p (&optional (now (get-universal-time)))
  "Return T when breeding interval has elapsed."
  (let ((interval (max 1 (or *breeding-cycle-interval-sec* 60))))
    (or (<= *last-breeding-cycle-time* 0)
        (>= (- now *last-breeding-cycle-time*) interval))))

(defparameter *wisdom-update-interval-sec*
  (%connector-env-int-or "SWIMMY_WISDOM_UPDATE_INTERVAL_SEC" 1800)
  "Minimum seconds between heavy wisdom updates.")

(defvar *last-wisdom-update-time* 0
  "Unix timestamp of the last wisdom update.")

(defun should-run-wisdom-update-p (&optional (now (get-universal-time)))
  "Return T when wisdom update interval has elapsed."
  (let ((interval (max 1 (or *wisdom-update-interval-sec* 1800))))
    (or (<= *last-wisdom-update-time* 0)
        (>= (- now *last-wisdom-update-time*) interval))))

(defun phase-7-wisdom-update (&key (now (get-universal-time)))
  "Wisdom Update (Civilization Handover)"
  (when (should-run-wisdom-update-p now)
    ;; V24: Native Lisp Wisdom Extraction (interval-gated for memory safety)
    (setf *last-wisdom-update-time* now)
    (format t "~%[CONNECTOR] [Phase 7] Wisdom Update (Native, interval=~ds)...~%"
            *wisdom-update-interval-sec*)
    (analyze-veterans)))

(defvar *startup-report-snapshot-written* nil
  "Set once after writing a local evolution report snapshot at startup.")

(defparameter *local-report-refresh-interval-sec*
  (%connector-env-int-or "SWIMMY_LOCAL_REPORT_REFRESH_INTERVAL_SEC" 300)
  "Minimum seconds between local evolution report/status refreshes.")

(defvar *last-local-report-refresh-time* 0
  "Unix timestamp of the last local evolution report/status refresh.")

(defun maybe-refresh-local-report-snapshot (&key (now (get-universal-time)) (reason "phase-7-local") (force nil))
  "Refresh local evolution report + status files without sending Discord notifications."
  (let ((interval (max 1 (or *local-report-refresh-interval-sec* 300))))
    (when (or force
              (<= *last-local-report-refresh-time* 0)
              (>= (- now *last-local-report-refresh-time*) interval))
      (when (and (fboundp 'swimmy.school::generate-evolution-report)
                 (fboundp 'swimmy.school::write-evolution-report-files))
        (handler-case
            (let ((report (swimmy.school::generate-evolution-report)))
              (swimmy.school::write-evolution-report-files report)
              (when (fboundp 'swimmy.school::write-oos-status-file)
                (ignore-errors (swimmy.school::write-oos-status-file :reason reason)))
              (when (fboundp 'swimmy.school::write-forward-status-file)
                (ignore-errors (swimmy.school::write-forward-status-file :reason reason)))
              (setf *last-local-report-refresh-time* now)
              (format t "[CONNECTOR] 📝 Local report snapshot refreshed (~a).~%" reason)
              t)
          (error (e)
            (format t "[CONNECTOR] ⚠️ Local report snapshot failed: ~a~%" e)
            nil))))))

(defun maybe-write-startup-report-snapshot ()
  "Refresh local report file once at startup to avoid stale dashboards after restarts."
  (unless *startup-report-snapshot-written*
    (when (maybe-refresh-local-report-snapshot :reason "startup" :force t)
      (setf *startup-report-snapshot-written* t)
      (format t "[CONNECTOR] 📝 Startup report snapshot refreshed.~%"))))

(defun phase-7-report ()
  "Send report if interval passed"
  (maybe-write-startup-report-snapshot)
  (maybe-refresh-local-report-snapshot :reason "phase-7-local")
  (when (fboundp 'swimmy.school::maybe-send-evolution-report)
    (swimmy.school::maybe-send-evolution-report :reason "phase-7")))

;; P11: KB Pruning every 6 hours (V48.0: Changed from weekly)
(defparameter *last-prune-time* 0)
(defconstant +prune-interval+ (* 1 3600)) ;; 1 Hour (V49.6: Aggressive Acceleration)

(defun phase-8-weekly-prune ()
  "P11: Run KB pruning weekly to maintain optimal KB size.
   Target: 18,349 → 5,000 strategies"
  (let ((now (get-universal-time)))
    (when (> (- now *last-prune-time*) +prune-interval+)
      (let ((incubator-count (if (fboundp 'get-strategies-by-rank)
                                 (length (or (ignore-errors (get-strategies-by-rank :incubator)) '()))
                                 0)))
        (when (> incubator-count 0)
          (format t "[CONNECTOR] ⏸️ Skipping Weekly KB Pruning (incubator backlog: ~d).~%" incubator-count)
          (return-from phase-8-weekly-prune nil)))
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



;; Independent flush loop for stagnant notification buffers
(defparameter *stagnant-flush-interval* 60)
(defvar *stagnant-flush-thread* nil)
(defvar *stagnant-flush-stop* nil)

(defun run-stagnant-flush-tick ()
  (when (fboundp 'swimmy.core:check-timeout-flushes)
    (swimmy.core:check-timeout-flushes))
  (when (fboundp 'update-heartbeat-file)
    (ignore-errors (update-heartbeat-file))))

(defun start-stagnant-flush-loop (&key (interval *stagnant-flush-interval*))
  (unless (and *stagnant-flush-thread*
               (sb-thread:thread-alive-p *stagnant-flush-thread*))
    (setf *stagnant-flush-stop* nil)
    (setf *stagnant-flush-thread*
          (sb-thread:make-thread
           (lambda ()
             (handler-case
                 (loop until *stagnant-flush-stop* do
                   (run-stagnant-flush-tick)
                   (sleep interval))
               (error (e)
                 (format *error-output* "[CONNECTOR] ⚠️ Stagnant flush loop failed: ~a~%" e))))
           :name "stagnant-flush-loop"))))

(defun start-evolution-service ()
  "Main Loop: The Connector"
  (format t "♾️  STARTING EVOLUTION SERVICE (Lisp-Native Orchestration) ♾️~%")
  (start-stagnant-flush-loop)
  ;; Ensure CMD publisher is available for CPCV/OOS dispatch in this daemon.
  (when (fboundp 'init-external-cmd-zmq)
    (init-external-cmd-zmq))
  (loop
    (format t "~%--- 🕰️ Cycle Start [Time: ~a] ---~%" (swimmy.core:get-time-string))

    ;; Sync metrics/ranks from DB for multi-process coherence
    (when (fboundp 'refresh-strategy-metrics-from-db)
      (format t "[CONNECTOR] 🔄 DB sync start...~%")
      (finish-output)
      (let ((t0 (get-internal-real-time)))
        (refresh-strategy-metrics-from-db)
        (let ((elapsed (/ (- (get-internal-real-time) t0)
                          internal-time-units-per-second)))
          (format t "[CONNECTOR] ✅ DB sync end (~,2fs)~%" elapsed)
          (finish-output))))
    
    ;; 1. Validation Batch (Formerly RR/Optimize)
    (format t "[CONNECTOR] ▶ Phase 1 validation start~%")
    (finish-output)
    (phase-1-validation)
    (format t "[CONNECTOR] ◀ Phase 1 validation end~%")
    (finish-output)
    
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
