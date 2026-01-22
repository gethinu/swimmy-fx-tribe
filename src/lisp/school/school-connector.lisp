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

(defun phase-1-optimize ()
  "Run Evolutionary Optimization (Legacy Python removed)"
  ;; (run-command '("python3" "tools/optimize_strategies.py"))
  (format t "[CONNECTOR] ⏩ Skipping Phase 1 (Python Legacy)...~%"))

(defun phase-2-patch ()
  "Apply Optimized Genes (Legacy Python removed)"
  ;; (run-command '("python3" "tools/generate_lisp_params.py"))
  (format t "[CONNECTOR] ⏩ Skipping Phase 2 (Python Legacy)...~%")
  t)

(defun phase-3-qualify ()
  "Run WFV Qualification (Native Lisp Loop)"
  (format t "[CONNECTOR] [Phase 3] Qualification (Native Backtest Loop)...~%")
  (when (fboundp 'run-qualification-cycle)
    (run-qualification-cycle)))

(defun phase-4-purge ()
  "Run The Selector (Native Lisp Battle Royale)"
  (format t "[CONNECTOR] [Phase 4] The Proving Grounds (Native)...~%")
  (execute-proving-grounds))

;; P8: phase-5-recruit (Scout) DELETED

(defun phase-6-breeding ()
  "Evolution (Breeding & Selection)"
  (format t "~%[CONNECTOR] [Phase 6] Evolution (Native Breeding)...~%")
  ;; V24: Directly call the native breeder
  (run-breeding-cycle))

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
(defconstant +report-interval+ (* 20 60)) ; 20 Minutes (V47.9: Request by Owner)

(defun phase-7-report ()
  "Send report if interval passed"
  (let ((now (get-universal-time)))
    (when (> (- now *last-report-time*) +report-interval+)
      (format t "[CONNECTOR] 📨 Sending Scheduled Evolution Report...~%")
      (swimmy.school::notify-evolution-report)
      (setf *last-report-time* now))))

;; P11: Weekly KB Pruning
(defparameter *last-prune-time* 0)
(defconstant +prune-interval+ (* 7 24 3600)) ; 7 Days (Weekly)

(defun phase-8-weekly-prune ()
  "P11: Run KB pruning weekly to maintain optimal KB size.
   Target: 18,349 → 5,000 strategies"
  (let ((now (get-universal-time)))
    (when (> (- now *last-prune-time*) +prune-interval+)
      (format t "[CONNECTOR] 🗑️ Running Weekly KB Pruning...~%")
      (handler-case
          (let ((removed (run-kb-pruning)))
            (format t "[CONNECTOR] ✅ Pruning complete: ~d strategies removed~%" removed)
            (setf *last-prune-time* now))
        (error (e)
          (format t "[CONNECTOR] ❌ Pruning error: ~a~%" e))))))



(defun start-evolution-service ()
  "Main Loop: The Connector"
  (format t "♾️  STARTING EVOLUTION SERVICE (Lisp-Native Orchestration) ♾️~%")
  (loop
    (format t "~%--- 🕰️ Cycle Start [Time: ~a] ---~%" (swimmy.core:get-time-string))
    
    ;; 1. Optimize
    (phase-1-optimize)
    
    ;; 2. Patch
    (phase-2-patch)
    
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
    
    (format t "~%--- ✅ Cycle Complete ---~%")
    ;; Simple sleep to prevent CPU burn if loop is too fast (though backtests take time)
    (sleep 1)))
