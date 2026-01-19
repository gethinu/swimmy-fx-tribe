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
  "Run WFV Qualification (Legacy Python removed)"
  ;; (run-command '("python3" "tools/run_qualification.py"))
  (format t "[CONNECTOR] ⏩ Skipping Phase 3 (Python Legacy)...~%"))

(defun phase-4-purge ()
  "Run The Selector (Native Lisp Battle Royale)"
  (format t "[CONNECTOR] [Phase 4] The Proving Grounds (Native)...~%")
  (execute-proving-grounds))

(defun phase-5-recruit ()
  "Recruitment (Lisp-Native Scout)"
  (format t "~%[CONNECTOR] [Phase 5] Recruitment (Native Scout)...~%")
  (recruit-scout))

(defun phase-6-breeding ()
  "Evolution (Breeding & Selection)"
  (format t "~%[CONNECTOR] [Phase 6] Evolution (Native Breeding)...~%")
  ;; V24: Directly call the native breeder
  (run-breeding-cycle))

(defun phase-7-wisdom-update ()
  "Wisdom Update (Civilization Handover)"
  ;; V24: Native Lisp Wisdom Extraction
  (format t "~%[CONNECTOR] [Phase 7] Wisdom Update (Native)...~%")
  (analyze-veterans)
  
  ;; V28.2: Send Factory Report (Restores Visibility)
  (format t "[CONNECTOR] 📨 Sending Evolution Factory Report...~%")
  (swimmy.school::notify-evolution-report))

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
    
    ;; 4. Purge
    (phase-4-purge)
    
    ;; 5. Recruit
    (phase-5-recruit)
    
    ;; 6. Breeding
    (phase-6-breeding)
    
    ;; 7. Wisdom
    (phase-7-wisdom-update)
    
    (format t "~%--- ✅ Cycle Complete ---~%")
    ;; Simple sleep to prevent CPU burn if loop is too fast (though backtests take time)
    (sleep 1)))
