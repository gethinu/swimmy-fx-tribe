
(in-package :cl-user)
(require :asdf)

;; Add current directory?
(push (uiop:getcwd) asdf:*central-registry*)

;; Initialize Quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(format t "[EVO] Loading Swimmy System...~%")
;; Silence warnings
(handler-bind ((style-warning #'muffle-warning)
               (warning #'muffle-warning))
  (asdf:load-system :swimmy))

;; Initialize (Load Strategies, Data, etc)
(format t "[EVO] Initializing School & Data...~%")
(swimmy.main::initialize-system)

;; --- The Missing Link Logic ---

;; 0. Recruitment (Phase 11: Lisp-Native Scout)
;; Direct Guardian Pipe Interaction (True Acceleration).
(format t "~%[EVO] 🔭 recruit-elite-strategy (Phase 11 Scout)...~%")
(swimmy.school:recruit-elite-strategy)

;; 1. Promotion (Incubator -> Battlefield)
;; Note: This requires strategies to have Sharpe > 0.5.
;; Recruit Elite Recruits now have proper Sharpe/Trades.
(format t "~%[EVO] 🏟️ executing-proving-grounds (Promotion)...~%")
(swimmy.school:execute-proving-grounds)

;; 2. Breeding (Battlefield -> Incubator)
;; Parents (Legacy 61 + New Elites) breed Gen 2 children.
;; Child is saved to strategies-dynamic.lisp via our call to save-recruit-to-lisp inside breed-strategies.
(format t "~%[EVO] 🧬 run-breeding-cycle (Generational Advance)...~%")
(swimmy.school:run-breeding-cycle)

(format t "~%[EVO] ✅ Breeding Cycle Completed.~%")
(sb-ext:exit :code 0)
