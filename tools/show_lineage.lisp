(in-package :cl-user)

;; Load ASDF
(require :asdf)

;; Add current directory to ASDF registry so it finds swimmy.asd
(push (uiop:getcwd) asdf:*central-registry*)

;; Load Swimmy System (Core logic only, no start)
(asdf:load-system :swimmy)

;; Load Strategy Definitions (Static Genome)
;; (This populates *strategy-knowledge-base*)
;; Note: loading swimmy system already loads strategies.lisp

;; Load Dynamic State (Evolved Strategies)
(let ((state-file (merge-pathnames ".swimmy/state.sexp" (user-homedir-pathname))))
  (if (probe-file state-file)
      (with-open-file (in state-file :direction :input)
        (let ((state (read in nil nil)))
          (when state
            (let ((strats (getf state :evolved-strategies)))
              (when strats
                (setf swimmy.school::*evolved-strategies* strats)
                (format t "~%[Lineage] Loaded ~d evolved strategies from state.~%" (length strats)))))))
      (format t "~%[Lineage] No state file found at ~a~%" state-file)))

;; Print Lineage
(swimmy.school:print-lineage)

(sb-ext:exit)
