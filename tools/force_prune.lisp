(in-package :swimmy.school)

(format t "🧹 FORCE PRUNING STARTED...~%")

;; 1. Check current size
(format t "Current KB Size: ~d~%" (length *strategy-knowledge-base*))

;; 2. Run Pruning
(run-kb-pruning)

;; 3. Check new size
(format t "New KB Size: ~d~%" (length *strategy-knowledge-base*))

(uiop:quit)
