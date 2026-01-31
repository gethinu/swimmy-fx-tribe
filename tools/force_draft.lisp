;; tools/force_draft.lisp
;; EMERGENCY SCRIPT: Manually populate S-Rank from Incubator
;; Triggered by Expert Panel "Laxative" Order.

(require :sb-posix)

;; Load Quicklisp & System
(let ((ql-setup "~/quicklisp/setup.lisp"))
  (if (probe-file ql-setup)
      (load ql-setup)
      (format t "⚠️ Quicklisp not found at ~a~%" ql-setup)))

(load "swimmy.asd")
(ql:quickload :swimmy :silent t)

(in-package :swimmy.school)

;; 1. Load ALL Incubator strategies (might take a moment)
(format t "[DRAFT] 📂 Loading Incubator (1200+ files)...~%")
(setf *strategy-knowledge-base* (swimmy.persistence:load-all-strategies))

(format t "[DRAFT] 📊 Loaded ~d strategies total.~%" (length *strategy-knowledge-base*))

(let ((incubators (remove-if-not (lambda (s)
                                   (or (null (strategy-rank s))
                                       (eq (strategy-rank s) :incubator)))
                                 *strategy-knowledge-base*))
      (promoted-count 0))
  
  (format t "[DRAFT] 🥚 Found ~d Incubator strategies.~%" (length incubators))

  ;; 2. Find Legends and promote them to :legend tier immediately
  (dolist (s incubators)
    (when (search "Legendary" (strategy-name s))
      (format t "[DRAFT] 👑 Found Legend: ~a. Promoting to LEGEND.~%" (strategy-name s))
      (ensure-rank s :legend "Force Draft Legend")
      (incf promoted-count)))
  
  ;; 3. Scan for "Top Talent" (Sharpe > 0.0 or just top 4)
  ;; Since we applied Laxative, we just want to fill the battlefield.
  ;; Group by category
  (dolist (cat '(:trend :reversion :breakout :scalp))
    (let* ((candidates (remove-if-not (lambda (s) (eq (strategy-category s) cat)) incubators))
           ;; Sort by Sharpe (descending), treat nil as -999
           (sorted (sort (copy-list candidates) #'> :key (lambda (s) (or (strategy-sharpe s) -999.0))))
           (top-picks (subseq sorted 0 (min 4 (length sorted)))))
      
      (format t "[DRAFT] ⚔️ Drafting for ~a... (Pool: ~d)~%" cat (length candidates))
      
      (dolist (pick top-picks)
        ;; Only promote if not already handled
        (when (or (null (strategy-rank pick)) (eq (strategy-rank pick) :incubator))
           ;; Check if it has AT LEAST non-negative sharpe? Or just forced?
           ;; Expert Panel said "Force Draft". So JUST DO IT.
           (format t "[DRAFT] 🎖️ Drafting ~a (Sharpe: ~a) to S-RANK.~%" 
                   (strategy-name pick) (strategy-sharpe pick))
           (ensure-rank pick :S "Force Draft S-Rank")
           (incf promoted-count)))))

  (format t "[DRAFT] ✅ Operation Complete. Promoted ~d strategies.~%" promoted-count))

(sb-ext:exit)
