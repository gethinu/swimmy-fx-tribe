(in-package :swimmy.school)

;;; ==========================================
;;; SWIMMY CORE: RITUALS (rituals.lisp)
;;; ==========================================
;;; Contains: Morning Ritual, Coming of Age, Funeral Rites
;;; Extracted from brain-ritual.lisp (Strangler Fig Phase 4)

(defun morning-ritual ()
  "Morning Ritual - Recite the Constitution and gather the system"
  (format t "~%")
  (format t "[L] ═══════════════════════════════════════~%")
  (format t "[L] 🌅 MORNING RITUAL - The Clans Gather~%")
  (format t "[L] ═══════════════════════════════════════~%")
  
  ;; Recite the Constitution
  (format t "~%[L] 📜 CONSTITUTION RECITATION:~%")
  (format t "[L] 「勝とうとするな。ただ、生き残れ。~%")
  (format t "[L]  そうすれば、最後に立っているのは我々だ。」~%~%")
  
  ;; Initialize Constitution if not done
  (unless *constitution*
    (initialize-constitution))
  
  ;; Initialize tribal dialect
  (if (fboundp 'initialize-tribal-dialect)
      (initialize-tribal-dialect))
  
  (format t "~%[L] 🎯 TODAY'S MISSION: ~a~%"
          (if (> *monthly-goal* 0)
              (format nil "月間目標 ¥~:d への道" *monthly-goal*)
              "生き残ること"))
  
  ;; V3.0: PM Feedback - Show KPIs clearly
  (format t "[L] 📈 現在の累積PnL: ¥~:d / 目標 ¥~:d (~,1f%)~%"
          (floor *accumulated-pnl*)
          *monthly-goal*
          (if (> *monthly-goal* 0)
              (* 100 (/ *accumulated-pnl* *monthly-goal*))
              0))
  
  ;; Calculate and show win rate from failure history
  (let* ((total-trades (+ (length *failure-history*) 
                          (if (boundp '*success-count*) *success-count* 0)))
         (losses (length *failure-history*))
         (wins (- total-trades losses))
         (win-rate (if (> total-trades 0) (* 100 (/ wins total-trades)) 0)))
    (when (> total-trades 0)
      (format t "[L] 🏆 勝率: ~,1f% (~d勝 / ~d敗)~%"
              win-rate wins losses)
      ;; V3.0: Update win rate history for trend (PM feedback)
      (push (list (get-universal-time) win-rate) *win-rate-history*)
      (when (> (length *win-rate-history*) *max-win-rate-history*)
        (setf *win-rate-history* (subseq *win-rate-history* 0 *max-win-rate-history*)))
      ;; Show trend if we have history
      (when (>= (length *win-rate-history*) 2)
        (let* ((current (second (first *win-rate-history*)))
               (previous (second (second *win-rate-history*)))
               (diff (- current previous))
               (trend-emoji (cond ((> diff 5) "📈") ((< diff -5) "📉") (t "➡️"))))
          (format t "[L] ~a 勝率トレンド: ~,1f%~%" trend-emoji diff)))))
  
  ;; V3.0: Display failure learning summary
  (handler-case
      (when (fboundp 'get-failure-summary)
        (let ((summary (get-failure-summary)))
          (when (and summary (> (length summary) 0))
            (format t "[L] 📊 LEARNING SUMMARY: ~a~%" summary))))
    (error (e) (format t "[L] Learning summary error: ~a~%" e)))
  
  ;; V3.0: Hour patterns analysis
  (handler-case
      (when (fboundp 'get-hour-patterns)
        (let ((patterns (get-hour-patterns)))
          (when patterns
            (format t "[L] 🕐 HOUR PATTERNS: ~a~%" patterns))))
    (error (e) (format t "[L] Hour patterns error: ~a~%" e)))
  
  ;; V3.0: Swarm accuracy report
  (handler-case
      (when (fboundp 'analyze-swarm-accuracy)
        (let ((accuracy (analyze-swarm-accuracy)))
          (when accuracy
            (format t "[L] 🐟 SWARM ACCURACY: ~a~%" accuracy))))
    (error (e) (format t "[L] Swarm accuracy error: ~a~%" e)))
  
  ;; V7.0: Metabolism - The Efficient Gardener Life Cycle
  (handler-case
      (when (fboundp 'run-metabolism)
        (run-metabolism))
    (error (e) (format t "[L] Metabolism error: ~a~%" e)))

  ;; V50.2: Phase 21 - The Evolution Factory (Breeding Cycle)
  (handler-case
      (when (fboundp 'process-breeding-cycle)
        (process-breeding-cycle))
    (error (e) (format t "[L] Breeding Cycle error: ~a~%" e)))
  
  (format t "[L] ═══════════════════════════════════════~%~%"))

(defun coming-of-age (strategy-name old-status new-status)
  "Coming of Age ceremony - when a strategy is promoted"
  (format t "~%")
  (format t "[L] ═══════════════════════════════════════~%")
  (format t "[L] 🎊 COMING OF AGE CEREMONY~%")
  (format t "[L] ═══════════════════════════════════════~%")
  (format t "[L] 🌟 ~a has proven worthy!~%" strategy-name)
  (format t "[L] 📜 Status: ~a → ~a~%" old-status new-status)
  (format t "[L] 🎺 The system celebrates!~%")
  (format t "[L] ═══════════════════════════════════════~%~%"))

(defun hold-funeral (strategy-name final-pnl lessons-learned)
  "Funeral ceremony - honor fallen strategies and learn from them"
  (format t "~%")
  (format t "[L] ═══════════════════════════════════════~%")
  (format t "[L] ⚰️ FUNERAL RITES~%")
  (format t "[L] ═══════════════════════════════════════~%")
  (format t "[L] 🕯️ We honor the fallen: ~a~%" strategy-name)
  (format t "[L] 📊 Final record: ¥~,2f~%" final-pnl)
  (format t "[L] 📖 Lessons learned: ~a~%" lessons-learned)
  (format t "[L] 🙏 May the wisdom live on in the system.~%")
  (format t "[L] ═══════════════════════════════════════~%~%")
  
  ;; V44.11: Close all positions for the deceased (Fix Ghost Positions)
  (when (fboundp 'force-close-strategy-positions)
    (handler-case 
        (force-close-strategy-positions strategy-name)
      (error (e) (format t "[L] ⚠️ Failed to close positions: ~a~%" e))))

  ;; V42.0: Naval Ravikant's Auto-Funeral (File Cleanup request)
  (handler-case
      (with-open-file (out "data/graveyard.txt" 
                           :direction :output 
                           :if-exists :append 
                           :if-does-not-exist :create)
        (write-line strategy-name out))
    (error (e)
      (format t "[L] ⚠️ Failed to issue death certificate: ~a~%" e))))
