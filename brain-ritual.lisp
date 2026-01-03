;; brain-ritual.lisp - Swimmy V4.0 Ritual Module
;; Contains: Morning Ritual, Ceremonies, Trading Day Summary
;; Loaded by: brain-core.lisp

;;; NOTE: This file depends on the following from brain-core.lisp:
;;; - *constitution*, *clans*, *monthly-goal*, *accumulated-pnl*
;;; - *failure-history*, *success-count*, *win-rate-history*
;;; - initialize-constitution, initialize-tribal-dialect

;;; ══════════════════════════════════════════════════════════════════
;;;  TRIBAL RITUALS (儀式)
;;; ══════════════════════════════════════════════════════════════════

(defun morning-ritual ()
  "Morning Ritual - Recite the Constitution and gather the clans"
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
  (initialize-tribal-dialect)
  
  ;; Each clan announces their philosophy
  (format t "[L] 🏛️ THE FOUR GREAT CLANS STAND READY:~%~%")
  (dolist (clan *clans*)
    (format t "[L] ~a ~a (~a)「~a」~%"
            (clan-emoji clan) (clan-name clan) (clan-title clan)
            (clan-philosophy clan)))
  
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
  
  ;; V3.0: Display failure learning summary (previously unused function!)
  (handler-case
      (let ((summary (get-failure-summary)))
        (when (and summary (> (length summary) 0))
          (format t "[L] 📊 LEARNING SUMMARY: ~a~%" summary)))
    (error (e) (format t "[L] Learning summary error: ~a~%" e)))
  
  ;; V3.0: Hour patterns analysis (previously unused!)
  (handler-case
      (let ((patterns (get-hour-patterns)))
        (when patterns
          (format t "[L] 🕐 HOUR PATTERNS: ~a~%" patterns)))
    (error (e) (format t "[L] Hour patterns error: ~a~%" e)))
  
  ;; V3.0: Swarm accuracy report (previously unused!)
  (handler-case
      (let ((accuracy (analyze-swarm-accuracy)))
        (when accuracy
          (format t "[L] 🐟 SWARM ACCURACY: ~a~%" accuracy)))
    (error (e) (format t "[L] Swarm accuracy error: ~a~%" e)))
  
  ;; V3.0: Clan treasury summary (previously unused!)
  (handler-case
      (let ((treasury (get-clan-treasury-summary)))
        (when treasury
          (format t "[L] 💰 TREASURY: ~a~%" treasury)))
    (error (e) (format t "[L] Treasury error: ~a~%" e)))
  
  ;; V4.0: Dreamer - Generate new strategy with Gemini
  (handler-case
      (progn
        (format t "[L] 💭 DREAMER: Generating new strategy...~%")
        (dream-code))
    (error (e) (format t "[L] Dreamer error: ~a~%" e)))
  
  (format t "[L] ═══════════════════════════════════════~%~%"))

(defun coming-of-age (strategy-name old-status new-status)
  "Coming of Age ceremony - when a strategy is promoted"
  (format t "~%")
  (format t "[L] ═══════════════════════════════════════~%")
  (format t "[L] 🎊 COMING OF AGE CEREMONY~%")
  (format t "[L] ═══════════════════════════════════════~%")
  (format t "[L] 🌟 ~a has proven worthy!~%" strategy-name)
  (format t "[L] 📜 Status: ~a → ~a~%" old-status new-status)
  (format t "[L] 🎺 The tribes celebrate!~%")
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
  (format t "[L] 🙏 May the wisdom live on in the tribe.~%")
  (format t "[L] ═══════════════════════════════════════~%~%"))

(defun start-brain ()
  (format t "~%[L] 🦈 Swimmy Ver 41.0 - V2.1 INDEPENDENT CLANS~%")
  (format t "[L] 🏹 Hunters | 🔮 Shamans | ⚔️ Breakers | 🗡️ Raiders~%")
  (format t "[L] 📜 Constitution | 👴 Elders | 🗣️ Tribal Dialect~%")
  
  ;; Clans already defined in school.lisp (Hunters, Shamans, Breakers, Raiders)
  
  ;; Initialize clan treasury
  (initialize-clan-treasury)
  
  ;; V5.1: Setup multi-channel Discord webhooks
  (setup-symbol-webhooks)
  
  ;; Morning Ritual
  (morning-ritual)
  
  (load-genome)
  (format t "[L] 📚 Strategies: ~d knowledge base + ~d evolved~%" 
          (length *strategy-knowledge-base*) (length *evolved-strategies*))
  (format t "[L] ⚙️ Daily Limit: ~d | Max Losses: ~d~%" *daily-loss-limit* *max-streak-losses*)
  (let ((ctx (pzmq:ctx-new)))
    (unwind-protect
         ;; V41.1: Fix ZMQ topology - Brain BINDS, Guardian CONNECTS
         ;; Guardian: PUSH->5555 (connect), SUB<-5556 (connect)  
         ;; Brain: PULL<-5555 (bind), PUB->5556 (bind)
         (let ((pull (pzmq:socket ctx :pull)) (pub (pzmq:socket ctx :pub)))
           (pzmq:bind pull "tcp://*:5555")  ; Receive market data from Guardian
           (pzmq:bind pub "tcp://*:5556")    ; Send commands to Guardian
           (setf *cmd-publisher* pub)
           (sleep 1)
           ;; Close all existing positions on startup (clean slate)
           (format t "[L] 🧹 Closing all existing positions...~%")
           (dolist (sym *supported-symbols*)
             (pzmq:send pub (jsown:to-json (jsown:new-js ("action" "CLOSE") ("symbol" sym) ("close_all" t)))))
           (sleep 1)
           ;; V6.10: Request history for EACH symbol individually (EURUSD/GBPUSD fix)
           (format t "[L] 📊 Requesting history for all symbols...~%")
           (dolist (sym *supported-symbols*)
             (format t "[L]    → Requesting ~a history...~%" sym)
             (pzmq:send pub (jsown:to-json (jsown:new-js ("action" "REQ_HISTORY") ("symbol" sym) ("volume" 0))))
             (sleep 0.5))  ; Stagger requests to avoid overwhelming MT5
           ;; Initial setup
           (assemble-team)
           (request-prediction)
           (loop (process-msg (pzmq:recv-string pull))))
      (pzmq:ctx-term ctx))))
;; (start-brain) - Moved to brain.lisp to prevent blocking load sequence
