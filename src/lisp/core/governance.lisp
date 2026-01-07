(in-package :swimmy.school)

;;; ==========================================
;;; SWIMMY CORE: GOVERNANCE (governance.lisp)
;;; ==========================================
;;; Contains the Constitution, High Council (御前会議), and Philosophy Logger
;;; Extracted from brain.lisp (Strangler Fig Phase 3)

;;; ══════════════════════════════════════════════════════════════════
;;;  2027 VISION: CONSTITUTION LAYER (憲法レイヤー)
;;; ══════════════════════════════════════════════════════════════════
;;; Instead of specific instructions, humans give values (Constitution)
;;; AI makes decisions aligned with these values autonomously

(defstruct core-value
  name           ; Keyword like :capital-preservation
  priority       ; 1-10 (higher = more important)
  description    ; Human-readable description
  threshold      ; Violation threshold
  check-fn)      ; (lambda (context) -> score 0.0-1.0)

;; NOTE: *constitution* and *constitution-version* defined in core/config.lisp

(defun initialize-constitution ()
  "Initialize Swimmy's Core Values - The fundamental principles that guide all decisions"
  (setf *constitution*
        (list
         ;; Priority 10: Capital Preservation (資本保全)
         (make-core-value
          :name :capital-preservation
          :priority 10
          :description "壊滅的損失を絶対回避。生き残ることが最優先。"
          :threshold 0.3
          :check-fn (lambda (ctx)
                      (let ((daily-loss (or (getf ctx :daily-pnl) 0))
                            (max-dd (or (getf ctx :max-drawdown) 0)))
                        (cond
                          ((< daily-loss -5000) 0.0)   ; Severe violation
                          ((< daily-loss -3000) 0.3)   ; High risk
                          ((< daily-loss -1000) 0.6)   ; Warning
                          ((> max-dd 20) 0.4)          ; High DD
                          (t 1.0)))))
         
         ;; Priority 9: Ethical Trading (倫理的取引)
         (make-core-value
          :name :ethical-trading
          :priority 9
          :description "市場操作に加担しない。グレーな取引は利益が出ても棄却。"
          :threshold 0.5
          :check-fn (lambda (ctx)
                      (let ((volatility (getf ctx :volatility-state)))
                        (if (eq volatility :extreme)
                            0.7  ; Caution during extreme volatility
                            1.0))))
         
         ;; Priority 8: Sustainability (持続可能性)
         (make-core-value
          :name :sustainability
          :priority 8
          :description "短期利益より長期生存。過度なリスクを取らない。"
          :threshold 0.4
          :check-fn (lambda (ctx)
                      (let ((danger (or (getf ctx :danger-level) 0))
                            (consecutive-losses (or (getf ctx :consecutive-losses) 0)))
                        (cond
                          ((>= danger 3) 0.2)
                          ((>= consecutive-losses 4) 0.3)
                          ((>= consecutive-losses 2) 0.6)
                          (t 1.0)))))
         
         ;; Priority 7: Continuous Learning (継続学習)
         (make-core-value
          :name :continuous-learning
          :priority 7
          :description "失敗から必ず学ぶ。同じ過ちを繰り返さない。"
          :threshold 0.5
          :check-fn (lambda (ctx)
                      (let ((similar-failure (getf ctx :similar-failure-count)))
                        (if (and similar-failure (> similar-failure 3))
                            0.4
                            1.0))))
         
         ;; Priority 6: Transparency (透明性)
         (make-core-value
          :name :transparency
          :priority 6
          :description "判断理由を常に記録。ブラックボックスにならない。"
          :threshold 0.6
          :check-fn (lambda (ctx)
                      ;; Always high score - this is about logging, not blocking
                      1.0))))
  
  (format t "[L] 📜 CONSTITUTION INITIALIZED (v~a)~%" *constitution-version*)
  (format t "[L] 📜 ~d Core Values loaded~%" (length *constitution*))
  *constitution*)

(defun evaluate-constitution (context)
  "Evaluate a decision against the Constitution. Returns overall alignment score."
  (unless *constitution*
    (initialize-constitution))
  
  (let ((total-score 0)
        (total-weight 0)
        (violations nil))
    
    (dolist (value *constitution*)
      (let* ((score (funcall (core-value-check-fn value) context))
             (priority (core-value-priority value))
             (weighted-score (* score priority)))
        
        (incf total-score weighted-score)
        (incf total-weight priority)
        
        ;; Track violations
        (when (< score (core-value-threshold value))
          (push (list :value (core-value-name value)
                     :score score
                     :priority priority
                     :description (core-value-description value))
                violations))))
    
    (let ((alignment (if (> total-weight 0) (/ total-score total-weight) 1.0)))
      (list :alignment alignment
            :violations violations
            :passed (null violations)))))

(defun constitution-allows-p (action context)
  "Check if Constitution allows this action. Returns T if allowed."
  (let* ((result (evaluate-constitution context))
         (alignment (getf result :alignment))
         (violations (getf result :violations)))
    
    (when violations
      (format t "[L] 📜 CONSTITUTION CHECK: ~a~%" action)
      (format t "[L] 📜 Alignment: ~,0f%~%" (* 100 alignment))
      (dolist (v violations)
        (format t "[L] 📜 ⚠️ Violation: ~a (~,0f%%) - ~a~%"
                (getf v :value) (* 100 (getf v :score)) (getf v :description))))
    
    (> alignment 0.5)))  ; Must be >50% aligned


;;; ══════════════════════════════════════════════════════════════════
;;;  2027 VISION: PHILOSOPHY LOGGER (哲学ロガー)
;;; ══════════════════════════════════════════════════════════════════
;;; Records not just WHAT happened, but WHY it happened
;;; The "Why" becomes the most important thing in 2028

;; NOTE: *philosophy-log*, *philosophy-log-max*, *philosophy-log-path* defined in core/config.lisp

(defstruct philosophy-entry
  timestamp
  action-type    ; :trade, :skip, :resign, :parameter-change
  what           ; What happened
  why            ; Why it happened (auto-generated reasoning)
  constitution-alignment  ; How well aligned with values
  context-snapshot        ; Captured context at decision time
  outcome)                ; Result of the decision (filled later)

(defun generate-why (action-type context decision)
  "Generate the philosophical 'Why' for a decision"
  (let ((parts nil))
    
    ;; Market context
    (when (getf context :regime)
      (push (format nil "市場レジームは~aであった" (getf context :regime)) parts))
    (when (getf context :volatility-state)
      (push (format nil "ボラティリティは~aであった" (getf context :volatility-state)) parts))
    
    ;; Constitution alignment
    (when (getf decision :passed)
      (push "憲法による価値判断と整合した" parts))
    (when (getf decision :violations)
      (push (format nil "以下の価値観との衝突があったが敢行した: ~a" 
                    (mapcar (lambda (v) (getf v :value)) (getf decision :violations))) parts))
    
    ;; Specific reasoning
    (case action-type
      (:trade 
       (push "シグナルとSWARM合意が閾値を超えたため、リスク許容範囲内で実行した" parts))
      (:skip 
       (push "シグナルはあったが、リスクまたは憲法上の懸念により見送った" parts))
      (:resign 
       (push "パフォーマンス低下または憲法違反の継続により、戦略の退場が必要と判断された" parts)))
    
    (format nil "~{~a。~}" (reverse parts))))

(defun log-philosophy (action-type what context &optional decision)
  "Log a philosophical entry about a decision"
  (let* ((why (generate-why action-type context decision))
         (alignment (getf decision :alignment))
         (entry (make-philosophy-entry
                 :timestamp (get-universal-time)
                 :action-type action-type
                 :what what
                 :why why
                 :constitution-alignment alignment
                 :context-snapshot context
                 :outcome nil)))
    
    (push entry *philosophy-log*)
    (if (> (length *philosophy-log*) *philosophy-log-max*)
        (setf *philosophy-log* (subseq *philosophy-log* 0 *philosophy-log-max*)))
    
    ;; Log to console
    (format t "[L] 🦉 PHILOSOPHY LOG: ~a~%" what)
    (format t "[L]    Why: ~a~%" why)
    
    entry))

;;; ══════════════════════════════════════════════════════════════════
;;;  HIGH COUNCIL (御前会議)
;;; ══════════════════════════════════════════════════════════════════
;;; 参加者: Grand Chieftain (あなた), Shaman (Opus), 4 Clan Chiefs
;;; 重要な決定は大首長に通知

(defstruct council-decision
  id
  proposal
  proposer         ; Which clan proposed
  votes            ; Plist of :clan → :approve/:reject/:abstain
  elder-advice     ; What elders said
  constitution-ok  ; Did it pass constitution check?
  final-decision   ; :approved, :rejected, :escalated
  chieftain-notified
  timestamp)

(defun gather-clan-votes (proposal proposer)
  "Gather votes from each clan"
  (let ((votes nil))
    (dolist (clan-data *clans*)
      (let* ((clan-id (clan-id clan-data))
             (vote (simulate-clan-vote clan-id proposal proposer)))
        (push (cons clan-id vote) votes)
        (format t "[L]    ~a ~a: ~a~%"
                (clan-emoji clan-data) (clan-name clan-data) vote)))
    votes))

(defun simulate-clan-vote (clan-id proposal proposer)
  "Simulate a clan's vote based on their personality"
  (cond
    ;; Own proposal - always approve
    ((eq clan-id proposer) :approve)
    
    ;; Shamans are cautious
    ((and (eq clan-id :reversion) 
          (eq *current-volatility-state* :extreme))
     :reject)
    
    ;; Breakers are aggressive
    ((eq clan-id :breakout) :approve)
    
    ;; Raiders abstain from big decisions
    ((and (eq clan-id :scalp)
          (search "aggressive" (string-downcase (format nil "~a" proposal))))
     :abstain)
    
    ;; Default: follow proposer if same risk profile
    (t :approve)))

(defun calculate-council-decision (decision)
  "Calculate final decision from votes and advisors"
  (let* ((votes (council-decision-votes decision))
         (elder-advice (council-decision-elder-advice decision))
         (const-ok (council-decision-constitution-ok decision))
         (approve-count (count :approve votes :key #'cdr))
         (total-votes (length votes))
         (approval-rate (if (> total-votes 0) (/ approve-count total-votes) 0)))
    
    (cond
      ;; Constitution forbids - REJECT
      ((not const-ok) :rejected)
      
      ;; Elders reject - ESCALATE to chieftain
      ((eq elder-advice :reject) :escalated)
      
      ;; Strong approval - APPROVED
      ((>= approval-rate *council-decision-threshold*) :approved)
      
      ;; Weak approval with elder caution - ESCALATE
      ((and (>= approval-rate 0.5) (eq elder-advice :caution)) :escalated)
      
      ;; Otherwise - REJECTED
      (t :rejected))))

(defun notify-chieftain (decision)
  "Notify the Grand Chieftain (user) via Discord about critical decision"
  (setf (council-decision-chieftain-notified decision) t)
  
  (let ((msg (format nil "~%🏛️ **HIGH COUNCIL REQUIRES YOUR ATTENTION**~%~%~
                          📜 **Proposal:** ~a~%~
                          🎺 **Proposed by:** ~a~%~
                          📋 **Council Decision:** ~a~%~
                          👴 **Elder Advice:** ~a~%~
                          📜 **Constitution:** ~a"
                     (council-decision-proposal decision)
                     (get-clan-display (council-decision-proposer decision))
                     (council-decision-final-decision decision)
                     (council-decision-elder-advice decision)
                     (if (council-decision-constitution-ok decision) "Permits" "Forbids"))))
    
    ;; Send to emergency channel
    (when (and (boundp '*discord-emergency-url*) *discord-emergency-url*)
      (handler-case
          (dex:post *discord-emergency-url*
                    :content (jsown:to-json 
                              (jsown:new-js 
                               ("embeds" (list (jsown:new-js 
                                               ("title" "🏛️ High Council Decision")
                                               ("description" msg)
                                               ("color" 15844367))))))  ; Gold
                    :headers '(("Content-Type" . "application/json"))
                    :read-timeout 3)
        (error (e) nil)))
    
    (format t "[L] 📱 Grand Chieftain notified via Discord~%")))

(defun convene-policy-council (proposal proposer-clan &key (urgency :normal))
  "Convene the Policy Council for important strategic decisions (not for trade execution)"
  (format t "~%[L] ═══════════════════════════════════════~%")
  (format t "[L] 🏛️ HIGH COUNCIL CONVENED~%")
  (format t "[L] ═══════════════════════════════════════~%")
  (format t "[L] 📜 Proposal: ~a~%" proposal)
  (format t "[L] 🎺 Proposed by: ~a~%" (get-clan-display proposer-clan))
  (format t "[L] ⚡ Urgency: ~a~%~%" urgency)
  
  (let ((decision (make-council-decision
                   :id (gensym "COUNCIL-")
                   :proposal proposal
                   :proposer proposer-clan
                   :votes nil
                   :elder-advice nil
                   :constitution-ok nil
                   :final-decision nil
                   :chieftain-notified nil
                   :timestamp (get-universal-time))))
    
    ;; Step 1: Gather clan votes
    (format t "[L] 📢 CLAN CHIEFS SPEAK:~%")
    (let ((votes (gather-clan-votes proposal proposer-clan)))
      (setf (council-decision-votes decision) votes))
    
    ;; Step 2: Consult elders
    (format t "~%[L] 👴 ELDERS COUNSEL:~%")
    (let* ((context (list :regime *current-regime*
                          :volatility-state *current-volatility-state*
                          :daily-pnl *daily-pnl*))
           (elder-vote (if (boundp '*hall-of-fame*)
                           (if *hall-of-fame* 
                               (elder-vote proposal context) 
                               :approve)
                           :approve)))
      (setf (council-decision-elder-advice decision) elder-vote)
      (format t "[L]    Elders recommend: ~a~%" elder-vote))
    
    ;; Step 3: Constitutional check
    (format t "~%[L] 📜 CONSTITUTION CHECK:~%")
    (let ((const-ok (or (null *constitution*)
                        (constitution-allows-p :trade (list :daily-pnl *daily-pnl*)))))
      (setf (council-decision-constitution-ok decision) const-ok)
      (format t "[L]    Constitution: ~a~%" (if const-ok "✅ PERMITS" "❌ FORBIDS")))
    
    ;; Step 4: Final decision
    (let ((final (calculate-council-decision decision)))
      (setf (council-decision-final-decision decision) final)
      
      (format t "~%[L] ═══════════════════════════════════════~%")
      (format t "[L] 📋 COUNCIL DECISION: ~a~%" final)
      (format t "[L] ═══════════════════════════════════════~%~%")
      
      ;; Step 5: Notify chieftain if critical
      (when (or (eq urgency :critical)
                (eq final :escalated)
                (and (eq *notify-chieftain-threshold* :all)))
        (notify-chieftain decision))
      
      ;; Log decision
      (push decision *council-log*)
      
      final)))
