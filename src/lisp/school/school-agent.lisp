;;; school-agent.lisp - Agent Skills and Memory Systems
;;; Extracted from school-state.lisp (V7.9++)
;;; Implements Context Compression, Episodic/Semantic Memory, and Multi-Agent Patterns

(in-package :swimmy.school)

;;; ==========================================
;;; V7.9++: CONTEXT COMPRESSION (Graham Critique #3 - Agent Skills)
;;; Implementing techniques from .agent/workflows/context-compression.md
;;; ==========================================

(defparameter *context-max-history* 50
  "Maximum items to keep in rolling context window")

(defun compress-strategy-list (strategies)
  "Schema-based compression: Keep only essential fields from strategies.
   Reduces token count for LLM processing."
  (mapcar (lambda (s)
            (list :id (getf s :id)
                  :wr (getf s :win-rate)
                  :n (getf s :trade-count)))
          strategies))

(defun compress-trade-log (log &optional (max-entries *context-max-history*))
  "Rolling window compression: Keep only recent N trades.
   Implements age-based expiration from Agent Skills."
  (let ((sorted (sort (copy-list log) #'> :key (lambda (x) (getf x :time)))))
    (subseq sorted 0 (min max-entries (length sorted)))))

(defun summarize-market-context (ticks)
  "Hierarchical summarization: Aggregate tick data into summary stats.
   Reduces 1000s of ticks to a handful of metrics."
  (when (and ticks (> (length ticks) 0))
    (let* ((prices (mapcar (lambda (tick) (getf tick :close)) ticks))
           (high (apply #'max prices))
           (low (apply #'min prices))
           (avg (/ (apply #'+ prices) (length prices)))
           (last-price (car (last prices)))
           (volatility (/ (- high low) avg)))
      (list :high high :low low :avg avg :last last-price :vol volatility :n (length ticks)))))

(defun compress-for-llm (data data-type)
  "Entry point for context compression. Dispatches by data type.
   Implements Agent Skills integration (Graham Critique #3)."
  (case data-type
    (:strategies (compress-strategy-list data))
    (:trades (compress-trade-log data))
    (:ticks (summarize-market-context data))
    (otherwise data)))

;;; ==========================================
;;; V7.9++: MEMORY SYSTEMS (Graham Critique #3 - Agent Skills)
;;; Implementing techniques from .agent/workflows/memory-systems.md
;;; ==========================================

(defparameter *episodic-buffer* nil "Short-term episodic memory buffer")
(defparameter *semantic-rules* (make-hash-table :test 'equal) "Long-term semantic rules")
(defparameter *episodic-buffer-max* 100 "Max episodic buffer size")

(defun store-episodic (event-type data)
  "Store event in episodic memory with timestamp."
  (push (list :time (get-universal-time) :type event-type :data data) *episodic-buffer*)
  (when (> (length *episodic-buffer*) *episodic-buffer-max*)
    (setf *episodic-buffer* (butlast *episodic-buffer*))))

(defun consolidate-to-semantic (event-type threshold)
  "Consolidate repeated episodic patterns into semantic rules."
  (let* ((relevant (remove-if-not (lambda (e) (eq (getf e :type) event-type)) *episodic-buffer*))
         (count (length relevant)))
    (when (>= count threshold)
      (setf (gethash event-type *semantic-rules*) 
            (list :pattern event-type :confidence (/ count (float *episodic-buffer-max*)) :n count))
      (format t "[M] 📚 Semantic rule consolidated: ~a (confidence: ~,1f%)~%" 
              event-type (* 100 (/ count (float *episodic-buffer-max*)))))))

(defun recall-semantic (event-type)
  "Recall semantic rule if exists."
  (gethash event-type *semantic-rules*))

;;; ==========================================
;;; V7.9++: MULTI-AGENT PATTERNS (Graham Critique #3 - Agent Skills)
;;; Implementing techniques from .agent/workflows/multi-agent-patterns.md
;;; ==========================================

(defparameter *agent-blackboard* (make-hash-table :test 'equal) "Shared blackboard for agents")
(defparameter *agent-votes* nil "Voting log for consensus")

(defun agent-post (agent-id topic data)
  "Post to shared blackboard (publish-subscribe pattern)."
  (setf (gethash (cons agent-id topic) *agent-blackboard*) 
        (list :agent agent-id :topic topic :data data :time (get-universal-time))))

(defun agent-read (topic)
  "Read all posts on a topic from blackboard."
  (let ((posts nil))
    (maphash (lambda (k v) 
               (when (equal (cdr k) topic)
                 (push v posts)))
             *agent-blackboard*)
    posts))

(defun agent-vote (agent-id decision confidence)
  "Cast a vote with confidence weighting."
  (push (list :agent agent-id :decision decision :confidence confidence) *agent-votes*))

(defun tally-votes ()
  "Tally votes using confidence-weighted consensus."
  (let ((tallies (make-hash-table :test 'equal)))
    (dolist (vote *agent-votes*)
      (let ((decision (getf vote :decision))
            (conf (getf vote :confidence)))
        (incf (gethash decision tallies 0) conf)))
    (let ((winner nil) (max-score 0))
      (maphash (lambda (k v) (when (> v max-score) (setf winner k max-score v))) tallies)
      (setf *agent-votes* nil)
      (values winner max-score))))

;;; ==========================================
;;; V7.9++: AGENT SKILLS TESTS (Graham: Test everything)
;;; ==========================================

(defun test-context-compression ()
  "Unit tests for context compression functions."
  (format t "~%🧪 Running Context Compression Tests...~%")
  (let ((passed 0) (failed 0))
    ;; Test compress-strategy-list
    (let* ((strats (list (list :id "S1" :win-rate 0.6 :trade-count 10 :extra "ignored")))
           (result (compress-strategy-list strats)))
      (if (and result (= (length result) 1) (getf (car result) :id))
          (progn (incf passed) (format t "  ✅ compress-strategy-list works~%"))
          (progn (incf failed) (format t "  ❌ compress-strategy-list failed~%"))))
    ;; Test summarize-market-context
    (let* ((ticks (list (list :close 100) (list :close 110) (list :close 105)))
           (result (summarize-market-context ticks)))
      (if (and result (= (getf result :high) 110) (= (getf result :low) 100))
          (progn (incf passed) (format t "  ✅ summarize-market-context works~%"))
          (progn (incf failed) (format t "  ❌ summarize-market-context failed~%"))))
    (format t "📊 Compression Tests: ~a passed, ~a failed~%" passed failed)
    (= failed 0)))

(format t "[L] 🤖 school-agent.lisp loaded - Agent Skills System Active~%")
