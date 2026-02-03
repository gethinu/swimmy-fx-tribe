;;; school-evolution-llm.lisp - LLM generation helpers

(in-package :swimmy.school)

;;; ==========================================
;;; VERBALIZED SAMPLING & LLM GENERATION
;;; ==========================================

(defparameter *vs-candidate-count* 5)
(defparameter *vs-temperature* 1.0)

(defun dream-code (&optional requested-type)
  "Dreamer hook (currently no-op placeholder)."
  (declare (ignore requested-type))
  nil)

(defun generate-vs-prompt (strategy-type context)
  "Generate a Verbalized Sampling prompt with deep context."
  (let ((analysis (or (getf context :analysis) "No analysis available")))
    (format nil "You are an expert algorithmic trading strategist.

TASK: Generate ~d distinct trading strategies of type '~a'.

MARKET CONTEXT:
~a

OUTPUT FORMAT (JSON):
{
  \"strategies\": [
    {
      \"name\": \"Strategy-Name\",
      \"probability\": 0.85,
      \"indicators\": [[\"sma\", 20], [\"rsi\", 14]],
      \"entry\": \"(cross-above open sma-20)\",
      \"exit\": \"(cross-below close sma-20)\",
      \"sl\": 0.05,
      \"tp\": 0.10,
      \"timeframe\": 15,
      \"reasoning\": \"Explanation...\"
    }
  ]
}

RULES:
1. Names must be unique (include Gen0 suffix).
2. SL must NOT exceed 0.10 (10%).
3. Logic must be valid Lisp s-expressions.
4. Timeframe MUST be one of: 5 (M5), 15 (M15), 60 (H1), 240 (H4). DO NOT USE 1 (M1).
5. Output ONLY valid JSON.

Generate strategies now:"
            *vs-candidate-count*
            strategy-type
            analysis)))

(defun evolve-via-llm (&optional requested-type)
  "Driver function: Auto-generate strategies using Verbalized Sampling."
  (format t "~%[L] 🧠 Evolving via LLM (Verbalized Sampling)...~%")

  ;; 1. Gather Context
  (let* ((analysis (if (fboundp 'get-structured-self-analysis)
                       (get-structured-self-analysis)
                       "Initializing..."))
         (tactics (get-regime-tactics))
         (wisdom (getf tactics :wisdom "Standard generation rules apply."))
         (target-indicators (getf tactics :indicators nil))
         (enhanced-analysis (format nil "~a~%~%[TACTICAL WISDOM] ~a~%~@[Recommended Indicators: ~{~a~^, ~}~]"
                                   analysis wisdom target-indicators))
         (context (list :analysis enhanced-analysis))
         (type-map '((:trend . "Trend Following")
                     (:reversion . "Mean Reversion")
                     (:breakout . "Breakout")
                     (:scalp . "Volatility Scalp")))
         (target-type (or (cdr (assoc requested-type type-map))
                          requested-type
                          (let ((types '("Trend Following" "Mean Reversion" "Breakout" "Volatility Scalp")))
                            (nth (random (length types)) types))))
         (prompt (generate-vs-prompt target-type context)))

    ;; 2. Call LLM
    (format t "[L] 📤 Prompting Gemini for ~a...~%" target-type)
    (let ((resp (swimmy.main:call-gemini prompt)))
      (when resp
        ;; 3. Parse JSON (Refactored Logic)
        (let ((start (search "{" resp))
              (end (search "}" resp :from-end t)))
          (when (and start end)
            (let* ((json-str (subseq resp start (1+ end)))
                   (strategies (parse-vs-response json-str)))

              (format t "[L] 📥 Received ~d candidates.~%" (length strategies))

              ;; 4. Process Candidates
              (dolist (pair strategies)
                (let ((strat (car pair))
                      (conf (cdr pair)))
                  ;; Enforce Safety Caps
                  (setf (strategy-sl strat) (min (strategy-sl strat) *safety-cap-sl*))
                  (setf (strategy-tp strat) (min (strategy-tp strat) *safety-cap-tp*))

                  (format t "[L] 🆕 Candidate: ~a (Conf: ~,2f) TF: ~d~%" (strategy-name strat) conf (strategy-timeframe strat))

                  (if (and *evolved-strategies* (> (length *evolved-strategies*) 0))
                       (request-clone-check strat nil)
                       (progn
                         (push strat *evolved-strategies*)
                         (request-backtest strat))))))))))))

;;; ==========================================
;;; LLM RESPONSE PARSING
;;; ==========================================

(defun parse-json-safely (json-str)
  (declare (ignore json-str))
  nil)

(defun parse-vs-response (json-str)
  "Parse Gemini JSON response via Python, extracting timeframe."
  (let ((strategies nil))
    (with-open-file (out "/tmp/swimmy_llm_response.json" :direction :output :if-exists :supersede)
      (write-line json-str out))

    (let ((script "
import json
try:
    with open('/tmp/swimmy_llm_response.json', 'r') as f:
        data = json.load(f)
    print('(')
    for s in data['strategies']:
        print('  (')
        print(f'    :name \"{s.get(\"name\", \"Unknown\")}\"')
        print(f'    :entry \"{s.get(\"entry\", \"\")}\"')
        print(f'    :exit \"{s.get(\"exit\", \"\")}\"')
        print(f'    :sl {s.get(\"sl\", 1.0)}')
        print(f'    :tp {s.get(\"tp\", 2.0)}')
        print(f'    :timeframe {s.get(\"timeframe\", 15)}')
        print(f'    :probability {s.get(\"probability\", 0.5)}')
        print('  )')
    print(')')
except Exception as e:
    print('NIL')
"))
      (let ((lisp-code-str
             (uiop:run-program
              (list "python3" "-c" script)
              :output :string
              :ignore-error-status t)))

        (when (and lisp-code-str (> (length lisp-code-str) 0) (not (string= (string-trim '(#\Space #\Newline) lisp-code-str) "NIL")))
          (handler-case
              (let ((parsed-list (swimmy.core:safe-read-sexp lisp-code-str :package :swimmy.school)))
                (when (listp parsed-list)
                  (dolist (item parsed-list)
                    (let ((name (getf item :name))
                          (entry-str (getf item :entry))
                          (exit-str (getf item :exit))
                          (sl (getf item :sl))
                          (tp (getf item :tp))
                          (tf (getf item :timeframe))
                          (prob (getf item :probability)))
                      (let ((entry (and (stringp entry-str)
                                        (not (string= entry-str ""))
                                        (safe-read-dsl-form entry-str)))
                            (exit (and (stringp exit-str)
                                       (not (string= exit-str ""))
                                       (safe-read-dsl-form exit-str))))
                        (when (and name entry exit)
                          (let ((s (make-strategy :name (format nil "~a-Gen0" name)
                                                  :entry entry
                                                  :exit exit
                                                  :sl sl :tp tp
                                                  :category :trend
                                                  :timeframe (if (numberp tf) tf 15) ; Default M15
                                                  :generation 0)))
                            (setf (strategy-regime-intent s) (or (when (boundp '*current-regime*) *current-regime*) :unknown))
                            (push (cons s prob) strategies))))))))
            (error (e)
              (format t "[PARSE ERROR] Transpilation failed: ~a~%" e)
              nil)))))
    strategies))

;; V49.2: Automated Tactical Injection Test (Uncle Bob Requirement)
(defun test-llm-tactical-injection ()
  "Verify that LLM prompt correctly incorporates tactical wisdom from the regime."
  (format t "~%🧪 Testing LLM Tactical Injection...~%")
  (let* ((*current-regime* :trend-mature)
         (tactics (get-regime-tactics))
         (wisdom (getf tactics :wisdom))
         (analysis "Test Analysis")
         (enhanced-analysis (format nil "~a~%~%[TACTICAL WISDOM] ~a" analysis wisdom))
         (prompt (generate-vs-prompt "Trend Following" (list :analysis enhanced-analysis))))
    (if (search "Mature trend" prompt)
        (progn (format t "  ✅ Tactical Wisdom found in prompt.~%") t)
        (progn (format t "  ❌ Tactical Wisdom MISSING from prompt!~%") nil))))

;;; ==========================================
;;; GENERATIVE CROSSOVER (AI BREEDING)
;;; ==========================================

(defun generate-crossover-prompt (parent-a parent-b analysis)
  (format nil "You are an expert algorithmic trading bio-engineer.
TASK: Create a HYBRID trading strategy by merging the logic of two parents.
CONTEXT:
Parent A: ~a (Category: ~a)
Logic: Entry=~a, Exit=~a
Parent B: ~a (Category: ~a)
Logic: Entry=~a, Exit=~a
MARKET REGIME: ~a

INSTRUCTIONS:
1. Analyze the strengths of both parents.
2. Create a child that combines their logic INTELLIGENTLY (not just random splicing).
3. If Parent A is Trend and B is Reversion, create a strategy that handles transitions.
4. Output Format: LISP S-EXPRESSION DO NOT USE JSON.

OUTPUT TEMPLATE:
(STRATEGY
  :name \"Child-Integration-Name\"
  :entry (and ...)
  :exit (or ...)
  :indicators ((rsi 14) ...)
  :sl 0.5
  :tp 1.0)

Generate only the Lisp code:"
          (strategy-name parent-a) (strategy-category parent-a) (strategy-entry parent-a) (strategy-exit parent-a)
          (strategy-name parent-b) (strategy-category parent-b) (strategy-entry parent-b) (strategy-exit parent-b)
          analysis))

(defun parse-lisp-strategy (lisp-str)
  "Parse Lisp strategy definition from response"
  (handler-case
      (let* ((start (search "(STRATEGY" lisp-str))
             (end (search ")" lisp-str :from-end t)))
        (when (and start end)
          (let ((data (swimmy.core:safe-read-sexp (subseq lisp-str start (1+ end))
                                                  :package :swimmy.school)))
            (when (and (listp data)
                       (symbolp (first data))
                       (string-equal (symbol-name (first data)) "STRATEGY"))
              ;; Format: (STRATEGY :name "X" :entry ... )
              (let* ((plist (cdr data))
                     (entry (getf plist :entry))
                     (exit (getf plist :exit)))
                (when (and entry exit)
                  (make-strategy
                   :name (getf plist :name)
                   :entry entry
                   :exit exit
                   :indicators (getf plist :indicators)
                   :sl (or (getf plist :sl) 1.0)
                   :tp (or (getf plist :tp) 2.0)
                   :category :trend ; Todo: Infer or inherit
                   :generation 1)))))))
    (error (e)
      (format t "[PARSE ERROR] ~a~%" e)
      nil)))

(defun crossover-strategy-generative (parent-a parent-b)
  "Perform Generative Crossover using LLM to merge two parents."
  (format t "[L] 🧠 GENERATIVE CROSSOVER: ~a + ~a~%" (strategy-name parent-a) (strategy-name parent-b))

  (let* ((analysis (if (fboundp 'get-structured-self-analysis) (get-structured-self-analysis) "N/A"))
         (prompt (generate-crossover-prompt parent-a parent-b analysis)))

    ;; Call Gemini
    (let ((resp (swimmy.main:call-gemini prompt)))
      (when resp
        (let ((child (parse-lisp-strategy resp)))
          (when child
            ;; Inherit/Fix Metadata
            (setf (strategy-generation child)
                  (1+ (max (or (strategy-generation parent-a) 0)
                           (or (strategy-generation parent-b) 0))))
            (setf (strategy-category child)
                  (if (> (random 1.0) 0.5) (strategy-category parent-a) (strategy-category parent-b)))

            (format t "[L] 👶 AI-Child Born: ~a~%" (strategy-name child))
            child))))))
