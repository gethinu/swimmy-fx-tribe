;; school-evolution.lisp - Genetic Evolution System for Strategies
;; V8.0: Self-Learning via Parameter Mutation

(in-package :swimmy.school)

;;; ==========================================
;;; UTILITIES
;;; ==========================================

(defun substitute-in-string (source old-sub new-sub)
  "Helper to replace substring"
  (let ((pos (search old-sub source)))
    (if pos
        (concatenate 'string (subseq source 0 pos) new-sub (subseq source (+ pos (length old-sub))))
        source)))

(defun rewrite-logic-symbols (sexpr old-val new-val type)
  "Recursively rewrite symbols in an S-expression based on parameter change.
   Example: (SMA-50) -> (SMA-60) when type='SMA', old=50, new=60"
  (cond
    ;; Base case: Atom
    ((atom sexpr)
     (if (symbolp sexpr)
         (let ((name (symbol-name sexpr)))
           (cond
             ;; Case 1: SMA/EMA/RSI/ADX/ATR standard format "TYPE-VAL"
             ((and (string= (string-upcase type) "SMA")
                   (search (format nil "SMA-~d" old-val) name))
              (intern (substitute-in-string name (format nil "SMA-~d" old-val) (format nil "SMA-~d" new-val)) (symbol-package sexpr)))
             
             ((and (string= (string-upcase type) "EMA")
                   (search (format nil "EMA-~d" old-val) name))
               (intern (substitute-in-string name (format nil "EMA-~d" old-val) (format nil "EMA-~d" new-val)) (symbol-package sexpr)))

             ((and (string= (string-upcase type) "RSI")
                   (search (format nil "RSI-~d" old-val) name))
               (intern (substitute-in-string name (format nil "RSI-~d" old-val) (format nil "RSI-~d" new-val)) (symbol-package sexpr)))

             ;; Generic fallback
             (t sexpr)))
         sexpr))
    
    ;; Recursive case: List
    ((listp sexpr)
     (mapcar (lambda (x) (rewrite-logic-symbols x old-val new-val type)) sexpr))))

;;; ==========================================
;;; MUTATION LOGIC
;;; ==========================================

;; Helper to extract root name
(defun get-root-name (name)
  "Extract root name, removing generation and mutation suffixes.
   Ex: 'Strat-Gen1-mut-RSI' -> 'Strat'"
  (let ((gen-pos (search "-Gen" name))
        (mut-pos (search "-mut" name)))
    (subseq name 0 (or gen-pos mut-pos (length name)))))

(defun mutate-strategy-param (strategy param-type old-val new-val)
  "Create a new strategy with a specific parameter mutated"
  (let* ((old-name (strategy-name strategy))
         (root (get-root-name old-name))
         ;; Handle generation tracking (Default to 0 if slot missing in old structs)
         (gen (if (slot-exists-p strategy 'generation) (strategy-generation strategy) 0))
         (new-gen (1+ gen))
         
         ;; Use 3 chars for clarity (e.g. RSI, SMA, EMA) instead of just B/R/S
         (clean-type (if (> (length param-type) 3) (subseq param-type 0 3) param-type))
         
         ;; New format: Root-GenN-mut-ParamVal
         (new-name (format nil "~a-Gen~d-mut-~a~d" root new-gen clean-type new-val))
         
         (indicators (copy-tree (strategy-indicators strategy)))
         (entry (copy-tree (strategy-entry strategy)))
         (exit (copy-tree (strategy-exit strategy))))
    
    ;; 1. Update Indicators List
    ;; e.g. ((sma 50) (rsi 14)) -> ((sma 60) (rsi 14))
    (setf indicators
          (mapcar (lambda (ind)
                    (if (and (eq (car ind) (intern param-type :swimmy.school))
                             (equal (second ind) old-val))
                        (cons (car ind) (cons new-val (cddr ind))) ; Update param
                        ind))
                  indicators))
    
    ;; 2. Rewrite Entry/Exit Logic
    (let ((new-entry (rewrite-logic-symbols entry old-val new-val param-type))
          (new-exit (rewrite-logic-symbols exit old-val new-val param-type)))
      
      ;; 3. Create New Strategy Object
      (make-strategy 
       :name new-name
       :indicators indicators
       :entry new-entry
       :exit new-exit
       :sl (strategy-sl strategy)
       :tp (strategy-tp strategy)
       :volume (strategy-volume strategy)
       :category (strategy-category strategy)
       :indicator-type (strategy-indicator-type strategy)
       :timeframe (strategy-timeframe strategy)
       :generation new-gen))))

;;; ==========================================
;;; GLOBAL CONSTANTS (Refactored per Uncle Bob)
;;; ==========================================

(defparameter *safety-cap-sl* 5.00 "Maximum Stop Loss (500 pips)")
(defparameter *safety-cap-tp* 10.00 "Maximum Take Profit (1000 pips)")

;;; ==========================================
;;; SAFETY LOGIC (Graham / Article 5)
;;; ==========================================

(defun ensure-safety-cap (sl tp)
  "Enforce maximum risk parameters (Graham Rule)
   Refactored V42.2: Using constants."
  (values (min sl *safety-cap-sl*)
          (min tp *safety-cap-tp*)))

(defun mutate-timeframe-param (strategy old-tf new-tf)
  "Create a new strategy with mutated timeframe"
  (let* ((old-name (strategy-name strategy))
         (root (get-root-name old-name))
         (gen (if (slot-exists-p strategy 'generation) (strategy-generation strategy) 0))
         (new-gen (1+ gen))
         (new-name (format nil "~a-Gen~d-mut-TF~d" root new-gen new-tf)))
    
    (make-strategy 
      :name new-name
      :indicators (copy-tree (strategy-indicators strategy))
      :entry (copy-tree (strategy-entry strategy))
      :exit (copy-tree (strategy-exit strategy))
      :sl (strategy-sl strategy)
      :tp (strategy-tp strategy)
      :volume (strategy-volume strategy)
      :category (strategy-category strategy)
      :indicator-type (strategy-indicator-type strategy)
      :timeframe new-tf
      :generation new-gen)))

(defun evolve-strategy (strategy)
  "Attempt to evolve a strategy by mutating one of its parameters."
  ;; 10% Chance to mutate Timeframe (Expert Panel Recommendation)
  (if (< (random 10) 1)
      (let* ((raw-tf (strategy-timeframe strategy))
             (current-tf (if (numberp raw-tf) raw-tf 0)) ; Handle "H1" strings
             (options '(5 15 60 240)) ; M5, M15, H1, H4
             (new-tf (nth (random (length options)) options)))
        (if (= new-tf current-tf)
            ;; If same, force change
            (evolve-strategy strategy) ; Retry
            (progn
              (format t "[EVOLUTION] 🧬 Mutating Timeframe: ~a M~d -> M~d~%" (strategy-name strategy) current-tf new-tf)
              (mutate-timeframe-param strategy current-tf new-tf))))

      ;; 90% Chance to mutate Indicators
      (let ((indicators (strategy-indicators strategy)))
        (when indicators
          ;; Pick random indicator to mutate
          (let* ((target (nth (random (length indicators)) indicators))
                 (type (string-upcase (symbol-name (car target))))
                 (param (second target))) 
            
            (when (numberp param)
              ;; Mutate by +/- 10-20%
              (let* ((mutation-factor (+ 0.8 (random 0.4))) 
                     (new-val (round (* param mutation-factor))))
                
                ;; Ensure change
                (when (= new-val param) (incf new-val))
                
                (format t "[EVOLUTION] 🧬 Mutating ~a: ~a ~d -> ~d~%" 
                        (strategy-name strategy) type param new-val)
                
                (mutate-strategy-param strategy type param new-val))))))))

(defun mutate-strategy (parent &optional (rate 0.2))
  "Wrapper for school-learning compatibility."
  (declare (ignore rate))
  (evolve-strategy parent))

;;; ==========================================
;;; VERBALIZED SAMPLING & LLM GENERATION
;;; ==========================================

(defparameter *vs-candidate-count* 5)
(defparameter *vs-temperature* 1.0)

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
         (context (list :analysis analysis))
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
              (let ((parsed-list (read-from-string lisp-code-str)))
                (dolist (item parsed-list)
                  (let ((name (getf item :name))
                        (entry-str (getf item :entry))
                        (exit-str (getf item :exit))
                        (sl (getf item :sl))
                        (tp (getf item :tp))
                        (tf (getf item :timeframe))
                        (prob (getf item :probability)))
                    
                    (let ((entry (if (string= entry-str "") nil (read-from-string entry-str)))
                          (exit (if (string= exit-str "") nil (read-from-string exit-str))))
                      
                      (when (and name entry exit)
                         (push (cons (make-strategy :name (format nil "~a-Gen0" name)
                                                    :entry entry 
                                                    :exit exit 
                                                    :sl sl :tp tp 
                                                    :category :trend
                                                    :timeframe (if (numberp tf) tf 15) ; Default M15
                                                    :generation 0)
                                     prob)
                               strategies))))))
            (error (e)
              (format t "[PARSE ERROR] Transpilation failed: ~a~%" e)
              nil)))))
    strategies))


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
          (let ((data (read-from-string (subseq lisp-str start (1+ end)))))
            ;; Format: (STRATEGY :name "X" :entry ... )
            (let ((plist (cdr data))) ; Skip STRATEGY symbol
               (make-strategy 
                :name (getf plist :name)
                :entry (getf plist :entry)
                :exit (getf plist :exit)
                :indicators (getf plist :indicators)
                :sl (or (getf plist :sl) 1.0)
                :tp (or (getf plist :tp) 2.0)
                :category :trend ; Todo: Infer or inherit
                :generation 1)))))
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

(defun evolve-population-via-mutation ()
  "Evolve population by applying random mutations (Genetic Algorithm).
   FIX: 2026-01-12 (Ponkotsu Repair)"
  (format t "[EVOLUTION] 🧬 Running Genetic Mutation Cycle...~%")
  (dolist (strat *strategy-knowledge-base*)
    ;; 5% Mutation Rate per cycle
    (when (< (random 1.0) 0.05)
      (mutate-strategy strat))))

;;; ==========================================
;;; ORCHESTRATION (Migrated from school-learning.lisp)
;;; ==========================================

(defun perform-reproduction ()
  "Allow dominant species to reproduce using Tournament Selection and Tribal Crossover."
  (let* ((population *evolved-strategies*)
         (current-count (length population))
         (max-population 50)
         (slots-available (- max-population current-count))
         (born-count 0))
    
    (when (> slots-available 0)
      (dotimes (i (min slots-available 5))
        (let ((roll (random 1.0))
              (child nil))
          
          (cond
            ;; 30% Tribal Crossover (Generative / AI-Driven)
            ((< roll 0.30)
             ;; Optimized Tribal Selection (Naval's O(1) Fix)
             (let* ((cats (loop for k being the hash-keys of *category-pools* collect k)))
               (when (> (length cats) 0)
                 (let* ((cat-a (nth (random (length cats)) cats))
                        (cat-b (if (> (length cats) 1)
                                   (loop for c = (nth (random (length cats)) cats)
                                         until (not (eq c cat-a))
                                         return c)
                                   cat-a)))
                   ;; Pick best from each tribe
                   (let ((tribe-a (gethash cat-a *category-pools*))
                         (tribe-b (gethash cat-b *category-pools*)))
                     (when (and tribe-a tribe-b)
                       (let ((p1 (select-parent-tournament tribe-a :k 2))
                             (p2 (select-parent-tournament tribe-b :k 2)))
                         ;; GENETIC DISTANCE CHECK (Musk's Sweet Spot)
                         (when (and p1 p2 (genetic-compatibility-p (extract-genome p1) (extract-genome p2)))
                           (format t "[L] 🧬 Tribal Crossover: ~a + ~a~%" (strategy-name p1) (strategy-name p2))
                           
                           ;; V17: Generative Crossover 
                           (if (and (boundp 'swimmy.main::*gemini-api-key*) 
                                    swimmy.main::*gemini-api-key*)
                               (setf child (crossover-strategy-generative p1 p2))
                               (setf child (crossover-strategy p1 p2)))))))))))
            
            ;; 40% Tournament Crossover (Meritocracy)
            ((< roll 0.70)
             (let ((p1 (select-parent-tournament population))
                   (p2 (select-parent-tournament population)))
               (when (and p1 p2)
                 ;; GENETIC DISTANCE CHECK
                 (when (genetic-compatibility-p (extract-genome p1) (extract-genome p2))
                   (format t "[L] ⚔️ Tournament Crossover: ~a + ~a~%" (strategy-name p1) (strategy-name p2))
                   (setf child (crossover-strategy p1 p2))))))
            
            ;; 30% Asexual Mutation (Local Optimization)
            (t
             (let ((p (select-parent-tournament population)))
               (when p
                 (setf child (mutate-strategy p 0.2))))))
          
          (when child
            (push child *evolved-strategies*)
            (incf born-count)))))
    born-count))

(defun perform-immigration ()
  "Inject diverse strategies if ecosystem is unbalanced."
  (let ((immigrant-count 0))
    (when (ecosystem-needs-diversity-p)
      (let* ((weak-niche (get-underpopulated-niche))
             (candidates (remove-if-not (lambda (s) (eq (strategy-category s) weak-niche))
                                      *evolved-strategies*)))
        (when candidates
          (let ((parent (first (sort candidates #'> :key (lambda (s) (or (strategy-sharpe s) -999))))))
             (format t "[L] 🧬 DIVERSITY INJECTION: Need ~a strategies~%" weak-niche)
             (if (and (fboundp 'evolve-via-llm)
                      (boundp 'swimmy.main::*gemini-api-key*)
                      swimmy.main::*gemini-api-key*)
                 (progn
                   (format t "[L] 🧠 Triggering LLM Evolution (VS) for ~a...~%" weak-niche)
                   (evolve-via-llm weak-niche)
                   (incf immigrant-count))
                 (let ((child (mutate-strategy parent 0.3))) 
                   (push child *evolved-strategies*)
                   (incf immigrant-count)))))))
    immigrant-count))

(defun apply-natural-selection ()
  "Orchestrate Evolution (Extinction -> Reproduction -> Immigration)"
  (let ((removed (perform-extinction))
        (born (perform-reproduction))
        (immigrated (perform-immigration)))
    
    (when (or (> removed 0) (> born 0) (> immigrated 0))
      (format t "[L] 🌿 ECOSYSTEM: ~d extinction, ~d reproduction, ~d immigration | Health: ~,0f%~%"
              removed born immigrated (* 100 (calculate-ecosystem-health))))))

(defun maintain-ecosystem-balance ()
  "Periodic ecosystem maintenance"
  ;; Apply natural selection pressure
  (apply-natural-selection)
  
  ;; Check diversity and suggest focus for new strategies
  (when (ecosystem-needs-diversity-p)
    (let ((weak-niche (get-underpopulated-niche)))
      (format t "[L] 🌱 Ecosystem needs ~a strategies for diversity~%" weak-niche)))
  
  ;; Report ecosystem state every N calls
  (let ((state (get-ecosystem-state)))
    (format t "[L] 🏞️ Population: ~d | Diversity: ~,2f | Balance: ~,0f% | Health: ~,0f%~%"
            (ecosystem-state-total-population state)
            (ecosystem-state-diversity-score state)
            (* 100 (ecosystem-state-niche-balance state))
            (* 100 (ecosystem-state-health-score state)))))

(defun get-ecosystem-recommendation ()
  "Get recommendation for new strategy generation"
  (let ((weak-niche (get-underpopulated-niche))
        (health (calculate-ecosystem-health)))
    (cond
      ((< health 0.3)
       (list :action :diversify
             :focus weak-niche
             :message "Ecosystem unhealthy - need diverse strategies"))
      ((ecosystem-needs-diversity-p)
       (list :action :specialize
             :focus weak-niche
             :message (format nil "Focus on ~a category" weak-niche)))
      (t
       (list :action :evolve
             :focus nil
             :message "Ecosystem healthy - continue evolution")))))
