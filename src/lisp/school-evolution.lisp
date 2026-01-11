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
;;; SAFETY LOGIC (Graham / Article 5)
;;; ==========================================

(defun ensure-safety-cap (sl tp)
  "Enforce maximum risk parameters (Graham Rule)
   Updated V42.1: Relaxed caps to support JPY pairs (e.g. SL 50 pips = 0.50)"
  (values (min sl 5.00)    ; Max SL 5.00 (500 pips JPY / 50000 pips EUR - Backtest handles EUR)
          (min tp 10.00))) ; Max TP 10.00

(defun evolve-strategy (strategy)
  "Attempt to evolve a strategy by mutating one of its parameters."
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
            
            ;; Safety Cap Check for SL/TP mutation
            ;; (Not mutating SL/TP here, but if we did, we'd clip them)
            
            (format t "[EVOLUTION] 🧬 Mutating ~a: ~a ~d -> ~d~%" 
                    (strategy-name strategy) type param new-val)
            
            (mutate-strategy-param strategy type param new-val)))))))

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
      \"reasoning\": \"Explanation...\"
    }
  ]
}

RULES:
1. Names must be unique (include Gen0 suffix).
2. SL must NOT exceed 0.10 (10%).
3. Logic must be valid Lisp s-expressions.
4. Output ONLY valid JSON.

Generate strategies now:"
            *vs-candidate-count*
            strategy-type
            analysis)))

(defun evolve-via-llm (&optional requested-type)
  "Driver function: Auto-generate strategies using Verbalized Sampling.
   If requested-type is provided (e.g. 'scalp'), focuses on that."
  (format t "~%[L] 🧠 Evolving via LLM (Verbalized Sampling)...~%")
  
  ;; 1. Gather Context
  (let* ((analysis (if (fboundp 'get-structured-self-analysis) 
                       (get-structured-self-analysis) 
                       "Initializing..."))
         (context (list :analysis analysis))
         ;; Map keyword niche to string description
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
        ;; 3. Parse JSON
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
                  ;; Enforce Safety Caps (Graham Rule - Relaxed for JPY Scaling)
                  ;; OLD: SL 0.10 (10 pips on JPY) -> Ponkotsu Guillotine
                  ;; NEW: SL 5.00 (500 pips on JPY), TP 10.00 (1000 pips on JPY)
                  ;; Let backtest filter out the crazy ones for EURUSD.
                  (setf (strategy-sl strat) (min (strategy-sl strat) 5.00))
                  (setf (strategy-tp strat) (min (strategy-tp strat) 10.00))
                  
                  (format t "[L] 🆕 Candidate: ~a (Conf: ~,2f)~%" (strategy-name strat) conf)
                  
                  ;; 5. Register (Check for Clones logic is in backtest)
                  (if (and *evolved-strategies* (> (length *evolved-strategies*) 0))
                       (request-clone-check strat nil)
                       (progn
                         (push strat *evolved-strategies*)
                         (request-backtest strat))))))))))))

