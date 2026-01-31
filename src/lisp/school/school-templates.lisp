;;; school-templates.lisp - Template-based Strategy Generation
;;; Extracted from dreamer2.lisp

(in-package :swimmy.school)

;;; ==========================================
;;; STRATEGY TEMPLATES (AI Generation)
;;; ==========================================
;;; Structured templates for AI-based strategy generation

(defparameter *strategy-templates*
  '((:trend-follow
     :description "Follow established trends using moving average crossovers"
     :indicators ((sma :param1) (sma :param2))
     :entry (cross-above sma-param1 sma-param2)
     :exit (cross-below sma-param1 sma-param2)
     :optimal-regime :trending
     :param-ranges ((:param1 3 15) (:param2 20 100)))
    
    (:mean-reversion
     :description "Trade bounces from Bollinger Bands extremes"
     :indicators ((bb 20 2))
     :entry (< price bb-lower)
     :exit (> price bb-middle)
     :optimal-regime :ranging
     :param-ranges ((:period 10 30) (:std 1.5 3.0)))
    
    (:breakout
     :description "Trade breakouts from consolidation ranges"
     :indicators ((atr :param1))
     :entry (> price (+ high-period (* multiplier atr)))
     :exit (< price (- entry (* 0.5 atr)))
     :optimal-regime :volatile
     :param-ranges ((:param1 10 20) (:period 20 50) (:multiplier 1.5 3.0)))
    
    (:rsi-reversal
     :description "Trade RSI overbought/oversold reversals"
     :indicators ((rsi :param1))
     :entry (< rsi 30)
     :exit (> rsi 70)
     :optimal-regime :ranging
     :param-ranges ((:param1 7 21)))
    
    (:macd-crossover
     :description "Trade MACD signal crossovers"
     :indicators ((macd :fast :slow :signal))
     :entry (cross-above macd-line signal-line)
     :exit (cross-below macd-line signal-line)
     :optimal-regime :trending
     :param-ranges ((:fast 8 16) (:slow 21 34) (:signal 7 12))))
  "Strategy templates for AI-based generation")

(defun get-template-for-regime (regime)
  "Get optimal strategy template for current market regime"
  (let ((matching (remove-if-not 
                   (lambda (tmpl) (eq (getf (cdr tmpl) :optimal-regime) regime))
                   *strategy-templates*)))
    (if matching
        (nth (random (length matching)) matching)
        (nth (random (length *strategy-templates*)) *strategy-templates*))))

(defun fill-template-params (template params)
  "Fill parameter placeholders in template with actual values"
  (if (atom template)
      (let ((param-entry (assoc template params)))
        (if param-entry
            (floor (second param-entry))
            template))
      (mapcar (lambda (x) (fill-template-params x params)) template)))

(defun generate-strategy-from-template (template-name &optional custom-params)
  "Generate a concrete strategy from a template with randomized or custom parameters"
  (let ((template (assoc template-name *strategy-templates*)))
    (when template
      (let* ((desc (cdr template))
             (param-ranges (getf desc :param-ranges))
             (params (or custom-params
                         (mapcar (lambda (range)
                                   (let ((name (first range))
                                         (min-v (second range))
                                         (max-v (third range)))
                                     (list name (+ min-v (random (float (- max-v min-v)))))))
                                 param-ranges)))
             (name (format nil "~a-~a" template-name (random 1000))))
        
        ;; Create strategy with filled-in parameters
        (make-strategy
         :name name
         :indicators (fill-template-params (getf desc :indicators) params)
         :entry (getf desc :entry)
         :exit (getf desc :exit)
         :sl (case template-name
               (:trend-follow 0.3)
               (:mean-reversion 0.2)
               (:breakout 0.4)
               (:rsi-reversal 0.25)
               (:macd-crossover 0.3)
               (t 0.25))
         :tp (case template-name
               (:trend-follow 0.6)
               (:mean-reversion 0.3)
               (:breakout 0.8)
               (:rsi-reversal 0.4)
               (:macd-crossover 0.5)
               (t 0.4))
         :volume 0.01)))))

(defun auto-generate-strategy-for-regime ()
  "Automatically generate a strategy optimized for current regime"
  (let* ((regime (if (boundp '*current-regime*) *current-regime* :trending))
         (template (get-template-for-regime regime)))
    (when template
      (let ((strategy (generate-strategy-from-template (car template))))
        (format t "[G] 🎯 Auto-generated ~a strategy for ~a regime~%"
                (car template) regime)
        strategy))))

;;; ==========================================
;;; SELF-GENERATING TEMPLATES (Naval)
;;; ==========================================

(defparameter *template-performance* (make-hash-table :test 'eq)
  "Track performance of each template type: template -> (wins losses avg-sharpe)")

(defun record-template-result (template-name won-p sharpe)
  "Record template performance for evolution"
  (let ((stats (gethash template-name *template-performance* (list 0 0 0.0 0))))
    (if won-p (incf (first stats)) (incf (second stats)))
    ;; Running average of sharpe
    (let ((n (+ (first stats) (second stats))))
      (setf (fourth stats) (/ (+ (* (fourth stats) (1- n)) sharpe) n)))
    (setf (gethash template-name *template-performance*) stats)))

(defun get-best-template ()
  "Get best performing template based on win rate and sharpe"
  (let ((best nil) (best-score -999))
    (maphash
     (lambda (name stats)
       (let* ((wins (first stats))
              (losses (second stats))
              (total (+ wins losses))
              (avg-sharpe (fourth stats))
              (win-rate (if (> total 0) (/ wins (float total)) 0.5))
              (score (+ (* win-rate 0.5) (* avg-sharpe 0.5))))
         (when (and (> total 5) (> score best-score))
           (setf best-score score)
           (setf best name))))
     *template-performance*)
    best))

(defun generate-llm-template-proposal ()
  "Naval improvement: Ask LLM to propose a new template structure"
  (let* ((existing-templates (mapcar #'car *strategy-templates*))
         (performance-summary (template-performance-summary))
         (prompt (format nil "You are a trading strategy architect.

CURRENT TEMPLATES: ~{~a~^, ~}
PERFORMANCE: ~a

TASK: Propose ONE new template structure that is DIFFERENT from existing ones.

OUTPUT FORMAT (Lisp s-expression):
(:NEW-TEMPLATE-NAME
  :description \"What this strategy does\"
  :indicators ((indicator-name param1 param2))
  :entry (condition)
  :exit (condition)
  :optimal-regime :trending/:ranging/:volatile
  :param-ranges ((:param1 min max)))

Focus on strategies NOT covered by existing templates.
Output ONLY the s-expression."
                         existing-templates performance-summary)))
    ;; Return prompt for LLM call (actual call handled externally)
    prompt))

(defun template-performance-summary ()
  "Generate summary of template performance"
  (let ((result nil))
    (maphash
     (lambda (name stats)
       (push (format nil "~a: ~d/~d wins (Sharpe ~,2f)"
                     name (first stats) (+ (first stats) (second stats))
                     (fourth stats))
             result))
     *template-performance*)
    (format nil "~{~a~^; ~}" result)))

(defun evolve-template (template-name)
  "Evolve a template by mutating its param-ranges"
  (let ((template (assoc template-name *strategy-templates*)))
    (when template
      (let* ((desc (cdr template))
             (param-ranges (getf desc :param-ranges))
             ;; Mutate ranges slightly
             (new-ranges 
              (mapcar (lambda (range)
                        (let* ((name (first range))
                               (min-v (second range))
                               (max-v (third range))
                               (delta (* (- max-v min-v) 0.1)))
                          (list name
                                (+ min-v (* (- (random 2.0) 1.0) delta))
                                (+ max-v (* (- (random 2.0) 1.0) delta)))))
                      param-ranges)))
        (format t "[T] 🧬 Evolved ~a ranges: ~a~%" template-name new-ranges)
        new-ranges))))
