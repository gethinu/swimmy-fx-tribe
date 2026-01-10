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

(defun mutate-strategy-param (strategy param-type old-val new-val)
  "Create a new strategy with a specific parameter mutated"
  (let* ((name (strategy-name strategy))
         (new-name (format nil "~a-mut-~a~d" name (subseq param-type 0 1) new-val))
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
       :timeframe (strategy-timeframe strategy)))))

(defun evolve-strategy (strategy)
  "Attempt to evolve a strategy by mutating one of its parameters. Returns new strategy or nil."
  (let ((indicators (strategy-indicators strategy)))
    (when indicators
      ;; Pick random indicator to mutate
      (let* ((target (nth (random (length indicators)) indicators))
             (type (string-upcase (symbol-name (car target))))
             (param (second target))) ; Assumes first param is the period (sma 50)
        
        (when (numberp param)
          ;; Mutate by +/- 10-20%
          (let* ((mutation-factor (+ 0.8 (random 0.4))) ; 0.8 to 1.2
                 (new-val (round (* param mutation-factor))))
            
            ;; Ensure change
            (when (= new-val param) (incf new-val))
            
            (format t "[EVOLUTION] 🧬 Mutating ~a: ~a ~d -> ~d~%" 
                    (strategy-name strategy) type param new-val)
            
            (mutate-strategy-param strategy type param new-val)))))))

(defun mutate-strategy (parent &optional (rate 0.2))
  "Wrapper for school-learning compatibility. Rate is currently unused in favor of internal logic."
  (declare (ignore rate))
  (evolve-strategy parent))
