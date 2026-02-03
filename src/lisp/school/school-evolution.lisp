;; school-evolution.lisp - Genetic Evolution System for Strategies (V8.0)

(in-package :swimmy.school)

;;; ==========================================
;;; UTILITIES
;;; ==========================================

(defun substitute-in-string (source old-sub new-sub)
  (let ((pos (search old-sub source)))
    (if pos (concatenate 'string (subseq source 0 pos) new-sub (subseq source (+ pos (length old-sub)))) source)))

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

         ;; V44.8: Use Human Readable Timestamp (YYMMDD-HHMMSS)
         (ts-str (multiple-value-bind (s m h d mo y) (decode-universal-time (get-universal-time))
                   (format nil "~2,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d" (mod y 100) mo d h m s)))
         (rand-suffix (random 1000))

         ;; New format: Root-GenN-mut-ParamVal-TS
         (new-name (format nil "~a-Gen~d-mut-~a~d-~a-~3,'0d" root new-gen clean-type new-val ts-str rand-suffix))

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
  (declare (ignore old-tf))
  (let* ((old-name (strategy-name strategy))
         (root (get-root-name old-name))
         (gen (if (slot-exists-p strategy 'generation) (strategy-generation strategy) 0))
         (new-gen (1+ gen))
         ;; V44.8: Readable Timestamp
         (ts-str (multiple-value-bind (s m h d mo y) (decode-universal-time (get-universal-time))
                   (format nil "~2,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d" (mod y 100) mo d h m s)))

         (new-name (format nil "~a-Gen~d-mut-TF~d-~a" root new-gen new-tf ts-str)))

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
