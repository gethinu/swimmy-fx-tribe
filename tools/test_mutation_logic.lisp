;; tools/test_mutation_logic.lisp
;; Prototype for Genetic Mutation logic

(defpackage :mutation-test (:use :cl))
(in-package :mutation-test)

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
              (intern (substitute-in-string name (format nil "SMA-~d" old-val) (format nil "SMA-~d" new-val))))
             
             ((and (string= (string-upcase type) "EMA")
                   (search (format nil "EMA-~d" old-val) name))
               (intern (substitute-in-string name (format nil "EMA-~d" old-val) (format nil "EMA-~d" new-val))))

             ;; Generic fallback for simple replacement
             (t sexpr)))
         sexpr))
    
    ;; Recursive case: List
    ((listp sexpr)
     (mapcar (lambda (x) (rewrite-logic-symbols x old-val new-val type)) sexpr))))

(defun substitute-in-string (source old-sub new-sub)
  "Helper to replace substring"
  (let ((pos (search old-sub source)))
    (if pos
        (concatenate 'string (subseq source 0 pos) new-sub (subseq source (+ pos (length old-sub))))
        source)))

(defun run-tests ()
  (format t "Running Mutation Logic Tests...~%")
  
  (let* ((logic '(and (> close sma-50) (cross-above sma-20 sma-50)))
         (mutated (rewrite-logic-symbols logic 50 60 "SMA")))
    (format t "Original: ~a~%" logic)
    (format t "Mutated (SMA 50->60): ~a~%" mutated)
    
    (if (equal mutated '(and (> close sma-60) (cross-above sma-20 sma-60)))
        (format t "✅ Test 1 Passed~%")
        (format t "❌ Test 1 Failed~%")))
        
  (let* ((logic '(and (> rsi-14 70)))
         (mutated (rewrite-logic-symbols logic 50 60 "SMA"))) ; Should not change
    (format t "Mutated (No Match): ~a~%" mutated)
    (if (equal mutated logic)
        (format t "✅ Test 2 Passed (No Change)~%")
        (format t "❌ Test 2 Failed~%"))))

(run-tests)
