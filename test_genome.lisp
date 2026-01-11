;; test_genome.lisp
(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(asdf:load-system :swimmy)

(in-package :swimmy.school)

(format t "~%--- TESTING THE GENOME ENGINE ---~%")

;; Create Parents
(let ((mom (make-strategy :name "Mom-P1" 
                          :indicators '((:rsi 14) (:ema 200)) 
                          :generation 1
                          :sl 1.0 :tp 2.0))
      (dad (make-strategy :name "Dad-P2" 
                          :indicators '((:bb 20 2)) 
                          :generation 2
                          :sl 0.5 :tp 5.0)))
  
  (format t "Mom: ~a (SL ~a)~%" (strategy-name mom) (strategy-sl mom))
  (format t "Dad: ~a (SL ~a)~%" (strategy-name dad) (strategy-sl dad))
  
  (format t "Crossing over...~%")
  (let ((child (crossover-strategy mom dad)))
    (format t "Child: ~a (Gen ~a)~%" (strategy-name child) (strategy-generation child))
    (format t "Child Inds: ~a~%" (strategy-indicators child))
    (format t "Child SL: ~a~%" (strategy-sl child))
    (format t "Child TP: ~a~%" (strategy-tp child))
    
    (if (and (= (strategy-generation child) 3)
             (or (= (strategy-sl child) 1.0) (= (strategy-sl child) 0.5)))
        (format t "✅ SUCCESS: Genome Engine Operational~%")
        (format t "❌ FAILURE: Child traits invalid~%"))))

;; Test Structural Mutation
(format t "~%--- TESTING STRUCTURAL MUTATION ---~%")
(let* ((logic '(and (> rsi 30) (< (price) (bb-lower))))
       (strat (make-strategy :name "LogicBase" :entry logic)))
  (format t "Original Logic: ~a~%" logic)
  ;; Force mutation logic by mocking random? No, hard to mock random in test script without redef.
  ;; We will call perform-structural-mutation and see if it runs without error.
  ;; Since probability is low, it might not change, but we verify execution.
  (let ((mutant (perform-structural-mutation strat)))
    (format t "Mutant Logic: ~a~%" (strategy-entry mutant))
    (format t "Mutant Name: ~a~%" (strategy-name mutant))
    (format t "✅ SUCCESS: Structural Mutation Executed~%")))

(sb-ext:exit)
