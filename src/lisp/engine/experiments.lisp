;;; ============================================================================
;;; engine/experiments.lisp - A/B Testing Framework
;;; ============================================================================
;;; Experiment tracking for strategy comparison
;;; Part of "The Efficient Gardener" refactoring
;;; ============================================================================

(in-package :swimmy.engine)

;;; ==========================================
;;; A/B TEST FRAMEWORK
;;; ==========================================

(defparameter *ab-tests* nil)
(defparameter *current-ab-test* nil)

(defstruct ab-test
  name
  variant-a      ; Configuration for A
  variant-b      ; Configuration for B
  current        ; :a or :b
  results-a      ; PnL list for A
  results-b      ; PnL list for B
  trades-a
  trades-b
  start-time
  status)        ; :running, :complete

(defun create-ab-test (name variant-a variant-b)
  "Create a new A/B test. Returns the test object."
  (let ((test (make-ab-test
               :name name
               :variant-a variant-a
               :variant-b variant-b
               :current :a
               :results-a nil
               :results-b nil
               :trades-a 0
               :trades-b 0
               :start-time (get-universal-time)
               :status :running)))
    (setf *current-ab-test* test)
    (push test *ab-tests*)
    (format t "[ENGINE] A/B test created: ~a~%" name)
    test))

(defun switch-ab-variant ()
  "Switch to the other variant."
  (when *current-ab-test*
    (setf (ab-test-current *current-ab-test*)
          (if (eq (ab-test-current *current-ab-test*) :a) :b :a))
    (format t "[ENGINE] A/B switched to variant ~a~%"
            (ab-test-current *current-ab-test*))))

(defun record-ab-result (pnl)
  "Record a result for current A/B test variant."
  (when *current-ab-test*
    (if (eq (ab-test-current *current-ab-test*) :a)
        (progn
          (push pnl (ab-test-results-a *current-ab-test*))
          (incf (ab-test-trades-a *current-ab-test*)))
        (progn
          (push pnl (ab-test-results-b *current-ab-test*))
          (incf (ab-test-trades-b *current-ab-test*))))))

(defun get-ab-test-results ()
  "Get current A/B test results as property list."
  (when *current-ab-test*
    (let* ((a-results (ab-test-results-a *current-ab-test*))
           (b-results (ab-test-results-b *current-ab-test*))
           (a-sum (reduce #'+ a-results :initial-value 0))
           (b-sum (reduce #'+ b-results :initial-value 0))
           (a-avg (if a-results (/ a-sum (length a-results)) 0))
           (b-avg (if b-results (/ b-sum (length b-results)) 0)))
      (list :name (ab-test-name *current-ab-test*)
            :winner (if (> a-avg b-avg) :a :b)
            :a-avg a-avg
            :b-avg b-avg
            :a-total a-sum
            :b-total b-sum
            :a-trades (ab-test-trades-a *current-ab-test*)
            :b-trades (ab-test-trades-b *current-ab-test*)))))

(defun complete-ab-test ()
  "Mark current A/B test as complete."
  (when *current-ab-test*
    (setf (ab-test-status *current-ab-test*) :complete)
    (let ((results (get-ab-test-results)))
      (format t "[ENGINE] A/B test complete: ~a, Winner: ~a~%"
              (getf results :name) (getf results :winner))
      results)))

(format t "[ENGINE] experiments.lisp loaded~%")
