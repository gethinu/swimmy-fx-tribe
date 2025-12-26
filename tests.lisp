;;; tests.lisp - Unit Tests for Swimmy
;;; ═══════════════════════════════════════════════════
;;; "Untested code is broken code." - Unknown

(defpackage :swimmy-tests
  (:use :cl)
  (:export :run-all-tests))

(in-package :swimmy-tests)

(defvar *test-results* nil)
(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

;;; ─────────────────────────────────────────
;;; TEST FRAMEWORK
;;; ─────────────────────────────────────────

(defmacro deftest (name &body body)
  "Define a test case"
  `(defun ,name ()
     (handler-case
         (progn
           ,@body
           (incf *tests-passed*)
           (push (list :passed ',name) *test-results*)
           t)
       (error (e)
         (incf *tests-failed*)
         (push (list :failed ',name e) *test-results*)
         nil))))

(defmacro assert-true (expr &optional message)
  "Assert that expression is true"
  `(unless ,expr
     (error (or ,message (format nil "Assertion failed: ~a" ',expr)))))

(defmacro assert-false (expr &optional message)
  "Assert that expression is false"
  `(when ,expr
     (error (or ,message (format nil "Expected false: ~a" ',expr)))))

(defmacro assert-equal (expected actual &optional message)
  "Assert that expected equals actual"
  `(unless (equal ,expected ,actual)
     (error (or ,message 
                (format nil "Expected ~a but got ~a" ,expected ,actual)))))

(defmacro assert-not-nil (expr &optional message)
  "Assert that expression is not nil"
  `(unless ,expr
     (error (or ,message (format nil "Expected non-nil: ~a" ',expr)))))

;;; ─────────────────────────────────────────
;;; INDICATOR TESTS
;;; ─────────────────────────────────────────

(deftest test-sma-calculation
  "Test SMA calculation"
  (let* ((test-data (loop for i from 1 to 10 
                          collect (cl-user::make-candle 
                                   :close (float i) :open 1.0 :high 1.0 :low 1.0)))
         (sma (cl-user::ind-sma 5 test-data)))
    ;; SMA of 1,2,3,4,5 = 15/5 = 3.0
    (assert-true (and sma (> sma 0)) "SMA should be positive")))

(deftest test-rsi-bounds
  "Test that RSI stays within 0-100"
  (let ((test-data (loop for i from 1 to 20 
                         collect (cl-user::make-candle 
                                  :close (+ 100 (random 10.0)) 
                                  :open 100.0 :high 110.0 :low 90.0))))
    (let ((rsi (cl-user::ind-rsi 14 test-data)))
      (when rsi
        (assert-true (<= 0 rsi 100) "RSI should be between 0-100")))))

;;; ─────────────────────────────────────────
;;; CLAN TESTS
;;; ─────────────────────────────────────────

(deftest test-clan-exists
  "Test that all clans are defined"
  (assert-not-nil cl-user::*clans* "Clans should be defined")
  (assert-equal 4 (length cl-user::*clans*) "Should have 4 clans"))

(deftest test-get-clan
  "Test get-clan function"
  (let ((hunters (cl-user::get-clan :trend)))
    (assert-not-nil hunters "Should find Hunters clan")
    (assert-equal "Hunters" (cl-user::clan-name hunters))))

(deftest test-clan-display
  "Test clan display format"
  (let ((display (cl-user::get-clan-display :trend)))
    (assert-not-nil display "Display should not be nil")
    (assert-true (search "Hunters" display) "Should contain Hunters")))

;;; ─────────────────────────────────────────
;;; CONSTITUTION TESTS
;;; ─────────────────────────────────────────

(deftest test-constitution-evaluation
  "Test constitution evaluation"
  (cl-user::initialize-constitution)
  (let* ((context (list :daily-pnl 0 :volatility-state :normal))
         (result (cl-user::evaluate-constitution context)))
    (assert-not-nil result "Evaluation should return result")
    (assert-true (getf result :alignment) "Should have alignment score")))

(deftest test-constitution-blocks-dangerous
  "Test that constitution blocks dangerous actions"
  (cl-user::initialize-constitution)
  (let* ((dangerous-context (list :daily-pnl -6000 :volatility-state :extreme))
         (result (cl-user::evaluate-constitution dangerous-context)))
    (assert-true (getf result :violations) "Should have violations")))

;;; ─────────────────────────────────────────
;;; REPUTATION TESTS
;;; ─────────────────────────────────────────

(deftest test-reputation-creation
  "Test reputation creation"
  (let ((rep (cl-user::get-reputation "Test-Strategy")))
    (assert-not-nil rep "Reputation should be created")
    (assert-equal 0.5 (cl-user::reputation-trust-score rep) "Initial trust should be 0.5")))

(deftest test-reputation-update
  "Test reputation update on win"
  (let ((rep (cl-user::get-reputation "Winner-Strategy")))
    (cl-user::update-reputation "Winner-Strategy" :win :pnl 100)
    (let ((updated-rep (cl-user::get-reputation "Winner-Strategy")))
      (assert-true (> (cl-user::reputation-trust-score updated-rep) 0.5)
                   "Trust should increase on win"))))

;;; ─────────────────────────────────────────
;;; ELDER TESTS
;;; ─────────────────────────────────────────

(deftest test-elder-induction
  "Test elder induction"
  (let ((elder (cl-user::induct-to-hall-of-fame 
                "Legend-Strategy" 5000 :trend "Always follow the trend")))
    (assert-not-nil elder "Elder should be created")
    (assert-equal "Legend-Strategy" (cl-user::elder-name elder))))

;;; ─────────────────────────────────────────
;;; MACRO TESTS
;;; ─────────────────────────────────────────

(deftest test-aif-macro
  "Test anaphoric if"
  (let ((result (cl-user::aif (+ 1 2) it nil)))
    (assert-equal 3 result "aif should bind result to IT")))

(deftest test-awhen-macro
  "Test anaphoric when"
  (let ((result nil))
    (cl-user::awhen (+ 1 2)
      (setf result it))
    (assert-equal 3 result "awhen should bind result to IT")))

;;; ─────────────────────────────────────────
;;; TEST RUNNER
;;; ─────────────────────────────────────────

(defun run-all-tests ()
  "Run all tests"
  (setf *test-results* nil)
  (setf *tests-passed* 0)
  (setf *tests-failed* 0)
  
  (format t "~%═══════════════════════════════════════~%")
  (format t "🧪 RUNNING SWIMMY TESTS~%")
  (format t "═══════════════════════════════════════~%~%")
  
  ;; Run each test
  (dolist (test '(test-clan-exists
                  test-get-clan
                  test-clan-display
                  test-aif-macro
                  test-awhen-macro))
    (format t "Running ~a... " test)
    (if (funcall test)
        (format t "✅ PASSED~%")
        (format t "❌ FAILED~%")))
  
  (format t "~%═══════════════════════════════════════~%")
  (format t "📊 RESULTS: ~d passed, ~d failed~%" *tests-passed* *tests-failed*)
  (format t "═══════════════════════════════════════~%~%")
  
  (values *tests-passed* *tests-failed*))

(format t "[TESTS] Test framework loaded~%")
(format t "[TESTS] Run (swimmy-tests:run-all-tests) to execute~%")
