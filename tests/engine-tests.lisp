;;; ============================================================================
;;; tests/engine-tests.lisp - Unit Tests for Engine Layer (Uncle Bob)
;;; ============================================================================
;;; "Without tests, we're just guessing." - Robert C. Martin
;;; ============================================================================

(in-package :cl-user)

;;; ==========================================
;;; TEST FRAMEWORK (Minimal)
;;; ==========================================

(defparameter *test-results* nil)
(defparameter *test-count* 0)
(defparameter *test-passed* 0)
(defparameter *test-failed* 0)

(defmacro deftest (name &body body)
  "Define a test case."
  `(progn
     (incf *test-count*)
     (handler-case
         (progn
           ,@body
           (incf *test-passed*)
           (push (list :pass ',name) *test-results*)
           (format t "[TEST] ✅ ~a~%" ',name))
       (error (e)
         (incf *test-failed*)
         (push (list :fail ',name e) *test-results*)
         (format t "[TEST] ❌ ~a: ~a~%" ',name e)))))

(defun assert-equal (expected actual &optional msg)
  "Assert that expected equals actual."
  (unless (equal expected actual)
    (error "~a: Expected ~a but got ~a" (or msg "Assertion failed") expected actual)))

(defun assert-true (value &optional msg)
  "Assert that value is true."
  (unless value
    (error "~a: Expected true but got ~a" (or msg "Assertion failed") value)))

(defun assert-nil (value &optional msg)
  "Assert that value is nil."
  (when value
    (error "~a: Expected nil but got ~a" (or msg "Assertion failed") value)))

;;; ==========================================
;;; TREASURY TESTS
;;; ==========================================

(deftest treasury-dynamic-threshold-default
  "Default volatility returns base threshold."
  (let ((*current-volatility-state* nil))
    (assert-equal 5000 (get-dynamic-treasury-threshold))))

(deftest treasury-dynamic-threshold-extreme
  "Extreme volatility returns 50% of base."
  (let ((*current-volatility-state* :extreme))
    (assert-equal 2500 (get-dynamic-treasury-threshold))))

(deftest treasury-dynamic-threshold-high
  "High volatility returns 75% of base."
  (let ((*current-volatility-state* :high))
    (assert-equal 3750 (get-dynamic-treasury-threshold))))

(deftest treasury-dynamic-threshold-low
  "Low volatility returns 150% of base."
  (let ((*current-volatility-state* :low))
    (assert-equal 7500 (get-dynamic-treasury-threshold))))

(deftest treasury-get-total-equity
  "Total equity = locked + accumulated."
  (let ((*locked-treasury* 1000)
        (*accumulated-pnl* 500))
    (assert-equal 1500 (get-total-equity))))

;;; ==========================================
;;; FAILSAFE TESTS
;;; ==========================================

(deftest failsafe-heartbeat-check-healthy
  "Heartbeat within timeout returns true."
  (let ((*last-heartbeat* (get-universal-time))
        (*heartbeat-timeout* 30))
    (assert-true (check-heartbeat))))

(deftest failsafe-heartbeat-check-dead
  "Heartbeat beyond timeout returns nil."
  (let ((*last-heartbeat* (- (get-universal-time) 60))
        (*heartbeat-timeout* 30))
    (assert-nil (check-heartbeat))))

;;; ==========================================
;;; LEDGER TESTS
;;; ==========================================

(deftest ledger-arm-to-plist
  "arm-to-plist converts correctly."
  (let* ((*arm-states* (make-hash-table))
         (arm (cons '(param1 param2) (cons 5 3)))
         (plist (arm-to-plist arm 0)))
    (assert-equal 0 (getf plist :index))
    (assert-equal '(param1 param2) (getf plist :params))
    (assert-equal 5 (getf plist :wins))
    (assert-equal 3 (getf plist :losses))))

;;; ==========================================
;;; RUN ALL TESTS
;;; ==========================================

(defun run-all-tests ()
  "Run all defined tests."
  (setf *test-results* nil
        *test-count* 0
        *test-passed* 0
        *test-failed* 0)
  
  (format t "~%[TEST] ═══════════════════════════════════════~%")
  (format t "[TEST] Running Engine Unit Tests...~%")
  (format t "[TEST] ═══════════════════════════════════════~%~%")
  
  ;; Treasury tests
  (treasury-dynamic-threshold-default)
  (treasury-dynamic-threshold-extreme)
  (treasury-dynamic-threshold-high)
  (treasury-dynamic-threshold-low)
  (treasury-get-total-equity)
  
  ;; Failsafe tests
  (failsafe-heartbeat-check-healthy)
  (failsafe-heartbeat-check-dead)
  
  ;; Ledger tests
  (ledger-arm-to-plist)
  
  (format t "~%[TEST] ═══════════════════════════════════════~%")
  (format t "[TEST] Results: ~d/~d passed (~,1f%)~%" 
          *test-passed* *test-count* 
          (* 100.0 (/ *test-passed* (max 1 *test-count*))))
  (format t "[TEST] ═══════════════════════════════════════~%")
  
  (zerop *test-failed*))

(format t "[TEST] engine-tests.lisp loaded. Run (run-all-tests) to execute.~%")
