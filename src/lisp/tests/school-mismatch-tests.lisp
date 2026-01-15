;;; school-mismatch-tests.lisp - Tests for Symbol Mismatch Logic (P12.5)
;;; Part of Expert Panel P1 Implementation
;;; ═══════════════════════════════════════════════════

(in-package :swimmy.tests)

(deftest test-check-symbol-mismatch-blocks-cross-trading
  "Test that GBPUSD strategy cannot trade USDJPY"
  (let ((result (swimmy.school::check-symbol-mismatch "Wisdom-GBPUSD-Trend" "USDJPY")))
    (assert-true result "Should block GBPUSD strategy on USDJPY")))

(deftest test-check-symbol-mismatch-allows-correct-pair
  "Test that GBPUSD strategy CAN trade GBPUSD"
  (let ((result (swimmy.school::check-symbol-mismatch "Wisdom-GBPUSD-Trend" "GBPUSD")))
    (assert-false result "Should NOT block GBPUSD strategy on GBPUSD")))

(deftest test-check-symbol-mismatch-allows-generic
  "Test that generic strategy CAN trade any pair"
  (let ((result (swimmy.school::check-symbol-mismatch "MACD-RSI-Trend" "USDJPY")))
    (assert-false result "Should NOT block generic strategy on USDJPY")))

(deftest test-check-symbol-mismatch-blocks-eurusd-on-gbpusd
  "Test that EURUSD strategy cannot trade GBPUSD"
  (let ((result (swimmy.school::check-symbol-mismatch "Super-EURUSD-Scalp" "GBPUSD")))
    (assert-true result "Should block EURUSD strategy on GBPUSD")))

(deftest test-check-symbol-mismatch-case-insensitive
  "Test that mismatch check is case-insensitive"
  (let ((result (swimmy.school::check-symbol-mismatch "Wisdom-gbpusd-Trend" "usdjpy")))
    (assert-true result "Should block case-insensitive mismatch")))
