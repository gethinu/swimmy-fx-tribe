;; test.lisp - Swimmy V5.0 Unit Tests
;; Run with: sbcl --script test.lisp

(load "brain.lisp" :if-does-not-exist nil)

(defvar *test-passed* 0)
(defvar *test-failed* 0)

(defmacro deftest (name &body body)
  `(handler-case
       (progn
         (format t "~%[TEST] ~a... " ,name)
         (if (progn ,@body)
             (progn
               (format t "✅ PASS")
               (incf *test-passed*))
             (progn
               (format t "❌ FAIL")
               (incf *test-failed*))))
     (error (e)
       (format t "❌ ERROR: ~a" e)
       (incf *test-failed*))))

;; ===== DSL TESTS =====

(deftest "ind-sma returns number"
  (let ((history (make-list 20 :initial-element (make-candle :close 100.0))))
    (numberp (ind-sma 10 history))))

(deftest "ind-ema returns number"
  (let ((history (make-list 20 :initial-element (make-candle :close 100.0))))
    (numberp (ind-ema 10 history))))

(deftest "ind-rsi returns 0-100"
  (let* ((up (make-candle :close 101.0))
         (down (make-candle :close 99.0))
         (history (list up down up down up down up down up down up down up down up down)))
    (let ((rsi (ind-rsi 14 history)))
      (and (numberp rsi) (>= rsi 0) (<= rsi 100)))))

(deftest "cross-above detects crossover"
  (cross-above 51 50 49 50))  ; 49<50 -> 51>50

(deftest "cross-above rejects non-crossover"
  (not (cross-above 51 50 51 50)))  ; 51>50 -> 51>50 (no cross)

(deftest "cross-below detects crossover"
  (cross-below 49 50 51 50))  ; 51>50 -> 49<50

;; ===== STRATEGY TESTS =====

(deftest "infer-strategy-category returns keyword"
  (let ((result (infer-strategy-category 
                  (make-strategy :name "Test-SMA" :indicators '((sma 10) (sma 20))))))
    (keywordp result)))

(deftest "get-category-lot returns positive number"
  (let ((lot (get-category-lot :trend)))
    (and (numberp lot) (> lot 0))))

;; ===== SCHOOL TESTS =====

(deftest "can-clan-trade-p returns boolean"
  (let ((result (can-clan-trade-p :test-clan)))
    (or (eq result t) (eq result nil))))

(deftest "total-exposure-allowed-p returns boolean"
  (let ((result (total-exposure-allowed-p)))
    (or (eq result t) (eq result nil))))

;; ===== BRAIN TESTS =====

(deftest "current-trading-session returns keyword"
  (let ((session (current-trading-session)))
    (member session '(:tokyo :london :ny :closed))))

(deftest "get-volatility-state returns keyword"
  (let ((result (get-volatility-state)))
    (keywordp result)))

;; ===== SUMMARY =====

(format t "~%~%═══════════════════════════════════════")
(format t "~%TEST RESULTS: ~d passed, ~d failed~%" *test-passed* *test-failed*)
(format t "═══════════════════════════════════════~%")

(if (zerop *test-failed*)
    (format t "✅ ALL TESTS PASSED!~%")
    (format t "❌ SOME TESTS FAILED~%"))
