;;; src/lisp/tests/pattern-similarity-gate-tests.lisp

(in-package :swimmy.tests)

(deftest test-pattern-gate-aligned
  "pattern gate should keep multiplier=1.0 when aligned and confident"
  (multiple-value-bind (mult reason)
      (swimmy.school::pattern-gate-multiplier :buy 0.70 0.20 0.10 :threshold 0.60 :mismatch-multiplier 0.70)
    (declare (ignore reason))
    (assert-equal 1.0 mult "Expected multiplier 1.0 when aligned")))

(deftest test-pattern-gate-disagree
  "pattern gate should reduce when disagreeing"
  (multiple-value-bind (mult reason)
      (swimmy.school::pattern-gate-multiplier :buy 0.10 0.80 0.10 :threshold 0.60 :mismatch-multiplier 0.70)
    (declare (ignore reason))
    (assert-equal 0.70 mult "Expected mismatch multiplier when disagreeing")))

(deftest test-pattern-gate-low-confidence
  "pattern gate should reduce when confidence is below threshold"
  (multiple-value-bind (mult reason)
      (swimmy.school::pattern-gate-multiplier :sell 0.40 0.59 0.01 :threshold 0.60 :mismatch-multiplier 0.70)
    (declare (ignore reason))
    (assert-equal 0.70 mult "Expected mismatch multiplier under low confidence")))

(deftest test-pattern-gate-flat
  "pattern gate should reduce when FLAT is dominant"
  (multiple-value-bind (mult reason)
      (swimmy.school::pattern-gate-multiplier :buy 0.10 0.10 0.80 :threshold 0.60 :mismatch-multiplier 0.70)
    (declare (ignore reason))
    (assert-equal 0.70 mult "Expected mismatch multiplier for FLAT regime")))

(deftest test-pattern-gate-missing-probs-fail-open
  "pattern gate should fail-open when probabilities are missing"
  (multiple-value-bind (mult reason)
      (swimmy.school::pattern-gate-multiplier :buy nil nil nil :threshold 0.60 :mismatch-multiplier 0.70)
    (declare (ignore reason))
    (assert-equal 1.0 mult "Expected fail-open multiplier 1.0 when missing probs")))

(deftest test-pattern-gate-apply-soft-gate
  "apply-pattern-similarity-gate should scale lot when service disagrees"
  (flet ((mock-query (&rest args)
           (declare (ignore args))
           ;; Simulate a confident DOWN regime.
           '((type . "PATTERN_SIMILARITY_RESULT")
             (status . "ok")
             (result . ((p_up . 0.10) (p_down . 0.80) (p_flat . 0.10))))))
    (multiple-value-bind (new-lot mult reason)
        (swimmy.school::apply-pattern-similarity-gate
         "USDJPY" "H1" :buy 0.05 (make-list 120) :query-fn #'mock-query :threshold 0.60 :mismatch-multiplier 0.70)
      (declare (ignore reason))
      (assert-true (< (abs (- 0.035 new-lot)) 1e-6) "Expected lot scaled by 0.7")
      (assert-true (< (abs (- 0.70 mult)) 1e-6) "Expected multiplier 0.7"))))

(deftest test-pattern-similarity-query-request-sexp
  "pattern similarity query request should be S-expression with required keys"
  (let* ((c (swimmy.core:make-candle :timestamp 1709234560 :open 1.0 :high 1.1 :low 0.9 :close 1.05 :volume 10))
         (msg (swimmy.core::build-pattern-similarity-query-request "USDJPY" "H1" (list c) :k 30)))
    (assert-true (search "(type . \"PATTERN_SIMILARITY\")" msg) "Expected type")
    (assert-true (search "(schema_version . 1)" msg) "Expected schema_version")
    (assert-true (search "(action . \"QUERY\")" msg) "Expected action")
    (assert-true (search "(symbol . \"USDJPY\")" msg) "Expected symbol")
    (assert-true (search "(timeframe . \"H1\")" msg) "Expected timeframe")
    (assert-true (search "(candles" msg) "Expected candles")))

(deftest test-pattern-similarity-query-retries-once-after-error
  "pattern-similarity-query should reconnect and retry once after transient error."
  (let* ((orig-ensure (symbol-function 'swimmy.core::ensure-pattern-similarity-connection))
         (orig-send (symbol-function 'pzmq:send))
         (orig-recv (symbol-function 'pzmq:recv-string))
         (orig-close (symbol-function 'pzmq:close))
         (orig-socket swimmy.core::*pattern-similarity-socket*)
         (orig-context swimmy.core::*pattern-similarity-context*)
         (recv-count 0)
         (close-count 0))
    (unwind-protect
        (progn
          (setf swimmy.core::*pattern-similarity-context* :ut-ctx)
          (setf swimmy.core::*pattern-similarity-socket* nil)
          (setf (symbol-function 'swimmy.core::ensure-pattern-similarity-connection)
                (lambda ()
                  (setf swimmy.core::*pattern-similarity-socket* :ut-sock)))
          (setf (symbol-function 'pzmq:send)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  t))
          (setf (symbol-function 'pzmq:recv-string)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (incf recv-count)
                  (if (= recv-count 1)
                      (error "C error EAGAIN: Resource temporarily unavailable.")
                      "((status . \"ok\") (result . ((p_up . 0.6) (p_down . 0.2) (p_flat . 0.2))))")))
          (setf (symbol-function 'pzmq:close)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (incf close-count)
                  t))
          (let* ((c (swimmy.core:make-candle :timestamp 1709234560 :open 1.0 :high 1.1 :low 0.9 :close 1.05 :volume 10))
                 (resp (swimmy.core::pattern-similarity-query "USDJPY" "H1" (list c) :k 30)))
            (assert-not-nil resp "Expected retry to recover response")
            (assert-equal "ok" (swimmy.core:sexp-alist-get resp 'status))
            (assert-equal 2 recv-count "Expected one retry after initial failure")
            (assert-equal 1 close-count "Expected socket reset after first failure")))
      (setf (symbol-function 'swimmy.core::ensure-pattern-similarity-connection) orig-ensure)
      (setf (symbol-function 'pzmq:send) orig-send)
      (setf (symbol-function 'pzmq:recv-string) orig-recv)
      (setf (symbol-function 'pzmq:close) orig-close)
      (setf swimmy.core::*pattern-similarity-socket* orig-socket)
      (setf swimmy.core::*pattern-similarity-context* orig-context))))

(deftest test-pattern-similarity-query-returns-nil-after-retry-failure
  "pattern-similarity-query should return NIL when retry also fails."
  (let* ((orig-ensure (symbol-function 'swimmy.core::ensure-pattern-similarity-connection))
         (orig-send (symbol-function 'pzmq:send))
         (orig-recv (symbol-function 'pzmq:recv-string))
         (orig-close (symbol-function 'pzmq:close))
         (orig-socket swimmy.core::*pattern-similarity-socket*)
         (orig-context swimmy.core::*pattern-similarity-context*)
         (recv-count 0)
         (close-count 0))
    (unwind-protect
        (progn
          (setf swimmy.core::*pattern-similarity-context* :ut-ctx)
          (setf swimmy.core::*pattern-similarity-socket* nil)
          (setf (symbol-function 'swimmy.core::ensure-pattern-similarity-connection)
                (lambda ()
                  (setf swimmy.core::*pattern-similarity-socket* :ut-sock)))
          (setf (symbol-function 'pzmq:send)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  t))
          (setf (symbol-function 'pzmq:recv-string)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (incf recv-count)
                  (error "C error EAGAIN: Resource temporarily unavailable.")))
          (setf (symbol-function 'pzmq:close)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (incf close-count)
                  t))
          (let* ((c (swimmy.core:make-candle :timestamp 1709234560 :open 1.0 :high 1.1 :low 0.9 :close 1.05 :volume 10))
                 (resp (swimmy.core::pattern-similarity-query "USDJPY" "H1" (list c) :k 30)))
            (assert-true (null resp) "Expected NIL after retry exhaustion")
            (assert-equal 2 recv-count "Expected two attempts total")
            (assert-equal 2 close-count "Expected socket reset on both failed attempts")))
      (setf (symbol-function 'swimmy.core::ensure-pattern-similarity-connection) orig-ensure)
      (setf (symbol-function 'pzmq:send) orig-send)
      (setf (symbol-function 'pzmq:recv-string) orig-recv)
      (setf (symbol-function 'pzmq:close) orig-close)
      (setf swimmy.core::*pattern-similarity-socket* orig-socket)
      (setf swimmy.core::*pattern-similarity-context* orig-context))))
