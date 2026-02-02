;;; src/lisp/tests/backtest-payload-tests.lisp
;;; Tests for backtest S-expression payload formatting

(in-package :swimmy.tests)

(defun parse-sexpr-payload (payload-str)
  "Read the S-expression payload emitted by backtest requests."
  (let ((*package* (find-package :swimmy.tests)))
    (multiple-value-bind (form _pos)
        (read-from-string payload-str)
      (declare (ignore _pos))
      form)))

(defun field (key payload)
  (cdr (assoc key payload :test #'eq)))

(deftest test-request-backtest-emits-sexpr
  "request-backtest should emit an S-expression payload with action BACKTEST."
  (let ((captured nil)
        (orig-send (symbol-function 'swimmy.school::send-zmq-msg)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::send-zmq-msg)
                (lambda (msg &key target)
                  (declare (ignore target))
                  (setf captured msg)))
          (let* ((strat (swimmy.school:make-strategy
                         :name "Test-BT"
                         :indicators '((sma 5) (sma 20))
                         :sl 0.001
                         :tp 0.002
                         :volume 0.01
                         :indicator-type "sma"
                         :timeframe 60
                         :symbol "USDJPY"))
                 (candles (list (swimmy.globals:make-candle
                                 :timestamp 1700000000
                                 :open 1.0 :high 1.0 :low 1.0 :close 1.0 :volume 1.0))))
            (swimmy.school:request-backtest strat :candles candles)))
      (setf (symbol-function 'swimmy.school::send-zmq-msg) orig-send))
    (assert-not-nil captured "Expected backtest payload to be sent")
    (assert-true (char= (char captured 0) #\() "Payload should be S-expression")
    (let ((payload (parse-sexpr-payload captured)))
      (assert-equal "BACKTEST" (field 'action payload) "Expected action=BACKTEST")
      (assert-true (or (field 'candles_file payload) (field 'candles payload))
                   "Expected either candles_file or candles present")
      (assert-true (numberp (field 'timeframe payload))
                   "Expected timeframe to be numeric"))))

(deftest test-request-backtest-indicator-type-symbol
  "request-backtest should emit indicator_type as a symbol for Guardian S-exp parsing."
  (let ((captured nil)
        (orig-send (symbol-function 'swimmy.school::send-zmq-msg)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::send-zmq-msg)
                (lambda (msg &key target)
                  (declare (ignore target))
                  (setf captured msg)))
          (let* ((strat (swimmy.school:make-strategy
                         :name "Test-BT-Indicator"
                         :indicators '((sma 5) (sma 20))
                         :sl 0.001
                         :tp 0.002
                         :volume 0.01
                         :indicator-type "sma"
                         :timeframe 60
                         :symbol "USDJPY"))
                 (candles (list (swimmy.globals:make-candle
                                 :timestamp 1700000000
                                 :open 1.0 :high 1.0 :low 1.0 :close 1.0 :volume 1.0))))
            (swimmy.school:request-backtest strat :candles candles)))
      (setf (symbol-function 'swimmy.school::send-zmq-msg) orig-send))
    (assert-not-nil captured "Expected backtest payload to be sent")
    (let* ((payload (parse-sexpr-payload captured))
           (strategy (field 'strategy payload))
           (indicator (field 'indicator_type strategy)))
      (assert-true (symbolp indicator) "Expected indicator_type to be a symbol")
      (assert-true (string-equal "sma" (symbol-name indicator))
                   "Expected indicator_type to be SMA symbol"))))

(deftest test-request-backtest-single-line
  "request-backtest should emit a single-line S-expression payload."
  (let ((captured nil)
        (orig-send (symbol-function 'swimmy.school::send-zmq-msg)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::send-zmq-msg)
                (lambda (msg &key target)
                  (declare (ignore target))
                  (setf captured msg)))
          (let* ((strat (swimmy.school:make-strategy
                         :name "Test-BT-Line"
                         :indicators '((sma 5) (sma 20))
                         :sl 0.001
                         :tp 0.002
                         :volume 0.01
                         :indicator-type "sma"
                         :timeframe 60
                         :symbol "USDJPY"))
                 (candles (list (swimmy.globals:make-candle
                                 :timestamp 1700000000
                                 :open 1.0 :high 1.0 :low 1.0 :close 1.0 :volume 1.0))))
            (swimmy.school:request-backtest strat :candles candles)))
      (setf (symbol-function 'swimmy.school::send-zmq-msg) orig-send))
    (assert-not-nil captured "Expected backtest payload to be sent")
    (assert-true (char= (char captured 0) #\()
                 "Expected payload to start with '('")
    (assert-true (not (or (find #\Newline captured)
                          (find #\Return captured)))
                 "Expected payload to be single-line (no newlines)")))

(deftest test-request-backtest-no-package-prefix
  "request-backtest should not leak package prefixes into S-expression payload."
  (let ((captured nil)
        (orig-send (symbol-function 'swimmy.school::send-zmq-msg)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::send-zmq-msg)
                (lambda (msg &key target)
                  (declare (ignore target))
                  (setf captured msg)))
          (let* ((strat (swimmy.school:make-strategy
                         :name "Test-BT-PKG"
                         :indicators '((sma 5) (sma 20))
                         :sl 0.001
                         :tp 0.002
                         :volume 0.01
                         :indicator-type "sma"
                         :timeframe 60
                         :symbol "USDJPY"))
                 (candles (list (swimmy.globals:make-candle
                                 :timestamp 1700000000
                                 :open 1.0 :high 1.0 :low 1.0 :close 1.0 :volume 1.0))))
            (swimmy.school:request-backtest strat :candles candles)))
      (setf (symbol-function 'swimmy.school::send-zmq-msg) orig-send))
    (assert-not-nil captured "Expected backtest payload to be sent")
    (assert-true (not (search "swimmy.school::" captured))
                 "Expected payload keys without package prefixes")))

(deftest test-request-backtest-v2-wraps-optionals
  "request-backtest-v2 should send S-expression payload with alist strategy"
  (let ((captured nil)
        (orig-send (symbol-function 'swimmy.school::send-zmq-msg))
        (orig-fetch (symbol-function 'swimmy.school::fetch-swap-history)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::send-zmq-msg)
                (lambda (msg &key target)
                  (declare (ignore target))
                  (setf captured msg)))
          (setf (symbol-function 'swimmy.school::fetch-swap-history)
                (lambda (symbol &key start-ts end-ts)
                  (declare (ignore symbol start-ts end-ts))
                  nil))
          (let ((strat (swimmy.school:make-strategy
                        :name "Test-BT-V2"
                        :indicators '((sma 5) (sma 20))
                        :sl 0.001
                        :tp 0.002
                        :volume 0.01
                        :indicator-type "sma"
                        :timeframe 60
                        :symbol "USDJPY")))
            (swimmy.school::request-backtest-v2 strat :start-date "2020.01.01" :end-date "2020.12.31")))
      (setf (symbol-function 'swimmy.school::send-zmq-msg) orig-send)
      (setf (symbol-function 'swimmy.school::fetch-swap-history) orig-fetch))
    (assert-not-nil captured "Expected V2 backtest payload to be sent")
    (assert-true (char= (char captured 0) #\() "Expected S-expression payload")
    (let ((payload (parse-sexpr-payload captured)))
      (let ((strategy (field 'strategy payload)))
        (assert-true (consp strategy) "Strategy should be an alist")
        (assert-equal "USDJPY" (field 'symbol payload) "Symbol should be USDJPY")
        (assert-true (numberp (field 'timeframe payload)) "Timeframe should be numeric"))
      (assert-true (field 'data_id payload) "Expected data_id present")
      (assert-true (field 'candles_file payload) "Expected candles_file present")
      (assert-true (field 'start_time payload) "Expected start_time present")
      (assert-true (field 'end_time payload) "Expected end_time present"))))
