;;; src/lisp/tests/backtest-payload-tests.lisp
;;; Tests for backtest S-expression payload formatting

(in-package :swimmy.tests)

(defun normalize-whitespace (s)
  "Collapse all whitespace to single spaces for robust substring checks."
  (with-output-to-string (out)
    (let ((prev-space nil))
      (loop for ch across s do
        (if (find ch '(#\Space #\Tab #\Newline #\Return))
            (unless prev-space
              (write-char #\Space out)
              (setf prev-space t))
            (progn
              (write-char ch out)
              (setf prev-space nil)))))))

(defun payload-has-option-list (payload key)
  "Check that KEY is followed by a list value like '(key (value))'."
  (let* ((normalized (normalize-whitespace payload))
         (needle (format nil "~a (" key)))
    (search needle normalized)))

(deftest test-request-backtest-wraps-optionals
  "request-backtest should emit JSON with option fields as strings"
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
    (let ((json (jsown:parse captured)))
      (assert-equal "BACKTEST" (jsown:val json "action") "Expected action=BACKTEST")
      (assert-true (stringp (jsown:val json "data_id"))
                   "Expected data_id to be a string")
      (assert-true (stringp (jsown:val json "candles_file"))
                   "Expected candles_file to be a string")
      (assert-true (numberp (jsown:val json "timeframe"))
                   "Expected timeframe to be numeric"))))

(deftest test-request-backtest-single-line
  "request-backtest should emit a single-line JSON payload."
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
    (assert-true (char= (char captured 0) #\{)
                 "Expected JSON payload to start with '{'")
    (assert-true (not (or (find #\Newline captured)
                          (find #\Return captured)))
                 "Expected payload to be single-line (no newlines)")))

(deftest test-request-backtest-no-package-prefix
  "request-backtest should not leak package prefixes into JSON payload."
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
  "request-backtest-v2 should wrap option fields and send alist strategy"
  (let ((captured nil)
        (orig-send (symbol-function 'pzmq:send))
        (orig-fetch (symbol-function 'swimmy.school::fetch-swap-history)))
    (unwind-protect
        (progn
          (setf (symbol-function 'pzmq:send)
                (lambda (socket msg)
                  (declare (ignore socket))
                  (setf captured msg)))
          (setf (symbol-function 'swimmy.school::fetch-swap-history)
                (lambda (symbol &key start-ts end-ts)
                  (declare (ignore symbol start-ts end-ts))
                  nil))
          (let ((swimmy.globals:*cmd-publisher* t))
            (let ((strat (swimmy.school:make-strategy
                          :name "Test-BT-V2"
                          :indicators '((sma 5) (sma 20))
                          :sl 0.001
                          :tp 0.002
                          :volume 0.01
                          :indicator-type "sma"
                          :timeframe 60
                          :symbol "USDJPY")))
              (swimmy.school::request-backtest-v2 strat :start-date "2020.01.01" :end-date "2020.12.31"))))
      (setf (symbol-function 'pzmq:send) orig-send)
      (setf (symbol-function 'swimmy.school::fetch-swap-history) orig-fetch))
    (assert-not-nil captured "Expected V2 backtest payload to be sent")
    (assert-true (search "strategy (" (normalize-whitespace captured))
                 "Expected strategy to be an alist in S-expression")
    (assert-true (payload-has-option-list captured "timeframe")
                 "Expected timeframe to be wrapped as option list")
    (assert-true (payload-has-option-list captured "candles_file")
                 "Expected candles_file to be wrapped as option list")
    (assert-true (payload-has-option-list captured "data_id")
                 "Expected data_id to be wrapped as option list")
    (assert-true (payload-has-option-list captured "start_time")
                 "Expected start_time to be wrapped as option list")
    (assert-true (payload-has-option-list captured "end_time")
                 "Expected end_time to be wrapped as option list")
    ))
