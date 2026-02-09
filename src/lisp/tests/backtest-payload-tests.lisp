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

(defun option-some (key payload)
  "Return the single value from an Option<T> field encoded as a one-element list.

Guardian `--backtest-only` uses serde_lexpr, which expects Option<T> values as:
  - None: ()
  - Some(x): (x)
So the alist entry prints like: (data_id \"USDJPY_M1\") / (start_time 1700000000)."
  (let ((raw (field key payload)))
    (assert-true (listp raw) (format nil "~a should be an Option list" key))
    (assert-equal 1 (length raw) (format nil "~a should be Some(x) (one-element list)" key))
    (first raw)))

(defun option-none (key payload)
  "Assert that an Option<T> field is present and encoded as an empty list."
  (let ((entry (assoc key payload :test #'eq)))
    (assert-true entry (format nil "~a should be present as Option None" key))
    (assert-equal (list key) entry (format nil "~a should be None (empty list)" key))
    t))

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
            ;; Force inline-candles path (avoid file mode) to make test deterministic.
            (swimmy.school:request-backtest strat :candles candles :suffix "_IS")))
      (setf (symbol-function 'swimmy.school::send-zmq-msg) orig-send))
    (assert-not-nil captured "Expected backtest payload to be sent")
    (assert-true (char= (char captured 0) #\() "Payload should be S-expression")
    (let ((payload (parse-sexpr-payload captured)))
      (assert-equal "BACKTEST" (field 'action payload) "Expected action=BACKTEST")
      (assert-true (field 'candles payload) "Expected candles present (inline mode)")
      (assert-true (numberp (option-some 'timeframe payload))
                   "Expected timeframe Option<i64> to contain a number"))))

(deftest test-request-backtest-file-mode-wraps-optionals
  "request-backtest (file mode) should wrap Option fields as one-element lists."
  (let ((captured nil)
        (orig-send (symbol-function 'swimmy.school::send-zmq-msg)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::send-zmq-msg)
                (lambda (msg &key target)
                  (declare (ignore target))
                  (setf captured msg)))
          ;; Ensure the CSV exists so request-backtest takes the fast-path file mode.
          (let* ((symbol "UTSYM")
                 (csv-path (swimmy.core::swimmy-path (format nil "data/historical/~a_M1.csv" symbol))))
            (ensure-directories-exist csv-path)
            (with-open-file (s csv-path :direction :output :if-exists :supersede :if-does-not-exist :create)
              (format s "1700000000,1,1,1,1,1~%"))
            (let* ((strat (swimmy.school:make-strategy
                           :name "Test-BT-File"
                           :indicators '((sma 5) (sma 20))
                           :sl 0.001
                           :tp 0.002
                           :volume 0.01
                           :indicator-type "sma"
                           :timeframe 60
                           :symbol symbol))
                   (candles (list (swimmy.globals:make-candle
                                   :timestamp 1700000000
                                   :open 1.0 :high 1.0 :low 1.0 :close 1.0 :volume 1.0))))
              (swimmy.school:request-backtest strat :candles candles))))
      (setf (symbol-function 'swimmy.school::send-zmq-msg) orig-send))
    (assert-not-nil captured "Expected backtest payload to be sent")
    (let ((payload (parse-sexpr-payload captured)))
      (assert-true (stringp (option-some 'data_id payload)) "Expected data_id Option<String>")
      (assert-true (stringp (option-some 'candles_file payload)) "Expected candles_file Option<String>")
      (assert-true (numberp (option-some 'timeframe payload)) "Expected timeframe Option<i64>"))))

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
        (assert-true (numberp (option-some 'timeframe payload)) "Timeframe Option<i64> should be numeric"))
      (assert-true (stringp (option-some 'data_id payload)) "Expected data_id Option<String>")
      (assert-true (stringp (option-some 'candles_file payload)) "Expected candles_file Option<String>")
      (assert-true (numberp (option-some 'start_time payload)) "Expected start_time Option<i64>")
      (assert-true (numberp (option-some 'end_time payload)) "Expected end_time Option<i64>"))))

(deftest test-request-backtest-v2-emits-none-optionals
  "request-backtest-v2 should emit Option fields as empty lists when nil"
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
                        :name "Test-BT-V2-NONE"
                        :indicators '((sma 5) (sma 20))
                        :sl 0.001
                        :tp 0.002
                        :volume 0.01
                        :indicator-type "sma"
                        :timeframe 60
                        :symbol "USDJPY")))
            (swimmy.school::request-backtest-v2 strat)))
      (setf (symbol-function 'swimmy.school::send-zmq-msg) orig-send)
      (setf (symbol-function 'swimmy.school::fetch-swap-history) orig-fetch))
    (assert-not-nil captured "Expected V2 backtest payload to be sent")
    (let ((payload (parse-sexpr-payload captured)))
      (option-none 'start_time payload)
      (option-none 'end_time payload)
      (assert-true (numberp (option-some 'timeframe payload)) "Expected timeframe Option<i64>")
      (assert-true (stringp (option-some 'data_id payload)) "Expected data_id Option<String>")
      (assert-true (stringp (option-some 'candles_file payload)) "Expected candles_file Option<String>"))))

(deftest test-request-backtest-v2-emits-phase-range-id
  "request-backtest-v2 should emit phase and range_id when provided"
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
                        :name "Test-BT-V2-PHASE"
                        :indicators '((sma 5) (sma 20))
                        :sl 0.001
                        :tp 0.002
                        :volume 0.01
                        :indicator-type "sma"
                        :timeframe 60
                        :symbol "USDJPY")))
            (swimmy.school::request-backtest-v2 strat
                                                :start-date "2011.01.01"
                                                :end-date "2020.12.31"
                                                :phase "phase1"
                                                :range-id "P1")))
      (setf (symbol-function 'swimmy.school::send-zmq-msg) orig-send)
      (setf (symbol-function 'swimmy.school::fetch-swap-history) orig-fetch))
    (assert-not-nil captured "Expected V2 backtest payload to be sent")
    (let ((payload (parse-sexpr-payload captured)))
      (assert-equal "phase1" (field 'phase payload) "Expected phase=phase1")
      (assert-equal "P1" (field 'range_id payload) "Expected range_id=P1"))))
