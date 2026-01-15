(defun check-symbol-mismatch (strategy-name symbol)
  "V44.3: Prevent GBPUSD strategies from trading USDJPY etc.
   Returns T if there is a mismatch (should block)."
  (let ((name (string-upcase strategy-name))
        (sym (string-upcase symbol)))
    ;; If strategy name explicitly lists a currency pair, ensuring it matches the current symbol
    (cond
      ;; If name contains the current symbol, it's definitely allowed
      ((search sym name) nil)
      ;; If name contains OTHER major pairs but NOT the current one, it's a mismatch
      ((search "USDJPY" name) (not (equal sym "USDJPY")))
      ((search "EURUSD" name) (not (equal sym "EURUSD")))
      ((search "GBPUSD" name) (not (equal sym "GBPUSD")))
      ((search "EURJPY" name) (not (equal sym "EURJPY")))
      ((search "GBPJPY" name) (not (equal sym "GBPJPY")))
      ((search "AUDUSD" name) (not (equal sym "AUDUSD")))
      ;; Default: If no pair specified in name, assume generic (allowed)
      (t nil))))

(format t "Testing Logic:~%")
(format t "Wisdom-GBPUSD on USDJPY: ~a (Expected T)~%" (check-symbol-mismatch "Wisdom-GBPUSD-Trend-123" "USDJPY"))
(format t "Wisdom-GBPUSD on GBPUSD: ~a (Expected NIL)~%" (check-symbol-mismatch "Wisdom-GBPUSD-Trend-123" "GBPUSD"))
(format t "Wisdom-Generic on USDJPY: ~a (Expected NIL)~%" (check-symbol-mismatch "Wisdom-Generic" "USDJPY"))
