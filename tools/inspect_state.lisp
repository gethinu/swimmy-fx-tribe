(format t "--- STATE INSPECTION ---~%")
(format t "*qual-expected-backtest-count*: ~a~%" 
        (if (boundp 'swimmy.globals:*qual-expected-backtest-count*)
            swimmy.globals:*qual-expected-backtest-count*
            "UNBOUND"))
(format t "*qual-backtest-results-buffer* length: ~a~%" 
        (if (boundp 'swimmy.globals:*qual-backtest-results-buffer*)
            (length swimmy.globals:*qual-backtest-results-buffer*)
            "UNBOUND"))
(format t "*rr-expected-backtest-count*: ~a~%" 
        (if (boundp 'swimmy.globals:*rr-expected-backtest-count*)
            swimmy.globals:*rr-expected-backtest-count*
            "UNBOUND"))
(format t "*rr-backtest-results-buffer* length: ~a~%" 
        (if (boundp 'swimmy.globals:*rr-backtest-results-buffer*)
            (length swimmy.globals:*rr-backtest-results-buffer*)
            "UNBOUND"))
(sb-ext:exit)
