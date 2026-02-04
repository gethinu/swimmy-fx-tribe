(in-package :swimmy.school)

(defun manual-trigger-backtest (strategy-name)
  "Manually trigger a backtest for a named strategy."
  (let ((strat (find strategy-name *strategy-knowledge-base* :key #'strategy-name :test #'string=)))
    (if strat
        (progn
          (format t "[MANUAL] 🚀 Triggering Backtest for ~a...~%" strategy-name)
          ;; Force trades/sharpe to 0 to ensure it looks like a candidate if checked,
          ;; though request-backtest doesn't check eligibility, only the loop does.
          (request-backtest strat)
          (format t "[MANUAL] ✅ Request sent via ZMQ. Check logs for Dispatcher response.~%"))
        (format t "[MANUAL] ❌ Strategy not found in KB: ~a~%" strategy-name))))

;; Example Usage:
;; (manual-trigger-backtest "Any-Strategy-Name")

;; Find a candidate to test (one that hasn't been backtested)
(let ((candidate (find-if (lambda (s) (and (member (strategy-tier s) '(:incubator :scout))
                                           (= (or (strategy-trades s) 0) 0)))
                          *strategy-knowledge-base*)))
  (if candidate
      (manual-trigger-backtest (strategy-name candidate))
      (format t "[MANUAL] ⚠️ No unbacktested candidates found to test.~%")))
