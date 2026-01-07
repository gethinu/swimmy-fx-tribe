;;; src/lisp/tests/dreamer-tests.lisp
;;; Regression tests for Dreamer/Genesis modules
;;; Preventing recurrence of Critical Bugs (e.g., Sharpe -0.19)

(in-package :swimmy.tests)

(deftest test-resample-candles-regression
  "Regression Test: Verify resample-candles logic handles Open/Close correctly (Newest-First input)"
  ;; Input: Newest First (Time 5, 4, 3, 2, 1)
  (let* ((c5 (cl-user::make-candle :timestamp 5 :open 14 :close 15 :high 15 :low 14 :volume 1))
         (c4 (cl-user::make-candle :timestamp 4 :open 13 :close 14 :high 14 :low 13 :volume 1))
         (c3 (cl-user::make-candle :timestamp 3 :open 12 :close 13 :high 13 :low 12 :volume 1))
         (c2 (cl-user::make-candle :timestamp 2 :open 11 :close 12 :high 12 :low 11 :volume 1))
         (c1 (cl-user::make-candle :timestamp 1 :open 10 :close 11 :high 11 :low 10 :volume 1))
         (input (list c5 c4 c3 c2 c1))
         (resampled (cl-user::resample-candles input 5)))
    
    (assert-equal 1 (length resampled) "Should result in 1 candle")
    (let ((c (first resampled)))
      ;; For a time-series 1..5:
      ;; Open should be from T=1 (Oldest in batch) -> 10
      ;; Close should be from T=5 (Newest in batch) -> 15
      ;; Timestamp should be T=5 (Newest) -> 5
      
      (assert-equal 10 (cl-user::candle-open c) "Open should match Oldest candle (Start of period)")
      (assert-equal 15 (cl-user::candle-close c) "Close should match Newest candle (End of period)")
      (assert-equal 5 (cl-user::candle-timestamp c) "Timestamp should match Newest candle"))))
