(in-package :cl-user)
(load "src/lisp/packages.lisp")
(load "src/lisp/core/globals.lisp")
(load "src/lisp/engine/metrics.lisp")
(load "src/lisp/school/school-analytics.lisp")

(defpackage :swimmy.benchmark
  (:use :cl :swimmy.school :swimmy.globals))
(in-package :swimmy.benchmark)

(defun run-sharpe-benchmark ()
  (format t "~%--- SHARPE RATIO BENCHMARK ---~%")
  (let ((returns (loop repeat 100 collect (- (random 100.0) 45.0)))) ;; Random returns
    (time 
     (loop repeat 10000 do
       (swimmy.engine::calculate-sharpe-ratio returns)))))

(defun run-logic-benchmark ()
  (format t "~%--- LOGIC PROCESSING BENCHMARK ---~%")
  (let ((candle (make-candle :open 100 :high 101 :low 99 :close 100.5 :timestamp 123456)))
    (time
     (loop repeat 100000 do
       (list candle)))))

(defun run-all ()
  (format t "Starting Swimmy Benchmarks...~%")
  (run-sharpe-benchmark)
  (run-logic-benchmark)
  (format t "Benchmark Complete.~%"))

(run-all)
