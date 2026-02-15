;;; strategy-migration-tests.lisp

(in-package :swimmy.tests)

(deftest test-normalize-legacy-plist->strategy
  "plist形式を strategy に変換して #S に再シリアライズする"
  (let* ((sexp "(:NAME \"Legacy-1\" :TIMEFRAME \"M5\" :DIRECTION :BOTH :SYMBOL \"USDJPY\" :SL 0.1 :TP 0.2 :SHARPE 0.3 :PROFIT-FACTOR 1.1 :WIN-RATE 45.0 :MAX-DD 0.1)")
         (result (swimmy.school::normalize-strategy-sexp
                  sexp
                  :indicators "(SMA-10)"
                  :entry "(> 1 0)"
                  :exit "(< 1 0)"))
         (strat (getf result :strategy))
         (out (getf result :sexp)))
    (assert-true (and strat (swimmy.school::strategy-p strat)) "Expected strategy instance")
    (assert-equal "Legacy-1" (strategy-name strat))
    (assert-equal 5 (strategy-timeframe strat))
    (assert-true
     (and (stringp out) (search "#S(STRATEGY" out))
     "Expected #S serialization")))

(deftest test-normalize-struct-roundtrip
  "#S形式はそのまま strategy として読み直せる"
  (let* ((s (swimmy.school:make-strategy :name "S-1" :symbol "USDJPY" :timeframe "H1"))
         (sexp (format nil "~s" s))
         (result (swimmy.school::normalize-strategy-sexp sexp))
         (strat (getf result :strategy))
         (out (getf result :sexp)))
    (assert-true
     (and (stringp out) (search "#S(STRATEGY" out))
     "Expected #S serialization")
    (assert-equal 60 (strategy-timeframe strat))))
