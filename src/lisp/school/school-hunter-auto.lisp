;;; school-hunter-auto.lisp
;;; ============================================================================
;;; AUTO-GENERATED STRATEGIES (via trigger_hunt.py)
;;; ============================================================================
;;; P9: Split from school-hunter.lisp for SRP compliance
;;; Expert Panel (Fowler): 1110行 → 分割
;;; ============================================================================

(in-package :swimmy.school)

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Scalp-1768098436
;;; Logic: RSI(12) + Bollinger(21, 2.2) Squeeze (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-scalp-1768098436 "Auto-Scalp-1768098436"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Scalp-1768098436"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((rsi 12) (bb 21 2.2))
   :entry '(and (< bb-width 0.0010) (> rsi 60) (> volume 0))
   :exit '(> pnl tp)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Scalp-1768098437
;;; Logic: RSI(12) + Bollinger(22, 2.1) Squeeze (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-scalp-1768098437 "Auto-Scalp-1768098437"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Scalp-1768098437"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((rsi 12) (bb 22 2.1))
   :entry '(and (< bb-width 0.0010) (> rsi 60) (> volume 0))
   :exit '(> pnl tp)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Scalp-1768098438
;;; Logic: RSI(16) + Bollinger(19, 2.0) Squeeze (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-scalp-1768098438 "Auto-Scalp-1768098438"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Scalp-1768098438"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((rsi 16) (bb 19 2.0))
   :entry '(and (< bb-width 0.0010) (> rsi 60) (> volume 0))
   :exit '(> pnl tp)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Scalp-1768098439
;;; Logic: RSI(16) + Bollinger(19, 2.0) Squeeze (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-scalp-1768098439 "Auto-Scalp-1768098439"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Scalp-1768098439"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((rsi 16) (bb 19 2.0))
   :entry '(and (< bb-width 0.0010) (> rsi 60) (> volume 0))
   :exit '(> pnl tp)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Breakout-1768098443
;;; Logic: DONCHIAN_BREAK(23) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-breakout-1768098443 "Auto-Breakout-1768098443"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Breakout-1768098443"
   :category :breakout
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((donchian 23))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Breakout-1768098444
;;; Logic: DONCHIAN_BREAK(24) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-breakout-1768098444 "Auto-Breakout-1768098444"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Breakout-1768098444"
   :category :breakout
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((donchian 24))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Breakout-1768098445
;;; Logic: DONCHIAN_BREAK(24) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-breakout-1768098445 "Auto-Breakout-1768098445"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Breakout-1768098445"
   :category :breakout
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((donchian 24))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Scalp-1768098446
;;; Logic: RSI(14) + Bollinger(22, 2.0) Squeeze (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-scalp-1768098446 "Auto-Scalp-1768098446"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Scalp-1768098446"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((rsi 14) (bb 22 2.0))
   :entry '(and (< bb-width 0.0010) (> rsi 60) (> volume 0))
   :exit '(> pnl tp)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Breakout-1768098446
;;; Logic: DONCHIAN_BREAK(20) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-breakout-1768098446 "Auto-Breakout-1768098446"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Breakout-1768098446"
   :category :breakout
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((donchian 20))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Scalp-1768098447
;;; Logic: MOMENTUM_SCALP_EMA(8)_RSI(6) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-scalp-1768098447 "Auto-Scalp-1768098447"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Scalp-1768098447"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((ema 9) (rsi 8))
   :entry '(and (> close ema) (> rsi 70))
   :exit '(or (> pnl tp) (< rsi 50))
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Breakout-1768098447
;;; Logic: DONCHIAN_BREAK(24) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-breakout-1768098447 "Auto-Breakout-1768098447"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Breakout-1768098447"
   :category :breakout
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((donchian 24))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Scalp-1768098448
;;; Logic: MOMENTUM_SCALP_EMA(9)_RSI(6) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-scalp-1768098448 "Auto-Scalp-1768098448"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Scalp-1768098448"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((ema 8) (rsi 8))
   :entry '(and (> close ema) (> rsi 70))
   :exit '(or (> pnl tp) (< rsi 50))
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Breakout-1768098448
;;; Logic: DONCHIAN_BREAK(19) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-breakout-1768098448 "Auto-Breakout-1768098448"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Breakout-1768098448"
   :category :breakout
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((donchian 19))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Scalp-1768098449
;;; Logic: MOMENTUM_SCALP_EMA(10)_RSI(6) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-scalp-1768098449 "Auto-Scalp-1768098449"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Scalp-1768098449"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((ema 10) (rsi 7))
   :entry '(and (> close ema) (> rsi 70))
   :exit '(or (> pnl tp) (< rsi 50))
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Breakout-1768098449
;;; Logic: DONCHIAN_BREAK(22) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-breakout-1768098449 "Auto-Breakout-1768098449"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Breakout-1768098449"
   :category :breakout
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((donchian 22))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Scalp-1768098450
;;; Logic: MOMENTUM_SCALP_EMA(10)_RSI(6) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-scalp-1768098450 "Auto-Scalp-1768098450"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Scalp-1768098450"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((ema 9) (rsi 6))
   :entry '(and (> close ema) (> rsi 70))
   :exit '(or (> pnl tp) (< rsi 50))
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Breakout-1768098450
;;; Logic: DONCHIAN_BREAK(25) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-breakout-1768098450 "Auto-Breakout-1768098450"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Breakout-1768098450"
   :category :breakout
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((donchian 25))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Scalp-1768098451
;;; Logic: MOMENTUM_SCALP_EMA(10)_RSI(7) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-scalp-1768098451 "Auto-Scalp-1768098451"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Scalp-1768098451"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((ema 9) (rsi 7))
   :entry '(and (> close ema) (> rsi 70))
   :exit '(or (> pnl tp) (< rsi 50))
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Breakout-1768098451
;;; Logic: DONCHIAN_BREAK(18) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-breakout-1768098451 "Auto-Breakout-1768098451"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Breakout-1768098451"
   :category :breakout
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((donchian 18))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Scalp-1768098452
;;; Logic: MOMENTUM_SCALP_EMA(9)_RSI(7) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-scalp-1768098452 "Auto-Scalp-1768098452"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Scalp-1768098452"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((ema 8) (rsi 7))
   :entry '(and (> close ema) (> rsi 70))
   :exit '(or (> pnl tp) (< rsi 50))
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Breakout-1768098452
;;; Logic: DONCHIAN_BREAK(18) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-breakout-1768098452 "Auto-Breakout-1768098452"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Breakout-1768098452"
   :category :breakout
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((donchian 18))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Scalp-1768098453
;;; Logic: MOMENTUM_SCALP_EMA(8)_RSI(6) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-scalp-1768098453 "Auto-Scalp-1768098453"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Scalp-1768098453"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((ema 8) (rsi 6))
   :entry '(and (> close ema) (> rsi 70))
   :exit '(or (> pnl tp) (< rsi 50))
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Breakout-1768098453
;;; Logic: DONCHIAN_BREAK(22) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-breakout-1768098453 "Auto-Breakout-1768098453"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Breakout-1768098453"
   :category :breakout
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((donchian 22))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Scalp-1768098454
;;; Logic: RSI(14) + Bollinger(21, 1.8) Squeeze (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-scalp-1768098454 "Auto-Scalp-1768098454"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Scalp-1768098454"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((rsi 14) (bb 21 1.8))
   :entry '(and (< bb-width 0.0010) (> rsi 60) (> volume 0))
   :exit '(> pnl tp)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Scalp-1768098455
;;; Logic: RSI(15) + Bollinger(22, 2.1) Squeeze (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-scalp-1768098455 "Auto-Scalp-1768098455"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Scalp-1768098455"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((rsi 15) (bb 22 2.1))
   :entry '(and (< bb-width 0.0010) (> rsi 60) (> volume 0))
   :exit '(> pnl tp)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Scalp-1768098456
;;; Logic: RSI(14) + Bollinger(22, 1.9) Squeeze (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-scalp-1768098456 "Auto-Scalp-1768098456"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Scalp-1768098456"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((rsi 14) (bb 22 1.9))
   :entry '(and (< bb-width 0.0010) (> rsi 60) (> volume 0))
   :exit '(> pnl tp)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Breakout-1768098457
;;; Logic: DONCHIAN_BREAK(23) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-breakout-1768098457 "Auto-Breakout-1768098457"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Breakout-1768098457"
   :category :breakout
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((donchian 23))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Breakout-1768098458
;;; Logic: DONCHIAN_BREAK(20) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-breakout-1768098458 "Auto-Breakout-1768098458"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Breakout-1768098458"
   :category :breakout
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((donchian 20))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Breakout-1768098459
;;; Logic: DONCHIAN_BREAK(22) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-breakout-1768098459 "Auto-Breakout-1768098459"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Breakout-1768098459"
   :category :breakout
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((donchian 22))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Breakout-1768098460
;;; Logic: DONCHIAN_BREAK(19) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-breakout-1768098460 "Auto-Breakout-1768098460"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Breakout-1768098460"
   :category :breakout
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((donchian 19))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Breakout-1768098461
;;; Logic: DONCHIAN_BREAK(18) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-breakout-1768098461 "Auto-Breakout-1768098461"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Breakout-1768098461"
   :category :breakout
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((donchian 18))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Breakout-1768098462
;;; Logic: DONCHIAN_BREAK(25) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-breakout-1768098462 "Auto-Breakout-1768098462"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Breakout-1768098462"
   :category :breakout
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((donchian 25))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Breakout-1768098463
;;; Logic: DONCHIAN_BREAK(19) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-breakout-1768098463 "Auto-Breakout-1768098463"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Breakout-1768098463"
   :category :breakout
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((donchian 19))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Breakout-1768098464
;;; Logic: DONCHIAN_BREAK(20) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-breakout-1768098464 "Auto-Breakout-1768098464"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Breakout-1768098464"
   :category :breakout
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((donchian 20))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Breakout-1768098465
;;; Logic: DONCHIAN_BREAK(19) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-breakout-1768098465 "Auto-Breakout-1768098465"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Breakout-1768098465"
   :category :breakout
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((donchian 19))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Breakout-1768098466
;;; Logic: DONCHIAN_BREAK(21) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-breakout-1768098466 "Auto-Breakout-1768098466"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Breakout-1768098466"
   :category :breakout
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((donchian 21))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Breakout-1768098467
;;; Logic: DONCHIAN_BREAK(19) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-breakout-1768098467 "Auto-Breakout-1768098467"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Breakout-1768098467"
   :category :breakout
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((donchian 19))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)
   :exit '(or (> pnl tp) (< pnl (- sl)))))
;;; ----------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Scalp-1768100857
;;; Logic: MOMENTUM_SCALP_EMA(10)_RSI(7) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-scalp-1768100857 "Auto-Scalp-1768100857"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Scalp-1768100857"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((ema 9) (rsi 7))
   :entry '(and (> close ema) (> rsi 70))
   :exit '(or (> pnl tp) (< rsi 50))
   ))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Reversion-1768117681
;;; Logic: RSI_OVERSOLD(16) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-reversion-1768117681 "Auto-Reversion-1768117681"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Reversion-1768117681"
   :category :reversion
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((rsi 16))
   :entry '(< rsi 30)
   :exit '(> rsi 50)
   ))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Breakout-1768117681
;;; Logic: DONCHIAN_BREAK(21) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-breakout-1768117681 "Auto-Breakout-1768117681"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Breakout-1768117681"
   :category :breakout
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((donchian 21))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)
   ))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Scalp-1768117681
;;; Logic: MOMENTUM_SCALP_EMA(8)_RSI(8) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-scalp-1768117681 "Auto-Scalp-1768117681"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Scalp-1768117681"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((ema 9) (rsi 6))
   :entry '(and (> close ema) (> rsi 70))
   :exit '(or (> pnl tp) (< rsi 50))
   ))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Reversion-1768118399
;;; Logic: RSI_OVERSOLD(12) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-reversion-1768118399 "Auto-Reversion-1768118399"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Reversion-1768118399"
   :category :reversion
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((rsi 12))
   :entry '(< rsi 30)
   :exit '(> rsi 50)
   ))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Breakout-1768118399
;;; Logic: DONCHIAN_BREAK(24) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-breakout-1768118399 "Auto-Breakout-1768118399"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Breakout-1768118399"
   :category :breakout
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((donchian 24))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)
   ))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Scalp-1768118399
;;; Logic: MOMENTUM_SCALP_EMA(8)_RSI(7) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-scalp-1768118399 "Auto-Scalp-1768118399"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Scalp-1768118399"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((ema 8) (rsi 7))
   :entry '(and (> close ema) (> rsi 70))
   :exit '(or (> pnl tp) (< rsi 50))
   ))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Reversion-1768121043
;;; Logic: RSI_OVERSOLD(16) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-reversion-1768121043 "Auto-Reversion-1768121043"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Reversion-1768121043"
   :category :reversion
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((rsi 16))
   :entry '(< rsi 30)
   :exit '(> rsi 50)
   ))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Breakout-1768121043
;;; Logic: DONCHIAN_BREAK(22) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-breakout-1768121043 "Auto-Breakout-1768121043"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Breakout-1768121043"
   :category :breakout
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((donchian 22))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)
   ))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Scalp-1768121043
;;; Logic: MOMENTUM_SCALP_EMA(10)_RSI(7) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-scalp-1768121043 "Auto-Scalp-1768121043"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Scalp-1768121043"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((ema 9) (rsi 6))
   :entry '(and (> close ema) (> rsi 70))
   :exit '(or (> pnl tp) (< rsi 50))
   ))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Auto-Scalp-1768126518
;;; Logic: MOMENTUM_SCALP_EMA(10)_RSI(7) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :auto-scalp-1768126518 "Auto-Scalp-1768126518"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Auto-Scalp-1768126518"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((ema 10) (rsi 7))
   :entry '(and (> close ema) (> rsi 70))
   :exit '(or (> pnl tp) (< rsi 50))
   ))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Scalp-MOMENTUM-SCALP-EMA10-RSI8-1768126846
;;; Logic: MOMENTUM_SCALP_EMA(10)_RSI(8) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :scalp-momentum-scalp-ema10-rsi8-1768126846 "Scalp-MOMENTUM-SCALP-EMA10-RSI8-1768126846"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Scalp-MOMENTUM-SCALP-EMA10-RSI8-1768126846"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((ema 10) (rsi 6))
   :entry '(and (> close ema) (> rsi 70))
   :exit '(or (> pnl tp) (< rsi 50))
   :exit '(or (> pnl tp) (< pnl (- sl)))))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Scalp-MOMENTUM-SCALP-EMA9-RSI7-Gen0-1768130621
;;; Logic: MOMENTUM_SCALP_EMA(9)_RSI(7) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :scalp-momentum-scalp-ema9-rsi7-gen0-1768130621 "Scalp-MOMENTUM-SCALP-EMA9-RSI7-Gen0-1768130621"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Scalp-MOMENTUM-SCALP-EMA9-RSI7-Gen0-1768130621"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((ema 10) (rsi 7))
   :entry '(and (> close ema) (> rsi 70))
   :exit '(or (> pnl tp) (< rsi 50))
   :exit '(or (> pnl tp) (< pnl (- sl)))))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Scalp-MOMENTUM-SCALP-EMA9-RSI8-Gen0-2601112027
;;; Logic: MOMENTUM_SCALP_EMA(9)_RSI(8) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :scalp-momentum-scalp-ema9-rsi8-gen0-2601112027 "Scalp-MOMENTUM-SCALP-EMA9-RSI8-Gen0-2601112027"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Scalp-MOMENTUM-SCALP-EMA9-RSI8-Gen0-2601112027"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((ema 8) (rsi 8))
   :entry '(and (> close ema) (> rsi 70))
   :exit '(or (> pnl tp) (< rsi 50))
   :exit '(or (> pnl tp) (< pnl (- sl)))))
