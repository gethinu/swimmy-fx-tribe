# External Legend Source Audit

日付: 2026-03-07 JST

対象:

- `Legend-London-Breakout-V1`
- `Legend-RSI-Reversion-V1`

目的:

- `src/lisp/strategies/strategies-legendary.lisp`
- `src/lisp/strategies/legends.lisp`
- `data/library/LEGEND/*.lisp`
- `data/memory/swimmy.db`

の間で揺れている定義を比較し、MQ5 移植時の正本を固定する。

## 結論

- `Legend-London-Breakout-V1`
  - MQ5 / freeze 後 docs の正本は `data/library/LEGEND/Legend-London-Breakout-V1.lisp` と DB row に固定する。
  - `strategies-legendary.lisp` の定義は legacy seed とみなし、現行運用正本には使わない。
- `Legend-RSI-Reversion-V1`
  - MQ5 / freeze 後 docs の正本は `src/lisp/strategies/legends.lisp` と library / DB row に固定する。
  - `strategies-legendary.lisp` の定義は別世代の古い案として扱い、現行正本には使わない。

## 1. Legend-London-Breakout-V1

### source 比較

`strategies-legendary.lisp`:

- `session-high/session-low 8 16`
- `break-above session-high`
- `time-exit 24`
- `SL=0.10`, `TP=0.20`
- `timeframe=60`
- 参照: `src/lisp/strategies/strategies-legendary.lisp:26`

`legends.lisp`:

- active constructor 自体が存在しない
- `London Breakout` は `Fragile Time-Logic` として削除扱い
- 参照: `src/lisp/strategies/legends.lisp:24`

library:

- `TIME-RANGE 08:00-09:00`
- `BREAK-HIGH-LOW 08:00-09:00`
- `TIME-CLOSE 16:00`
- `SL=0.2`, `TP=0.6`
- `timeframe=3600`
- 参照: `data/library/LEGEND/Legend-London-Breakout-V1.lisp:1`

DB row:

- `indicators=((TIME-RANGE 08:00-09:00))`
- `entry=((BREAK-HIGH-LOW 08:00-09:00))`
- `exit=((TIME-CLOSE 16:00))`
- `symbol=USDJPY`, `direction=BOTH`, `timeframe=3600`
- metrics: `Sharpe=0.33899`, `PF=1.05310`, `Trades=194`, `MaxDD=0.00155`

### 判断

- `legends.lisp` には active source がなく、現行 runtime で summon される経路でもない。
- library と DB row はロジック・SL/TP・timeframe が一致している。
- DB metrics も library/DB 側の定義で回収されているため、MQ5 側はこれに合わせるのが最も監査可能。

### 決定

- 正本: `library + DB row`
- legacy reference only: `strategies-legendary.lisp`
- MQ5 採用仕様:
  - `08:00-09:00` レンジ
  - 上下ブレイクアウト
  - `16:00` 時間決済
  - `SL=0.20`, `TP=0.60`
  - 実装時間足は `H1` 相当として扱う

## 2. Legend-RSI-Reversion-V1

### source 比較

`strategies-legendary.lisp`:

- `RSI(2)`
- entry: `< rsi-2 5`
- exit: `> close sma-5`
- `SL=0.088`, `TP=0.091`
- `timeframe=1440`
- 参照: `src/lisp/strategies/strategies-legendary.lisp:19`

`legends.lisp`:

- `RSI(2)`
- entry: `(:rsi-below 10)`
- exit: `(:rsi-above 90)`
- `SL=0.10`, `TP=0.10`
- `timeframe=300`
- active summon path に載っている
- 参照: `src/lisp/strategies/legends.lisp:26`

library:

- `RSI(2)`
- `RSI-BELOW 10`
- `RSI-ABOVE 90`
- `SL=0.1`, `TP=0.1`
- `timeframe=300`
- 参照: `data/library/LEGEND/Legend-RSI-Reversion-V1.lisp:1`

DB row:

- `indicators=((RSI 2))`
- `entry=((RSI-BELOW 10))`
- `exit=((RSI-ABOVE 90))`
- `symbol=USDJPY`, `direction=BOTH`, `timeframe=300`
- metrics: `Sharpe=-0.16765`, `PF=0.97433`, `Trades=3216`, `MaxDD=0.00336`

### 判断

- `legends.lisp` と library / DB row が完全に一致している。
- `strategies-legendary.lisp` の定義だけが別物で、timeframe・entry/exit・SL/TP がすべてズレている。
- 実際に summon path を持つのは `legends.lisp` 側なので、現行正本は明確にこちら。

### 決定

- 正本: `legends.lisp + library + DB row`
- legacy reference only: `strategies-legendary.lisp`
- MQ5 採用仕様:
  - `RSI(2) < 10` で long entry
  - `RSI(2) > 90` で long exit
  - short 側は MQ5 実装上の対称推論
  - `SL=0.10`, `TP=0.10`
  - `M5`

## 3. freeze 後に残る cleanup

- `strategies-legendary.lisp` の stale 定義は、再開後に
  - 削除する
  - `legacy seed only` コメントを付ける
  - library / DB 正本へ寄せて更新する

のいずれかで明示的に処理した方がよい。

- 今回の MQ5 / docs では、実運用正本を変えず、監査上の決定だけを先に固定した。
