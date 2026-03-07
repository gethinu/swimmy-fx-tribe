# Legend / Sランク棚卸し

日付: 2026-03-07 JST

## 1. ランタイム状況

- 2026-03-07 時点で、Swimmy 本体は凍結判断で停止済みです。
- 実測上も、`systemctl --type=service --state=running | rg 'swimmy|strategy_hunter'` では running hit は確認されませんでした。
- `ps` に残っていたのは補助プロセスだけです:
  - `tools/arxiv-scout/arxiv_scout.py --daemon`
  - `tools/inference_worker.py`
  - `tools/pending_manager.py`
- したがって、live runtime を前提にした作業はここで打ち止めにし、以降は offline で進められる MQ5 / doc 整備を優先します。
- 直近の report file は「停止前後の最新観測値」として保持します:
  - `data/reports/backtest_status.txt` 更新時刻 `2026-03-07 11:01 JST`: `count=40539`, `pending=0`
  - `data/reports/oos_status.txt` 更新時刻 `2026-03-07 14:48 JST`: `sent=0 retry=0 success=1739 failure=5 pending=0`
  - `data/reports/cpcv_status.txt` 更新時刻 `2026-03-07 14:51 JST`: `0 queued | 0 sent | 0 received | 0 failed`, `reason=no-candidates`

## 2. Legend 一覧

- DB 上で active な `:LEGEND`: 62
- 正本として意図されている Legend 集合: `src/lisp/strategies/legend-61.lisp` の 61 件
- `data/library/LEGEND` 内のファイル数: 64

### 重要な差分

- canonical set を超えて DB にだけあるもの:
  - `TEST-REFRESH-ACTIVE-SEXP-SYNC`
- library にだけあり、現行 DB では active ではないもの:
  - `Legend-London-Breakout-V1`
  - `Legend-RSI-Reversion-V1`

### 実務上の解釈

- MQ5 向けにエッジ抽出するなら、最も扱いやすい正本は `src/lisp/strategies/legend-61.lisp` の human-readable な Legend 群です。
- `TEST-REFRESH-ACTIVE-SEXP-SYNC` は test / maintenance 系の artifact に見えるので、売買エッジとして扱うべきではありません。
- 上の 2 本の `Legend-*` は disk 上には存在するので別途レビュー対象にはなりますが、現行 DB の active Legend set には入っていません。

## 3. MQ5 化の優先順位

「個別エッジとして MQ5 にしやすい」「trade count も最低限ある」という観点では、現状の優先候補は次です。

| 優先度 | 戦略 | TF | Sharpe | PF | Trades | 最初にやる理由 |
| --- | --- | --- | ---: | ---: | ---: | --- |
| 1 | `Perfect-Order-SMA` | M30 | 0.333 | 1.150 | 5415 | きれいな順張りテンプレートで、単純さとサンプル数のバランスが良い |
| 2 | `Simple-Momentum-Sync` | M30 | 0.265 | 1.106 | 5864 | 素直なモメンタム系で、trade 数も十分ある |
| 3 | `Sweet-Chariot-SMA-40` | M30 | 0.231 | 1.093 | 6710 | 件数が安定しており、MQ5 に表現しやすい |
| 4 | `Pullback-Breakout` | H1 | 0.230 | 1.084 | 7374 | positive な Legend 群の中で最も sample size が大きい |
| 5 | `Trend-Pullback-Entry` | M15 | 0.151 | 1.071 | 4871 | pullback の概念が単純で、それでも十分な件数がある |
| 6 | `MACD-Above-Zero-Cross` | H4 | 0.164 | 1.097 | 2351 | 標準的な MACD trend logic |
| 7 | `MACD-Signal-Cross` | H4 | 0.164 | 1.097 | 2351 | 同じ MACD ファミリーで、MT5 検証しやすい |
| 8 | `MACD-Zero-Cross-Long` | H4 | 0.149 | 1.088 | 2351 | もう一つの portable な MACD edge |
| 9 | `MACD-Expansion` | H4 | 0.141 | 1.085 | 2350 | continuation 系として移植しやすい |
| 10 | `Crossover-Plus-MACD` | D1 | 0.148 | 1.150 | 957 | 高時間足で、運用面でも単純 |

### 2026-03-07 freeze 時点の進捗

- この優先表のうち、active candidate として維持している named Legend は次の 7 本です:
  - `Perfect-Order-SMA`
  - `Simple-Momentum-Sync`
  - `Pullback-Breakout`
  - `Trend-Pullback-Entry`
  - `Sweet-Chariot-SMA-40`
  - `MACD-Above-Zero-Cross`
  - `MACD-Signal-Cross`
- external Legend 2 本も完了済みです:
  - `Legend-London-Breakout-V1`
  - `Legend-RSI-Reversion-V1`
- `MACD-Zero-Cross-Long` / `MACD-Expansion` / `Crossover-Plus-MACD`
  は実装と smoke までは実施したが、PF が弱いため dropped 扱いにし、source / tester artifact を削除した。
- surviving MQ5 artifact の compile は `22 / 22` 成功済みです:
  - 詳細: `doc/knowledge/legend_mt5_compile_report_20260307.md`
- external Legend の canonical decision も監査済みです:
  - 詳細: `doc/knowledge/legend_external_source_audit_20260307.md`
- `strategies-legendary.lisp` に残っていた external Legend の stale draft も、legacy note に整理済みです。
- したがって、この表の中で named Legend として未ポートの主候補は一旦なく、次にやるなら archive ではない active shortlist の深掘りが先です。

### 二軍候補

- `Bladerunner` (W1, Sharpe 0.143, PF 1.789, 53 trades)
- `Triple-EMA-Trend-Follow` (W1, Sharpe 0.141, PF 1.752, 53 trades)
- `Triple-Screen-Proxy` (W1, Sharpe 0.183, PF 1.577, 118 trades)
- `Fibonacci-EMA-Scalp` (W1, Sharpe 0.071, PF 1.186, 149 trades)

PF だけを見ると魅力はありますが、trade count がまだ少ないので、最初の MQ5 化対象よりは後ろに回すのが自然です。

### 最初にやらない方がいいもの

- M1 の Legend 群は、グループ全体として弱いです。
- 現在の M1 Legends 集計:
  - 20 strategies
  - average Sharpe 約 `-4.197`
  - average PF 約 `0.63`
  - average trades 約 `447973.9`
- つまり 1 分足 Legend の多くは、activity は非常に多い一方で、単独エッジとしては説得力が弱いです。

## 4. 現在の A ランク一覧

現在 DB で active な `:A` 戦略:

| 戦略 | TF | Sharpe | PF | WR | Trades | MaxDD |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| `Bred-Bred--550-Gen30-N3980176629-115` | H60 | 17.265 | 9.416 | 85.7% | 35 | 0.0116% |
| `Bred-Bred--437-Gen31-N3980235907-169` | H60 | 17.042 | 9.304 | 88.6% | 35 | 0.0119% |
| `Bred-Bred--914-Gen29-N3980188079-337` | H60 | 16.482 | 9.063 | 85.7% | 35 | 0.0116% |
| `Bred-Bred--867-Gen35-N3980223264-106` | H60 | 15.983 | 9.417 | 88.2% | 34 | 0.0122% |

注記:

- 数字だけ見れば非常に強いですが、opaque な名前の bred 個体です。
- DB から `indicators`, `entry`, `exit` を抽出すれば MQ5 候補にはできますが、human-readable な Legend set ほど即移植しやすくはありません。

## 5. 確認できた過去 S ランク名

現在 active な `:S` の件数は 0 です。

`doc/memo8.md` と `s_conformance_backup_20260217` から復元できた historical `:S` は次の 13 件です。

| 戦略 | 現在ランク | 現在 Sharpe | PF | Trades | OOS Sharpe | CPCV pass rate |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| `Bred-Bred--508-Gen32-N3980040829-808` | `:GRAVEYARD` | 0.390 | 9.769 | 35 | 18.142 | 0.378 |
| `Bred-Bred--187-Gen23-N3980038264-261` | `:GRAVEYARD` | 0.386 | 9.620 | 35 | 17.750 | 0.378 |
| `Bred-Bred--222-Gen30-N3980040329-718` | `:GRAVEYARD` | 17.113 | 8.926 | 35 | 17.113 | 0.378 |
| `Bred-Bred--436-Gen32-N3980040463-744` | `:GRAVEYARD` | 16.180 | 8.137 | 35 | 16.180 | 0.378 |
| `Bred-Bred--794-Gen32-N3980040593-767` | `:GRAVEYARD` | 16.089 | 8.010 | 35 | 16.089 | 0.422 |
| `Bred-Bred--723-Gen29-N3980038311-278` | `:GRAVEYARD` | 13.475 | 5.329 | 35 | 13.475 | 0.711 |
| `Bred-Bred--128-Gen28-N3980038170-239` | `:GRAVEYARD` | 13.421 | 5.904 | 34 | 13.421 | 0.333 |
| `Bred-Bred--940-Gen31-N3980039835-605` | `:GRAVEYARD` | 12.483 | 4.992 | 35 | 12.483 | 0.311 |
| `Bred-Bred--586-Gen29-N3980038495-317` | `:GRAVEYARD` | 12.287 | 4.817 | 34 | 12.287 | 0.400 |
| `Bred-Bred--458-Gen32-N3980040289-704` | `:GRAVEYARD` | 11.751 | 4.280 | 35 | 11.751 | 0.511 |
| `Bred-Bred--139-Gen11-N3979972567-6` | `:GRAVEYARD` | 0.214 | 1.625 | 160 | 0.000 | 0.000 |
| `Bred-Bred--139-Gen11-N3979972610-6` | `:GRAVEYARD` | 0.214 | 1.625 | 160 | 0.000 | 0.000 |
| `RECRUIT-RND-1768781166-12` | `:GRAVEYARD` | -0.080 | 0.986 | 317 | 0.000 | 0.000 |

### 実務上の解釈

- historical `:S` は、ほとんどが bred 由来で名前も opaque、しかも現在は graveyard 落ちしています。
- ただし、2026-03-07 の offline 作業で 13 件すべてについて自然言語化と MQ5 化までは完了しました。
- baseline の isolated Strategy Tester 実測も `tools/mt5_inventory_tester.py --job-set historical_s`
  で 13 件すべて実行済みです:
  - artifact: `data/reports/mt5/inventory_tester/run_20260307_064816/`
- shortlist の latest-window 追試も追加済みです:
  - artifact: `data/reports/mt5/inventory_tester/run_20260307_101456/`
  - `historical-s-bred940-trend-core` は early `2026` で回復
  - `legend-pullback-breakout` は early `2026` でも負け
  - `legend-macd-above-zero-cross` は依然主候補だが、latest window は `4` trades と薄い
- `timeframe=3600` 個体の扱いも未解決ではありません。
  移植コード側は `PERIOD_CURRENT` を使い、自動 runner 側は tester `Period=H1` を与える契約で統一済みです。
- したがって残っているのは reverse-engineering そのものではなく、強化検証と整理です:
  - full walk-forward をどこまで追加するか
  - parameter retune 実験をやるか
  - broker variance を確認するか
  - stale source を将来どこまで掃除するかの code cleanup 判断
- 速く MQ5 化を進めるなら、先に named Legend set から触るのが正解です。

## 6. 推奨する次の手順

MQ5 化そのもの、offline compile / audit、baseline tester 実測までは完了したので、
freeze 後の残タスクは次です。

1. `legend-macd-above-zero-cross` / `legend-pullback-breakout` / `historical-s-bred940-trend-core`
   を中心に、fixed-window の次として full walk-forward か rolling-window を追加する。
2. `batch-4` の 3 本は dropped 扱いを維持し、active comparison からは外す。
3. runtime が凍結のままなら、MQ5 / doc 一式をコミット単位で整理し、stale source の cleanup 境界を決める。
   - cleanup boundary note: `doc/knowledge/legend_mt5_cleanup_boundary_20260307.md`
