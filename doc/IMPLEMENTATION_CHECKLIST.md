# 🟢 Swimmy V2.0 実装チェックリスト

**更新日**: 2025-12-27 23:25  
**目的**: 全機能の実装・呼び出し状況を追跡

---

## 凡例

- ❌ 未実装
- ⚠️ 定義あり・未呼出
- 🔶 部分実装
- ✅ 完了・動作確認済み

---

## 1. 🏛️ 部族システム

| 項目 | ファイル | 行 | 状態 |
|------|----------|-----|------|
| `*clans*` 4部族定義 | school.lisp | 874 | ✅ |
| `get-hunter-signal` MACD+ADX+Kalman | school.lisp | 2297 | ✅ |
| `get-shaman-signal` RSI+BB | school.lisp | 2335 | ✅ |
| `get-breaker-signal` ATR breakout | school.lisp | 2352 | ✅ |
| `get-raider-signal` EMA cross | school.lisp | 2371 | ✅ |
| `collect-all-tribe-signals` | school.lisp | 2391 | ✅ |
| `aggregate-tribe-signals` | school.lisp | 2398 | ✅ |
| 部族シグナル → トレード反映 | brain.lisp | 1893 | ✅ |
| `apply-hedge-logic` (Breakersヘッジ) | school.lisp | 2134 | ✅ |

---

## 2. 🎭 儀式・階級システム

| 項目 | ファイル | 行 | 状態 |
|------|----------|-----|------|
| `morning-ritual` | brain.lisp | 2408 | ✅ |
| `coming-of-age` | school.lisp | 989,999,1009 | ✅ |
| `hold-funeral` | school.lisp | 1741 | ✅ |
| `record-strategy-trade` → 儀式呼出 | school.lisp | 2197 | ✅ |
| `check-promotion` → coming-of-age | school.lisp | 1032 | ✅ |
| 階級別ロット倍率 | school.lisp | 2105 | ✅ |

---

## 3. 🧠 学習システム

| 項目 | ファイル | 行 | 状態 |
|------|----------|-----|------|
| `train-neural` (NN学習) | brain.lisp | 2036 | ✅ |
| トレード結果からNN学習 | brain.lisp | 1786-1789 | ✅ |
| クローズ時NN学習 | school.lisp | 2191 | ✅ |
| **パラメータ自動調整** | brain.lisp | 1955-1984 | ✅ NEW |
| - Sharpe < 0 → SL縮小 | brain.lisp | 1959-1964 | ✅ |
| - Sharpe > 1 → TP拡大 | brain.lisp | 1966-1971 | ✅ |
| - R:R < 2 → TP改善 | brain.lisp | 1973-1978 | ✅ |
| `evolve-population` (遺伝的進化) | brain.lisp | 1887 | ✅ |

---

## 4. 📚 論文実装 (research.lisp)

| 論文 | 関数 | 定義 | 呼出 | 状態 |
|------|------|------|------|------|
| #13 Dual Trend | `dual-trend-signal` | L42 | school.lisp:2315 | ✅ |
| #16 Kalman | `ind-kalman-trend` | dsl.lisp:87 | school.lisp:2313 | ✅ |
| #17 Model Switch | `select-optimal-model` | L98 | school.lisp:2219 | ✅ |
| #18 Vol Scale | `volatility-scaled-lot` | L158 | school.lisp:2103 | ✅ |
| HMM Regime | `detect-regime-hmm` | L222 | school.lisp:2221 | ✅ |
| Research Analysis | `research-enhanced-analysis` | L381 | school.lisp:2217 | ✅ |
| Mean Reversion | `estimate-mean-reversion` | L248 | research.lisp内 | ✅ |
| Ensemble Vote | `ensemble-vote` | L300 | research.lisp内 | ✅ |

---

## 5. 💹 戦略 (strategies.lisp)

| 項目 | 状態 |
|------|------|
| 61戦略定義 | ✅ |
| `build-category-pools` でロード | ✅ school.lisp:1126 |
| `evaluate-strategy-signal` で評価 | ✅ school.lisp:1185 |
| `collect-strategy-votes` で投票 | ✅ school.lisp:1250 |
| `swarm-trade-decision` で決定 | ✅ school.lisp:2224 |

**使用インジケータ分布**:
- SMA: 78, EMA: 72, RSI: 56, BB: 33
- MACD: 28, Stoch: 27, ATR: 8, CCI: 4

---

## 6. 🦀 Rust (guardian/)

| ファイル | 機能 | 状態 |
|----------|------|------|
| main.rs | 通信・コマンド処理 | ✅ |
| backtester.rs | バックテスト実行 | ✅ |
| neural.rs | NN推論 | ✅ |
| tournament.rs | トーナメント | ✅ |
| lstm.rs | LSTM推論 | ✅ |
| mcts.rs | MCTS探索 | ✅ |

**Lispから呼び出される機能**:
- BACKTEST (4箇所), EVOLVE (4箇所), PREDICT (4箇所), CLONE_CHECK (1箇所)

---

## 7. ✅ 全項目完了

| 項目 | 状態 |
|------|------|
| `contribute-to-treasury` | ✅ school.lisp:2201 |
| `ind-kalman-velocity` | ✅ get-raider-signal で使用 |
| TRIBE warn (rating) | 🔶 handler-caseで抑制 |
