# 📚 論文知見リファレンス

Based on 33 research papers (2024-2025)

## 実装済み論文

| # | タイトル | 実装ファイル | 主要関数 |
|---|----------|-------------|----------|
| 1 | Verbalized Sampling | dreamer2.lisp | `dream-with-vs`, `vs-diversity-score` |
| 2 | Kalman Filter | dsl.lisp | `ind-kalman`, `ind-kalman-trend` |
| 6 | TradingAgents | mixseek.lisp | `run-competition-round` |
| 13 | Dual Trend | research.lisp | `dual-trend-signal` |
| 17 | Vol Model Switch | research.lisp | `select-optimal-model` |
| 18 | Vol Rewards | research.lisp | `volatility-scaled-lot` |
| 16 | Kalman+HMM | research.lisp | `detect-regime-hmm` |
| 11 | Latent Params | research.lisp | `estimate-mean-reversion` |
| 10 | Ensemble | research.lisp | `ensemble-vote` |
| 27 | Normalization | research.lisp | `normalize-indicator` |
| 4 | Quality Check | research.lisp | `quality-check-strategy` |
| 9 | Zero-Shot Caution | research.lisp | `apply-zero-shot-caution` |
| 5 | Trading-R1 | llm-integration.lisp | `generate-investment-thesis` |
| 14 | PrimoGPT | llm-integration.lisp | `create-text-feature-prompt` |
| 28 | FinLlama | llm-integration.lisp | `integrate-sentiment` |
| 24 | SYENS | llm-integration.lisp | `match-expert-sketch` |

## 使用方法

### 統合分析
```lisp
(research-enhanced-analysis *candle-history*)
```

### LLM意思決定サポート
```lisp
(llm-enhanced-decision "USDJPY" analysis context)
```

### ボラティリティ調整ロット
```lisp
(volatility-scaled-lot 0.01 *candle-history*)
```

## 参考リソース

- #20: [Awesome Systematic Trading](https://github.com/wangzhe3224/awesome-systematic-trading)
- #21: [Time-Series-Library](https://github.com/thuml/Time-Series-Library)
- #32: [FinRL-Tutorials](https://github.com/AI4Finance-Foundation/FinRL-Tutorials)
- #33: [DQN-Trading](https://github.com/MehranTaghian/DQN-Trading)

## 将来参考・汎用

| # | タイトル | arXiv | 用途 |
|---|----------|-------|------|
| 30 | Large Causal Models from LLMs | [2512.07796](https://arxiv.org/abs/2512.07796) | 因果推論・意思決定支援 |

> **#30 DEMOCRITUS**: LLMから因果関係を抽出し、大規模因果モデル（LCM）を構築するシステム。
> ビジネス意思決定、戦略立案、複雑系の可視化に有用。
> トレーディング直接適用は低いが、AI戦略生成・知識ベース構築に参照価値あり。

---
*Last Updated: 2025-12-28*
