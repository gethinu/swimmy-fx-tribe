# 🐟 Swimmy Ver 18 オーナーズガイド

**最終版:** 2026-01-17 (V45.0 - Full Spec Era)
**リーダー判断:** Elon Musk (Expert Panel Verified)

---

## 🚀 起動 / 停止 (Systemd)

```bash
# 全サービス起動 (Zombie Processの一掃を含む)
make run  # 推奨 (kill-zombies を自動実行)

# または個別起動
systemctl --user start swimmy-brain swimmy-guardian swimmy-data-keeper

# 全サービス停止
systemctl --user stop swimmy-brain swimmy-guardian swimmy-data-keeper

# ステータス確認
make status

# リアルタイムログ監視
make logs
```

---

## 🌊 Full Spec Implementation (V45.0 - Memo3 Complete)

2026-01-17、システムは「市場適応型トレーディングOS」としての全機能を実装しました。

### 1. 7つの市場状態 (Softmax Regime)
単純な「トレンド/レンジ」ではなく、**確率的な市場認識**を行います。
`detect-market-regime` は `(TrendMature: 0.8, RangeExpansion: 0.2)` のようなベクトルを出力を返します。

### 2. ケリー基準による資金管理 (Kelly Criterion)
ロット数は固定ではなく、戦略の「優位性（Edge）」に基づいて動的に計算されます。
- `f* = p - q/b` （勝率とリスクリワード比から算出）
- **Safety**: ハーフケリーを採用し、最大リスクを口座資金の10%に制限。

### 3. ベイズ適応 (Bayesian Adaptation)
戦略の信頼度は、ベイズ統計（Beta分布）を用いて更新されます。
少ないトレード数でも統計的に正しい信頼区間を推定し、"使えない戦略"を早期に見切ります。

---
## 🔒 アトミック予約システム (Atomic Allocation)

V44.2で「椅子取りゲーム (Musical Chairs)」バグを修正しました。
戦略は、トレード判断の **計算前** にスロットを物理的に確保します。

**フロー:**
1. `try-reserve-warrior-slot` が空きスロットを探す。
2. 空いていれば **即時予約** (Pending登録)。
3. 確保できた場合のみ、AI予測などの計算に進む。
4. 最終的にトレードしなかった場合、予約は即時解除される。

これにより、Magic Numberの衝突は物理的に発生しません。

---

## 🛑 段階的リスク冷却 (Tiered Cooldown)

以前の「Circuit Breaker」「Resignation」は統合されました。
損失が出るたびに、冷却期間（休止時間）が段階的に長くなります。

**ティア構成:**
`3m → 5m → 10m → 15m → 30m → 45m → 1h → 2h → 3h → 4h → EOD (強制終了)`

- **ペナルティ**: 負けるたびにティアが 1 上がり、冷却時間が伸びる。
- **回復**: 勝つとティアが 1 下がる。
- **EOD**: 最大ティアに達すると、その日は強制終了（辞表提出）。

---

## 📋 システム構成

```
┌─────────────────────────────────────────────────────┐
│  MT5 EA (SwimmyBridge Ver 15.2)                     │
│  - 3通貨ペア (USDJPY, EURUSD, GBPUSD)              │
│  - Multi-TF History Support                         │
│  - Strategy Transparency (Comment Field)            │
├─────────────────────────────────────────────────────┤
│  GUARDIAN (Rust)        Port 5557/5559/5560         │
│  - MT5通信、注文執行、バックテスト、RiskGate        │
├─────────────────────────────────────────────────────┤
│  BRAIN (Lisp)           Port 5555/5556 (PUB)        │
│  - シグナル生成、学習、Evolution、Heartbeat         │
├─────────────────────────────────────────────────────┤
│  DATA KEEPER (Python)   Port 5561                   │
│  - 10M Candle Buffer (Async Save)                   │
└─────────────────────────────────────────────────────┘
```

| サービス | systemd name | 役割 |
|----------|--------------|------|
| Brain | `swimmy-brain` | シグナル生成、学習、進化 |
| Guardian | `swimmy-guardian` | MT5通信、注文執行、バックテスト |
| Data Keeper | `swimmy-data-keeper` | ヒストリカルデータ永続化 |
| Evolution | `swimmy-evolution` | **Hyper-Time Evolution (無限進化ループ)**, Purge, Wisdom |

---

## 🧬 SRP モジュール構成 (V44.2)

`school-execution.lisp` もリファクタリングされ、全ファイルがSRP準拠（600行以下）です。

```
src/lisp/
├── school.lisp                    (33行) オーケストレーター
├── school/
│   ├── school-allocation.lisp    (Atomic Allocation)
│   ├── school-execution.lisp     (Atomic Execution)
│   ├── school-danger.lisp        (Tiered Cooldown)
│   └── ...
```

---

## 🛡️ EA (SwimmyBridge) Ver 15.2

MT5のExpertsフォルダにコピーし、コンパイルして適用してください。

**新機能:**
- ✅ マルチタイムフレーム履歴対応 (`REQ_HISTORY` + `tf` パラメータ)
- ✅ W1, D1, H4, H1, M30, M15, M5, M1 対応
- ✅ 戦略名 (Comment) 対応

**ログで確認:**
```
🚀 Swimmy Bridge Ver 15.2 - Multi-TF Support
📊 This EA handles: USDJPY only
✅ PUB connected to Guardian
✅ SUB connected to Guardian
```

---

## 🏁 Launch Checklist

```bash
# 1. Quality Gate 確認
make quality-gate

# 2. サービス状態確認
make status

# 3. MT5 EA バージョン確認
# → EA ログで "Ver 15.2" を確認

# 4. ログ確認
tail -30 logs/swimmy.log
tail -10 logs/guardian.log
```

---

## 🚨 緊急時対応

```bash
# ゾンビプロセス一掃（サービス再起動で自動実行）
make run

# 手動でゾンビを殺す場合
make kill-zombies

# その後、正常に再起動
# その後、正常に再起動
systemctl --user restart swimmy-brain swimmy-guardian swimmy-data-keeper swimmy-evolution
```

---

## 🧭 Swimmy Philosophy

### 1. Speed = Survival
Rustのバックテスト速度（50,000本/ms）で「失敗を高速に捨てる」。

### 2. Hot Reloading = Adaptation
Lispでシステム停止なしにロジック修正可能。

### 3. SRP = Antifragility
細かいモジュール分割により、1箇所の障害が全体を殺さない。
