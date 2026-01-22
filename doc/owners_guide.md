# 🐟 Swimmy Ver 18 オーナーズガイド

**最終版:** 2026-01-22 (V48.0 - Rank System Overhaul)
**リーダー判断:** Elon Musk (Expert Panel Verified)

---

## 🆕 V48.0 新機能 (2026-01-22)

### 1. Rank System Fix
- **問題解決**: `strategy-rank` が常にnilだった問題を修正
- **B-RANK昇格**: Phase 1 BT合格(Sharpe≥0.1)で自動昇格
- **Graveyard連携**: 失敗戦略に`:graveyard`ランク設定

### 2. CPCV自動検証
- **Phase 3.5**: 進化ループにCPCV検証フェーズ追加
- **run-a-rank-cpcv-batch**: 5件/5分間隔で処理
- **S-RANK昇格**: CPCV合格時に自動昇格+Discord通知

### 3. 基準値DB対応
- **rank_criteria.sql**: PostgreSQLスキーマ
- **動的読込**: DB接続時は5分キャッシュで基準値取得
- **フォールバック**: DB未接続時はハードコード値使用

### 4. Backtest最適化
- **Symbol対応**: 戦略ごとのsymbolでバックテスト
- **バッチサイズ**: 300→1000に増加
- **サイクル完了通知**: 一周終了時にサマリー送信

### 5. 4氏族システム削除
- **mixseek.lisp**: 無効化
- **comomentum.lisp**: 有用機能のみ保持

---

## 🆕 V47.10 新機能 (2026-01-22)

### 1. P10 KB Optimization
- **戦略プルーニング**: 18,349戦略 → 目標5,000
  - 低Sharpe削除 (< 0.08)
  - 90日非活性削除
  - 類似戦略削除 (distance < 0.1)
- **school-pruning.lisp**: `run-kb-pruning` でワンコマンド実行
- **自動実行**: 週1回 (`phase-8-weekly-prune`)

> [!WARNING]
> **PostgreSQLサーバーは現在未起動です。**
> 基準値DBは無効。ハードコード値（フォールバック）で動作中。
> DB接続が必要な場合: `sudo systemctl start postgresql`

### 2. P12 True CPCV Integration
- **Guardian CPCV_VALIDATE action**: 真のCPCV検証
- **request-cpcv-validation**: Lisp→Rust ZMQ呼び出し
- **validate-for-s-rank-promotion**: S-RANK昇格時自動CPCV検証
- **基準**: Sharpe≥0.5, PF≥1.5, WR≥45%, MaxDD<15%, CPCV PASS

### 3. P9 OOS Validation (CPCV Lite)
- **A-RANK昇格検証**: 70/30分割でOOS検証
- **school-validation.lisp**: `validate-for-a-rank-promotion`
- **基準**: Sharpe≥0.3, PF≥1.2, WR≥40%, MaxDD<20%

---

## 📜 P8 Strategy Pipeline (V47.8)

### 入口統一 (2入口)
- **Founder**: 外部検証済み戦略 (Web/論文/Hunted/Legend)
- **Breeder**: 既存高性能戦略の交配

### add-to-kb関数
戦略追加の単一エントリーポイント。BT検証必須 (Sharpe ≥ 0.1)。

---

## 🎯 ランク体系 (B/A/S)




## 🚀 起動 / 停止 (Systemd)

```bash
# 全サービス起動 (Zombie Processの一掃を含む)
make run  # 推奨

# または個別起動
systemctl --user start swimmy-brain swimmy-guardian swimmy-school swimmy-data-keeper

# 全サービス停止
systemctl --user stop swimmy-brain swimmy-guardian swimmy-school swimmy-data-keeper

# ステータス確認
make status

# リアルタイムログ監視
make logs

# 🧬 進化状況モニター (Multi-Currency Visualizer)
./tools/monitor_evolution.sh
```

---

## 🌊 Lisp Native Implementation (V46.0)

2026-01-19、システムは「完全なるLisp化」を達成しました。Pythonによるオーケストレーションは廃止されました。

### 1. Pure Lisp Daemon (swimmy-school)
進化・学習・最適化のループは、Systemd管理下のSBCLプロセス (`swimmy-school`) によって直接制御されます。
- **高速化**: Pythonインタプリタの起動オーバーヘッドがゼロになりました。
- **堅牢性**: Systemdがプロセスの死活監視を行い、クラッシュ時に自動復旧します。

### 2. Sharded File Persistence (The Great Library)
戦略データを単一の巨大ファイルではなく、戦略ごとに個別のファイル (`data/library/<tier>/<name>.lisp`) として管理します。
- **Incubator**: 生まれたて/テスト中
- **Battlefield**: 交配待ちのエリート
- **Graveyard**: 敗者
- **Legends**: 外部から招喚された英雄

### 2. 7つの市場状態 (Softmax Regime Detection V45.0)

単純な「トレンド/レンジ」ではなく、**確率的な市場認識**を行います。

**レジーム一覧:**
| レジーム | 説明 | 取引対象 |
|---------|------|---------|
| `trend-early` | トレンド初期 | trend, breakout |
| `trend-mature` | トレンド成熟 | trend, breakout |
| `trend-exhausted` | トレンド枯渇 | mean-reversion |
| `range-expansion` | レンジ拡大 | range, mean-reversion |
| `range-compression` | レンジ縮小 | breakout, trend |
| `volatile-spike` | ボラ急上昇 | ⚠️ LEGEND/scalp のみ |
| `illiquid` | 流動性枯渇 | ⚠️ LEGEND/scalp のみ |

**Softmax確率:**
- `detect-market-regime` は `(TrendMature: 0.8, RangeExpansion: 0.2)` のような確率ベクトルを返す
- 最高確率のレジームが `*current-regime*` にセットされる
- 複数レジームが近い確率の場合、コンフィデンスが下がる

**シンボル別レジーム (V45.0):**
- 各通貨ペアごとに独立したレジーム判定を実施
- `(gethash symbol *candle-histories*)` で通貨別履歴を使用
- 例: USDJPYがトレンド中でもEURUSDはレンジの可能性あり

**リスク対応:**
- `volatile-spike` / `illiquid` 時は全戦略スキャンではなく、LEGEND/scalp 戦略のみに制限
- 完全停止はせず、防御的トレードを許可（V45.0で修正）

### 3. ケリー基準による資金管理 (Kelly Criterion)
ロット数は固定ではなく、戦略の「優位性（Edge）」に基づいて動的に計算されます。
- `f* = p - q/b` （勝率とリスクリワード比から算出）
- **Safety**: ハーフケリーを採用し、最大リスクを口座資金の10%に制限。

### 4. ベイズ適応 (Bayesian Adaptation)
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
│  - シグナル生成、学習、State Management             │
├─────────────────────────────────────────────────────┤
│  SCHOOL (Lisp)          Systemd Managed             │
│  - 進化、淘汰、リクルート (Daemon Native)           │
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
| School | `swimmy-school` | **Hyper-Time Evolution (無限進化ループ)**, Purge, Wisdom |

---

## 🧬 SRP モジュール構成 (V46.0)

`school-execution.lisp` もリファクタリングされ、全ファイルがSRP準拠（600行以下）です。

```
src/lisp/
├── school.lisp                    (Orchestrator - 33行)
├── school/
│   ├── school-daemon.lisp        (Systemd Entry Point)
│   ├── school-connector.lisp     (Evolution Loop)
│   ├── school-scout.lisp         (Recruitment)
│   ├── school-breeder.lisp       (Breeding/Wisdom)
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
- ✅ マルチカレンシー対応 (USDJPY, EURUSD, GBPUSD)

**ログで確認:**
```
🚀 Swimmy Bridge Ver 15.2 - Multi-TF Support
📊 This EA handles: USDJPY only
✅ PUB connected to Guardian
✅ SUB connected to Guardian
```

---

## 🏁 Launch Checklist (Lisp Native)

```bash
# 1. Quality Gate 確認
make quality-gate

# 2. サービス状態確認
make status

# 3. MT5 EA バージョン確認
# → EA ログで "Ver 15.2" を確認

# 4. ログ確認
journalctl --user -u swimmy-school -f
journalctl --user -u swimmy-brain -f
```

---

## 🚨 緊急時対応

```bash
# ゾンビプロセス一掃（サービス再起動で自動実行）
make run

# 手動でゾンビを殺す場合
make kill-zombies

# その後、正常に再起動
systemctl --user restart swimmy-brain swimmy-guardian swimmy-data-keeper swimmy-school
```

---

## 🧭 Swimmy Philosophy

### 1. Speed = Survival
Rustのバックテスト速度（50,000本/ms）で「失敗を高速に捨てる」。

### 2. Hot Reloading = Adaptation
Lispでシステム停止なしにロジック修正可能。

### 3. SRP = Antifragility
細かいモジュール分割により、1箇所の障害が全体を殺さない。
