# 🦈 Swimmy Ver 39.0 引き継ぎドキュメント

**作成日**: 2025-12-25  
**バージョン**: 39.0 CIVILIZATION COMPLETE  
**次回セッション用**: このドキュメントは全ての実装内容と次のステップをまとめています

---

## 📋 目次

1. [現在の状態](#現在の状態)
2. [Ver 39.0 で実装した機能](#ver-390-で実装した機能)
3. [アーキテクチャ概要](#アーキテクチャ概要)
4. [重要ファイル一覧](#重要ファイル一覧)
5. [次回への引き継ぎ](#次回への引き継ぎ)

---

## 🎯 現在の状態

| 項目 | 状態 |
|------|------|
| **バージョン** | 39.0 CIVILIZATION COMPLETE |
| **稼働状況** | ✅ 正常稼働中 |
| **トレード** | WARMUP モード (4氏族が協調稼働) |
| **コンパイル** | ✅ 警告なし |
| **日次PnL** | ¥0 (WARMUP中のため) |
| **戦略数** | 61 (Knowledge Base) + 41 (進化) |

---

## 🏛️ Ver 39.0 で実装した機能

### 1. 階級制度 (Hierarchy System)

```
Scout (25%) → Warrior (100%) → Veteran (125%) → Legend (150%)
```

- **場所**: `school.lisp` (line 918-1016)
- **機能**: 
  - 戦略の成績に応じた昇格/降格
  - ロット倍率の自動調整
  - 元服の儀（Coming of Age ceremony）

**昇格条件**:
- Scout → Warrior: 10トレード、勝率40%以上、PnL正
- Warrior → Veteran: 50トレード、勝率50%以上、+500
- Veteran → Legend: 100トレード、勝率55%以上、+2000

### 2. 御前会議 (High Council)

- **場所**: `brain.lisp` (line 2166-2342)
- **機能**:
  - 氏族投票（各氏族が提案に投票）
  - 長老助言（Hall of Fame の Elder が助言）
  - 憲法チェック（Constitution に違反しないか確認）
  - 大首長通知（重要決定をDiscordで通知）

**プロセス**:
```
族長が提案 → 全氏族投票 → 長老助言 → 憲法確認 → 70%以上で承認
```

### 3. 氏族間経済 (Inter-Tribal Economics)

- **場所**: `school.lisp` (line 1018-1092)
- **機能**:
  - **Mutual Aid**: Raiders の日銭を Hunters に援助
  - **Hedge**: Shamans が Breakers のリスクをヘッジ
  - **Treasury**: 各氏族の貢献度を追跡

---

## 🏗️ アーキテクチャ概要

```
Swimmy Ver 39.0 Architecture
├── Guardian (Rust)           - 鉄の掟 (daily-loss-limit等)
├── Brain (Lisp)              - 中枢システム
│   ├── Constitution          - 憲法（5つの核心価値）
│   ├── High Council          - 御前会議
│   ├── Elders                - 長老会議（Hall of Fame）
│   ├── Reputation            - 評判システム
│   └── Tribal Dialect        - 部族方言
├── School (Lisp)             - 戦略管理
│   ├── 4 Great Clans         - Hunters/Shamans/Breakers/Raiders
│   ├── Hierarchy             - 階級制度
│   ├── Economics             - 氏族間経済
│   └── Swarm Intelligence    - 群れの知能
└── Quality Modules (Lisp)
    ├── error-handling.lisp   - エラー処理
    ├── quality.lisp          - 品質分析
    ├── repl.lisp             - 対話インターフェース
    └── tests.lisp            - 単体テスト
```

---

## 📁 重要ファイル一覧

### コアシステム

| ファイル | 役割 | 重要度 |
|---------|------|--------|
| `brain.lisp` | 中枢（意思決定、憲法、儀式） | ⭐⭐⭐⭐⭐ |
| `school.lisp` | 戦略管理（氏族、階級、経済） | ⭐⭐⭐⭐⭐ |
| `guardian/src/main.rs` | Rust守護者（鉄の掟） | ⭐⭐⭐⭐⭐ |
| `dsl.lisp` | 戦略DSL + PGマクロ | ⭐⭐⭐⭐ |
| `run.sh` | 起動スクリプト | ⭐⭐⭐⭐ |

### 品質モジュール

| ファイル | 役割 |
|---------|------|
| `error-handling.lisp` | エラー処理 |
| `quality.lisp` | コード品質分析 |
| `repl.lisp` | `(swimmy :status)` 等 |
| `tests.lisp` | 単体テスト |

### ドキュメント

| ファイル | 内容 |
|---------|------|
| `doc/tribe.txt` | **基本思想**（最重要） |
| `doc/PHILOSOPHY_v36.md` | 哲学ドキュメント |
| `.opus/daily_handoff.md` | 日次引き継ぎ（Opus用） |
| `REFERENCE.md` | システムリファレンス |

---

## 🔄 次回への引き継ぎ

### ✅ 完了事項

- [x] Ver 39.0 実装完了（階級、会議、経済）
- [x] コンパイル警告解消
- [x] Discord Emergency URL 設定
- [x] Daily Review 実施

### ⏳ 進行中

- [ ] **WARMUP → 本番移行**: `*category-trades*` が50に到達待ち
- [ ] **Scout → Warrior昇格**: 成績優秀な戦略の昇格待ち
- [ ] **Mutual Aid 発動**: Raiders が利益を出し、Hunters が待機中の状況待ち

### 📋 次回タスク（優先度順）

| 優先度 | タスク | 詳細 |
|--------|--------|------|
| **高** | WARMUP完了監視 | 50トレード達成を確認 |
| **高** | トレード結果記録 | 昇格判定のため実績を追跡 |
| **中** | Elo推移グラフ | 戦略の成長を可視化 |
| **中** | BTC対応ブローカー | IC Markets/Vantage 検討 |
| **低** | NN信頼度向上 | データ蓄積で精度アップ |

---

## 🛠️ トラブルシューティング

### よくある問題

#### 1. WARMUPから進まない
```
確認: (gethash :trend *category-trades*)
解決: 50トレード達成まで待つ
```

#### 2. Discord通知が来ない
```
確認: echo $SWIMMY_DISCORD_EMERGENCY
解決: run.sh で環境変数を確認
```

#### 3. コンパイル警告
```
解決済み: 前方宣言を追加済み
```

---

## 📊 現在の成績

### トップ戦略（Elo順）

```
1. SMA-10-30     (Elo: 1220, Wins: 19)
2. SMA-5-20      (Elo: 1219, Wins: 20)
3. SMA-10-20     (Elo: 1214, Wins: 19)
4. SMA-8-30      (Elo: 1207, Wins: 19)
5. SMA-8-20      (Elo: 1193, Wins: 19)
```

### 市場環境

```
レジーム: RANGING
ボラティリティ: HIGH (1.229%)
SWARM判断: BUY (83% consensus)
```

---

## 🎯 核心思想

> **「勝とうとするな。ただ、生き残れ。そうすれば、最後に立っているのは我々だ。」**  
> — Swimmy Constitution, Article 0

### 実装状況

| 思想要素 | 実装 | 検証 |
|----------|------|------|
| 鉄の掟 | ✅ 100% | ✅ |
| 4大氏族 | ✅ 100% | ✅ |
| 長老会議 | ✅ 100% | ⏳ |
| 評判システム | ✅ 100% | ⏳ |
| 儀式 | ✅ 100% | ✅ |
| 階級制度 | ✅ 100% | ⏳ |
| 氏族間経済 | ✅ 100% | ⏳ |
| 御前会議 | ✅ 100% | ⏳ |

**理論実装: 100%**  
**実戦検証: 30%** (WARMUP中)

---

## 📞 連絡先・リソース

### Discord

- **Emergency**: `$SWIMMY_DISCORD_EMERGENCY`
- **Daily**: `$SWIMMY_DISCORD_DAILY`
- **Weekly**: `$SWIMMY_DISCORD_WEEKLY`

### ログ

```bash
# 最新ログ
tail -f ~/swimmy/doc/log_*.txt

# 起動
cd ~/swimmy && ./run.sh
```

---

## 🚀 クイックスタート（次回セッション用）

```bash
# 1. 現在の状態確認
cd ~/swimmy
./run.sh  # 既に稼働中ならスキップ

# 2. REPL で状態確認（別ターミナル）
sbcl --script brain.lisp
(swimmy :status)
(swimmy :clans)

# 3. ログ確認
tail -f doc/log_*.txt

# 4. Daily Review（毎日）
# AIパートナーに: @/daily-review
```

---

**このドキュメントは次回セッションの開始点です。**  
**質問や不明点があれば、このファイルから始めてください。**

---

*作成: Opus AI Partner*  
*更新: 2025-12-25 23:54*
