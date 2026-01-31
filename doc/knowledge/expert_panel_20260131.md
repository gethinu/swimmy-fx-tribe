# 🦅 Expert Panel Report

**Date:** 2026-01-31
**Leader:** Elon Musk
**Trigger:** /expert-panel 「システムの状況は？」

## 🏛️ 常設顧問の意見
### Taleb: 破滅リスクが“入力面”に露出しすぎ
- ZMQ受信メッセージを`read-from-string`で直接読み込んでいる。`*read-eval*`無効化もなく、外部入力経由でコード実行リスクがある。`src/lisp/core/message-dispatcher.lisp:102-108`
- LLM出力の`entry/exit`を`read-from-string`で評価しており、毒入り出力が即実行される構造。これは破滅の入口。`src/lisp/school/school-evolution.lisp:330-341`
- Backtest V2が「修正TODO入り」で運用に耐えていない。壊れた検証は“静かな破滅”を招く。`src/lisp/school/school-backtest-v2.lisp:41-52,133-140`

### Graham: プロダクトの“真実の状態”が曖昧
- 実行バナーはVer 41.5だが、オーナーズガイドはV50.5。ユーザーは何を動かしているか分からない。`src/lisp/main.lisp:134` / `doc/owners_guide.md:1`
- アーキテクチャ文書が2025-12-29更新のまま。2026-01-28の実装とズレている。スケールの前に可視性が崩壊。`doc/SYSTEM_ARCHITECTURE.md:3`

### Naval: レバレッジの前に「失敗しない自動化」
- `tools/install_services.sh`が`cp`失敗を黙殺し、ユニットが無くても`enable`/`restart`を実行する。自動化が“成功を偽装”している。`tools/install_services.sh:7-18`
- 起動時に全シンボル×全TFのヒストリを同期ロード。時間とコストが線形に増え、レバレッジを食い潰す。`src/lisp/main.lisp:87-113`

### Jim Simons: 検証パイプラインが数学的に破綻
- Backtest V2のpayloadが`strategy-json`のまま（コメントで誤り宣言）。検証が誤入力なら統計は無意味。`src/lisp/school/school-backtest-v2.lisp:41-52`
- Phase2（2021-）の昇格ロジックが未実装。OOS検証が“言葉だけ”。`src/lisp/school/school-backtest-v2.lisp:133-140`

## 💻 技術パネルの意見
### Fowler: God Functionと多責務が肥大
- `initialize-system`がIO・DB・通知・学習準備を一括で実行し、しかも`load-hall-of-fame`が二重呼び出し。責務分離できていない。`src/lisp/main.lisp:69-113`
- `internal-process-msg`が入力パース〜副作用まで抱え込み、拡張不能な巨大関数になっている。`src/lisp/core/message-dispatcher.lisp:102-218`

### Hickey: シンプルさが失われたグローバル状態
- `config.lisp`に“設定”と“ランタイム状態”が混在。グローバル可変の塊は理解不能を生む。`src/lisp/core/config.lisp:182-207`
- `*supported-symbols*`がコード固定。運用がコード編集に依存している。`src/lisp/core/config.lisp:10-12`

### Uncle Bob: テスト不在が“簡単なバグ”を通す
- `report_system_status.py`で`re`未import。実運用で即落ちる。これはテストゼロの証拠。`tools/report_system_status.py:61-70`
- `parse-json-safely`が空実装、`get-screening-range`がコメント付きの未修正。クリーンコード以前に未完了。`src/lisp/school/school-evolution.lisp:293-294` / `src/lisp/school/school-backtest-v2.lisp:74-77`

## 🚀 ビジョナリーの意見
### Ng: LLMパイプラインが“安全性”を欠いている
- LLM出力のJSONを外部Pythonで変換し、さらに`read-from-string`で戦略を生成。入力検証もスキーマも無い。`src/lisp/school/school-evolution.lisp:296-355`
- Inferenceクライアントは失敗時に`nil`を返すだけで監視・復旧・再試行が無い。品質が静かに劣化する。`src/lisp/core/inference-client.lisp:23-52`

### López de Prado: OOSを名乗るが“実装が無い”
- Phase2昇格ロジックが空。OOSでの抑制が機能せず、過学習が放置される。`src/lisp/school/school-backtest-v2.lisp:133-140`

### Gene Kim: 監視とデプロイが信用できない
- 監視レポートが`re`未importでクラッシュする。正常性の数字が嘘になる。`tools/report_system_status.py:61-70`
- systemdの配置が`systemd/`前提だが、コピー失敗を黙殺するスクリプト設計。運用手順が壊れやすい。`tools/install_services.sh:7-18` / `scripts/install-service.sh:7-22`

## 🚀 Musk's Decision (Final)
> 「今の状態で“ライブ運用”はするな。入力安全性・検証整合性・監視の3点が揃うまで停止。やるべきは“安全な読み取り”“正しい検証”“嘘のない監視”だ。見栄えの新機能は後回し。」

## Actionable Items
1. `read-from-string`の入力経路を全面封鎖（`*read-eval*`無効化＋ホワイトリストパーサ）: `src/lisp/core/message-dispatcher.lisp`, `src/lisp/school/school-evolution.lisp`, `src/lisp/core/persistence.lisp`。
2. Backtest V2のpayload修正（`strategy-alist`に統一）とPhase2昇格ロジック実装＋テスト追加。`src/lisp/school/school-backtest-v2.lisp`。
3. 監視スクリプトの即時修正（`import re`）と実行テスト追加。`tools/report_system_status.py`。
4. 起動シーケンスの責務分割＆重複削除（`load-hall-of-fame`重複など）。`src/lisp/main.lisp`。
5. ドキュメントの整合更新（Ver表記とアーキ更新日）。`doc/owners_guide.md`, `doc/SYSTEM_ARCHITECTURE.md`。
6. systemdユニットの正本を固定し、コピー失敗時に即失敗するスクリプトへ修正。`tools/install_services.sh`, `scripts/install-service.sh`。

---

# 🦅 Expert Panel Report

**Date:** 2026-01-31  
**Leader:** Elon Musk  
**Trigger:** 61本のV3.0シグナルがレジェンドとして復活したが「本当に有用か／交配に使われているか」を再検証せよ

## 🏛️ 常設顧問の意見
### Taleb
- レジェンド61本を無条件にBreeding母集団へ戻したことで、過去のドローダウン特性を再評価せず希釈した。SharpeやMaxDDの再計測がないまま「immortal」扱いは過剰リスク。`src/lisp/strategies/legend-61.lisp`
- Breeding選抜はrank重み+Sharpeのみでボラリスクや相関制約が弱い。Legendが大量に入ると同質リスクが集中する懸念。`src/lisp/school/school-breeder.lisp:143-165`

### Paul Graham
- 61本を一括ロードするが、ロード時にバージョン管理・由来メタが付かない。プロダクトとして「どの戦略が生きているか」が見えず、運用ドキュメントの信頼が下がる。`src/lisp/strategies/legend-61.lisp`

### Naval
- レジェンド61が「交配優先度+3」加点で常に上位に来るため、学習資源が歴史的ストラに偏り、新規生成のレバレッジを殺している。レジェンド交配のクォータや確率制御が必要。`src/lisp/school/school-breeder.lisp:143-165`

### Jim Simons
- 61本のパラメータは再推定なしに再登録されている。市場体制が変化した2024-2026のデータで再フィット・再検証（OOS/CPCV）が無いので統計的有意性が疑わしい。`strategies_v3.lisp`

## 💻 技術パネルの意見
### Martin Fowler
- `legend-61.lisp` が `strategies_v3.lisp` をread-evalして再構築するが、エラーハンドリング・スキーマ検証が無い。破損ファイルでBrainが起動不能になる。読み込みと検証を分離するサービス層を作るべき。`src/lisp/strategies/legend-61.lisp`

### Rich Hickey
- レジェンド復元が `initialize-system` に直接書かれ、副作用を増やしている。初期化手続きは小さく純粋であるべきで、復元は明示的なタスクに分離するほうがシンプル。`src/lisp/main.lisp:69-119`

### Uncle Bob
- 61本が正しく「交配に使われるか」をテストする自動テストが無い。少なくとも breedingペア選択がLegendを含むこと、墓場送りにならないことを検証するテストを追加せよ。`src/lisp/school/school-breeder.lisp` / `src/lisp/school/school-rank-system.lisp`

## 🚀 ビジョナリーの意見
### Andrew Ng
- 61本の特徴量設計はクラシック指標のみ。現行のML/LLM生成パイプラインと相互学習していない。特徴量拡張やメタ学習に組み込まないと単なる博物館展示になる。

### López de Prado
- レジェンドをBreedingに投入するなら、**相関制約**と**多重検定補正**を必須に。現在の相関チェックは「0.95超で棄却」だけで、61本が密集するTF/ロジック帯域に耐えられない。`src/lisp/school/school-breeder.lisp:150-165`

### Gene Kim
- 運用可観測性が不足。復元後のLegendが何本残存し、何本が子を産んだかをダッシュボードに可視化すべき。現状はログ探しになる。

## 🚀 Musk's Decision (Final)
> 「61本は歴史的アセットとして扱う。  
>  1) レジェンドは墓場に落とさないが、Breedingへの投入割合は制御する。  
>  2) 現行データで再BT/OOSを走らせ、統計的にダメなものは “Legend-Archive” に退避。  
>  3) 初期化から復元を切り離し、明示的なコマンドでロードする。  
>  4) メタデータとダッシュボードで状態を見える化する。」

## Actionable Items
1. Breeding制御: Legendの選抜に上限（例: 各カテゴリ上位5本まで）と相関しきい値を強化。`src/lisp/school/school-breeder.lisp`
2. 再検証: 61本を最新データで一括BT→OOS→CPCVし、Sharpe/MaxDD/相関を記録。基準未満は `data/library/LEGEND_ARCHIVE/` へ移動し rankを `:legend-archive` に設定。
3. 可視化: ダッシュボードに「Legend存続数」「Legend由来子数」「Archive数」を追加。`tools/dashboard.py` 等。
4. 初期化分離: `initialize-system` から `restore-legend-61` を外し、管理用スクリプト/コマンドに移動。誤読でBrain起動が止まらないようにする。
5. テスト: Breeding選抜がLegendを含むこと、Legendが墓場送りされないことを自動テスト化（Lispユニットテスト）。
