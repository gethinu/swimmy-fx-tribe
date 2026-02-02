# 🦅 Expert Panel Report

**Date:** 2026-02-02  
**Leader:** Elon Musk  
**Trigger:** /expert-panel「ZMQ通信＋ローカル保存までS式化(B案)で進めたい。意見が欲しい」

## 🏛️ 常設顧問の意見
### Taleb: “S式全域化は攻撃面の拡大。安全なリーダ無しは自滅”
- 受信側の中心は`message-dispatcher`でS式を読んでいる。全ZMQをS式にすると、**入力面が全方位化**する。安全なリーダ/検証なしで進めれば破滅。`src/lisp/core/message-dispatcher.lisp:121-129`
- 仕様がJSON前提のままなのにS式に進めば、運用監査の真実が崩れる。**まず仕様の一枚岩化**が先。`docs/llm/INTERFACES.md:3-16`

### Graham: “仕様の正義が割れているまま移行するな”
- インターフェースは**JSONと明記**され、Backtestだけ例外的にS式。B案はこの分裂を全域に拡散する。`docs/llm/INTERFACES.md:3-126`
- SPECはData Keeperや各コンポーネントを前提に書かれているが、S式化の責務は未定義。**まずSPEC/ARCH/STATEの整合が必要**。`docs/llm/SPEC.md:7-53`, `docs/llm/ARCHITECTURE.md:35-56`

### Naval: “保守性を上げるどころか、全言語に痛みを配る”
- Lisp内だけでもJSON依存が広範囲。全域S式は**MQL5/Rust/Pythonまで改修**が必須で、保守性が逆に低下する。`src/lisp/system/runner.lisp:72-103`, `src/lisp/core/data-client.lisp:60-122`
- ZMQだけでなくローカル保存もS式化するなら、既存のJSONL監査/ログ運用が壊れる。**保守性を理由に運用の基盤を壊すのは本末転倒**。`src/lisp/core/db-adapter.lisp:14-54`, `src/lisp/logger.lisp:7-35`

### Jim Simons: “検証基盤が切断される”
- 統合テストはJSON入力/出力に依存。全域S式化は**テストの再設計**が必須。`src/lisp/tests/integration-tests.lisp:13-31`
- 監査/統計はJSONL前提で集計されている。S式化で過去比較が不可能になる。`src/lisp/core/db-adapter.lisp:46-54`

## 💻 技術パネルの意見
### Fowler: “境界の崩壊が最大の技術負債”
- `internal-process-msg`はS式/JSONの二重分岐。全域S式化は**境界の共通化（変換層）**なしでは破滅的。`src/lisp/core/message-dispatcher.lisp:121-343`
- Backtest ServiceはS式入力でも**辞書→JSON返却が残る**。仕様が揺れたまま。`tools/backtest_service.py:520-528`

### Hickey: “Lispの都合で全員を巻き込むな”
- S式はLisp内部では最強だが、**他言語での正規化設計がない**。全域S式化は複雑性を増す。`docs/llm/INTERFACES.md:3-16`
- ローカル保存S式化は、JSON前提の可観測性（logger/telemetry/live_status）と衝突する。`src/lisp/logger.lisp:7-35`, `src/lisp/school/school-telemetry.lisp:7-34`, `src/lisp/shell/notifications.lisp:123-167`

### Uncle Bob: “テストと移行計画なしの統一は事故”
- JSON→S式の**移行テストが存在しない**。BacktestだけでなくZMQ/保存の全域を変えるなら、失敗時のロールバック/互換モードが必須。`src/lisp/tests/integration-tests.lisp:13-31`
- Notifier/DiscordはJSON送信前提。全域S式化で壊れる箇所が明確。`src/lisp/core/discord.lisp:40-68`

## 🚀 ビジョナリーの意見
### Ng: “観測とAI運用の互換性が落ちる”
- テレメトリやログはJSON前提で可視化されている。S式化は**監視の再構築**が必要。`src/lisp/school/school-telemetry.lisp:7-58`, `src/lisp/logger.lisp:7-35`

### López de Prado: “履歴の連続性が断裂する”
- JSONL監査/ログをS式に変えると過去比較が途切れる。統計の前提が崩れる。`src/lisp/core/db-adapter.lisp:46-54`

### Gene Kim: “運用は壊れやすくなる”
- `live_status.json`やDiscord通知は運用の生命線。S式化は運用ツールの互換性を壊す。`src/lisp/shell/notifications.lisp:123-167`, `src/lisp/core/discord.lisp:40-68`

## 🚀 Musk's Decision (Final)
> 「B案は“やるなら徹底的に設計してから”だ。  
>  まず**SPEC/INTERFACES/STATEの全面改訂**で正義を一本化しろ。  
>  その上で段階移行（互換モード→S式強制）を設計し、テストとロールバックを用意する。  
>  いきなり全域S式はやらない。設計と検証の準備が整ったら実装に入れ。」

## Actionable Items
1. **ドキュメント正義の一本化**：`docs/llm/INTERFACES.md` のEncoding/Backtest記述をB案に合わせ全面更新。`docs/llm/SPEC.md`, `docs/llm/ARCHITECTURE.md`, `docs/llm/STATE.md`も同時更新。`docs/llm/INTERFACES.md:3-126`, `docs/llm/SPEC.md:44-53`
2. **境界設計**：ZMQ全ポートのS式スキーマを定義し、JSONとの互換モード/移行期限を明記。`docs/llm/INTERFACES.md:3-16`
3. **Backtest Serviceの返却統一**：S式入力時にJSON返却する箇所を廃止し、結果もS式で返す。`tools/backtest_service.py:520-528`
4. **ローカル保存の優先順位**：JSONL/telemetry/live_status/backtest_cacheのどれをS式に変えるかを優先順で決め、移行スクリプトを用意。`src/lisp/core/db-adapter.lisp:14-54`, `src/lisp/school/school-telemetry.lisp:7-34`, `src/lisp/shell/notifications.lisp:123-167`, `src/lisp/school/school-backtest-utils.lisp:9-80`
5. **テスト再設計**：JSON前提の統合テストをS式対応に改修し、互換モード/強制モード両方を検証する。`src/lisp/tests/integration-tests.lisp:13-31`

# 🦅 Expert Panel Report

**Date:** 2026-02-02  
**Leader:** Elon Musk  
**Trigger:** /expert-panel「どれを『正』とする？ DB / Library / In-memory KB」

## 🏛️ 常設顧問の意見
### Taleb: “3つの真実は、どれも真実じゃない”
- KBはDB+Libraryのマージで作られ、さらに**墓場はメモリから除去**される。報告で墓場数が0になっても当たり前。これは運用上の**隠れた脆弱性**。`src/lisp/strategies/strategies.lisp:14-59`
- Evolution Reportは**メモリKBでS/A/Bを数え、墓場だけLibraryのファイル数**を使う。混血は事故の始まり。`src/lisp/school/school-narrative.lisp:206-212`

### Graham: “報告の正義が割れている”
- Owner’s Guideは「Evolution Factory Reportで確認」と言うが、そのReportは混血ソース。**確認方法が嘘**。`doc/owners_guide.md:175-177`, `src/lisp/school/school-narrative.lisp:203-212`
- 仕様上はSQL移行完了。ならDBを真実にする以外ない。迷ってる時点で設計が死んでる。`doc/knowledge/implementation_plan_v49.8.md:8-14`

### Naval: “レバレッジがない三重化”
- DBに同期しないLibraryは**運用コストだけ増える**。`refresh-strategy-metrics-from-db`がある時点でDBが主戦場。`src/lisp/school/school-narrative.lisp:203-205`, `src/lisp/school/school-db.lisp:247-292`
- “真実は一つ”の宣言が無いと、**毎回整合作業で人生を失う**。

### Jim Simons: “統計の足場が崩れている”
- Backtest Summaryのランク分布は**メモリKB**由来。墓場が落ちているならサンプルバイアス確定。`src/lisp/core/discord.lisp:198-206`, `src/lisp/strategies/strategies.lisp:53-55`
- 集計軸が揺れるとSharpeや勝率の比較は無意味。**データの一貫性が最大のアルファ**だ。

## 💻 技術パネルの意見
### Fowler: “層の汚染が致命傷”
- `generate-evolution-report`がDB→KB同期しつつ、墓場はLibrary直参照。**レイヤー違反**の典型。`src/lisp/school/school-narrative.lisp:203-212`
- `init-knowledge-base`はDB優先マージだが、最終的にKBが真実かどうかが曖昧。Repository層が不在。`src/lisp/strategies/strategies.lisp:14-24`

### Hickey: “シンプルさの敵は二重のDB”
- DBパスが**2系統**ある。`data/memory/swimmy.db`と`data/swimmy.db`。これ自体が真実を崩す。`src/lisp/core/sqlite-manager.lisp:6-12`, `src/lisp/core/schema.lisp:16-20`
- “Libraryが真実”と言いながら、ReportでDB同期を強制している。**設計が言葉に勝っている**。`src/lisp/school/school-narrative.lisp:203-205`

### Uncle Bob: “テスト不在の合意は幻想”
- ソース整合性を検証するテストが無い。**壊れるまで誰も気づかない**。`src/lisp/tests/backtest-db-tests.lisp`, `src/lisp/tests/school-split-tests.lisp`
- 仕様の真実がないので、テストが書けないのが根本原因。

## 🚀 ビジョナリーの意見
### Ng: “データドリフトが静かに学習を殺す”
- KB/DB/Libraryの整合が崩れると、学習データもラベルも崩れる。**静かな劣化**が最悪。`src/lisp/school/school-db.lisp:247-292`

### López de Prado: “選別基準が揺れると過学習が再来する”
- Backtest結果とRank分布の母集団が一致しない。**選択バイアス**の温床。`src/lisp/core/discord.lisp:198-206`, `src/lisp/strategies/strategies.lisp:53-55`

### Gene Kim: “運用は数字の信頼で成り立つ”
- レポートの数が一致しない時点で、監視は無意味になる。**Opsの信用を壊すな**。`src/lisp/school/school-narrative.lisp:203-212`, `doc/owners_guide.md:175-177`

## 🚀 Musk's Decision (Final)
> 「DBを真実にする。Libraryは**派生スナップショット**、In-memoryは**キャッシュ**だ。  
>  混血レポートは今すぐやめる。ソースを一本化し、数字の信用を回復しろ。」

## Actionable Items
1. **真実の宣言**：DBを公式ソース・Libraryを派生・KBをキャッシュと明記。`doc/owners_guide.md:175-177`, `doc/knowledge/implementation_plan_v49.8.md:8-14`
2. **Reportの統一**：`notify-backtest-summary` と `generate-evolution-report` を**DB集計**に統一。墓場数もDB基準に。`src/lisp/core/discord.lisp:198-206`, `src/lisp/school/school-narrative.lisp:203-212`, `src/lisp/school/school-db.lisp:247-292`
3. **DBパスの一本化**：旧 `data/swimmy.db` 系の参照を整理・廃止。`src/lisp/core/sqlite-manager.lisp:6-12`, `src/lisp/core/schema.lisp:16-20`
4. **整合性テスト**：Report生成時にDB/KB/Libraryの差分を検出するスモークテストを追加。`src/lisp/tests/backtest-db-tests.lisp`, `src/lisp/tests/school-split-tests.lisp`

# 🦅 Expert Panel Report

**Date:** 2026-02-02  
**Leader:** Elon Musk  
**Trigger:** /expert-panel「ローカル保存S式化の対象範囲（最小/中間/最大）を1つ選定して進めたい」

## 🏛️ 常設顧問の意見
### Taleb: “最大はRuin。観測の目が潰れるなら終わり”
- ローカル保存は運用の生命線。`live_status.json`と`system_metrics.json`は**運用可観測性そのもの**。ここを一気に変えるなら移行失敗が即死。`src/lisp/shell/notifications.lisp:123-167`, `src/lisp/school/school-telemetry.lisp:7-34`
- `data/`や`db/data/`のJSON/JSONL全面変換は「破壊的な一括手術」。**復旧不能リスク**を積む。最大は却下。`tools/report_status.py:35-96`

### Graham: “問題は範囲。やり過ぎはスピードを殺す”
- SPECはローカル保存S式化の**対象範囲が未確定**と明記。ここを決めずに実装へ進むと失速する。`docs/llm/SPEC.md:51-75`
- 最小は成果が薄い。最大は失速。**中間が唯一、意思決定として合理的**。`docs/llm/SPEC.md:51-56`

### Naval: “レバレッジのない改修はやめろ”
- 価値のある出力はDiscord/報告系。`report_backtest_summary.py`と`discord_bot.py`はJSON前提。全部をS式にするのは**レバレッジが低い**。`tools/report_backtest_summary.py:41-106`, `src/python/discord_bot.py:38-99`
- 影響範囲が広いのに利益が薄い（最大）。やるなら**運用の3ファイルだけ**に絞れ。`tools/report_status.py:35-96`

### Jim Simons: “比較可能性を壊すな”
- `backtest_cache.json`はランキング/報告の基準。形式変更で**過去比較が壊れる**。移行スクリプトなしの最大は論外。`src/lisp/school/school-backtest-utils.lisp:9-80`, `tools/report_status.py:80-107`

## 💻 技術パネルの意見
### Fowler: “境界がここ。変えるなら変換層を作れ”
- `live_status.json`はLisp→Pythonの境界。**この境界を一括破壊するなら変換層が必須**。`src/lisp/shell/notifications.lisp:123-167`, `src/python/discord_bot.py:75-99`
- `system_metrics.json`と`backtest_cache.json`は報告系の入口。ここだけ変えるなら中間で十分。`src/lisp/school/school-telemetry.lisp:7-34`, `tools/report_status.py:35-96`

### Hickey: “Lisp最適化で他言語を殺すな”
- S式はLisp内では簡潔だが、Python側では**自前パーサが必要**。最大は複雑性を爆増させる。`tools/report_backtest_summary.py:58-106`, `src/python/discord_bot.py:75-99`
- 中間なら「Lisp側のS式化 + Python側の最小アダプタ」で済む。  

### Uncle Bob: “テストがない範囲はやるな”
- JSON前提の実運用ツールが複数ある。**互換テストなしに最大は事故**。`tools/report_status.py:35-96`, `tools/report_backtest_summary.py:58-106`
- 中間に絞ってテストを書け。最大は今の体制では無謀。  

## 🚀 ビジョナリーの意見
### Ng: “観測系を壊すと学習が止まる”
- テレメトリは学習/運用の血流。ここを壊すと不具合が見えなくなる。**中間で止めるのが安全**。`src/lisp/school/school-telemetry.lisp:7-34`

### López de Prado: “履歴の連続性を守れ”
- JSONL全面変換は**統計的連続性の破壊**。最大は分析の前提を壊す。`tools/report_status.py:35-96`

### Gene Kim: “運用のフィードバックループは最優先”
- `live_status.json`はDiscord運用の中心。ここはS式化の対象に含めるべきだが、**全域変換は不要**。`src/lisp/shell/notifications.lisp:123-167`, `src/python/discord_bot.py:75-99`

## 🚀 Musk's Decision (Final)
> 「**中間**で行く。  
>  Backtest + Telemetry/Status だけをS式化し、`data/`と`db/data/`のJSON/JSONLは温存する。  
>  いま必要なのは“動く運用”だ。全面移行は勝ってからだ。」

## Actionable Items
1. **決定の記録**：`docs/llm/SPEC.md` の「ローカル保存S式化の対象範囲」を **中間**に確定し、`docs/llm/STATE.md` の決定事項と次アクションを更新。`docs/llm/SPEC.md:51-75`, `docs/llm/STATE.md:12-82`
2. **対象ファイルの明確化**：`data/backtest_cache.json`、`data/system_metrics.json`、`.opus/live_status.json` のS式化を対象に固定（最大は見送り）。`src/lisp/school/school-backtest-utils.lisp:9-80`, `src/lisp/school/school-telemetry.lisp:7-34`, `src/lisp/shell/notifications.lisp:123-167`
3. **互換・移行**：S式への移行スクリプトを用意し、Python側はS式対応または変換アダプタで対応。`tools/report_status.py:35-96`, `tools/report_backtest_summary.py:41-106`, `src/python/discord_bot.py:75-99`
4. **テスト**：ローカル保存の読み書き（S式/旧JSON）を最小テストで保証。最大範囲の変換は保留。  

# 🦅 Expert Panel Report

**Date:** 2026-02-02  
**Leader:** Elon Musk  
**Trigger:** /expert-panel「S式即時単独で進める。さらに提案があれば意見して」

## 🏛️ 常設顧問の意見
### Taleb: “停止許容でも、壊れる設計はRuinだ”
- 即時単独は「止めて直す」前提だが、**部分書き込み/壊れたS式**で再起不能になる。現状のJSON書き込みは逐次`format`で**原子的でない**。S式化で同じ設計なら事故る。`src/lisp/shell/notifications.lisp:123-179`, `src/lisp/school/school-telemetry.lisp:22-34`
- Python側に**安全なS式リーダがない**。Lisp側は`safe-read-sexp`で守っているが、Pythonは裸。`src/lisp/core/safe-read.lisp:5-13`, `tools/report_status.py:40-48`

### Graham: “仕様を書かずに統一を叫ぶな”
- S式即時単独なら、**保存スキーマの正義**が必要。`backtest_cache`/`system_metrics`/`live_status`のS式フォーマットを明文化しないと、1週間で破綻する。`src/lisp/school/school-backtest-utils.lisp:48-80`, `src/lisp/school/school-telemetry.lisp:22-34`, `src/lisp/shell/notifications.lisp:123-179`

### Naval: “レバレッジのある最小工数に絞れ”
- JSON読み込みが3箇所に散っている。S式即時単独なら**Python共通パーサ**を作り、1箇所変更で済む設計にしろ。`tools/report_status.py:40-48`, `tools/report_backtest_summary.py:58-66`, `src/python/discord_bot.py:75-83`

### Jim Simons: “比較可能性の死”
- `backtest_cache`はランキングの基準。S式化に**バージョンタグ**が無いと、将来の再計算ができない。`src/lisp/school/school-backtest-utils.lisp:48-80`

## 💻 技術パネルの意見
### Fowler: “書き込み経路を一本化しろ”
- JSON書き込みが**3つの独立実装**に散らばっている。S式化は**共通I/Oモジュール**を作らないと再び分裂する。`src/lisp/shell/notifications.lisp:123-179`, `src/lisp/school/school-telemetry.lisp:22-34`, `src/lisp/school/school-backtest-utils.lisp:48-80`

### Hickey: “S式を使うなら、最小の形にしろ”
- alistのキー形式がバラつくとPython側で地獄。**symbol/keywordを一貫**させる設計が必須。現状はJSONキー前提で揺れている。`tools/report_backtest_summary.py:89-98`

### Uncle Bob: “テスト不在の即時単独は自殺”
- JSON前提のコードがまだ生きている。**最小のパーサテスト**と**ファイルI/Oテスト**なしで切り替えるのは事故。`tools/report_status.py:40-48`, `tools/report_backtest_summary.py:58-66`, `src/python/discord_bot.py:75-83`

## 🚀 ビジョナリーの意見
### Ng: “観測は止めない。止めるなら復旧設計を先に”
- 停止許容でも、復旧時に**何を信頼するか**が未定義。`system_metrics`のschema_version/last_updatedを必須にしろ。`src/lisp/school/school-telemetry.lisp:22-34`

### López de Prado: “データの系譜が切れる”
- 旧JSONをバックアップするだけでは不十分。**S式変換後の検算**が必要。`tools/report_status.py:80-107`, `src/lisp/school/school-backtest-utils.lisp:48-80`

### Gene Kim: “運用は痛みを可視化しろ”
- S式移行の失敗は静かに死ぬ。**書き込み失敗のアラート**を最優先で作れ。`src/lisp/shell/notifications.lisp:127-135`, `src/lisp/school/school-telemetry.lisp:22-34`

## 🚀 Musk's Decision (Final)
> 「S式即時単独はやる。だが“事故の温床”は今のままだ。  
>  **スキーマ定義・原子書き込み・Python共通パーサ・最小テスト**の4点セットを先に固めろ。  
>  それが終わるまで移行開始はしない。」

## Actionable Items
1. **S式スキーマ定義**：`backtest_cache/system_metrics/live_status`のS式構造と`schema_version`を文書化。`src/lisp/school/school-backtest-utils.lisp:48-80`, `src/lisp/school/school-telemetry.lisp:22-34`, `src/lisp/shell/notifications.lisp:123-179`
2. **原子書き込み**：S式保存は必ず`tmp→rename`で原子化（部分書き込み回避）。`src/lisp/shell/notifications.lisp:127-135`, `src/lisp/school/school-telemetry.lisp:22-34`, `src/lisp/school/school-backtest-utils.lisp:68-79`
3. **Python共通S式パーサ**：3箇所のJSON読み込みを共通S式パーサへ集約。`tools/report_status.py:40-48`, `tools/report_backtest_summary.py:58-66`, `src/python/discord_bot.py:75-83`
4. **最小テスト**：S式読み書きのスモークテストを追加（空/破損/正常の3ケース）。  

# 🦅 Expert Panel Report

**Date:** 2026-02-02  
**Leader:** Elon Musk  
**Trigger:** /expert-panel「単独レポート（oos_status.txt）の更新タイミングを決めたい」

## 🏛️ 常設顧問の意見
### Taleb: “メモリ計測は幻影。再起動で事故が消える”
- OOSメトリクスと失敗カウンタが**プロセス内メモリのみ**で、再起動でゼロになる。Ruinの兆候を消している。`src/lisp/school/school-validation.lisp:24` `src/lisp/school/school-validation.lisp:27`
- OOSキューはDBにあるのに**報告で無視**される。滞留や失敗が視界ゼロ。`src/lisp/school/school-db.lisp:92`

### Graham: “確認方法が嘘になってる”
- Owner’s GuideはEvolution Reportで確認と言うが、**ReportにOOS健康情報が無い**。ユーザーは真実に辿り着けない。`doc/owners_guide.md:175` `src/lisp/school/school-narrative.lisp:200`
- `oos-metrics-summary-line`は定義済みなのに未使用。**設計と実装の断絶**。`src/lisp/school/school-narrative.lisp:289`

### Naval: “レバレッジのある場所を捨ててる”
- 送信/受信のホットパスがあるのにstdoutログだけ。**イベント駆動でステータス更新**すれば最小工数で最大効果。`src/lisp/school/school-validation.lisp:94` `src/lisp/school/school-validation.lisp:132`
- DBキューがあるのに、集計はメモリ。**再起動耐性ゼロ**。`src/lisp/school/school-db.lisp:128` `src/lisp/school/school-validation.lisp:24`

### Jim Simons: “統計が死んでいる”
- レイテンシはrequest-id一致時のみ。**欠落が観測バイアス**になる。`src/lisp/core/message-dispatcher.lisp:206`
- 平均しか持たず、分散や履歴が消える。**統計の足場が無い**。`src/lisp/school/school-validation.lisp:157`

## 💻 技術パネルの意見
### Fowler: “レイヤー分断で設計が割れている”
- Report生成はOOSメトリクスを**一切参照しない**。責務分離のまま接続が無い。`src/lisp/school/school-narrative.lisp:200` `src/lisp/school/school-narrative.lisp:289`
- 報告は定期処理に依存だが、OOSはイベント駆動が自然。**更新タイミングがレイヤーで不一致**。`src/lisp/school/school-connector.lisp:79`

### Hickey: “状態が増えすぎてシンプルさを殺してる”
- 可変hash/listで状態保持、reset契約が曖昧。**複雑性の温床**。`src/lisp/school/school-validation.lisp:24` `src/lisp/school/school-validation.lisp:49`
- 真実はDBに置け。キューがあるなら**集計もDB**で良い。`src/lisp/school/school-db.lisp:92`

### Uncle Bob: “テストが無い場所は壊れる”
- OOS検証のテストはあるが、**レポート更新のテストがゼロ**。回帰に気づけない。`src/lisp/tests/school-split-tests.lisp:170`
- 仕様に更新タイミングが明記されていない。**テストの書きようがない**。`doc/owners_guide.md:175`

## 🚀 ビジョナリーの意見
### Ng: “OOS可観測性が薄いと品質が崩れる”
- OOSは品質ゲート。Reportに載らないのは**学習監査の放棄**。`src/lisp/school/school-narrative.lisp:200`

### López de Prado: “過学習を再輸入する設計”
- OOSキューの滞留を可視化しないと**選択バイアスの温床**になる。`src/lisp/school/school-db.lisp:92`

### Gene Kim: “Opsの視界がない”
- 定期報告に依存するのにOOSの失敗が入らない。**運用の盲点**。`src/lisp/school/school-connector.lisp:79`
- 既存のアラート経路をOOSに使っていない。**Ops負債**。`src/lisp/shell/notifications.lisp:87`

## 🚀 Musk's Decision (Final)
> 「更新は**イベント駆動が主**、**1時間ごとの再生成**が保険だ。  
>  OOSは品質ゲート。statusを“その場で”更新し、取りこぼしは定期で埋める。  
>  さらにEvolution ReportにOOS行を追加して、確認導線を一本化する。  
>  余計な履歴や可視化の過剰実装は今はやらない。」

## Actionable Items
1. **Evolution ReportにOOS行を埋め込む**：`oos-metrics-summary-line`を`generate-evolution-report`に組み込み。`src/lisp/school/school-narrative.lisp:200` `src/lisp/school/school-narrative.lisp:289`
2. **イベント駆動更新**：`maybe-request-oos-backtest` と `handle-oos-backtest-result` から `oos_status.txt` を更新。`src/lisp/school/school-validation.lisp:94` `src/lisp/school/school-validation.lisp:132`
3. **定期リカバリ更新**：`phase-7-report` と同タイミングで `oos_status.txt` を再生成。`src/lisp/school/school-connector.lisp:79`
4. **真実の一本化**：OOS status の集計はDBの `oos_queue` をソースにする。`src/lisp/school/school-db.lisp:92`
5. **監視アラート**：pending最古が閾値超ならDiscordに警告。`src/lisp/shell/notifications.lisp:87`
6. **テスト追加**：OOS status更新/Report統合のスモークテスト追加。`src/lisp/tests/school-split-tests.lisp:170`
