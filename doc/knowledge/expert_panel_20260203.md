# 🦅 Expert Panel Report

**Date:** 2026-02-03
**Leader:** Elon Musk
**Trigger:** 旧データ完全移行（A: オフライン一括移行）設計レビュー

## 🏛️ 常設顧問の意見
### Taleb:
- 「戦略だけ」移行は危険。`trade_logs` と `oos_queue` を落とせば運用の監査と回復力が死ぬ（`src/lisp/school/school-db.lisp:59-103`）。
- 自動マイグレーションが `#S` 件数<100 で走る設計は“後戻り爆弾”。スワップ直後に再移行されるリスクを遮断しろ（`src/lisp/school/school-db.lisp:6-118`）。
- 構造体変更の互換性注意が出ているのに、移行設計に「互換性検証」が無いのは自殺行為（`doc/owners_guide.md:38-41`）。

### Graham:
- いまの案は実装が重い。既に `INSERT OR REPLACE` で戦略を“正しい形式”に書き直す経路がある。それを使わないのは無駄（`src/lisp/school/school-db.lisp:218-249`）。
- `safe-read-sexp` と `read-from-string` が混在している時点で設計がぶれている。移行の最小単位は「既存の正規経路に乗せること」だけでいい（`src/lisp/school/school-db.lisp:334-357`、`src/lisp/school/school-db.lisp:388-393`）。

### Naval:
- レバレッジ不足。S式互換性は既に仕様に書いてある（`doc/owners_guide.md:52-54`）。ここを守るなら、変換ロジックは“1箇所”で良い。
- 新しい移行スクリプトより、既存のアップサート関数を再利用した方が運用コストが低い（`src/lisp/school/school-db.lisp:218-249`）。

### Simons:
- 指標を落とすとランクが崩壊する。`sharpe`/`profit_factor` などは表カラムにも構造体にも存在する。移行で 0 に潰す設計は統計的に致命傷（`src/lisp/school/school-db.lisp:13-36`、`src/lisp/dsl.lisp:155-166`）。
- `rank` は `:RANK` 文字列として保存される。ここを落とすと昇格/降格が初期化される（`src/lisp/school/school-db.lisp:241`）。

## 💻 技術パネルの意見
### Fowler:
- 移行ロジックは DB 層に閉じるべき。`init-db` と `migrate-existing-data` があるのに、別途スクリプトを作るのは設計の逸脱（`src/lisp/school/school-db.lisp:9-118`、`src/lisp/school/school-db.lisp:407-419`）。
- スキーマの重複定義（swap_history が二重）を見る限り、設計の“正本”が曖昧だ。移行手順には schema の出典を明記せよ（`src/lisp/school/school-db.lisp:73-93`）。

### Hickey:
- 手書きマッピングは壊れる。`strategy` はスロットが多く、追加も頻繁。plist→`make-strategy` の手動対応は必ず抜ける（`src/lisp/dsl.lisp:155-188`）。
- 読み込みは `safe-read-sexp` で統一しろ。現状は `read-from-string` が混ざっており、安全性と互換性の両方を毀損している（`src/lisp/school/school-db.lisp:334-357`、`src/lisp/school/school-db.lisp:388-393`）。

### Uncle Bob:
- テストがないのは論外。移行は“壊れたら全損”の機能なのに、自動テストが存在しない（`src/lisp/tests.lisp:1-48`）。
- 少なくとも「plist を入れると `strategy-p` が真になる」「全件が `#S` に再シリアライズされる」テストを追加すべきだ。

## 🚀 ビジョナリーの意見
### Ng:
- 学習/評価の指標が 0 に戻れば、A/S の育成が止まる。`oos_sharpe` と `cpcv` 指標は必ず保持しろ（`src/lisp/school/school-db.lisp:34-36`、`src/lisp/dsl.lisp:183-186`）。

### López de Prado:
- 変換不能データの“隔離”は正しいが、隔離率と理由が見えないと、統計的にバイアスが増える。カテゴリ・方向・ランクの分布を保持したまま移行しないと過剰最適化を助長する（`src/lisp/school/school-db.lisp:26-31`、`src/lisp/dsl.lisp:164-176`）。

### Gene Kim:
- ランブックに移行手順が無い。運用で再現できない改善は“存在しない”のと同じ（`doc/runbook.md:1-60`）。
- systemd停止/起動手順と検証観測点（ログ/件数/レポート）を明文化しろ。

## 🚀 Musk's Decision (Final)
> 「A案はやる。ただし“完全移行”と言うなら、戦略以外のテーブルも含めること、そして正規のアップサート経路で再シリアライズすること。テストとランブックが無い状態で本番移行は許可しない。」

## Actionable Items
1. `strategies` だけでなく `trade_logs` と `oos_queue` を含む全テーブルの移行方針を明文化する（`src/lisp/school/school-db.lisp:59-103`）。
2. 変換は既存のアップサート経路（`format nil "~s" strat`）を必ず使い、手書きマッピングは最小限にする（`src/lisp/school/school-db.lisp:218-249`）。
3. 移行時は自動マイグレーションが走らないことを保証する（`src/lisp/school/school-db.lisp:6-118`）。
4. 移行テストを追加し、旧 plist → `strategy-p` 成功 と `#S` 再シリアライズを自動検証する（`src/lisp/tests.lisp:1-48`）。
5. runbook に停止/バックアップ/検証/復旧の手順を追記する（`doc/runbook.md:1-60`）。

---

# 🦅 Expert Panel Report

**Date:** 2026-02-03
**Leader:** Elon Musk
**Trigger:** /expert-panel「test_heartbeat.py が unittest 探索で実送信される問題。修正方針の相談」

## 🏛️ 常設顧問の意見
### Taleb:
- “テスト実行で外部に実送信”は運用リスクの塊。失敗時の通知暴発やWebhook無効化で**テスト全体が停止**する（`test_heartbeat.py:1-29`）。
- Webhook URLの**ハードコード**はリークと回転不能の二重リスク。最悪は通知先が第三者に乗っ取られる（`test_heartbeat.py:8-9`）。

### Graham:
- `test_heartbeat.py` は**テストではない**のに test_*.py を名乗っているのが設計ミス。名前の一行で全体が壊れる（`test_heartbeat.py:1-9`）。
- まず小さく直せ。**移動/改名だけ**でunittest探索から外せる。これ以上の“大工事”は不要。

### Naval:
- 実送信スクリプトは**ツール**として分離すべき。CLIにして `scripts/` へ置けば、再利用性も高く誤実行も減る（`test_heartbeat.py:1-29`）。
- 既に webhook がenvで管理される設計があるのに（`.env`/既存通知系）**再発明**してる。

### Simons:
- “テストが0件で止まる”のは**計測破壊**。品質ゲートが壊れている証拠。ハードコードURLは統計的に**再現性が無い**（`test_heartbeat.py:8-9`）。
- 送信可否を**ユニットテストに混ぜる**のは誤り。外部I/Oはモックで分離し、実送信は手動ツールに限定すべき。

## 💻 技術パネルの意見
### Fowler:
- テスト探索の契約に反している。`test_*.py` なのに副作用があるのは**テスト設計違反**（`test_heartbeat.py:1-29`）。
- 解決策は構造化で、`scripts/` など“ツール領域”に移すのが設計として一番分かりやすい。

### Hickey:
- “名前がtestだから実行される”は**偶然動作**に依存している。**名前で意味を表す**べきで、送信は `send_heartbeat.py` のように明確化すべき。
- `if __name__ == "__main__":` が無いのは**Pythonの基本を無視**している（`test_heartbeat.py:1-29`）。

### Uncle Bob:
- テストは**決定的で再現可能**であるべき。外部Webhookは非決定的なのでテストに入れてはいけない。
- どうしても送信確認を残すなら、**名前をtestから外す**＋**main guard**が最低限。さらに送信先はenvから読むようにしろ。

## 🚀 ビジョナリーの意見
### Ng:
- 送信テストは“運用ツール”であって、テストランナーに混ぜるべきではない。**評価フローの汚染**が起きる（`test_heartbeat.py:1-29`）。

### López de Prado:
- 外部I/Oを混ぜると**テスト結果が分布で揺れる**。その時点で品質指標として意味を失う。

### Gene Kim:
- 失敗時に“テスト0件”は運用上最悪。CI/手動検証の**信頼性を破壊**する。まず探索対象から外せ（`test_heartbeat.py:1-29`）。
- Webhookは**秘匿情報**。コードに置くのは運用上NG。

## 🚀 Musk's Decision (Final)
> 「即時に“test_*.py”から外せ。次にmain guardとenv化で事故を封じる。実送信はツールとして隔離し、テストは決定的に保て。」

## Actionable Items
1. `test_heartbeat.py` を `scripts/send_heartbeat.py` へ移動・改名してunittest探索から除外する（`test_heartbeat.py:1-29`）。
2. 実送信処理を `if __name__ == "__main__":` でガードし、インポート時の副作用を排除する（`test_heartbeat.py:1-29`）。
3. Webhook URL を環境変数化し、コード直書きを廃止する（`test_heartbeat.py:8-9`）。

---

# 🦅 Expert Panel Report

**Date:** 2026-02-03
**Leader:** Elon Musk
**Trigger:** /expert-panel「MCPサーバ本体の公開方式（stdio/HTTP/WS）選定」

## 🏛️ 常設顧問の意見
### Taleb:
- HTTP/WSで外部公開するのは“自爆ボタン”。APIキーが設定されていても検証されていない（`tools/mcp_gateway.py:33-43`、`tools/mcp_gateway_config.py:15-18`）。
- 取引禁止でもbacktestは実行できる。外部公開は資源枯渇やDoSで運用を殺すだけ（`tools/mcp_gateway.py:37-43`）。

### Graham:
- まだ“サーバ”がないのにプロトコル議論をしている。今あるのは関数だけだ（`tools/mcp_gateway.py:33-43`）。
- テストも関数単位でしかない。まずstdioで薄いラッパーを作れ（`tools/test_mcp_gateway.py:1-9`）。

### Naval:
- 目的は自動化のレバレッジだ。WSは重い。ローカルMCPならstdioが最小コスト（`systemd/swimmy-mcp-gateway.service:1-11`）。
- いまのユニットは“デーモン常駐”なのにやることが無い。まずCLI/stdioで価値を出せ（`systemd/swimmy-mcp-gateway.service:1-11`）。

### Simons:
- PUBで投げっぱなしは統計的に最悪。受理確認が無い限り、HTTP/WSは“見た目だけの信頼性”（`tools/mcp_gateway_zmq.py:4-11`、`tools/mcp_gateway.py:37-43`）。
- status取得はファイル読むだけで、統合されていない。外部公開の前に監査経路を作れ（`tools/mcp_gateway_observer.py:1-8`）。

## 💻 技術パネルの意見
### Fowler:
- `bind_host`/`bind_port` が死んでいる。サーバ境界が無い設計ドリフトだ（`tools/mcp_gateway_config.py:5-18`、`tools/mcp_gateway.py:33-43`）。
- アーキ図にMCP境界が無いのに公開方式だけ議論している。まず設計図に載せろ（`doc/SYSTEM_ARCHITECTURE.md:12-18`）。

### Hickey:
- グローバル状態でソケットを握る設計は複雑化の温床。プロトコルを増やす前に“単純さ”を取り戻せ（`tools/mcp_gateway.py:23-30`）。
- 外部境界はJSONと書いてあるのに、MCPのTransportが明記されていない。仕様の欠落だ（`docs/llm/INTERFACES.md:3-20`）。

### Uncle Bob:
- サーバ契約のテストが無い。戻り値のステータスしか見ていない（`tools/test_mcp_gateway.py:1-9`）。
- APIキーは存在するがテストされていない。認可は“無いのと同じ”（`tools/mcp_gateway_config.py:15-18`）。

## 🚀 ビジョナリーの意見
### Ng:
- LLM/MCP連携の初手はstdioが標準。HTTP/WSを足すほど運用が重くなる（`docs/llm/INTERFACES.md:16-20`）。
- いまのスコープはread-only/backtest-exec。重い双方向接続は不要（`docs/llm/STATE.md:4-9`）。

### López de Prado:
- まだtradeは封印なのに“将来のためにWS”は過学習。現状に合う最小構成にせよ（`docs/llm/STATE.md:4-9`、`tools/mcp_gateway.py:33-34`）。
- 仕様の空白が多い。アーキ図にMCPが無い状態で公開方式を選ぶのは統計的に誤り（`doc/SYSTEM_ARCHITECTURE.md:12-18`）。

### Gene Kim:
- HTTP/WSは運用負債を増やす。systemd/監視/ランブックが揃うまで外部公開は禁物（`systemd/swimmy-mcp-gateway.service:1-11`）。
- 監査経路が弱い。status取得が単体関数で止まっているのは運用事故の種（`tools/mcp_gateway_observer.py:1-8`）。

## 🚀 Musk's Decision (Final)
> 「まずはstdioで行く。HTTP/WSは“要件が出てから”だ。ネットワーク公開はAPIキー検証と監査ログが揃うまで禁止。」

## Actionable Items
1. MCPサーバ(stdio)の薄いエントリを追加し、`handle_backtest_submit`/`handle_trade_submit` を呼ぶ（`tools/mcp_gateway.py:33-43`）。
2. APIキー検証をサーバ境界で必須化し、未設定/不一致は即拒否（`tools/mcp_gateway_config.py:15-18`）。
3. TransportとI/O仕様を `docs/llm/INTERFACES.md` に明記し、アーキテクチャ図にもMCP境界を追記（`docs/llm/INTERFACES.md:3-20`、`doc/SYSTEM_ARCHITECTURE.md:12-18`）。
4. MCPサーバ契約テストを追加し、trade.submitの403とbacktest.submitのrequest_idを固定化（`tools/test_mcp_gateway.py:1-9`）。
5. もしHTTP/WSを検討するなら、systemdユニット/運用手順/公開ポート設計を先にドキュメント化（`systemd/swimmy-mcp-gateway.service:1-11`）。

---

# 🦅 Expert Panel Report

**Date:** 2026-02-03
**Leader:** Elon Musk
**Trigger:** SwimmyBridge.mq5 の最新化可否 / MT5 ZMQプロトコル整合性レビュー

## 🏛️ 常設顧問の意見
### Taleb:
- 「S式統一」が正本なのに、MT5側はJSONパーサ前提で動いている。境界が“静かに壊れる”典型で、誤パースはそのまま誤注文になる（`docs/llm/INTERFACES.md:5`、`docs/llm/STATE.md:21`、`src/mt5/SwimmyBridge.mq5:82`）。
- WSL IPが手動かつコードのデフォルトが固定値。誤接続・無接続時の“沈黙”は破滅リスク（`docs/llm/STATE.md:24`、`src/mt5/SwimmyBridge.mq5:16`）。

### Graham:
- 仕様と実装の整合が取れていない時点で「最新化しなくて大丈夫」はあり得ない。正本はS式、実装はJSONで完全に二重管理（`docs/llm/SPEC.md:44`、`docs/llm/ARCHITECTURE.md:33`、`src/mt5/SwimmyBridge.mq5:598`）。
- `doc/SYSTEM_ARCHITECTURE.md` は2025-12-29のV3.0で、`brain.lisp` 前提の記述が残っている。現行構成と齟齬がある（`doc/SYSTEM_ARCHITECTURE.md:1`、`doc/SYSTEM_ARCHITECTURE.md:35`、`doc/SYSTEM_ARCHITECTURE.md:151`）。

### Naval:
- プロトコルが「ドキュメント」「Lisp生成」「MT5パース」の3箇所に分散。変更のたびに3倍の運用コストが発生する（`docs/llm/INTERFACES.md:52`、`src/lisp/core/execution-protocol.lisp:31`、`src/mt5/SwimmyBridge.mq5:469`）。
- `ORDER_OPEN` のキーが `action/symbol` と `side/instrument` で割れている。これでは自動化レバレッジが増えない（`docs/llm/INTERFACES.md:52`、`src/lisp/core/execution-protocol.lisp:56`、`src/mt5/SwimmyBridge.mq5:491`）。

### Simons:
- `REQ_HISTORY` の要求フィールドが `volume` と `count` で食い違い、MT5側で100,000本送信にフォールバックしている。データ量と統計の一貫性が壊れる（`src/lisp/system/runner.lisp:102`、`src/mt5/SwimmyBridge.mq5:543`、`src/mt5/SwimmyBridge.mq5:337`）。
- `SWAP_DATA` 等のデータが実運用で使われているのに、仕様書にスキーマが無い。統計再現性が担保できない（`src/lisp/core/message-dispatcher.lisp:279`、`src/mt5/SwimmyBridge.mq5:575`、`docs/llm/INTERFACES.md:22`）。

## 💻 技術パネルの意見
### Fowler:
- 仕様はS式、実装はJSON、さらに`ORDER_OPEN`のキーまで別物。これは境界契約の崩壊で、設計として破綻している（`docs/llm/INTERFACES.md:5`、`docs/llm/INTERFACES.md:52`、`src/lisp/core/execution-protocol.lisp:56`、`src/mt5/SwimmyBridge.mq5:491`）。
- 文字列部分一致で命令判定するのは“偶然動作”。`"BUY"`が含まれるだけで注文処理が走るのは危険（`src/mt5/SwimmyBridge.mq5:491`）。

### Hickey:
- 「S式統一」と言いながらJSON前提コードが増殖している。シンプルさが完全に失われている（`docs/llm/STATE.md:21`、`src/lisp/core/message-dispatcher.lisp:279`、`src/mt5/SwimmyBridge.mq5:82`）。
- `execution-protocol.lisp` にメッセージ型が定義されているのに、仕様書と一致しない。二重定義は複雑性の温床（`src/lisp/core/execution-protocol.lisp:10`、`docs/llm/INTERFACES.md:52`）。

### Uncle Bob:
- 仕様と実装の整合性を検証するテストが無い。`make-order-message` がMT5で受理される保証はゼロ（`src/lisp/core/execution-protocol.lisp:31`、`src/mt5/SwimmyBridge.mq5:469`）。
- JSONパースが手書きで脆弱。鍵が見つからないと0に落ちる設計はバグを隠すだけ（`src/mt5/SwimmyBridge.mq5:85`）。

## 🚀 ビジョナリーの意見
### Ng:
- 学習・監視の入力（TICK/ACCOUNT_INFO）がJSON依存で動いている。S式移行の方針と食い違えば、学習の信頼性が崩れる（`docs/llm/INTERFACES.md:22`、`src/lisp/core/message-dispatcher.lisp:279`）。

### López de Prado:
- `CLOSE_SHORT_TF` の挙動が仕様化されていない。D1以上を“保護”するロジックは統計的に重大なバイアスを生む（`src/mt5/SwimmyBridge.mq5:426`、`src/mt5/SwimmyBridge.mq5:539`）。

### Gene Kim:
- ドキュメントが多層で矛盾している。運用現場は“どれが正本か”で迷う。これは事故の前兆（`docs/llm/SPEC.md:10`、`doc/owners_guide.md:305`、`doc/SYSTEM_ARCHITECTURE.md:1`）。
- 既知のWSL IP手動問題が放置されている。運用バグが仕様に残っているのはDevOpsとして失格（`docs/llm/STATE.md:24`、`src/mt5/SwimmyBridge.mq5:16`）。

## 🚀 Musk's Decision (Final)
> 「“最新化しなくて良い”は却下だ。まずMT5プロトコルの正本を決めろ。S式で行くならMT5とLispを統一、JSON例外なら仕様に明記。決着がつくまで変更禁止。」

## Actionable Items
1. MT5↔Guardianのエンコーディング方針を決定し、`docs/llm/INTERFACES.md` と `docs/llm/STATE.md` に明記する（`docs/llm/INTERFACES.md:5`、`docs/llm/STATE.md:21`）。
2. `ORDER_OPEN` のキー（`action/symbol/lot` vs `side/instrument/lot/volume`）を統一し、ドキュメントと両実装を揃える（`docs/llm/INTERFACES.md:52`、`src/lisp/core/execution-protocol.lisp:56`、`src/mt5/SwimmyBridge.mq5:491`）。
3. `REQ_HISTORY` の `volume/count` 食い違いを修正し、要求/応答スキーマを仕様化する（`src/lisp/system/runner.lisp:102`、`src/mt5/SwimmyBridge.mq5:543`）。
4. `HISTORY/POSITIONS/SWAP_DATA/ORDER_ACK/TRADE_CLOSED/GET_POSITIONS/GET_SWAP/CLOSE_SHORT_TF` を `docs/llm/INTERFACES.md` に追加し、既存運用と一致させる（`src/mt5/SwimmyBridge.mq5:235`、`src/mt5/SwimmyBridge.mq5:679`、`src/mt5/SwimmyBridge.mq5:575`）。
5. `InpWSL_IP` の固定値依存を排除し、設定/自動検出とドキュメントを整合させる（`src/mt5/SwimmyBridge.mq5:16`、`docs/llm/STATE.md:24`）。

---

# 🦅 Expert Panel Report

**Date:** 2026-02-03
**Leader:** Elon Musk
**Trigger:** MT5↔Guardian エンコーディング方針（S式統一 vs MT5 JSON例外）の最終決定

## 🏛️ 常設顧問の意見
### Taleb:
- 「S式統一」が正本なのに、MT5側は手書きJSONパースで動いている。これは“静かな破滅”で、欠損キーが0に落ちても検知されない（`docs/llm/INTERFACES.md:5`、`docs/llm/SPEC.md:51`、`src/mt5/SwimmyBridge.mq5:82`）。
- 文字列部分一致で `BUY/SELL` を拾うのは事故の温床。ログ混入で誤注文が起きる（`src/mt5/SwimmyBridge.mq5:491`）。

### Graham:
- ドキュメントがS式、LispはJSON生成、MT5はJSON手書き。この三重分岐はスケールしない。決定して一本化しろ（`docs/llm/INTERFACES.md:5`、`src/lisp/core/execution-protocol.lisp:31`、`src/mt5/SwimmyBridge.mq5:82`）。
- `ORDER_OPEN` のキーが `action/symbol` と `side/instrument` でズレている。仕様が死んでいる（`docs/llm/INTERFACES.md:99`、`src/lisp/core/execution-protocol.lisp:56`、`src/mt5/SwimmyBridge.mq5:471`）。

### Naval:
- “例外”を作るなら境界で吸収しろ。MT5だけJSONなら Rust で変換層を固定し、Lisp/MT5の二重実装はやめろ（`src/lisp/core/message-dispatcher.lisp:279`、`src/lisp/core/execution-protocol.lisp:31`）。
- 単一の契約と単一のテストを持て。いまは契約がコードに分散している（`docs/llm/INTERFACES.md:22`、`src/mt5/SwimmyBridge.mq5:469`）。

### Simons:
- 仕様と実装のズレは統計的に“ノイズ”として現れる。HISTORYやSWAPが学習や監視のデータ品質を落とす（`docs/llm/INTERFACES.md:49`、`src/mt5/SwimmyBridge.mq5:575`、`src/lisp/core/message-dispatcher.lisp:279`）。

## 💻 技術パネルの意見
### Fowler:
- `type` ではなく文字列部分一致で分岐するのは境界の崩壊。`ORDER_OPEN` は厳密判定に戻せ（`src/mt5/SwimmyBridge.mq5:491`）。
- 仕様（S式）と実装（JSON）を同居させるなら、**翻訳レイヤ**を明示し、そこ以外では混ぜるな（`docs/llm/INTERFACES.md:5`、`src/lisp/core/execution-protocol.lisp:31`）。

### Hickey:
- 文字列検索でJSONをパースするのは壊れる。S式統一か、少なくともJSONパーサを共有しろ（`src/mt5/SwimmyBridge.mq5:85`）。
- `INTERFACES.md` が正本なら、Lispの`execution-protocol`はS式で書くべき。現状は“仕様の分裂”だ（`docs/llm/INTERFACES.md:22`、`src/lisp/core/execution-protocol.lisp:31`）。

### Uncle Bob:
- プロトコル契約テストが足りない。`ORDER_OPEN` のキーや `REQ_HISTORY` の必須/任意が壊れても検知できない（`docs/llm/INTERFACES.md:99`、`src/mt5/SwimmyBridge.mq5:543`）。
- 欠損キーを0で握り潰すのは“バグの隠蔽”。入力バリデーションを追加しろ（`src/mt5/SwimmyBridge.mq5:85`）。

## 🚀 ビジョナリーの意見
### Ng:
- 学習系は入力の一貫性が命。S式/JSONの揺れは指標のドリフトに直結する（`docs/llm/SPEC.md:51`、`src/lisp/core/message-dispatcher.lisp:279`）。

### López de Prado:
- `CLOSE_SHORT_TF` のような例外処理は統計バイアスを生む。仕様に明記した以上、影響評価と適用条件を固定しろ（`docs/llm/INTERFACES.md:141`、`src/mt5/SwimmyBridge.mq5:539`）。

### Gene Kim:
- 仕様/実装/運用がねじれている。まず“どれが正本か”を確定してから運用に反映しろ（`docs/llm/STATE.md:21`、`docs/llm/INTERFACES.md:5`）。

## 🚀 Musk's Decision (Final)
> 「どちらかを“今日決める”。S式統一ならMT5/Guardianの変換を一本化。JSON例外ならそれを仕様に明記し、境界で完結させろ。決定前の機能追加は禁止。」

## Actionable Items
1. S式統一 or MT5 JSON例外の**最終決定**を行い、`SPEC/ARCHITECTURE/INTERFACES/STATE` に明記する（`docs/llm/SPEC.md:51`、`docs/llm/ARCHITECTURE.md:33`、`docs/llm/INTERFACES.md:5`、`docs/llm/STATE.md:21`）。
2. `ORDER_OPEN` のキーと判定ルールを単一化し、文字列部分一致を廃止する（`docs/llm/INTERFACES.md:99`、`src/mt5/SwimmyBridge.mq5:491`）。
3. プロトコル契約テストを追加し、MT5↔Guardian のメッセージ構造をCIで固定化する（`src/lisp/tests.lisp:140`）。

---

# 🦅 Expert Panel Report

**Date:** 2026-02-03
**Leader:** Elon Musk
**Trigger:** /expert-panel「MCP stdioサーバをsystemd常駐（B案）で行くべきか？」

## 🏛️ 常設顧問の意見
### Taleb:
- 常駐化は攻撃面を増やす。APIキー検証が未実装のままプロセスを上げるのは破滅的（`tools/mcp_gateway_config.py:12-18`、`tools/mcp_gateway.py:33-43`）。
- 今は“trade封印”だがバックテストは実行できる。常駐でリソース枯渇を招く（`tools/mcp_gateway.py:37-43`）。

### Graham:
- MCP stdioは本来オンデマンド。常駐化は過剰だし、CLIが無いのに常駐を決めるのは順序が逆（`tools/mcp_gateway.py:33-43`）。
- systemdユニットがある時点で“やった気”になるが、実体は関数群だけ（`systemd/swimmy-mcp-gateway.service:1-11`）。

### Naval:
- レバレッジ視点では、まずstdioサーバを小さく作ってMCPホストに起動させるべき。常駐化は運用負債の先送り（`systemd/swimmy-mcp-gateway.service:1-11`）。
- 標準のMCP接続（stdio）を外して独自常駐に走るのは自動化の逆（`docs/llm/INTERFACES.md:16-20`）。

### Simons:
- PUBで投げっぱなしは検証不能。常駐化しても統計的な信頼性は上がらない（`tools/mcp_gateway_zmq.py:4-11`、`tools/mcp_gateway.py:37-43`）。
- status/metricsはまだ実装されていない。常駐の前に“観測”を整備しろ（`docs/llm/INTERFACES.md:16-20`）。

## 💻 技術パネルの意見
### Fowler:
- `bind_host`/`bind_port` が未使用。常駐の話をする前に“サーバ境界”の責務を定義せよ（`tools/mcp_gateway_config.py:5-18`）。
- アーキテクチャ図にMCP境界が無い。運用形態を決める前に設計図を更新せよ（`doc/SYSTEM_ARCHITECTURE.md:12-18`）。

### Hickey:
- グローバルなソケットキャッシュは常駐化と最悪の相性。複雑さが一気に増える（`tools/mcp_gateway.py:23-30`）。
- MCPの外部境界仕様が曖昧なまま常駐を決めるのは設計の怠慢（`docs/llm/INTERFACES.md:3-20`）。

### Uncle Bob:
- 常駐サービスなら契約テスト必須だが、現状は関数呼び出しテストしかない（`tools/test_mcp_gateway.py:1-9`）。
- APIキー認可のテストが無い。常駐化は“無防備な常時公開”と同義（`tools/mcp_gateway_config.py:12-18`）。

## 🚀 ビジョナリーの意見
### Ng:
- MCPの典型はstdioオンデマンド。常駐は運用上の摩擦を生み、MCPホストとの相性も悪い（`docs/llm/INTERFACES.md:16-20`）。

### López de Prado:
- 常駐化は“将来のため”という過学習。今のスコープ（read-only/backtest-exec）に合わない（`docs/llm/STATE.md:4-9`）。
- 監査・メトリクス経路が無い状態で常駐を選ぶのは統計的に誤り（`docs/llm/INTERFACES.md:16-20`）。

### Gene Kim:
- systemd常駐は運用負債。runbook/監視/ログが揃わない限りやるべきではない（`systemd/swimmy-mcp-gateway.service:1-11`）。
- まずはオンデマンドで稼働実績を積み、必要になってから常駐化すべきだ。

## 🚀 Musk's Decision (Final)
> 「B案（常駐）は今はやらない。まずMCP stdioオンデマンドで動かし、APIキー検証と監査ログが揃ったら常駐を検討する。」

## Actionable Items
1. MCP stdioサーバをオンデマンド起動で実装し、常駐前提のsystemdユニットは保留扱いにする（`systemd/swimmy-mcp-gateway.service:1-11`）。
2. APIキー検証と監査ログを実装し、常駐化の前提条件を満たす（`tools/mcp_gateway_config.py:12-18`、`tools/mcp_gateway.py:33-43`）。
3. MCP境界仕様（stdio/JSON-RPC）を `docs/llm/INTERFACES.md` とアーキ図に明記する（`docs/llm/INTERFACES.md:3-20`、`doc/SYSTEM_ARCHITECTURE.md:12-18`）。
4. 常駐化を検討する場合はrunbookに停止/障害/再起動の運用手順を先に追加する（`doc/owners_guide.md:1-20`）。

---

# 🦅 Expert Panel Report

**Date:** 2026-02-03
**Leader:** Elon Musk
**Trigger:** /expert-panel「MCP stdio JSON-RPC設計。JSONで良いか？」

## 🏛️ 常設顧問の意見
### Taleb:
- JSONそのものは良いが、**“JSONなら何でもOK”**になるのが最大の破局要因。JSON-RPC以外を通す設計は即死（`docs/llm/INTERFACES.md:3-20`）。
- `params.api_key` 前提は“盗まれたら終わり”の設計。認可は必須だが、**未実装で公開するのが最悪**（`tools/mcp_gateway_config.py:12-18`）。

### Graham:
- JSON-RPC一本で十分。**テスト用1行JSONモードは過剰**で、仕様の分岐を生むだけ。
- `backtest.status` が最新サマリしか返せないなら、**request_idは“嘘”**になる。最小構成なら最小に徹せよ（`src/lisp/core/message-dispatcher.lisp:105-121`）。

### Naval:
- JSONは外部境界でのみ使え。内部S式と混ぜた瞬間にメンテ不能になる（`docs/llm/INTERFACES.md:3-20`）。
- stdioオンデマンドなら軽く回せる。**“jsonならいいよね？”はYesだが“jsonなら何でも”はNo**。

### Simons:
- `backtest_status.txt` は集計サマリ。**request_id紐付け無しで返すのは統計的に誤導**（`src/lisp/core/message-dispatcher.lisp:105-121`）。
- system.metrics の真実は `system_metrics.sexp`。JSONを足すなら“正本がどれか”を決めろ（`tools/report_status.py:33-83`）。

## 💻 技術パネルの意見
### Fowler:
- MCP境界が設計図に無い。JSON-RPCの“仕様点”を図とドキュメントに明記せよ（`doc/SYSTEM_ARCHITECTURE.md:12-18`、`docs/llm/INTERFACES.md:3-20`）。
- `bind_host`/`bind_port` が死んでいる。サーバ境界の責務が曖昧だ（`tools/mcp_gateway_config.py:5-18`）。

### Hickey:
- 仕様の二重化は複雑さを増やす。JSON-RPCに固定し、**別フォーマットの受理は切れ**。
- 内部はS式正本。JSONは“入り口だけ”というルールを文書化しろ（`docs/llm/INTERFACES.md:3-20`）。

### Uncle Bob:
- JSON-RPCの契約テストが無い。**フレーミング（Content-Length）とエラー構造**をテストで固定すべき。
- `trade.submit` 403 と `backtest.submit` 202 の“約束”をテストに落とせ（`tools/mcp_gateway.py:33-43`）。

## 🚀 ビジョナリーの意見
### Ng:
- LLM/MCPの現場はJSON-RPCが標準。**JSONは正しい**が、スキーマ検証無しは危険。

### López de Prado:
- “latest summary” を返すなら明記せよ。**誤ラベルは評価を歪める**（`src/lisp/core/message-dispatcher.lisp:105-121`）。

### Gene Kim:
- JSON-RPCはOKだが、**ログと監査が無いなら運用不可**。request_idの追跡線を作れ。
- runbookに「stdio起動/ログ確認/障害時の切り分け」を書け（`doc/owners_guide.md:1-20`）。

## 🚀 Musk's Decision (Final)
> 「JSONはOK。ただしJSON-RPC 2.0のみに限定し、**自由形式JSONは受け付けない**。request_idが本当に追跡できるまで`backtest.status`は“latest summary”と明記。APIキー検証と監査ログが揃うまで公開はしない。」

## Actionable Items
1. JSON-RPC 2.0のフレーミング（Content-Length）仕様を明文化し、**非JSON-RPC入力は拒否**する（`docs/llm/INTERFACES.md:3-20`）。
2. `backtest.status` の返却が最新サマリであることを明記し、将来のrequest_id紐付け計画を設ける（`src/lisp/core/message-dispatcher.lisp:105-121`）。
3. `system_metrics.sexp` を正本とし、JSONは境界のみと文書化する（`tools/report_status.py:33-83`）。
4. JSON-RPC契約テスト（403/202/エラー構造）を追加する（`tools/mcp_gateway.py:33-43`）。
5. MCP境界をアーキ図とrunbookに追記する（`doc/SYSTEM_ARCHITECTURE.md:12-18`、`doc/owners_guide.md:1-20`）。
