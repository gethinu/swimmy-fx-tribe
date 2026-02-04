# 🦅 Expert Panel Report

**Date:** 2026-02-04  
**Leader:** Elon Musk  
**Trigger:** 「批判レポートだけでなく、相談用途でも expert-panel を使いたい。調整方針は？」

## 🏛️ 常設顧問の意見
### Taleb: “批判”と“相談”は混ぜるな
- ワークフローは意地悪な批判を前提に設計されている。ここに相談モードを混ぜると、**リスク評価が薄まる**。`doc/archive/workflows/expert-panel.md:7-38,83` に反する。  
- 対応するなら「モード切替」を明示して、**どちらの契約で話しているかを固定**しろ。

### Graham: 使い分けは最小限に
- 相談モードが欲しいなら、**最小の差分**で良い。  
- 出力形式は維持しつつ、「目的」「制約」「成功条件」を冒頭で1行追加するだけで十分。`doc/archive/workflows/expert-panel.md:49-79` のフォーマットに追記するのが筋。

### Naval: 相談は“選択肢”が命
- 相談として使うなら、各専門家の意見に**2択/3択の提案**を混ぜろ。  
- MuskのDecisionは「やる/やらない」だけでなく、**“今やる / 後でやる / やらない”の時間軸**を示せ。

### Jim Simons: 相談は「不確実性」を明記せよ
- 相談レポートは、**どこが不確実か**を明示しないと価値がない。  
- レポート内に「未確定前提」「必要データ」を必須項目にするべき。

## 💻 技術パネルの意見
### Fowler: 契約としての“モード”を明文化
- 現行フローは「批判専用」。相談モードを追加するなら、**Workflowに明示的なモード選択を追加**すべき。`doc/archive/workflows/expert-panel.md:28-48` への追記が必要。  
- “共存”より“分離”が安全。モードごとに注意事項を分けろ。

### Hickey: シンプルに、ただし境界は明確に
- 二重目的は複雑さを生む。相談モードは**最小の差分**に留めろ。  
- 批判レポートの骨格を保ちつつ「相談向けの短い提案」を1段落追加する程度でよい。

### Uncle Bob: テンプレートに“意図”を固定しろ
- 相談用途でもブレないように、**テンプレートに目的欄を追加**しろ。  
- 目的が曖昧だとレビューがブレる。`doc/archive/workflows/expert-panel.md:49-79` のテンプレ修正が必要。

## 🚀 ビジョナリーの意見
### Ng: 相談には“前提質問”が必要
- 相談として使うなら、**前提質問**を必須化しろ（時間/予算/優先度）。  
- 「批判」と「相談」は入力の質が違う。最初に入力を揃える仕組みが必要。

### López de Prado: “助言の過学習”を防げ
- 相談は“望みの答え”に寄りやすい。  
- 強制的に**対抗意見**を入れるべき。批判モードの強さを落とさない構造が必要。

### Gene Kim: 運用フローを壊すな
- 相談モード追加は運用フローの変更。**どの時点でどのモードを使うか**を決めるべき。  
- 迷うなら、最初の質問で「批判/相談」だけを選ばせるシンプルな運用にせよ。

## 🚀 Musk's Decision (Final)
> 「“相談モード”は追加する。ただし**批判モードを弱めない**。最小差分で、モード選択を明示し、テンプレに目的・前提・不確実性を追加する。」

## Actionable Items
1. expert-panel ワークフローに「mode: critique / consult」を追加し、注意事項を分離: `doc/archive/workflows/expert-panel.md:28-48,81-85`
2. 出力テンプレに「目的」「前提」「不確実性」を追加（相談モードのみ必須）: `doc/archive/workflows/expert-panel.md:49-79`
3. 相談モードでは各専門家の意見に2択/3択の提案を必ず含めるルールを追加: `doc/archive/workflows/expert-panel.md:34-38`
4. “対抗意見”の必須化（相談で結論が片寄らないように）: `doc/archive/workflows/expert-panel.md:34-38`

---

# 🦅 Expert Panel Report

**Date:** 2026-02-04
**Leader:** Elon Musk
**Trigger:** MCP/MT5プロトコル整合と backtest_status の request_id 設計相談

## 🏛️ 常設顧問の意見
### Taleb:
- `ORDER_OPEN` が文字列検索に依存しており、偶発的な "BUY/SELL" 文字列で誤発火する危険がある。これは「想定外取引」という破滅的リスクそのものだ（`src/mt5/SwimmyBridge.mq5:491-498`）。
- WSL IP がデフォルト固定かつ手動更新。障害時に復旧不能になる典型的な“単一点”だ（`src/mt5/SwimmyBridge.mq5:16-22`、`docs/llm/STATE.md:24-26`）。

### Graham:
- ドキュメントは `action/symbol`、実装は `side/instrument`。このズレは「誰も触りたくないコード」を量産する（`docs/llm/INTERFACES.md:104-114`、`src/lisp/core/execution-protocol.lisp:43-67`）。
- MCPの認証が `params.api_key` 前提。インターフェースが泥臭く、クライアント側の実装コストが無駄に上がる（`tools/mcp_stdio_server.py:55-71`）。

### Naval:
- `backtest_status.txt` に request_id が無いので、結果と要求を自動で結び付けられない。これは自動化のレバレッジを捨てている（`src/lisp/core/message-dispatcher.lisp:115-123`）。
- MCP経由の backtest でも `request_id` を発行し直しており、外部IDとの連携が不可能。パイプラインの“橋”が切れている（`tools/mcp_gateway.py:37-42`）。

### Simons:
- request_id 不在は検証の再現性破壊。バックテスト結果の遅延・落ち・重複が測定不能だ（`src/lisp/core/message-dispatcher.lisp:115-123`）。
- 実装と仕様がズレている状態は、パフォーマンス統計の意味を壊す。測定系が壊れているのに学習するのは地雷（`docs/llm/INTERFACES.md:104-114`、`src/lisp/core/execution-protocol.lisp:59-62`）。

## 💻 技術パネルの意見
### Fowler:
- Protocol V2 を名乗るなら“厳密”が必要。`StringFind` でフォールバックするのは設計が二重化している証拠（`src/mt5/SwimmyBridge.mq5:491-504`）。
- 仕様書と実装の二重管理はアーキテクチャ腐敗の入口。単一の真実に統一しろ（`docs/llm/INTERFACES.md:104-114`、`src/lisp/core/execution-protocol.lisp:43-67`）。

### Hickey:
- MQLの“手作りJSONパーサ”は脆い。仕様変更一つで壊れる。`GetStringFromJson`/`GetValueFromJson` は構造化データの扱い方として最悪（`src/mt5/SwimmyBridge.mq5:85-112`）。
- `ORDER_OPEN` が厳格でない時点で「プロトコル」という言葉が嘘になる（`src/mt5/SwimmyBridge.mq5:491-498`）。

### Uncle Bob:
- テストが浅すぎる。`test_mcp_gateway.py` は200系の成否確認しかしておらず、契約テストになっていない（`tools/test_mcp_gateway.py:1-9`）。
- 認証や拒否動作、入力バリデーションに関する自動テストが存在しない。壊れても気付けない設計だ（`tools/mcp_stdio_server.py:55-71`）。

## 🚀 ビジョナリーの意見
### Ng:
- request_id の欠落で「遅延と欠損を学習できない」。性能評価が盲目になる（`src/lisp/core/message-dispatcher.lisp:115-123`）。
- MT5とLispのヒストリ不整合が既知課題なのに、契約テストがない（`docs/llm/STATE.md:24-26`）。

### López de Prado:
- 仕様ドリフトは統計汚染。`action/symbol` と `side/instrument` の混在は、観測データの混濁そのもの（`docs/llm/INTERFACES.md:104-114`、`src/lisp/core/execution-protocol.lisp:59-62`）。
- request_id 不在は検証バイアスを生む。結果の紐付けが曖昧な統計は信用不能（`src/lisp/core/message-dispatcher.lisp:115-123`）。

### Gene Kim:
- WSL IP の手動設定は運用事故の温床。復旧手順に依存する構成は“止まる前提”だ（`docs/llm/STATE.md:24-26`、`src/mt5/SwimmyBridge.mq5:16-22`）。
- MCPログには request_id が残るのに、backtest_status に残らない。運用監視が断絶している（`tools/mcp_stdio_server.py:113-147`、`src/lisp/core/message-dispatcher.lisp:115-123`）。

## 🚀 Musk's Decision (Final)
> 「ORDER_OPEN は厳格化で確定。フォールバックは削除し、`side/instrument` に一本化。backtest_status は last_request_id のみを持たせる。仕様書と実装の二重管理は今すぐ止める。契約テストを追加し、運用の“見える化”を優先する。」

## Actionable Items
1. `ORDER_OPEN` のフォールバックを削除し、`type` + `instrument/side` の厳格入力に統一する（`src/mt5/SwimmyBridge.mq5:470-524`）。
2. `docs/llm/INTERFACES.md` の `ORDER_OPEN` 定義を `instrument/side` に修正し、実装と一致させる（`docs/llm/INTERFACES.md:104-114`、`src/lisp/core/execution-protocol.lisp:43-67`）。
3. `backtest_status.txt` に `last_request_id` を追記し、MCPの request_id を追跡可能にする（`src/lisp/core/message-dispatcher.lisp:115-123`、`tools/mcp_gateway.py:37-42`）。
4. MCP JSON-RPC の契約テストを強化し、認証・拒否・正常系を自動化する（`tools/test_mcp_gateway.py:1-9`、`tools/mcp_stdio_server.py:55-71`）。
5. WSL IP の固定値を廃止し、起動時に必須設定チェックを入れる（`src/mt5/SwimmyBridge.mq5:16-22`、`docs/llm/STATE.md:24-26`）。

---

# 🦅 Expert Panel Report

**Date:** 2026-02-04
**Leader:** Elon Musk
**Trigger:** /expert-panel「S式版とJSON版の混在でなぜこうなったか」

## 🏛️ 常設顧問の意見
### Taleb:
- 文字列検索で `BUY/SELL` を拾って `ORDER_OPEN` に化ける設計は“事故の種”。フォーマットの揺れで誤発注が起きる（`src/mt5/SwimmyBridge.mq5:469-505`）。
- WSL IP が固定値で手動運用。可用性が単一点に集約されていて、障害時に復旧不能になる（`src/mt5/SwimmyBridge.mq5:16-22`、`docs/llm/STATE.md:24-26`）。

### Graham:
- `ORDER_OPEN` の仕様が `action/symbol`（ドキュメント）と `instrument/side`（実装）で分裂している。誰も正しい仕様を知らない状態（`docs/llm/INTERFACES.md:104-114`、`src/lisp/core/execution-protocol.lisp:43-67`）。
- “S式へ統一”と書きながら、MT5はJSONパーサで動いている。言葉とコードが噛み合ってない（`docs/llm/STATE.md:21-22`、`src/mt5/SwimmyBridge.mq5:82-113`）。

### Naval:
- `request_id` が backtest_status に残らないので、外部MCPと内部結果の自動連携が不可能（`src/lisp/core/message-dispatcher.lisp:102-123`）。
- MCP側は毎回新しい `request_id` を発行していて、外部と内部の関連付けが切れる（`tools/mcp_gateway.py:37-42`）。レバレッジを自分で捨てている。

### Simons:
- request_id 不在は統計上の“再現性破壊”。遅延・欠損・重複の計測ができず、検証が崩壊する（`src/lisp/core/message-dispatcher.lisp:102-123`）。
- 実装がJSON・S式で揺れるとバックテスト結果の解釈も揺れる。測定系の一貫性が失われる（`docs/llm/STATE.md:21-22`、`src/mt5/SwimmyBridge.mq5:82-113`）。

## 💻 技術パネルの意見
### Fowler:
- “境界の仕様”が曖昧。MCPはJSON-RPCで厳密なのに、MT5側は文字列検索でフォールバックする。境界設計が崩れている（`tools/mcp_stdio_server.py:55-81`、`src/mt5/SwimmyBridge.mq5:469-505`）。
- ドキュメントが唯一の正本になっていない。S式記述なのにJSON実装が走っている。設計の正本を明示せよ（`docs/llm/INTERFACES.md:104-114`、`src/mt5/SwimmyBridge.mq5:82-113`）。

### Hickey:
- MQL5 の手製JSONパーサは脆い。構造化データを文字列で拾う設計は事故を増やす（`src/mt5/SwimmyBridge.mq5:82-113`）。
- 「S式統一」の原則があるのに、MT5は別ルール。統一しないと全体のシンプルさが崩れる（`docs/llm/STATE.md:21-22`）。

### Uncle Bob:
- プロトコルの契約テストが薄い。`test_mcp_gateway.py` は成功/失敗の表層しか見ていない（`tools/test_mcp_gateway.py:1-9`）。
- 仕様逸脱を検知する自動テストが無く、ズレが“見えないバグ”として蓄積される（`docs/llm/INTERFACES.md:104-114`、`src/lisp/core/execution-protocol.lisp:43-67`）。

## 🚀 ビジョナリーの意見
### Ng:
- request_id が無いと遅延・欠損が学習に反映できない。評価指標が盲目になる（`src/lisp/core/message-dispatcher.lisp:102-123`）。

### López de Prado:
- JSON/S式の混在は計測バイアス。データ定義の揺れはバックテスト評価のブレを増幅させる（`docs/llm/STATE.md:21-22`、`src/mt5/SwimmyBridge.mq5:82-113`）。

### Gene Kim:
- “手動でWSL IP設定”が既知課題なのに放置。運用ミスの温床（`docs/llm/STATE.md:24-26`、`src/mt5/SwimmyBridge.mq5:16-22`）。
- ドキュメントと実装のズレは運用手順を無効化する。事故時の手順が信用できない（`docs/llm/INTERFACES.md:104-114`、`src/mt5/SwimmyBridge.mq5:82-113`）。

## 🚀 Musk's Decision (Final)
> 「“正本”を一つに固定する。MT5はプロトコルを厳格化し、ドキュメントとテストで固定する。request_id を end-to-end で追跡可能にし、外部と内部の整合を崩さない。二重仕様は今すぐ廃止。」

## Actionable Items
1. MT5プロトコルの正本を **JSON/S式のどちらかに固定**し、ドキュメントと実装を一致させる（`docs/llm/STATE.md:21-22`、`docs/llm/INTERFACES.md:104-114`、`src/mt5/SwimmyBridge.mq5:82-113`）。
2. `ORDER_OPEN` を厳格化し、フォールバック検索を廃止する（`src/mt5/SwimmyBridge.mq5:469-505`）。
3. `backtest_status.txt` に last_request_id を出力し、トレーサビリティを確保する（`src/lisp/core/message-dispatcher.lisp:102-123`）。
4. MCP側の `request_id` を外部IDと一致させ、エンドツーエンドで保存する（`tools/mcp_gateway.py:37-42`）。
5. 契約テストを追加し、仕様ズレをCIで止める（`tools/test_mcp_gateway.py:1-9`、`docs/llm/INTERFACES.md:104-114`）。

---

# 🦅 Expert Panel Report

**Date:** 2026-02-04
**Leader:** Elon Musk
**Trigger:** /expert-panel「S式でいいよね？」

## 🏛️ 常設顧問の意見
### Taleb:
- “S式でいい”と言うなら、JSONパーサが残っている限り事故は避けられない。誤パース＝誤指令の温床だ（`src/mt5/SwimmyBridge.mq5:82-113`、`src/mt5/SwimmyBridge.mq5:469-505`）。
- STATEに「内部ZMQはS式」と書きつつ、実コードがJSONなら“信頼の源泉”が壊れている（`docs/llm/STATE.md:21-22`、`src/mt5/SwimmyBridge.mq5:82-113`）。

### Graham:
- 正本がない。ドキュメントはS式、実装はJSON。この状態で“どっちでいい？”は設計放棄だ（`docs/llm/INTERFACES.md:104-114`、`src/mt5/SwimmyBridge.mq5:82-113`）。
- まず“正本”を決めて削るべき。S式にするならJSON系コードを消す、JSONにするならS式記述を消す。混在は無駄の極み（`docs/llm/INTERFACES.md:104-114`）。

### Naval:
- S式はLispの強み。内部ZMQをS式に寄せるのは合理的だが、**外部境界(JSON-RPC)との変換点**を明示しないとレバレッジが死ぬ（`doc/SYSTEM_ARCHITECTURE.md:12-18`、`tools/mcp_stdio_server.py:55-81`）。
- request_idの追跡が切れているなら、どのフォーマットを選んでも自動化は不完全（`tools/mcp_gateway.py:37-42`、`src/lisp/core/message-dispatcher.lisp:102-123`）。

### Simons:
- フォーマット混在は計測ノイズ。S式で統一するなら、**統計に入る前に必ず統一**しろ（`docs/llm/STATE.md:21-22`）。
- 結果の再現性を守るには、request_idをエンドツーエンドで残すのが最低条件（`src/lisp/core/message-dispatcher.lisp:102-123`）。

## 💻 技術パネルの意見
### Fowler:
- 仕様と実装の乖離が最大の設計欠陥。S式に寄せるなら、`execution-protocol.lisp` のJSON生成をS式化するか、Guardianに翻訳責務を明示しろ（`src/lisp/core/execution-protocol.lisp:31-67`、`doc/SYSTEM_ARCHITECTURE.md:12-18`）。
- MT5側のパーサがJSON前提で残る限り、S式正本は“口だけ”になる（`src/mt5/SwimmyBridge.mq5:82-113`）。

### Hickey:
- Lisp的にはS式が正しい。だが一貫性が無い限り“シンプル”は成立しない（`docs/llm/STATE.md:21-22`）。
- 手製JSONパーサは脆い。S式統一ならJSONパーサを削除し、S式のみの厳格パースにすべき（`src/mt5/SwimmyBridge.mq5:82-113`）。

### Uncle Bob:
- 仕様違反を検知するテストがない。S式を正とするなら、契約テストで“JSONが来たら拒否”まで自動化すべき（`tools/test_mcp_gateway.py:1-9`、`docs/llm/INTERFACES.md:104-114`）。

## 🚀 ビジョナリーの意見
### Ng:
- フォーマットの揺れは欠損を生む。学習系の品質が落ちる。S式に統一するなら“境界での正規化”を必ず定義しろ（`docs/llm/STATE.md:21-22`）。

### López de Prado:
- データ定義の揺れは統計的バイアス。S式に固定するなら、履歴・トレード・バックテストの全ルートを統一せよ（`docs/llm/INTERFACES.md:104-114`）。

### Gene Kim:
- 運用では“正本が一つ”が最優先。S式にするなら runbook/owners guide に明文化し、オンボーディングで迷わせるな（`doc/owners_guide.md:47-54`、`doc/SYSTEM_ARCHITECTURE.md:12-18`）。

## 🚀 Musk's Decision (Final)
> 「S式で行く。ただし“内部ZMQはS式、外部APIはJSON”を明確に線引きする。JSONパーサはMT5から排除し、実装・ドキュメント・テストをS式で固定する。request_idの追跡も同時に直す。」

## Actionable Items
1. **S式を正本に固定**し、MT5のJSONパーサを撤去（または無効化）してS式のみ受け付ける（`src/mt5/SwimmyBridge.mq5:82-113`）。
2. Lisp→Guardianの送信経路でS式を保証する（必要なら `execution-protocol.lisp` をS式化、もしくはGuardian側で明示変換）（`src/lisp/core/execution-protocol.lisp:31-67`、`doc/SYSTEM_ARCHITECTURE.md:12-18`）。
3. `ORDER_OPEN` のキーを `instrument`/`side` に統一し、ドキュメントと一致させる（`docs/llm/INTERFACES.md:104-114`、`src/lisp/core/execution-protocol.lisp:43-67`）。
4. request_id を last_request_id として追跡できるよう status 出力を拡張する（`src/lisp/core/message-dispatcher.lisp:102-123`）。
5. 契約テストで“JSON混入を検知して失敗”するガードを追加する（`tools/test_mcp_gateway.py:1-9`）。
