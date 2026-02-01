# 🦅 Expert Panel Report

**Date:** 2026-02-01
**Leader:** Elon Musk
**Trigger:** /expert-panel「lisp統一でいいよね？json入れたら鈍足化するよね？なんか良いことあるの？」

## 🏛️ 常設顧問の意見
### Taleb: “読む＝実行”の破滅面がむき出し
- 外部入力を `read-from-string` で直接読む。`*read-eval*` 無効化も無し。これは即死級の入力面。`src/lisp/core/message-dispatcher.lisp:102-108`
- LLM出力の `entry/exit` を `read-from-string` で評価。毒入り出力が即実行される構造。`src/lisp/school/school-evolution.lisp:337-349`

### Graham: プロトコル混在で“実行の真実”が揺らぐ
- S式とJSONで処理が分岐していて、V2ハンドラはS式経路にしか無い。プロトコル次第で挙動が変わる時点でプロダクトが壊れてる。`src/lisp/core/message-dispatcher.lisp:165-167,265-317`
- 仕様より実装が先行し、説明（Lisp統一）と実態（JSON処理実装）がズレる。`src/lisp/core/message-dispatcher.lisp:219-317`

### Naval: 自動化が“脆い自動化”になっている
- LLM応答の解析をPython subprocessに依存。遅延・失敗時の復旧設計が見えない。`src/lisp/school/school-evolution.lisp:309-333`
- `parse-json-safely` が空実装。安全策の“設計だけ”が放置されている。`src/lisp/school/school-evolution.lisp:299-301`

### Jim Simons: 検証パイプラインが数学的に信用できない
- Backtest V2のpayloadが`strategy-json`のまま。コメントで誤りを自認しているのに未修正。統計が嘘になる。`src/lisp/school/school-backtest-v2.lisp:41-52`
- Phase2昇格ロジックが未実装。OOS検証は“口だけ”。`src/lisp/school/school-backtest-v2.lisp:133-140`

## 💻 技術パネルの意見
### Fowler: 1関数に全て詰め込み過ぎ
- `internal-process-msg` がパース・分岐・副作用全部持ち。S式/JSONの二重実装で変更が壊れやすい。`src/lisp/core/message-dispatcher.lisp:102-318`
- Backtest V2はコメント内TODOが残り、設計意図がコードで保証されていない。`src/lisp/school/school-backtest-v2.lisp:41-52`

### Hickey: Lisp統一は良いが“安全な読み”が前提
- Lisp統一自体は悪くない。しかし `read-from-string` の無制限入力はシンプルの敵。安全なリーダかホワイトリストで境界を守れ。`src/lisp/core/message-dispatcher.lisp:102-108`
- 既にJSON経路が存在する時点で“統一されていない”。不要なら削除、使うなら規約化。`src/lisp/core/message-dispatcher.lisp:219-317`

### Uncle Bob: テスト不足が致命傷を放置
- 監視レポートで `re` 未import。実運用で即落ちる。テストがあれば一発で見つかる。`tools/report_system_status.py:61-70`
- `parse-json-safely` が未実装のまま。安全策がテストも仕様も無い。`src/lisp/school/school-evolution.lisp:299-301`

## 🚀 ビジョナリーの意見
### Ng: LLMパイプラインが“安全性ゼロ”で運用されている
- LLM出力→Python→`read-from-string` という多段変換で、検証もスキーマも無い。攻撃・誤出力に弱すぎる。`src/lisp/school/school-evolution.lisp:309-349`

### López de Prado: OOSを名乗るが実装が無い
- Phase2が未実装の時点で過学習対策は幻想。`src/lisp/school/school-backtest-v2.lisp:133-140`
- JSON経路にはV2ハンドラが無い。検証がプロトコル依存で崩れる。`src/lisp/core/message-dispatcher.lisp:165-167,265-317`

### Gene Kim: 監視が壊れてるなら運用も壊れてる
- `report_system_status.py` が `re` 未importで即死する。監視不能は運用不能。`tools/report_system_status.py:61-70`

## 🚀 Musk's Decision (Final)
> 「Lisp統一は“安全に読む”なら許可する。JSONは必須ではないが、いまの混在は最悪だ。速度の議論は計測してからやれ。まず“安全な入力”と“同一挙動”を保証しろ。」

## Actionable Items
1. 外部入力のS式パースを安全化（`*read-eval*`無効化＋ホワイトリストリーダ＋スキーマ検証）。`src/lisp/core/message-dispatcher.lisp`, `src/lisp/school/school-evolution.lisp`
2. S式/JSONの処理を正規化（共通の内部表現に寄せ、V2ハンドラの挙動差を排除）。`src/lisp/core/message-dispatcher.lisp`
3. Backtest V2のpayload修正とPhase2昇格ロジック実装。`src/lisp/school/school-backtest-v2.lisp`
4. LLM応答の安全デコードを実装し、`parse-json-safely` を実装＋テスト追加。`src/lisp/school/school-evolution.lisp`
5. 監視スクリプトの即時修正（`import re`）＋実行テスト追加。`tools/report_system_status.py`

---

# 🦅 Expert Panel Report

**Date:** 2026-02-01  
**Leader:** Elon Musk  
**Trigger:** 「V3.0の61戦略はレジェンドとして再登録したが、交配に使う価値は本当にあるか？S式統一後の運用で問題はないか？」

## 🏛️ 常設顧問の意見
### Taleb:
- Sharpe/ProfitFactor の下限を設けず 59 本を一括 LEGEND 登録。復元直後に全てが「安全資産」とみなされ、リスクゲートが空振りする。`tools/restore_legend_61.lisp`, `src/lisp/strategies/legend-61.lisp`
- アーカイブ基準が S<0.1 / PF<1.0 と緩い。レバレッジを誤ると一撃死の温床。`src/lisp/strategies/legend-61.lisp:67-90`

### Graham:
- 復元がスクリプト手動前提で、自動起動パスに載っていない。再起動後にレジェンドが空のままでも気付けない。`tools/restore_legend_61.lisp`
- DBとファイルの二重管理で真実が揺れる。`persistence.lisp` は LEGEND-ARCHIVE をロードしないため、監視と実態が乖離。`src/lisp/core/persistence.lisp`

### Naval:
- 61戦略の再検証キューが手動。バックテスト結果を待たずに breeding へ流れる自動化は“脆い自動化”。`src/lisp/strategies/legend-61.lisp:92-105`
- quicklisp 依存がハードコードされ、CI/コンテナで落ちる設計。`tools/restore_legend_61.lisp`

### Jim Simons:
- 相関フィルタ 0.35 だけでは多様性が担保されず、EMA系が山ほど残っている。統計的に冗長。`src/lisp/school/school-breeder.lisp:128-175`
- 61本のうち2本のみ重複判定で落ち、残りの分布検証(OOS/CPCV)が皆無。数学的根拠が弱い。

## 💻 技術パネルの意見
### Fowler:
- `school-breeder.lisp` がプール制御・相関チェック・Discord通知まで抱え込み、凝集度が低い。テストが1本だけ。`src/lisp/school/school-breeder.lisp`, `tools/test_legend_protection.lisp`
- ディレクトリ名が rank とズレていたバグ修正は良いが、読み書きの責務を persistence に集約すべき。`src/lisp/core/persistence.lisp`

### Hickey:
- グローバル状態 (`*strategy-knowledge-base*`, `*category-pools*`) に強く依存。純粋関数として再利用できず、REPL検証がしにくい。`src/lisp/school/school-breeder.lisp`
- LEGEND アーカイブをロードしない設計はデータ不変性の前提を壊す。読み取りも書き込みも同じ境界で扱うべき。

### Uncle Bob:
- `tools/test_legend_protection.lisp` は墓場送りブロックしか検証せず、restore/archiving/queue の回帰テストが無い。テストピラミッドの最下段が欠落。  
- Quicklispローダのエラーハンドリング無しで、失敗時に exit code 1 を返さない箇所が残る。`tools/restore_legend_61.lisp`

## 🚀 ビジョナリーの意見
### Ng:
- 61戦略は古典的テクニカルのみで ML シグナルが無い。LLM/Guardian とのハイブリッド評価も未実装。`src/lisp/strategies/legend-61.lisp`
- 再検証キューが ML 評価ループに接続されず、学習データに貢献しない。

### López de Prado:
- CPCV/OOS を通していない“バニラ戦略”を LEGEND に置くのはデータマイニングバイアス。ランキングに反映する前に検証パイプを必須化すべき。`src/lisp/strategies/legend-61.lisp:92-105`
- 相関 0.35 は同系統クラスタを量産する。クラスタ単位のターンオーバーを導入せよ。`src/lisp/school/school-breeder.lisp`

### Gene Kim:
- ダッシュボードは LEGEND_ARCHIVE を 0 件表示のまま。監視と実態が乖離し、運用判断を誤る。`tools/dashboard.py`
- 復元・バックテスト・アーカイブが手作業で、runbook化されていない。SRE視点で危険。

## 🚀 Musk's Decision (Final)
> 「61本は“種ライブラリ”として残す。ただし全件を冷凍保存扱いにして、再検証が終わるまで breeding には入れない。DBを真実にし、ダッシュボードが同じ数を示すまで出荷禁止。スクリプトは自動起動とCIテストを付けろ。」

## Actionable Items
1. 復元直後に `queue-legend-revalidation` を自動実行し、完了まで breeding から除外するフラグを導入。`src/lisp/strategies/legend-61.lisp`, `src/lisp/school/school-breeder.lisp`
2. LEGEND-ARCHIVE を persistence と dashboard の双方で読み取れるよう統一し、DB/ファイルの単一ソースを決める。`src/lisp/core/persistence.lisp`, `tools/dashboard.py`
3. CIで `tools/test_legend_protection.lisp` を実行し、restore/archiving/queue をカバーする追加テストを作成。`tools/test_legend_protection.lisp`
4. Quicklisp依存を明示し、失敗時は exit code を返すラッパを追加。`tools/restore_legend_61.lisp`

---

# 🦅 Expert Panel Report

**Date:** 2026-02-01  
**Leader:** Elon Musk  
**Trigger:** 「systemd運用レベルを user → system に統一すべきか？（B案）」

## 🏛️ 常設顧問の意見
### Taleb: “二重起動は即死級の運用リスク”
- `systemd/swimmy.service` に `User=` / `Group=` が無く、systemレベルで入れると root で動く。`%h` も root の home になり、誤動作と権限事故の温床。`systemd/swimmy.service:7-16`
- 公式Runbookが `systemctl --user` を要求する一方で、正義は「systemd 4サービス」と記載。二重起動が起きる設計は“破滅の入口”。`doc/owners_guide.md:182-205`, `docs/llm/ARCHITECTURE.md:74-76`, `docs/llm/STATE.md:15`

### Graham: “仕様と実装の分裂は組織を殺す”
- SPECでは4サービス運用だが、リポジトリには monolith 用 `swimmy.service` が残存。どっちが正義か不明な時点でスケール不能。`docs/llm/SPEC.md:55-58`, `systemd/swimmy.service:8-9`, `run.sh:29-31`
- `doc/SYSTEM_ARCHITECTURE.md` が V3.0/2025-12-29 のまま。現行V50.5の運用議論に使えない。`doc/SYSTEM_ARCHITECTURE.md:1-4`

### Naval: “自動化が再現できないなら自動化じゃない”
- Runbookは `--user` 前提なのに、systemレベル移行に必要なユニットの正本が repo に揃っていない。手作業運用が固定化される。`doc/owners_guide.md:182-196`, `systemd/swimmy.service`, `systemd/swimmy-data-keeper.service`

### Jim Simons: “ポート設計とサービス境界の整合が無い”
- ポートが固定設計なのに、systemd側で相互排他や依存関係が定義されていない。二重起動は統計的検証を壊す。`docs/llm/INTERFACES.md:6-14`, `systemd/swimmy.service:1-12`, `systemd/swimmy-data-keeper.service:1-10`

## 💻 技術パネルの意見
### Fowler: “境界が曖昧で設計負債が増殖”
- アーキテクチャは4サービスだが、`swimmy.service` は `run.sh` で monolith を起動する。境界違反が運用事故を生む。`docs/llm/ARCHITECTURE.md:46-76`, `systemd/swimmy.service:8-9`, `run.sh:29-31`

### Hickey: “複雑性を自分で増やしている”
- `--user` と system の二重運用は“余計な複雑性”。どちらかを消すだけで設計は半分シンプルになる。`doc/owners_guide.md:182-205`, `docs/llm/SPEC.md:55-58`

### Uncle Bob: “安全でなくテストも無い”
- systemdユニットが root 起動前提に見えるのは危険。最低限 `User=swimmy` を明示し、HOME解決を保証すべき。`systemd/swimmy.service:7-16`, `systemd/swimmy-data-keeper.service:5-15`

## 🚀 ビジョナリーの意見
### Ng: “データが落ちたら学習は死ぬ”
- Data Keeperは学習データの生命線。運用レベルの混乱で止まるのは致命傷。`docs/llm/ARCHITECTURE.md:53-56`, `doc/owners_guide.md:182-196`

### López de Prado: “二重起動はデータ汚染”
- 同一ポートの多重バインド失敗は「止まる」だけでなく、復旧時のデータ抜け・重複を引き起こす。検証統計が破綻する。`docs/llm/INTERFACES.md:6-14`, `doc/owners_guide.md:182-205`

### Gene Kim: “Runbookの分裂は運用不能”
- 仕様(4サービス)とRunbook(--user)の不一致はSRE的にアウト。事故の再現性が無く、復旧できない。`docs/llm/SPEC.md:55-58`, `doc/owners_guide.md:182-205`

## 🚀 Musk's Decision (Final)
> 「systemレベル統一は“正しい”が、今のまま移行するのは間違いだ。root起動・runbook分裂・monolith残存を全部潰してからやれ。4サービスを正本にし、swimmy.serviceは引退させる。」

## Actionable Items
1. systemレベルの正本ユニットを repo に揃える（`swimmy-brain`, `swimmy-guardian`, `swimmy-school`, `swimmy-data-keeper`）。`systemd/`
2. systemdユニットに `User=swimmy` / `Group=swimmy` を明示し、`WorkingDirectory=/home/swimmy/swimmy` を固定。`systemd/swimmy.service`, `systemd/swimmy-data-keeper.service`
3. monolith `swimmy.service` を廃止/封印（`Conflicts=` で4サービスと排他、または削除）。`systemd/swimmy.service`
4. Runbookを systemd (system) に一本化し、`--user` 記述を除去。`doc/owners_guide.md`, `docs/owners_guide.md`
5. 仕様ドキュメント側にも「systemレベル統一」を明記。`docs/llm/SPEC.md`, `docs/llm/ARCHITECTURE.md`, `docs/llm/STATE.md`

---

# 🦅 Expert Panel Report

**Date:** 2026-02-01  
**Leader:** Elon Musk  
**Trigger:** /expert-panel「設計.txtのライフサイクルがDiscordで0結果のまま。再構築すべきか？」

## 🏛️ 常設顧問の意見
### Taleb: “失敗を成功として数える時点で破滅”
- Backtest ServiceがGuardian欠損時にSharpe=0の「成功風」結果を返し、集計はそれを成果として扱う。静かな破滅。`tools/backtest_service.py:372-412`, `src/lisp/core/discord.lisp:195-207`
- 新規戦略はrankがNULLにならずPhase1評価が走らない。失敗が見えずに蓄積する設計。`src/lisp/dsl.lisp:190-199`, `src/lisp/school/school-kb.lisp:231-249`, `src/lisp/school/school-rank-system.lisp:451-469`

### Graham: “仕様と実装が別物”
- 設計は3通貨ペアで最良選択だが、実装は戦略の既存symbolでしか回さない。プロダクトの真実が曖昧。`doc/設計.txt:4-5`, `src/lisp/strategies/strategies.lisp:118-123`
- 設計はBランク100で淘汰開始だが、実装は10で開始。ユーザー期待と挙動がズレる。`doc/設計.txt:7`, `src/lisp/school/school-rank-system.lisp:27`

### Naval: “自動化が“自動化”に見えるだけ”
- S式経路だけがV2ハンドラを通り、JSON経路は素通り。輸送経路で結果が変わる自動化はレバレッジじゃない。`src/lisp/core/message-dispatcher.lisp:165-167`, `src/lisp/core/message-dispatcher.lisp:265-317`
- rankスロットが:incubator/:scoutで埋まり、評価の条件「rank=nil」が二度と満たされない。自動評価が永久停止。`src/lisp/dsl.lisp:190-199`, `src/lisp/school/school-breeder.lisp:103-120`, `src/lisp/school/school-rank-system.lisp:451-469`

### Jim Simons: “検証パイプラインが数学的に破綻”
- Phase2検証が未実装。OOS/CPCVが「あるふり」だけで統計が嘘。`src/lisp/school/school-backtest-v2.lisp:128-136`
- Backtest結果のキー欠損を0で埋めるため、0.00 Sharpe/0 tradesが“正常”として流れる。統計的に無意味。`src/lisp/core/message-dispatcher.lisp:122-139`, `src/lisp/core/message-dispatcher.lisp:269-286`

## 💻 技術パネルの意見
### Fowler: “同じ機能が2つ、しかも挙動が違う”
- `internal-process-msg`がS式/JSONで二重実装。V2の結果処理が片側だけ。拡張不能な分岐肥大。`src/lisp/core/message-dispatcher.lisp:102-218`, `src/lisp/core/message-dispatcher.lisp:219-317`
- `run-rank-evaluation`が「新規評価」をうたうのに、実際は`apply-backtest-result`の副作用に依存。設計と実行がズレる。`src/lisp/school/school-rank-system.lisp:323-335`, `src/lisp/school/school-rank-system.lisp:451-469`

### Hickey: “rankとtierが同じ箱に入っている”
- rankスロットに:incubator/:scoutを入れており、B/A/Sの意味が壊れている。状態モデルが破綻。`src/lisp/dsl.lisp:155-168`, `src/lisp/school/school-breeder.lisp:117-119`, `src/lisp/school/school-kb.lisp:237-249`
- timeframesの真実が2つある。`*timeframes*`にMNがあるのに、culling側は無視。シンプルさの敵。`src/lisp/school/school-constants.lisp:7-9`, `src/lisp/school/school-rank-system.lisp:219-223`

### Uncle Bob: “TODOが残っている時点で未完成”
- `get-screening-range`がコメント付きの未修正、Phase2ロジックは空。テスト以前に未完成。`src/lisp/school/school-backtest-v2.lisp:70-74`, `src/lisp/school/school-backtest-v2.lisp:128-136`
- Sharpe=0を検知してもログだけ。異常に対してアサーションもフェイルも無い。`src/lisp/school/school-rank-system.lisp:462-466`

## 🚀 ビジョナリーの意見
### Ng: “学習の入力が設計と一致しない”
- 設計は3通貨ペアで最良選択だが、実装は単一symbolで固定。学習フィードバックが設計とズレる。`doc/設計.txt:4-5`, `src/lisp/strategies/strategies.lisp:118-123`

### López de Prado: “OOS/CPCVが機能していない”
- Phase2未実装＋rankがB/A/Sに乗らないため、OOS/CPCVの入口すら開かない。過学習対策が死んでいる。`src/lisp/school/school-backtest-v2.lisp:128-136`, `src/lisp/school/school-rank-system.lisp:451-469`
- 月足がculling対象外で選抜バイアスが発生。`src/lisp/school/school-constants.lisp:7-9`, `src/lisp/school/school-rank-system.lisp:219-223`

### Gene Kim: “監視が嘘なら運用も嘘”
- DiscordのRank Distributionは`strategy-rank`依存なのに、rankが更新されない設計。レポートが真実を反映しない。`src/lisp/core/discord.lisp:199-207`, `src/lisp/school/school-rank-system.lisp:451-469`
- Backtestエラーが0.00として流れ、アラートに昇格しない。可観測性が壊れている。`tools/backtest_service.py:372-412`, `src/lisp/core/message-dispatcher.lisp:287-292`

## 🚀 Musk's Decision (Final)
> 「再構築は“最後の手段”だ。今はやるな。  
>  まずrank/tier混同とV2の欠落を直せ。Backtest結果の経路差異を消せ。  
>  その上で“1本の戦略が B→A→S に到達する”ゴールデンパスを1日で証明しろ。  
>  それでも止まるなら、最小構成で再構築する。」

## Actionable Items
1. `rank`と`tier`を分離し、`rank`初期値を`nil`に統一。`defstrategy`/breeder/add-to-kbの`:incubator`/`:scout`投入を`tier`側へ移管。`src/lisp/dsl.lisp`, `src/lisp/school/school-breeder.lisp`, `src/lisp/school/school-kb.lisp`
2. `apply-backtest-result`が`rank`の状態に依存せずPhase1評価を発火できるよう条件を明示化（`rank=nil`/`tier=:incubator`等）。`src/lisp/school/school-rank-system.lisp`
3. V2 Phase2を実装（A/S昇格 or OOS/CPCVへ接続）し、`get-screening-range`のTODOを解消。`src/lisp/school/school-backtest-v2.lisp`
4. BACKTEST_RESULT処理をS式/JSONで一本化し、V2ハンドラを両経路で必ず呼ぶ。`src/lisp/core/message-dispatcher.lisp`
5. Backtestエラーは“失敗”としてアラート化・集計除外する（Sharpe=0を成功扱いしない）。`tools/backtest_service.py`, `src/lisp/core/discord.lisp`
6. 3通貨ペアでの最良選択フローを実装し、戦略のsymbolを上書きする。`doc/設計.txt`, `src/lisp/strategies/strategies.lisp`
7. timeframesの単一ソース化（MNをculling/評価対象に含める）とBランク閾値の整合。`src/lisp/school/school-constants.lisp`, `src/lisp/school/school-rank-system.lisp`

# 🦅 Expert Panel Report

**Date:** 2026-02-01
**Leader:** Elon Musk
**Trigger:** /expert-panel「ベンチ廃止の是非と現状レビュー」

## 🏛️ 常設顧問の意見
### Taleb: “読む＝実行”と“状態改変”が無防備
- 外部入力を `read-from-string` で直接評価している。`*read-eval*`の無効化も無い。破滅面が裸。`src/lisp/core/message-dispatcher.lisp:102-108`
- LLM出力の `entry/exit` をそのまま `read-from-string` で評価。毒入り出力の実行経路が完成している。`src/lisp/school/school-evolution.lisp:337-349`
- ベンチ/キルは状態を書き換えるだけで監査ログが無い。破滅検知（いつ・なぜ死んだか）の因果が追えない。`src/lisp/strategies/strategies.lisp:268-294` / `src/lisp/school/school-lifecycle.lisp:49-63`

### Graham: “真実の状態”が二重化されている
- 起動バナーは Ver 41.5、オーナーズガイドは V50.5。プロダクトの真実が崩壊。`src/lisp/main.lisp:136-140` / `doc/owners_guide.md:1`
- アーキテクチャ文書が 2025-12-29 更新のまま。2026-01-28の変更が反映されず、運用判断がズレる。`doc/SYSTEM_ARCHITECTURE.md:3`

### Naval: 自動化が“脆い自動化”でレバレッジを殺す
- systemdインストールが `cp` 失敗を黙殺。失敗しても“成功ログ”が出る自動化は嘘。`tools/install_services.sh:7`
- 起動時に全シンボル×全TFの履歴ロード。冷起動コストが線形に増え、レバレッジが消える。`src/lisp/main.lisp:87-113`

### Jim Simons: 検証パイプラインが数学的に破綻
- Backtest V2のpayloadが `strategy-json` のまま（コメントで誤り宣言）。入力が壊れていれば統計は無意味。`src/lisp/school/school-backtest-v2.lisp:41-52`
- Phase2（OOS）昇格ロジックが空。検証フェーズが“存在しない”。`src/lisp/school/school-backtest-v2.lisp:133-140`

## 💻 技術パネルの意見
### Fowler: God Function化が進行
- `initialize-system` がIO/DB/通知/学習準備を全部抱え、`load-hall-of-fame` が二重呼び出し。責務が崩壊。`src/lisp/main.lisp:70-123`
- `internal-process-msg` がパース〜副作用まで抱え込み、拡張不能な巨大関数。`src/lisp/core/message-dispatcher.lisp:102-140`

### Hickey: “設定”と“状態”が混在している
- `config.lisp` が本来の設定だけでなく、ランタイム状態（candle/arms/portfolioなど）を持ち込んでいる。シンプルさが崩れる。`src/lisp/core/config.lisp:182-207`
- 同一状態が複数箇所に定義され、真実のソースが不明（例: `*benched-arms*`）。`src/lisp/core/config.lisp:258` / `src/lisp/core/globals.lisp:126`

### Uncle Bob: テスト不在で“簡単なバグ”が残る
- 監視レポートが `re` 未importで落ちる。テストがあれば防げる初歩。`tools/report_system_status.py:61-70`
- `get-screening-range` がコメント付きで未整理。設計意図がコードに反映されていない。`src/lisp/school/school-backtest-v2.lisp:74-77`

## 🚀 ビジョナリーの意見
### Ng: LLMパイプラインの安全性がゼロ
- LLM出力→Python→`read-from-string` の多段変換で、スキーマ検証もサニタイズも無し。安全性が欠落。`src/lisp/school/school-evolution.lisp:303-349`

### López de Prado: OOS検証が“言葉だけ”
- Phase2の昇格ロジックが未実装。OOSを名乗っても統計的な意味が無い。`src/lisp/school/school-backtest-v2.lisp:133-140`

### Gene Kim: 監視とデプロイが信用できない
- systemdインストールが fail-fast ではなく、デプロイ成功を偽装する。`tools/install_services.sh:7-18`
- 監視スクリプトが依存不足でクラッシュする。可観測性が嘘になる。`tools/report_system_status.py:61-70`

## 🚀 Musk's Decision (Final)
> 「“入力安全・検証整合・運用の正直さ”が揃うまで拡張は止める。ベンチを廃止するなら、二重系統の状態機械を全部潰せ。やるべきは“安全な読み取り”“正しい検証”“壊れない自動化”だ。」

## Actionable Items
1. `read-from-string`経路を封鎖（`*read-eval*`無効化＋安全リーダ＋スキーマ検証）: `src/lisp/core/message-dispatcher.lisp`, `src/lisp/school/school-evolution.lisp` ほか。
2. Backtest V2のpayload修正（`strategy-alist`統一）＋Phase2昇格ロジック実装＋テスト追加。`src/lisp/school/school-backtest-v2.lisp`。
3. systemdインストールをfail-fast化（コピー失敗で即停止、ユニット存在検証追加）。`tools/install_services.sh`。
4. 監視レポートの修正（`import re`）＋スモークテスト追加。`tools/report_system_status.py`。
5. 起動シーケンスの責務分割と重複削除（`load-hall-of-fame`二重呼び出し排除）。`src/lisp/main.lisp`。
6. 設定と状態の分離（`config`からランタイム状態を排除、`globals`に一本化）。`src/lisp/core/config.lisp`, `src/lisp/core/globals.lisp`。
7. ベンチ廃止の系統整理（戦略ベンチ・アームベンチを削除、評価/実行/通知の分岐を更新）。`src/lisp/strategies/strategies.lisp`, `src/lisp/school/school-lifecycle.lisp`, `src/lisp/engine/positions.lisp`, `src/lisp/engine/heartbeat.lisp`。

# 🦅 Expert Panel Report

**Date:** 2026-02-01
**Leader:** Elon Musk
**Trigger:** /expert-panel「ベンチ廃止の判断（置き換え方針が決められない）」

## 🏛️ 常設顧問の意見
### Taleb: “保険を外す”なら代替の破滅回避が必須
- 連敗ベンチ（短期のノイズ遮断）を削るなら、代わりに**リスクの硬いガード**が必要。現状はベンチが唯一の即時遮断になっている。`src/lisp/school/school-lifecycle.lisp:19-63`
- ストラ評価でのベンチ/キルが曖昧。廃止するなら“即墓場”など明確な死のルールに統一しろ。`src/lisp/strategies/strategies.lisp:268-294`

### Graham: “真実のルール”が二重化されている
- ベンチが「戦略ステータス」と「アーム除外」で2系統。どっちが真実か分からない。廃止方針は**二重系統の排除**に直結させよ。`src/lisp/strategies/strategies.lisp:257-294` / `src/lisp/engine/positions.lisp:66-124`

### Naval: 自動化のコストを最小化する決断を
- 実行アームのベンチは運用判断の温床。Thompson Samplingで自然に弱者が落ちるなら**ベンチは不要**。`src/lisp/engine/portfolio.lisp:48-70` / `src/lisp/engine/positions.lisp:107-112`

### Jim Simons: 検証と実行の“淘汰規則”を分離しろ
- 評価フェーズでベンチするのは曖昧。統計的に悪いものは**除外（墓場/kill）**すべき。実行フェーズの一時停止とは分けて考えろ。`src/lisp/strategies/strategies.lisp:460-507`

## 💻 技術パネルの意見
### Fowler: ベンチは分岐を増やすだけ
- `collect-strategy-signals` に「benched なら無視」という分岐が散在。廃止するなら**分岐ごと削除**して可読性を戻せ。`src/lisp/school/school-evaluation.lisp:223-229`

### Hickey: 状態の重複が設計負債
- `*benched-arms*` が `globals` と `config` に重複定義。廃止は**真実のソース整理のチャンス**。`src/lisp/core/config.lisp:256-259` / `src/lisp/core/globals.lisp:125-128`

### Uncle Bob: 期待動作のテストが無い
- ベンチ廃止後の期待動作（除外/継続/kill）をテストで固定しないと、回帰で復活する。`src/lisp/engine/positions.lisp:107-124` / `src/lisp/strategies/strategies.lisp:281-294`

## 🚀 ビジョナリーの意見
### Ng: “ノイズ除去”は学習ではなく実行層で
- 連敗遮断は本来リスク管理。学習層のベンチより、実行層のリスク制御に寄せるほうが筋。`src/lisp/engine/positions.lisp:107-124`

### López de Prado: 淘汰ルールは統計で決めろ
- Backtest評価は統計の場。ベンチは曖昧な棚上げになる。**基準未満は墓場送り**が整合的。`src/lisp/strategies/strategies.lisp:460-507`

### Gene Kim: 監視で追える状態に
- Heartbeatにベンチ数を出すなら、廃止後は削除して**真実だけ表示**に統一せよ。`src/lisp/engine/heartbeat.lisp:40-69`

## 🚀 Musk's Decision (Final)
> 「ベンチは廃止する。だが“何もしない”は却下だ。**評価フェーズは墓場/killで即時淘汰**、実行フェーズは**ベンチ無しでThompson Samplingとリスク管理に委ねる**。分岐は消し、真実のソースを一本化しろ。」

## Actionable Items
1. 評価フェーズのベンチを“即墓場/kill”に置換（Sharpe/PF/WR条件のベンチ→墓場）。`src/lisp/strategies/strategies.lisp`
2. 実行フェーズのベンチ除外を削除（`arm-benched-p`/`bench-arm`/`*benched-arms*`廃止）。`src/lisp/engine/positions.lisp`, `src/lisp/engine/portfolio.lisp`, `src/lisp/core/config.lisp`, `src/lisp/core/globals.lisp`
3. シグナル評価のbenched分岐削除。`src/lisp/school/school-evaluation.lisp`
4. Heartbeatのbenched表示削除。`src/lisp/engine/heartbeat.lisp`
5. 廃止後の期待動作をテストで固定（評価の淘汰/実行の継続）。`tests/` or `src/lisp/tests.lisp`
