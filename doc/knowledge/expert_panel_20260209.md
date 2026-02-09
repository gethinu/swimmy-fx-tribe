# 🦅 Expert Panel Report (Critique)

**Date:** 2026-02-09
**Leader:** Elon Musk
**Mode:** critique
**Trigger:** 「[$expert-panel](/home/swimmy/.codex/skills/expert-panel/SKILL.md)意見ください。」

## 🏛️ 常設顧問の意見
### Taleb:
「Swarm合意は**値が存在していないのに安全弁に使われている**。これは“計測不能リスク”だ。ゼロや初期値でゲートが開閉されるなら安全装置ではなく**破綻装置**だ。」
- 根拠: High Council が `*last-swarm-consensus*` を参照（`src/lisp/school/school-voting.lisp:310-333`）。
- 根拠: 実運用の Swarm 計算が外されている（`src/lisp/school/school-execution.lisp:350-353`）。
- 根拠: 初期値が設定済み（`src/lisp/core/globals.lisp:70`）。

### Graham:
「Swarmは“実装している体裁”だけが残っている。**設計が商品になっていない**。これではPanelの意見が“飾り”になる。」
- 根拠: Swarmの投票/集計ロジックは存在するが、運用経路に接続されていない（`src/lisp/school/school-voting.lisp:64-187` / `src/lisp/school/school-execution.lisp:334-386`）。
- 根拠: 評価側に Swarm 로ジック削除のコメント（`src/lisp/school/school-evaluation.lisp:69-72`）。

### Naval:
「実用性は“テストに落ちる指摘かどうか”で決まる。**現状はテストで守られていない**。」
- 根拠: Swarm合意の更新経路に対するテストが無い（`src/lisp/tests.lisp:1195-1212` は値のセットだけ）。

### Simons:
「合意値は**推定量として成立していない**。計算はあるが測定はしていない。」
- 根拠: `swarm-trade-decision` は存在するが呼び出しが無い（`src/lisp/school/school-voting.lisp:139-187` / `src/lisp/school/school-execution.lisp:350-353`）。

## 💻 技術パネルの意見
### Fowler:
「状態が三重化している。**同じ状態が3箇所で定義**されているのは破綻。」
- 根拠: `*last-swarm-consensus*` が重複定義（`src/lisp/core/globals.lisp:70` / `src/lisp/school/school-execution.lisp:10` / `src/lisp/school/school-state.lisp:59`）。

### Hickey:
「Swarmを外したなら**コードとコメントを削れ**。残すなら配線しろ。」
- 根拠: “Swarm Logic Removed” という死んだ意図の痕跡（`src/lisp/school/school-execution.lisp:350-353` / `src/lisp/school/school-evaluation.lisp:69-72`）。

### Uncle Bob:
「**型が壊れている**。High Councilは plist 前提なのに文字列を渡している。」
- 根拠: `convene-high-council` は `getf` で `:symbol` を読む（`src/lisp/school/school-voting.lisp:310-316`）。
- 根拠: 呼び出し側は文字列を渡す（`src/lisp/school/school-execution.lisp:210-213`）。

## 🚀 ビジョナリーの意見
### Ng:
「Swarmの精度評価は**意思決定に返っていない**。学習の閉ループがない。」
- 根拠: accuracy 分析はログだけで運用に反映しない（`src/lisp/school/school-voting.lisp:294-304`）。

### López de Prado:
「Danger Lv2 が合意値に依存するなら、**合意値は再現性のある推定量**でなければならない。現状は儀式だ。」
- 根拠: 固定閾値で判定（`src/lisp/school/school-voting.lisp:330-333`）。

### Gene Kim:
「運用通知の経路が意図とずれる可能性がある。**Status と Live Feed の混線**は運用事故。」
- 根拠: Statusは `*status-webhook-url*` へ送信（`src/lisp/core/discord.lisp:191-194`）。
- 根拠: `*status-webhook-url*` が `recruit` へ束ねられている（`src/lisp/core/config.lisp:188-189`）。

## 🚀 Musk's Decision (Final)
> 「Swarm合意は**実運用で配線されない限り廃止する**。安全弁に偽物の値は使わない。型の破綻は即修正。残すなら“測る・使う・テストする”を同時に。」

## Actionable Items
1. `*last-swarm-consensus*` の正本を1箇所に統一し、未接続なら削除する。`src/lisp/core/globals.lisp:70` `src/lisp/school/school-execution.lisp:10` `src/lisp/school/school-state.lisp:59`
2. High Council の `proposal` を plist に統一し、型不整合の回帰テストを追加。`src/lisp/school/school-voting.lisp:310-316` `src/lisp/school/school-execution.lisp:210-213` `src/lisp/tests.lisp:1195-1212`
3. Swarmの設計を残すなら配線、外すならコメントとドキュメントを削除。`src/lisp/school/school-evaluation.lisp:69-72`
4. Status通知の webhook ルーティングを明示し、Live Feed 混線を防止。`src/lisp/core/discord.lisp:191-194` `src/lisp/core/config.lisp:188-189`
