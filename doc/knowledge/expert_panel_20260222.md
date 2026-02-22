# 🦅 Expert Panel Report (Consult)

**Date:** 2026-02-22
**Leader:** Elon Musk
**Mode:** consult
**Trigger:** `[$expert-panel] ... これ、どうする？`
**Purpose:** XAU AutoBot の「バックテスト良好 / 実運用不調」を前提に、`NO_GO` 維持か再挑戦かを意思決定する。
**Constraints:**
- 週末（市場クローズ）で比較サイクルが `SKIP` になる。
- 期間固定トライアル方針（途中で閾値を動かさない）。
- 既存運用で別プロセスが生きている可能性。
**Success Criteria:**
- 14日固定窓で `closed_positions>=30`, `PF>=1.10`, `WR>=0.42`, `net_profit>0` を満たして `GO`。
- トライアル対象（magic/comment）が実際に約定履歴へ反映されていることを証明できる。
- 判定レポートが「成績不良」と「試行不成立（対象取引ゼロ）」を区別できる。
**Assumptions:**
- Trial V2 は `magic=560072` / `comment=xau_autobot_trial_v2_20260222` で分離運用する意図。
- 既存本番系は `magic=560070` が稼働中。
**Uncertainties:**
- Windows 側常駐ジョブ（Startup/Task Scheduler）が何本 bot を維持しているか。
- Trial V2 実行が実際に live 発注まで到達しているか（dry-run と混同していないか）。

## 🏛️ 常設顧問の意見

### Taleb: 「今の `NO_GO` は妥当。問題は“弱い成績”以前に“試行の同定失敗”だ」
- 根拠:
  - Trial判定は `NO_GO` だが、対象 summary がゼロ (`closed_positions=0`, `PF=0`, `WR=0`, `net=0`)。`data/reports/xau_autobot_trial_judge.json:4`, `data/reports/xau_autobot_trial_judge.json:7`
  - Trial live report 診断で `after_magic_filter=0`。つまり `magic=560072` の約定が見つかっていない。`data/reports/xau_autobot_live_report_trial_v2_latest.json:29`
  - 同診断の top magic に `560070` が出ており、別系統の実績が動いている。`data/reports/xau_autobot_live_report_trial_v2_latest.json:55`
- 批判:
  - これは「戦略が弱い」判定ではなく、「試行が成立していない」判定。ここを区別しないと ruin 回避ロジックとして不十分。
- 選択肢:
  1. `NO_GO` 維持のまま trial invalid（対象取引ゼロ）を明示し、再試行する。（推奨）
  2. そのまま閾値を下げる。
  - トレードオフ: 2は観測誤差を勝率改善と誤認するリスクが高い。

### Graham: 「ノイズ通知が多すぎる。意思決定に使える情報へ圧縮しろ」
- 根拠:
  - 直近30 run が全て `market_closed` で `SKIP`。`data/reports/xau_autobot_performance_summary.json:7`, `data/reports/xau_autobot_performance_summary.json:11`
  - compare history は週末にも毎回通知済み。`data/reports/xau_autobot_cycle_comparison_history.jsonl`（末尾8件）
- 批判:
  - 「市場が閉まっている」通知はイベントではなくカレンダー情報。毎回送ると本当の異常が埋もれる。
- 選択肢:
  1. 週末は compare 自体を止める。
  2. compare は回すが `market_closed` 通知を抑制する。（推奨）
  - トレードオフ: 1はシンプル、2は可観測性を維持。

### Naval: 「自動化の前提が崩れてる。単一責任の実行者を強制しろ」
- 根拠:
  - 試行スクリプトは lock なしで起動する。`tools/xau_autobot_trial_v2_start.sh:11`
  - Windows loop の既定 config は本番 active (`tuned_auto_active`)。`tools/windows/xau_autobot_live_loop.ps1:4`
  - 実際にその config で live process が稼働中（`xau_autobot_live_loop.ps1 ... tuned_auto_active.json ... -Live`）。
- 批判:
  - 実行者が複数化すると、誰の成績を誰が判定しているか分からなくなる。これでは自動運用ではなく手動事故。
- 選択肢:
  1. `single-writer lock` を導入し、同時実行を禁止する。（推奨）
  2. 人手運用で都度 stop/start する。
  - トレードオフ: 2は短期で楽だが、再発確率が高い。

## 💻 技術パネルの意見

### Fowler: 「判定の層分離が曖昧。‘成績悪化’と‘試行不成立’を同じ `NO_GO` にしている」
- 根拠:
  - trial judge は summary閾値のみを判定し、diagnostics（magic一致件数）を評価していない。`tools/xau_autobot_trial_judge.py:83`, `tools/xau_autobot_trial_judge.py:96`
  - v50.7 文書は `NO_GO` のみ記録しており、不成立理由が見えない。`doc/knowledge/implementation_plan_v50.7.md:24`
- 批判:
  - 判定モデルの責務が粗い。デバッグ不能な `NO_GO` は改善ループを壊す。
- 選択肢:
  1. verdict を `GO / NO_GO / INVALID_TRIAL` に3値化する。（推奨）
  2. 現状維持で手作業診断を続ける。

### Hickey: 「データ同定が弱い。report選択が“最新ファイル優先”だけだと汚染される」
- 根拠:
  - promotion 側の live report解決は最新ファイル選択が中心で、`magic/comment` 検証をしていない。`tools/xau_autobot_promote_best.py:76`, `tools/xau_autobot_promote_best.py:96`
  - `_load_live_report` は summary 数値のみ抽出し識別子を落としている。`tools/xau_autobot_promote_best.py:44`
- 批判:
  - 識別子を落とした時点で、別戦略の成績混入を防げない。システムが「何を測っているか」を自分で証明できない。
- 選択肢:
  1. `trial_id/magic/comment_prefix` 一致を必須化。（推奨）
  2. ファイル名規約だけで運用。

### Uncle Bob: 「テストが通っても運用事故は防げない。欠けているテストがある」
- 根拠:
  - `test_xau_autobot_trial_judge.py` は path選択・単純閾値のみ。magic不一致/試行不成立ケースが無い。`tools/tests/test_xau_autobot_trial_judge.py:14`
  - `xau_autobot_trial_v2_start.sh` の同時実行防止に対応するテストが存在しない。
- 批判:
  - 現状テストは「関数は動く」を示すだけで、「運用で壊れない」を保証していない。
- 選択肢:
  1. 先に failing test を3本追加してから修正する。（推奨）
  2. スクリプト修正を先に進める。

## 🚀 ビジョナリーの意見

### Ng: 「モデルの勝ち負け以前に、観測設計が不十分」
- 根拠:
  - promotion report では live gap が大幅悪化（PF 1.25→0.62, WR 0.45→0.29, net -8986）。`data/reports/xau_autobot_promotion.json:95`, `data/reports/xau_autobot_promotion.json:99`, `data/reports/xau_autobot_promotion.json:102`
  - しかし `fail_on_live_underperforming=false` で昇格ブロックしない。`data/reports/xau_autobot_promotion.json:83`
- 批判:
  - “学習した”と“運用で耐える”の接続が弱い。評価パイプラインが warning を意思決定へ伝達し切れていない。
- 選択肢:
  1. underperforming を hard block に戻す。
  2. 期間固定 trial として分離し、本番昇格を一時停止する。（推奨）

### López de Prado: 「過剰適合より先に実験設計を正せ。サンプルの定義が壊れている」
- 根拠:
  - trial判定のサンプルは 14日 window でも対象取引ゼロ。`data/reports/xau_autobot_trial_judge.json:5`, `data/reports/xau_autobot_trial_judge.json:7`
  - judge は `net_profit > threshold`（非負ではなく厳密正）判定。`tools/xau_autobot_trial_judge.py:93`
- 批判:
  - “統計的優位性なし”ではなく“データ欠損”。これを混同すると改善優先度を誤る。
- 選択肢:
  1. `INVALID_TRIAL` を導入し、サンプル不足/識別不一致を別扱い。（推奨）
  2. 既存の `NO_GO` 一本で運用。

### Gene Kim: 「運用パイプラインはあるが、状態遷移が定義されていない」
- 根拠:
  - performance summary で `promotion_runs=0` かつ compare は連続 SKIP。`data/reports/xau_autobot_performance_summary.json:15`, `data/reports/xau_autobot_performance_summary.json:7`
  - promotion history ファイル自体が存在しない（状態追跡不可）。
- 批判:
  - 「実行中」「試行成立」「判定完了」のステートが分離されていないため、週明け運用で再び同じ混乱が起こる。
- 選択肢:
  1. run_id ベースで `STARTED -> ACTIVE -> EVALUATED` を記録する。（推奨）
  2. 既存の点在JSONだけで追う。

## 🚀 Musk's Decision (Final)
> 「`NO_GO` は維持で正しい。ただし理由は“戦略が弱い”ではなく“Trial V2 の測定不成立”だ。まず計測系を直し、次に14日固定 trial を1本だけ走らせる。閾値いじりはやるな。」

### やるべきこと
1. **現行 `NO_GO` は維持**（本番昇格しない）。
2. **Trial 成立判定を追加**: `after_magic_filter==0` または `after_comment_prefix_filter==0` を `INVALID_TRIAL` にする。
3. **単一起動を強制**: live/trial 起動前に既存 `xau_autobot` プロセスを検査し、同時実行を禁止。
4. **週末ノイズ通知を止める**: `market_closed` 通知は抑制（または集約）。
5. **再試行は1本のみ**: 市場オープン後に Trial V2 を 14日固定で再実行し、途中パラメータ変更禁止。

### やらなくていいこと
1. いま閾値を緩めること。
2. `NO_GO` を「運が悪かった」で覆すこと。
3. guard の ON/OFF 議論を先にやること（先に計測同定）。

## Actionable Items
1. `tools/xau_autobot_trial_judge.py` に `INVALID_TRIAL` 判定（magic/comment一致件数不足）を追加し、`data/reports/xau_autobot_trial_judge.json` に `trial_valid` を出力する。
2. `tools/xau_autobot_trial_v2_start.sh` に preflight（既存プロセス検査 + lock）を追加し、二重起動を fail-fast にする。
3. `tools/xau_autobot_promote_best.py` の live report 解決で `magic/comment_prefix` 一致チェックを導入し、別戦略レポート混入を禁止する。
4. `tools/xau_autobot_cycle_compare.py` か runner 側で `market_closed` 通知の連投抑止（1日1回上限）を入れる。
5. `tools/tests/test_xau_autobot_trial_judge.py` に `INVALID_TRIAL` 系、`tools/tests/` に起動lock系の回帰テストを追加する。
