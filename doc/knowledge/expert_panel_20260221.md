# 🦅 Expert Panel Report (Consult)

**Date:** 2026-02-21  
**Leader:** Elon Musk  
**Mode:** consult  
**Trigger:** 「過学習検査は必要なんじゃないの？」  
**Purpose:** 過学習検査をどこまで必須化すべきかを、現行契約と実装の整合で決める。  
**Constraints:** `docs/llm/SPEC.md` / `docs/llm/ARCHITECTURE.md` / `docs/llm/INTERFACES.md` / `docs/llm/STATE.md` を正本とする。  
**Success Criteria:**  
1. 「研究評価」と「実運用Go/No-Go」の分離を崩さない。  
2. 過学習検査を強化しても、ライブ投入が永久停止しない。  
3. 実装と正本契約の矛盾を解消できる。  
**Assumptions:** `deployment_gate_status` と `trade_logs(execution_mode='LIVE')` が運用判定の正本データ源。  
**Uncertainties:** Live投入前に必要な「forward実績」を LIVE 実績で要求し続ける設計を維持するか。

## 🏛️ 常設顧問の意見
### Taleb
- 結論: 過学習検査は必須。単一指標の勝ち逃げは破滅に直結する。  
- 現状はすでに最低限ある。Aで OOS Sharpe、Sで CPCV、実運用で forward + live-edge guard。`docs/llm/SPEC.md:26` `docs/llm/SPEC.md:27` `docs/llm/SPEC.md:40` `docs/llm/SPEC.md:41`  
- ただし「厳しさの置き方」が悪い。`*min-s-rank-strategies-for-live*=2` で全体停止できる設計は、失敗回避ではなく機会喪失リスク。`src/lisp/school/school-execution.lisp:29` `src/lisp/school/school-execution.lisp:1112`  
- **選択肢**
1. 現状維持（安全側だが、実運用ゼロ固定に陥る）
2. S件数ゲートを撤廃し `deployment_gate_status` のみで実行可否判定（契約整合が最も高い）
3. S件数ではなく `LIVE_READY` 件数ゲートへ置換（安全性と整合の折衷）

### Graham
- 正本契約は「Rankだけで live 可否を決めるな」だが、実装はまだ S-rank 件数ゲートを先に噛ませる。`docs/llm/STATE.md:31` `src/lisp/school/school-execution.lisp:1077`  
- これは小さくない矛盾。設計メッセージが二重化している。  
- **選択肢**
1. `s-rank-gate-passed-p` を live経路から外す（最小変更で一貫）
2. `s-rank-gate-passed-p` を `live-ready-pool-sufficient-p` に改名し判定対象を `deployment_gate_status` へ変更
3. S-gateを残すなら `STATE` を更新して「例外契約」として明文化

### Naval
- 過学習検査は「あるかどうか」ではなく「資本投入トリガーと直結しているか」で決まる。  
- 今は forward が `LIVE` 実績を要求する一方、live入口が Sゲートで閉じるため、自己矛盾で `LIVE_READY` 到達が難化する。`src/lisp/school/school-validation.lisp:581` `src/lisp/school/school-validation.lisp:657` `src/lisp/school/school-execution.lisp:1112`  
- **選択肢**
1. Forward を paper execution mode でも算入可能にする（到達可能性↑、厳密性↓）
2. 先に少量 live を許可する bootstrap 枠を設ける（到達可能性と厳密性の中間）
3. 完全 live 実績主義を維持し、Sゲートのみ撤廃する（契約整合重視）

### Jim Simons
- A判定の OOS は Sharpe 単独で、過学習検査としては弱い。`src/lisp/school/school-rank-system.lisp:528` `src/lisp/school/school-rank-system.lisp:529`  
- Sでは CPCV pass_rate/maxdd を見ており妥当。`src/lisp/school/school-rank-system.lisp:515` `src/lisp/school/school-rank-system.lisp:517`  
- **選択肢**
1. Aにも CPCV-lite（pass_rate下限のみ）を追加
2. Aは現状維持、LIVE_READY 要件に CPCV pass_rate の下限を追加
3. AはOOS単独維持、forward証拠（days/trades）を増量して代替

## 💻 技術パネルの意見
### Fowler
- 良い点: WFV は既定OFFかつ analysis-only で昇格非関与。`src/lisp/core/config.lisp:281` `src/lisp/school/school-backtest.lisp:593`  
- 悪い点: 実運用判定分離を導入したのに、旧来の S-count gate が live 経路の前段に残存。`src/lisp/school/school-execution.lisp:1112`  
- リファクタ推奨: 「実行前ゲート」を `deployment gate` / `live edge` / `risk` の3層に固定し、rank件数ゲートを削除または別目的に分離。

### Hickey
- いまの複雑性は「概念の重複」。Rankは研究用、live可否は運用用と言いながら、実行で rank件数を使う。`docs/llm/STATE.md:64` `src/lisp/school/school-execution.lisp:1077`  
- 単純化案: `verify-signal-authority` に集約済みの `deployment_gate_status` 判定を唯一の運用ゲートにする。`src/lisp/school/school-execution.lisp:740` `src/lisp/school/school-execution.lisp:742`

### Uncle Bob
- テストは deployment gate / live-edge の fail-closed を持っていて良い。`src/lisp/tests.lisp:4709` `src/lisp/tests.lisp:4746`  
- ただし「State契約と矛盾する S-rank件数ゲート」については契約テストが不足。  
- 必須テスト追加:
1. `deployment_gate_status=LIVE_READY` かつ S件数不足でも発注可能（または契約上禁止なら明記）
2. Forward算入ロジックが live閉塞時に永久 `FORWARD_FAIL` へ偏らないこと

## 🚀 ビジョナリーの意見
### Ng
- 過学習検査は必要。特に単一OOS split だけでは多重試行バイアスに弱い。  
- 追加するなら、A段階で軽量CPCVか、LIVE_READY時にCPCV再確認を足すのが実務的。

### López de Prado
- 「OOS + CPCV + Forward」の三層は方向として正しい。  
- 問題は順序で、現状は live実績が得られないと forward を満たせない構造になりうる。これは検査強化ではなく検査不能化。  
- **選択肢**
1. strict: LIVE_READYに CPCV 必須化 + S件数ゲート撤廃
2. balanced: Aは現状、LIVE_READYに `CPCV pass_rate>=0.6` を追加
3. minimal: まず S件数ゲート撤廃のみ実施し、現行 forward を観測

### Gene Kim
- 可観測性は整っている。forward status レポートと集計がある。`src/lisp/school/school-validation.lisp:715` `src/lisp/school/school-narrative.lisp:531`  
- 運用上の不足は「閉塞の早期検知KPI」。`FORWARD_RUNNING` 長期滞留と `FORWARD_FAIL(insufficient trades)` の連続をアラート化すべき。

## 対抗意見（反証）
- 反証: 「過学習検査を増やすと、ますます live できなくなる」  
- 回答: その懸念は正しい。だから検査を弱めるのではなく、`Rank件数ゲート` と `deployment_gate_status` の役割重複を解消し、同じ厳しさでも到達可能な順序に直すべき。

## 🚀 Musk's Decision (Final)
> 「答えは Yes。過学習検査は必要だ。ただし今は“厳しい”のではなく“詰まる”設計が混じっている。Rankゲートを実運用ゲートから外し、到達可能な厳格性に再配置しろ。」

**Do**
1. live実行の最終判定を `deployment_gate_status + live-edge guard` に統一する。`docs/llm/STATE.md:31` `src/lisp/school/school-execution.lisp:742`  
2. `s-rank-gate-passed-p` の live経路関与を停止するか、`LIVE_READY` 件数基準へ置換する。`src/lisp/school/school-execution.lisp:1077`  
3. 過学習検査強化は「A/CPCV-lite追加」か「LIVE_READY要件へCPCV追加」のどちらか1つから始める（同時導入しない）。

**Don’t**
1. 「検査を緩める」方向で解決しない。  
2. 正本（STATE）を更新せずに実装だけ先行しない。  
3. WFV を昇格経路に戻さない（analysis-only維持）。`docs/llm/STATE.md:66` `src/lisp/school/school-backtest.lisp:598`

## Actionable Items
1. **契約確認**: `docs/llm/STATE.md` の live gate 契約（Rank非依存）と `s-rank-gate-passed-p` の扱いを整合させる方針を決定。  
2. **実装方針A/B選定**:  
   - A案: S件数ゲート撤廃（最短）  
   - B案: S件数ゲートを `LIVE_READY件数` ゲートへ置換（運用安全寄り）  
3. **過学習検査の次段**: `LIVE_READY` に CPCV下限を追加するかを決める（`STATE`→`INTERFACES`必要時→実装の順）。  
4. **テスト追加**: `deployment_gate_status` 優先の実行経路を保証する回帰テストを追加。  
5. **運用監視**: `FORWARD_FAIL: insufficient trades` の連続発生を定量監視し、閉塞を自動検知する。
