# CPCV Median Gate for S-Rank Design

**Goal**
- S判定を「IS Sharpe ≥ 0.5」＋「CPCV中央値(PF/WR/MaxDD/Sharpe) + pass_rate」で確定し、S到達性と実運用妥当性を両立する。

**Non-goals**
- A/Bランクの基準は変更しない。
- CPCVのアルゴリズムや分割方式は変更しない。

## Current Behavior (Problems)
- S判定のPF/WR/MaxDDがIS値に固定され、CPCV中央値が評価に入らないため、S到達が実質停止している。
- `CPCV_RESULT`に `median_pf/median_wr/median_maxdd` が無く、Lisp側で保存・判定できない。
- Evolution ReportにCPCVゲート失敗理由（PF/WR/MaxDD）が出ず、原因分析が難しい。
- `CPCV_VALIDATE` のリクエスト仕様が `docs/llm/INTERFACES.md` に未定義。

## Proposed Design
**1) CPCV_RESULT拡張**
- Guardianは `median_pf/median_wr/median_maxdd` をCPCV結果として返却。
- LispはCPCV結果を受信し、Strategyへ保存＋DBへ永続化。

**2) S判定ルール**
- **IS Sharpe ≥ 0.5** は必須。
- **PF/WR/MaxDDはCPCV中央値で判定**。
- CPCV gateは `median_sharpe ≥ 0.5` と `pass_rate ≥ 0.5` を維持。

**3) Report/診断**
- Evolution Reportに「CPCV中央値のPF/WR/MaxDDで落ちた数」を追加。
- CPCV gate failure理由を定量表示し、運用時の診断を容易にする。

**4) 選抜/投票の指標強化**
- Sharpe偏重を緩和し、PFとMaxDDに重みを与える。
- 選抜/育種と投票の両方で同じ指標設計を適用。

## Data Flow
1. Lispが `CPCV_VALIDATE` を送信（S式）。
2. GuardianがCPCVを実行し、`CPCV_RESULT`に中央値指標を付与して返却。
3. LispがStrategyへ反映し、`swimmy.db`へ永続化。
4. S判定は「IS Sharpe + CPCV中央値」で確定。
5. Reportは中央値指標での失敗理由を表示。

## Storage & Migration
- `strategies` テーブルに `cpcv_median_pf` / `cpcv_median_wr` / `cpcv_median_maxdd` を追加。
- 既存データはNULL/0.0から開始。必要ならCPCV再実行で埋める。

## Testing
- Guardian: CPCV_RESULTに新フィールドが含まれることをユニットテストで保証。
- Lisp: CPCV_RESULTの取り込み、DB永続化、S判定ルールをテストで検証。
- 仕様テスト: `INTERFACES.md` に `CPCV_VALIDATE` を明記。

## Rollout
- Guardian更新 → Lisp更新 → systemd restart。
- CPCVバッチを回して中央値指標を蓄積。
- ReportでS到達性と失敗理由を監視。
