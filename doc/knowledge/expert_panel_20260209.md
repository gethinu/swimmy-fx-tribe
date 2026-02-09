# 🦅 Expert Panel Report (Critique)

**Date:** 2026-02-09
**Leader:** Elon Musk
**Mode:** critique
**Trigger:** 「[$expert-panel](/home/swimmy/.codex/skills/expert-panel/SKILL.md)意見ください」

## 🏛️ 常設顧問の意見
### Taleb:
「S=0は安全ではなく、**計測不能な破綻リスク**だ。ゲート失敗の理由がレポートに出ないなら、危険は“静かに”進行する。**観測されないリスクは必ず爆発する**。」
- 根拠: CPCV候補なし時の失敗内訳は内部に留まる（`src/lisp/school/school-validation.lisp:490-495`）。
- 根拠: レポートはCPCVの「0 queued/received」しか出さない（`data/reports/evolution_factory_report.txt:26-27`）。

### Graham:
「S基準を守るのは正しいが、**到達経路が成立していない**。PF/MaxDDを満たせないなら、選抜と最適化の“設計”が間違っている。**収益化が永遠に始まらない設計は事業として失敗**だ。」
- 根拠: S条件はPF/MaxDD必須（`docs/llm/SPEC.md:23-26`, `src/lisp/school/school-rank-system.lisp:22-25`）。
- 根拠: PF/MaxDDはBACKTEST_RESULT由来で、OOSはSharpeのみ更新（`src/lisp/core/message-dispatcher.lisp:284-289`, `src/lisp/school/school-validation.lisp:300-303`）。

### Naval:
「CPCVは“送って終わり”で、**送達確認・再送・相関IDの運用が弱い**。自動化ではなく“祈り”。障害時に何が失われたかも分からない。」
- 根拠: CPCV送信は非同期でACKや再送なし（`src/lisp/school/school-validation.lisp:390-405`）。
- 根拠: publisher不在時はドロップ（`src/lisp/school/school-backtest-utils.lisp:92-121`）。

### Simons:
「CPCVはPF/WR/MaxDDの中央値を計算しているのに、**結果として捨てている**。統計を集めて捨てる設計は破綻。S判定がPF/MaxDDを要求するなら、CPCVの統計も連鎖させるべきだ。」
- 根拠: CPCV集計にmedian_pf/median_wr/median_maxddあり（`guardian/src/cpcv.rs:48-55`）。
- 根拠: CPCV_RESULTペイロードはSharpe中心（`guardian/src/main.rs:399-423`）。

## 💻 技術パネルの意見
### Fowler:
「**設計の正本が二重化**している。`doc/SYSTEM_ARCHITECTURE.md`はV3.0で2025年、`docs/llm/ARCHITECTURE.md`は2026年。どちらが正本か曖昧な時点で設計は腐敗する。」
- 根拠: 旧アーキテクチャの更新日と版（`doc/SYSTEM_ARCHITECTURE.md:1-14`）。
- 根拠: 現行アーキテクチャ（`docs/llm/ARCHITECTURE.md:1-96`）。

### Hickey:
「**CPCV_VALIDATEの要求スキーマが仕様に無い**。Guardianは必須キーを厳格に要求するので、仕様の欠落は即バグだ。」
- 根拠: INTERFACESはCPCV_RESULTのみで要求が無い（`docs/llm/INTERFACES.md:265-295`）。
- 根拠: Guardianはaction/strategy_name/symbol/candles_file必須（`guardian/src/main.rs:344-363`）。

### Uncle Bob:
「境界テストが足りない。GuardianのCPCVユニットテストはあるが、**Lisp→Guardianの契約テストが無い**。境界の契約は“壊れる前提”で守るべきだ。」
- 根拠: Lisp側は結果処理テストのみ（`src/lisp/tests.lisp:329-367`）。
- 根拠: Guardian側は単体テストのみ（`guardian/src/main.rs:1991-2046`）。

## 🚀 ビジョナリーの意見
### Ng:
「最適化圧力がSharpe偏重。PF/MaxDDを改善する**学習シグナルが弱い**。評価基準と最適化基準が一致していない。」
- 根拠: 投票重みはSharpeのみ（`src/lisp/school/school-voting.lisp:37-46`）。
- 根拠: S条件はPF/MaxDD必須（`src/lisp/school/school-rank-system.lisp:22-25`）。

### López de Prado:
「CPCVは“最終判定”のはずが、**入口制限**になっている。基準未達ならCPCVに入らないため、探索が萎む。過剰選別の構造だ。」
- 根拠: CPCV候補抽出がS基準を事前に要求（`src/lisp/school/school-validation.lisp:463-495`）。

### Gene Kim:
「運用の正本はsystemd(system)だと明記されているが、実運用は揺れる。さらにCPCVレポートは“0”としか出ず理由が見えない。**観測が状態しか捉えていない**。」
- 根拠: systemd(system)正本（`docs/llm/STATE.md:21-26`）。
- 根拠: レポートはCPCV理由を出さない（`data/reports/evolution_factory_report.txt:26-27`）。

## 🚀 Musk's Decision (Final)
> 「S基準は維持する。**ただし“到達のための計測と契約”を最優先で直す**。CPCVが動き、PF/MaxDDの情報が流れるようになった時点で、ようやく改善を始められる。ライブはSが出るまで封印。」

## Actionable Items
1. CPCVゲート失敗理由（PF/WR/MaxDD不足）を**レポートに露出**し、`evolution_factory_report` と一致させる: `src/lisp/school/school-validation.lisp`, `src/lisp/school/school-narrative.lisp`
2. **CPCV_VALIDATE要求スキーマをINTERFACESに明文化**（必須キーと例を追加）: `docs/llm/INTERFACES.md`
3. CPCV_RESULTに**median_pf/median_wr/median_maxddを追加**し、Lisp側で取り込めるようにする: `guardian/src/cpcv.rs`, `guardian/src/main.rs`, `src/lisp/core/message-dispatcher.lisp`, `docs/llm/INTERFACES.md`
4. **Lisp↔GuardianのCPCV契約テスト**を追加（S式必須キーとrequest_id相関を検証）: `src/lisp/tests.lisp`, `guardian/src/main.rs` or `tools/`
5. **PF/MaxDDへの最適化圧力を評価系へ組み込む**（Sharpe偏重是正）: `src/lisp/school/school-voting.lisp`, `src/lisp/school/school-rank-system.lisp`
6. `doc/SYSTEM_ARCHITECTURE.md` を **廃止・誘導**し、`docs/llm/ARCHITECTURE.md` を正本化: `doc/SYSTEM_ARCHITECTURE.md`, `docs/llm/ARCHITECTURE.md`
