# Armada Canary Rollout Guide (2026-02-20)

> Status update (2026-02-20): Armada-specific gate/guard is currently paused in production.
> Keep `SWIMMY_ARMADA_CANARY_MODE=0` unless explicitly running an experiment.

## Goal
Armada再現モデルをいきなり全開にせず、`A` 昇格ゲート中心の canary で段階投入する。

## Option 1: Single switch (recommended)
`.env` または `config/.env.systemd` で以下を有効化:

```bash
SWIMMY_ARMADA_CANARY_MODE=1
```

この設定で以下が有効化される:
- Armada Core gate: ON
- Armada gate適用ランク: `(:A)` のみ
- trade profile必須: ON
- Armada kill switch: ON
- kill thresholds: hard DD 12%, weekly DD 4%, rolling PF 30d>=1.0, trade-ratio 60d>=0.5

## Option 2: Fine-grained control
個別制御する場合:

```bash
SWIMMY_ARMADA_CORE_PROFILE_ENABLED=1
SWIMMY_ARMADA_CORE_APPLY_RANKS=A
SWIMMY_ARMADA_CORE_REQUIRE_TRADE_PROFILE=1
SWIMMY_ARMADA_KILL_SWITCH_ENABLED=1
```

## Runtime toggle (REPL)
起動後に手動で有効化する場合:

```lisp
(swimmy.school:enable-armada-canary-mode)
```

## Rollback
即時ロールバックは以下:

```bash
SWIMMY_ARMADA_CANARY_MODE=0
SWIMMY_ARMADA_CORE_PROFILE_ENABLED=0
SWIMMY_ARMADA_KILL_SWITCH_ENABLED=0
```
