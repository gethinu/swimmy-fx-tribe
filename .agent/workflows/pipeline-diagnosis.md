---
description: Diagnose evolution pipeline health (backtest flow, strategy metrics, queue status)
---

# Pipeline Diagnosis Workflow

進化パイプラインの健全性を診断するワークフロー。バックテストフロー、戦略メトリクス、キュー状況を確認する。

## 1. サービス状態の確認

```bash
systemctl --user status swimmy-brain swimmy-guardian swimmy-school swimmy-data-keeper 2>&1 | head -60
```

ポート確認:
```bash
ss -tulnp 2>&1 | grep -E "555[5679]|556[0123]|558[01]"
```

## 2. 戦略メトリクス確認

// turbo
```bash
cd /home/swimmy/swimmy && python3 -c "
import sqlite3
conn = sqlite3.connect('data/memory/swimmy.db')
cur = conn.cursor()

print('=== Strategy Rank Summary ===')
cur.execute('SELECT rank, COUNT(*) FROM strategies GROUP BY rank ORDER BY rank')
for r in cur.fetchall(): print(f'{r[0]}: {r[1]}')

print('\n=== Sharpe Status ===')
cur.execute('SELECT COUNT(*) FROM strategies WHERE sharpe IS NOT NULL AND sharpe > 0')
print(f'Valid Sharpe: {cur.fetchone()[0]}')
cur.execute('SELECT COUNT(*) FROM strategies WHERE sharpe IS NULL OR sharpe = 0')
print(f'NULL/0 Sharpe: {cur.fetchone()[0]}')

print('\n=== Top 10 by Sharpe ===')
cur.execute('SELECT rank, name, ROUND(sharpe, 3) FROM strategies WHERE sharpe > 0.1 ORDER BY sharpe DESC LIMIT 10')
for r in cur.fetchall(): print(f'{r[0]:12} | S={r[2]:6} | {r[1]}')
"
```

## 3. バックテスト結果フローの確認

// turbo
```bash
cd /home/swimmy/swimmy && cat data/reports/backtest_status.txt 2>/dev/null || echo "File not found"
```

// turbo
```bash
cd /home/swimmy/swimmy && cat data/reports/oos_status.txt 2>/dev/null || echo "File not found"
```

## 4. Evolution Factory Report 確認

// turbo
```bash
cd /home/swimmy/swimmy && cat data/reports/evolution_factory_report.txt 2>/dev/null | head -40
```

## 5. Backtest Service プロセス確認

// turbo
```bash
ps aux | grep -E "backtest|guardian.*backtest" | grep -v grep
```

## 6. 直近のログ確認

Brain ログ:
```bash
journalctl --user -u swimmy-brain -n 50 --no-pager 2>&1 | tail -30
```

School ログ:
```bash
journalctl --user -u swimmy-school -n 50 --no-pager 2>&1 | tail -30
```

## 7. 診断結果の評価

以下を確認:
- **Sharpe未評価戦略が大量にある場合**: バックテスト結果フローに問題あり
- **S-Rank / A-Rank がゼロの場合**: OOS/CPCV検証パイプラインが停止
- **backtest_status.txt の count が増えていない場合**: 結果が返ってきていない
- **Guardian --backtest-only が高CPU使用率の場合**: 処理が詰まっている

## 8. （オプション）詳細デバッグ

デバッグログ有効化:
```bash
export SWIMMY_BACKTEST_DEBUG_RECV=1
export SWIMMY_BACKTEST_DUMP_GUARDIAN=1
```

DLQ(Dead Letter Queue) 確認:
```bash
journalctl --user -u swimmy-brain -n 200 --no-pager 2>&1 | grep -i "DLQ\|dead.letter"
```
