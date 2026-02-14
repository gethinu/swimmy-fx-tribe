 Backtest Result
 Round-Robin KB Batch
 02/14 16:35 JST

Progress: 126/140 results received.

Throughput: 2.25 results/sec | ETA: 6s

Current Rank Distribution:
 S-Rank: 31 |  A-Rank: 284 |  B-Rank: 151
 Graveyard/Pending: 301251

A Stage1 Failures (Batch): sharpe<0.45=6 pf<1.30=26 wr<43%=13 maxdd>=0.16=4 pass=100 total=126~%~% Strongest Categories (Current Batch):~% • USDJPY/BOTH/M3600: 8.74 Sharpe (99 strats)
• USDJPY/BOTH/M1440: 1.01 Sharpe (3 strats)
• USDJPY/BOTH/M300: 0.66 Sharpe (19 strats)
• New/Testing: -15.17 Sharpe (5 strats)
~% Top Strategy Results (Latest):~% • Bred-Bred--508-Gen32-N3980040829-808: S=18.14 PF=9.77 WR=88.6% DD=0.01% (35 trades)
• Bred-Bred--187-Gen23-N3980038264-261: S=17.75 PF=9.62 WR=88.6% DD=0.01% (35 trades)
• Bred-Bred--222-Gen30-N3980040329-718: S=17.11 PF=8.93 WR=85.7% DD=0.01% (35 trades)
• Bred-Bred--436-Gen32-N3980040463-744: S=16.18 PF=8.14 WR=88.6% DD=0.02% (35 trades)
• Bred-Bred--794-Gen32-N3980040593-767: S=16.09 PF=8.01 WR=85.7% DD=0.02% (35 trades)
• Bred-Bred--723-Gen29-N3980038311-278: S=13.47 PF=5.33 WR=80.0% DD=0.02% (35 trades)
• Bred-Bred--128-Gen28-N3980038170-239: S=13.42 PF=5.90 WR=88.2% DD=0.02% (34 trades)
• Bred-Bred--940-Gen31-N3980039835-605: S=12.48 PF=4.99 WR=88.6% DD=0.03% (35 trades)
• Bred-Bred--586-Gen29-N3980038495-317: S=12.29 PF=4.82 WR=85.3% DD=0.02% (34 trades)
• Bred-Bred--458-Gen32-N3980040289-704: S=11.75 PF=4.28 WR=80.0% DD=0.02% (35 trades)


 Evolution Factory Report
 Evolution Factory Report
Current status of the autonomous strategy generation pipeline.

 Knowledge Base (Active)
15262 Strategies

 S-Rank (Verified Elite)
31 (IS Sharpe≥0.75 PF≥1.70 WR≥50% MaxDD<10% + CPCV pass_rate≥70% & median MaxDD<12% + MC/DryRun)

 A-Rank (Pro)
274 (Sharpe≥0.45 PF≥1.30 WR≥43% MaxDD<16% + OOS≥0.35 + Expectancy>0 + MC/DryRun)

 B-Rank (Selection)
155 (Sharpe≥0.15 PF≥1.05 WR≥35% MaxDD<25%)

 New Recruits (24h)
459

 Graveyard
301134 (Library 330798)

 Retired
50119 (Library 66060)

 Source Drift:
KB active mismatch (DB=15262 KB=65375),
Archive canonical (DB archive=351253 Library canonical=351245 delta(DB-LibraryCanonical)=+8) (Library raw GRAVEYARD=330798 RETIRED=66060 overlap=45613),


 CPCV Status
1 queued | 1 sent | 0 received | 0 failed (send 0 / result 0: runtime 0 / criteria 0) | inflight 1 | last start: 02/14 16:26 JST / 07:26 UTC | reason: dispatch
CPCV Gate Failures: sharpe<0.75=0 pf<1.70=15 wr<0.50=10 maxdd>=0.10=0 elite=274 total=274
CPCV Stage2 Failures: pass_rate<70%=9 maxdd>=0.12=0 total=9

OOS sent: 2 retry: 0 success: 310 failure: 0 pending: 2 oldest: 72s (data 0 send 0 db 0) latency(avg/min/max): 54.00/54.00/54.00 sec

Validation Coverage (DB): OOS done=310 CPCV done=232 | Active OOS=299 CPCV=179

A Stage1 Failures (24h DB): sharpe<0.45=19992 pf<1.30=21038 wr<43%=19584 maxdd>=0.16=243 pass=1021 total=22234

A Gate Pressure (Active B): total=155 pass_all=25 | pass sharpe=154 pf=100 wr=61 maxdd=148 | pf_near[1.24,1.30)=0

A Near-Miss Candidates (B):
Bred-Bred--871-Ge..-35C50 deficit=0.000 fails=NONE | S=1.77 PF=1.32 WR=49% DD=0.0%,
Bred-Bred--391-Ge..310BCF deficit=0.000 fails=NONE | S=2.35 PF=1.40 WR=47% DD=0.1%,
Bred-Bred--436-Ge..-5997B deficit=0.000 fails=NONE | S=5.22 PF=2.21 WR=78% DD=0.1%,
Bred-Bred--947-Ge..-25F5C deficit=0.000 fails=NONE | S=5.32 PF=2.06 WR=50% DD=0.1%,
Bred-Bred--882-Ge..28190A deficit=0.000 fails=NONE | S=5.54 PF=2.11 WR=63% DD=0.1%,


A Candidate Funnel (latest):
USDJPY/BOTH/M3600 b=20 base=20 ready=20 queued=2,
USDJPY/BOTH/M1440 b=3 base=1 ready=1 queued=0,
USDJPY/SELL/M3600 b=0 base=0 ready=0 queued=0,
USDJPY/BUY/M3600 b=0 base=0 ready=0 queued=0,
USDJPY/BOTH/M1 b=0 base=0 ready=0 queued=0,
USDJPY/SELL/M1 b=0 base=0 ready=0 queued=0,



 Top Candidates:
Bred-Bred--508-Ge..80864E (S=18.14, A),
Bred-Bred--187-Ge..261DDE (S=17.75, A),
Bred-Bred--222-Ge..7188FD (S=17.11, A),
Bred-Bred--436-Ge..744B54 (S=16.18, A),
Bred-Bred--794-Ge..76718D (S=16.09, A),


 System Status
 Evolution Daemon Active
 Native Lisp Orchestration (V28)
02/14 16:26 JST / 07:26 UTC

---

## 読み方メモ（混在して見える理由）

このファイルは「Backtest Result（RRバッチの進捗スナップショット）」と「Evolution Factory Report（全体サマリ）」を**同じ場所に貼っただけ**なので、**生成時刻が違えば数値が一致しない**のが正常。

例（このmemoの中だけでも時刻が違う）:
- Backtest Result: `02/14 16:35 JST`
- Evolution Factory Report: `02/14 16:26 JST / 07:26 UTC`

### 正本（Source of Truth）
- Rank分布（S/A/B等）: `data/memory/swimmy.db` の `strategies` 集計
- OOS/CPCV の進捗: `data/reports/oos_status.txt` / `data/reports/cpcv_status.txt` の `updated:` 行 + mtime
- RRバックテスト進捗: `data/reports/backtest_status.txt` の `count/pending` + mtime
- `Source Drift`（DB vs KB/Library）は「ズレを見せる」ための表示。**DBを正本として読む**（KB/Libraryはドリフト検知）。

### よくある誤読
- `Progress: 126/140` は「RRバッチの受信数」。A/S昇格や Stage2（OOS/CPCV/MC/DryRun）の進捗とは別。
- Sharpeが強くても S が増えないことがある:
  - `rank.promotion.blocked` のログで、`CPCV-PASS-RATE` や `DryRun gate failed: insufficient slippage samples (need >=20)` 等の**ゲート落ち**が起きる。
  - つまり「OOS/CPCVが詰まってる」ではなく「回ってるが条件で落ちてる」ケースが混ざる。

### 1-shot 確認コマンド（ズレ潰し）
```bash
cd /home/swimmy/swimmy
ls -lh --time-style=long-iso data/reports/backtest_status.txt data/reports/oos_status.txt data/reports/cpcv_status.txt data/reports/evolution_factory_report.txt
cat data/reports/backtest_status.txt
cat data/reports/oos_status.txt
cat data/reports/cpcv_status.txt
python3 - <<'PY'
import sqlite3
db='/home/swimmy/swimmy/data/memory/swimmy.db'
con=sqlite3.connect(db); cur=con.cursor()
cur.execute("SELECT rank, COUNT(*) FROM strategies GROUP BY rank ORDER BY rank")
print(cur.fetchall())
PY
rg -n "rank\\.promotion\\.blocked|DryRun gate failed|CPCV-PASS-RATE" logs/swimmy.json.log | tail -80
```

### SERV870 観測（参考: 2026-02-14 16:29-16:31 JST 実測）
- DB rank: `:S=31, :A=278, :B=153`（`strategies` 集計）
- `oos_queue` は空（未処理キューは観測されず）
- `cpcv_status.txt`: `received 11 / failed 7 (criteria 7)`（16:29更新）
