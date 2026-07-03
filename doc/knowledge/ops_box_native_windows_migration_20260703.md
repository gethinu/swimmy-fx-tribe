# ops box → native Windows 移植手順書 (mini-PC / SERV870)

作成: 2026-07-03 / 前提: 全ストラテジー廃棄・ゼロ構築・後方互換不要
判断根拠: 別途の基盤比較で **(A) native Windows @ mini-PC** を採用済み。
これは「実装」ではなく「手順書」。commit しない。

---

## 0. 全体方針と用語

- **コア (無改造で先に動かす)**: SBCL Brain / SBCL School / Rust Guardian。全て TCP ZMQ。
  OS 抽象 (uiop / sb-posix / std) で書かれており、コード改変ゼロで Windows native 起動可能。
- **運用の皮 (今回書き換える対象)**: systemd 35 service + 17 timer / watchdog.py の systemctl 依存 /
  fcntl ロック / .sh 群 / ハードコード Linux パス。
- **一元変数**: 全パスを `%SWIMMY_HOME%`（PowerShell `$env:SWIMMY_HOME`）に集約。
  既定 `D:\swimmy`。MT5 も同一ドライブ `D:\MT5\...`（同一 FS で価格データ到達問題を消す）。
- **Python**: 実行系を `%SWIMMY_HOME%\.venv\Scripts\python.exe` に統一（Linux の `.venv/bin/python3` 相当）。

移植の本質は「臓器移植」ではなく「皮の張り替え」。着手順序は §7。

---

## 1. systemd → Task Scheduler 対応表

### 1.1 マッピング規則（共通）

| systemd | Task Scheduler (PowerShell) | 備考 |
|---|---|---|
| `Type=simple` + `Restart=always` | `New-ScheduledTaskTrigger -AtStartup` + `Settings -RestartCount N -RestartInterval` | 常駐デーモン。`setup_mt5_autostart_task.ps1` の `-RestartCount 10 / -RestartInterval 1min` を雛形に |
| `Type=simple` + `Restart=on-failure` | 同上 + ラッパで exit code 0 は再起動しない | 下記 §1.5 ラッパ参照 |
| `Type=oneshot` + `.timer` | `New-ScheduledTaskTrigger` (時刻/間隔) 単発起動タスク | Restart 無し |
| `RestartSec=N` | `-RestartInterval (New-TimeSpan -Seconds N)` | 既定は 5 or 10s |
| `StartLimitIntervalSec=300 / Burst=5` | `-RestartCount 5`（間隔で近似）| Task Scheduler は「回数」で表現。厳密な time-window は無いので回数近似 |
| `WorkingDirectory=%h/swimmy` | ラッパ .ps1 冒頭で `Set-Location $env:SWIMMY_HOME` | %h/$H は全廃 |
| `StandardOutput=append:.../x.log` | ラッパ内 `*>> $log`（PowerShell リダイレクト） | §1.5 |
| `StandardOutput=journal` | ラッパ内ファイルリダイレクト + Event Log（任意） | journald は無いのでファイル一本化 |
| `ExecStartPre=` / `ExecStartPost=` | ラッパ .ps1 内の前後行 | 下記個別対応 |
| `ExecStopPost=notify_failure.sh $SERVICE_RESULT` | ラッパの `try/finally` で終了コード判定→Discord webhook | §1.5・§4 |

**登録単位**: 各サービス = 1つの `Register-ScheduledTask`。ラッパ .ps1 を1本ずつ用意し、
`scripts\svc\<name>.ps1` に置く。登録は `scripts\install_tasks.ps1`（`install_services.sh` 相当）で一括。

### 1.2 常駐デーモン（Restart=always / -AtStartup 常駐）— 13本

| service | ExecStart 実体 | Restart | Task 化メモ |
|---|---|---|---|
| swimmy-brain | `run.sh`（→ `run.ps1`, §4.1） | always/10s | コア。SBCL 起動+ログローテ。最優先で疎通 |
| swimmy-school | `sbcl … school-daemon.lisp`（ExecStartPre で `restore_legend_61.lisp`）| always/10s | コア。ラッパで Pre 行を先頭に。`--dynamic-space-size $env:SWIMMY_SBCL_DYNAMIC_SPACE_MB` |
| swimmy-guardian | `guardian\target\release\guardian.exe` | always/5s | コア。ExecStartPre の `fuser -k 5557/tcp` → §1.4 ポート解放に置換 |
| swimmy-data-keeper | `python tools\data_keeper.py` | always/10s | fcntl ロック有り→§3 |
| swimmy-notifier | `python tools\notifier.py` | always/5s | `.user.service` 重複は1本に統合（後方互換不要） |
| swimmy-risk | `python tools\risk_gateway.py` | always/5s | |
| swimmy-evolution | `python tools\evolution_daemon.py` | always/10s | fcntl ロック+pgrep→§3・§4 |
| swimmy-pattern-similarity | `python tools\pattern_similarity_service.py` | always/10s | |
| swimmy-discord-bot | `python src\python\discord_bot.py` | always/5s | fcntl ロック→§3 |
| swimmy-mcp-gateway | `python tools\mcp_gateway.py` | always | RestartSec 未指定→既定10s |
| mt5_account_sync | `python tools\mt5_account_sync.py` | always/10s | ExecStopPost 通知→§1.5 finally |
| pending_manager | `python tools\pending_manager.py` | always/10s | |
| strategy_hunter | `python tools\strategy_hunter.py` | always/10s | |

補足: `swimmy-watchdog`（always/10s）も常駐だが中身が systemctl 依存のため §2 で別設計。
`swimmy-backtest`（on-failure/10s）と `xau-autobot-exec`（on-failure/10s）は §1.5 の exit-code 判定ラッパ。

### 1.3 timer 駆動 oneshot（17 timer → 定期起動タスク）

`Persistent=true` は「停止中に逃した発火を起動時にキャッチアップ」。Task Scheduler の
`-StartWhenAvailable`（`Settings`）が最も近いが**分単位の高頻度取りこぼしは完全再現しない**ため、
高頻度系は §1.6 の「last-run スタンプ補完」を併用する。

| timer | スケジュール | Task Scheduler トリガ | Persistent 補完 |
|---|---|---|---|
| swimmy-openclaw-signal-sync | `*:0/5`(5分) | `-RepetitionInterval 5min -RepetitionDuration ([TimeSpan]::MaxValue)` | last-run 補完(§1.6) |
| swimmy-oos-monitor | `*:0/10` | 10min 反復 | last-run 補完 |
| swimmy-polymarket-openclaw-status | `*:0/10` | 10min 反復 | last-run 補完 |
| swimmy-macro | `*:0/15` | 15min 反復 | last-run 補完 |
| swimmy-polymarket-openclaw | `*:0/30` | 30min 反復 | last-run 補完 |
| swimmy-archive-reconcile | `hourly` +Rand10m | 毎時 + `-RandomDelay 10min` | `-StartWhenAvailable` |
| swimmy-forward-probe-watch | `hourly` +Rand5m | 毎時 + RandomDelay 5min | 同上 |
| swimmy-armada-paper-readiness | `daily` +Rand15m | 毎日 + RandomDelay 15min | 同上 |
| swimmy-edge-scorecard | `daily` +Rand10m | 毎日 + RandomDelay 10min | 同上 |
| swimmy-rank-conformance-audit | `daily` +Rand15m | 毎日 + RandomDelay 15min | `.user` 重複は1本化 |
| swimmy-polymarket-weather-calibration | `06:05` +Rand10m | 毎日 06:05 + RandomDelay | 同上 |
| swimmy-pattern-backend-calibration | `06:20` +Rand15m | 毎日 06:20 + RandomDelay | 同上 |
| swimmy-system-audit | `06:40` +Rand20m | 毎日 06:40 + RandomDelay | 同上 |
| swimmy-history-update | `monthly` | 毎月1日 | Guardian stop→update→start を1ラッパ化(§1.4) |
| swimmy-polymarket-weather-snapshot | `OnBoot5m`+`Active15m` | 起動5分後 + 15min 反復 | `-StartWhenAvailable` |
| swimmy-trend-arbitrage | `OnBoot5m`+`Active2h` | 起動5分後 + 2h 反復 | 同上 |
| xau-autobot-cycle | `OnBoot2min`+`Active15m` | 起動2分後 + 15min 反復 | 同上 |

`RandomizedDelaySec` = `New-ScheduledTaskTrigger … -RandomDelay`。等価。

### 1.4 サービス間依存の特殊ケース

- **swimmy-history-update**: systemd では `ExecStartPre=systemctl stop guardian` /
  `ExecStartPost=systemctl start guardian`。→ **1本のラッパ .ps1** に統合：
  `Stop-ScheduledTask guardian` → `update_history_smart.py` → `Start-ScheduledTask guardian`。
  Task 間の Wants/After が無いので、この「オーケストレーション行」をラッパに内包する。
- **guardian の ExecStartPre `fuser -k 5557/tcp`**: 起動前のポート占有解放。
  → PowerShell 置換: `Get-NetTCPConnection -LocalPort 5557 | %{ Stop-Process -Id $_.OwningProcess -Force }`（try/catch で握り潰し、`-` 相当）。

### 1.5 ラッパ .ps1 テンプレート（全サービス共通の骨格）

```powershell
# scripts\svc\_wrapper_template.ps1
param([string]$Log)
$ErrorActionPreference = 'Continue'
Set-Location $env:SWIMMY_HOME
# .env 読み込み（run.ps1 と共通のローダを dot-source, §5）
. "$env:SWIMMY_HOME\scripts\load_env.ps1"
try {
    # ── ExecStart 本体をここに（例）──
    & "$env:SWIMMY_HOME\.venv\Scripts\python.exe" tools\risk_gateway.py *>> "$env:SWIMMY_HOME\logs\risk.log"
    $code = $LASTEXITCODE
} finally {
    # ExecStopPost=notify_failure.sh 相当（mt5_account_sync 等のみ）
    if ($code -ne 0) {
        & "$env:SWIMMY_HOME\scripts\notify_failure.ps1" $code "Risk Gateway"
    }
}
exit $code
```

- **on-failure セマンティクス**（backtest / xau-autobot-exec）: Task の再起動設定はそのままに、
  ラッパ末尾で `exit 0` を返せば Task Scheduler は「成功終了」とみなし再起動しない
  （＝正常終了時に無用な再起動を防ぐ）。異常時は非0で返し `-RestartCount` に委ねる。

### 1.6 Persistent 取りこぼし補完（高頻度 timer 用）

5〜30分間隔系は、mini-PC のスリープ/再起動での取りこぼしを Task Scheduler だけでは埋めきれない。
各 oneshot ラッパ先頭に「last-run スタンプ」チェックを入れる：

```powershell
# scripts\svc\_catchup.ps1  (dot-source)
$stamp = "$env:SWIMMY_HOME\data\runstamps\$Name.txt"
$interval = [TimeSpan]::FromMinutes($IntervalMin)
if (Test-Path $stamp) {
    $last = Get-Content $stamp | Get-Date
    if ((Get-Date) - $last -lt $interval) { exit 0 }  # まだ間隔内→スキップ
}
# …本体実行…
(Get-Date).ToString('o') | Set-Content $stamp
```

これで「起動直後にまとめて発火→間隔内は自然スキップ」となり、Persistent の実用的等価を得る。
※ Date.now 相当は実行時に取得。手順書上の設計で、実装時に各 IntervalMin を §1.3 表から流し込む。

---

## 2. watchdog.py の PowerShell/psutil 移植

現状 (`tools/watchdog.py`) の Linux 依存箇所と置換設計：

| 箇所 | 現状 | Windows 置換 |
|---|---|---|
| `:278-301 _systemd_main_pid` | `systemctl show -p MainPID --value <svc>` で PID 取得 | Task Scheduler タスク→PID は非自明。**代替**: Guardian の PUB(5557)/Brain の PUB(5556) を watchdog 自身が SUB 購読し「メッセージ沈黙時間」で生死判定（既存 silence-timer ロジックはそのまま活かせる）。PID はプロセス名で `Get-CimInstance Win32_Process -Filter "Name='sbcl.exe'"` または psutil `process_iter(['name','cmdline'])` で cmdline に `school-daemon.lisp` / `brain` を含むものを特定 |
| `:304-349 _kill_service_main_pid` | `os.kill(pid, SIGTERM)`→5s→`SIGKILL` | psutil: `p.terminate()`→`p.wait(5)`→`p.kill()`。SIGTERM/SIGKILL 直呼びを psutil の terminate/kill に置換（cross-platform 化） |
| `:265 _pid_is_alive` | `os.kill(pid, 0)` | `psutil.pid_exists(pid)` |
| `:478-489 restart 試行` | `sudo -n systemctl restart` → `systemctl restart` → SIGTERM fallback | **単純化**: 権限昇格 (sudo/polkit) の概念が消えるので3段フォールバック不要。`Restart-ScheduledTask`（= `Stop`+`Start`）または「該当プロセスを psutil で kill → Task Scheduler の -RestartCount が自動再起動」の2択に集約 |
| `:352-400 PID 変更検出 grace` | systemd MainPID の変化で startup-grace リセット | psutil で掴んだ Process オブジェクトの `create_time()` 変化を「再起動検出」に使う。ロジック本体（silence timer / grace）は無改造で流用可 |

**移植方針**: watchdog.py は「Linux 専用行だけ」を差し替える。監視ロジック（silence 検知・grace・
Discord alert `:403-`）は OS 非依存でそのまま。依存追加は `pip install psutil`。
systemctl 系の3段フォールバックは Windows では**削除して単純化**（後方互換不要のため）。

---

## 3. fcntl → portalocker 置換リスト

| ファイル | 現状 | 置換 |
|---|---|---|
| `src/python/discord_bot.py:51,57` | `fcntl.lockf(fp, LOCK_EX\|LOCK_NB)` + `/tmp/swimmy_discord_bot.pid` | `portalocker.lock(fp, LOCK_EX\|LOCK_NB)`。ロックファイルを `%SWIMMY_HOME%\data\locks\discord_bot.lock`（`/tmp` 直書き廃止） |
| `tools/evolution_daemon.py:1,9,55` | `import fcntl` + `/tmp/swimmy_evolution_daemon.lock` | 同上 → `data\locks\evolution_daemon.lock` |
| `tools/data_keeper.py:6,851` | `import fcntl` + `/tmp/swimmy_data_keeper.lock` | 同上 → `data\locks\data_keeper.lock` |

- 依存追加: `pip install portalocker`（cross-platform、`msvcrt.locking` を薄くラップ）。
- 共通ヘルパ `src/python/single_instance.py` を新設し、3ファイルはそれを import する形に一本化推奨。
- ロック置き場 `%SWIMMY_HOME%\data\locks\` を install 時に作成（§5 の一元パスに従属）。

---

## 4. .sh の .ps1 化（トリアージ）

リポジトリ全体で **.sh は 71本**。ただし ops 稼働に必須なのは一部で、大半は dev/CI/test。
全部を機械翻訳せず、**役割で3階層にトリアージ**する。

### 4.1 ops クリティカル（サービスが直接叩く／要 .ps1 化）— 優先
| .sh | 呼び元 | .ps1 化方針 |
|---|---|---|
| `run.sh` | swimmy-brain | **最重要**。`run.ps1` へ: `mktemp`→`New-TemporaryFile`、`date +%Y%m%d_%H%M%S`→`Get-Date -Format`、ログローテ `ls -t \| tail -n +8 \| xargs rm`→`Get-ChildItem logs\swimmy.*.log \| Sort LastWriteTime -Desc \| Select -Skip 7 \| Remove-Item`、`source .env`→§5 ローダ、`sbcl --dynamic-space-size … \| tee`→`sbcl … *>&1 \| Tee-Object` |
| `tools/notify_failure.sh` | ExecStopPost 群 | `notify_failure.ps1`: `$SERVICE_RESULT` 引数→ラッパ finally の exit code、Discord webhook POST は `Invoke-RestMethod` |
| `tools/edge_scorecard_runner.sh` | swimmy-edge-scorecard | ラッパ .ps1 に吸収 |
| `tools/ops/armada_paper_readiness_runner.sh` | armada-paper-readiness | 同上 |
| `tools/ops/forward_probe_watch_runner.sh` | forward-probe-watch | 同上 |
| `tools/rank_conformance_audit_runner.sh` | rank-conformance-audit | 同上 |
| `tools/trend_arbitrage_runner.sh` | trend-arbitrage | 同上 |
| `tools/system_audit.sh` | system-audit | 同上 |
| `tools/xau_autobot_cycle_runner.sh` / `xau_autobot_active_runner.sh` | xau-autobot | 同上 |

### 4.2 運用補助（watchdog/監視系・随時 .ps1 化）
`tools/kill_zombies.sh`（`pkill`→`Stop-Process`）、`reload.sh`（`pgrep/pkill`→`Get-Process`）、
`check_guardian_health.sh` / `check_school_health.sh`（`awk/cut/pgrep`→`Get-Process` + `Select-String`）、
`log_watchdog.sh`、`loop_evolution.sh`、`monitor_evolution.sh`、`morning_ritual.sh`、`clean.sh`。
→ サービス化しないものは手動運用 .ps1 として個別移植。

### 4.3 dev/CI/test（ops box に不要・移植しない）
`scripts/ci-test.sh`、`compile_*_mt5.sh`（MT5 コンパイルは Windows で MetaEditor CLI 別途）、
`setup_dev.sh`、`setup_quicklisp_local_projects.sh`、`sbcl_*_check.sh`、`run_benchmarks.sh`、
`test_*.sh`、`tools/tests/*.sh`、`install_*_service.sh`（= systemd install、§1 の `install_tasks.ps1` が代替）、
`consolidate_env.sh`（`sed -i` で systemd unit 書換→不要）、`ops/install_swimmy_systemctl_sudoers.sh`（sudoers→不要）。
→ **移植せず破棄 or dev 環境限定**。全ストラテジー廃棄前提なのでキャンペーン系 .sh も要否を都度判断。

### 共通置換辞書（Unix→PowerShell）
| Unix | PowerShell |
|---|---|
| `pgrep -f X` | `Get-CimInstance Win32_Process -Filter "CommandLine LIKE '%X%'"` / psutil |
| `pkill -9 X` | `Get-Process … \| Stop-Process -Force` |
| `mktemp` | `New-TemporaryFile` |
| `date +FMT` | `Get-Date -Format FMT` |
| `sed -i` | `(Get-Content f) -replace … \| Set-Content f`（※ systemd 書換用途は廃止） |
| `awk/cut` | `-split` / `Select-Object` / `Select-String` |
| `ls -t \| tail -n +N \| xargs rm` | `Get-ChildItem \| Sort … \| Select -Skip (N-1) \| Remove-Item` |
| `\| tee f` | `\| Tee-Object f` |
| `source .env` | §5 ローダ |

---

## 5. ハードコード Linux パス → SWIMMY_HOME 一元化

現状: 33ファイルに `/home/swimmy/swimmy`・`/tmp/` 直書き。systemd の `%h`/`$H`、Python 定数、.sh。

### 一元化ルール
1. **単一の真実**: `$env:SWIMMY_HOME`（既定 `D:\swimmy`）。全 .ps1 は冒頭で `Set-Location $env:SWIMMY_HOME`。
2. **Python**: パス直書きを廃止し `os.environ["SWIMMY_HOME"]` 起点 + `pathlib.Path` join に統一。
   `config.lisp` は既に `SWIMMY_HOME` を尊重（`run.sh:5` の `${SWIMMY_HOME:-...}` と同系）→ 全モジュールをこの規約に寄せる。
3. **`/tmp/` 全廃**: ロック/一時ファイルは `%SWIMMY_HOME%\data\locks\` と `New-TemporaryFile`（= `%TEMP%`）へ。
   §3 のロック3件、`run.sh` の bootstrap temp、各 daemon の `.lock` が該当。
4. **`.env` ローダ** `scripts\load_env.ps1`（`source .env` 相当）:
   ```powershell
   $envFile = Join-Path $env:SWIMMY_HOME '.env'
   if (Test-Path $envFile) {
     Get-Content $envFile | ? { $_ -match '^\s*[^#].*=' } | % {
       $k,$v = $_ -split '=',2; [Environment]::SetEnvironmentVariable($k.Trim(), $v.Trim())
     }
   }
   ```
5. **machine 環境変数**: `SWIMMY_HOME` / `SWIMMY_SBCL_DYNAMIC_SPACE_MB` / `SWIMMY_ZMQ_HOST` を
   `[Environment]::SetEnvironmentVariable(..., 'Machine')` で OS に登録（Task Scheduler の -AtStartup タスクからも見えるように）。
6. **監査**: `grep -rn '/home/swimmy\|/tmp/' --include=*.py --include=*.lisp` が 0 になるまで潰す（受け入れ基準）。

---

## 6. 価格データパイプライン（同一 FS 完結・再発防止）

**今回詰まった核心**: `data/historical/USDJPY_M1.csv` 等をコードが読むのに実体が ops box(Linux) 側にしか無く、
リポジトリは空、cross-box 未到達（`regen_engine_redesign` A-1(5) / `OPERATIONS.md`）。native Windows 化で境界は消えるが、
**「MT5 proprietary バイナリ → guardian が読む CSV」への変換・配置**を同一 FS 内で必ず自動化する。

### 設計
```
D:\MT5\...            ← MT5 native の履歴（proprietary .hcc/.dat）
        │  ①エクスポート（同一マシン・同一FS、9p 境界なし）
        ▼
D:\swimmy\data\historical\<SYMBOL>_M1.csv   ← guardian / school-backtest が読む正準 CSV
```

1. **エクスポータ**: MT5 側から CSV を吐く経路を1つに固定。選択肢:
   - (a) MT5 に常駐 EA/Script（`CopyRates`→ファイル書き出し）を置き、`D:\swimmy\data\historical\` に直接 write。
   - (b) 既存 `tools/update_history_smart.py`（swimmy-history-update が呼ぶ）を Windows で動かし、
     MT5 Python API（`MetaTrader5` pip パッケージ）で `copy_rates_range` → CSV。**(b) 推奨**（既存パイプの再利用）。
2. **正準命名**: `school-backtest-v2.lisp:56` が期待する `data/historical/~a_M1.csv`（`data_id` 例 `USDJPY_M1`）に一致させる。
   `*backtest-csv-override*`（`config.lisp:135` / `SWIMMY_BACKTEST_CSV_OVERRIDE`）は**既定で未設定にし、銘柄別 CSV を正とする**
   （override による「全銘柄同一 CSV 採点」事故の再発防止）。
3. **存在ガード（再発防止の要）**: サービス起動前チェックを install/起動ラッパに追加：
   ```powershell
   $need = 'USDJPY','EURUSD','GBPUSD'  # 実運用シンボル
   $miss = $need | ? { -not (Test-Path "$env:SWIMMY_HOME\data\historical\${_}_M1.csv") }
   if ($miss) { & notify_failure.ps1 1 "historical CSV missing: $($miss -join ',')"; exit 1 }
   ```
   → 「data/historical 空のまま実験が進む」を起動時に fail-fast で止める。
4. **鮮度**: history-update（月次）に加え、CSV の最終更新日をヘルスチェック（`check_*_health.ps1`）に含める。
5. **同期の単純化**: cross-box コピー・rsync・9p は**全廃**。全て `D:` 上のファイル move/write のみ。

---

## 7. 着手順序と検証手順

**原則**: コアは無改造。まず TCP ZMQ 疎通を native で確認 → 運用層を1本ずつ移す → 最後にデータ再発防止。

### Phase 0: 足場（0.5日）
- `SWIMMY_HOME=D:\swimmy` 機械環境変数、`.venv` 作成、`pip install psutil portalocker MetaTrader5`。
- `scripts\load_env.ps1` と `install_tasks.ps1` の枠だけ作る。
- **検証**: `python -c "import zmq, portalocker, psutil"` / `sbcl --version` / `cargo --version` が通る。

### Phase 1: コア無改造で TCP 疎通（1日）— 最優先
- `cargo build --release` で `guardian.exe` 生成。SBCL で `run.ps1`（まず手動起動）→ Brain 起動。School 手動起動。
- **検証**: 3プロセスを手動起動し、ZMQ ポート 5555/5556/5557/5559 が bind/connect 成功。
  `Get-NetTCPConnection -LocalPort 5555,5556,5557` で LISTEN 確認。Brain↔Guardian のメッセージが1往復流れる。
  → **ここが GO/NO-GO ゲート**。コアが native で喋れば移植の前提が確定。

### Phase 2: 常駐デーモンの Task 化（1-2日）
- §1.2 の13本 + brain/school/guardian をラッパ .ps1 化 → `install_tasks.ps1` で登録。
- fcntl 3件を portalocker 化（§3）。パス一元化（§5）を並行。
- **検証**: mini-PC 再起動 → 全常駐が -AtStartup で上がる。1つを `Stop-Process` で殺し、
  `-RestartCount` で自動復帰することを確認（Restart=always 相当の検証）。

### Phase 3: watchdog 移植（1日）
- §2 に沿って systemctl 依存を psutil + silence-timer に置換。3段フォールバック削除。
- **検証**: Brain を意図的に沈黙させ、watchdog が検知→再起動→startup-grace でフラッピングしないことを確認。

### Phase 4: timer/oneshot 群（1-2日）
- §1.3 の17本を定期タスク化 + §1.6 last-run 補完。history-update の stop→update→start オーケストレーション（§1.4）。
- **検証**: 各タスクを `Start-ScheduledTask` で即時実行しログ出力確認。mini-PC を数時間スリープ→復帰で
  高頻度タスクが「まとめ発火→間隔内スキップ」する（Persistent 等価）ことを確認。

### Phase 5: 価格データ再発防止（1日）— 締め
- §6 のエクスポータ + 存在ガード + 鮮度チェックを組む。
- **検証**: `data\historical\` を空にして起動 → **fail-fast で止まる**こと（＝今回の詰まりが構造的に再現不能）。
  CSV 投入後に school-backtest が銘柄別に採点し、override 未設定を確認。

### 破棄（後方互換不要）
- systemd/*.service・*.timer 全削除、`consolidate_env.sh`/sudoers/`install_*_service.sh`、
  Linux 専用 .sh（§4.3）、`/tmp` 直書き、WSL/9p 前提の記述。

**総工数目安**: 6-9 人日（コア改変ゼロ。作業は運用層の張り替えとデータ配管の一本化に集中）。

---

## 付録: 受け入れ基準（Definition of Done）
1. mini-PC コールドブート → コア3 + 常駐13 が自動起動、5分以内に全 LISTEN。
2. `grep -rn '/home/swimmy\|/tmp/\|systemctl\|fcntl' --include=*.py --include=*.lisp` = 0。
3. 任意の常駐を kill → 自動復帰。Brain 沈黙 → watchdog 再起動 & 非フラッピング。
4. `data\historical\` 空 → 起動が fail-fast（サイレント続行しない）。
5. WSL / 9p / cross-box / journald への依存が構成から消えている。
