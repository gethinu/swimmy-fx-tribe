# run.ps1 — PHASE 1 SCAFFOLD (native Windows port of run.sh)
# Handbook: doc/knowledge/ops_box_native_windows_migration_20260703.md §4.1, §7 Phase 1.
#
# Faithful port of run.sh with the bash/Linux dependencies removed:
#   mktemp        -> New-TemporaryFile
#   date +FMT     -> Get-Date -Format
#   ls|tail|xargs -> Get-ChildItem|Sort|Select -Skip|Remove-Item
#   source .env   -> load_env.ps1
#   | tee         -> Tee-Object
# All paths are %SWIMMY_HOME%-relative. No /home/swimmy, no /tmp.
#
# -Component brain    (default) : launch SBCL Brain only  (== run.sh, one Task Scheduler task)
# -Component guardian           : launch Rust guardian.exe only
# -Component core               : launch Brain + Guardian + data_keeper for the
#                                 Phase 1 GO/NO-GO TCP ZMQ smoke test (foreground, Ctrl-C to stop)
#
# THIS IS A TEMPLATE. Do not wire to Task Scheduler until Phase 1 smoke test passes.

[CmdletBinding()]
param(
    [ValidateSet('brain', 'guardian', 'core')]
    [string]$Component = 'brain',
    [switch]$SkipDataGuard
)

$ErrorActionPreference = 'Stop'
. "$PSScriptRoot\load_env.ps1"
Set-Location $env:SWIMMY_HOME

$logDir = Join-Path $env:SWIMMY_HOME 'logs'
if (-not (Test-Path -LiteralPath $logDir)) { New-Item -ItemType Directory -Path $logDir | Out-Null }

# --- fail-fast price-data guard (handbook §6) ---
if (-not $SkipDataGuard) {
    & "$PSScriptRoot\check_historical_data.ps1"
    if ($LASTEXITCODE -ne 0) { throw "run.ps1 aborted: historical data guard failed." }
}

function Start-Brain {
    # Log rotation parity with run.sh:24-30
    $mainLog = Join-Path $logDir 'swimmy.log'
    if (Test-Path -LiteralPath $mainLog) {
        $stamp = Get-Date -Format 'yyyyMMdd_HHmmss'
        Move-Item -LiteralPath $mainLog -Destination (Join-Path $logDir "swimmy.$stamp.log")
    }
    # Keep only last 7 rotated logs (run.sh:30)
    Get-ChildItem -LiteralPath $logDir -Filter 'swimmy.*.log' -ErrorAction SilentlyContinue |
        Sort-Object LastWriteTime -Descending | Select-Object -Skip 7 | Remove-Item -Force -ErrorAction SilentlyContinue

    # Bootstrap entrypoint (run.sh:33-65) — prefer brain.lisp, else temp ASDF bootstrap.
    $bootFile = Join-Path $env:SWIMMY_HOME 'brain.lisp'
    $bootTemp = $null
    if (-not (Test-Path -LiteralPath $bootFile)) {
        Write-Warning 'brain.lisp not found; falling back to ASDF bootstrap.'
        $bootTemp = New-TemporaryFile
        @'
(in-package :cl-user)
(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(let ((ql (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file ql) (load ql)))
(format t "~%[LOADER] Loading Swimmy System via ASDF...~%")
(handler-case
    (handler-bind ((style-warning #'muffle-warning) (warning #'muffle-warning))
      (asdf:load-system :swimmy))
  (error (c) (format t "~%[FATAL] Failed to load system: ~a~%" c) (sb-ext:exit :code 1)))
(uiop:symbol-call :swimmy.main :start-system)
'@ | Set-Content -LiteralPath $bootTemp -Encoding UTF8
        $bootFile = $bootTemp.FullName
    }

    $heap = if ($env:SWIMMY_SBCL_DYNAMIC_SPACE_MB) { $env:SWIMMY_SBCL_DYNAMIC_SPACE_MB } else { '6144' }
    $sbcl = Resolve-Sbcl
    Write-Host "[$(Get-Date)] Starting Swimmy Ver 41.5 (heap ${heap}MB) via $sbcl..."
    try {
        # sbcl ... 2>&1 | tee logs/swimmy.log  (run.sh:71)
        & $sbcl --dynamic-space-size $heap --noinform --load $bootFile 2>&1 | Tee-Object -FilePath $mainLog
    } finally {
        if ($bootTemp) { Remove-Item -LiteralPath $bootTemp -Force -ErrorAction SilentlyContinue }
    }
}

function Resolve-Sbcl {
    # winget's SBCL MSI installs to Program Files but does not always add it to PATH.
    $c = Get-Command sbcl -ErrorAction SilentlyContinue
    if ($c) { return $c.Source }
    foreach ($p in @(
        'C:\Program Files\Steel Bank Common Lisp\sbcl.exe',
        'C:\Program Files (x86)\Steel Bank Common Lisp\sbcl.exe',
        (Join-Path $env:LOCALAPPDATA 'Programs\Steel Bank Common Lisp\sbcl.exe')
    )) { if (Test-Path -LiteralPath $p) { return $p } }
    throw "sbcl.exe not found on PATH or standard install dirs. Install SBCL or add it to PATH."
}

function Start-Guardian {
    # Workspace build lands the exe at the repo-root target\, not guardian\target\.
    $exe = Join-Path $env:SWIMMY_HOME 'target\release\guardian.exe'
    if (-not (Test-Path -LiteralPath $exe)) {
        $exe = Join-Path $env:SWIMMY_HOME 'guardian\target\release\guardian.exe'
    }
    if (-not (Test-Path -LiteralPath $exe)) {
        throw "guardian.exe not found (checked target\ and guardian\target\) — run 'cargo build --release' first."
    }
    # ExecStartPre parity: free port 5557 if a stale holder exists (handbook §1.4, was `fuser -k 5557/tcp`).
    try {
        Get-NetTCPConnection -LocalPort 5557 -State Listen -ErrorAction SilentlyContinue |
            ForEach-Object { Stop-Process -Id $_.OwningProcess -Force -ErrorAction SilentlyContinue }
    } catch { }
    $gLog = Join-Path $logDir 'guardian.log'
    Write-Host "[$(Get-Date)] Starting Guardian..."
    & $exe 2>&1 | Tee-Object -FilePath $gLog
}

switch ($Component) {
    'brain'    { Start-Brain }
    'guardian' { Start-Guardian }
    'core' {
        # Phase 1 GO/NO-GO smoke test: bring up all three core processes as background
        # jobs, verify TCP ZMQ ports, then hold. Ctrl-C to tear down.
        Write-Host '=== PHASE 1 CORE SMOKE TEST (Brain + Guardian + data_keeper) ===' -ForegroundColor Cyan
        $py = Join-Path $env:SWIMMY_HOME '.venv\Scripts\python.exe'
        $jobs = @()
        $jobs += Start-Job -Name brain    -ScriptBlock { & "$using:PSScriptRoot\run.ps1" -Component brain -SkipDataGuard }
        $jobs += Start-Job -Name guardian -ScriptBlock { & "$using:PSScriptRoot\run.ps1" -Component guardian }
        if (Test-Path -LiteralPath $py) {
            $jobs += Start-Job -Name data_keeper -ScriptBlock {
                & "$using:py" (Join-Path $using:env:SWIMMY_HOME 'tools\data_keeper.py')
            }
        }
        Start-Sleep -Seconds 8
        Write-Host 'Checking ZMQ TCP ports (5555 PULL / 5556 PUB brain, 5557 PUB guardian, 5559 SUB, 5561 data_keeper)...'
        $ports = 5555, 5556, 5557, 5559, 5561
        foreach ($p in $ports) {
            $listening = Get-NetTCPConnection -LocalPort $p -State Listen -ErrorAction SilentlyContinue
            $mark = if ($listening) { 'LISTEN  OK' } else { 'DOWN' }
            $color = if ($listening) { 'Green' } else { 'Red' }
            Write-Host ("  port {0,-5} : {1}" -f $p, $mark) -ForegroundColor $color
        }
        Write-Host 'Smoke jobs running. Inspect: Receive-Job -Name brain -Keep. Stop: Get-Job | Stop-Job; Get-Job | Remove-Job' -ForegroundColor Yellow
    }
}
