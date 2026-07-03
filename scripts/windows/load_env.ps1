# load_env.ps1 — PHASE 1 SCAFFOLD (native Windows ops box migration)
# Equivalent of `set -a; source .env; set +a` in run.sh.
# Dot-source this from run.ps1 and every service wrapper:  . "$PSScriptRoot\load_env.ps1"
#
# Reads %SWIMMY_HOME%\.env (KEY=VALUE, # comments) into the current process env.
# Does NOT start anything. See doc/knowledge/ops_box_native_windows_migration_20260703.md §5.

$ErrorActionPreference = 'Stop'

if (-not $env:SWIMMY_HOME -or [string]::IsNullOrWhiteSpace($env:SWIMMY_HOME)) {
    # Default matches the handbook: mini-PC keeps repo + MT5 on D:.
    $env:SWIMMY_HOME = 'D:\swimmy'
}

$envFile = Join-Path $env:SWIMMY_HOME '.env'
if (Test-Path -LiteralPath $envFile) {
    Get-Content -LiteralPath $envFile | Where-Object { $_ -match '^\s*[^#].*=' } | ForEach-Object {
        $pair = $_ -split '=', 2
        $key = $pair[0].Trim()
        $val = if ($pair.Count -gt 1) { $pair[1].Trim() } else { '' }
        # Strip surrounding quotes if present.
        if ($val.Length -ge 2 -and (($val[0] -eq '"' -and $val[-1] -eq '"') -or ($val[0] -eq "'" -and $val[-1] -eq "'"))) {
            $val = $val.Substring(1, $val.Length - 2)
        }
        [Environment]::SetEnvironmentVariable($key, $val)
    }
}

# Dev safety parity with run.sh:16-19
if ($env:SWIMMY_FORCE_DISABLE_DISCORD -eq '1') {
    [Environment]::SetEnvironmentVariable('SWIMMY_DISABLE_DISCORD', '1')
}
