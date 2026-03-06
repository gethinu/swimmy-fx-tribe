[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string]$TerminalPath,
    [string]$TerminalArguments = ""
)

$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest

if (-not (Test-Path -LiteralPath $TerminalPath)) {
    throw "terminal64.exe not found: $TerminalPath"
}

# Global single-instance guard: if any MT5 terminal process exists, skip new launch.
$existing = Get-Process -Name "terminal64" -ErrorAction SilentlyContinue
if ($existing) {
    Write-Host "MT5 already running. Skip launch."
    exit 0
}

$startParams = @{
    FilePath         = $TerminalPath
    WorkingDirectory = (Split-Path -Path $TerminalPath -Parent)
}

if (-not [string]::IsNullOrWhiteSpace($TerminalArguments)) {
    $startParams.ArgumentList = $TerminalArguments
}

Start-Process @startParams | Out-Null
Start-Sleep -Seconds 2

$after = Get-Process -Name "terminal64" -ErrorAction SilentlyContinue
if (-not $after) {
    throw "MT5 launch did not create terminal64.exe process."
}
