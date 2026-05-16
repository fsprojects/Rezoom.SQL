#requires -Version 5
<#
.SYNOPSIS
    Builds and runs the Rezoom.SQL type-provider user smoke tests.

.DESCRIPTION
    These two projects exercise the actual published Rezoom.SQL.Provider* nupkgs
    against live SQLite and Postgres connections. They replace the legacy
    "close VS in the user, build the provider, reopen, smoke test" workflow.

    SQLite tests are hermetic. They drop/recreate the rzsql.db file each run.
    Postgres tests skip themselves (NUnit Inconclusive) when no server is
    reachable at the configured connection string. Override the default via:
        $env:REZOOM_TPU_POSTGRES = 'Host=...;Database=...;Username=...;Password=...'

    The wrapper nupkgs (Rezoom.SQL.Provider.{SQLite,Postgres,TSQL} 0.12.0) must
    already be in ../../.localfeed or another configured NuGet source.

.PARAMETER Skip
    Subset to skip: 'Postgres', 'SQLite', or 'None' (default).

.PARAMETER Configuration
    Debug (default) or Release.
#>
[CmdletBinding()]
param(
    [ValidateSet('None', 'SQLite', 'Postgres')]
    [string]$Skip = 'None',
    [ValidateSet('Debug', 'Release')]
    [string]$Configuration = 'Debug'
)

$ErrorActionPreference = 'Stop'
$root = $PSScriptRoot

$projects = @()
if ($Skip -ne 'SQLite') {
    $projects += Join-Path $root 'TypeProviderUser.SQLite\TypeProviderUser.SQLite.fsproj'
}
if ($Skip -ne 'Postgres') {
    $projects += Join-Path $root 'TypeProviderUser.Postgres\TypeProviderUser.Postgres.fsproj'
}

$failed = @()
foreach ($proj in $projects) {
    Write-Host ""
    Write-Host "==> dotnet test $([System.IO.Path]::GetFileNameWithoutExtension($proj))" -ForegroundColor Cyan
    & dotnet test $proj -c $Configuration --nologo
    if ($LASTEXITCODE -ne 0) { $failed += $proj }
}

if ($failed.Count -gt 0) {
    Write-Host ""
    Write-Host "Failed:" -ForegroundColor Red
    $failed | ForEach-Object { Write-Host "  - $_" -ForegroundColor Red }
    exit 1
}

Write-Host ""
Write-Host "All TP user tests passed." -ForegroundColor Green
