#requires -Version 5
<#
.SYNOPSIS
    Pack the three Rezoom.SQL parent libraries into the local feed.

.DESCRIPTION
    Packs FParsec-Pipes, LicenseToCIL, and Rezoom from their sibling repos at
    the versions declared in each fsproj, dropping the .nupkg files into the
    umbrella .localfeed. Run this when you've changed a parent lib and want
    Rezoom.SQL (or the TPUs / demos) to pick up the fresh bits.

    These libs are infrequently changed, so their versions are bumped manually
    in each fsproj. The centralized version.props mechanism only covers the
    Rezoom.SQL packages. If you're publishing parent changes, bump the
    parent's <Version> first.

    Assumes the umbrella checkout layout: this repo lives alongside Rezoom,
    FParsec-Pipes, and LicenseToCIL under a common parent directory.

.PARAMETER Only
    Pack only the named parent. One of FParsec-Pipes, LicenseToCIL, Rezoom.
    Default: all three.
#>
[CmdletBinding()]
param(
    [ValidateSet('FParsec-Pipes', 'LicenseToCIL', 'Rezoom')]
    [string]$Only
)

$ErrorActionPreference = 'Stop'
$repoRoot = Resolve-Path (Join-Path $PSScriptRoot '..')
$umbrellaRoot = Split-Path -Parent $repoRoot
$feed = Join-Path $umbrellaRoot '.localfeed'
if (-not (Test-Path $feed)) {
    New-Item -ItemType Directory -Path $feed | Out-Null
}

$parents = @(
    @{ Name = 'FParsec-Pipes'; Project = 'FParsec-Pipes/FParsec-Pipes/FParsec-Pipes.fsproj'; PkgId = 'fparsec-pipes' }
    @{ Name = 'LicenseToCIL';  Project = 'LicenseToCIL/LicenseToCIL/LicenseToCIL.fsproj';     PkgId = 'licensetocil' }
    @{ Name = 'Rezoom';        Project = 'Rezoom/src/Rezoom/Rezoom.fsproj';                   PkgId = 'rezoom' }
)
if ($Only) {
    $parents = $parents | Where-Object { $_.Name -eq $Only }
}

$packagesRoot = Join-Path $HOME '.nuget/packages'

foreach ($p in $parents) {
    $projPath = Join-Path $umbrellaRoot $p.Project
    if (-not (Test-Path $projPath)) {
        Write-Host "Missing $($p.Name) at $projPath. Expected sibling checkout. Skipping." -ForegroundColor Yellow
        continue
    }

    # Read the version from the fsproj so we can clear the matching cache entry
    # and stay readable in the output.
    [xml]$proj = Get-Content $projPath
    $version = ($proj.Project.PropertyGroup.Version -as [string]).Trim()

    if ($version) {
        $cacheDir = Join-Path $packagesRoot "$($p.PkgId)/$version"
        if (Test-Path $cacheDir) { Remove-Item -Recurse -Force $cacheDir }
    }

    Write-Host ""
    Write-Host "==> pack $($p.Name) ($version)" -ForegroundColor Cyan
    & dotnet pack $projPath -c Release -o $feed --nologo 2>&1 | Select-Object -Last 2
    if ($LASTEXITCODE -ne 0) { throw "pack failed for $($p.Name)" }
}

Write-Host ""
Write-Host "Done. Packages available at $feed" -ForegroundColor Green
