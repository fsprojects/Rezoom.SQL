#requires -Version 5
<#
.SYNOPSIS
    Pack a dev prerelease of every Rezoom.SQL package into the local feed.

.DESCRIPTION
    Bumps the dev counter in version.local.props (one above the highest existing
    prerelease in the local feed) and runs `dotnet pack` for the three runtime
    packages plus the three wrapper meta-packages, in topological order.

    After this script runs, every consumer (TPUs, demos, future projects in the
    same repo) automatically picks up the new prerelease on its next restore,
    because their <PackageReference> uses $(RezoomSqlPkgVersion) which is
    composed from version.props + version.local.props.

    Assumes the umbrella checkout layout: a directory containing both this repo
    and a .localfeed/ sibling. The umbrella directory name doesn't matter.
#>
[CmdletBinding()]
param()

$ErrorActionPreference = 'Stop'
$repoRoot = Resolve-Path (Join-Path $PSScriptRoot '..')

# .localfeed sits beside the repo in the umbrella directory. We don't care what
# the umbrella is called — just that it's the parent.
$umbrellaRoot = Split-Path -Parent $repoRoot
$feed = Join-Path $umbrellaRoot '.localfeed'
if (-not (Test-Path $feed)) {
    New-Item -ItemType Directory -Path $feed | Out-Null
}

# Read the release version from the committed version.props.
$versionPropsPath = Join-Path $repoRoot 'version.props'
[xml]$versionProps = Get-Content $versionPropsPath
$baseVersion = $versionProps.Project.PropertyGroup.RezoomSqlVersion.Trim()

# Find the highest existing dev counter for this base version in the feed.
# Use the .NET API directly: Get-ChildItem -Filter has been observed to
# return nothing in some restricted PowerShell hosts.
$pattern = "Rezoom.SQL.Provider.$baseVersion-dev.*.nupkg"
$existing = [System.IO.Directory]::GetFiles($feed, $pattern)
$highest = 0
foreach ($f in $existing) {
    $name = [System.IO.Path]::GetFileNameWithoutExtension($f)
    if ($name -match "-dev\.(\d+)$") {
        $n = [int]$Matches[1]
        if ($n -gt $highest) { $highest = $n }
    }
}
$next = $highest + 1
$suffix = "dev.$next"
$fullVersion = "$baseVersion-$suffix"

# Persist the suffix so consumers pick it up on their next restore.
$localPropsPath = Join-Path $repoRoot 'version.local.props'
$localPropsBody = @"
<Project>
  <PropertyGroup>
    <!-- Written by build/pack-dev.ps1. Gitignored. Delete (or run pack-release.ps1)
         to return to release-version builds. -->
    <RezoomSqlVersionSuffix>$suffix</RezoomSqlVersionSuffix>
  </PropertyGroup>
</Project>
"@
Set-Content -Path $localPropsPath -Value $localPropsBody -NoNewline -Encoding utf8

Write-Host "Set version to $fullVersion in version.local.props" -ForegroundColor Cyan

# Clear any stale entries in the global packages cache for this version.
$pkgs = @(
    'rezoom.sql.mapping','rezoom.sql.compiler','rezoom.sql.provider',
    'rezoom.sql.provider.sqlite','rezoom.sql.provider.tsql','rezoom.sql.provider.postgres'
)
foreach ($p in $pkgs) {
    $cacheDir = Join-Path "$env:USERPROFILE\.nuget\packages" "$p\$fullVersion"
    if (Test-Path $cacheDir) { Remove-Item -Recurse -Force $cacheDir }
}

# Pack in topological order: runtime libs first, then wrappers that reference them.
$projects = @(
    'src/Rezoom.SQL.Mapping/Rezoom.SQL.Mapping.fsproj',
    'src/Rezoom.SQL.Compiler/Rezoom.SQL.Compiler.fsproj',
    'src/Rezoom.SQL.Provider/Rezoom.SQL.Provider.fsproj',
    'src/Rezoom.SQL.Provider.TSQL/Rezoom.SQL.Provider.TSQL.csproj',
    'src/Rezoom.SQL.Provider.SQLite/Rezoom.SQL.Provider.SQLite.csproj',
    'src/Rezoom.SQL.Provider.Postgres/Rezoom.SQL.Provider.Postgres.csproj'
)
foreach ($p in $projects) {
    $path = Join-Path $repoRoot $p
    Write-Host ""
    Write-Host "==> pack $p" -ForegroundColor Cyan
    & dotnet pack $path -c Release -o $feed --nologo 2>&1 | Select-Object -Last 2
    if ($LASTEXITCODE -ne 0) { throw "pack failed for $p" }
}

Write-Host ""
Write-Host "Packed $fullVersion to $feed" -ForegroundColor Green
Write-Host "Consumers will restore this version automatically on next build."
