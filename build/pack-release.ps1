#requires -Version 5
<#
.SYNOPSIS
    Pack a release (non-prerelease) build of every Rezoom.SQL package.

.DESCRIPTION
    Deletes version.local.props if present (so the build has no prerelease
    suffix), then runs `dotnet pack` for the three runtime packages plus the
    three wrapper meta-packages in topological order. The resulting version
    is whatever <RezoomSqlVersion> says in version.props.

    Errors out if the working tree has uncommitted changes — release builds
    should be reproducible from a tagged commit.

.PARAMETER OutputPath
    Where to drop the .nupkg files. Defaults to the local feed in the umbrella
    directory (sibling .localfeed/ to the repo).

.PARAMETER Force
    Skip the working-tree-clean check. Use only when you know what you're doing.
#>
[CmdletBinding()]
param(
    [string]$OutputPath,
    [switch]$Force
)

$ErrorActionPreference = 'Stop'
$repoRoot = Resolve-Path (Join-Path $PSScriptRoot '..')

if (-not $Force) {
    Push-Location $repoRoot
    try {
        $dirty = (& git status --porcelain) -join "`n"
        if ($dirty) {
            Write-Host "Working tree is not clean:" -ForegroundColor Red
            Write-Host $dirty
            Write-Host ""
            Write-Host "Commit or stash before running a release pack, or pass -Force." -ForegroundColor Red
            exit 1
        }
    } finally {
        Pop-Location
    }
}

# Resolve output path: explicit parameter wins, else the umbrella feed.
if ([string]::IsNullOrEmpty($OutputPath)) {
    $umbrellaRoot = Split-Path -Parent $repoRoot
    $OutputPath = Join-Path $umbrellaRoot '.localfeed'
}
if (-not (Test-Path $OutputPath)) {
    New-Item -ItemType Directory -Path $OutputPath | Out-Null
}

# Delete any local dev suffix so we get the bare release version.
$localPropsPath = Join-Path $repoRoot 'version.local.props'
if (Test-Path $localPropsPath) {
    Remove-Item -Force $localPropsPath
}

# Read the release version we're about to publish.
[xml]$versionProps = Get-Content (Join-Path $repoRoot 'version.props')
$releaseVersion = $versionProps.Project.PropertyGroup.RezoomSqlVersion.Trim()

Write-Host "Packing release version $releaseVersion to $OutputPath" -ForegroundColor Cyan

# Clear any matching entries in the global packages cache.
$pkgs = @(
    'rezoom.sql.annotations',
    'rezoom.sql.mapping','rezoom.sql.compiler','rezoom.sql.provider',
    'rezoom.sql.provider.sqlite','rezoom.sql.provider.tsql','rezoom.sql.provider.postgres'
)
$packagesRoot = Join-Path $HOME '.nuget/packages'
foreach ($p in $pkgs) {
    $cacheDir = Join-Path $packagesRoot "$p/$releaseVersion"
    if (Test-Path $cacheDir) { Remove-Item -Recurse -Force $cacheDir }
}

$projects = @(
    'src/Rezoom.SQL.Annotations/Rezoom.SQL.Annotations.csproj',
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
    & dotnet pack $path -c Release -o $OutputPath --nologo 2>&1 | Select-Object -Last 2
    if ($LASTEXITCODE -ne 0) { throw "pack failed for $p" }
}

Write-Host ""
Write-Host "Packed release $releaseVersion to $OutputPath" -ForegroundColor Green
Write-Host "Tag and push:" -ForegroundColor Yellow
Write-Host "  git tag v$releaseVersion"
Write-Host "  git push origin v$releaseVersion"
