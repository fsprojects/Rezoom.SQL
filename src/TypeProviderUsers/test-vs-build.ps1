[CmdletBinding()]
param(
    [ValidateSet('Debug', 'Release')]
    [string]$Configuration = 'Debug',
    [switch]$NoRestore,
    [switch]$ClearNugetCache
)

# Builds TypeProviderUser.SQLite.fsproj using VS 18's MSBuild + F# tooling
# (under Program Files), which reproduces FS3033 type-provider errors that
# don't fire under `dotnet build` / `dotnet test`.
#
# Edit $msbuild below if you're on a different VS edition/version.

$ErrorActionPreference = 'Stop'
$root = $PSScriptRoot
$proj = Join-Path $root 'TypeProviderUser.SQLite\TypeProviderUser.SQLite.fsproj'

$msbuild = 'C:\Program Files\Microsoft Visual Studio\18\Community\MSBuild\Current\Bin\amd64\MSBuild.exe'
if (-not (Test-Path $msbuild)) {
    Write-Host ('VS 18 MSBuild not found at ' + $msbuild) -ForegroundColor Red
    exit 2
}

if ($ClearNugetCache) {
    $repoRoot = Resolve-Path (Join-Path $root '..\..')
    $localPropsPath = Join-Path $repoRoot 'version.local.props'
    if (Test-Path $localPropsPath) {
        [xml]$xml = Get-Content $localPropsPath
        $suffix = $xml.Project.PropertyGroup.RezoomSqlVersionSuffix
        [xml]$verXml = Get-Content (Join-Path $repoRoot 'version.props')
        $baseVer = $verXml.Project.PropertyGroup.RezoomSqlVersion.Trim()
        $fullVer = $baseVer + '-' + $suffix
        $pkgs = @(
            'rezoom.sql.mapping','rezoom.sql.compiler','rezoom.sql.provider',
            'rezoom.sql.provider.sqlite','rezoom.sql.provider.tsql','rezoom.sql.provider.postgres'
        )
        $packagesRoot = Join-Path $HOME '.nuget/packages'
        foreach ($p in $pkgs) {
            $cacheDir = Join-Path $packagesRoot ($p + '/' + $fullVer)
            if (Test-Path $cacheDir) {
                Write-Host ('Clearing ' + $cacheDir) -ForegroundColor DarkGray
                Remove-Item -Recurse -Force $cacheDir
            }
        }
    }
}

$objDir = Join-Path (Split-Path $proj -Parent) 'obj'
if (Test-Path $objDir) { Remove-Item -Recurse -Force $objDir }

$targets = if ($NoRestore) { 'Build' } else { 'Restore;Build' }
$mbArgs = @($proj, ('/p:Configuration=' + $Configuration), ('/t:' + $targets), '/v:n', '/nologo')

Write-Host ('==> ' + $msbuild + ' ' + ($mbArgs -join ' ')) -ForegroundColor Cyan
& $msbuild @mbArgs
exit $LASTEXITCODE
