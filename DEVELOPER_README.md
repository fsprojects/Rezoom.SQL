# Rezoom.SQL developer notes

Notes for working on this repo.

## Prerequisites

- .NET SDK 10 or newer
- PowerShell 5.1+ on Windows; PowerShell 7+ on Linux/macOS (install: <https://learn.microsoft.com/powershell/scripting/install/installing-powershell>). The build scripts (`build/pack-dev.ps1`, `build/pack-release.ps1`) use PowerShell.

## Repo layout assumption

The dev workflow assumes you have this repo cloned alongside its sibling repos (most importantly
[Rezoom](https://github.com/rspeele/Rezoom)) under a common parent directory. The parent's name doesn't matter, but the
siblings need to be together.

```
my-rezoom-source-code/
    .localfeed/         (NuGet feed for in-progress dev packages)
    NuGet.config        (configures .localfeed as a package source)
    Rezoom/
    Rezoom.SQL/         (this repo)
    FParsec-Pipes/
    LicenseToCIL/
```

You need a `NuGet.config` at the parent level so package restore finds
`.localfeed` ahead of nuget.org:

```xml
<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <packageSources>
    <clear />
    <add key="local" value=".localfeed" />
    <add key="nuget.org" value="https://api.nuget.org/v3/index.json" protocolVersion="3" />
  </packageSources>
</configuration>
```

This allows pushing local "updates" to the NuGet packages so the TypeProviderUser tests can reference them just like a
real downstream user.

If you're only working on Rezoom.SQL and don't need to make changes to Rezoom, you can use the latest published Rezoom
from nuget.org. If you're changing both, pack Rezoom into the same `.localfeed` first using whatever script that repo
provides.

## Versioning during dev

All Rezoom.SQL packages share a single version. Two files define it:

- `version.props` (committed): `RezoomSqlVersion = 0.13.0`, represents the upcoming or
  current release version. Bumped only at actual releases.
- `version.local.props` (gitignored): written by `pack-dev.ps1`, contains
  `RezoomSqlVersionSuffix = dev.N`. The combined version becomes
  `0.13.0-dev.N`. Bumped extremely frequently during development, every time we have to "pack-dev.ps1" and smoke-test the TPUs.

Every package and consumer reads these via `Directory.Build.props`. Wrapper
csprojs and TPU / demo fsprojs reference our packages as
`Version="$(RezoomSqlPkgVersion)"`, which resolves to the current dev or
release version automatically.

## Scripts

### `build/pack-dev.ps1`

Bumps the dev counter (one above the highest existing prerelease in the local feed), writes `version.local.props`, packs
all six packages. Run after any change you want the TPUs or demos to see.

```powershell
./build/pack-dev.ps1
```

### `build/pack-release.ps1`

Deletes `version.local.props` so the build has no prerelease suffix, then packs all six packages at the release version.
Errors out if the working tree is dirty (override with `-Force`). After it succeeds, tag and push:

```powershell
./build/pack-release.ps1
git tag v1.0.0
git push origin v1.0.0
```

### `build/pack-parents.ps1`

Packs the three parent libs (FParsec-Pipes, LicenseToCIL, Rezoom) from their sibling repos at the versions declared in
each fsproj. Use this when you've edited a parent and want Rezoom.SQL to pick up the new dev version.

```powershell
./build/pack-parents.ps1                  # all three
./build/pack-parents.ps1 -Only Rezoom     # one at a time
```

The parents don't participate in the centralized `version.props` mechanism (they change rarely; their versions are
bumped manually). If you're publishing parent changes, bump the parent's `<Version>` first, then run this script.

### `build/regen-doc-nav.ps1`

Rewrites the breadcrumb + prev/next nav blocks at the top and bottom of every doc page listed in `SUMMARY.md`. Run after
editing `SUMMARY.md` (adding pages, reordering, renaming) to bring all nav links back into sync.

```powershell
./build/regen-doc-nav.ps1
```

### `src/TypeProviderUsers/test-tp-users.ps1`

Runs `dotnet test` on both TPU projects. SQLite can make its own DB file, but Postgres auto-
skips when no server is reachable. Either set up your local Postgres like mine, with an
`rz` user and password `testtest`, or use `REZOOM_TPU_POSTGRES` to override the
connection string.

### `src/TypeProviderUsers/test-vs-build.ps1`

Confirms the build still works in VS/MSBuild. It's very easy to write generative TP code that works in `dotnet build`
but breaks in MSBuild.

## Edit-rebuild loop for TP work

1. Edit something in `src/Rezoom.SQL.Provider/`, `Rezoom.SQL.Compiler/`, or
   `Rezoom.SQL.Mapping/`.
2. Make sure it passes the easy tests in Rezoom.SQL.Test and Rezoom.SQL.Provider.Test.
3. Run `./build/pack-dev.ps1` to generate local dev packages.
4. Run `./src/TypeProviderUsers/test-tp-users.ps1` (or `dotnet test` the specific one
   you care about). Restore picks up the new dev version automatically.
5. Also run `./src/TypeProviderUsers/test-vs-build.ps1` to confirm the TP works in a VS/msbuild environment too.

If something doesn't update as expected, the usual suspect is a stale entry
in `~/.nuget/packages/<pkg>/<version>/`. `pack-dev.ps1` clears those for the
version it's about to publish, but if you manually pack something else into
the feed, you may need to clear by hand.

## Release flow

1. Update `RezoomSqlVersion` in `version.props` for the new release.
2. Update `<PackageReleaseNotes>` in the relevant csprojs/fsprojs.
3. Update `CHANGELOG.md` (when one exists) and any docs that mention versions.
4. Commit. Working tree should be clean.
5. Run `./build/pack-release.ps1`. Verify the resulting `.nupkg`s in the feed.
6. Push to nuget.org.
7. Tag `v<version>` and push the tag.
