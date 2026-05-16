# Rezoom.SQL developer notes

Notes for working on this repo.

## Prerequisites

- .NET SDK 10 or newer
- PowerShell 5.1+ on Windows; PowerShell 7+ on Linux/macOS (install: <https://learn.microsoft.com/powershell/scripting/install/installing-powershell>). The build scripts (`build/pack-dev.ps1`, `build/pack-release.ps1`) are pwsh-only.

## Repo layout assumption

The dev workflow assumes you have this repo cloned alongside its sibling
repos (most importantly [Rezoom](https://github.com/rspeele/Rezoom)) under a
common parent directory. The parent's name doesn't matter, but the
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

If you're only working on Rezoom.SQL and don't need to iterate on Rezoom,
you can use the latest published Rezoom from nuget.org. If you're changing
both, pack Rezoom into the same `.localfeed` first using whatever script that
repo provides.

## Why the unusual setup

Rezoom.SQL has two artifacts that interact awkwardly during dev:

- The **type provider** (`Rezoom.SQL.Provider`) is loaded by `fsc` at compile
  time, not at the consumer's runtime. To test a TP change, a project has to
  reference a NuGet-installed version of the provider, not a project-reference
  to its source. (Project references don't trigger the same TP loading path.)

- The **TP user smoke tests** (in `src/TypeProviderUsers/`) exercise the TP
  exactly the way a consumer would — they `PackageReference` the wrapper
  meta-packages (e.g. `Rezoom.SQL.Provider.SQLite`) which transitively bring
  in `Rezoom.SQL.Provider`. So testing a TP change means packing your changes,
  then letting the TPUs restore the new package and rebuild.

The local feed + version-bump dance encodes this. Run `build/pack-dev.ps1`
after a Provider change; the TPUs (and any consumer in the repo) automatically
restore the new version on their next build.

## Versioning during dev

All Rezoom.SQL packages share a single version. Two files compose it:

- `version.props` (committed): `RezoomSqlVersion = 0.13.0`, represents the upcoming or
  current release version. Bumped only at actual releases.
- `version.local.props` (gitignored): written by `pack-dev.ps1`, contains
  `RezoomSqlVersionSuffix = dev.N`. The combined version becomes
  `0.13.0-dev.N`. Bumped extremely frequently during development, every time we have to smoke-test the TPUs.

Every package and consumer reads these via `Directory.Build.props`. Wrapper
csprojs and TPU / demo fsprojs reference our packages as
`Version="$(RezoomSqlPkgVersion)"`, which resolves to the current dev or
release version automatically.

## Scripts

### `build/pack-dev.ps1`

Bumps the dev counter (one above the highest existing prerelease in the local
feed), writes `version.local.props`, packs all six packages. Run after any
change you want the TPUs or demos to see.

```powershell
./build/pack-dev.ps1
```

### `build/pack-release.ps1`

Deletes `version.local.props` so the build has no prerelease suffix, then
packs all six packages at the release version. Errors out if the working
tree is dirty (override with `-Force`). After it succeeds, tag and push:

```powershell
./build/pack-release.ps1
git tag v0.13.0
git push origin v0.13.0
```

Drop the `.nupkg`s into wherever your nuget.org push lives.

### `src/TypeProviderUsers/test-tp-users.ps1`

Runs `dotnet test` on both TPU projects. SQLite can make its own DB file, but Postgres auto-
skips when no server is reachable. Either set up your local Postgres like mine, with an
`rz` user and password `testtest`, or use `REZOOM_TPU_POSTGRES` to override the
connection string.

## Why the TPUs aren't in the main sln

The TPUs reference the wrapper packages via NuGet, not via project reference.
Putting them in the same sln as `Rezoom.SQL.Provider` is confusing: opening
the sln in VS suggests project-ref semantics, but the TPUs actually consume
whatever's currently packed into `.localfeed`. Changing Provider source
without running `pack-dev.ps1` would silently leave the TPUs on the old
version.

They live as standalone fsprojs under `src/TypeProviderUsers/`. Open them
individually (or via the test runner script) when you need to verify TP
behavior end-to-end.

## Edit-rebuild loop for TP work

1. Edit something in `src/Rezoom.SQL.Provider/`, `Rezoom.SQL.Compiler/`, or
   `Rezoom.SQL.Mapping/`.
2. Run `./build/pack-dev.ps1`. New version is `0.13.0-dev.<N+1>`.
3. Run `./src/TypeProviderUsers/test-tp-users.ps1` (or `dotnet test` the specific one
   you care about). Restore picks up the new dev version automatically.
4. Iterate.

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
6. Push to nuget.org via your usual mechanism.
7. Tag `v<version>` and push the tag.
