# Changelog

## [Unreleased]

## [0.13.0] - 2026-XX-XX

The Rip Van Winkle release.

Rezoom.SQL was dormant from 2020 until this work; 0.13.0 brings it to the 2026 .NET ecosystem. There are breaking changes for anyone upgrading from 0.6.x, which is unlikely due to the
long dead time and small userbase in the first place. However, on the unlikely chance somebody *does* have a legacy project on an old disk somewhere, the changes should be straightforward to make:

- Change your app.config to an AppSettings.json
- Plug in the configuration at startup by registering a PlanExecutor with your DI context and using that to run plans.
- Or use the old Execution.execute approach, but use your DI context (IServiceProvider) in the ExecutionConfig record you pass to it.

See new docs and also check out the samples under the demos folder.

Updating the type provider to work on .NET Standard was based off work originally done by @rkosafo in the "standard" branch.

### Added
- Multi-target netstandard2.0 / net8.0 / net10.0.
- `Migrate(MigrationConfig, IServiceProvider)` for DI-friendly migrations.
- `Migrate(MigrationConfig, connectionString)` overload for standalone migrator tools (resolves [#49](https://github.com/rspeele/Rezoom.SQL/issues/49)).
- `PlanExecutor` for plan execution via DI.
- TypeProviderUser smoke tests under `src/TypeProviderUsers/`, runnable via `test-tp-users.ps1`. These exercise the actual published Provider package against live SQLite and Postgres connections.
- GitHub Actions CI workflow.
- `DEVELOPER_README.md` documenting the local-feed dev loop and pack scripts.
- Centralized `Directory.Build.props` + `version.props` / `version.local.props` so the six Rezoom.SQL packages share a single version source.
- Command type now carries its compile-time Backend as a property. This made it possible to remove the "Provider" part of runtime config since the default can be inferred from the compile-time backend.

### Changed
- Configuration: `IConfiguration` (appsettings.json, env vars, etc.) replaces `System.Configuration.ConfigurationManager`.
- `Migrate` signature: `Migrate(MigrationConfig, IServiceProvider)` instead of `Migrate(MigrationConfig)`. Need the service provider for runtime configuration now, can't use static ConfigurationManager.
- TP-generated code registers each model's default ADO.NET driver automatically, so `RezoomSQL:Providers:{name}` in `appsettings.json` is now optional. Set it only to override the canonical driver for the dialect.
- SQL Server: `Microsoft.Data.SqlClient` 5.2.2+ replaces the deprecated `System.Data.SqlClient`.
- SQLite: `Microsoft.Data.Sqlite` 8.0.10+ replaces `System.Data.SQLite.Core` for cross-platform compatibility.
- FSharp.Core baseline: 8.0.401+. Embedded `TaskBuilder.fs` dropped in favor of FSharp.Core's built-in `task { }`.
- Tutorial, configuration docs, and API reference tweaked to match the new API. `App.config` XML in samples replaced with `appsettings.json` throughout.
- Rezoom internals: eliminated custom IServiceConfig in favor of standard IServiceProvider. Renamed ServiceContext to PlanContext to make its usage more clear.

### Removed
- `System.Configuration.ConfigurationManager` runtime dependency.
- `dynamicCommand` and `connectionDynamicCommand` from `Rezoom.SQL.Raw`. Dynamic SQL use cases are best handled by `unsafe_inject_raw` inside a static `SQL<...>` query instead. See [doc/Language/DynamicSQL.md](doc/Language/DynamicSQL.md).
- The separate `TypeProviderUsers.sln` (TPUs are now standalone fsprojs).
- Travis CI workflow.

### Fixed
- Type provider loads correctly on both the .NET 10 SDK (via `AssemblyLoadContext.Resolving`) and VS2026's F# tooling (via `AppDomain.AssemblyResolve` plus a netstandard2.0 path redirect for net8.0 references).
- 3 generative TP bugs worked around.
- Fixed broken images, links in GitBooks docs.

## [0.6.2] - 2018

Last release before the NET 10 revamp. See git history for changes prior to this.

[Unreleased]: https://github.com/rspeele/Rezoom.SQL/compare/v0.13.0...HEAD
[0.13.0]: https://github.com/rspeele/Rezoom.SQL/compare/v0.6.2...v0.13.0
