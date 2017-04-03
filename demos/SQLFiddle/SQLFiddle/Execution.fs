module SQLFiddle.Execution
open Rezoom

[<CompiledName("Migrate")>]
let migrate() =
    FiddleModel.Migrate(Rezoom.SQL.Migrations.MigrationConfig.Default)

[<CompiledName("Execute")>]
let execute (plan : 'a Plan) =
    Execution.execute Execution.ExecutionConfig.Default plan

