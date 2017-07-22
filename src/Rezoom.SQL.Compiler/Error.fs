module Rezoom.SQL.Compiler.Error

let parseError msg =
    sprintf "SQ000: %O" msg
let cannotUnify left right =
    sprintf "SQ001: The types %O and %O cannot be unified" left right
let reservedKeywordAsName keyword =
    sprintf "SQ002: Reserved keyword ``%O`` used as name" keyword
let noSuchFunction func =
    sprintf "SQ003: No such function: ``%O``" func
let insufficientArguments func got expected =
    sprintf "SQ004: Insufficient arguments to function ``%O`` (found %d, expected at least %d)"
        func got expected
let excessiveArguments func got expected =
    sprintf "SQ005: Too many arguments to function ``%O`` (found %d, expected at most %d)"
        func got expected
let functionDoesNotPermitWildcard func =
    sprintf "SQ006: Function ``%O`` cannot take a wildcard (*) argument" func
let jamesBond, jamesBondEasterEgg =
    "SQ007: Expected martini shaken (found ``stirred``)", "CREATE VIEW TO A KILL"
let functionDoesNotPermitDistinct func =
    sprintf "SQ008: Function ``%O`` cannot take a DISTINCT argument" func
let mismatchedColumnNameCount names cols =
    sprintf "SQ009: %d columns named for a query for %d columns" names cols
let schemaNameInColumnReference name =
    sprintf "SQ010: Unsupported schema name in column reference: ``%O``" name
let noSuchObject ty name =
    sprintf "SQ011: No such %s: ``%O``" ty name
let noSuchTable name = noSuchObject "table" name
let objectNotATable name =
    sprintf "SQ012: Object ``%O`` is not a table" name
let objectAlreadyExists name =
    sprintf "SQ013: Object ``%O`` already exists" name
let objectIsNotA ty name =
    sprintf "SQ014: Object ``%O`` is not a %s" name ty
let noSuchTableInFrom name =
    sprintf "SQ015: No such table in FROM clause: ``%O``" name
let noSuchColumn name =
    sprintf "SQ016: No such column: ``%O``" name
let noSuchColumnInFrom name =
    sprintf "SQ017: No such column in FROM clause: ``%O``" name
let columnAlreadyExists name =
    sprintf "SQ018: Column ``%O`` already exists" name
let noSuchColumnToSet tbl col =
    sprintf "SQ019: No such column in table ``%O`` to set: ``%O``" tbl col
let noSuchSchema schema =
    sprintf "SQ020: No such schema: ``%O``" schema
let ambiguousColumn name =
    sprintf "SQ021: Ambiguous column: ``%O``" name
let ambiguousColumnBetween name tbl1 tbl2 =
    sprintf "SQ022: Ambiguous column: ``%O`` (may refer to %O.%O or %O.%O)"
        name tbl1 name tbl2 name
let tableNameAlreadyInScope name =
    sprintf "SQ023: Table name already in scope: ``%O``" name
let columnReferenceWithoutFrom name =
    sprintf "SQ024: Cannot reference column name ``%O`` in query without a FROM clause" name
let multipleColumnsForInSelect count =
    sprintf "SQ025: Expected 1 column for IN(SELECT ...), but found %d" count
let multipleColumnsForScalarSubquery count =
    sprintf "SQ026: Expected 1 column for scalar subquery, but found %d" count
let subqueryRequiresAnAlias =
    sprintf "SQ027: This subquery must be given an alias"
let expressionRequiresAlias =
    sprintf "SQ028: Expression-valued column requires an alias (what should the column name be?)"
let tableWildcardWithoutFromClause name =
    sprintf "SQ029: SELECT statement must have a FROM clause to use ``%O.*``" name
let wildcardWithoutFromClause =
    sprintf "SQ030: SELECT statement must have a FROM clause to use ``*``"
let navPropertyMissingKeys name =
    sprintf "SQ031: The navigation property clause ``%O`` must contain at least one key column" name
let expectedKnownColumnCount got expected =
    sprintf "SQ032: Expected %d columns in table, but found %d" expected got
let valuesRequiresKnownShape =
    sprintf "SQ033: A VALUES() clause can only be used when column names are implied by the surrounding context"
let columnNotAggregated =
    "SQ034: Can't reference column outside of an aggregate function"
    + " because this query uses aggregate functions without a GROUP BY clause"
let columnNotGroupedBy =
    "SQ035: Can't reference column outside of an aggregate function"
    + " because the GROUP BY clause does not include this column"
let indexSchemasMismatch indexName tableName =
    sprintf "SQ036: Can't create index ``%O`` in a different schema from its table ``%O``" indexName tableName
let vendorMismatch got expected =
    sprintf "Vendor-specific code for ``%O`` cannot be compiled for backend ``%O``" got expected
let sameVendorDelimiters delim =
    sprintf "SQ037: Opening and closing delimiters for vendor statement are identical ``%s``" delim
(* let exprMustBeNullable =
    sprintf "SQ038: Expression is not nullable; but is required to be in this context" *)
let aggregateInWhereClause =
    sprintf "SQ039: A WHERE clause cannot contain aggregates -- consider using a HAVING clause"
let insertMissingColumns (missing : 'a seq) =
    sprintf "SQ040: INSERT statement missing columns: ``%s``"
        (missing |> Seq.map (fun x -> (x :> obj).ToString()) |> String.concat "``, ``")
let insertIntoNonTable =
    sprintf "SQ041: INSERT into non-tables (including views) is not supported"
let minorMigrationContainsDestruction =
    sprintf "SQ042: The migration ``%s`` contains destructive statements, so it must increment the version number"
let migrationFileNameWrong =
    sprintf "SQ043: The file ``%s`` seems to be intended to be a migration, but its filename does not match the rules"
let migrationContainsParameter =
    sprintf "SQ044: The file ``%s`` contains a parameterized statement, but migrations cannot be parameterized"
let commandContainsTooManyResultSets =
    sprintf "SQ045: The command returns too many result sets (%d) to provide types for"
let commandChangesSchema =
    sprintf "SQ046: The command includes a change to the database schema which it does not undo"
let commandLeavesTempTable = // special case of SQ046
    sprintf "SQ047: The command creates a temp table and does not drop it"
let constraintAlreadyExists name =
    sprintf "SQ048: Constraint ``%O`` already exists" name
let indexAlreadyExists name =
    sprintf "SQ049: Index ``%O`` already exists" name
let tableIsReferencedByFKs name referencing =
    sprintf
        "SQ050: The table ``%O`` cannot be dropped because it is referenced by other tables %s" 
        name
        (String.concat ", " (referencing |> Seq.map (sprintf "``%O``")))
let columnIsReferencedByConstraints name referencing =
    sprintf
        "SQ051: The column ``%O`` cannot be dropped because it is referenced by constraints %s"
        name
        (String.concat ", " (referencing |> Seq.map (sprintf "``%O``")))