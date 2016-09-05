namespace Rezoom.ORM.SQLProvider
open System
open System.Collections.Generic

type ILanguageStatement =
    abstract member ModelChange : IModel option
    abstract member ResultSets : ISchemaQuery IReadOnlyList
    abstract member TablesRead : ISchemaTable IReadOnlyList
    abstract member TablesWritten : ISchemaTable IReadOnlyList
    abstract member Parameters : (BindParameter * ColumnType) IReadOnlyList