namespace Rezoom.ORM.SQLProvider
open System
open System.Collections.Generic

type InferredTypeCore =
    | AnyType
    | OneOfTypes of CoreColumnType Set
    | ExactlyType of CoreColumnType

type InferredType =
    {
        Inferred : InferredTypeCore
        Nullable : bool option
    }

type Parameter =
    abstract member ParameterName : string
    abstract member InferredType : InferredType

type ILanguageStatement =
    abstract member ModelChange : IModel option
    abstract member ResultSets : ISchemaQuery IReadOnlyList
    abstract member TablesModified : ISchemaTable IReadOnlyList
    abstract member Parameters : Parameter IReadOnlyList