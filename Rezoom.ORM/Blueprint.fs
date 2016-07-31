namespace Rezoom.ORM
open System
open System.Collections.Generic
open System.Reflection

type Setter =
    /// We initialize this column by passing it to the composite's constructor.
    | SetConstructorParameter of ParameterInfo
    /// We initialize this column by setting a field post-construction.
    | SetField of FieldInfo
    /// We initialize this column by setting a property post-construction.
    | SetProperty of PropertyInfo

type Getter =
    | GetField of FieldInfo
    | GetProperty of PropertyInfo
    member this.MemberInfo =
        match this with
        | GetField f -> f :> MemberInfo
        | GetProperty p -> p :> MemberInfo

type Column =
    {
        /// The name of this column. This is the basename of the SQL column name that
        /// will represent it. This should always be treated case-insensitively.
        Name : string
        /// The blueprint for this column's type.
        Blueprint : Blueprint Lazy
        /// The way to set this column when initializing an instance of the composite type.
        Setter : Setter
        /// The way to get this column's value (could be used for analyzing expression trees).
        Getter : Getter option
    }

and Composite =
    {
        /// The constructor to use when instantiating this composite type.
        /// All parameters must be supplied by columns.
        Constructor : ConstructorInfo
        /// The identity column for this composite type, if any.
        Identity : Column option
        /// All the columns of this composite type (including the identity, if any).
        /// Indexed by name, case insensitive.
        Columns : IReadOnlyDictionary<string, Column>
    }

and Primitive =
    {
        /// A method converting an object to the output type.
        Converter : RowConversionMethod
    }

and BlueprintShape =
    | Primitive of Primitive
    | Composite of Composite

and ElementBlueprint =
    {
        Shape : BlueprintShape
        /// The element type this blueprint specifies how to construct.
        Output : Type
    }
    member this.IsOne =
        match this.Shape with
        | Primitive _ -> true
        | Composite c ->
            c.Columns.Values
            |> Seq.forall (fun c -> c.Blueprint.Value.IsOne)

and Cardinality =
    | One of ElementBlueprint
    /// Carries an element type blueprint and a method converting an ICollection<Builder<ElementType>>
    /// to the target collection type.
    | Many of ElementBlueprint * ConversionMethod

and Blueprint =
    {
        Cardinality : Cardinality
        /// The type (possibly a collectiont ype) this blueprint specifies how to construct.
        Output : Type
    }
    member this.IsOne =
        match this.Cardinality with
        | One e -> e.IsOne
        | Many _ -> false
