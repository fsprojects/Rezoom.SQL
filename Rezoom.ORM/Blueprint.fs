namespace Rezoom.ORM
open System
open System.Reflection

/// A conversion that assumes an obj is on the stack, and pushes a value of whatever type is being
/// converted to (depends on the context in which you see the conversion).
type ConversionMethod = Emit.ILGenerator -> unit

type Setter =
    /// We initialize this column by passing it to the composite's constructor.
    | SetConstructorParameter of ParameterInfo
    /// We initialize this column by setting a property post-construction.
    | SetProperty of PropertyInfo

type Getter =
    | GetField of FieldInfo
    | GetProperty of PropertyInfo

type Column =
    {
        /// The name of this column. This is the basename of the SQL column name that
        /// will represent it.
        Name : string
        /// The blueprint for this column's type.
        Blueprint : Blueprint
        /// The way to set this column when initializing an instance of the composite type.
        Setter : Setter
        /// The way to get this column's value (could be used for analyzing expression trees).
        Getter : Getter
    }

and Composite =
    {
        /// The constructor to use when instantiating this composite type.
        /// All parameters must be supplied by columns.
        Constructor : ConstructorInfo
        /// The identity column for this composite type, if any.
        Identity : Column option
        /// All the columns of this composite type (including the identity, if any).
        Columns : Column array
    }

and Primitive =
    {
        /// A method converting an object to the output type.
        Converter : ConversionMethod
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

and Cardinality =
    | Single
    /// Carries a method converting a List<Builder<ElementType>> to the target collection type.
    | Many of ConversionMethod

and Blueprint =
    {
        Element : ElementBlueprint
        Cardinality : Cardinality
        /// The type this blueprint specifies how to construct.
        /// If x.Cardinality = Single, x.Output will match x.Element.Output.
        Output : Type
    }
