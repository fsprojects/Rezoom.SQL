namespace Rezoom.SQL

/// Represents a type that is returned from a scalar query.
type IScalar<'a> =
    abstract member ScalarValue : 'a