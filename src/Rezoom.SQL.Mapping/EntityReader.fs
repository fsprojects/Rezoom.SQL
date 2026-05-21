namespace Rezoom.SQL.Mapping

type ColumnId = int

// An EntityReader<Foo> is a way of building a Foo from a SQL result set.
// Note that, despite the name, 'a doesn't have to be a singular database entity like `User`.
// It can be a collection type too. In fact most of the time when we process a result set
// we will want an array of Users. That is accomplished by an EntityReader<User[]>.
// The implementation of any given EntityReader<'a> is statically built and cached in StaticEntityReaderTemplate.fs.
// That ends up making the EntityReader<User[]> (multiple) with codegen in ManyEntityColumnGenerator.fs.
// That generated code internally uses an EntityReader<User> (singular) to process each individual user.
// Codegen for *those* entity readers is in CompositeColumnGenerator.fs,
// It, in turn, uses a bunch of EntityReader<typeofthecolumn>, one for each column on the User!
// Those readers mostly have implementation by PrimitiveColumnGenerator.fs, except when we have complex
// nav-properties to handle, in which case the shape can recurse down into yet more CompositeColumnGenerators
// and ManyEntityColumnGenerators.
// In short, it's EntityReaders ALL THE WAY DOWN.

[<AbstractClass>]
type EntityReader() =
    /// Called once at beginning of processing to inform the EntityReader
    /// of the shape of the rows it's going to be given. This is the only
    /// time it gets to know what the name is of each column, when reading rows it
    /// has to access column values by integer index for XTreme 1337 h4ck3r Speed.
    abstract member ProcessColumns : ColumnMap -> unit
    /// Called once per row to supply the reader with data. Reader tracks internal state
    /// it will eventually spit out with .ToEntity() when all rows have been processed.
    abstract member Read : Row -> unit
    /// Used to help nav-properties that are bidirectional. If we have a User
    /// who has many Comments, that we are loading with our SQL MANY(...) nav prop feature,
    /// we may want each Comment to have a User property so the nav can be consumed either way.
    /// That User property pointing "back up" the hierarchy is what SetReverse is for.
    /// The parent EntityReader for User calls SetReverse(reverseColumnId, self) on its children.
    abstract member SetReverse : ColumnId * obj -> unit

[<AbstractClass>]
type EntityReader<'ent>() =
    inherit EntityReader()
    /// Used by collection-type entity readers. The reader for `User array` internally
    /// creates another new EntityReader<User>() every time it finishes building one User
    /// and starts building the next. Rather than have the next reader ProcessColumns again
    /// to know what is going on (expensive), we have the previous reader bring it up to speed
    /// by calling previousUserReader.ImpartKnowledgeToNext(nextUserReader).
    abstract member ImpartKnowledgeToNext : EntityReader<'ent> -> unit
    /// Spit out the entity we have built from the rows we have consumed.
    abstract member ToEntity : unit -> 'ent

/// No-op EntityReader that wraps an already-constructed entity.
/// ToEntity returns the wrapped value. Used by CompositeColumnGenerator's
/// generated SetReverse methods to install a parent reference into a child reader's backreference col slot.
type ObjectEntityReader<'ent>(ent : 'ent) =
    inherit EntityReader<'ent>()
    override __.ImpartKnowledgeToNext(_) = ()
    override __.ProcessColumns(_) = ()
    override __.Read(_) = ()
    override __.SetReverse(_, _) = ()
    override __.ToEntity() = ent

[<AbstractClass>]
type EntityReaderTemplate<'ent>() =
    abstract member CreateReader : unit -> 'ent EntityReader