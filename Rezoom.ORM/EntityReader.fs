namespace Rezoom.ORM

type ColumnId = int

[<AbstractClass>]
type EntityReader<'ent>() =
    abstract member ImpartKnowledgeToNext : EntityReader<'ent> -> unit
    abstract member ProcessColumns : ColumnMap -> unit
    abstract member Read : Row -> unit
    abstract member SetReverse : ColumnId * obj -> unit
    abstract member ToEntity : unit -> 'ent

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