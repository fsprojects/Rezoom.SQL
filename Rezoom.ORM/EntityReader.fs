namespace Rezoom.ORM

[<AbstractClass>]
type EntityReader<'ent> =
    abstract member Next : unit -> EntityReader<'ent>
    abstract member ProcessColumns : ColumnMap -> unit
    abstract member Read : Row -> unit
    abstract member ToEntity : unit -> 'ent

[<AbstractClass>]
type EntityReaderTemplate<'ent> =
    abstract member CreateReader : unit -> 'ent EntityReader