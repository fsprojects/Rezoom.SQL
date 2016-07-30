namespace Rezoom.ORM

[<AbstractClass>]
type EntityReader<'ent> =
    abstract member ProcessColumns : ColumnMap -> unit
    abstract member ProcessRow : Row -> unit
    abstract member ToEntity : unit -> 'ent