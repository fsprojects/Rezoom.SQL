namespace Rezoom.ORM

type IEntityReader<'ent> =
    abstract member ToEntity : unit -> 'ent

