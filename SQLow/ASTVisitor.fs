namespace SQLow
open System
open System.Collections.Generic
open System.Globalization

[<AbstractClass>]
type ASTVisitor<'t1, 'e1, 't2, 'e2>() =
    abstract member VisitLiteral : Literal -> Literal
    default __.VisitLiteral x = x
    abstract member VisitBindParameter : BindParameter -> BindParameter
    default __.VisitBindParameter x = x
    abstract member VisitColumnName : ColumnName<'t1> -> ColumnName<'t2>
    abstract member VisitCast : CastExpr<'t1, 'e1> -> CastExpr<'t2, 'e2>
    // todo: everything else