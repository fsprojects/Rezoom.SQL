module Rezoom.ORM.Linq.Query
open Rezoom.ORM
open Rezoom.ORM.Linq
open Rezoom.ORM.Linq.Relational
open System
open System.Reflection
open System.Linq.Expressions

let private mainTableAlias = "t"

let table<'a> =
    let blueprint = Blueprint.ofType typeof<'a>
    let shape = blueprint.Cardinality.Element.Shape
    match shape with
    | Primitive _ -> 
        failwithf
            "Can't guess the table that corresponds to a primitve type (%O)"
            blueprint.Output
    | Composite composite ->
        let table =
            {   TableName = composite.TableName
                AsAlias = [ mainTableAlias ]
            }
        let selections =
            [ for column in composite.Columns.Values do
                match column.Blueprint.Value.Cardinality.Element.Shape with
                | Composite _ -> ()
                | Primitive _ ->
                    yield
                        {   Select = ColumnExpr (mainTableAlias, column.Name)
                            AsAlias = None
                        }
            ]
        {   Selections = selections
            Inclusions = Set.empty
            FromTable = table
            Joins = Map.empty
            Predicates = []
            OrderBy = []
        }

let rec private makeExpression (expr : Expression) =
    match expr with
    | :? BinaryExpression as binary -> makeBinaryExpression binary
    | :? UnaryExpression as unary -> makeUnaryExpression unary
    | :? ConstantExpression as constant -> LiteralExpr constant.Value
    | :? MemberExpression as mem -> makeMemberExpression mem

and makeMemberExpression (mem : MemberExpression) =
    match mem.Expression with
    | null ->
        ParameterExpr <|
        match mem.Member with
        | :? PropertyInfo as prop -> prop.GetValue(null)
        | :? FieldInfo as field -> field.GetValue(null)
        | _ -> failwithf "Unsupported static member type %O" (mem.Member.GetType())
    | parent ->
        let blueprint = Blueprint.ofType parent.Type
        match blueprint.Cardinality with
        | One { Shape = Composite composite } ->
            match composite.ColumnByGetter(mem.Member) with
            | None -> failwith "what"
            | Some column ->
                match parent with
                | :? ParameterExpression -> ColumnExpr (mainTableAlias, column.Name)
                | _ -> failwith "nimpl"
        | _ -> failwith "huh"

and makeUnaryExpression (unary : UnaryExpression) =
    let operator =
        match unary.NodeType with
        | ExpressionType.Not -> Not
        | ExpressionType.Negate -> Negate
        | ExpressionType.OnesComplement -> BitComplement
        | n -> failwithf "Unsupported unary operator node type %O" n
    UnaryExpr (operator, makeExpression unary.Operand)

and makeBinaryExpression (binary : BinaryExpression) =
    let operator =
        match binary.NodeType with
        | ExpressionType.Add -> Add
        | ExpressionType.Subtract -> Sub
        | ExpressionType.Multiply -> Mul
        | ExpressionType.Divide -> Div
        | ExpressionType.Equal -> Equal
        | ExpressionType.NotEqual -> NotEqual
        | ExpressionType.GreaterThan -> GreaterThan
        | ExpressionType.GreaterThanOrEqual -> GreaterThanOrEqual
        | ExpressionType.LessThan -> LessThan
        | ExpressionType.LessThanOrEqual -> LessThanOrEqual
        | ExpressionType.AndAlso -> And
        | ExpressionType.OrElse -> Or
        | ExpressionType.And -> BitAnd
        | ExpressionType.Or -> BitOr
        | ExpressionType.ExclusiveOr -> BitXor
        | ExpressionType.LeftShift -> BitShiftL
        | ExpressionType.RightShift -> BitShiftR
        | n -> failwithf "Unsupported binary operator node type %O" n
    BinaryExpr (operator, makeExpression binary.Left, makeExpression binary.Right)