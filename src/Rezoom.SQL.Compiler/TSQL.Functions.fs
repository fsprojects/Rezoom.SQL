module Rezoom.SQL.Compiler.TSQL.TSQLFunctions
open System
open System.Collections.Generic
open Rezoom.SQL.Compiler
open Rezoom.SQL.Compiler.BackendUtilities
open Rezoom.SQL.Compiler.Translators
open Rezoom.SQL.Compiler.FunctionDeclarations

type CustomTranslator = ExprTranslator -> TFunctionInvocationExpr -> Fragments

let private noArgProc name ret =
    proc name [] ret, Some <| fun _ _ -> [| text <| name.ToUpperInvariant() |] :> _ seq
let private atAtProc name ret =
    proc name [] ret, Some <| fun _ _ -> [| text <| "@@" + name.ToUpperInvariant() |] :> _ seq
let private datePartWhitelist =
    [|  "year"; "yy"; "yyyy"
        "quarter"; "qq"; "q"
        "month"; "mm"; "m"
        "dayofyear"; "dy"; "y"
        "day"; "dd"; "d"
        "week"; "wk"; "ww"
        "weekday"; "dw"
        "hour"; "hh"
        "minute"; "mi"; "n"
        "second"; "ss"; "s"
        "millisecond"; "ms"
        "microsecond"; "mcs"
        "nanosecond"; "ns"
        "tzoffset"; "tz"
        "iso_week"; "isowk"; "isoww"
    |] |> fun arr -> HashSet(arr, StringComparer.OrdinalIgnoreCase)
let private datePartFunc name otherArgs ret =
    func name (string :: otherArgs) ret,
        Some <| fun (expr : ExprTranslator) (invoc : TFunctionInvocationExpr) ->
            seq {
                yield text invoc.FunctionName.Value
                yield text "("
                match invoc.Arguments with
                | ArgumentList (None, args) when args.Length > 0 ->
                    match args.[0] with
                    | { Value = LiteralExpr (StringLiteral lit) } ->
                        if datePartWhitelist.Contains(lit) then
                            yield text lit
                        else
                            failAt args.[0].Source <|
                                sprintf "DATEPART argument must be one of %A" (List.ofSeq datePartWhitelist)
                    | _ ->
                        failAt args.[0].Source "DATEPART argument must be a string literal"
                    for i = 1 to args.Length - 1 do
                        yield text ","
                        yield! expr.Expr(args.[i], FirstClassValue)
                | _ -> bug "Can't use datePartFunc with no args"
                yield text ")"
            }
let iifCustom =
    func "iif" [ boolean; infect a'; infect a' ] a',
        Some <| fun (expr : ExprTranslator) (invoc : TFunctionInvocationExpr) ->
            match invoc.Arguments with
            | ArgumentList (None, [| cond; ifTrue; ifFalse |]) ->
                [|  yield text "IIF("
                    yield! expr.Expr(cond, Predicate)
                    yield text ","
                    yield! expr.Expr(ifTrue, FirstClassValue)
                    yield text ","
                    yield! expr.Expr(ifFalse, FirstClassValue)
                    yield text ")"
                |] :> _ seq
            | _ -> bug "Impossible arguments to iif"
let private aggregate name args ret = aggregate name args ret, None
let private aggregateW name args ret = aggregateW name args ret, None
let private func name args ret = func name args ret, None
let private proc name args ret = proc name args ret, None
let private i = integral
let private ii = infect i
let private date = datetime
let private specialFunctions = Dictionary()
let private addCustom (funcType : FunctionType, custom) =
    match custom with
    | None -> funcType
    | Some custom ->
        specialFunctions.[funcType.FunctionName] <- custom
        funcType
let getCustom (funcName : Name) =
    let succ, value = specialFunctions.TryGetValue(funcName)
    if succ then Some value else None
let functions =
    [|  // aggregate functions
        aggregate "avg" [ numeric a' ] (nullable a')
        aggregateW "count" [ scalar ] int32
        aggregateW "count_big" [ scalar ] int64
        aggregate "grouping" [ scalar ] boolean
        aggregate "grouping_id" [ vararg scalar ] int32
        aggregate "max" [ a' ] (nullable a')
        aggregate "min" [ a' ] (nullable a')
        aggregate "sum" [ numeric a' ] a'
        aggregate "stdev" [ numeric scalar ] (nullable float64)
        aggregate "stdevp" [ numeric scalar ] (nullable float64)
        aggregate "var" [ numeric scalar ] (nullable float64)
        aggregate "varp" [ numeric scalar ] (nullable float64)
        // @@FUNCTIONNAME builtins
        atAtProc "rowcount" int32
        atAtProc "datefirst" int16
        atAtProc "dbts" binary
        atAtProc "langid" int16
        atAtProc "language" string
        atAtProc "lock_timeout" int32
        atAtProc "max_connections" int32
        atAtProc "max_precision" int16
        atAtProc "nestlevel" int32
        atAtProc "options" int32
        atAtProc "remserver" string
        atAtProc "servername" string
        atAtProc "servicename" string
        atAtProc "spid" int16
        atAtProc "textsize" int32
        atAtProc "version" string
        atAtProc "cursor_rows" int32
        atAtProc "fetch_status" int32
        atAtProc "identity" i
        // identity
        proc "scope_identity" [] i
        // date/time functions from https://msdn.microsoft.com/en-us/library/ms186724.aspx
        noArgProc "current_timestamp" datetime
        proc "sysdatetime" [] datetime
        proc "sysdatetimeoffset" [] datetimeoffset
        proc "sysutcdatetime" [] datetime
        proc "getdate" [] datetime
        proc "getutcdate" [] datetime
        datePartFunc "datename" [ infect datetime ] string
        datePartFunc "dateadd" [ infect datetime ] string
        datePartFunc "datediff" [ infect datetime; infect datetime ] int32
        datePartFunc "datediff_big" [ infect datetime; infect datetime ] int64
        datePartFunc "dateadd" [ infect i; infect datetime ] datetime
        func "day" [ infect datetime ] i
        func "month" [ infect datetime ] i
        func "year" [ infect datetime ] i
        func "datefromparts" [ ii; ii; ii ] date
        func "datetime2fromparts" [ ii; ii; ii; ii; ii; ii; ii; ii ] datetime
        func "datetimefromparts" [ ii; ii; ii; ii; ii; ii; ii ] datetime
        func "datetimeoffsetfromparts" [ ii; ii; ii; ii; ii; ii; ii; ii; ii; ii ] datetimeoffset
        func "smalldatetimefromparts" [ ii; ii; ii; ii; ii ] datetime
        func "todatetimeoffset" [ infect datetime; infect scalar ] datetimeoffset
        // math funcs from https://msdn.microsoft.com/en-us/library/ms177516.aspx
        func "acos" [ infect fractional ] float64
        func "asin" [ infect fractional ] float64
        func "atan" [ infect fractional ] float64
        func "atn2" [ infect fractional; infect fractional ] float64
        func "ceiling" [ infect (numeric a') ] a'
        func "cos" [ infect fractional] float64
        func "cot" [ infect fractional ] float64
        func "degrees" [ infect (numeric a') ] a'
        func "exp" [ infect fractional ] float64
        func "floor" [ infect (numeric a') ] a'
        func "log" [ infect num; infect (optional i) ] float64
        func "log10" [ infect num ] float64
        func "pi" [] float64
        func "power" [ infect (numeric a'); infect num ] a'
        func "radians" [ infect (numeric a') ] a'
        func "rand" [ infect (optional i) ] float64
        func "round" [ infect (numeric a'); infect i ] a'
        func "sign" [ infect (numeric a') ] a'
        func "sin" [ infect fractional ] float64
        func "sqrt" [ infect (numeric a') ] float64
        func "square" [ infect (numeric a') ] float64
        func "tan" [ infect fractional ] float64
        // JSON functions from https://msdn.microsoft.com/en-us/library/dn921900.aspx
        func "isjson" [ infect string ] boolean
        func "json_value" [ infect string; infect string ] string
        func "json_query" [ infect string; infect string ] string
        func "json_modify" [ infect string; infect string; infect string ] string
        // logical funcs from https://msdn.microsoft.com/en-us/library/hh213226.aspx
        func "choose" [ infect i; vararg (infect a') ] a'
        iifCustom
        // skip over "metadata functions" (for now) from https://msdn.microsoft.com/en-us/library/ms187812.aspx
        // ...
        // also "security functions" (for now) from https://msdn.microsoft.com/en-us/library/ms186236.aspx
        // ...
        // so onto string functions from https://msdn.microsoft.com/en-us/library/ms181984.aspx
        func "ascii" [ infect string ] int32
        func "concat" [ string; string; vararg string ] string
        func "format" [ infect scalar; infect string; optional (infect string) ] string
        func "lower" [ infect string ] string
        func "upper" [ infect string ] string
        func "patindex" [ infect string; infect string ] integral
        func "replicate" [ infect string; infect integral ] string
        func "rtrim" [ infect string ] string
        func "ltrim" [ infect string ] string
        func "str" [ infect fractional; varargN 2 integral ] string
        // func "string_split" [ infect string; infect string ] string_table // wait till we can do TVFs
        func "translate" [ infect string; infect string; infect string ] string
        func "char" [ infect integral ] string
        func "concat_ws" [ infect string; scalar; scalar; vararg scalar ] string
        func "left" [ infect string; infect integral ] string
        func "right" [ infect string; infect integral ] string
        func "quotename" [ infect string; optional (infect string) ] string
        func "reverse" [ infect string ] string
        func "soundex" [ infect string ] string
        // func "string_agg" // wtf, how do we support this?d it has its own special clause type...
        func "stuff" [ infect (a' |> constrained StringishTypeClass); infect integral; infect integral; string ] a'
        func "trim" [ infect string ] string // come on TSQL, "characters from"? cut it out...
        func "charindex" [ infect string; infect string ; optional integral ] integral
        func "difference" [ infect string; infect string ] int32
        func "len" [ infect string ] integral
        func "datalength" [ infect string ] integral
        func "nchar" [ infect integral ] string
        func "replace" [ infect string; infect string; infect string ] string
        func "space" [ infect integral ] string
        func "string_escape" [ infect string; infect string ] string // TODO: enforce literal on 2nd arg?
        func "substring" [ infect a' |> constrained StringishTypeClass; infect integral; infect integral ] a'
        func "unicode" [ infect string ] int32
        // guid functions
        proc "newid" [] guid
        proc "newsequentialid" [] guid
        // missing: system functions, system statistical functions, text and image functions
    |] |> Array.map addCustom |> DefaultFunctions.extendedBy
