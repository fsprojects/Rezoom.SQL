module private StaticQL.Parser
open System
open System.Collections.Generic
open System.Globalization
open FParsec
open FParsec.Pipes
open FParsec.Pipes.Precedence

/// Get the source position the parser is currently at.
let private sourcePosition =
    %% +.p<Position>
    -%> translatePosition

/// Wraps any parser with source information.
let private withSource (parser : Parser<'a, unit>) =
    %% +.sourcePosition
    -- +.parser
    -- +.sourcePosition
    -%> fun startPos value endPos ->
        {   WithSource.Source = { StartPosition = startPos; EndPosition = endPos }
            Value = value
        }

/// A line comment begins with -- and continues through the end of the line.
let private lineComment =
    %% "--" -- restOfLine true -|> ()

/// A block comment begins with /* and continues until a trailing */ is found.
/// Nested block comments are not allowed, so additional /* tokens found
/// after the first are ignored.
let private blockComment =
    %% "/*" -- skipCharsTillString "*/" true Int32.MaxValue -|> ()

/// Where whitespace is expected, it can be one of...
let private whitespaceUnit =
    %[  lineComment // a line comment
        blockComment // a block comment
        spaces1 // one or more whitespace characters
    ]

/// Optional whitespace: 0 or more whitespace units
let private ws = skipMany whitespaceUnit

/// Add optional trailing whitespace to a parser.
let inline private tws parser = %parser .>> ws

/// Required whitespace: 1 or more whitespace units
let private ws1 = skipMany1 whitespaceUnit

/// A name wrapped in double quotes (standard SQL).
let private quotedName =
    let escapedQuote =
        %% "\"\"" -|> "\"" // A pair of double quotes escapes a double quote character
    let regularChars =
        many1Satisfy ((<>) '"') // Any run of non-quote characters is literal
    %% '"' -- +.([regularChars; escapedQuote] * qty.[0..]) -- '"'
    -|> (String.Concat >> Name) // Glue together the parts of the string

/// A name wrapped in square brackets (T-SQL style).
let private bracketedName =
    let escapedBracket =
        %% "]]" -|> "]" // A pair of right brackets escapes a right bracket character
    let regularChars =
        many1Satisfy ((<>) ']') // Any run of non-bracket characters is literal
    %% '[' -- +.([regularChars; escapedBracket] * qty.[0..]) -- ']'
    -|> (String.Concat >> Name)

/// A name wrapped in backticks (MySQL style)
let private backtickedName =
    let escapedTick =
        %% "``" -|> "`" // A pair of backticks escapes a backtick character
    let regularChars =
        many1Satisfy ((<>) '`') // Any run of non-backtick characters is literal
    %% '`' -- +.([regularChars; escapedTick] * qty.[0..]) -- '`'
    -|> (String.Concat >> Name)

let private sqlKeywords =
    [   "ADD"; "ALL"; "ALTER";
        "AND"; "AS";
        "BETWEEN"; "CASE"; "CHECK"; "COLLATE";
        "COMMIT"; "CONFLICT"; "CONSTRAINT"; "CREATE"; "CROSS";
        "DEFAULT"; "DEFERRABLE"; "DELETE";
        "DISTINCT"; "DROP"; "ELSE"; "ESCAPE"; "EXCEPT";
        "EXISTS"; "FOREIGN"; "FROM";
        "FULL"; "GLOB"; "GROUP"; "HAVING"; "IN";
        "INNER"; "INSERT";
        "INTERSECT"; "INTO"; "IS"; "ISNULL"; "JOIN"; "LEFT";
        "LIMIT"; "NATURAL"; "NOT"; "NOTNULL"; "NULL";
        "ON"; "OR"; "ORDER"; "OUTER"; "PRIMARY";
        "REFERENCES";
        "RIGHT";
        "SELECT"; "SET"; "TABLE"; "THEN";
        "TO"; "TRANSACTION"; "UNION"; "UNIQUE"; "UPDATE"; "USING";
        "VALUES"; "WHEN"; "WHERE";
        // Note: we don't include TEMP in this list because it is a schema name.
    ] |> fun kws ->
        HashSet<string>(kws, StringComparer.OrdinalIgnoreCase)
        // Since SQL is case-insensitive, be sure to ignore case
        // in this hash set.

let private isInitialIdentifierCharacter c =
    c = '_'
    || c >= 'a' && c <= 'z'
    || c >= 'A' && c <= 'Z'

let private isFollowingIdentifierCharacter c =
    isInitialIdentifierCharacter c
    || c >= '0' && c <= '9'
    || c = '$'

let private unquotedNameOrKeyword =
    many1Satisfy2 isInitialIdentifierCharacter isFollowingIdentifierCharacter
    |>> Name

/// A plain, unquoted name.
let private unquotedName =
    unquotedNameOrKeyword >>=? fun ident ->
        if sqlKeywords.Contains(ident.ToString()) then
            fail (sprintf "Reserved keyword %O used as name" ident)
        else
            preturn ident

let private name =
    %[  quotedName
        bracketedName
        backtickedName
        unquotedName
    ]

let private stringLiteral =
   (let escapedQuote =
        %% "''" -|> "'" // A pair of single quotes escapes a single quote character
    let regularChars =
        many1Satisfy ((<>) '\'') // Any run of non-quote characters is literal
    %% '\'' -- +.([regularChars; escapedQuote] * qty.[0..]) -- '\''
    -|> String.Concat)
    <?> "string-literal"

let private nameOrString =
    %[  name
        %% +.stringLiteral -|> Name
    ]

let private nameOrKeyword =
    %[  quotedName
        bracketedName
        backtickedName
        unquotedNameOrKeyword
        %% +.stringLiteral -|> Name
    ]

let private objectName =
    (%% +.sourcePosition 
    -- +.nameOrKeyword
    -- ws
    -- +.(zeroOrOne * (%% '.' -- ws -? +.nameOrKeyword -- ws -|> id))
    -- +.sourcePosition
    -|> fun pos1 name1 name2 pos2 ->
        let pos = { StartPosition = pos1; EndPosition = pos2 }
        match name2 with
        | None ->
            { Source = pos; SchemaName = None; ObjectName = name1; Info = () }
        | Some name2 ->
            { Source = pos; SchemaName = Some name1; ObjectName = name2; Info = () })
    <?> "object-name"

let private columnName =
    (qty.[1..3] / tws '.' * tws name
    |> withSource
    |>> fun { Value = names; Source = src } ->
        match names.Count with
        | 1 -> { Table = None; ColumnName = names.[0] }
        | 2 ->
            {   Table = Some { Source = src; SchemaName = None; ObjectName = names.[0]; Info = () }
                ColumnName = names.[1]
            }
        | 3 ->
            {   Table = Some { Source = src; SchemaName = Some names.[0]; ObjectName = names.[1]; Info = () }
                ColumnName = names.[2]
            }
        | _ -> failwith "Unreachable")
    <?> "column-name"

let private qualifiedColumnName =
   (%% +.withSource nameOrString
    -- ws
    -? '.'
    -- +.(qty.[0..2] / tws '.' * tws nameOrString)
    -|> fun initial rest ->
        match rest.Count with
        | 0 -> { Table = None; ColumnName = initial.Value }
        | 1 ->
            {   Table = Some { Source = initial.Source; SchemaName = None; ObjectName = initial.Value; Info = () }
                ColumnName = rest.[0]
            }
        | 2 ->
            {   Table =
                    Some { Source = initial.Source; SchemaName = Some initial.Value; ObjectName = rest.[0]; Info = () }
                ColumnName = rest.[1]
            }
        | _ -> failwith "Unreachable")
    <?> "qualified-column-name"

let private namedBindParameter =
    %% '@'
    -- +.unquotedNameOrKeyword
    -|> fun name -> NamedParameter name

let private bindParameter = namedBindParameter <?> "bind-parameter"

let private kw str =
    %% ci str
    -? notFollowedByL (satisfy isFollowingIdentifierCharacter) str
    -- ws
    -|> ()

let private nullLiteral =
    %% kw "NULL" -|> NullLiteral

let private currentTimeLiteral =
    %% kw "CURRENT_TIME" -|> CurrentTimeLiteral

let private currentDateLiteral =
    %% kw "CURRENT_DATE" -|> CurrentDateLiteral

let private currentTimestampLiteral =
    %% kw "CURRENT_TIMESTAMP" -|> CurrentTimestampLiteral

let private blobLiteral =
    let octet =
        %% +.(qty.[2] * hex)
        -|> fun pair -> Byte.Parse(String(pair), NumberStyles.HexNumber)
    (%% ['x';'X']
    -? '\''
    -- +.(octet * qty.[0..])
    -- '\''
    -|> (Seq.toArray >> BlobLiteral))
    <?> "blob-literal"

let private numericLiteral =
    let options =
        NumberLiteralOptions.AllowHexadecimal
        ||| NumberLiteralOptions.AllowFraction
        ||| NumberLiteralOptions.AllowFractionWOIntegerPart
        ||| NumberLiteralOptions.AllowExponent
    numberLiteral options "numeric-literal" >>= fun lit ->
        if lit.IsInteger then
            lit.String |> uint64 |> IntegerLiteral |> preturn
        else if lit.IsHexadecimal then
            fail "hexadecimal floats are not permitted"
        else 
            lit.String |> float |> FloatLiteral |> preturn

let private signedNumericLiteral =
    let sign =
        %[  %% '+' -|> 1
            %% '-' -|> -1
            preturn 0
        ]
    %% +.sign
    -- ws
    -- +.numericLiteral
    -|> fun sign value -> { Sign = sign; Value = value }

let private literal =
    %[  %% +.numericLiteral -|> NumericLiteral
        %% +.stringLiteral -|> StringLiteral
        blobLiteral
        nullLiteral
        currentTimeLiteral
        currentDateLiteral
        currentTimestampLiteral
    ]

let private typeName =
    let maxBound = %% '(' -- ws -- +.p<int> -- ws -- ')' -- ws -%> id
    %[  %% kw "STRING" -- +.(zeroOrOne * maxBound) -%> StringTypeName
        %% kw "BINARY" -- +.(zeroOrOne * maxBound) -%> StringTypeName
        %% kw "INT8" -%> IntegerTypeName Integer8
        %% kw "INT16" -%> IntegerTypeName Integer16
        %% kw "INT32" -%> IntegerTypeName Integer32
        %% kw "INT64" -%> IntegerTypeName Integer64
        %% kw "INT" -%> IntegerTypeName Integer32
        %% kw "FLOAT32" -%> FloatTypeName Float32
        %% kw "FLOAT64" -%> FloatTypeName Float64
        %% kw "FLOAT" -%> FloatTypeName Float64
        %% kw "DECIMAL" -%> DecimalTypeName
        %% kw "BOOL" -%> BooleanTypeName
        %% kw "DATETIME" -%> DateTimeTypeName
        %% kw "DATETIMEOFFSET" -%> DateTimeOffsetTypeName
    ]

let private cast expr =
    %% kw "CAST"
    -- '('
    -- ws
    -- +.expr
    -- kw "AS"
    -- +. typeName
    -- ws
    -- ')'
    -|> fun ex typeName -> { Expression = ex; AsType = typeName }

let private functionArguments (expr : Parser<Expr<unit, unit>, unit>) =
    %[  %% '*' -- ws -|> ArgumentWildcard
        %% +.((%% kw "DISTINCT" -- ws -|> Distinct) * zeroOrOne)
        -- +.(qty.[0..] / tws ',' * expr)
        -|> fun distinct args -> ArgumentList (distinct, args)
    ]

let private functionInvocation expr =
    %% +.nameOrKeyword
    -- ws
    -? '('
    -- ws
    -- +.functionArguments expr
    -- ')'
    -|> fun name args -> { FunctionName = name; Arguments = args }

let private case expr =
    let whenClause =
        %% kw "WHEN"
        -- +.expr
        -- kw "THEN"
        -- +.expr
        -%> auto
    let elseClause =
        %% kw "ELSE"
        -- +.expr
        -|> id
    let whenForm =
        %% +.(whenClause * qty.[1..])
        -- +.withSource (elseClause * zeroOrOne)
        -- kw "END"
        -|> fun cases els -> { Input = None; Cases = cases; Else = els }
    let ofForm =
        %% +.expr
        -- +.whenForm
        -|> fun ofExpr case -> { case with Input = Some ofExpr }
    %% kw "CASE"
    -- +.[ whenForm; ofForm ]
    -|> id

let private expr, private exprImpl = createParserForwardedToRef<Expr<unit, unit>, unit>()
let private selectStmt, private selectStmtImpl = createParserForwardedToRef<SelectStmt<unit, unit>, unit>()

let private binary op e1 e2 =
    {   Expr.Value = BinaryExpr { BinaryExpr.Operator = op; Left = e1; Right = e2 }
        Source = SourceInfo.Between(e1.Source, e2.Source)
        Info = ()
    }    

let private unary op e1 =
    {   Expr.Value = UnaryExpr { UnaryExpr.Operator = op; Operand = e1 }
        Source = e1.Source
        Info = ()
    }

let private tableInvocation =
    let args =
        %% '(' -- ws -- +.(qty.[0..] / tws ',' * expr) -- ')' -|> id
    %% +.objectName
    -- ws
    -- +.(args * zeroOrOne)
    -|> fun name args -> { Table = name; Arguments = args }

let private collateOperator =
    %% kw "COLLATE"
    -- +.withSource name
    -|> fun collation expr ->
        {   Expr.Value = CollateExpr { Input = expr; Collation = collation.Value }
            Source = collation.Source
            Info = ()
        }

let private isOperator =
    %% kw "IS"
    -- +.(zeroOrOne * kw "NOT")
    -|> function
    | Some () -> binary IsNot
    | None -> binary Is

let private inOperator =
    %% +.(zeroOrOne * kw "NOT")
    -? +.withSource (kw "IN")
    -- +.withSource
            %[  %% '('
                -- ws
                --
                    +.[
                        %% +.selectStmt -|> InSelect
                        %% +.(qty.[0..] / tws ',' * expr) -|> InExpressions
                    ]
                -- ')'
                -|> id
                %% +.tableInvocation -|> InTable
            ]
    -|> fun invert op inSet left ->
        {   Expr.Source = op.Source
            Value = InExpr { Invert = Option.isSome invert; Input = left; Set = inSet }
            Info = ()
        }

let private similarityOperator =
    let similar invert (op : SimilarityOperator WithSource) left right escape =
        {   Expr.Source = op.Source
            Value =
                {   Invert = Option.isSome invert
                    Operator = op.Value
                    Input = left
                    Pattern = right
                    Escape = escape
                } |> SimilarityExpr
            Info = ()
        }
    let op =
        %[  %% kw "LIKE" -|> Like
            %% kw "GLOB" -|> Glob
            %% kw "MATCH" -|> Match
            %% kw "REGEXP" -|> Regexp
        ] |> withSource
    %% +.(zeroOrOne * kw "NOT")
    -? +.op
    -|> similar

let private notNullOperator =
    %[
        kw "NOTNULL"
        %% kw "NOT" -? kw "NULL" -|> ()
    ]
    |> withSource
    |>> fun op left ->
        {   Expr.Source = op.Source
            Value = UnaryExpr { Operator = NotNull; Operand = left }
            Info = ()
        }

let private betweenOperator =
    let between invert input low high =
        {   Invert = Option.isSome invert
            Input = input
            Low = low
            High = high
        }
    %% +.(zeroOrOne * kw "NOT")
    -? +.withSource (kw "BETWEEN")
    -|> fun invert op input low high ->
        {   Expr.Source = op.Source
            Value = BetweenExpr (between invert input low high)
            Info = ()
        }

let private raiseTrigger =
    %% kw "RAISE"
    -- '('
    -- ws
    -- +.[  %% kw "IGNORE" -|> RaiseIgnore
            %% kw "ROLLBACK" -- ',' -- ws -- +.stringLiteral -- ws -|> RaiseRollback
            %% kw "ABORT" -- ',' -- ws -- +.stringLiteral -- ws -|> RaiseAbort
            %% kw "FAIL" -- ',' -- ws -- +.stringLiteral -- ws -|> RaiseFail
        ]
    -- ')'
    -|> RaiseExpr

let private term (expr : Parser<Expr<unit, unit>, unit>) =
    let parenthesized =
        %[
            %% +.selectStmt -|> ScalarSubqueryExpr
            %% +.expr -|> fun e -> e.Value
        ]
    %% +.sourcePosition
    -- +.[
            %% '(' -- ws -- +.parenthesized -- ')' -|> id
            %% kw "EXISTS" -- ws -- '(' -- ws -- +.selectStmt -- ')' -|> ExistsExpr
            %% +.qualifiedColumnName -|> ColumnNameExpr
            %% +.literal -|> LiteralExpr
            %% +.bindParameter -|> BindParameterExpr
            %% +.cast expr -|> CastExpr
            %% +.case expr -|> CaseExpr
            raiseTrigger
            %% +.functionInvocation expr -|> FunctionInvocationExpr
            %% +.columnName -|> ColumnNameExpr
        ]
    -- +.sourcePosition
    -%> fun startPos value endPos ->
        {   Expr.Value = value
            Source = { StartPosition = startPos; EndPosition = endPos }
            Info = ()
        }

let private operators = [
    [
        postfixc collateOperator
    ]
    [
        prefix (kw "NOT") <| unary Not
        prefix '~' <| unary BitNot
        prefix '-' <| unary Negative
        prefix '+' id
    ]
    [
        infixl "||" <| binary Concatenate
    ]
    [
        infixl '*' <| binary Multiply
        infixl '/' <| binary Divide
        infixl '%' <| binary Modulo
    ]
    [
        infixl '+' <| binary Add
        infixl '-' <| binary Subtract
    ]
    [
        infixl "<<" <| binary BitShiftLeft
        infixl ">>" <| binary BitShiftRight
        infixl '&' <| binary BitAnd
        infixl '|' <| binary BitOr
    ]
    [
        infixl ">=" <| binary GreaterThanOrEqual
        infixl "<=" <| binary LessThanOrEqual
        infixl (%% '<' -? notFollowedBy (skipChar '>') -|> ()) <| binary LessThan
        infixl '>' <| binary GreaterThan
    ]
    [
        infixl "==" <| binary Equal
        infixl "=" <| binary Equal
        infixl "!=" <| binary NotEqual
        infixl "<>" <| binary NotEqual
        infixlc isOperator
        ternaryolc similarityOperator (kw "ESCAPE")
        postfix (kw "ISNULL") <| unary IsNull
        postfixc notNullOperator
        postfixc inOperator
        ternarylc betweenOperator (kw "AND")
    ]
    [
        infixl (kw "AND") <| binary And
    ]
    [
        infixl (kw "OR") <| binary Or
    ]
]

do
    exprImpl :=
        {   Whitespace = ws
            Term = term
            Operators = operators    
        } |> Precedence.expression

let private parenthesizedColumnNames =
    %% '('
    -- ws
    -- +.(qty.[0..] / tws ',' * tws nameOrString)
    -- ')'
    -- ws
    -|> id

let private commonTableExpression =
    %% +.nameOrKeyword
    -- ws
    -- +.(zeroOrOne * withSource parenthesizedColumnNames)
    -- kw "AS"
    -- '('
    -- +.selectStmt
    -- ')'
    -- ws
    -|> fun table cols asSelect ->
        { Name = table; ColumnNames = cols; AsSelect = asSelect; Info = () }

let private withClause =
    %% kw "WITH"
    -- +.(zeroOrOne * tws (kw "RECURSIVE"))
    -- +.(qty.[1..] / tws ',' * commonTableExpression)
    -|> fun recurs ctes ->
        { Recursive = Option.isSome recurs; Tables = ctes }

let private asAlias =
    %% (zeroOrOne * kw "AS")
    -? +.nameOrString
    -|> id

let private resultColumn =
    %% +.[
        %% '*' -|> ColumnsWildcard
        %% +.name -- '.' -? '*' -|> TableColumnsWildcard
        %% +.expr -- +.(asAlias * zeroOrOne) -|> fun ex alias -> Column (ex, alias)
    ] -- ws -|> id

let private selectColumns =
    %% kw "SELECT"
    -- +.[  %% kw "DISTINCT" -|> Some DistinctColumns
            %% kw "ALL" -|> Some AllColumns
            preturn None
        ]
    -- +.(qty.[1..] / tws ',' * withSource resultColumn)
    -|> fun distinct cols -> { Distinct = distinct; Columns = cols }

let private indexHint =
    %[
        %% kw "INDEXED" -- kw "BY" -- +.nameOrKeyword -- ws -|> IndexedBy
        %% kw "NOT" -- kw "INDEXED" -|> NotIndexed
    ]

let private tableOrSubquery (tableExpr : Parser<TableExpr<unit, unit>, unit>) =
    let subterm =
        %% +.selectStmt
        -|> fun select alias -> TableOrSubquery { Table = Subquery select; Alias = alias; Info = () }
    let by =
        %[  %% +.indexHint -|> fun indexed table ->
                TableOrSubquery { Table = Table (table, Some indexed); Alias = None; Info = () }
            %% +.(asAlias * zeroOrOne) -- +.(indexHint * zeroOrOne)
                -|> fun alias indexed table ->
                    TableOrSubquery { Table = Table (table, indexed); Alias = alias; Info = () }
        ]

    %[  %% +.tableInvocation -- +.by -|> fun table by -> by table
        %% '(' -- ws -- +.subterm -- ')' -- ws -- +.(asAlias * zeroOrOne) -|> (<|)
    ]

let private joinType =
    %[
        %% kw "LEFT" -- (tws (kw "OUTER") * zeroOrOne) -|> LeftOuter
        %% kw "INNER" -|> Inner
        %% kw "CROSS" -|> Cross
        %% ws -|> Inner
    ]

let private joinConstraint =
    %[
        %% kw "ON" -- +.expr -- ws -|> JoinOn
        %% kw "USING" -- '(' -- ws -- +.(qty.[1..] / tws ',' * name) -- ')' -- ws -|> JoinUsing
        preturn JoinUnconstrained
    ]

let private tableExpr =
    precursive <| fun tableExpr ->
        let term = tableOrSubquery tableExpr |> withSource
        let natural = %% kw "NATURAL" -|> ()   
        let join =
            %% +.(
                    %[
                        %% ','
                            -|> fun left right constr ->
                                {   JoinType = Inner
                                    LeftTable = left
                                    RightTable = right
                                    Constraint = constr
                                } |> Join
                        %% +.(natural * zeroOrOne) -- +.joinType -- kw "JOIN"
                            -|> fun natural join left right constr ->
                                let joinType = if Option.isSome natural then Natural join else join
                                {   JoinType = joinType
                                    LeftTable = left
                                    RightTable = right
                                    Constraint = constr
                                } |> Join
                    ] |> withSource)
            -- ws
            -- +.term
            -- ws
            -- +.joinConstraint
            -|> fun f joinTo joinOn left -> { TableExpr.Source = f.Source; Value = f.Value left joinTo joinOn }
        %% +.term
        -- ws
        -- +.(join * qty.[0..])
        -|> Seq.fold (|>)

let private valuesClause =
    let valuesRow =
        %% '('
        -- ws
        -- +.(qty.[0..] / tws ',' * expr)
        -- ')'
        -- ws
        -|> id

    %% kw "VALUES"
    -- ws
    -- +.(qty.[1..] / tws ',' * withSource valuesRow)
    -- ws
    -|> id

let private fromClause =
    %% kw "FROM"
    -- +.tableExpr
    -|> id

let private whereClause =
    %% kw "WHERE"
    -- +.expr
    -|> id

let private havingClause =
    %% kw "HAVING"
    -- +.expr
    -|> id

let private groupByClause =
    %% kw "GROUP"
    -- kw "BY"
    -- +.(qty.[1..] / tws ',' * expr)
    -- +.(zeroOrOne * havingClause)
    -|> fun by having -> { By = by; Having = having }

let private selectCore =
    %% +.selectColumns
    -- +.(fromClause * zeroOrOne)
    -- +.(whereClause * zeroOrOne)
    -- +.(groupByClause * zeroOrOne)
    -|> fun cols table where groupBy ->
        {   Columns = cols
            From = table
            Where = where
            GroupBy = groupBy
            Info = ()
        }

let private compoundTerm =
    %% +.sourcePosition
    -- +.[  %% +.valuesClause -|> Values
            %% +.selectCore -|> Select
        ]
    -- +.sourcePosition
    -|> fun pos1 term pos2 ->
        {   Source = { StartPosition = pos1; EndPosition = pos2 }
            Value = term
            Info = ()
        }

let private compoundExpr =
    let compoundOperation =
        %[  %% kw "UNION" -- +.(zeroOrOne * kw "ALL") -|> function
                | Some () -> fun left right -> UnionAll (left, right)
                | None -> fun left right -> Union (left, right)
            %% kw "INTERSECT" -|> fun left right -> Intersect (left, right)
            %% kw "EXCEPT" -|> fun left right -> Except (left, right)
        ] |> withSource
    let compoundNext =
        %% +.compoundOperation
        -- +.compoundTerm
        -|> fun f right left -> { CompoundExpr.Source = f.Source; Value = f.Value left right }
    %% +.(compoundTerm |>> fun t -> { CompoundExpr.Source = t.Source; Value = CompoundTerm t })
    -- +.(compoundNext * qty.[0..])
    -|> Seq.fold (|>)

let private orderDirection =
    %[
        %% kw "DESC" -|> Descending
        %% kw "ASC" -|> Ascending
        preturn Ascending
    ]

let private orderingTerm =
    %% +.expr
    -- +.orderDirection
    -- ws
    -|> fun expr dir -> { By = expr; Direction = dir }

let private orderBy =
    %% kw "ORDER"
    -- kw "BY"
    -- +.(qty.[1..] / tws ',' * orderingTerm)
    -|> id

let private limit =
    let offset =
        %% [%% ',' -- ws -|> (); kw "OFFSET"]
        -- +.expr
        -|> id
    %% kw "LIMIT"
    -- +.expr
    -- +.(zeroOrOne * offset)
    -|> fun limit offset -> { Limit = limit; Offset = offset }

do
    selectStmtImpl :=
        (
            %% +.(zeroOrOne * withClause)
            -? +.compoundExpr
            -- +.(zeroOrOne * orderBy)
            -- +.(zeroOrOne * limit)
            -|> fun cte comp orderBy limit ->
                {
                    With = cte
                    Compound = comp
                    OrderBy = orderBy
                    Limit = limit
                    Info = ()
                }
        ) |> withSource

let private conflictClause =
    let onConflict =
        %% kw "ON" -- kw "CONFLICT" -|> ()
    let clause =
        %% (onConflict * zeroOrOne)
        -- +.[
                %% kw "ROLLBACK" -|> Rollback
                %% kw "ABORT" -|> Abort
                %% kw "FAIL" -|> Fail
                %% kw "IGNORE" -|> Ignore
                %% kw "REPLACE" -|> Replace
            ]
        -|> id
    zeroOrOne * clause

let private foreignKeyRule =
    let eventRule =
        %% kw "ON"
        -- +.[
                %% kw "DELETE" -|> OnDelete
                %% kw "UPDATE" -|> OnUpdate
            ]
        -- +.[
                %% kw "SET" -- +.[ %% kw "NULL" -|> SetNull; %% kw "DEFAULT" -|> SetDefault ] -|> id
                %% kw "CASCADE" -|> Cascade
                %% kw "RESTRICT" -|> Restrict
                %% kw "NO" -- kw "ACTION" -|> NoAction
            ]
        -|> fun evt handler -> EventRule (evt, handler)
    let matchRule =
        %% kw "MATCH"
        -- +.name
        -- ws
        -|> MatchRule
    %[ eventRule; matchRule ]


let private foreignKeyDeferClause =
    let initially =
        %% kw "INITIALLY" -- +.[ %% kw "DEFERRED" -|> true; %% kw "IMMEDIATE" -|> false ] -|> id
    %% +.(zeroOrOne * kw "NOT")
    -? kw "DEFERRABLE"
    -- +.(zeroOrOne * initially)
    -|> fun not init -> { Deferrable = Option.isNone not; InitiallyDeferred = init }

let private foreignKeyClause =
    %% kw "REFERENCES"
    -- +.objectName
    -- +.(zeroOrOne * parenthesizedColumnNames)
    -- +.(qty.[0..] * foreignKeyRule)
    -- +.(zeroOrOne * foreignKeyDeferClause)
    -|> fun table cols rules defer ->
        {
            ReferencesTable = table
            ReferencesColumns = cols
            Rules = rules
            Defer = defer
        }

let private constraintName =
    %% kw "CONSTRAINT"
    -- +.name
    -- ws
    -|> id

let private primaryKeyClause =
    %% kw "PRIMARY"
    -- kw "KEY"
    -- +.orderDirection
    -- ws
    -- +.conflictClause
    -- +.(zeroOrOne * tws (kw "AUTOINCREMENT"))
    -|> fun dir conflict auto ->
        {
            Order = dir
            ConflictClause = conflict
            AutoIncrement = Option.isSome auto
        }

let private constraintType =
    let signedToExpr (signed : SignedNumericLiteral WithSource) =
        let expr = signed.Value.Value |> NumericLiteral |> LiteralExpr
        let expr = { Expr.Source = signed.Source; Value = expr; Info = () }
        if signed.Value.Sign < 0 then
            { Expr.Source = expr.Source; Value = UnaryExpr { Operator = Negative; Operand = expr }; Info = () } 
        else expr
    let defaultValue =
        %[
            %% +.withSource signedNumericLiteral -|> signedToExpr
            %% +.withSource literal -|> fun lit -> { Source = lit.Source; Value = LiteralExpr lit.Value; Info = () }
            %% '(' -- ws -- +.expr -- ')' -|> id
            // docs don't mention this, but it works
            %% +.withSource name
                -|> fun name ->
                    { Source = name.Source; Value = name.Value.ToString() |> StringLiteral |> LiteralExpr; Info = () }
        ]
    %[
        %% +.primaryKeyClause -|> PrimaryKeyConstraint
        %% kw "NOT"  -- kw "NULL" -- +.conflictClause -|> NotNullConstraint
        %% kw "NULL" -|> NullableConstraint
        %% kw "UNIQUE" -- +.conflictClause -|> UniqueConstraint
        %% kw "CHECK" -- '(' -- ws -- +.expr -- ')' -|> CheckConstraint
        %% kw "DEFAULT" -- +.defaultValue -|> DefaultConstraint
        %% kw "COLLATE" -- +.name -|> CollateConstraint
        %% +.foreignKeyClause -|> ForeignKeyConstraint
    ]

let private columnConstraint =
    %% +.(zeroOrOne * constraintName)
    -- +.constraintType
    -- ws
    -|> fun name cty -> { Name = name; ColumnConstraintType = cty }

let private columnDef =
    %% +.nameOrKeyword
    -- ws
    -- +.typeName
    -- +.(columnConstraint * qty.[0..])
    -|> fun name typeName constraints ->
        {   Name = name
            Type = typeName
            Constraints = constraints
        }

let private alterTableStmt =
    let renameTo =  
        %% kw "RENAME"
        -- kw "TO"
        -- +.name
        -|> RenameTo
    let addColumn =
        %% kw "ADD"
        -- zeroOrOne * kw "COLUMN"
        -- +.columnDef
        -|> AddColumn
    %% kw "ALTER"
    -- kw "TABLE"
    -- +.objectName
    -- +.[ renameTo; addColumn ]
    -|> fun table alteration -> { Table = table; Alteration = alteration }

let private tableIndexConstraintType =
    %[
        %% kw "PRIMARY" -- kw "KEY" -|> PrimaryKey
        %% kw "UNIQUE" -|> Unique
    ]

let private indexedColumns =
    %% '('
    -- ws
    -- +.(qty.[1..] / tws ',' * (%% +.expr -- +.orderDirection -%> auto))
    -- ')'
    -- ws
    -|> id

let private tableIndexConstraint =
    %% +.tableIndexConstraintType
    -- +.indexedColumns
    -- +.conflictClause
    -|> fun cty cols conflict ->
        { Type = cty; IndexedColumns = cols; ConflictClause = conflict }

let private tableConstraintType =
    let foreignKey =
        %% kw "FOREIGN"
        -- kw "KEY"
        -- +.parenthesizedColumnNames
        -- +.foreignKeyClause
        -|> fun columns fk -> TableForeignKeyConstraint (columns, fk)
    %[
        %% kw "CHECK" -- '(' -- ws -- +.expr -- ')' -|> TableCheckConstraint
        foreignKey
        %% +.tableIndexConstraint -|> TableIndexConstraint
    ]

let private tableConstraint =
    %% +.(zeroOrOne * constraintName)
    -- +.tableConstraintType
    -- ws
    -|> fun name cty -> { Name = name; TableConstraintType = cty }

let private createTableDefinition =
    let part =
        %[
            %% +.tableConstraint -|> Choice1Of2
            %% +.columnDef -|> Choice2Of2
        ]
    %% '('
    -- ws
    -- +.(qty.[0..] /. tws ',' * part)
    -- ')'
    -- ws
    -- +.(zeroOrOne * (%% kw "WITHOUT" -- kw "ROWID" -- ws -|> ()))
    -|> fun parts without ->
        {
            Columns =
                parts |> Seq.choose (function | Choice2Of2 cdef -> Some cdef | Choice1Of2 _ -> None) |> ResizeArray
            Constraints =
                parts |> Seq.choose (function | Choice1Of2 ct -> Some ct | Choice2Of2 _ -> None) |> ResizeArray
            WithoutRowId = Option.isSome without
        }

let private createTableAs =
    %[  %% kw "AS" -- +.selectStmt -|> CreateAsSelect
        %% +.createTableDefinition -|> CreateAsDefinition
    ]

let private ifNotExists = %(zeroOrOne * (%% kw "IF" -- kw "NOT" -- kw "EXISTS" -|> ()))

let private temporary = %(zeroOrOne * [kw "TEMPORARY"; kw "TEMP"])
        
let private createTableStmt =
    %% kw "CREATE"
    -- +.temporary
    -? kw "TABLE"
    -- +.ifNotExists
    -- +.objectName
    -- +.createTableAs
    -|> fun temp ifNotExists name createAs ->
        {   Temporary = Option.isSome temp
            IfNotExists = Option.isSome ifNotExists
            Name = name
            As = createAs
        }

let private analyzeStmt =
    %% kw "ANALYZE"
    -- +.(zeroOrOne * objectName)
    -|> id

let private attachStmt =
    %% kw "ATTACH"
    -- zeroOrOne * kw "DATABASE"
    -- +.expr
    -- kw "AS"
    -- +.nameOrKeyword
    -|> fun ex schemaName -> ex, schemaName

let private beginStmt =
    %% kw "BEGIN"
    -- zeroOrOne * kw "TRANSACTION"
    -|> BeginStmt

let private commitStmt =
    %% [ kw "COMMIT"; kw "END" ]
    -- zeroOrOne * kw "TRANSACTION"
    -|> CommitStmt

let private rollbackStmt =
    %% kw "ROLLBACK"
    -- zeroOrOne * kw "TRANSACTION"
    -|> RollbackStmt

let private createIndexStmt =
    %% kw "CREATE"
    -- +.(zeroOrOne * kw "UNIQUE")
    -? kw "INDEX"
    -- +.ifNotExists
    -- +.objectName
    -- kw "ON"
    -- +.objectName
    -- +.indexedColumns
    -- +.(zeroOrOne * (%% kw "WHERE" -- +.expr -|> id))
    -|> fun unique ifNotExists indexName tableName cols whereExpr ->
        {   Unique = Option.isSome unique
            IfNotExists = Option.isSome ifNotExists
            IndexName = indexName
            TableName = tableName
            IndexedColumns = cols
            Where = whereExpr
        }

let private qualifiedTableName =
    %% +.objectName
    -- +.(zeroOrOne * indexHint)
    -|> fun tableName hint ->
        {   TableName = tableName
            IndexHint = hint
        }

let private deleteStmt =
    %% +.(zeroOrOne * withClause)
    -? kw "DELETE"
    -- kw "FROM"
    -- +.qualifiedTableName
    -- +.(zeroOrOne * whereClause)
    -- +.(zeroOrOne * orderBy)
    -- +.(zeroOrOne * limit)
    -|> fun withClause fromTable where orderBy limit ->
        {   With = withClause
            DeleteFrom = fromTable
            Where = where
            OrderBy = orderBy
            Limit = limit
        }

let private updateOr =
    %% kw "OR"
    -- +.[
            %% kw "ROLLBACK" -|> UpdateOrRollback
            %% kw "ABORT" -|> UpdateOrAbort
            %% kw "REPLACE" -|> UpdateOrReplace
            %% kw "FAIL" -|> UpdateOrFail
            %% kw "IGNORE" -|> UpdateOrIgnore
        ]
    -|> id

let private updateStmt =
    let setColumn =
        %% +.nameOrString
        -- ws
        -- '='
        -- ws
        -- +.expr
        -|> fun name expr -> name, expr
    %% +.(zeroOrOne * withClause)
    -? kw "UPDATE"
    -- +.(zeroOrOne * updateOr)
    -- +.qualifiedTableName
    -- kw "SET"
    -- +.(qty.[1..] / tws ',' * setColumn)
    -- +.(zeroOrOne * whereClause)
    -- +.(zeroOrOne * orderBy)
    -- +.(zeroOrOne * limit)
    -|> fun withClause updateOr table sets where orderBy limit ->
        {   With = withClause
            UpdateTable = table
            Or = updateOr
            Set = sets
            Where = where
            OrderBy = orderBy
            Limit = limit
        }

let private insertOr =
    let orPart =
        %% kw "OR"
        -- +.[
                %% kw "REPLACE" -|> InsertOrReplace
                %% kw "ROLLBACK" -|> InsertOrRollback
                %% kw "ABORT" -|> InsertOrAbort
                %% kw "FAIL" -|> InsertOrFail
                %% kw "IGNORE" -|> InsertOrIgnore
            ]
        -|> id
    %[  %% kw "REPLACE" -|> Some InsertOrReplace
        %% kw "INSERT" -- +.(zeroOrOne * orPart) -|> id
    ]

let private insertStmt =
    %% +.(zeroOrOne * withClause)
    -? +.insertOr
    -- kw "INTO"
    -- +.objectName
    -- +.(zeroOrOne * parenthesizedColumnNames)
    -- +.[
            %% kw "DEFAULT" -- kw "VALUES" -|> None
            %% +.selectStmt -|> Some
        ]
    -|> fun withClause insert table cols data ->
        {   With = withClause
            Or = insert
            InsertInto = table
            Columns = cols
            Data = data
        }

let private triggerSchedule =
    %[
        %% kw "BEFORE" -|> Before
        %% kw "AFTER" -|> After
        %% kw "INSTEAD" -- kw "OF" -|> InsteadOf
        preturn Before
    ]

let private triggerCause =
    let updateColumns =
        %% kw "OF" -- +.(qty.[1..] / tws ',' * tws name) -|> id
    %[  %% kw "DELETE" -|> DeleteOn
        %% kw "INSERT" -|> InsertOn
        %% kw "UPDATE" -- +.(zeroOrOne * updateColumns) -|> UpdateOn
    ]

let private triggerAction =
    %[  %% +.selectStmt -|> TriggerSelect
        %% +.deleteStmt -|> TriggerDelete
        %% +.updateStmt -|> TriggerUpdate
        %% +.insertStmt -|> TriggerInsert
    ]

let private createTriggerStmt =
    let whenClause =
        %% kw "WHEN"
        -- +.expr
        -|> id
    %% kw "CREATE"
    -- +.temporary
    -? kw "TRIGGER"
    -- +.ifNotExists
    -- +.objectName
    -- +.triggerSchedule
    -- +.triggerCause
    -- kw "ON"
    -- +.objectName
    -- zeroOrOne * (%% kw "FOR" -- kw "EACH" -- kw "ROW" -|> ())
    -- +.(zeroOrOne * whenClause)
    -- kw "BEGIN"
    -- +.(qty.[1..] /. tws ';' * tws triggerAction)
    -- kw "END"
    -|> fun temp ifne triggerName schedule cause tableName whenClause actions ->
        {   Temporary = Option.isSome temp
            IfNotExists = Option.isSome ifne
            TriggerName = triggerName
            TableName = tableName
            Schedule = schedule
            Cause = cause
            Condition = whenClause
            Actions = actions
        }

let private createViewStmt =
    %% kw "CREATE"
    -- +.temporary
    -? kw "VIEW"
    -- +.ifNotExists
    -- +.objectName
    -- +.(zeroOrOne * parenthesizedColumnNames)
    -- kw "AS"
    -- +.selectStmt
    -|> fun temp ifNotExists viewName cols asSelect ->
        {   Temporary = Option.isSome temp
            IfNotExists = Option.isSome ifNotExists
            ViewName = viewName
            ColumnNames = cols
            AsSelect = asSelect
        }

let private ifExists =
    %[  %% kw "IF" -- kw "EXISTS" -|> true
        preturn false
    ]

let private dropObjectType =
    %[  %% kw "INDEX" -|> DropIndex
        %% kw "TABLE" -|> DropTable
        %% kw "TRIGGER" -|> DropTrigger
        %% kw "VIEW" -|> DropView
    ]

let private dropObjectStmt =
    %% kw "DROP"
    -? +.dropObjectType
    -- +.ifExists
    -- +.objectName
    -|> fun dropType ifExists name ->
        { Drop = dropType; IfExists = ifExists; ObjectName = name }

let private stmt =
    %[  %% +.alterTableStmt -|> AlterTableStmt
        %% +.createIndexStmt -|> CreateIndexStmt
        %% +.createTableStmt -|> CreateTableStmt
        %% +.createTriggerStmt -|> CreateTriggerStmt
        %% +.createViewStmt -|> CreateViewStmt
        %% +.deleteStmt -|> DeleteStmt
        %% +.dropObjectStmt -|> DropObjectStmt
        %% +.insertStmt -|> InsertStmt
        %% +.selectStmt -|> SelectStmt
        %% +.updateStmt -|> UpdateStmt
        beginStmt
        commitStmt
        rollbackStmt
    ]

let private stmtsAtLeast min =
    %% ws
    -- +.(qty.[min..] /. tws ';' * tws stmt)
    -- eof
    -|> id

let private stmts = stmtsAtLeast 0

let parseStatements sourceDescription source =
    match runParserOnString stmts () sourceDescription source with
    | Success (statements, _, _) -> statements
    | Failure (reason, err, _) ->
        let sourceInfo = SourceInfo.OfPosition(translatePosition err.Position)
        failAt sourceInfo reason
