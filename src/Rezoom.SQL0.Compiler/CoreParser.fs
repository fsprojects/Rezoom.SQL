// Parses our typechecked subset of the SQL language.

module private Rezoom.SQL.Compiler.CoreParser
open System
open System.Collections.Generic
open System.Globalization
open FParsec
open FParsec.Pipes
open FParsec.Pipes.Precedence
open Rezoom.SQL.Compiler

/// Get the source position the parser is currently at.
let private sourcePosition =
    %% +.p<Position>
    -%> translatePosition

/// Wraps any parser with source information.
let withSource (parser : Parser<'a, unit>) =
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
    ] <?> "whitespace"

/// Optional whitespace: 0 or more whitespace units
let ws = skipMany whitespaceUnit

/// Add optional trailing whitespace to a parser.
let inline tws parser = %parser .>> ws

/// Required whitespace: 1 or more whitespace units
let ws1 = skipMany1 whitespaceUnit

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
            FParsec.Primitives.fail (Error.reservedKeywordAsName ident)
        else
            preturn ident

let name =
    %[  quotedName
        bracketedName
        backtickedName
        unquotedName
    ] <?> "name"

let private stringLiteral =
   (let escapedQuote =
        %% "''" -|> "'" // A pair of single quotes escapes a single quote character
    let regularChars =
        many1Satisfy ((<>) '\'') // Any run of non-quote characters is literal
    %% '\'' -- +.([regularChars; escapedQuote] * qty.[0..]) -- '\''
    -|> String.Concat)
    <?> "string-literal"

let private nameOrKeyword =
    %[  quotedName
        bracketedName
        backtickedName
        unquotedNameOrKeyword
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
        | _ -> bug "Unreachable")
    <?> "column-name"

let private namedBindParameter =
    %% '@'
    -- +.unquotedNameOrKeyword
    -|> fun name -> NamedParameter name

let private bindParameter = namedBindParameter <?> "bind-parameter"

let private kw str =
    %% ci str
    -? notFollowedByL (satisfy isFollowingIdentifierCharacter) str
    -- ws
    -|> () <?> str

let private nullLiteral =
    %% kw "NULL" -|> NullLiteral

let private booleanLiteral =
    %[  %% kw "TRUE" -|> BooleanLiteral true
        %% kw "FALSE" -|> BooleanLiteral false
    ]

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

let private dateTimeishLiteral =
    let digit = digit |>> fun c -> int c - int '0'
    let digits n =
        qty.[n] * digit |>> Array.fold (fun acc next -> acc * 10 + next) 0
    let date = %% +.digits 4 -- '-' -- +.digits 2 -- '-' -- +.digits 2 -%> auto
    let time = %% ci 'T' -- +.digits 2 -- ':' -- +.digits 2 -- ':' -- +.digits 2 -%> auto
    let ms =
        %% '.' -- +.(qty.[1..3] * digit)
        -|> fun ds ->
            let n = Seq.fold (fun acc next -> acc * 10 + next) 0 ds
            let delta = ds.Count - 3
            if delta > 0 then n / pown 10 delta
            elif delta < 0 then n * pown 10 (-delta)
            else n
    let offsetPart =
        %% +.[ %% '+' -|> 1; %% '-' -|> -1 ]
        -- +.digits 2
        -- ':'
        -- +.digits 2
        -%> auto
    let timePart =
        %% +.time
        -- +.(zeroOrOne * ms)
        -- +.(zeroOrOne * offsetPart)
        -%> auto
    %% +.(date <?> "date-literal")
    ?- +.(zeroOrOne * (timePart <?> "time-literal"))
    -|> fun (year, month, day) time ->
        match time with
        | None -> DateTime(year, month, day, 0, 0, 0, DateTimeKind.Utc) |> DateTimeLiteral
        | Some ((hour, minute, second), ms, offset) ->
            let ms = ms |? 0
            let dateTime = DateTime(year, month, day, hour, minute, second, ms)
            match offset with
            | None ->
                DateTime.SpecifyKind(dateTime, DateTimeKind.Utc)
                |> DateTimeLiteral
            | Some (sign, offsetHour, offsetMinute) ->
                DateTimeOffset(dateTime, TimeSpan(offsetHour * sign, offsetMinute * sign, 0))
                |> DateTimeOffsetLiteral

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
            FParsec.Primitives.fail "hexadecimal floats are not permitted"
        else 
            lit.String |> float |> FloatLiteral |> preturn

let private literal =
    %[  booleanLiteral
        nullLiteral
        blobLiteral
        %% +.stringLiteral -|> StringLiteral
        dateTimeishLiteral
        %% +.numericLiteral -|> NumericLiteral
    ] <?> "literal"

let private typeName =
    let maxBound = %% '(' -- ws -- +.p<int> -- ws -- ')' -- ws -%> id
    %[  %% kw "STRING" -- +.(zeroOrOne * maxBound) -%> StringTypeName
        %% kw "GUID" -%> GuidTypeName
        %% kw "BINARY" -- +.(zeroOrOne * maxBound) -%> BinaryTypeName
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
    ] <?> "type-name"

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
        -|> fun distinct args -> ArgumentList (distinct, args.ToArray())
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
        -|> fun cases els -> { Input = None; Cases = cases.ToArray(); Else = els }
    let ofForm =
        %% +.expr
        -- +.whenForm
        -|> fun ofExpr case -> { case with Input = Some ofExpr }
    %% kw "CASE"
    -- +.[ whenForm; ofForm ]
    -|> id

let expr, private exprImpl = createParserForwardedToRef<Expr<unit, unit>, unit>()
let private selectStmt, private selectStmtImpl =
    createParserForwardedToRef<SelectStmt<unit, unit>, unit>()
let private selectStmtWithoutCTE, private selectStmtWithoutCTEImpl =
    createParserForwardedToRef<SelectStmt<unit, unit>, unit>()

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
    -|> fun name args -> { Table = name; Arguments = args |> Option.map (fun r -> r.ToArray()) }

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
                        %% +.selectStmtWithoutCTE -|> InSelect
                        %% +.(qty.[0..] / tws ',' * expr) -|> (fun exs -> exs.ToArray() |> InExpressions)
                    ]
                -- ')'
                -|> id
                %% +.bindParameter -|> InParameter
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
            %% kw "MATCH" -|> Match
            %% kw "REGEXP" -|> Regexp
        ] |> withSource
    %% +.(zeroOrOne * kw "NOT")
    -? +.op
    -|> similar

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

let private term (expr : Parser<Expr<unit, unit>, unit>) =
    let parenthesized =
        %[
            %% +.selectStmtWithoutCTE -|> ScalarSubqueryExpr
            %% +.expr -|> fun e -> e.Value
        ]
    %% +.sourcePosition
    -- +.[
            %% '(' -- ws -- +.parenthesized -- ')' -|> id
            %% kw "EXISTS" -- ws -- '(' -- ws -- +.selectStmtWithoutCTE -- ')' -|> ExistsExpr
            %% +.literal -|> LiteralExpr
            %% +.bindParameter -|> BindParameterExpr
            %% +.cast expr -|> CastExpr
            %% +.case expr -|> CaseExpr
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
        Precedence.expression
            {   Whitespace = ws
                Term = term
                Operators = operators    
            } <?> "expr"

let private parenthesizedColumnNames =
    %% '('
    -- ws
    -- +.(qty.[0..] / tws ',' * tws (withSource name))
    -- ')'
    -- ws
    -|> fun vs -> vs.ToArray()

let private commonTableExpression =
    %% +.nameOrKeyword
    -- ws
    -- +.(zeroOrOne * withSource parenthesizedColumnNames)
    -- kw "AS"
    -- '('
    -- ws
    -- +.selectStmtWithoutCTE
    -- ')'
    -- ws
    -|> fun table cols asSelect ->
        {   Name = table
            ColumnNames = cols
            AsSelect = asSelect
            Info = ()
        }

let private withClause =
    %% kw "WITH"
    -- +.(zeroOrOne * kw "RECURSIVE")
    -- +.(qty.[1..] / tws ',' * commonTableExpression)
    -|> fun recurs ctes ->
        { Recursive = Option.isSome recurs; Tables = ctes.ToArray() }

let private asAlias =
    %% (zeroOrOne * kw "AS")
    -? +.name
    -|> id

let private resultColumnNavCardinality =
    %[
        %% kw "MANY" -|> NavMany
        %% kw "OPTIONAL" -|> NavOptional
        %% kw "ONE" -|> NavOne
    ]

let private resultColumnCase (resultColumns : Parser<_, unit>) =
    let nav =
        %% +.resultColumnNavCardinality
        -? +.nameOrKeyword
        -- ws
        -- '('
        -- ws
        -- +.resultColumns
        -- ')'
        -- ws
        -|> fun cardinality name cols ->
            {   Cardinality = cardinality
                Name = name
                Columns = cols
            } |> ColumnNav
    %% +.[
        %% '*' -|> ColumnsWildcard
        nav
        %% +.name -- '.' -? '*' -|> TableColumnsWildcard
        %% +.expr -- +.(asAlias * zeroOrOne) -|> fun ex alias -> Column (ex, alias)
    ] -- ws -|> id

let private resultColumns =
    precursive <| fun resultColumns ->
        let column =
            %% +.withSource (resultColumnCase resultColumns)
            -|> fun case ->
                {   ResultColumn.Case = case.Value
                    Source = case.Source
                }
        %% +.(qty.[1..] /. tws ',' * column)
        -|> Seq.toArray

let private selectColumns =
    let badTop =
        (%ci "TOP" <?> "TOP")
        .>> FParsec.Primitives.fail
            "SELECT TOP (X) syntax is not supported, use LIMIT (X) at the end of your query instead"
    %% kw "SELECT"
    -- (zeroOrOne * badTop)
    -- +.[  %% kw "DISTINCT" -|> Some Distinct
            preturn None
        ]
    -- +.resultColumns
    -|> fun distinct cols -> { Distinct = distinct; Columns = cols }

let private tableOrSubquery =
    let subterm =
        %% +.selectStmtWithoutCTE
        -|> fun select alias -> TableOrSubquery { Table = Subquery select; Alias = alias; Info = () }
    let by =
        %% +.(asAlias * zeroOrOne)
        -|> fun alias table ->
            TableOrSubquery { Table = Table table; Alias = alias; Info = () }
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

let private joinConstraint = %% kw "ON" -- +.expr -- ws -|> JoinOn

let private tableExpr = // parses table expr (with left-associative joins)
    let term = tableOrSubquery |> withSource
    let natural = %% kw "NATURAL" -|> ()   
    let join =
        %[  %% ',' -- ws -- +.withSource term
            -|> fun right left ->
                {   TableExpr.Source = right.Source
                    Value =
                        {   JoinType = Inner
                            LeftTable = left
                            RightTable = right.Value
                            Constraint = JoinUnconstrained
                        } |> Join
                }
            %% +.(natural * zeroOrOne) -- +.withSource joinType -- kw "JOIN" -- +.term -- ws -- +.joinConstraint
            -|> fun natural join right constr left ->
                let joinType = if Option.isSome natural then Natural join.Value else join.Value
                {   TableExpr.Source = join.Source
                    Value =
                        {   JoinType = joinType
                            LeftTable = left
                            RightTable = right
                            Constraint = constr
                        } |> Join
                }
        ]
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
        -|> fun vs -> vs.ToArray()

    %% kw "VALUES"
    -- ws
    -- +.(qty.[1..] / tws ',' * withSource valuesRow)
    -- ws
    -|> fun vs -> vs.ToArray()

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
    -|> fun by having -> { By = by.ToArray(); Having = having }

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
        {   CompoundTerm.Source = { StartPosition = pos1; EndPosition = pos2 }
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
    -|> fun by -> by.ToArray()

let private limit =
    let offset =
        %% [%% ',' -- ws -|> (); kw "OFFSET"]
        -- +.expr
        -|> id
    %% kw "LIMIT"
    -- +.expr
    -- +.(zeroOrOne * offset)
    -|> fun limit offset -> { Limit = limit; Offset = offset }

let private selectStmtPendingCTE =
    %% +.withSource compoundExpr
    -- +.(zeroOrOne * orderBy)
    -- +.(zeroOrOne * limit)
    -|> fun comp orderBy limit cte ->
        {   WithSource.Source = comp.Source
            Value =
                {   With = cte
                    Compound = comp.Value
                    OrderBy = orderBy
                    Limit = limit
                    Info = ()
                }
        }

do
    selectStmtWithoutCTEImpl := selectStmtPendingCTE |>> ((|>) None)
    selectStmtImpl :=
        %% +.(zeroOrOne * withClause)
        -? +.selectStmtPendingCTE
        -|> (|>)

let private onDeleteAction =
    %% kw "ON"
    -- kw "DELETE"
    -- +.[
            %% kw "SET" -- +.[ %% kw "NULL" -|> SetNull; %% kw "DEFAULT" -|> SetDefault ] -|> id
            %% kw "CASCADE" -|> Cascade
            %% kw "RESTRICT" -|> Restrict
            %% kw "NO" -- kw "ACTION" -|> NoAction
        ]
    -|> id

let private foreignKeyClause =
    %% kw "REFERENCES"
    -- +.objectName
    -- +.parenthesizedColumnNames
    -- +.(zeroOrOne * onDeleteAction)
    -|> fun table cols onDelete ->
        {
            ReferencesTable = table
            ReferencesColumns = cols
            OnDelete = onDelete
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
    -- +.(zeroOrOne * tws (kw "AUTOINCREMENT"))
    -|> fun dir auto ->
        {
            Order = dir
            AutoIncrement = Option.isSome auto
        }

let private constraintType =
    %[
        %% +.primaryKeyClause -|> PrimaryKeyConstraint
        %% kw "UNIQUE" -|> UniqueConstraint
        %% +.foreignKeyClause -|> ForeignKeyConstraint
    ]

let private columnConstraint =
    %% +.(zeroOrOne * constraintName)
    -- +.constraintType
    -- ws
    -|> fun name cty columnName tblName ->
        {   Name = name |? cty.DefaultName(tblName, columnName)
            ColumnConstraintType = cty
        }

let private columnDef =
    let collation = %% kw "COLLATE" -- +.name -- ws -|> id
    let defaultValue = %% kw "DEFAULT" -- +.expr -|> id
    %% +.nameOrKeyword
    -- ws
    -- +.typeName
    -- +.(zeroOrOne * kw "NULL")
    -- +.(zeroOrOne * collation)
    -- +.(zeroOrOne * defaultValue)
    -- +.(columnConstraint * qty.[0..])
    -|> fun name typeName nullable collation defaultVal constraints tblName ->
        {   Name = name
            Type = typeName
            Nullable = Option.isSome nullable
            Collation = collation
            DefaultValue = defaultVal
            Constraints = constraints |> Seq.map (fun f -> f name tblName) |> Seq.toArray
        }

let private tableIndexConstraintType =
    %[
        %% kw "PRIMARY" -- kw "KEY" -|> PrimaryKey
        %% kw "UNIQUE" -|> Unique
    ]

let private indexedColumns =
    %% '('
    -- ws
    -- +.(qty.[1..] / tws ',' * withSource (%% +.nameOrKeyword -- ws -- +.orderDirection -%> auto))
    -- ')'
    -- ws
    -|> fun vs -> vs.ToArray()

let private tableIndexConstraint =
    %% +.tableIndexConstraintType
    -- +.indexedColumns
    -|> fun cty cols ->
        { Type = cty; IndexedColumns = cols }

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
    -|> fun name cty tblName ->
        {   Name = match name with | Some name -> name | None -> cty.DefaultName(tblName)
            TableConstraintType = cty
        }

let private alterTableStmt =
    let renameTo =  
        %% kw "RENAME"
        -- kw "TO"
        -- +.name
        -|> RenameTo
    let add =
        let addColumn =
            %% kw "COLUMN" -- +.withSource columnDef
            -|> fun cdef tblName -> AddColumn (applySource cdef tblName)
        let addDefault =
            %% kw "DEFAULT" -- kw "FOR" -- +.name -- ws -- +.expr
            -|> fun name expr _ -> AddDefault (name, expr)
        let addConstraint = withSource tableConstraint |>> fun cstr tblName -> AddConstraint (applySource cstr tblName)
        %% kw "ADD"
        -- +.[  addColumn
                addDefault
                addConstraint
            ]
        -|> id
    let drop =
        let dropColumn = %% kw "COLUMN" -- +.nameOrKeyword -|> DropColumn
        let dropConstraint = %% kw "CONSTRAINT" -- +.nameOrKeyword -|> DropConstraint
        let dropDefault = %% kw "DEFAULT" -- kw "FOR" -- +.nameOrKeyword -|> DropDefault
        %% kw "DROP"
        -- +.[  dropColumn
                dropConstraint
                dropDefault
            ]
        -|> id
    let alterColumn =
        let makeNotNullable =
            %% kw "NOT" -- kw "NULL" -|> fun name ->
                ChangeNullability { ExistingInfo = (); Column = name; NewNullable = false }
        let makeNullable =
            %% kw "NULL" -|> fun name ->
                ChangeNullability { ExistingInfo = (); Column = name; NewNullable = true }
        let changeCollation =
            %% kw "COLLATE"
            -- ws
            -- +.name
            -|> fun collation columnName ->
                ChangeCollation { ExistingInfo = (); Column = columnName; NewCollation = collation }
        let changeType =
            %% +.typeName
            -|> fun typeName columnName ->
                ChangeType { ExistingInfo = (); Column = columnName; NewType = typeName }
        %% kw "ALTER"
        -- kw "COLUMN"
        -- +.name
        -- ws
        -- +.[  makeNotNullable
                makeNullable
                changeCollation
                changeType
            ]
        -|> (|>)
    let ignoreTblName parser = parser |>> fun x _ -> x
    %% kw "ALTER"
    -- kw "TABLE"
    -- +.objectName
    -- +.[  ignoreTblName renameTo
            add
            ignoreTblName drop
            ignoreTblName alterColumn
        ]
    -|> fun table alteration -> { Table = table; Alteration = alteration table.ObjectName }

let private createTableDefinition =
    let part =
        %[
            %% +.withSource tableConstraint -|> Choice1Of2
            %% +.withSource columnDef -|> Choice2Of2
        ]
    %% '('
    -- ws
    -- +.(qty.[0..] /. tws ',' * part)
    -- ')'
    -- ws
    -|> fun parts tblName ->
        {   Columns =
                parts
                |> Seq.choose (function | Choice2Of2 cdef -> Some (applySource cdef tblName) | Choice1Of2 _ -> None)
                |> Seq.toArray
            Constraints =
                parts
                |> Seq.choose (function | Choice1Of2 ct -> Some (applySource ct tblName) | Choice2Of2 _ -> None)
                |> Seq.toArray
        }

let private createTableAs =
    %[  %% kw "AS" -- +.selectStmt -|> fun select _ -> CreateAsSelect select
        %% +.createTableDefinition -|> fun def tblName -> CreateAsDefinition (def tblName)
    ]

let private temporary = %(zeroOrOne * [kw "TEMPORARY"; kw "TEMP"])
        
let private createTableStmt =
    %% kw "CREATE"
    -- +.temporary
    -? kw "TABLE"
    -- +.objectName
    -- +.createTableAs
    -|> fun temp name createAs ->
        {   Temporary = Option.isSome temp
            Name = name
            As = createAs name.ObjectName
        }

let private createIndexStmt =
    %% kw "CREATE"
    -- +.(zeroOrOne * kw "UNIQUE")
    -? kw "INDEX"
    -- +.objectName
    -- kw "ON"
    -- +.objectName
    -- +.indexedColumns
    -- +.(zeroOrOne * (%% kw "WHERE" -- +.expr -|> id))
    -|> fun unique indexName tableName cols whereExpr ->
        {   Unique = Option.isSome unique
            IndexName = indexName
            TableName = tableName
            IndexedColumns = cols
            Where = whereExpr
        }

let private deleteStmt =
    %% kw "DELETE"
    -- kw "FROM"
    -- +.objectName
    -- +.(zeroOrOne * whereClause)
    -- +.(zeroOrOne * orderBy)
    -- +.(zeroOrOne * limit)
    -|> fun fromTable where orderBy limit withClause ->
        {   With = withClause
            DeleteFrom = fromTable
            Where = where
            OrderBy = orderBy
            Limit = limit
        } |> DeleteStmt

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
        %% +.withSource name
        -- ws
        -- '='
        -- ws
        -- +.expr
        -|> fun name expr -> name, expr
    %% kw "UPDATE"
    -- +.(zeroOrOne * updateOr)
    -- +.objectName
    -- kw "SET"
    -- +.(qty.[1..] / tws ',' * setColumn)
    -- +.(zeroOrOne * whereClause)
    -- +.(zeroOrOne * orderBy)
    -- +.(zeroOrOne * limit)
    -|> fun updateOr table sets where orderBy limit withClause ->
        {   With = withClause
            UpdateTable = table
            Or = updateOr
            Set = sets.ToArray()
            Where = where
            OrderBy = orderBy
            Limit = limit
        } |> UpdateStmt

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
    let insertStmtRow =
        let keyValue = %% +.withSource nameOrKeyword -- ws -- '=' -- ws -- +.expr -%> auto
        %% +.withSource (kw "ROW")
        -- +.(qty.[1..] / tws ',' * keyValue)
        -|> fun { Source = src } pairs ->
            let at v = { Source = src; Value = v }
            let selectStmt =
                {   Compound =
                        CompoundTerm
                            {   Source = src
                                Value = Values ([| at [| for _, e in pairs -> e |] |])
                                Info = ()
                            } |> at
                    With = None
                    OrderBy = None
                    Limit = None
                    Info = ()
                }
            [| for n, _ in pairs -> n |], at selectStmt
    let insertStmtSelect =
        %% +.parenthesizedColumnNames
        -- +.selectStmtWithoutCTE
        -%> auto
    %% +.insertOr
    -- kw "INTO"
    -- +.objectName
    -- +.[ insertStmtRow; insertStmtSelect ]
    -|> fun insert table (cols, data) withClause ->
        {   With = withClause
            Or = insert
            InsertInto = table
            Columns = cols
            Data = data
        } |> InsertStmt

let private createViewStmt =
    %% kw "CREATE"
    -- +.temporary
    -? kw "VIEW"
    -- +.objectName
    -- +.(zeroOrOne * parenthesizedColumnNames)
    -- kw "AS"
    -- +.selectStmt
    -|> fun temp viewName cols asSelect ->
        {   Temporary = Option.isSome temp
            ViewName = viewName
            ColumnNames = cols
            AsSelect = asSelect
        }

let private dropObjectType =
    %[  %% kw "INDEX" -|> DropIndex
        %% kw "TABLE" -|> DropTable
        %% kw "VIEW" -|> DropView
    ]

let private dropObjectStmt =
    %% kw "DROP"
    -? +.dropObjectType
    -- +.objectName
    -|> fun dropType name ->
        { Drop = dropType; ObjectName = name }

let private cteStmt =
    %% +.(zeroOrOne * withClause)
    -- +.[
            deleteStmt
            insertStmt
            updateStmt
            %% +.selectStmtPendingCTE -|>
                fun select withClause -> select withClause |> SelectStmt
        ]
    -|> (|>)

let coreStmt =
    %[  %% +.alterTableStmt -|> AlterTableStmt
        %% +.createIndexStmt -|> CreateIndexStmt
        %% +.createTableStmt -|> CreateTableStmt
        %% +.createViewStmt -|> CreateViewStmt
        %% +.dropObjectStmt -|> DropObjectStmt
        cteStmt
    ]

let coreStmts =
    %% ws
    -- +.(qty.[0..] /. tws ';' * tws coreStmt)
    -|> fun s -> s.ToArray()