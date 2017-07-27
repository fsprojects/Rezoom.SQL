module Rezoom.SQL.Compiler.Postgres.PostgresFunctions
open Rezoom.SQL.Compiler
open Rezoom.SQL.Compiler.FunctionDeclarations

let functions =
    [|  // math https://www.postgresql.org/docs/9.6/static/functions-math.html
        func "cbrt" [ infect num ] float64
        func "sqrt" [ infect num ] float64
        func "ceil" [ infect (numeric a') ] a'
        func "ceiling" [ infect (numeric a') ] a'
        func "degrees" [ infect float64 ] float64
        func "div" [ infect (numeric a'); infect a' ] a'
        func "exp" [ infect (numeric a') ] a'
        func "floor" [ infect (numeric a') ] a'
        func "ln" [ infect (numeric a') ] a'
        func "log" [ infect (numeric a') ] a'
        func "mod" [ infect (intish a'); infect a' ] a'
        func "pi" [] float64
        func "power" [ infect (fracish a'); infect a' ] a'
        func "radians" [ infect float64 ] float64
        func "round" [ infect (numeric a'); optional (infect int32) ] a'
        func "scale" [ infect decimal ] decimal
        func "sign" [ infect (numeric a') ] a'
        func "sqrt" [ infect (numeric a') ] a'
        func "trunc" [ infect (numeric a'); optional (infect int32) ] a'
        func "width_bucket" [ infect num; infect num; infect num; infect int32 ] int32

        proc "random" [] float64
        // proc "setseed" [ float64 ] void // can't model void results

        func "acos" [ infect float64 ] float64
        func "acosd" [ infect float64 ] float64
        func "asin" [ infect float64 ] float64
        func "asind" [ infect float64 ] float64
        func "atan" [ infect float64 ] float64
        func "atand" [ infect float64 ] float64
        func "atan2" [ infect float64; infect float64 ] float64
        func "atan2d" [ infect float64; infect float64 ] float64
        func "cos" [ infect float64 ] float64
        func "cosd" [ infect float64 ] float64
        func "cot" [ infect float64 ] float64
        func "cotd" [ infect float64 ] float64
        func "sin" [ infect float64 ] float64
        func "sind" [ infect float64 ] float64
        func "tan" [ infect float64 ] float64
        func "tand" [ infect float64 ] float64

        // string functions https://www.postgresql.org/docs/9.6/static/functions-string.html
        func "bit_length" [ infect (stringish a') ] int32
        func "char_length" [ infect string ] int32
        func "character_length" [ infect string ] int32
        func "lower" [ infect string ] string
        func "octet_length" [ infect (stringish a') ] int32 // this works on BYTEA too
        // func "overlay" ... // wacky syntax! would have to do more work for this like TSQL special funcs
        // func "position" ... // wacky syntax: position('needle' in 'haystack')
        // substring has wacky syntax in documentation, but works fine without it
        func "substring" [ infect (stringish a'); infect int32; optional (infect int32) ] a'
        // trim has wacky syntax in documentation, but works fine without it
        // (except we can't specify leading/trailing)
        func "trim" [ infect string; optional (infect string) ] string
        func "upper" [ infect string ] string
        func "ascii" [ infect string ] int32
        func "chr" [ infect int32] string
        func "concat" [ scalar; vararg scalar ] string
        func "convert" [ infect binary; infect string; infect string ] binary
        func "convert_from" [ infect binary; infect string ] string
        func "convert_to" [ infect string; infect string ] binary
        func "decode" [ infect string; infect string ] binary
        func "encode" [ infect binary; infect string ] string
        func "format" [ infect string; vararg scalar ] string
        func "initcap" [ infect string ] string
        func "left" [ infect string; infect int32 ] string
        func "length" [ infect (stringish a'); optional (infect string) ] int32
        func "lpad" [ infect string; infect int32; optional (infect string) ] string
        func "ltrim" [ infect string; optional (infect string) ] string
        func "md5" [ infect (stringish a') ] string
        proc "pg_client_encoding" [] string
        // questionably useful in static SQL
        func "quote_ident" [ infect string ] string
        func "quote_literal" [ infect scalar ] string
        func "quote_nullable" [ scalar ] string
        // func "regexp_matches" // cannot represent TVF
        func "regexp_replace" [ infect string; infect string; infect string; optional (infect string) ] string
        func "repeat" [ infect string; infect int32 ] string
        func "replace" [ infect string; infect string ] string
        func "reverse" [ infect string ] string
        func "right" [ infect string; infect int32 ] string
        func "rpad" [ infect string; infect int32; optional (infect string) ] string
        func "rtrim" [ infect string; optional (infect string) ] string
        func "split_part" [ infect string; infect string; infect int32 ] string
        func "strpos" [ infect string; infect string ] int32
        func "substr" [ infect string; infect int32; optional (infect int32) ] string
        func "to_ascii" [ infect string; optional (infect string) ] string
        func "to_hex" [ integral ] string
        func "translate" [ infect string; infect string; infect string ] string

        // binary string functions https://www.postgresql.org/docs/9.6/static/functions-binarystring.html
        func "btrim" [ infect binary; optional (infect binary) ] binary
        func "get_bit" [ infect binary; infect int32 ] int32
        func "get_byte" [ infect binary; infect int32 ] int32
        func "set_bit" [ infect binary; infect int32; infect int32 ] binary
        func "set_byte" [ infect binary; infect int32; infect int32 ] binary

        // aggregate functions
        aggregate "avg" [ numeric a' ] (nullable a')
        aggregateW "count" [ scalar ] int64
        aggregate "max" [ a' ] (nullable a')
        aggregate "min" [ a' ] (nullable a')
        aggregate "sum" [ numeric a' ] a'
    |] |> DefaultFunctions.extendedBy