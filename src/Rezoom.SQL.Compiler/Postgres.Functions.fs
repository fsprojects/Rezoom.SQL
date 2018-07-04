module Rezoom.SQL.Compiler.Postgres.PostgresFunctions
open Rezoom.SQL.Compiler
open Rezoom.SQL.Compiler.FunctionDeclarations

let functions =
    [|  proc "lastval" [] integral
        func "nullif" [ a'; a' ] (nullable a')
        func "greatest" [ infect a'; infect (vararg a') ] a'
        func "least" [ infect a'; infect (vararg a') ] a'

        // math https://www.postgresql.org/docs/9.6/static/functions-math.html
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

        // formatting functions https://www.postgresql.org/docs/current/static/functions-formatting.html
        func "to_char" [ infect scalar ] string
        // func "to_date" [ infect string; infect string ] date // we don't have a date type
        func "to_number" [ infect string; infect string ] num
        func "to_timestamp" [ infect scalar; optional (infect string) ] datetimey

        // date/time functions https://www.postgresql.org/docs/current/static/functions-datetime.html
        // func "age" [ infect datetimey; infect datetimey ] interval // we don't have interval types
        proc "clock_timestamp" [] datetimey
        // TODO: translate without parens
        // proc "current_timestamp" [] datetimey
        func "date_part" [ infect string; infect datetimey ] float64
        func "date_trunc" [ infect string; infect datetimey ] datetimey
        // TODO: handle funky syntax extract(hour from timestamp '...')
        // func "extract" [ string; datetimey ] float64
        func "isfinite" [ infect datetimey ] boolean
        // no justify_whatever since we don't have intervals
        func "make_timestamp"
            (List.map infect [ int32; int32; int32; int32; int32; float64 ]) datetimey
        func "make_timestamptz"
            (List.map infect [ int32; int32; int32; int32; int32; float64; optional string ]) datetimey
        proc "now" [] datetimey
        proc "statement_timestamp" [] datetimey
        proc "timeofday" [] string
        proc "transaction_timestamp" [] datetimey
        func "to_timestamp" [ infect float64 ] datetimey

        // no enum, array, range, or geometric functions because we can't handle those types

        // no full text search or xml functions yet -- might want to handle these later
        // https://www.postgresql.org/docs/current/static/functions-textsearch.html
        // https://www.postgresql.org/docs/current/static/functions-xml.html
        // https://www.postgresql.org/docs/current/static/functions-json.html

        // aggregate functions
        aggregate "avg" [ numeric a' ] (nullable a')
        aggregateW "count" [ scalar ] int64
        aggregate "max" [ a' ] (nullable a')
        aggregate "min" [ a' ] (nullable a')
        aggregate "sum" [ numeric a' ] a'
        aggregate "bit_and" [ intish a' ] (nullable a')
        aggregate "bit_or" [ intish a' ] (nullable a')
        aggregate "bool_and" [ boolean ] boolean
        aggregate "bool_or" [ boolean ] boolean
        aggregate "every" [ boolean ] boolean
        aggregate "json_agg" [ scalar ] string // pretend json is a string... 
        aggregate "json_object_agg" [ string; scalar ] string
        aggregate "string_agg" [ stringish a'; stringish a' ] a'
        // statistical aggregate functions
        aggregate "corr" [ float64; float64 ] (nullable float64)
        aggregate "covar_pop" [ float64; float64 ] (nullable float64)
        aggregate "covar_samp" [ float64; float64 ] (nullable float64)
        aggregate "regr_avgx" [ float64; float64 ] (nullable float64)
        aggregate "regr_avgy" [ float64; float64 ] (nullable float64)
        aggregate "regr_count" [ float64; float64 ] int64
        aggregate "regr_intercept" [ float64; float64 ] (nullable float64)
        aggregate "regr_r2" [ float64; float64 ] (nullable float64)
        aggregate "regr_slope" [ float64; float64 ] (nullable float64)
        aggregate "regr_sxx" [ float64; float64 ] float64
        aggregate "regr_sxy" [ float64; float64 ] float64
        aggregate "regr_syy" [ float64; float64 ] float64
        aggregate "stddev" [ numeric a' ] (nullable a')
        aggregate "stddev_pop" [ numeric a' ] (nullable a')
        aggregate "stddev_samp" [ numeric a' ] (nullable a')
        aggregate "variance" [ numeric a' ] (nullable a')
        aggregate "var_pop" [ numeric a' ] (nullable a')
        aggregate "var_samp" [ numeric a' ] (nullable a')
        aggregate "grouping" [ scalar; vararg scalar ] int32

        // system functions https://www.postgresql.org/docs/current/static/functions-info.html
        proc "current_database" [] string
        proc "current_query" [] string
        proc "current_schema" [] string
        proc "pg_conf_load_time" [] datetimey
        // can't do inet_client_addr because it returns inet
        proc "inet_client_port" [] int32
        // can't do inet_server_addr because it returns inet
        proc "inet_server_port" [] int32
        proc "pg_backend_pid" [] int32
        proc "pg_notification_queue_usage" [] float64
        proc "pg_postmaster_start_time" [] datetimey
        proc "pg_trigger_depth" [] int32
        proc "version" [] string

        proc "has_any_column_privilege" [ infect string; infect string; infect string ] boolean
        proc "has_any_column_privilege" [ infect string; infect string ] boolean
        proc "has_column_privilege"  [ infect string; infect string; infect string; infect string ] boolean
        proc "has_column_privilege" [ infect string; infect string; infect string ] boolean
        proc "has_database_privilege" [ infect string; infect string; infect string ] boolean
        proc "has_database_privilege" [ infect string; infect string ] boolean
        proc "has_foreign_data_wrapper_privilege" [ infect string; infect string; infect string ] boolean
        proc "has_foreign_data_wrapper_privilege" [ infect string; infect string ] boolean
        proc "has_function_privilege" [ infect string; infect string; infect string ] boolean
        proc "has_function_privilege" [ infect string; infect string ] boolean
        proc "has_language_privilege" [ infect string; infect string; infect string ] boolean
        proc "has_language_privilege" [ infect string; infect string ] boolean
        proc "has_schema_privilege" [ infect string; infect string; infect string ] boolean
        proc "has_schema_privilege" [ infect string; infect string ] boolean
        proc "has_sequence_privilege" [ infect string; infect string; infect string ] boolean
        proc "has_sequence_privilege" [ infect string; infect string ] boolean
        proc "has_server_privilege" [ infect string; infect string; infect string ] boolean
        proc "has_server_privilege" [ infect string; infect string ] boolean
        proc "has_table_privilege" [ infect string; infect string; infect string ] boolean
        proc "has_table_privilege" [ infect string; infect string ] boolean
        proc "has_tablespace_privilege" [ infect string; infect string; infect string ] boolean
        proc "has_tablespace_privilege" [ infect string; infect string ] boolean
        proc "has_type_privilege" [ infect string; infect string; infect string ] boolean
        proc "has_type_privilege" [ infect string; infect string ] boolean
        proc "pg_has_role" [ infect string; infect string; infect string ] boolean
        proc "pg_has_role" [ infect string; infect string ] boolean
        proc "row_security_active" [ infect string ] boolean

        // uuid-ossp https://www.postgresql.org/docs/current/static/uuid-ossp.html
        proc "uuid_generate_v1" [] guid
        proc "uuid_generate_v1mc" [] guid
        func "uuid_generate_v3" [ infect guid; infect string ] guid
        proc "uuid_generate_v4" [] guid
        func "uuid_generate_v5" [ infect guid; infect string ] guid
        func "uuid_nil" [] guid
        func "uuid_ns_dns" [] guid
        func "uuid_ns_url" [] guid
        func "uuid_ns_oid" [] guid
        func "uuid_ns_x500" [] guid
    |] |> DefaultFunctions.extendedBy