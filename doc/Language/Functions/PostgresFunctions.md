# Postgres functions

This is the complete list of built-in functions supported by the `"postgres"`
backend.

Some Postgres functions have been excluded because they operate on data types
RZSQL cannot currently model, such as `range` and `array` types. I hope to one
day add support for these types, but for now this project is **not** a good fit
if you plan to make significant use of these Postgres-specific types.

## abs
    abs(<numeric> a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## acos
    acos(FLOAT64^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## acosd
    acosd(FLOAT64^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## ascii
    ascii(STRING^) -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## asin
    asin(FLOAT64^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## asind
    asind(FLOAT64^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## atan
    atan(FLOAT64^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## atan2
    atan2(FLOAT64^, FLOAT64^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## atan2d
    atan2d(FLOAT64^, FLOAT64^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## atand
    atand(FLOAT64^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## avg
    avg(<numeric> a) -> a?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## bit_and
    bit_and(<integral> a) -> a?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## bit_length
    bit_length(<stringish> a^) -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## bit_or
    bit_or(<integral> a) -> a?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## bool_and
    bool_and(BOOL) -> BOOL
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## bool_or
    bool_or(BOOL) -> BOOL
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## btrim
    btrim(BINARY^, BINARY^{0..1}) -> BINARY
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## cbrt
    cbrt(<numeric>^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## ceil
    ceil(<numeric> a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## ceiling
    ceiling(<numeric> a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## character_length
    character_length(STRING^) -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## char_length
    char_length(STRING^) -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## chr
    chr(INT^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## clock_timestamp
    clock_timestamp() -> <datetimeish>
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## coalesce
    coalesce(a?, a?{0..*}, a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## concat
    concat(<scalar>, <scalar>{0..*}) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## convert
    convert(BINARY^, STRING^, STRING^) -> BINARY
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## convert_from
    convert_from(BINARY^, STRING^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## convert_to
    convert_to(STRING^, STRING^) -> BINARY
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## corr
    corr(FLOAT64, FLOAT64) -> FLOAT64?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## cos
    cos(FLOAT64^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## cosd
    cosd(FLOAT64^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## cot
    cot(FLOAT64^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## cotd
    cotd(FLOAT64^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## count
    count(<scalar>) -> INT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## covar_pop
    covar_pop(FLOAT64, FLOAT64) -> FLOAT64?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## covar_samp
    covar_samp(FLOAT64, FLOAT64) -> FLOAT64?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## current_database
    current_database() -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## current_query
    current_query() -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## current_schema
    current_schema() -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## date_part
    date_part(STRING^, <datetimeish>^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## date_trunc
    date_trunc(STRING^, <datetimeish>^) -> <datetimeish>
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## decode
    decode(STRING^, STRING^) -> BINARY
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## degrees
    degrees(FLOAT64^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## div
    div(<numeric> a^, a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## encode
    encode(BINARY^, STRING^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## every
    every(BOOL) -> BOOL
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## exp
    exp(<numeric> a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## floor
    floor(<numeric> a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## format
    format(STRING^, <scalar>{0..*}) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## get_bit
    get_bit(BINARY^, INT^) -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## get_byte
    get_byte(BINARY^, INT^) -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## greatest
    greatest(a^, a^{0..*}) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## grouping
    grouping(<scalar>, <scalar>{0..*}) -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## has_any_column_privilege
    has_any_column_privilege(STRING^, STRING^) -> BOOL
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## has_column_privilege
    has_column_privilege(STRING^, STRING^, STRING^) -> BOOL
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## has_database_privilege
    has_database_privilege(STRING^, STRING^) -> BOOL
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## has_foreign_data_wrapper_privilege
    has_foreign_data_wrapper_privilege(STRING^, STRING^) -> BOOL
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## has_function_privilege
    has_function_privilege(STRING^, STRING^) -> BOOL
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## has_language_privilege
    has_language_privilege(STRING^, STRING^) -> BOOL
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## has_schema_privilege
    has_schema_privilege(STRING^, STRING^) -> BOOL
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## has_sequence_privilege
    has_sequence_privilege(STRING^, STRING^) -> BOOL
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## has_server_privilege
    has_server_privilege(STRING^, STRING^) -> BOOL
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## has_tablespace_privilege
    has_tablespace_privilege(STRING^, STRING^) -> BOOL
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## has_table_privilege
    has_table_privilege(STRING^, STRING^) -> BOOL
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## has_type_privilege
    has_type_privilege(STRING^, STRING^) -> BOOL
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## impure
    impure(a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|yes|no|

## inet_client_port
    inet_client_port() -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## inet_server_port
    inet_server_port() -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## initcap
    initcap(STRING^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## isfinite
    isfinite(<datetimeish>^) -> BOOL
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## json_agg
    json_agg(<scalar>) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## json_object_agg
    json_object_agg(STRING, <scalar>) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## lastval
    lastval() -> <integral>
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## least
    least(a^, a^{0..*}) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## left
    left(STRING^, INT^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## length
    length(<stringish> a^, STRING^{0..1}) -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## ln
    ln(<numeric> a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## log
    log(<numeric> a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## lower
    lower(STRING^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## lpad
    lpad(STRING^, INT^, STRING^{0..1}) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## ltrim
    ltrim(STRING^, STRING^{0..1}) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## make_timestamp
    make_timestamp(INT^, INT^, INT^, INT^, INT^, FLOAT64^) -> <datetimeish>
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## make_timestamptz
    make_timestamptz(INT^, INT^, INT^, INT^, INT^, FLOAT64^, STRING^{0..1}) -> <datetimeish>
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## max
    max(a) -> a?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## md5
    md5(<stringish> a^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## min
    min(a) -> a?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## mod
    mod(<integral> a^, a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## now
    now() -> <datetimeish>
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## nullable
    nullable(a?) -> a?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|yes|no|

## nullif
    nullif(a, a) -> a?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## octet_length
    octet_length(<stringish> a^) -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## pg_backend_pid
    pg_backend_pid() -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## pg_client_encoding
    pg_client_encoding() -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## pg_conf_load_time
    pg_conf_load_time() -> <datetimeish>
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## pg_has_role
    pg_has_role(STRING^, STRING^) -> BOOL
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## pg_notification_queue_usage
    pg_notification_queue_usage() -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## pg_postmaster_start_time
    pg_postmaster_start_time() -> <datetimeish>
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## pg_trigger_depth
    pg_trigger_depth() -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## pi
    pi() -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## power
    power(<fractional> a^, a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## quote_ident
    quote_ident(STRING^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## quote_literal
    quote_literal(<scalar>^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## quote_nullable
    quote_nullable(<scalar>) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## radians
    radians(FLOAT64^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## random
    random() -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## regexp_replace
    regexp_replace(STRING^, STRING^, STRING^, STRING^{0..1}) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## regr_avgx
    regr_avgx(FLOAT64, FLOAT64) -> FLOAT64?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## regr_avgy
    regr_avgy(FLOAT64, FLOAT64) -> FLOAT64?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## regr_count
    regr_count(FLOAT64, FLOAT64) -> INT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## regr_intercept
    regr_intercept(FLOAT64, FLOAT64) -> FLOAT64?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## regr_r2
    regr_r2(FLOAT64, FLOAT64) -> FLOAT64?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## regr_slope
    regr_slope(FLOAT64, FLOAT64) -> FLOAT64?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## regr_sxx
    regr_sxx(FLOAT64, FLOAT64) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## regr_sxy
    regr_sxy(FLOAT64, FLOAT64) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## regr_syy
    regr_syy(FLOAT64, FLOAT64) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## repeat
    repeat(STRING^, INT^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## replace
    replace(STRING^, STRING^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## reverse
    reverse(STRING^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## right
    right(STRING^, INT^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## round
    round(<numeric> a^, INT^{0..1}) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## row_security_active
    row_security_active(STRING^) -> BOOL
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## rpad
    rpad(STRING^, INT^, STRING^{0..1}) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## rtrim
    rtrim(STRING^, STRING^{0..1}) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## scale
    scale(DECIMAL^) -> DECIMAL
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## set_bit
    set_bit(BINARY^, INT^, INT^) -> BINARY
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## set_byte
    set_byte(BINARY^, INT^, INT^) -> BINARY
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## sign
    sign(<numeric> a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## sin
    sin(FLOAT64^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## sind
    sind(FLOAT64^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## split_part
    split_part(STRING^, STRING^, INT^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## sqrt
    sqrt(<numeric> a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## statement_timestamp
    statement_timestamp() -> <datetimeish>
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## stddev
    stddev(<numeric> a) -> a?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## stddev_pop
    stddev_pop(<numeric> a) -> a?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## stddev_samp
    stddev_samp(<numeric> a) -> a?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## string_agg
    string_agg(<stringish> a, <stringish> a) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## strpos
    strpos(STRING^, STRING^) -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## substr
    substr(STRING^, INT^, INT^{0..1}) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## substring
    substring(<stringish> a^, INT^, INT^{0..1}) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## sum
    sum(<numeric> a) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## tan
    tan(FLOAT64^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## tand
    tand(FLOAT64^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## timeofday
    timeofday() -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## to_ascii
    to_ascii(STRING^, STRING^{0..1}) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## to_char
    to_char(<scalar>^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## to_hex
    to_hex(<integral>) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## to_number
    to_number(STRING^, STRING^) -> <numeric>
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## to_timestamp
    to_timestamp(FLOAT64^) -> <datetimeish>
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## transaction_timestamp
    transaction_timestamp() -> <datetimeish>
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## translate
    translate(STRING^, STRING^, STRING^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## trim
    trim(STRING^, STRING^{0..1}) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## trunc
    trunc(<numeric> a^, INT^{0..1}) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## unsafe_coerce
    unsafe_coerce(<scalar>^) -> <scalar>
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|yes|no|

## upper
    upper(STRING^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## variance
    variance(<numeric> a) -> a?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## var_pop
    var_pop(<numeric> a) -> a?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## var_samp
    var_samp(<numeric> a) -> a?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## version
    version() -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## width_bucket
    width_bucket(<numeric>^, <numeric>^, <numeric>^, INT^) -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

