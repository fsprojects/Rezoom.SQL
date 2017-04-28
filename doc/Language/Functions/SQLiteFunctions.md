# SQLite functions

This is the complete list of built-in functions supported by the `"sqlite"`
backend.

## abs
    abs(<numeric> a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## avg
    avg(<numeric> a) -> FLOAT64?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## changes
    changes() -> INT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## char
    char(STRING{0..*}) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## coalesce
    coalesce(a?, a?{0..*}, a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## count
    count(<scalar>) -> INT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## date
    date(STRING, STRING{0..*}) -> STRING?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## datetime
    datetime(STRING, STRING{0..*}) -> STRING?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## glob
    glob(STRING^, STRING^) -> BOOL
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## group_concat
    group_concat(STRING^, STRING{0..1}) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## hex
    hex(BINARY) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## ifnull
    ifnull(a?, a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## impure
    impure(a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|yes|no|

## instr
    instr(<stringish> a^, a^) -> INT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## julianday
    julianday(STRING, STRING{0..*}) -> STRING?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## last_insert_rowid
    last_insert_rowid() -> INT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## length
    length(<stringish>^) -> INT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## like
    like(STRING^, STRING^, STRING^{0..1}) -> BOOL
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## likelihood
    likelihood(BOOL, FLOAT64) -> BOOL
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## likely
    likely(BOOL) -> BOOL
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## lower
    lower(STRING^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## ltrim
    ltrim(STRING^, STRING^{0..1}) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## max
    max(a^, a^{0..*}) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## min
    min(a^, a^{0..*}) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

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

## printf
    printf(STRING^, <scalar>{0..*}) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## quote
    quote(<scalar>) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## random
    random() -> INT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## randomblob
    randomblob() -> BINARY
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## replace
    replace(STRING^, STRING^, STRING^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## round
    round(FLOAT64^, <integral>^{0..1}) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## rtrim
    rtrim(STRING^, STRING^{0..1}) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## soundex
    soundex(STRING^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## sqlite_compileoption_get
    sqlite_compileoption_get(<integral>) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## sqlite_compileoption_used
    sqlite_compileoption_used(STRING^) -> BOOL
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## sqlite_source_id
    sqlite_source_id() -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## sqlite_version
    sqlite_version() -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## strftime
    strftime(STRING, STRING, STRING{0..*}) -> STRING?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## substr
    substr(STRING^, <integral>^, <integral>^{0..1}) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## sum
    sum(<numeric> a) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## time
    time(STRING, STRING{0..*}) -> STRING?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## total
    total(<numeric> a) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## total_changes
    total_changes() -> INT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## trim
    trim(STRING^, <integral>^{0..1}) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## typeof
    typeof(<scalar>) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## unicode
    unicode(STRING^) -> INT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## unlikely
    unlikely(BOOL) -> BOOL
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

## zeroblob
    zeroblob(<integral>) -> BINARY
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

