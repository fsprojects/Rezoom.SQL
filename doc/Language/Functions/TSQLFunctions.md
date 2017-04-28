# T-SQL functions

This is the complete list of built-in functions supported by the `"tsql"`
backend.

## abs
    abs(<numeric> a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## acos
    acos(<fractional>^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## ascii
    ascii(STRING^) -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## asin
    asin(<fractional>^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## atan
    atan(<fractional>^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## atn2
    atn2(<fractional>^, <fractional>^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## avg
    avg(<numeric> a) -> a?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## ceiling
    ceiling(<numeric> a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## char
    char(<integral>^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## charindex
    charindex(STRING^, STRING^, <integral>{0..1}) -> <integral>
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## choose
    choose(<integral>^, a^{0..*}) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## coalesce
    coalesce(a?, a?{0..*}, a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## concat
    concat(STRING, STRING, STRING{0..*}) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## concat_ws
    concat_ws(STRING^, <scalar>, <scalar>, <scalar>{0..*}) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## cos
    cos(<fractional>^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## cot
    cot(<fractional>^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## count
    count(<scalar>) -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## count_big
    count_big(<scalar>) -> INT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## current_timestamp
    current_timestamp() -> DATETIME
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## cursor_rows
    cursor_rows() -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## datalength
    datalength(STRING^) -> <integral>
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## dateadd
    dateadd(STRING, <integral>^, DATETIME^) -> DATETIME
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## datediff
    datediff(STRING, DATETIME^, DATETIME^) -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## datediff_big
    datediff_big(STRING, DATETIME^, DATETIME^) -> INT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## datefirst
    datefirst() -> INT8
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## datefromparts
    datefromparts(<integral>^, <integral>^, <integral>^) -> DATETIME
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## datename
    datename(STRING, DATETIME^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## datetime2fromparts
    datetime2fromparts(<integral>^, <integral>^, <integral>^, <integral>^, <integral>^, <integral>^, <integral>^, <integral>^) -> DATETIME
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## datetimefromparts
    datetimefromparts(<integral>^, <integral>^, <integral>^, <integral>^, <integral>^, <integral>^, <integral>^) -> DATETIME
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## datetimeoffsetfromparts
    datetimeoffsetfromparts(<integral>^, <integral>^, <integral>^, <integral>^, <integral>^, <integral>^, <integral>^, <integral>^, <integral>^, <integral>^) -> DATETIMEOFFSET
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## day
    day(DATETIME^) -> <integral>
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## dbts
    dbts() -> BINARY
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## degrees
    degrees(<numeric> a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## difference
    difference(STRING^, STRING^) -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## exp
    exp(<fractional>^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## fetch_status
    fetch_status() -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## floor
    floor(<numeric> a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## format
    format(<scalar>^, STRING^, STRING^{0..1}) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## getdate
    getdate() -> DATETIME
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## getutcdate
    getutcdate() -> DATETIME
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## grouping
    grouping(<scalar>) -> INT8
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## grouping_id
    grouping_id(<scalar>{0..*}) -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## identity
    identity() -> <integral>
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## iif
    iif(BOOL, a^, a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## impure
    impure(a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|yes|no|

## isjson
    isjson(STRING^) -> BOOL
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## json_modify
    json_modify(STRING^, STRING^, STRING^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## json_query
    json_query(STRING^, STRING^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## json_value
    json_value(STRING^, STRING^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## langid
    langid() -> INT8
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## language
    language() -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## left
    left(STRING^, <integral>^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## len
    len(STRING^) -> <integral>
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## lock_timeout
    lock_timeout() -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## log
    log(<numeric>^, <integral>^{0..1}) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## log10
    log10(<numeric>^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## lower
    lower(STRING^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## ltrim
    ltrim(STRING^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## max
    max(a) -> a?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## max_connections
    max_connections() -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## max_precision
    max_precision() -> INT8
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## min
    min(a) -> a?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## month
    month(DATETIME^) -> <integral>
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## nchar
    nchar(<integral>^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## nestlevel
    nestlevel() -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## nullable
    nullable(a?) -> a?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|yes|no|

## options
    options() -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## patindex
    patindex(STRING^, STRING^) -> <integral>
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## pi
    pi() -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## power
    power(<numeric> a^, <numeric>^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## quotename
    quotename(STRING^, STRING^{0..1}) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## radians
    radians(<numeric> a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## rand
    rand(<integral>^{0..1}) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## remserver
    remserver() -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## replace
    replace(STRING^, STRING^, STRING^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## replicate
    replicate(STRING^, <integral>^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## reverse
    reverse(STRING^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## right
    right(STRING^, <integral>^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## round
    round(<numeric> a^, <integral>^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## rtrim
    rtrim(STRING^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## scope_identity
    scope_identity() -> <integral>
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## servername
    servername() -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## servicename
    servicename() -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## sign
    sign(<numeric> a^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## sin
    sin(<fractional>^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## smalldatetimefromparts
    smalldatetimefromparts(<integral>^, <integral>^, <integral>^, <integral>^, <integral>^) -> DATETIME
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## soundex
    soundex(STRING^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## space
    space(<integral>^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## spid
    spid() -> INT8
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## sqrt
    sqrt(<numeric> a^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## square
    square(<numeric> a^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## stdev
    stdev(<numeric>) -> FLOAT64?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## stdevp
    stdevp(<numeric>) -> FLOAT64?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## str
    str(<fractional>^, <integral>{0..2}) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## string_escape
    string_escape(STRING^, STRING^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## stuff
    stuff(<stringish> a^, <integral>^, <integral>^, STRING) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## substring
    substring(<stringish> a^, <integral>^, <integral>^) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## sum
    sum(<numeric> a) -> a
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## sysdatetime
    sysdatetime() -> DATETIME
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## sysdatetimeoffset
    sysdatetimeoffset() -> DATETIMEOFFSET
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## sysutcdatetime
    sysutcdatetime() -> DATETIME
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## tan
    tan(<fractional>^) -> FLOAT64
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## textsize
    textsize() -> INT
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## translate
    translate(STRING^, STRING^, STRING^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## trim
    trim(STRING^) -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

## unicode
    unicode(STRING^) -> INT
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

## var
    var(<numeric>) -> FLOAT64?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## varp
    varp(<numeric>) -> FLOAT64?
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|yes|

## version
    version() -> STRING
| Idempotent | Erased | Aggregate |
|-|-|-|
|no|no|no|

## year
    year(DATETIME^) -> <integral>
| Idempotent | Erased | Aggregate |
|-|-|-|
|yes|no|no|

