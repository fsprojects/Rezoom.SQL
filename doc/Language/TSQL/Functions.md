# T-SQL functions

This is the complete list of built-in functions supported by the `"tsql"`
backend.

A function type signature is documented as:

    name(arg1, arg2... argN) -> ret

`arg1` through `argN` and `ret` describe the types of the function's parameters
and return value. These are best explained through examples:

    INT

A value of the INT [data type](../DataTypes.md).

    <numeric>

A value of any data type under the `<numeric>` part of the [data types
tree](../DataTypes.md).

    <numeric>^

Same as above, but the caret indicates that if this argument is inferred to be
nullable, the function's return value will also be inferred to be nullable.

    <numeric>?

The expression given for this argument will be assumed to be nullable. This can
cause bind parameters to be made nullable.

    a

A type variable. All occurences of the same type variable in the function type
signature must be the exact same type. So for example, this function can take
two INTs or two FLOAT64s but not one INT and one FLOAT64:

    example(a, a) -> BOOL

Type variables can also be constrained to a subset of data types, like so:

    <numeric> a

Hopefully this gets the idea across well enough to decipher the actual function signatures.

# Functions in alphabetical order

## abs
    abs(<numeric> a^) -> a
Idempotent: yes
Erased: no
Aggregate: no

## acos
    acos(<fractional>^) -> FLOAT64
Idempotent: yes
Erased: no
Aggregate: no

## ascii
    ascii(STRING^) -> INT
Idempotent: yes
Erased: no
Aggregate: no

## asin
    asin(<fractional>^) -> FLOAT64
Idempotent: yes
Erased: no
Aggregate: no

## atan
    atan(<fractional>^) -> FLOAT64
Idempotent: yes
Erased: no
Aggregate: no

## atn2
    atn2(<fractional>^, <fractional>^) -> FLOAT64
Idempotent: yes
Erased: no
Aggregate: no

## avg
    avg(<numeric> a) -> a?
Idempotent: yes
Erased: no
Aggregate: yes

## ceiling
    ceiling(<numeric> a^) -> a
Idempotent: yes
Erased: no
Aggregate: no

## char
    char(<integral>^) -> STRING
Idempotent: yes
Erased: no
Aggregate: no

## charindex
    charindex(STRING^, STRING^, <integral>{0..1}) -> <integral>
Idempotent: yes
Erased: no
Aggregate: no

## choose
    choose(<integral>^, a^{0..*}) -> a
Idempotent: yes
Erased: no
Aggregate: no

## coalesce
    coalesce(a?, a?{0..*}, a^) -> a
Idempotent: yes
Erased: no
Aggregate: no

## concat
    concat(STRING, STRING, STRING{0..*}) -> STRING
Idempotent: yes
Erased: no
Aggregate: no

## concat_ws
    concat_ws(STRING^, <scalar>, <scalar>, <scalar>{0..*}) -> STRING
Idempotent: yes
Erased: no
Aggregate: no

## cos
    cos(<fractional>^) -> FLOAT64
Idempotent: yes
Erased: no
Aggregate: no

## cot
    cot(<fractional>^) -> FLOAT64
Idempotent: yes
Erased: no
Aggregate: no

## count
    count(<scalar>) -> INT
Idempotent: yes
Erased: no
Aggregate: yes

## count_big
    count_big(<scalar>) -> INT64
Idempotent: yes
Erased: no
Aggregate: yes

## current_timestamp
    current_timestamp() -> DATETIME
Idempotent: no
Erased: no
Aggregate: no

## cursor_rows
    cursor_rows() -> INT
Idempotent: no
Erased: no
Aggregate: no

## datalength
    datalength(STRING^) -> <integral>
Idempotent: yes
Erased: no
Aggregate: no

## dateadd
    dateadd(STRING, <integral>^, DATETIME^) -> DATETIME
Idempotent: yes
Erased: no
Aggregate: no

## datediff
    datediff(STRING, DATETIME^, DATETIME^) -> INT
Idempotent: yes
Erased: no
Aggregate: no

## datediff_big
    datediff_big(STRING, DATETIME^, DATETIME^) -> INT64
Idempotent: yes
Erased: no
Aggregate: no

## datefirst
    datefirst() -> INT8
Idempotent: no
Erased: no
Aggregate: no

## datefromparts
    datefromparts(<integral>^, <integral>^, <integral>^) -> DATETIME
Idempotent: yes
Erased: no
Aggregate: no

## datename
    datename(STRING, DATETIME^) -> STRING
Idempotent: yes
Erased: no
Aggregate: no

## datetime2fromparts
    datetime2fromparts(<integral>^, <integral>^, <integral>^, <integral>^, <integral>^, <integral>^, <integral>^, <integral>^) -> DATETIME
Idempotent: yes
Erased: no
Aggregate: no

## datetimefromparts
    datetimefromparts(<integral>^, <integral>^, <integral>^, <integral>^, <integral>^, <integral>^, <integral>^) -> DATETIME
Idempotent: yes
Erased: no
Aggregate: no

## datetimeoffsetfromparts
    datetimeoffsetfromparts(<integral>^, <integral>^, <integral>^, <integral>^, <integral>^, <integral>^, <integral>^, <integral>^, <integral>^, <integral>^) -> DATETIMEOFFSET
Idempotent: yes
Erased: no
Aggregate: no

## day
    day(DATETIME^) -> <integral>
Idempotent: yes
Erased: no
Aggregate: no

## dbts
    dbts() -> BINARY
Idempotent: no
Erased: no
Aggregate: no

## degrees
    degrees(<numeric> a^) -> a
Idempotent: yes
Erased: no
Aggregate: no

## difference
    difference(STRING^, STRING^) -> INT
Idempotent: yes
Erased: no
Aggregate: no

## exp
    exp(<fractional>^) -> FLOAT64
Idempotent: yes
Erased: no
Aggregate: no

## fetch_status
    fetch_status() -> INT
Idempotent: no
Erased: no
Aggregate: no

## floor
    floor(<numeric> a^) -> a
Idempotent: yes
Erased: no
Aggregate: no

## format
    format(<scalar>^, STRING^, STRING^{0..1}) -> STRING
Idempotent: yes
Erased: no
Aggregate: no

## getdate
    getdate() -> DATETIME
Idempotent: no
Erased: no
Aggregate: no

## getutcdate
    getutcdate() -> DATETIME
Idempotent: no
Erased: no
Aggregate: no

## grouping
    grouping(<scalar>) -> INT8
Idempotent: yes
Erased: no
Aggregate: yes

## grouping_id
    grouping_id(<scalar>{0..*}) -> INT
Idempotent: yes
Erased: no
Aggregate: yes

## identity
    identity() -> <integral>
Idempotent: no
Erased: no
Aggregate: no

## iif
    iif(BOOL, a^, a^) -> a
Idempotent: yes
Erased: no
Aggregate: no

## impure
    impure(a^) -> a
Idempotent: no
Erased: yes
Aggregate: no

## isjson
    isjson(STRING^) -> BOOL
Idempotent: yes
Erased: no
Aggregate: no

## json_modify
    json_modify(STRING^, STRING^, STRING^) -> STRING
Idempotent: yes
Erased: no
Aggregate: no

## json_query
    json_query(STRING^, STRING^) -> STRING
Idempotent: yes
Erased: no
Aggregate: no

## json_value
    json_value(STRING^, STRING^) -> STRING
Idempotent: yes
Erased: no
Aggregate: no

## langid
    langid() -> INT8
Idempotent: no
Erased: no
Aggregate: no

## language
    language() -> STRING
Idempotent: no
Erased: no
Aggregate: no

## left
    left(STRING^, <integral>^) -> STRING
Idempotent: yes
Erased: no
Aggregate: no

## len
    len(STRING^) -> <integral>
Idempotent: yes
Erased: no
Aggregate: no

## lock_timeout
    lock_timeout() -> INT
Idempotent: no
Erased: no
Aggregate: no

## log
    log(<numeric>^, <integral>^{0..1}) -> FLOAT64
Idempotent: yes
Erased: no
Aggregate: no

## log10
    log10(<numeric>^) -> FLOAT64
Idempotent: yes
Erased: no
Aggregate: no

## lower
    lower(STRING^) -> STRING
Idempotent: yes
Erased: no
Aggregate: no

## ltrim
    ltrim(STRING^) -> STRING
Idempotent: yes
Erased: no
Aggregate: no

## max
    max(a) -> a?
Idempotent: yes
Erased: no
Aggregate: yes

## max_connections
    max_connections() -> INT
Idempotent: no
Erased: no
Aggregate: no

## max_precision
    max_precision() -> INT8
Idempotent: no
Erased: no
Aggregate: no

## min
    min(a) -> a?
Idempotent: yes
Erased: no
Aggregate: yes

## month
    month(DATETIME^) -> <integral>
Idempotent: yes
Erased: no
Aggregate: no

## nchar
    nchar(<integral>^) -> STRING
Idempotent: yes
Erased: no
Aggregate: no

## nestlevel
    nestlevel() -> INT
Idempotent: no
Erased: no
Aggregate: no

## nullable
    nullable(a?) -> a?
Idempotent: yes
Erased: yes
Aggregate: no

## options
    options() -> INT
Idempotent: no
Erased: no
Aggregate: no

## patindex
    patindex(STRING^, STRING^) -> <integral>
Idempotent: yes
Erased: no
Aggregate: no

## pi
    pi() -> FLOAT64
Idempotent: yes
Erased: no
Aggregate: no

## power
    power(<numeric> a^, <numeric>^) -> a
Idempotent: yes
Erased: no
Aggregate: no

## quotename
    quotename(STRING^, STRING^{0..1}) -> STRING
Idempotent: yes
Erased: no
Aggregate: no

## radians
    radians(<numeric> a^) -> a
Idempotent: yes
Erased: no
Aggregate: no

## rand
    rand(<integral>^{0..1}) -> FLOAT64
Idempotent: yes
Erased: no
Aggregate: no

## remserver
    remserver() -> STRING
Idempotent: no
Erased: no
Aggregate: no

## replace
    replace(STRING^, STRING^, STRING^) -> STRING
Idempotent: yes
Erased: no
Aggregate: no

## replicate
    replicate(STRING^, <integral>^) -> STRING
Idempotent: yes
Erased: no
Aggregate: no

## reverse
    reverse(STRING^) -> STRING
Idempotent: yes
Erased: no
Aggregate: no

## right
    right(STRING^, <integral>^) -> STRING
Idempotent: yes
Erased: no
Aggregate: no

## round
    round(<numeric> a^, <integral>^) -> a
Idempotent: yes
Erased: no
Aggregate: no

## rtrim
    rtrim(STRING^) -> STRING
Idempotent: yes
Erased: no
Aggregate: no

## scope_identity
    scope_identity() -> <integral>
Idempotent: no
Erased: no
Aggregate: no

## servername
    servername() -> STRING
Idempotent: no
Erased: no
Aggregate: no

## servicename
    servicename() -> STRING
Idempotent: no
Erased: no
Aggregate: no

## sign
    sign(<numeric> a^) -> a
Idempotent: yes
Erased: no
Aggregate: no

## sin
    sin(<fractional>^) -> FLOAT64
Idempotent: yes
Erased: no
Aggregate: no

## smalldatetimefromparts
    smalldatetimefromparts(<integral>^, <integral>^, <integral>^, <integral>^, <integral>^) -> DATETIME
Idempotent: yes
Erased: no
Aggregate: no

## soundex
    soundex(STRING^) -> STRING
Idempotent: yes
Erased: no
Aggregate: no

## space
    space(<integral>^) -> STRING
Idempotent: yes
Erased: no
Aggregate: no

## spid
    spid() -> INT8
Idempotent: no
Erased: no
Aggregate: no

## sqrt
    sqrt(<numeric> a^) -> FLOAT64
Idempotent: yes
Erased: no
Aggregate: no

## square
    square(<numeric> a^) -> FLOAT64
Idempotent: yes
Erased: no
Aggregate: no

## stdev
    stdev(<numeric>) -> FLOAT64?
Idempotent: yes
Erased: no
Aggregate: yes

## stdevp
    stdevp(<numeric>) -> FLOAT64?
Idempotent: yes
Erased: no
Aggregate: yes

## str
    str(<fractional>^, <integral>{0..2}) -> STRING
Idempotent: yes
Erased: no
Aggregate: no

## string_escape
    string_escape(STRING^, STRING^) -> STRING
Idempotent: yes
Erased: no
Aggregate: no

## stuff
    stuff(<stringish> a^, <integral>^, <integral>^, STRING) -> a
Idempotent: yes
Erased: no
Aggregate: no

## substring
    substring(<stringish> a^, <integral>^, <integral>^) -> a
Idempotent: yes
Erased: no
Aggregate: no

## sum
    sum(<numeric> a) -> a
Idempotent: yes
Erased: no
Aggregate: yes

## sysdatetime
    sysdatetime() -> DATETIME
Idempotent: no
Erased: no
Aggregate: no

## sysdatetimeoffset
    sysdatetimeoffset() -> DATETIMEOFFSET
Idempotent: no
Erased: no
Aggregate: no

## sysutcdatetime
    sysutcdatetime() -> DATETIME
Idempotent: no
Erased: no
Aggregate: no

## tan
    tan(<fractional>^) -> FLOAT64
Idempotent: yes
Erased: no
Aggregate: no

## textsize
    textsize() -> INT
Idempotent: no
Erased: no
Aggregate: no

## translate
    translate(STRING^, STRING^, STRING^) -> STRING
Idempotent: yes
Erased: no
Aggregate: no

## trim
    trim(STRING^) -> STRING
Idempotent: yes
Erased: no
Aggregate: no

## unicode
    unicode(STRING^) -> INT
Idempotent: yes
Erased: no
Aggregate: no

## unsafe_coerce
    unsafe_coerce(<scalar>^) -> <scalar>
Idempotent: yes
Erased: yes
Aggregate: no

## upper
    upper(STRING^) -> STRING
Idempotent: yes
Erased: no
Aggregate: no

## var
    var(<numeric>) -> FLOAT64?
Idempotent: yes
Erased: no
Aggregate: yes

## varp
    varp(<numeric>) -> FLOAT64?
Idempotent: yes
Erased: no
Aggregate: yes

## version
    version() -> STRING
Idempotent: no
Erased: no
Aggregate: no

## year
    year(DATETIME^) -> <integral>
Idempotent: yes
Erased: no
Aggregate: no

