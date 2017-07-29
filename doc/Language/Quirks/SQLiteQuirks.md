# SQLite Quirks

Here are some surprises you may encounter when using RZSQL with SQLite.

## Decimal is not supported

SQLite has no way to perform math on decimal numbers. It can do math on integers
or IEEE floating point numbers.

Decimals could be stored as strings behind the scenes, but operations like
multiplication would typecheck normally at RZSQL compile time while potentially
leading to catastrophic loss of precision at runtime.

If you need to store decimal numbers in SQLite, you should do it yourself (as
strings or fixed-point integers) so you don't inadvertently rely on SQLite to do
floating-point math in a radix and precision that it cannot support.

## DateTimeOffset is not supported

On SQLite, RZSQL supports storing UTC `DateTime`s, represented under the covers
as ISO8601 strings.

`DateTimeOffset` could have been supported in the same way, but comparisons
would be incorrect. Two `DateTimeOffset` values that represent the same UTC time
should compare equal, even if their offsets were different. This would not be
possible using a string representation.

## Most forms of ALTER TABLE are not supported

Only `ALTER TABLE RENAME TO` and `ALTER TABLE ADD COLUMN` work on SQLite. This
matches the actual support for schema changes in this database, so it won't be
"fixed" unless SQLite itself is someday "fixed".


