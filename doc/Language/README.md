# Language

This section covers the SQL dialect implemented by Rezoom.SQL.

In a perfect world, this would not be necessary. After all, there is an ANSI standard for SQL, which is even updated on
a regular basis. However, with no disrespect meant to the standard authors (who are not at fault), this is a joke,
because the SQL dialects are used in reality have only a vague resemblance to the standard and to each other.

For this reason, Rezoom.SQL implements yet another SQL dialect, which it can translate to the appropriate syntax for
different backends.

This SQL dialect is based very closely on [SQLite's flavor of SQL](https://www.sqlite.org/lang.html). In fact, it was
implemented by starting with a complete SQLite parser, removing features that don't make sense on other backends, and
adding a couple extensions. I mention this fact because it means if you can't figure out how to do something from this
documentation, you can try looking it up for SQLite and there's a good chance the syntax is identical.






