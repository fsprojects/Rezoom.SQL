# Quirks

This section is intended to inform readers of potential surprises they may
encounter when using RZSQL, due to differences between database backends.

In general RZSQL tries not to mangle your SQL code too much. Presumably, if you
didn't want a high degree of control over your SQL, you would be using LINQ.

However, RZSQL *does* try to apply a consistent set of data types, query
features, and schema change operations to a very _inconsistent_ set of database
backends. Accordingly there are some areas where square pegs had to be driven
into round holes. These are documented by backend in the following sub-chapters.