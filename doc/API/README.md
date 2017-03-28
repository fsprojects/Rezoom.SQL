# API

This section covers the F# API to Rezoom.SQL.

Because most of the complexity is contained in the SQL language, the public API
is rather small. The API surface intended for public use is in the following
namespaces:

* Rezoom.SQL: type providers.

* Rezoom.SQL.Migrations:  migration configuration and utilities.

* Rezoom.SQL.Synchronous: extension methods for running provided SQL commands
  synchronously

* Rezoom.SQL.Asynchronous: extension methods for running provided SQL commands
  asynchronously

* Rezoom.SQL.Plans: extension methods for running provided SQL commands as
  Rezoom `Plan`s.

The following namespaces also contain many members that are technically public,
but are _unstable and undocumented_ as they are intended for internal use. These
are public in part due to technical limitations, and in part because I think you
should be able to use the code as long as you accept that you're coloring
outside the lines and minor version releases may break the API.

* Rezoom.SQL.Compiler: implementation of the RZSQL language, typechecker, backends.

* Rezoom.SQL.Mapping: code used at runtime by RZSQL provided types, primarily
  for deserializing objects from SQL result sets.



