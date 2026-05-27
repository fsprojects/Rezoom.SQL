# Rezoom.SQL.Compiler

Contains the AST, parser, and typechecker for the Rezoom SQL dialect.

Includes the backend implementations e.g. TSQL, Postgres, etc. that
translate the typechecked AST to raw command fragments.

Defines the compile-time configuration (rzsql.json) and logic to
calculate a user's database model from their migration scripts.