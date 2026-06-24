---
title: Language Omissions
parent: Language
nav_order: 19
---

<!-- nav-top -->
[Home](../../README.md) &gt; [Language](README.md) &gt; Language Omissions

[&larr; Dynamic SQL](DynamicSQL.md) | [UserTypes &rarr;](../UserTypes/README.md)
<!-- /nav-top -->

# Language Omissions

If you are already familiar with a SQL dialect, here are some features you may
miss that are not currently supported by RZSQL.

For many of these, you can use [vendor statements](VendorStatements.md) to trade
type safety for access to your database backend's special features.

## On the horizon

These features, while not currently supported, fit in well with RZSQL's design
goals and would make sense to add eventually. Pull requests would of course be
appreciated!

* Support for mixing dynamic SQL into static queries, especially `ORDER BY` clause
* Tools to generate an RZSQL migration file from an existing DB
* Window functions
* Table-valued functions
* User-defined functions
* Enums
* Local variables in queries
* Inline (at F# compile-time) views and user-defined functions
* `json` and `xml` data types with some dynamic query operations

## In my dreams

These features are considered desirable, but would be more difficult to fit into
RZSQL's design. I probably won't get around to adding them unless I'm around to
work on this project [a long time](https://www.youtube.com/watch?v=izQB2-Kmiic):

* Interpreted in-memory implementation of each DB backend, for easy unit testing
* Linq-ish query builder using the type provider's database model
* Statically typed (w/ schema) JSON/XML access

## In my nightmares

These features will **never** be added. You can use vendor statements for them
though!

* General-purpose programming features (`IF`, `WHILE`, cursors)
* Stored procedures

---
<!-- nav-bottom -->
[&larr; Dynamic SQL](DynamicSQL.md) | [UserTypes &rarr;](../UserTypes/README.md)
<!-- /nav-bottom -->

