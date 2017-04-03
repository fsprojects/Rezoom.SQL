module SQLFiddle.Domain
open Rezoom

val saveFiddle : FiddleInput -> FiddleId Plan

val getFiddle : FiddleId -> CheckedFiddle Plan

val checkFiddle : FiddleInput -> CheckedFiddle Plan