module SQLFiddle.Persistence
open Rezoom

val saveFiddle : FiddleInput -> FiddleId Plan

val getFiddle : FiddleId -> FiddleInput Plan

