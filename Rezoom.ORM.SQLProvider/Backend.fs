namespace Rezoom.ORM.SQLProvider
open System
open System.Collections.Generic
open Rezoom.ORM
open SQLow

type IBackend =
    abstract member ToCommandFragments : Stmt IReadOnlyList -> CommandFragment IReadOnlyList