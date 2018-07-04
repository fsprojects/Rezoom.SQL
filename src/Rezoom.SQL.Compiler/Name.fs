namespace Rezoom.SQL.Compiler
open System
open System.Collections.Generic

type Name(str : string) =
    member inline private __.String = str
    member inline private __.InlineEquals(other : Name) =
        str.Equals(other.String, StringComparison.OrdinalIgnoreCase)
    member inline private __.InlineCompareTo(other : Name) =
        String.Compare(str, other.String, StringComparison.OrdinalIgnoreCase)
    member this.Value = str
    member this.Equals(other) = this.InlineEquals(other)
    member this.CompareTo(other) = this.InlineCompareTo(other)
    override __.ToString() = str
    override this.Equals(other : obj) =
        match other with 
        | :? Name as name -> this.InlineEquals(name)
        | _ -> false
    override this.GetHashCode() =
        StringComparer.OrdinalIgnoreCase.GetHashCode(str)
    interface IEquatable<Name> with
        member this.Equals(name) = this.InlineEquals(name)
    interface IComparable<Name> with
        member this.CompareTo(other) = this.InlineCompareTo(other)
    interface IComparable with
        member this.CompareTo(other) =
            match other with
            | :? Name as name -> this.InlineCompareTo(name)
            | _ -> invalidArg "other" "Argument is not a Name"
            
    static member op_Explicit(name : Name) = name.String
    static member op_Explicit(name : string) = Name(name)
    static member (+) (name : Name, str : string) = Name(name.String + str)
    static member (+) (str : string, name : Name) = Name(str + name.String)
    static member (+) (name1 : Name, name2 : Name) = Name(name1.String + name2.String)
    
        
