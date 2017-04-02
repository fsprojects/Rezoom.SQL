namespace SQLFiddle
open Rezoom.SQL
open Rezoom.SQL.Compiler

type FiddleModel = SQLModel<".">

type FiddleBackend =
    | SQLiteFiddle
    | TSQLFiddle
    override this.ToString() =
        match this with
        | SQLiteFiddle -> "sqlite"
        | TSQLFiddle -> "tsql"
    static member Parse(str : string) =
        match str with
        | "sqlite" -> Some SQLiteFiddle
        | "tsql" -> Some TSQLFiddle
        | _ -> None

type FiddleId =
    | FiddleId of byte array
    override this.ToString() =
        let (FiddleId bytes) = this
        bytes |> Seq.map (fun b -> b.ToString("X2")) |> String.concat ""

type FiddleInput =
    {   Backend : FiddleBackend
        Model : string
        Command : string
        Valid : bool
    }

type FiddleErrorType =
    | ModelError
    | CommandError

type FiddleError =
    {   Type : FiddleErrorType
        StartIndex : int
        EndIndex : int
        Message : string
    }

type FiddleType =
    {   Nullable : bool
        Name : string
    }

type FiddleTypedName =
    {   Name : string
        Type : FiddleType
    }

type FiddleResultSet =
    {   Columns : FiddleTypedName list
    }

type FiddleTypeInformation =
    {   Parameters : FiddleTypedName list
        ResultSets : FiddleResultSet list
    }

type FiddleOutput =
    | FiddleInvalid of FiddleError
    | FiddleValid of FiddleTypeInformation

type CheckedFiddle =
    {   Input : FiddleInput
        Output : FiddleOutput
    }

type StandardFiddle =
    {   Id : FiddleId
        Title : string
    }