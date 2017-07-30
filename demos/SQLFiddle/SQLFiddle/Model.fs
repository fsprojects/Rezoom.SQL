namespace SQLFiddle
open Rezoom.SQL
open Rezoom.SQL.Compiler

type FiddleModel = SQLModel<".">

type FiddleBackend =
    | SQLiteFiddle
    | TSQLFiddle
    | PostgresFiddle
    override this.ToString() =
        match this with
        | SQLiteFiddle -> "sqlite"
        | TSQLFiddle -> "tsql"
        | PostgresFiddle -> "postgres"
    static member Parse(str : string) =
        match str with
        | "sqlite" -> Some SQLiteFiddle
        | "tsql" -> Some TSQLFiddle
        | "postgres" -> Some PostgresFiddle
        | _ -> None

type FiddleId =
    | FiddleId of byte array
    override this.ToString() =
        let (FiddleId bytes) = this
        bytes |> Seq.map (fun b -> b.ToString("X2")) |> String.concat ""
    static member Parse(str : string) =
        let nibble char =
            if char >= '0' && char <= '9' then
                int char - int '0'
            elif char >= 'a' && char <= 'f' then
                int char - int 'a' + 10
            elif char >= 'A' && char <= 'F' then
                int char - int 'A' + 10
            else
                -1
        [|  let mutable highNibble = -1
            for character in str do
                let nibble = nibble character
                if nibble >= 0 then
                    if highNibble < 0 then highNibble <- nibble
                    else
                        yield byte ((highNibble <<< 4) ||| nibble)
                        highNibble <- -1
        |] |> FiddleId

type FiddleInput =
    {   Backend : FiddleBackend
        Model : string
        Command : string
    }

type FiddleErrorType =
    | ModelError
    | CommandError

type FiddleError =
    {   Type : FiddleErrorType
        StartLine : int
        StartColumn : int
        EndLine : int
        EndColumn : int
        Reason : string
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
        Idempotent : bool
        ReadTables : string list
        WriteTables : string list
        BackendModel : string
        BackendCommand : string
    }

type FiddleOutput =
    | FiddleInvalid of FiddleError
    | FiddleValid of FiddleTypeInformation

type CheckedFiddle =
    {   Input : FiddleInput
        Output : FiddleOutput
    }