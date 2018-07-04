module Rezoom.SQL.Compiler.Config
open System
open FParsec

type ConfigBackend =
    | Identity // outputs Rezoom.SQL that can be parsed back
    | SQLite
    | TSQL
    | Postgres
    member this.ToBackend() =
        match this with
        | Identity -> DefaultBackend() :> IBackend
        | SQLite -> SQLite.SQLiteBackend() :> IBackend
        | TSQL -> TSQL.TSQLBackend() :> IBackend
        | Postgres -> Postgres.PostgresBackend() :> IBackend

type ConfigOptionalStyle =
    | CsStyle // optional value types get wrapped in Nullable, optional reference types untouched
    | FsStyle // all optional types wrapped in FSharpOption

type Config =
    {   /// Which backend to use.
        Backend : ConfigBackend
        /// Path to the migrations folder relative to the directory the config file resides in.
        MigrationsPath : string
        /// Connection string name to use at runtime.
        ConnectionName : string
        /// Type generation style for optionals.
        Optionals : ConfigOptionalStyle
    }

let defaultConfig =
    {   Backend = Identity
        MigrationsPath = "."
        ConnectionName = "rzsql"
        Optionals = FsStyle
    }

module private Parser =
    open FParsec.Pipes

    let backend =
        %% '"'
        -- +.[  %% ci "SQLITE" -|> SQLite
                %% [ ci "TSQL"; ci "MSSQL" ] -|> TSQL
                %% ci "POSTGRES" -- zeroOrOne * ci "QL" -|> Postgres
                %% ci "RZSQL" -|> Identity
            ]
        -- '"'
        -|> id

    let optionals =
        %% '"'
        -- +.[  %% ci "C#" -|> CsStyle
                %% ci "F#" -|> FsStyle
            ]
        -- '"'
        -|> id

    let stringLiteral =
        let escape =
            anyOf "\"\\/bfnrt"
            |>> function
                | 'b' -> '\b'
                | 'f' -> '\u000C'
                | 'n' -> '\n'
                | 'r' -> '\r'
                | 't' -> '\t'
                | c -> c

        let unicodeEscape =
            %% 'u'
            -- +.(qty.[4] * hex)
            -|> fun hexes -> Int32.Parse(String(hexes)) |> char

        let escapedChar = %% '\\' -- +.[ escape; unicodeEscape ] -|> string
        let normalChars = manySatisfy (function | '"' | '\\' -> false | _ -> true)

        %% '"'
        -- +.stringsSepBy normalChars escapedChar
        -- '"'
        -|> id

    let prop (name : string) (parser : Parser<'a, 'u>) =
        %% ci ("\"" + name + "\"")
        -- spaces
        -- ':'
        -- spaces
        -- +.parser
        -- spaces
        -|> id

    let property =
        %[
            prop "BACKEND" (backend |>> fun backend config -> { config with Backend = backend })
            prop "MIGRATIONS" (stringLiteral |>> fun path config -> { config with MigrationsPath = path })
            prop "CONNECTIONNAME" (stringLiteral |>> fun conn config -> { config with ConnectionName = conn })
            prop "OPTIONALS" (optionals |>> fun opts config -> { config with Optionals = opts })
        ]

    let config : Parser<Config, unit> =
        let comma = %% ',' -- spaces -|> ()
        %% '{'
        -- spaces
        -- +.(qty.[0..] /. comma * property)
        -- '}'
        -- spaces
        -- eof
        -|> Seq.fold (|>) defaultConfig

let parseConfig sourceDescription source =
    match runParserOnString Parser.config () sourceDescription source with
    | Success (statements, _, _) -> statements
    | Failure (reason, err, _) ->
        let sourceInfo = SourceInfo.OfPosition(translatePosition err.Position)
        failAt sourceInfo reason

let parseConfigFile path =
    parseConfig path (IO.File.ReadAllText(path))