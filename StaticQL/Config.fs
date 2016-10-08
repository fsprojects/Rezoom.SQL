module StaticQL.Config
open System
open FParsec

type ConfigBackend =
    | SQLite
    | TSQL
    | PostgreSQL
    | MySQL
    member this.ToBackend() =
        match this with
        | SQLite -> SQLite.SQLiteBackend() :> IBackend
        | _ -> failwithf "Unimplemented backend %A" this // TODO

type Config =
    {   /// Which backend to use.
        Backend : ConfigBackend
        /// Path to the migrations folder relative to the directory the config file resides in.
        MigrationsPath : string
    }

let defaultConfig =
    {   Backend = SQLite
        MigrationsPath = "."
    }

module private Parser =
    open FParsec.Pipes

    let backend =
        %% '"'
        -- +.[
                %% ci "SQLITE" -|> SQLite
                %% [ ci "TSQL"; ci "MSSQL" ] -|> TSQL
                %% ci "POSTGRES" -- zeroOrOne * ci "QL" -|> PostgreSQL
                %% ci "MYSQL" -|> MySQL
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