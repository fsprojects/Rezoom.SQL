#load "../packages/FSharp.Formatting.2.14.4/FSharp.Formatting.fsx"
open FSharp.Literate
open System.IO

let source = __SOURCE_DIRECTORY__
let output = Path.Combine(__SOURCE_DIRECTORY__, "bin/doc")
let template = Path.Combine(source, "template.html")

let copies =
    [   "tips.js"
        "style.css"
    ]

let inputs =
    [   "Tutorial.fsx"
    ]

for input in inputs do
    Literate.ProcessScriptFile
        ( input = Path.Combine(source, input)
        , output = Path.Combine(output, input.Replace(".fsx", ".html"))
        , templateFile = template
        , lineNumbers = false
        , replacements =
            [   "project-name", "Rezoom.SQL"
                "github-link", "https://github.com/rspeele/Rezoom.SQL"
            ]
        )

for copy in copies do
    File.Copy(Path.Combine(source, copy), Path.Combine(output, copy), overwrite = true)