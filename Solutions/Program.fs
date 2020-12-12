module Aoc.Program

open AoC
open CommandLine


[<Verb("new", HelpText = "Create files for a new day")>]
type NewOptions = {
    [<Option('e', "event", Required = true, HelpText = "An event (such as 2015, 2016 etc)")>] Event : string
    [<Option('d', "day", Required = true, HelpText = "A day (such as 1, 2 etc)")>] Day : string
    
}

[<Verb("run", HelpText = "Run the firstStar() or secondStar() methods of a day, or both")>]
type RunOptions = {
    [<Option('f', "first", Required = false, HelpText = "Run first star")>] First : bool
    [<Option('s', "second", Required = false, HelpText = "Run second star")>] Second : bool
}

let printByType (text : string, result : obj) =
    match result with
    | :? System.Int32 as x -> printf "%s %i" text x
    | :? System.Double as x -> printf "%s %f" text x
    | :? System.String as x -> printf "%s %s" text x
    | x -> printf "%s %A" text result

let newAocDay opts =
    0

let runAocDay (opts: RunOptions) = 
    if opts.First then
        printByType ("First star: ", E2016.Day02.firstStar())
        printfn "\n"
    if opts.Second then
        printByType ("Second star:", E2016.Day02.secondStar())
        printfn "\n"
    0

[<EntryPoint>]
let main args =
    let result = Parser.Default.ParseArguments<NewOptions, RunOptions> args
    match result with
    | :? CommandLine.Parsed<obj> as command ->
        match command.Value with
        | :? NewOptions as opts -> newAocDay opts
        | :? RunOptions as opts -> runAocDay opts
        | _ -> failwithf "Not implemented: %O" command
    | :? CommandLine.NotParsed<obj> -> 1
    | _ -> -1

