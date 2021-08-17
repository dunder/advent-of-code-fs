module Aoc.Program

open CommandLine
open System.IO
open System.Reflection
open System.Xml.Linq

// https://github.com/commandlineparser/commandline

[<Verb("new", HelpText = "Create files for a new day")>]
type NewOptions = {
    [<Option('e', "event", Required = true, HelpText = "An event (such as 2015, 2016 etc)")>] Event : int
    [<Option('d', "day", Required = true, HelpText = "A day (such as 1, 2 etc)")>] Day : int
    [<Option("whatif", Required = false, HelpText = "What if, simulates without creating any files")>] WhatIf : bool
    
}

[<Verb("run", HelpText = "Run the firstStar() or secondStar() methods of a day, or both")>]
type RunOptions = {
    [<Option('e', "event", Required = true, HelpText = "An event (such as 2015, 2016 etc)")>] Event : int
    [<Option('d', "day", Required = true, HelpText = "A day (such as 1, 2 etc)")>] Day : int
    [<Option('f', "first", Required = false, HelpText = "Run first star")>] First : bool
    [<Option('s', "second", Required = false, HelpText = "Run second star")>] Second : bool
    [<Option('x', "example", Required = false, HelpText = "Run example n (where n is 1, 2 etc)")>] Example : int option
}

let callFunction (event: int) (day: int) (name: string) = 
    let asm = Assembly.GetExecutingAssembly()
  
    let moduleName = sprintf "AoC.E%i.Day%02i, Solutions" event day
    let dayModules = asm.GetTypes() |> Array.filter (fun t -> t.AssemblyQualifiedName.StartsWith(moduleName))
    let dayModule = dayModules |> Array.exactlyOne
    let method = dayModule.GetMethod(name)
    method.Invoke(null, [||])

let printByType (text : string, result : obj) =
    match result with
    | :? System.Int32 as x -> printf "%s %i" text x
    | :? System.Double as x -> printf "%s %f" text x
    | :? System.String as x -> printf "%s %s" text x
    | x -> printf "%s %A" text result

let newAocDay (opts: NewOptions) =
    let eventDirectory = Path.Combine(__SOURCE_DIRECTORY__, sprintf "%i" opts.Event)
    let inputDirectory = Path.Combine(eventDirectory, "Input")
    let dayFile = Path.Combine(eventDirectory, sprintf "Day%02i.fs" opts.Day)

    if not <| Directory.Exists(eventDirectory) then
        printfn "This is a new event, creating new directories ..."
        if not <| opts.WhatIf then
            Directory.CreateDirectory(eventDirectory) |> ignore
        printfn "Created a new event directory: %s" eventDirectory
        if not <| opts.WhatIf then
            Directory.CreateDirectory(inputDirectory) |> ignore
        printfn "Created a new input directory: %s" inputDirectory

    if not <| File.Exists(dayFile) then
        let templateFile = Path.Combine(__SOURCE_DIRECTORY__, "DayTemplate.fs")
        let template = File.ReadAllText(templateFile)
        let content = 
            template
                .Replace("DayXX", sprintf "Day%02i" opts.Day)
                .Replace("Event20XX", sprintf "Event%i" opts.Event)
                .Replace("20XX", sprintf "%i" opts.Event)
        
        if not <| opts.WhatIf then
            File.WriteAllText(dayFile, content)
        printfn "Created the solution file: %s" dayFile
        
        let inputFile = Path.Combine(inputDirectory, sprintf "Day%02i.txt" opts.Day)
        
        if not <| opts.WhatIf then
            File.WriteAllText(inputFile, "")
        printfn "Created the input file: %s" inputFile

        let projectFile = XDocument.Load(Path.Combine(__SOURCE_DIRECTORY__, "Solutions.fsproj"))

        let root = projectFile.Root.Elements


        0
    else
        printfn "The file '%s' already exists." dayFile
        1

let runAocDay (opts: RunOptions) = 
    
    if opts.First then
        let result = callFunction opts.Event opts.Day "firstStar"
        printByType ("First star: ", result)
        printfn "\n"
    if opts.Second then
        let result = callFunction opts.Event opts.Day "secondStar"
        printByType ("Second star:", result)
        printfn "\n"
   
    match opts.Example with
    | Some(nr) -> 
        let example = sprintf "example%i" nr
        let result = callFunction opts.Event opts.Day example
        printByType (sprintf "Example %i:" nr, result)
        printfn "\n"
    | None -> ()

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

