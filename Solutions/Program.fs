module Aoc.Program

open System.IO
open AoC

let printByType (text : string, result : obj) =
    match result with
    | :? System.Int32 as x -> printf "%s %i" text x
    | :? System.Double as x -> printf "%s %f" text x
    | :? System.String as x -> printf "%s %s" text x
    | x -> printf "%s %A" text result

[<EntryPoint>]
let main _args =
    for n in 4..25 do
        let fileName = sprintf "C:\\Users\\matjan\\source\\repos\\advent-of-code-fs\\Solutions\\2020\\Input\\Day%02i.txt" n
        File.Create(fileName) |> ignore
    //printByType ("First star: ", E2015.Day15.firstStar())
    //printfn "\n"
    //printByType ("Second star:", E2015.Day05.secondStar())
    //printfn "\n"
    0

