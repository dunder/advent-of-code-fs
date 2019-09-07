module Aoc.Program

open AoC.E2015

let printByType (text : string, result : obj) =
    match result with
    | :? System.Int32 as x -> printf "%s %i" text x
    | :? System.Double as x -> printf "%s %f" text x
    | :? System.String as x -> printf "%s %s" text x
    | x -> printf "%s %A" text result

[<EntryPoint>]
let main _args =
    printByType ("First star: ", Day02.firstStar())
    printfn "\n"
    printByType ("Second star:", Day02.secondStar())
    printfn "\n"
    0

