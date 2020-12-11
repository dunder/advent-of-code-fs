module Aoc.Program

open AoC

let printByType (text : string, result : obj) =
    match result with
    | :? System.Int32 as x -> printf "%s %i" text x
    | :? System.Double as x -> printf "%s %f" text x
    | :? System.String as x -> printf "%s %s" text x
    | x -> printf "%s %A" text result

[<EntryPoint>]
let main _args =
    //printByType ("First star: ", E2020.Day11.firstStar())
    //printfn "\n"
    printByType ("Second star:", E2020.Day11.secondStar())
    printfn "\n"
    0

