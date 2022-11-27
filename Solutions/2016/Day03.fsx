// --- Day 3: Squares With Three Sides ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day03.txt") |> List.ofSeq

let parse (lines: string list) =
    lines
    |> List.map(fun line -> 
        line.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)         
        |> Array.map int
        |> List.ofArray
    )

let possible (triangle: int list) =
    match triangle with 
    | [a;b;c] -> a + b > c && a + c > b && c + b > a
    | _ -> failwithf "Not a triangle: %A" triangle

[4; 5; 6] |> possible

input 
|> parse
|> List.where possible
|> List.length

let firstStar =
    0

let chunkToTriangles (chunk:int list list) =
    [   
        for i in 0..chunk.Length-1 do
            let row = chunk[i]
            yield [for _ in 0..row.Length-1 do yield row[i]]
    ]

let parseVertical (lines: string list) =
    lines
    |> parse
    |> List.chunkBySize 3
    |> List.map chunkToTriangles
    |> List.concat


input
|> parseVertical
|> List.where possible
|> List.length


firstStar

let secondStar = 
    0

secondStar

