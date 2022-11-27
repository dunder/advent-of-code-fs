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

let example = [
    "101 301 501"
    "102 302 502"
    "103 303 503"
    "201 401 601"
    "202 402 602"
    "203 403 603"
]



let chunkToTriangles (chunk:int list list) =
    let width = chunk |> List.head |> List.length
    let chunkSize = chunk |> List.length
    [   
        for columnIndex in 0..width-1 do
            yield [for rowIndex in 0..chunkSize-1 do yield chunk[rowIndex][columnIndex]]
    ]

example |> parse |> chunkToTriangles



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

