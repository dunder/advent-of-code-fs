module AoC.E2016.Day03

// --- Day 3: Squares With Three Sides ---

open AoC
open IO


let input = readInputLines "2016" "Day03" |> List.ofSeq

let parse (lines: string list) =
    lines
    |> List.map(fun line -> 
        line.Split(" ") 
        |> Array.where (fun x -> x.Length > 0)
        |> Array.map (fun number -> number.Trim() |> int)
        |> List.ofArray
    )

let possible (triangle: int list) =
    match triangle with 
    | [a;b;c] -> a + b > c && a + c > b && c + b > a
    | _ -> failwithf "Not a triangle: %A" triangle


let firstStar () =
    input 
    |> parse
    |> List.where possible
    |> List.length


let chunkToTriangles (chunk:int list list) =
    let width = chunk |> List.head |> List.length
    let chunkSize = chunk |> List.length
    [   
        for columnIndex in 0..width-1 do
            yield [for rowIndex in 0..chunkSize-1 do yield chunk[rowIndex][columnIndex]]
    ]

let parseVertical (lines: string list) =
    lines
    |> parse
    |> List.chunkBySize 3
    |> List.map chunkToTriangles
    |> List.concat


let secondStar () = 
    input
    |> parseVertical
    |> List.where possible
    |> List.length