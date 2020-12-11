module AoC.E2016.Day03

open AoC
open IO

// --- Day 3: Squares With Three Sides ---

let input = readInputLines "2016" "Day03" |> List.ofSeq

let lineToInts (line: string) = line.Split("  ", System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int |> List.ofArray

let parse (lines: list<string>) = lines |> List.map lineToInts

let parseColumn (lines: list<string>) = 
    let sliceColumns lines = 
        let intLines = lines |> List.map lineToInts
        let sliceColumn col = intLines |> List.map (fun line -> line.[col]) |> List.ofSeq
        [0..2]
        |> List.map sliceColumn
    lines 
        |> List.chunkBySize 3 
        |> List.map sliceColumns
    |> List.collect (id)

let couldBeTriangle = function
    | [x;y;z] -> x + y > z && x + z > y && y + z > x
    | x -> failwithf "Expecting triangles to be three ints, was: %A" x


let firstStar () =
    let x = parse input
    x |> List.filter couldBeTriangle |> List.length

let secondStar () = 
    let x = parseColumn input
    x |> List.filter couldBeTriangle |> List.length


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =

        Assert.Equal(1050, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(1921, secondStar())
