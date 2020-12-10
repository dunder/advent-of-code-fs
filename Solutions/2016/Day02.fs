module AoC.E2016.Day02

open AoC
open IO
open Matrix

// --- Day 2: Bathroom Security ---

let input = readInputLines "2016" "Day02" |> List.ofSeq

let oneMove c pos =
    match c with
    | 'U' -> pos |> move Direction.North
    | 'R' -> pos |> move Direction.East
    | 'D' -> pos |> move Direction.South
    | 'L' -> pos |> move Direction.West
    | c -> failwithf "Unrecognized move: %c" c
    
let validKeyPositions = { X = 0; Y = 0; } |> adjacentAllDirections |> Set.ofSeq

let isValidKeyPosition pos =
    validKeyPositions |> Set.contains pos

let key = function
    | { X = -1; Y = 1 } -> "1"
    | { X = 0; Y = 1 } -> "2"
    | { X = 1; Y = 1 } -> "3"
    | { X = -1; Y = 0 } -> "4"
    | { X = 0; Y = 0 } -> "5"
    | { X = 1; Y = 0 } -> "6"
    | { X = -1; Y = -1 } -> "7"
    | { X = 0; Y = -1 } -> "8"
    | { X = 1; Y = -1 } -> "9"
    | pos -> failwithf "Invalid key: %A" pos

let evaluate line = 
    ({ X = 0; Y = 0; }, (line: string))
    |> Seq.unfold (fun (pos, keySequences) -> 
        if keySequences |> Seq.isEmpty then
            None
        else
            let nextMove = keySequences |> Seq.head
            let tryNext = oneMove nextMove pos
            let next = 
                if isValidKeyPosition tryNext then
                    tryNext
                else 
                    pos
            Some ((pos, keySequences), (next, keySequences.[1..]))
    )
    |> Seq.last
    |> fst
    |> key

let firstStar () =
    input |> Seq.map evaluate |> System.String.Concat
    


let secondStar () =
    0


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =

        Assert.Equal("74921", firstStar())

    [<Fact>]
    let ``second star`` () =

        Assert.Equal(-1, secondStar())
