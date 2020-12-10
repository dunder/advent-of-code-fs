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
    
let validKeyPositions = { X = 0; Y = 0; } |> adjacentAllDirections |> Set.ofSeq |> Set.add { X = 0; Y = 0 }

let isValidKeyPosition pos =
    validKeyPositions |> Set.contains pos

let validBathroomKeyPositions = 
    [
        { X = 0; Y = 2}
        { X = -1; Y = 1}; { X = 0; Y = 1}; { X = 1; Y = 1}
        { X = -2; Y = 0}; { X = -1; Y = 0}; { X = 0; Y = 0}; { X = 1; Y = 0}; { X = 2; Y = 0}
        { X = -1; Y = -1}; { X = 0; Y = -1}; { X = 1; Y = -1}
        { X = 0; Y = -2}
    ]
    |> Set.ofSeq

let isValidBathroomKeyPosition pos =
    validBathroomKeyPositions |> Set.contains pos

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

let bathRoomKey = function
    | { X = 0; Y = 2} -> "1"
    | { X = -1; Y = 1} -> "2"
    | { X = 0; Y = 1} -> "3"
    | { X = 1; Y = 1} -> "4"
    | { X = -2; Y = 0} -> "5"
    | { X = -1; Y = 0} -> "6"
    | { X = 0; Y = 0} -> "7"
    | { X = 1; Y = 0} -> "8"
    | { X = 2; Y = 0} -> "9"
    | { X = -1; Y = -1} -> "A"
    | { X = 0; Y = -1} -> "B"
    | { X = 1; Y = -1} -> "C"
    | { X = 0; Y = -2} -> "D"
    | pos -> failwithf "Invalid key: %A" pos

let evaluate isValidKeyPosition position line = 
    (position, (line: string))
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
            Some ((next, keySequences), (next, keySequences.[1..]))
    )
 
let findCode lines isValidKeyPosition keyLookup = 
    lines 
    |> List.scan (fun (position, keys) line -> 
        let xs = evaluate isValidKeyPosition position line
        let p = xs |> Seq.last |> fst
        let k = keyLookup p
        (p, k::keys)
    ) ({ X = 0; Y = 0 }, [])
    |> Seq.last 
    |> snd
    |> Seq.rev
    |> System.String.Concat

let firstStar () =
    findCode input isValidKeyPosition key
    
let secondStar () =
    findCode input isValidBathroomKeyPosition bathRoomKey


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =

        Assert.Equal("74921", firstStar())

    [<Fact>]
    let ``second star`` () =

        Assert.Equal("A6B35", secondStar())
