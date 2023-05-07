module AoC.E2022.Day23

// --- Day 23: Unstable Diffusion ---

open AoC
open IO


let input = readInputLines "2022" "Day23" |> List.ofSeq


type Direction = | North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest

let directions = [ North; NorthEast; East; SouthEast; South; SouthWest; West; NorthWest ]

let turnOrder = [ North; South; West; East ]

let nextRound round = 
    [
        for i in 0..3 do
            yield turnOrder[(i+round)%turnOrder.Length]
    ]

let parse (lines: string list) =
    lines
    |> List.indexed
    |> List.fold (fun elves (y, line) ->
        line
        |> Seq.indexed
        |> Seq.fold (fun elves (x, c) -> 
            match c with
            | '#' -> elves |> Set.add (x,y)
            | _ -> elves
        ) elves
    ) Set.empty
    
let move (x,y) direction =
    match direction with
    | North -> x, y-1
    | NorthWest -> x-1, y-1
    | NorthEast -> x+1, y-1
    | South -> x, y+1
    | SouthWest -> x-1, y+1
    | SouthEast -> x+1, y+1
    | West -> x-1, y
    | East -> x+1, y

let adjacent pos = directions |> List.map (fun direction -> direction, move pos direction )

let quadrant direction =
    match direction with
    | North -> [ North; NorthEast; NorthWest ]
    | South -> [ South; SouthEast; SouthWest ]
    | West -> [ West; NorthWest; SouthWest ]
    | East -> [ East; NorthEast; SouthEast ]
    | _ -> failwithf "Only main directions: North, South, West and East allowed"

let proposedMove (elves: Set<int*int>) round position =
    if elves |> Set.contains position |> not then
        failwithf "That's no elf!"
    else
        let adjacent = adjacent position

        let hasSpace = adjacent |> List.map snd |> List.forall (fun pos -> elves |> Set.contains pos |> not)

        if hasSpace then
            None
        else
            nextRound round
            |> Seq.tryFind (fun direction ->
                quadrant direction
                |> Seq.map (move position)
                |> Seq.forall (fun pos -> elves |> Set.contains pos |> not )
            )
            |> Option.map (fun direction -> move position direction, position)
    
let validMoves (elves: Set<int*int>) round =
    elves
    |> Seq.map (proposedMove elves round)
    |> Seq.choose (id)
    |> Seq.groupBy fst
    |> Seq.filter (fun (key, group) -> group |> Seq.length = 1)
    |> Seq.map snd
    |> Seq.collect (id)
    |> Seq.toList

let moveOneElf (elves: Set<int*int>) (destination, source) = elves |> Set.remove source |> Set.add destination

let moveAllElves (elves: Set<int*int>) round =
    validMoves elves round
    |> List.fold moveOneElf elves

let run rounds (elves: Set<int*int>) =
    [0..rounds-1]
    |> List.fold moveAllElves elves

let bounds (elves: Set<int*int>) =
    let minx = elves |> Set.map fst |> Seq.min
    let maxx = elves |> Set.map fst |> Seq.max
    let miny = elves |> Set.map snd |> Seq.min
    let maxy = elves |> Set.map snd |> Seq.max
    minx, maxx, miny, maxy

let countSquares (minx, maxx, miny, maxy) =     
    (maxx-minx+1)*(maxy-miny+1)

let countEmpty (elves: Set<int*int>) =
    let totalSquares =
        elves
        |> bounds
        |> countSquares
    totalSquares - elves.Count

let firstStar () =
    input
    |> parse
    |> run 10
    |> countEmpty

let run2 (elves: Set<int*int>) = 
    Seq.unfold(fun (elves, round) ->
        let nextElves = round |> moveAllElves elves
        let nextState = nextElves, round+1
        if nextElves = elves then
            None
        else
            Some(nextState, nextState)
    ) (elves, 0)
    |> Seq.last
    |> snd
    |> (+) 1

let secondStar () = 
    input
    |> parse
    |> run2