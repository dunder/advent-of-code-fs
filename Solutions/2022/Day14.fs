module AoC.E2022.Day14

// --- Day 14: Regolith Reservoir ---

open AoC
open IO


let input = readInputLines "2022" "Day14" |> List.ofSeq

let source = 500,0

let parse (rows: string list) =
    rows
    |> List.map (fun row -> 
        let positions = row.Split(" -> ")
        positions |> Array.map (fun position -> 
            let parts = position.Split(",")
            parts[0] |> int, parts[1] |> int
        )
        |> List.ofArray
    )

type Position = int*int
type Cave = { Blocks: Set<Position>; Bottom: int; Sand: Position }

let lineToPositions (start, stop) =
    let startx, starty = start
    let stopx, stopy = stop
    let vertical = startx = stopx
    if vertical then
        let s = min starty stopy
        let e = max starty stopy
        [s..e] |> List.map (fun y -> startx, y)
    else    
        let s = min startx stopx
        let e = max startx stopx
        [s..e] |> List.map (fun x -> x, starty)

let positionsToLine positions =
    positions
    |> List.pairwise
    |> List.map lineToPositions
    |> List.concat
    |> Set.ofList

let buildCave (rows: string list) =

    let blocks = 
        rows
        |> parse
        |> List.map positionsToLine
        |> List.reduce Set.union

    let bottom = blocks |> Set.toList |> List.map snd |> List.max

    { Blocks = blocks; Bottom = bottom; Sand = source }

let overflow (cave: Cave) =
    let x, y = cave.Sand
    y > cave.Bottom

let tryDown blocked (cave: Cave) = 
    let x, y = cave.Sand
    let down = x, y + 1
    if blocked cave down then 
        None
    else 
        Some({cave with Sand = down})

let tryLeft blocked (cave: Cave) =
    let x, y = cave.Sand
    let left = x - 1, y + 1
    if blocked cave left then
        None
    else 
        Some({cave with Sand = left })

let tryRight blocked (cave: Cave) =
    let x, y = cave.Sand
    let right = x + 1, y + 1
    if blocked cave right then
        None
    else 
        Some({cave with Sand = right})

let toRest (cave: Cave) =
    { cave with Blocks = cave.Blocks |> Set.add cave.Sand; Sand = source }

let releaseSand (emptyCave: Cave) =

    let blocked (cave: Cave) (position: Position) =
        cave.Blocks |> Set.contains position

    let nextCave (cave: Cave) =

        let down = tryDown blocked cave
        match down with
        | Some down -> 
            Some(down, down)
        | None ->
            let left = tryLeft blocked cave
            match left with
            | Some left -> 
                Some(left, left)
            | None -> 
                let right = tryRight blocked cave
                match right with
                | Some right -> 
                    Some(right, right)
                | None -> 
                    let rest = toRest cave
                    Some(rest, rest)

    Seq.unfold nextCave emptyCave
    |> Seq.find overflow

let countUnitOfSands cave =
    let before = cave.Blocks |> Set.count
    let overflowed = releaseSand cave
    let after = overflowed.Blocks |> Set.count
    after - before 

let firstStar () =
    input
    |> buildCave
    |> countUnitOfSands


let buidlCaveWithBottom (rows: string list) =

    let blocks = 
        rows
        |> parse
        |> List.map positionsToLine
        |> List.reduce Set.union

    let depth = blocks |> Set.toList |> List.map snd |> List.max

    { Blocks = blocks; Bottom = depth + 2; Sand = source }

let full (cave: Cave) =
    cave.Blocks |> Set.contains source

let fillWithSand (emptyCave: Cave) =

    let blocked (cave: Cave) (position: Position) =
        cave.Blocks |> Set.contains position ||
        let x, y = position
        y = cave.Bottom

    let nextCave (cave: Cave) =

        let down = tryDown blocked cave
        match down with
        | Some down -> 
            Some(down, down)
        | None ->
            let left = tryLeft blocked cave
            match left with
            | Some left -> 
                Some(left, left)
            | None -> 
                let right = tryRight blocked cave
                match right with
                | Some right -> 
                    Some(right, right)
                | None -> 
                    let rest = toRest cave
                    Some(rest, rest)
    Seq.unfold nextCave emptyCave
    |> Seq.find full

let countUnitOfSandsWhenFull cave =
    let before = cave.Blocks |> Set.count
    let overflowed = fillWithSand cave
    let after = overflowed.Blocks |> Set.count
    after - before 


let secondStar () = 
    input
    |> buidlCaveWithBottom
    |> countUnitOfSandsWhenFull