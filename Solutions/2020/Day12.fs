module AoC.E2020.Day12

open AoC
open IO
open Matrix

// --- Day 12: Rain Risk ---

let input = readInputLines "2020" "Day12" |> List.ofSeq

let parse (lines: list<string>) = 
    lines
    |> Seq.map (fun line ->
        let direction = line.[0]
        let count = line.[1..] |> int
        direction, count
    )

let toDirection = function
    | 'N' -> Direction.North
    | 'E' -> Direction.East
    | 'S' -> Direction.South
    | 'W' -> Direction.West
    | c -> failwithf "Unrecognized direction %c" c

let moveInstruction facing direction count position = 
    let newPosition = moveN count direction position
    (newPosition, facing)

let turnInstruction direction turning degrees position = 
    let times = degrees / 90
    let newDirection = [1..times] |> Seq.fold (fun state _ -> turn state turning) direction
    (position, newDirection)

let moveWaypoint wp direction count position =
    let newWp = wp |> moveN count direction
    (position, newWp)

let rotateWaypoint (wp: Position) (turning: Turn) (count: int) (position: Position) =
    let times = count / 90
    let newWp = 
        [1..times] 
        |> Seq.fold (fun state _ -> 
            match turning with
            | Turn.Left -> { X = -1 * state.Y; Y = state.X }
            | Turn.Right ->  { X = state.Y; Y = -1 * state.X }
        ) wp
    (position, newWp)

let moveToWaypoint wp count position = 
    let newX = position.X + count * wp.X
    let newY = position.Y + count * wp.Y
    let nextPosition = { X = newX; Y = newY }
    (nextPosition, wp)

let firstStar () =
    parse input
    |> Seq.fold (fun (position, direction) (c, count)-> 
            
        let next = 
            match c with
            | 'N' -> moveInstruction direction (toDirection c) count position
            | 'S' -> moveInstruction direction (toDirection c) count position
            | 'E' -> moveInstruction direction (toDirection c) count position
            | 'W' -> moveInstruction direction (toDirection c) count position
            | 'L' -> turnInstruction direction Turn.Left count position
            | 'R' -> turnInstruction direction Turn.Right count position
            | 'F' -> moveInstruction direction direction count position
            | c -> failwithf "Unrecognized direction instruction: %c" c

        next
    ) ( {X = 0; Y = 0}, Direction.East)
    |> fst
    |> manhattanDistance { X=0; Y=0 }

let secondStar () = 
    parse input
    |> Seq.fold (fun (position, wp) (c, count) -> 
            
        let next = 
            match c with
            | 'N' -> moveWaypoint wp (toDirection c) count position
            | 'S' -> moveWaypoint wp (toDirection c) count position
            | 'E' -> moveWaypoint wp (toDirection c) count position
            | 'W' -> moveWaypoint wp (toDirection c) count position
            | 'L' -> rotateWaypoint wp Turn.Left count position
            | 'R' -> rotateWaypoint wp Turn.Right count position
            | 'F' -> moveToWaypoint wp count position
            | c -> failwithf "Unrecognized direction instruction: %c" c

        next
    ) ({X = 0; Y = 0}, {X = 10; Y = 1})
    |> fst
    |> manhattanDistance { X=0; Y=0 }


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(-1, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(-1, secondStar())
