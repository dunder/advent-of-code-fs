module AoC.E2022.Day09

// --- Day 9: Rope Bridge ---

open AoC
open IO


let input = readInputLines "2022" "Day09" |> List.ofSeq


type Direction = Up | Right | Down | Left
type Move = Direction * int

let parse (lines: string list) =

    let toMove (line: string) =
        let parts = line.Split(" ")
        let direction = parts[0]
        let length = parts[1] |> int
        match direction with
        | "U" -> Move (Up, length)
        | "R" -> Move (Right, length)
        | "D" -> Move (Down, length)
        | "L" -> Move (Left, length)
        | _ -> failwithf "Illgal direction: %s" direction

    lines
    |> List.map toMove

type Position = int*int
type Rope = { Head: Position; Tail: Position}

let isTouching rope =
    let headX, headY = rope.Head
    let tailX, tailY = rope.Tail

    abs (headX - tailX) < 2 && 
    abs (headY - tailY) < 2

let moveKnot position direction =
    let x, y = position
    match direction with
    | Up -> x, y-1
    | Right -> x+1, y
    | Down -> x, y+1
    | Left -> x-1, y

let moveTail rope direction =
    let headX, headY = rope.Head
    let tailX, tailY = rope.Tail

    if isTouching rope then
        rope
    else
        match direction with
        | Up -> { rope with Tail = headX, tailY - 1 }
        | Right -> { rope with Tail = tailX + 1, headY }
        | Down -> { rope with Tail = headX, tailY + 1 }
        | Left -> { rope with Tail = tailX - 1, headY }
        
let moveRope direction rope =
    let newHead = moveKnot rope.Head direction
    let newRope = { rope with Head = newHead }
    moveTail newRope direction

type MoveState = { Visited: Set<Position>; Rope: Rope }

let nextState (moveState: MoveState) (move: Move) =
    let direction, length = move
    
    let nextState state (i: int) = 
        let newRope = moveRope direction state.Rope
        let newVisited = state.Visited |> Set.add newRope.Tail

        { state with Visited = newVisited; Rope = newRope }

    [1..length]
    |> Seq.fold nextState moveState

let applyMoves moves =
    let initialVisited = Set.empty |> Set.add (0,0)
    let initialState = 
        { Visited = initialVisited;  Rope = { Head = 0,0; Tail = 0,0} }
    moves
    |> List.fold nextState initialState


let firstStar () =
    input 
    |> parse 
    |> applyMoves 
    |> fun x -> x.Visited.Count


let secondStar () = 
    0