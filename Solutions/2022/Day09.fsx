// --- Day 9: Rope Bridge ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day09.txt") |> List.ofSeq


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

let run moves =
    let initialVisited = Set.empty |> Set.add (0,0)
    let initialState = 
        { Visited = initialVisited;  Rope = { Head = 0,0; Tail = 0,0} }
    moves
    |> List.fold nextState initialState

let example = { Head = 0,0; Tail = 0,0}

moveRope Down example

let initialVisited = Set.empty |> Set.add (0,0)
let initialState = { Visited = initialVisited;  Rope = { Head = 0,0; Tail = 0,0} }

nextState initialState (Down, 2)

let exampleMoves = 
    [
        "R 4"
        "U 4"
        "L 3"
        "D 1"
        "R 4"
        "D 1"
        "L 5"
        "R 2"
    ]

exampleMoves |> parse |> run |> fun x -> x.Visited |> Seq.toList
input |> parse |> run |> fun x -> x.Visited.Count


// 7198: That's not the right answer; your answer is too high.

// let longRope = List.replicate 10 (0,0)

// type LongRopeState = { Visited: Set<Position>; LongRope: Position list }

// type FoldState = { Visited: Set<Position>; Head: Position; LongRope: Position list; Direction: Direction }

let moveTail2 (rope: Rope) direction =
    let headX, headY = rope.Head
    let tailX, tailY = rope.Tail

    // printfn "moveTail2 rope: %A direction: %A" rope direction

    if isTouching rope then
        // printfn "  no move"
        rope, direction
    else
        match direction with
        | Up -> 
            // printfn "  move up"
            let direction = 
                match sign (headX - tailX) with
                | -1 -> Left
                | 0 -> Up
                | 1 -> Right
            { rope with Tail = headX, tailY - 1 }, direction
        | Right ->
            // printfn "  move right"
            let direction = 
                match sign (headY - tailY) with
                | -1 -> Up
                | 0 -> Right
                | 1 -> Down
            { rope with Tail = tailX + 1, headY }, direction
        | Down ->
            // printfn "  move down"
            let direction = 
                match sign (headX - tailX) with
                | -1 -> Right
                | 0 -> Down
                | 1 -> Left
            { rope with Tail = headX, tailY + 1 }, direction
        | Left ->
            // printfn "  move left"
            let direction = 
                match sign (headY - tailY) with
                | -1 -> Down
                | 0 -> Left
                | 1 -> Up
            { rope with Tail = tailX - 1, headY }, direction

type LongRope = Map<int, Position>

let startRope = [0..9] |> List.map (fun i -> i, (0,0)) |> Map.ofList

type Bridge = { LongRope: LongRope; Knot: int; Visited: Set<Position> }

let startBridge = { LongRope = startRope; Knot = 0; Visited = Set.empty }


let printBridge (bridge: Bridge) =
    printfn ""
    printfn ""

    let longRope = bridge.LongRope

    let lookup = 
        longRope 
        |> Map.toList
        |> List.map (fun (key, value) -> value, key)
        |> List.groupBy fst
        |> Map.ofList

    let headx, heady = longRope[0]
    let fromx = headx - 10
    let tox = headx + 10
    let fromy = heady - 10
    let toy = heady + 10

    for y in fromy..toy do
        for x in fromx..tox do
            if lookup |> Map.containsKey (x, y) then
                let knot = lookup[x,y] |> List.minBy snd |> snd
                printf "%i" knot
            else 
                printf "." 
        printfn ""

let follow tail head = 
    let headx, heady = head
    let tailx, taily = tail
    let xdiff = headx - tailx
    let ydiff = heady - taily

    let rope = { Head = head; Tail = tail }

    if isTouching rope then
        tail
    else
        match xdiff, ydiff with
        | 0, -2 -> tailx, taily-1
        | 1, -2 | 2, -2 | 2, -1 -> tailx + 1, taily-1
        | 2, 0 -> tailx + 1, taily
        | 2, 1 | 2, 2 | 1, 2 -> tailx + 1, taily + 1
        | 0, 2 -> tailx, taily + 1
        | -1, 2 | -2, 2 | -2, 1 -> tailx-1, taily + 1
        | -2, 0 -> tailx-1, taily
        | -2, -1 | -2, -2 | -1, -2 -> tailx-1, taily-1
        | _ -> failwithf "Should not be here. head: %i, %i tail: %i, %i" headx heady tailx taily

let moveNextKnot (bridge: Bridge) (direction: Direction) =
    
    let isHead = bridge.Knot = 0

    if isHead then        
        let newHead = moveKnot bridge.LongRope[0] direction
        let newLongRope = bridge.LongRope |> Map.add 0 newHead
        // printfn "adjusted head: %A -> head: %A" bridge.LongRope[0] newHead
        { bridge with LongRope = newLongRope; Knot = 1 }
    else        
        let knot = bridge.Knot
        let previousKnot = knot - 1
        let lastKnot = knot = bridge.LongRope.Count-1
        let nextKnot = if lastKnot then 0 else knot + 1

        let rope = { Head = bridge.LongRope[previousKnot]; Tail = bridge.LongRope[knot]}

        let nextTail = follow rope.Tail rope.Head
        // printfn "adjusted head: %A tail: %A -> head: %A tail %A" rope.Head rope.Tail rope.Head nextTail
        let nextLongRope = bridge.LongRope |> Map.add knot nextTail
        let nextVisited = 
            if lastKnot then
                // printfn "added new tail: %A" nextTail
                bridge.Visited |> Set.add nextTail
            else
                // printfn "not tail: %i" knot
                bridge.Visited

        { bridge with LongRope = nextLongRope; Knot = nextKnot; Visited = nextVisited }
        
let moveLongRope (bridge: Bridge) (direction: Direction)=
    [1..bridge.LongRope.Count]
    |> List.fold (fun b i -> moveNextKnot b direction) bridge

let applyMove (bridge: Bridge) (move: Move) =
    let direction, length = move

    // printfn "== %A ==" move

    let moveLongRopeFold bridge i =         
        let newBridge = moveLongRope bridge direction
        // printBridge newBridge
        newBridge

    [1..length]
    |> List.fold moveLongRopeFold bridge

let applyAllMoves (bridge: Bridge) (moves: string list) =

    moves
    |> parse
    |> List.fold applyMove startBridge

let allVisited (bridge: Bridge) (moves: string list) =

    (applyAllMoves bridge moves).Visited |> Set.count

let circumference = [0,-2; 1, -2; 2, -2; 2, -1; 2, 0; 2, 1; 2, 2; 1, 2; 0, 2; -1, 2; -2, 2; -2, 1; -2, 0; -2, -1; -2, -2; -1, -2]

circumference |> List.map (follow (0,0))

let exampleResult = 
    applyAllMoves startBridge exampleMoves

allVisited exampleResult exampleMoves

let result = applyAllMoves startBridge input

allVisited result input

let b1 = moveLongRope startBridge Right
let b2 = moveLongRope b1 Right
let b3 = moveLongRope b2 Right

printBridge exampleResult
printBridge b2
printBridge b3

let bridge1 = applyMove startBridge (Right,4)
let bridge2 = applyMove bridge1 (Up, 4)
let bridge3 = applyMove bridge2 (Left, 2)
let bridge4 = applyMove bridge3 (Left, 2)

printBridge bridge1
printBridge bridge2
printBridge bridge3
printBridge bridge4

let firstStar =
    0

firstStar

let secondStar = 
    0

secondStar

