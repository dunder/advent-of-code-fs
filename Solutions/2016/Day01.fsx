// >>> insert day tagline here <<<

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadAllText(@".\Input\Day01.txt")

let example = "R5, L5, R5, R3"

type Direction = North | East | South | West

type Turn = Left | Right

let toTurn c = 
    match c with
    | 'L' -> Left
    | 'R' -> Right
    | _ -> failwithf "Invalid turn: %c" c

let parse (text: string) = 
    text.Split(", ")
    |> Seq.map(fun instruction -> instruction[0] |> toTurn, instruction.Substring(1) |> int)

let turn heading direction  =
    match (heading, direction) with
    | North, Left -> West
    | North, Right -> East
    | East, Left -> North
    | East, Right -> South
    | South, Left -> East
    | South, Right -> West
    | West, Left -> South
    | West, Right -> North

let distance x y = abs x + abs y

let move (x, y, heading) steps=
    match heading with
    | North -> x, y + steps, heading
    | East -> x + steps, y, heading
    | South -> x, y - steps, heading
    | West -> x - steps, y, heading

let turnAndMove (x, y, heading) (direction, steps) =
    let newHeading = turn heading direction
    move (x, y, newHeading) steps

example
|> parse
|> Seq.fold (turnAndMove) (0, 0, North)
|> fun (x, y, direction) -> x, y
||> distance

let firstStar =
    input
    |> parse
    |> Seq.fold turnAndMove (0, 0, North)
    |> fun (x, y, _) -> x, y
    ||> distance

firstStar

let example2 = "R8, R4, R4, R8"

type Operation =
    | Turn of Turn
    | Step

let toOperations (turn, steps) =
    seq {
        yield Turn(turn)
        yield! Seq.replicate steps Step
    }

let movement (x, y, direction, keep) operation =
    match operation with
    | Turn t -> 
        let newDirection = turn direction t
        (x, y, newDirection, false)
    | Step -> 
        match direction with
        | North -> (x, y + 1, direction, true)
        | East -> (x + 1, y, direction, true)
        | South -> (x, y - 1, direction, true)
        | West -> (x - 1, y, direction, true)

let collectVisited (_, visited, found) (x, y) =
    (x, y), visited |> Set.add (x, y), visited |> Set.contains (x, y)



input
|> parse
|> Seq.map toOperations
|> Seq.concat
|> Seq.scan movement (0, 0, North, false)
|> Seq.where (fun (_, _, _, keep) -> keep)
|> Seq.map (fun (x, y, _, _) -> x, y)
|> Seq.scan collectVisited ((0,0), Set.empty, false)
|> Seq.find (fun ((x, y), visited, found) -> found)
|> (fun ((x, y), visited, found) -> (x, y))
||> distance


let secondStar = 
    0

secondStar

