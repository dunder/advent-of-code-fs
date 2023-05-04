module AoC.E2022.Day22

// --- Day 22: Monkey Map ---

open System.Text.RegularExpressions

open AoC
open IO

let input = readInputLines "2022" "Day22" |> List.ofSeq


type Tile = | Open | Wall
type Turn = | Left | Right

type Instruction = | Turn of Turn | Walk of int

type Face = | Right | Down | Left | Up
type Direction = | Forward | Reverse

module Face = 
    let opposite = function
        | Right -> Left
        | Down -> Up
        | Left -> Right
        | Up -> Down

type Location = { Position: int*int; Face: Face; Visited: Map<int*int,Face> }

module Location = 
    let visit location =
        { 
            location with 
                Visited = location.Visited |> Map.add location.Position location.Face }

module Turn =

    let turn (facing: Face) (turn: Turn) = 
        match facing, turn with
        | Up, Turn.Right -> Right
        | Up, Turn.Left -> Left
        | Right, Turn.Right -> Down
        | Right, Turn.Left -> Up
        | Down, Turn.Right -> Left
        | Down, Turn.Left -> Right
        | Left, Turn.Right -> Up
        | Left, Turn.Left -> Down

let parse (lines: list<string>) =
    let width = lines.Head.Length
    let tiles = 
        lines
        |> List.indexed
        |> List.fold(fun state (row, line) ->
            line
            |> Seq.indexed
            |> Seq.fold(fun state (column, c) ->
                match c with
                | '.' -> state |> Map.add (row + 1, column + 1) Open
                | '#' -> state |> Map.add (row + 1, column + 1) Wall
                | _ -> state
            ) state
        ) Map.empty

    let parsePath input =
        Regex.Match(input, @"((\d+)|([R|L]))+").Groups[1].Captures
        |> Seq.map (fun g ->
            match g.Value with
            | "R" -> Turn(Turn.Right)
            | "L" -> Turn(Turn.Left)
            | n -> Walk(int n)
        )
        |> Seq.toList

    width, tiles, lines |> List.last |> parsePath

let start width (map: Map<int*int,Tile>) =
    let startColumn = 
        [1..width] 
        |> List.find (fun column -> 
            let pos = 1, column
            map.ContainsKey(pos) && map[pos] = Open
        )
    { Position = 1, startColumn; Face = Right; Visited = Map.empty |> Map.add (1, startColumn) Right }

let step location direction =
    let face = 
        match direction with
        | Forward -> location.Face
        | Reverse -> location.Face |> Face.opposite
    { 
        location with
            Position = 
                match face with
                | Up -> fst location.Position - 1, snd location.Position
                | Right -> fst location.Position, snd location.Position + 1
                | Down -> fst location.Position + 1, snd location.Position
                | Left -> fst location.Position, snd location.Position - 1
    }

let wrapAround (map: Map<int*int,Tile>) location =
    let positions = 
        List.unfold (fun state -> 
            let newLocation = step state Reverse
            if map.ContainsKey(newLocation.Position) then
                Some(newLocation, newLocation)
            else
                None
        ) location
    if positions |> List.isEmpty then
        location
    else
        positions |> List.last

let next (map: Map<int*int,Tile>) wrapAround location =
    let newLocation = step location Forward
    if map.ContainsKey(newLocation.Position) then
        match map[newLocation.Position] with
        | Open -> Some (newLocation |> Location.visit)
        | Wall -> None
    else
        let leaving = (step location Forward)
        let x, y = leaving.Position

        let newLocation = wrapAround map location

        if map.ContainsKey(newLocation.Position) && map[newLocation.Position] = Open then
            Some (newLocation |> Location.visit)
        else
            None

let move (map: Map<int*int,Tile>) wrapAround (location: Location) (instruction: Instruction) =
    match instruction with
    | Turn direction -> { location with Face = direction |> Turn.turn location.Face }
    | Walk steps -> 
        let positions = 
            List.unfold (fun (location, steps) ->
                if steps = 0 then
                    None
                else
                    let newLocation = next map wrapAround location
                    match newLocation with
                    | Some newLocation -> 
                        let newLocation = newLocation |> Location.visit
                        Some((newLocation, steps-1), (newLocation, steps-1))
                    | None -> None
            ) (location, steps)

        let newLocation = 
            if positions |> List.isEmpty then
                location
            else 
                positions |> List.last |> fst

        let result = { newLocation with Position = newLocation.Position }

        result

let walk (map: Map<int*int,Tile>) wrapAround (path: list<Instruction>) start =
    path
    |> List.fold (move map wrapAround) start

let password location = 
    let faceValue = function
        | Right -> 0
        | Down -> 1
        | Left -> 2
        | Up -> 3
    let row, column = location.Position
    1000 * row +
    4 * column +
    faceValue location.Face

//              1     2
//           +-----+-----+
//        14 |     |     |
//           |     |     | 3
//           +-----+-----+
//        13 |     |   4
//       12  |     | 5
//     +-----+-----+
//  11 |     |     |
//     |     |     | 6
//     +-----+-----+
//  10 |     |  7
//     |     | 8
//     +-----+
//        9

let wrapAroundQube (map: Map<int*int,Tile>) location =
    let row, column = location.Position
    let face = location.Face
    match face, row, column with
    | Up, row, column when row = 1 && column < 101 -> { location with Position = column + 100, 1; Face = Right } // 1-10
    | Up, row, column when row = 1 && column > 100 -> { location with Position = 200, column - 100 } // 2-9
    | Right, row, column when column = 150 -> { location with Position = 101 + (50-row), column - 50; Face = Left } // 3-6
    | Down, row, column when row = 50 && column > 100 -> { location with Position = column - 50, 100; Face = Left } // 4-5
    | Right, row, column when row < 101 && column = 100 -> { location with Position = 50, row + 50; Face = Up } // 5-4
    | Right, row, column when column = 100 -> { location with Position = 1 + (150 - row), column + 50; Face = Left } // 6-3
    | Down, row, column when row = 150 -> { location with Position = column + 100, 50; Face = Left } // 7-8
    | Right, row, column when column = 50 -> { location with Position = 150, row - 100; Face = Up } // 8-7
    | Down, row, column when row = 200 -> { location with Position = 1, column + 100 } // 9-2
    | Left, row, column when row > 150 && column = 1 -> { location with Position = 1, row - 100; Face = Down } // 10-1
    | Left, row, column when column = 1 -> { location with Position = 1 + (150 - row), column + 50; Face = Right } // 11-14
    | Up, row, column when row = 101 ->{ location with Position = column + 50, 51; Face = Right } // 12-13
    | Left, row, column when row > 50 && column = 51 ->{ location with Position = 101, row - 50; Face = Down } // 13-12
    | Left, row, column when column = 51 -> { location with Position = 101 + (50 - row), column - 50; Face = Right } // 14-11
    | _  -> failwithf "Undefined wrap around for %3i, %3i" row column


let firstStar () =
    let width, map, path = input |> parse
    let startLocation = start width map
    walk map wrapAround path startLocation |> password

let secondStar () = 
    let width, map, path = input |> parse
    let startLocation = start width map
    walk map wrapAroundQube path startLocation |> password