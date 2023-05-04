// --- Day 22: Monkey Map ---

open System.IO
open System.Text.RegularExpressions

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day22.txt") |> List.ofSeq

let example = [
    "        ...#"
    "        .#.."
    "        #..."
    "        ...."
    "...#.......#"
    "........#..."
    "..#....#...."
    "..........#."
    "        ...#...."
    "        .....#.."
    "        .#......"
    "        ......#."
    ""
    "10R5L5R10L4R5L5"
]

type Tile = | Open | Wall
type Turn = | Left | Right

// type Instruction = { Turn: Turn; Steps: int }
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



let printToFile (map: Map<int*int,Tile>) location =
    async {
        use sw = new StreamWriter(new FileStream(@".\Input\Day22-map.txt",  FileMode.Create, FileAccess.Write, FileShare.None, bufferSize = 4096, useAsync= true))
        let faceToChar = function
        | Up -> "^"
        | Right -> ">"
        | Down -> "v"
        | Left -> "<"

        let tileToChar = function
            | Open -> "."
            | Wall -> "#"

        let rows = map |> Map.keys |> Seq.maxBy (fun (row, column) -> row) |> fst
        let columns = map |> Map.keys |> Seq.maxBy (fun (row, column) -> column) |> snd
        
        for row in 1..rows do
            for column in 1..columns do
                let visited = location.Visited |> Map.tryFind(row, column)
                match visited with
                | Some face -> do! sw.WriteAsync(sprintf "%s" (faceToChar face)) |> Async.AwaitTask
                | None ->
                    let tile = map |> Map.tryFind(row, column)
                    match tile with
                    | Some tile -> do! sw.WriteAsync(sprintf "%s" (tileToChar tile)) |> Async.AwaitTask
                    | None -> do! sw.WriteAsync(" ") |> Async.AwaitTask
            do! sw.WriteLineAsync("") |> Async.AwaitTask
    }
    |> Async.RunSynchronously


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
        
        // let newx, newy = newLocation.Position
        // printf "Wrapping around from %3i, %3i" x y
        // printfn ", entering %3i, %3i" newx newy

        // printToFile map newLocation |> ignore
        // System.Console.ReadKey() |> ignore

        if map.ContainsKey(newLocation.Position) && map[newLocation.Position] = Open then
            Some (newLocation |> Location.visit)
        else
            printfn ""
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

        // printfn "You are at: %A" result
        // printToFile map instruction result |> ignore
        // System.Console.ReadKey() |> ignore

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

let print (map: Map<int*int,Tile>) location =

    let faceToChar = function
        | Up -> "^"
        | Right -> ">"
        | Down -> "v"
        | Left -> "<"

    let tileToChar = function
        | Open -> "."
        | Wall -> "#"

    let rows = map |> Map.keys |> Seq.maxBy (fun (row, column) -> row) |> fst
    let columns = map |> Map.keys |> Seq.maxBy (fun (row, column) -> column) |> snd
    printfn "map: %ix%i" rows columns
    for row in 1..rows do
        for column in 1..columns do
            let visited = location.Visited |> Map.tryFind(row, column)
            match visited with
            | Some face -> printf "%s" (faceToChar face)
            | None ->
                let tile = map |> Map.tryFind(row, column)
                match tile with
                | Some tile -> printf "%s" (tileToChar tile)
                | None -> printf " "
        printfn ""


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
    | Up, row, column when row = 1 && column < 101 -> 
        printfn "1->10"
        { location with Position = column + 100, 1; Face = Right } //  1 -> 10, column 51 -> row 151
    | Up, row, column when row = 1 && column > 100 -> 
        printfn "2->9"
        { location with Position = 200, column - 100 } //  2 -> 9, column 101 -> column 1, column 151 -> column 51
    | Right, row, column when column = 150 -> 
        printfn "3->6"
        { location with Position = 101 + (50-row), column - 50; Face = Left } //  3 -> 6, row 1 -> row 150, row 50 -> row 101
    | Down, row, column when row = 50 && column > 100 -> 
        printfn "4->5"
        { location with Position = column - 50, 100; Face = Left } //  4 -> 5, column 101 -> row 51, column 150 -> row 100
    | Right, row, column when row < 101 && column = 100 -> 
        printfn "5->4"
        { location with Position = 50, row + 50; Face = Up } //  5 -> 4, row 51 -> column 101, row 100 -> column 150
    | Right, row, column when column = 100 -> 
        printfn "6->3"
        { location with Position = 1 + (150 - row), column + 50; Face = Left } //  6 -> 3, row 101 -> row 50, row 150 -> row 1
    | Down, row, column when row = 150 -> 
        printfn "7->8"
        { location with Position = column + 100, 50; Face = Left } //  7 -> 8, column 51 -> row 151
    | Right, row, column when column = 50 -> 
        printfn "8->7"
        { location with Position = 150, row - 100; Face = Up } //  8 -> 7
    | Down, row, column when row = 200 ->
        printfn "9->2"
        { location with Position = 1, column + 100 } //  9 -> 2
    | Left, row, column when row > 150 && column = 1 -> 
        printfn "10->1"
        { location with Position = 1, row - 100; Face = Down } // 10 -> 1, row 151 -> column 51, row 200 -> column 100
    | Left, row, column when column = 1 ->
        printfn "11->14"
        { location with Position = 1 + (150 - row), column + 50; Face = Right } // 11 -> 14, row 101 -> row 50, row 150 -> row 1
    | Up, row, column when row = 101 ->
        printfn "12->13"
        { location with Position = column + 50, 51; Face = Right } // 12 -> 13, column 1 -> row 51, column 50 -> row 100
    | Left, row, column when row > 50 && column = 51 ->
        printfn "13->12"
        { location with Position = 101, row - 50; Face = Down } // 13 -> 12, row 100 -> column 50, row 51 -> column 1
    | Left, row, column when column = 51 ->
        printfn "14->11"
        { location with Position = 101 + (50 - row), column - 50; Face = Right } // 14 -> 11, row 1 -> row 150, row 50 -> row 101
    | _  -> failwithf "Undefined wrap around for %3i, %3i" row column



let printSidesToFile (map: Map<int*int,Tile>) (sides: Map<int*int, Location>) =
    async {
        use sw = new StreamWriter(new FileStream(@".\Input\Day22-map.txt",  FileMode.Create, FileAccess.Write, FileShare.None, bufferSize = 4096, useAsync= true))
        let faceToChar = function
        | Up -> "^"
        | Right -> ">"
        | Down -> "v"
        | Left -> "<"

        let tileToChar = function
            | Open -> "."
            | Wall -> "#"

        let rows = map |> Map.keys |> Seq.maxBy (fun (row, column) -> row) |> fst
        let columns = map |> Map.keys |> Seq.maxBy (fun (row, column) -> column) |> snd
        
        for row in 1..rows do
            for column in 1..columns do
                let exitSide = sides |> Map.tryFind (row, column)
                match exitSide with
                | Some location ->
                    do! sw.WriteAsync(sprintf "%s" (faceToChar location.Face)) |> Async.AwaitTask
                | None -> 
                    let tile = map |> Map.tryFind(row, column)
                    match tile with
                    | Some tile -> do! sw.WriteAsync(sprintf "%s" (tileToChar tile)) |> Async.AwaitTask
                    | None -> do! sw.WriteAsync(" ") |> Async.AwaitTask
            do! sw.WriteLineAsync("") |> Async.AwaitTask
    }
    |> Async.RunSynchronously

let side (map: Map<int*int,Tile>) (rowFrom, rowTo, columnFrom, columnTo, face) = 
    [
        for row in rowFrom..rowTo do  
            for column in columnFrom..columnTo do
                let location = { Position = row, column; Face = face; Visited = Map.empty }
                let opposite = wrapAroundQube map location
                let x, y = location.Position
                let enterRow, exitRow = opposite.Position
                let enterX, enterY = opposite.Position
                printfn "exit: %3i,%3i facing: %A, enter: %3i%3i facing: %A" x y location.Face enterX enterY opposite.Face
                yield (row,column), location
                yield (enterRow, exitRow), opposite
    ]
    |> Map.ofList

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


let sides =
    [
        1,1,51,100, Up // 1-10
        151, 200, 1, 1, Left // 10-1
        1,1,101,150, Up // 2-9
        200, 200, 1, 50, Down // 9-2
        1,50,150,150, Right // 3-6
        101, 150, 100, 100, Right // 6-3
        50,50,101,150, Down // 4-5
        51,100,100,100, Right // 5-4
        150, 150, 51, 100, Down // 7-8
        151, 200, 50, 50, Right // 8-7
        101, 150, 1, 1, Left // 11-14
        1, 50, 51, 51, Left // 14-11
        101, 101, 1, 50, Up // 12-13
        51, 100, 51, 51, Left // 13-12
    ]


let w, m, p = input |> parse

let sideData = side m sides[0]

sideData |> Map.count

printSidesToFile m sideData

wrapAroundQube m { Position = 200, 50; Face = Face.Up; Visited = Map.empty }


let width, map, path = example |> parse
path |> List.last
let startLocation = start width map

let stopLocation = walk map wrapAround path startLocation

let answer = stopLocation |> password

print map stopLocation

let stop = move map wrapAround startLocation (Walk(5))

let test = move map wrapAround { Position = 6,1; Face = Right; Visited = Map.empty } (Walk(10))
wrapAround map { Position = 6,10; Face = Right; Visited = Map.empty }



let firstStar =
    let width, map, path = input |> parse
    let startLocation = start width map
    walk map wrapAround path startLocation |> password

// That's not the right answer; your answer is too low. If you're stuck, make sure you're using the full input data; 
// there are also some general tips on the about page, or you can ask for hints on the subreddit.
// Please wait one minute before trying again. (You guessed 29364.)

firstStar

let secondStar = 
    let width, map, path = input |> parse
    let startLocation = start width map
    walk map wrapAroundQube path startLocation |> password

secondStar

// That's not the right answer; your answer is too high. If you're stuck, make sure you're using the full input data; 
// there are also some general tips on the about page, or you can ask for hints on the subreddit. 
// Please wait one minute before trying again. (You guessed 155080.)

// That's not the right answer; your answer is too high. If you're stuck, make sure you're using the full input data;
// there are also some general tips on the about page, or you can ask for hints on the subreddit.
// Please wait one minute before trying again. (You guessed 124261.)

// That's not the right answer; your answer is too low. If you're stuck, make sure you're using the full input data;
// there are also some general tips on the about page, or you can ask for hints on the subreddit.
// Please wait one minute before trying again. (You guessed 89232.)