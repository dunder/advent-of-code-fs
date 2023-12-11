// >>> insert day tagline here <<<

open System
open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day02.txt") |> List.ofSeq

let example = [
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
]

type Set = { Red: int; Green: int; Blue: int }
type Game = { Id : int; Sets : list<Set> }

let parse (input: list<string>) =

    let parseColorCount (colorCount: string) =
        let parts = colorCount.Split(" ")
        let count = parts[0] |> int
        let color = parts[1]
        match color with 
        | "red" -> { Red = count; Green = 0; Blue = 0 }
        | "green" -> { Red = 0; Green = count; Blue = 0 }
        | "blue" -> { Red = 0; Green = 0; Blue = count }
        | _ -> failwithf "Unknown color: %s" color
        
    let parseSet (set: string) = set.Split(", ") |> Seq.map parseColorCount

    let aggregateSet (sets: Set seq) =
        sets |> Seq.fold (fun acc set -> 
        { acc with 
            Red = acc.Red + set.Red
            Blue = acc.Blue + set.Blue
            Green = acc.Green + set.Green} ) { Red = 0; Blue = 0; Green = 0}

    let parseLine (line: string) =
        let parts = line.Split(": ")
        let gameId = parts[0]["Game ".Length..] |> int
        let sets = 
            parts[1].Split("; ") 
            |> Array.map (parseSet >> aggregateSet)
            |> List.ofArray

        { Id = gameId; Sets = sets}

    input |> List.map parseLine
        
parse example
    
let isPossible bag game =
    game.Sets 
    |> Seq.forall (fun set -> set.Red <= bag.Red && set.Green <= bag.Green && set.Blue <= bag.Blue)

let possible games =
    games
    |> List.filter (isPossible { Red = 12; Green = 13; Blue = 14})
    |> List.map (fun game -> game.Id)
    |> List.sum

possible (parse example)

let minimumCubes game1 game2 =
    {  
        Red = max game1.Red game2.Red
        Green = max game1.Green game2.Green
        Blue = max game1.Blue game2.Blue 
    }

let minimumSetOfQubes game = game.Sets |> Seq.reduce minimumCubes

let power set = set.Red * set.Green * set.Blue

{ Id = 1; Sets = [{ Red = 4; Green = 0; Blue = 3 }; { Red = 1; Green = 2; Blue = 6 }; { Red = 0; Green = 2; Blue = 0 }]}
|> minimumSetOfQubes
|> power

example 
|> parse
|> List.map (minimumSetOfQubes >> power)
|> List.sum


let firstStar =
    input |> (parse >> possible)

firstStar

let secondStar = 
    input 
    |> parse
    |> List.map (minimumSetOfQubes >> power)
    |> List.sum

secondStar

