module AoC.E2023.Day02

// --- Day 2: Cube Conundrum ---

open AoC
open IO


let input = readInputLines "2023" "Day02" |> List.ofSeq

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

let isPossible bag game =
    game.Sets 
    |> Seq.forall (fun set -> set.Red <= bag.Red && set.Green <= bag.Green && set.Blue <= bag.Blue)

let possible games =
    games
    |> List.filter (isPossible { Red = 12; Green = 13; Blue = 14})
    |> List.map (fun game -> game.Id)
    |> List.sum

let minimumCubes game1 game2 =
    {  
        Red = max game1.Red game2.Red
        Green = max game1.Green game2.Green
        Blue = max game1.Blue game2.Blue 
    }

let minimumSetOfQubes game = game.Sets |> Seq.reduce minimumCubes

let power set = set.Red * set.Green * set.Blue


let firstStar () =
    input |> (parse >> possible)

let secondStar () = 
    input 
    |> parse
    |> List.map (minimumSetOfQubes >> power)
    |> List.sum