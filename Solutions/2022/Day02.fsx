// --- Day 2: Rock Paper Scissors ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day02.txt") |> List.ofSeq

type Shape = Rock | Paper | Scissors
type Outcome = Win | Draw | Lose

let parse (lines: string list) = 
    lines
    |> List.map (fun line ->
        
        let oppnenentCommand = line[0]
        let yourCommand = line[2]
        
        let opponentsPlay =
            match oppnenentCommand with
            | 'A' -> Rock
            | 'B' -> Paper
            | 'C' -> Scissors
            | _ -> failwithf "Cannot parse opponents command. Bad input: %s" line
        
        let myPlay = 
            match yourCommand with
            | 'X' -> Rock
            | 'Y' -> Paper
            | 'Z' -> Scissors
            | _ -> failwithf "Cannot parse your command. Bad input: %s" line

        opponentsPlay, myPlay
    )

input |> parse

let shapeScore play = 
    match play with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let roundOutcome player opponent =
    match player, opponent with
    | Rock, Rock -> Draw
    | Rock, Paper -> Lose
    | Rock, Scissors -> Win
    | Paper, Rock -> Win
    | Paper, Paper -> Draw
    | Paper, Scissors -> Lose
    | Scissors, Rock -> Lose
    | Scissors, Paper -> Win
    | Scissors, Scissors -> Draw

let outcomeScore outcome =
    match outcome with
    | Win -> 6
    | Draw -> 3
    | Lose -> 0

let example = [
    "A Y"
    "B X"
    "C Z"
]

let roundScore player opponent = shapeScore player + outcomeScore (roundOutcome player opponent)

input
|> parse
|> List.fold(fun total (player, opponent) -> total + roundScore opponent player) 0

    
let totalScore total (opponent, player) = total + roundScore player opponent


let firstStar =
    input
    |> parse
    |> List.fold totalScore 0

firstStar

let parse2 (lines: string list) = 
    lines
    |> List.map (fun line ->
        let opponent = line[0]
        let you = line[2]
        
        let oponentPlay =
            match opponent with
            | 'A' -> Rock
            | 'B' -> Paper
            | 'C' -> Scissors
            | _ -> failwithf "Unexpected input when reading opponent: %s" line

        let yourPlay = 
            match you with
            | 'X' -> Lose
            | 'Y' -> Draw
            | 'Z' -> Win
            | _ -> failwithf "Unexpected input when reading you: %s" line
        
        oponentPlay, yourPlay
    )

let predict opponent outcome =
    match opponent, outcome with
    | Rock, Lose -> Scissors
    | Rock, Draw -> Rock
    | Rock, Win -> Paper
    | Paper, Lose -> Rock
    | Paper, Draw -> Paper
    | Paper, Win -> Scissors
    | Scissors, Lose -> Paper
    | Scissors, Draw -> Scissors
    | Scissors, Win -> Rock

let scoreOutcome prediction =
    match prediction with
    | Win -> 6
    | Draw -> 3
    | Lose -> 0

example |> parse2

input
|> parse2
|> List.fold(
    fun total (player, outcome) -> 
        let prediction = predict player outcome

        total + scoreOutcome outcome + shapeScore prediction) 0


let secondStar = 
    0

secondStar

