module AoC.E2022.Day02

// --- Day 2: Rock Paper Scissors ---

open AoC
open IO


let input = readInputLines "2022" "Day02" |> List.ofSeq

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

let shapeScore play = 
    match play with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let roundOutcome player opponent =
    match player, opponent with
    | Rock, Paper -> Lose
    | Rock, Scissors -> Win
    | Paper, Rock -> Win
    | Paper, Scissors -> Lose
    | Scissors, Rock -> Lose
    | Scissors, Paper -> Win
    | _, _ -> Draw

let outcomeScore outcome =
    match outcome with
    | Win -> 6
    | Draw -> 3
    | Lose -> 0

let roundScore player opponent = shapeScore player + outcomeScore (roundOutcome player opponent)
    
let totalScore total (opponent, player) = total + roundScore player opponent


let firstStar () =
    input
    |> parse
    |> List.fold totalScore 0


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
    | opponent, Draw -> opponent
    | Rock, Lose -> Scissors
    | Rock, Win -> Paper
    | Paper, Lose -> Rock
    | Paper, Win -> Scissors
    | Scissors, Lose -> Paper
    | Scissors, Win -> Rock

let scoreOutcome prediction =
    match prediction with
    | Win -> 6
    | Draw -> 3
    | Lose -> 0

let roundScore2 player outcome =
    let prediction = predict player outcome
    scoreOutcome outcome + shapeScore prediction

let totalScore2 total (player, outcome) = total + roundScore2 player outcome


let secondStar () = 
    input
    |> parse2
    |> List.fold totalScore2 0