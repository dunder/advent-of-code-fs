// --- Day 21: Dirac Dice ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

// Player 1 starting position: 8
// Player 2 starting position: 5

// let input = File.ReadLines(@".\Input\Day21.txt") |> List.ofSeq

let input = [(1,8);(2,5)] |> Map.ofList

let example = [(1,4);(2,8)] |> Map.ofList

let roll times dice = [dice..dice+times-1] |> Seq.sum

let roll3 = roll 3

roll3 1

let round dice (positions:Map<int,int>) player (scoreBoard:Map<int,int>) =
    let currentPosition = positions.[player]
    let move = currentPosition + roll3 dice
    let newPosition = move % 10
    let newPosition = if newPosition = 0 then 10 else newPosition
    let newPositions = positions |> Map.add player newPosition
    let nextPlayer = if player = 1 then 2 else 1
    let currentScore = scoreBoard.[player]
    let newScoreBoard = scoreBoard |> Map.add player (currentScore + newPosition)

    dice + 3, newPositions, nextPlayer, newScoreBoard

let play dice (positions:Map<int,int>) player (scoreBoard:Map<int,int>) =
    Seq.unfold (fun (dice, positions, player, scores) ->
        let nextDice, newPositions, nextPlayer, newScores = round dice positions player scores
        let nextState = nextDice, newPositions, nextPlayer, newScores
        Some (nextState, nextState)
    ) (dice, positions, player, scoreBoard)
    |> Seq.find (fun (_, _, _, scoreBoard) -> scoreBoard.[1] >= 1000 || scoreBoard.[2] >= 1000)

let firstStar =
    let data = input
    let scores = [(1,0);(2,0)] |> Map.ofList
    let dice = 1
    let player = 1

    let dice, positions, player, scores = play dice data player scores

    let winningDice = dice-1
    let lowestScore = scores |> Map.toList |> List.map snd |> List.sort |> List.head
    winningDice * lowestScore

firstStar

let secondStar = 
    0

secondStar

