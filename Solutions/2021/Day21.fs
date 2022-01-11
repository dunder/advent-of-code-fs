module AoC.E2021.Day21

// --- Day 21: Dirac Dice ---

open AoC
open IO


let input = [(1,8);(2,5)] |> Map.ofList


let roll times dice = [dice..dice+times-1] |> Seq.sum

let roll3 = roll 3

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

let firstStar () =
    let data = input
    let scores = [(1,0);(2,0)] |> Map.ofList
    let dice = 1
    let player = 1

    let dice, positions, player, scores = play dice data player scores

    let winningDice = dice-1
    let lowestScore = scores |> Map.toList |> List.map snd |> List.sort |> List.head
    winningDice * lowestScore

// 27 different outcomes when throwing the dice 3 times
let outcomes = 
    [
        for i in 1..3 do
            for j in 1..3 do
                for k in 1..3 ->
                    i + j + k
    ]

// totals the occurances of each outcome, the total 3 occurrs one time (1+1+1), 
// the total 4 occurrs 3 times (1+1+2, 1+2+1, 2+1+1) etc
let outcomeByCount = outcomes |> List.countBy id

// the end position after moving a number of steps from one position
let move start steps = 
    let stop = (start + steps) % 10
    if stop = 0 then 10 else stop

// https://wj32.org/wp/2016/01/13/f-code-memoize-a-recursive-function/
let memoize f =
    let mem = System.Collections.Generic.Dictionary<'a, 'b>();
    let rec g key = h g key
    and h r key =
        match mem.TryGetValue(key) with
        | (true, value) -> value
        | _ ->
        let value = f g key
        mem.Add(key, value)
        value
    g

let diracDice recurseF (player1Position, player2Position, scorePlayer1, scorePlayer2) = 
    let nextTurn (winsPlayer1, winsPlayer2) (outcome, occurances) =
        
        // alternating players between levels so this will actually be player 2 for levels 2, 4 etc
        let player1NextPosition = move player1Position outcome
        let player1NextScore = scorePlayer1 + (player1NextPosition |> int64)
                
        // swap the players for the next round
        let (player2NextWins, player1NextWins) = recurseF(player2Position,player1NextPosition,scorePlayer2,player1NextScore)

        winsPlayer1 + player1NextWins * (occurances |> int64), winsPlayer2 + player2NextWins * (occurances |> int64)

    // since we are alternating players for every "level", this check is effectively a check that player 1 has reached 
    // the winning score after a full turn (both players have made 3*n throws with the dice)
    if scorePlayer2 >= 21L then
        0L, 1L
    else 
        outcomeByCount |> List.fold nextTurn (0L, 0L)

let playDiracDice player1Position player2Position = ((memoize diracDice) (player1Position, player2Position, 0L, 0L))

let secondStar () = 
    let player1Wins, player2Wins = playDiracDice 8 5 

    max player1Wins player2Wins