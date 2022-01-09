// --- Day 21: Dirac Dice ---

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


let rec diracDice player1Position player2Position (turn:int) (scorePlayer1: int64) (scorePlayer2: int64) =
    
    let nextTurn (winsPlayer1, winsPlayer2) (outcome, occurances) =
        
        let player1Turn = turn = 0
        let player2Turn = not player1Turn
        let nextTurn = if turn = 0 then 1 else 0
        let player1NextPosition = if player1Turn then move player1Position outcome else player1Position
        let player2NextPosition = if player2Turn then move player2Position outcome else player2Position
        let player1NextScore = if player1Turn then scorePlayer1 + (player1NextPosition |> int64) else scorePlayer1
        let player2NextScore = if player2Turn then scorePlayer2 + (player2NextPosition |> int64) else scorePlayer2
                
        let (player1NextWins, player2NextWins) = diracDice player1NextPosition player2NextPosition nextTurn player1NextScore player2NextScore
        winsPlayer1 + player1NextWins * (occurances |> int64), winsPlayer2 + player2NextWins * (occurances |> int64)

    if scorePlayer1 >= 21L then
        1L, 0L
    else if scorePlayer2 >= 21L then
        0L, 1L
    else 
        outcomeByCount |> List.fold nextTurn (0L, 0L)

diracDice 4 8 0 0L 0L

let secondStar = 
    let player1Wins, player2Wins = diracDice 8 5 0 0L 0L

    max player1Wins player2Wins


secondStar

