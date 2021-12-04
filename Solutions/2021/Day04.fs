module AoC.E2021.Day04

// --- Day 4: Giant Squid ---

open AoC
open IO

let input = readInputLines "2021" "Day04" |> List.ofSeq

let parseNumbers (data: string seq) = 
    let line = data |> Seq.head
    line.Split(",") 
    |> Seq.map System.Int32.Parse 
    |> Seq.toList

let parseBoards data = 
    let boardData = 
        data 
        |> Seq.filter (System.String.IsNullOrEmpty >> not)
        |> Seq.skip 1
        |> Seq.map (fun line -> 
            let numbers = line.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
            numbers |> Array.map System.Int32.Parse
        )
        |> Seq.chunkBySize 5    
        |> Seq.toList

    let boards = 
        boardData
        |> Seq.map (fun board -> 
            let rows = 5
            let columns = 5

            let matrix = Array2D.zeroCreate rows columns

            for column in 0..columns-1 do
                for row in 0..rows-1 do
                    matrix.[row, column] <- board.[row].[column], false

            matrix
        )
            
    boards |> Seq.toList

let flat2Darray array2D = 
    seq { for x in [0..(Array2D.length1 array2D) - 1] do 
                for y in [0..(Array2D.length2 array2D) - 1] do 
                    yield array2D.[x, y] }

let bingo (row: array<int*bool>) =
    let markCount = 
        row 
        |> Seq.map snd
        |> Seq.filter id 
        |> Seq.length
    let rowLength = row |> Seq.length
    markCount = rowLength

let hasBingo (markedBoard:(int*bool)[,]) =
    let rowBingo =
        [0..4]
        |> Seq.exists (fun row -> markedBoard.[row, *] |> bingo)
    let columnBingo = 
       [0..4]
        |> Seq.exists (fun column -> markedBoard.[*, column] |> bingo)
    rowBingo || columnBingo

let mark (board:(int*bool)[,]) (value:int)=
    let found =
        seq {
            for i in 0..4 do
                for j in 0..4 do
                    yield (i,j), fst board.[i,j]
        }
        |> 
        Seq.tryFind (fun (_,v ) -> v = value)
    match found with
    | Some ((i,j), v) -> board.[i,j] <- v, true
    | None -> ()

let score (board:(int*bool)[,]) n =
    let unmarked = 
        board 
        |> Array2D.map (fun (value, marked) -> if marked then 0 else value) 
        |> flat2Darray 
        |> Seq.sum
    unmarked * n

let firstStar() =
    let data = input
    let numbers = data |> parseNumbers
    let boards = data |> parseBoards
    let boardCount = boards |> Seq.length
    let bingo =
        seq {
            for n in numbers do
                for board in 0..boardCount-1 do
                    let a = boards.[board]
                    mark a n
                    yield boards.[board], n
        }    
        |> Seq.find (fun (board, n) -> board |> hasBingo)
    bingo ||> score

let score2 (board:(int*bool)[,]) =
    let unmarked = 
        board 
        |> Array2D.map (fun (value, marked) -> if marked then 0 else value) 
        |> flat2Darray 
        |> Seq.sum
    unmarked

let secondStar() =     
    let data = input
    let numbers = data |> parseNumbers
    let boards = data |> parseBoards
    let boardCount = boards |> Seq.length
    let bingo =
        boards
        |> Seq.mapi (fun i board ->
            
            let found =
                seq {
                for n in numbers do
                    mark board n
                    yield board, n
                }    
                |> Seq.findIndex (fun (board, n) -> board |> hasBingo)
            i, found
        )
    let boardIndex, bingoIndex =
        bingo 
        |> Seq.sortByDescending (fun (boardIndex, bingoIndex) -> bingoIndex)
        |> Seq.toList
        |> Seq.head
    
    let winningNumber = numbers.[bingoIndex] 
    let winningUnmarkedScore = score2 boards.[boardIndex] 
    winningNumber * winningUnmarkedScore