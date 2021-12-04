// --- Day 4: Giant Squid ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day04.txt") |> List.ofSeq

let example = [
    "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
    ""
    "22 13 17 11  0"
    " 8  2 23  4 24"
    "21  9 14 16  7"
    " 6 10  3 18  5"
    " 1 12 20 15 19"
    ""
    " 3 15  0  2 22"
    " 9 18 13 17  5"
    "19  8  7 25 23"
    "20 11 10 24  4"
    "14 21 16 12  6"
    ""
    "14 21 17 24  4"
    "10 16 15  9 19"
    "18  8 23 26 20"
    "22 11 13  6  5"
    " 2  0 12  3  7"
]

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
    

let boardExample:(int*bool)[,] = Array2D.init 5 5 (fun i j -> (i + j + i*4 + 1, false))

boardExample.[1,0] <- fst boardExample.[1,0], true
boardExample.[1,1] <- fst boardExample.[1,1], true
boardExample.[1,2] <- fst boardExample.[1,2], true
boardExample.[1,3] <- fst boardExample.[1,3], true
boardExample.[1,4] <- fst boardExample.[1,4], true

hasBingo boardExample

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


mark boardExample 7
boardExample

boardExample |> flat2Darray

let score (board:(int*bool)[,]) n =
    let unmarked = 
        board 
        |> Array2D.map (fun (value, marked) -> if marked then 0 else value) 
        |> flat2Darray 
        |> Seq.sum
    unmarked * n

let firstStar =
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

firstStar

let score2 (board:(int*bool)[,]) =
    let unmarked = 
        board 
        |> Array2D.map (fun (value, marked) -> if marked then 0 else value) 
        |> flat2Darray 
        |> Seq.sum
    unmarked

let secondStar =     
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

secondStar

