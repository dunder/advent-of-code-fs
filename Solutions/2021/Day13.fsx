// --- Day 13: Transparent Origami ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day13.txt") |> List.ofSeq

let example = [
    "6,10"
    "0,14"
    "9,10"
    "0,3"
    "10,4"
    "4,11"
    "6,0"
    "6,12"
    "4,1"
    "0,13"
    "10,12"
    "3,4"
    "3,0"
    "8,4"
    "1,10"
    "2,14"
    "8,10"
    "9,0"
    ""
    "fold along y=7"
    "fold along x=5"
]

let isFoldInstruction (line: string) = line.StartsWith("fold along")

let parseDots (lines: string list) (maxXFold, maxYFold) =
    let dots =
        lines
        |> List.filter (System.String.IsNullOrEmpty >> not)
        |> List.filter (isFoldInstruction >> not)
        |> List.map (fun line -> 
            let parts = line.Split(",")
            parts.[0] |> int, parts.[1] |> int
        )

    
    // let dimensions (dots: list<int*int>) = 
    //     let maxColumn = dots |> List.map fst |> List.max
    //     let maxRow = dots |> List.map snd |> List.max
    //     maxColumn + 1, maxRow + 1

    let maxColumn = maxXFold * 2 + 1
    let maxRow = maxYFold * 2 + 1
    
    let instructions = Array2D.init maxRow maxColumn (fun _ _ -> false)
    
    dots
    |>
    List.iter (fun (column, row) -> instructions.[row, column] <- true)

    instructions


let parseFolds (lines: string list) =
    lines
    |> List.filter isFoldInstruction
    |> List.map (fun fold -> 
        let text = fold.Substring("fold along ".Length)
        let parts = text.Split("=")
        parts.[0].[0], parts.[1] |> int
        )

let calculateMaxFolds folds = 
    let maxXFold =
        folds
        |> List.filter (fun fold -> fst fold = 'x')
        |> List.head
        |> snd

    let maxYFold =
        folds
        |> List.filter (fun fold -> fst fold = 'y')
        |> List.head
        |> snd

    maxXFold, maxYFold

parseDots example
parseFolds example


let foldRow (instructions: bool[,]) row =
    printfn "dimensions = %i rows %i columns " (instructions.GetLength(0)) (instructions.GetLength(1))
    
    let upper = instructions.[0..row-1, *]
    let lower = instructions.[row+1.., *]

    printfn "trying to fold by y %i" row
    printfn "upper dimensions = %i rows %i columns " (upper.GetLength(0)) (upper.GetLength(1))
    printfn "lower dimensions = %i rows %i columns " (lower.GetLength(0)) (lower.GetLength(1))

    for row in 0..upper.GetLength(0)-1 do
        let rowLower = lower.GetLength(0)-1 - row
        for column in 0..upper.GetLength(1)-1 do
            let upperValue =  upper.[row, column] || lower.[rowLower, column]
            upper.[row, column] <- upperValue

    upper
    
let foldColumn (instructions: bool[,]) column =
    printfn "dimensions = %i rows %i columns " (instructions.GetLength(0)) (instructions.GetLength(1))

    let left = instructions.[*, 0..column-1]
    let right = instructions.[*, column+1..]

    printfn "trying to fold by x %i" column
    printfn "left dimensions = %i rows %i columns " (left.GetLength(0)) (left.GetLength(1))
    printfn "right dimensions = %i rows %i columns " (right.GetLength(0)) (right.GetLength(1))

    for column in 0..left.GetLength(1)-1 do
        let columnRight = right.GetLength(1)-1 - column
        for row in 0..left.GetLength(0)-1 do
            let leftValue =  left.[row, column] || right.[row, columnRight]
            left.[row, column] <- leftValue

    left

let allElements (a:'a[,]) =
    seq { 
        for row in 0 .. a.GetLength(0)-1 do
          for column in 0 .. a.GetLength(1)-1 do 
            yield (row, column), a.[row,column] 
    }

let allValues (a:'a[,]) = a |>allElements |> Seq.map snd

let print (instructions: bool[,]) = 
    for row in 0..instructions.GetLength(0)-1 do
        for column in 0..instructions.GetLength(1)-1 do
            match instructions.[row, column] with
            | true -> printf "#"
            | false -> printf "."
        printfn ""

let countDots (instructions: bool[,]) =
    instructions
    |> allValues
    |> Seq.filter id
    |> Seq.length

let fold folds (instructions: bool[,]) =
    folds
    |> Seq.fold (fun instructions (dimension, foldAt) ->
        match dimension with
        | 'y' -> foldRow instructions foldAt
        | 'x' -> foldColumn instructions foldAt
        | _ -> failwithf "Illegal folding dimension: %c " dimension
    ) instructions

let firstStar () =
    let data = input 
    let folds = parseFolds data
    let maxFolds = calculateMaxFolds folds
    let instructions = parseDots data maxFolds
    let firstFold = [folds.Head]
    let result = fold firstFold instructions
    countDots result

let secondStar () =
    let data = input 
    let folds = parseFolds data
    let maxFolds = calculateMaxFolds folds
    let instructions = parseDots data maxFolds
    let code = fold folds instructions
    print code