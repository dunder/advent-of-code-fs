module AoC.E2020.Day11

open AoC
open IO
open Matrix 

// --- Day 11: Seating System ---

let input = readInputLines "2020" "Day11" |> List.ofSeq

let parse (lines: list<string>) : int[,] =
    let rows = Seq.length lines
    let columns = String.length <| Seq.head lines
    let matrix = Array2D.zeroCreate rows columns
    
    for column in 0..columns-1 do
        for row in 0..rows-1 do
            let c = lines.[row].[column]
            let state = 
                match c with 
                | '#' -> 2
                | 'L' -> 1
                | '.' -> 0
                | _ -> failwithf "Unkown character '%c'" c
            matrix.[row, column] <- state
        
    matrix

let isAvailable (matrix:int[,]) p = matrix.[p.Y,p.X] = 1

let isTaken (matrix:int[,]) p = matrix.[p.Y, p.X] = 2

let isFloor (matrix:int[,]) p = matrix.[p.Y, p.X] = 0

let inMatrix (matrix:int[,]) p =
    let height = matrix.GetLength(0)
    let width = matrix.GetLength(1)
    p.X >= 0 && p.Y >= 0 && p.X < width && p.Y < height

let countTaken (matrix:int[,]) =
    seq {
        for y in 0..matrix.GetLength(0)-1 do
            for x in 0..matrix.GetLength(1)-1 do
                if matrix.[y,x] = 2 then 1 else 0
    }
    |> Seq.sum

let transform matrix p = 

    let adjacent = adjacentAllDirections p |> Seq.filter (inMatrix matrix) |> Seq.filter (isFloor matrix >> not)
    
    if isAvailable matrix p then
        let allAvailable = adjacent |> Seq.forall (fun x -> isAvailable matrix x)
        if allAvailable then
            2
        else
            matrix.[p.Y, p.X]
    elif isTaken matrix p then
        let taken = adjacent |> Seq.filter (fun x -> isTaken matrix x) |> Seq.length
        if taken >= 4 then
            1
        else
            matrix.[p.Y, p.X]
    else
        matrix.[p.Y, p.X]

let visible (matrix:int[,]) (p:Position) (d:Direction)= 
    move d p
    |> List.unfold (fun state -> 
        if inMatrix matrix state |> not then
            None
        else
            let next = move d state
            Some (state, next)
    )
    |> List.tryFind (isFloor matrix >> not)

let transformVisible matrix p = 

    let adjacentVisible = allDirections |> Seq.map (visible matrix p) |> Seq.choose (id)

    if isAvailable matrix p then
        let allAvailable = adjacentVisible |> Seq.forall (fun x -> isAvailable matrix x)
        if allAvailable then
            2
        else
            matrix.[p.Y, p.X]
    elif isTaken matrix p then
        let taken = adjacentVisible |> Seq.filter (fun x -> isTaken matrix x) |> Seq.length
        if taken >= 5 then
            1
        else
            matrix.[p.Y, p.X]
    else
        matrix.[p.Y, p.X]

let transformAll transform (matrix:int[,]) = 
    matrix
    |> Array2D.mapi (fun y x _ -> { X = x; Y = y} |> transform matrix)
   
let print (matrix:int[,]) : unit =
    matrix |> Array2D.iteri (fun _ column state -> 
        let c = 
            match state with 
            | 2 -> '#'
            | 1 -> 'L'
            | 0 -> '.'
            | _ -> failwithf ""
        printf "%c" c
        if column = matrix.GetLength(1)-1 then
            printfn ""
    )

let seatsTakenWhenStable lines transform = 
    parse lines
    |> Seq.unfold (fun matrix -> 
        let taken = countTaken matrix
        let mNext = matrix |> (transformAll transform)
        let takenAfter = countTaken mNext
        if taken = takenAfter then
            None
        else 
            Some (mNext, mNext)
    )
    |> Seq.last |> countTaken

let firstStar () =
    seatsTakenWhenStable input transform
    
let secondStar () = 
    seatsTakenWhenStable input transformVisible


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(2283, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(2054, secondStar())
