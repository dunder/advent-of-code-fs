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
                | '#' -> 1
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

let transform matrix (copy:int[,]) p = 

    let adjacent = adjacentAllDirections p |> Seq.filter (inMatrix matrix) |> Seq.filter (fun p -> not (isFloor matrix p))
    
    if isAvailable matrix p then
        let allAvailable = adjacent |> Seq.forall (fun x -> isAvailable matrix x)
        if allAvailable then
            copy.[p.Y, p.X] <- 2
    elif isTaken matrix p then
        let taken = adjacent |> Seq.filter (fun x -> isTaken matrix x) |> Seq.length
        if taken >= 4 then
            copy.[p.Y, p.X] <- 1

    copy

let adjacentLine (matrix:int[,]) (d:Direction) (p:Position) = 
    let p = move d p
    p |> List.unfold (fun state -> 
        let outside = not (inMatrix matrix state)
        if outside then
            None
        else
            let next = move d state
            Some (state, next)
    )
    |> List.tryFind (fun x -> 
        let pos = x
        let result = (not (isFloor matrix x))
        result
    )
    
let transform2 matrix (copy:int[,]) p = 

    let n = adjacentLine matrix Direction.North p
    let ne = adjacentLine matrix Direction.NorthEeast p
    let e = adjacentLine matrix Direction.East p
    let se = adjacentLine matrix Direction.SouthEast p
    let s = adjacentLine matrix Direction.South p
    let sw = adjacentLine matrix Direction.SouthWest p
    let w = adjacentLine matrix Direction.West p
    let nw = adjacentLine matrix Direction.NorthWest p

    let adjacent = [n; ne; e; se; s; sw; w; nw] |> List.choose (id)

    if isAvailable matrix p then
        let allAvailable = adjacent |> Seq.forall (fun x -> isAvailable matrix x)
        if allAvailable then
            copy.[p.Y, p.X] <- 2
    elif isTaken matrix p then
        let taken = adjacent |> Seq.filter (fun x -> isTaken matrix x) |> Seq.length
        if taken >= 5 then
            copy.[p.Y, p.X] <- 1

    copy

let transformAll transform (matrix:int[,]) = 
    let copy = Array2D.copy matrix
    seq {
        for y in 0..matrix.GetLength(0)-1 do
            for x in 0..matrix.GetLength(1)-1 do
                { X = x; Y = y }
    }
    |> Seq.fold (fun m p -> 
        let x = transform matrix m p
        x
    ) copy

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

let firstStar () =
    
    let m = parse input
    
    m
    |> Seq.unfold (fun matrix -> 
        let taken = countTaken matrix
        let mNext = matrix |> (transformAll transform)
        let takenAfter = countTaken mNext
        print mNext
        printfn ""
        if taken = takenAfter then
            None
        else 
            Some (mNext, mNext)
    )
    |> Seq.last |> countTaken
    
let secondStar () = 

    let m = parse input
    m
    |> Seq.unfold (fun matrix -> 
        let taken = countTaken matrix
        let mNext = matrix |> (transformAll transform2)
        let takenAfter = countTaken mNext
        print mNext
        printfn ""
        if taken = takenAfter then
            None
        else 
            Some (mNext, mNext)
    )
    |> Seq.last |> countTaken


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(2283, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(2054, secondStar())
