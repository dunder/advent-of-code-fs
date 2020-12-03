module AoC.E2020.Day03

open AoC
open IO
open Matrix

// --- Day 3: Toboggan Trajectory ---

let input = readInputLines "2020" "Day03" |> List.ofSeq

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
                | '.' -> 0
                | _ -> failwithf "Unkown character '%c'" c
            matrix.[row, column] <- state
        
    matrix

let isTree (matrix:int[,]) p =
    let width = matrix.GetLength(1)

    let x = p.X % width
    let y = p.Y
   
    matrix.[y,x] = 1

let slope p = 
    Seq.unfold (fun (last, current) -> 
        Some (last, (current, ({ X = current.X + p.X; Y = current.Y + p.Y})))
    ) (({ X = 0; Y = 0 }), ({X = p.X; Y = p.Y}))

let firstStar () =

    let matrix = parse input
    let height = matrix.GetLength(0)

    slope { X = 3; Y = 1 }
    |> Seq.takeWhile (fun p -> p.Y < height)
    |> Seq.filter (fun p -> isTree matrix p)
    |> Seq.length
        
let secondStar () = 
    
    let matrix = parse input
    let height = matrix.GetLength(0)

    seq {
        { X = 1; Y = 1}
        { X = 3; Y = 1}
        { X = 5; Y = 1}
        { X = 7; Y = 1}
        { X = 1; Y = 2}
    }
    |> Seq.fold (fun acc p -> 
        let trees =
            slope p
            |> Seq.takeWhile (fun p -> p.Y < height)
            |> Seq.filter (fun p -> isTree matrix p)
            |> Seq.length
            |> int64
        acc * trees
    ) 1L
    

module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(162, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(3064612320L, secondStar())
