module AoC.E2015.Day18

open AoC
open IO
open Matrix

// --- Day 18: Like a GIF For Your Yard ---

let input = readInputLines "2015" "Day18"
 
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
            matrix.[column,row] <- state
        
    matrix

let print (matrix:int[,]) : unit =
    matrix |> Array2D.iteri (fun _ column state -> 
        let c = 
            match state with 
            | 1 -> '#'
            | 0 -> '.'
            | _ -> failwithf ""
        printf "%c" c
        if column = matrix.GetLength(1)-1 then
            printfn ""
    )

let inMatrix (matrix: int[,], p: Position) =
    let rows = matrix.GetLength(0)
    let columns = matrix.GetLength(1)
    p.X >= 0 && p.X < columns && p.Y >= 0 &&  p.Y < rows

let adjacent matrix p = 
    adjacentAllDirections p 
    |> Seq.filter (fun x -> inMatrix(matrix, x))

let on (matrix: int[,], p: Position) = 
    matrix.[p.X,p.Y] = 1

let nextState matrix p state =
    
    let adjacentCount = 
        adjacent matrix p 
        |> Seq.filter (fun x -> on(matrix, x))
        |> Seq.length

    match (state, adjacentCount) with
    | (1, 2) -> 1
    | (1, 3) -> 1
    | (0, 3) -> 1
    | _ -> 0

let next (matrix:int[,], nextState:int[,]->Position->int->int) : int[,] =
    let rows = matrix.GetLength(0)
    let columns = matrix.GetLength(1)
    let newMatrix = Array2D.zeroCreate rows columns

    for row in 0..matrix.GetLength(0)-1 do
        for column in 0..matrix.GetLength(1)-1 do
            let state = matrix.[row, column]
            let p = { X = row; Y = column}
            let newState = nextState matrix p state
            newMatrix.[row, column] <- newState

    newMatrix

let corners (matrix:int[,]) = 
    let rows = matrix.GetLength(0)
    let columns = matrix.GetLength(1)

    seq {
        { X = 0; Y = 0 }
        { X = columns-1; Y = 0 }
        { X = 0; Y = rows-1 }
        { X = columns-1; Y = rows-1 }
    }
    
let nextState2 matrix p state =
    let corners = corners matrix

    if Seq.contains p corners then
        1
    else
        nextState matrix p state

let firstStar () =
    let initialMatrix = parse <| List.ofSeq input

    { 1..100 }
    |> Seq.fold (fun matrix _ -> (next(matrix,nextState))) initialMatrix
    |> Seq.cast<int>
    |> Seq.sum
   

let secondStar () = 
    let initialMatrix = parse <| List.ofSeq input
    
    for p in corners initialMatrix do
        initialMatrix.[p.X,p.Y] <- 1

    { 1..100 }
    |> Seq.fold (fun matrix _ -> (next(matrix, nextState2))) initialMatrix
    |> Seq.cast<int>
    |> Seq.sum

module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =

        Assert.Equal(1061, firstStar())

    [<Fact>]
    let ``second star`` () =

        Assert.Equal(1006, secondStar())

    type Expected() as this = 
        inherit TheoryData<int, Position>()
        do 
            this.Add(0, { X = 0; Y = 0 })
            this.Add(1,  { X = 1; Y = 0 })
            this.Add(0, { X = 2; Y = 0 })
            this.Add(1,  { X = 3; Y = 0 })
            this.Add(0, { X = 4; Y = 0 })
            this.Add(1,  { X = 5; Y = 0 })

            this.Add(0, { X = 0; Y = 1 })
            this.Add(0, { X = 1; Y = 1 })
            this.Add(0, { X = 2; Y = 1 })
            this.Add(1,  { X = 3; Y = 1 })
            this.Add(1,  { X = 4; Y = 1 })
            this.Add(0, { X = 5; Y = 1 })

            this.Add(1,  { X = 0; Y = 2 })
            this.Add(0, { X = 1; Y = 2 })
            this.Add(0, { X = 2; Y = 2 })
            this.Add(0, { X = 3; Y = 2 })
            this.Add(0, { X = 4; Y = 2 })
            this.Add(1,  { X = 5; Y = 2 })

            this.Add(0, { X = 0; Y = 3 })
            this.Add(0, { X = 1; Y = 3 })
            this.Add(1,  { X = 2; Y = 3 })
            this.Add(0, { X = 3; Y = 3 })
            this.Add(0, { X = 4; Y = 3 })
            this.Add(0, { X = 5; Y = 3 })

            this.Add(1,  { X = 0; Y = 4 })
            this.Add(0, { X = 1; Y = 4 })
            this.Add(1,  { X = 2; Y = 4 })
            this.Add(0, { X = 3; Y = 4 })
            this.Add(0, { X = 4; Y = 4 })
            this.Add(1,  { X = 5; Y = 4 })

            this.Add(1,  { X = 0; Y = 5 })
            this.Add(1,  { X = 1; Y = 5 })
            this.Add(1,  { X = 2; Y = 5 })
            this.Add(1,  { X = 3; Y = 5 })
            this.Add(0, { X = 4; Y = 5 })
            this.Add(0, { X = 5; Y = 5 })

    [<Theory; ClassData(typeof<Expected>)>]
    let ``parse test`` (expectedState: int, position: Position) : unit =
        let lines = [
            ".#.#.#"
            "...##."
            "#....#"
            "..#..."
            "#.#..#"
            "####.."
        ]

        let matrix = parse lines

        Assert.Equal(expectedState, matrix.[position.X,position.Y])

    [<Fact>]
    let ``next test`` () =
        let start = [
            ".#.#.#"
            "...##."
            "#....#"
            "..#..."
            "#.#..#"
            "####.."
        ]

        let matrix1 = parse start

        let next = next(matrix1, nextState)

        let expected = [
            "..##.."
            "..##.#"
            "...##."
            "......"
            "#....."
            "#.##.."
        ]

        let matrixExpected = parse expected

        Assert.Equal(matrixExpected, next)