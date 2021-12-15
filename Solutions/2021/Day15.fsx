// >>> insert day tagline here <<<

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day15.txt") |> List.ofSeq

let example = [
    "1163751742"
    "1381373672"
    "2136511328"
    "3694931569"
    "7463417111"
    "1319128137"
    "1359912421"
    "3125421639"
    "1293138521"
    "2311944581"
]

type Queue<'a> =
    | Queue of 'a list * 'a list

module Queue =
    let empty = Queue([], [])

    let enqueue e q = 
        match q with
        | Queue(fs, bs) -> Queue(e :: fs, bs)

    let dequeue q = 
        match q with
        | Queue([], []) -> failwith "Empty queue!"
        | Queue(fs, b :: bs) -> b, Queue(fs, bs)
        | Queue(fs, []) -> 
            let bs = List.rev fs
            bs.Head, Queue([], bs.Tail)

    let isEmpty q = 
        function
        | Queue([], []) -> true
        | _ -> false

let allElements (a:'a[,]) =
    seq { 
        for row in 0 .. a.GetLength(0)-1 do
          for column in 0 .. a.GetLength(1)-1 do 
            yield (row, column), a.[row,column] 
    }

let allValues (a:'a[,]) = a |>allElements |> Seq.map snd

let parse (lines: string list) =
    let rows = lines |> Seq.length
    let columns = lines.[0] |> Seq.length

    let inline charToInt c = int c - int '0'

    Array2D.init rows columns (fun row column -> lines.[row].[column] |> charToInt)

let adjacent (a:'a[,]) p =
    let row, column = p
    let height = a.GetLength(0)
    let width = a.GetLength(1)
    
    let north = row, column - 1
    let east = row + 1, column
    let south = row, column + 1
    let west = row - 1, column

    let withinBounds width height (row, column) = row >= 0 && row < height && column >= 0 && column < width

    let within = withinBounds width height
    
    let directions = [north; east; south; west]
    
    seq {
        for x in 0..directions.Length-1 do
            let direction = directions.[x]
            if within direction then
                yield direction
    }


let firstStar =
    let riskLevels = parse example
    riskLevels

firstStar

let secondStar = 
    0

secondStar

