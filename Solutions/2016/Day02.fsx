// --- Day 2: Bathroom Security ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day02.txt") |> List.ofSeq

type Direction = Up | Right | Down | Left

let toDirection c = 
    match c with
    | 'U' -> Up
    | 'R' -> Right
    | 'D' -> Down
    | 'L' -> Left
    | _ -> failwithf "Illegal direction: %c" c

let example = 
    [
        "ULL"
        "RRDDD"
        "LURDL"
        "UUUUD"
    ]

let parse (lines: string list) =
    lines
    |> List.map(fun line -> line |> Seq.map toDirection |> Seq.toList)

example |> parse

let move (x, y) direction =    
    match direction with
    | Up when y < 1 -> x, y + 1
    | Right when x < 1 -> x + 1, y
    | Down when y > -1 -> x, y - 1
    | Left when x > -1 -> x - 1, y
    | _ -> x, y

let toDigit (x, y) =
    match x, y with
    | -1,1 -> "1"
    | 0,1 -> "2"
    | 1,1 -> "3"
    | -1,0 -> "4"
    | 0,0 -> "5"
    | 1,0 -> "6"
    | -1,-1 -> "7"
    | 0,-1 -> "8"
    | 1,-1 -> "9"
    | _ -> failwithf "Illegal position: %i, %i" x y

["LURDL"] |> parse |> Seq.head |> Seq.scan move (0, 0) |> Seq.toList

input
|> parse
|> List.scan (fun (x, y) sequences -> 
    sequences 
    |> List.fold move (x, y) 
) (0,0)
|> List.skip 1
|> List.map toDigit



let firstStar =
    0

firstStar

let diamondKeypadToDigit (x, y) =
    match x, y with
    | 0, 2 -> "1"
    | -1, 1 -> "2"
    | 0, 1 -> "3"
    | 1, 1 -> "4"
    | -2, 0 -> "5"
    | -1, 0 -> "6"
    | 0, 0 -> "7"
    | 1, 0 -> "8"
    | 2, 0 -> "9"
    | -1, -1 -> "A"
    | 0, -1 -> "B"
    | 1, -1 -> "C"
    | 0, -2 -> "D"
    | _ -> failwithf "Illegal position: %i, %i" x y

let diamondMove (x, y) direction =    
    match direction with
    | Up when x = 0 && y < 2 -> x, y + 1
    | Up when (x = -1 || x = 1) && y < 1 -> x, y + 1
    | Right when y = 0 && x < 2 -> x + 1, y
    | Right when (y = 1 || y = -1) && x < 1 -> x + 1, y
    | Down when x = 0 && y > -2 -> x, y - 1
    | Down when (x = -1 || x = 1) && y > -1 -> x, y - 1
    | Left when y = 0 && x > -2 -> x - 1, y
    | Left when (y = -1 || y = 1) && x > -1 -> x - 1, y
    | _ -> x, y

input
|> parse
|> List.scan (fun (x, y) sequences -> 
    sequences 
    |> List.fold diamondMove (x, y) 
) (0,0)
|> List.skip 1
|> List.map diamondKeypadToDigit


let secondStar = 
    0

secondStar

