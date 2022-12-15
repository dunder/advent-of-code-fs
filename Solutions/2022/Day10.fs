module AoC.E2022.Day10

// --- Day 10: Cathode-Ray Tube ---

open AoC
open IO

open System.Text.RegularExpressions

let input = readInputLines "2022" "Day10" |> List.ofSeq


type Instruction = 
    | Add of int
    | NoOp

let (|Regex|_|) (pattern: string) (input: string) = 
    let m = Regex.Match(input, pattern) 
    if m.Success then 
        Some(List.tail [for g in m.Groups -> g.Value])
    else
        None

let parse (lines: string list) =
    lines
    |> List.map (fun line -> 
        match line with
        | Regex "addx (-?\d+)" [x] -> Add(x |> int)
        | Regex "noop" [] -> NoOp
        | _ -> failwithf "Unknown instruction: %s" line
    )

let updateRegister (cycles: int list) instruction =
    let x = cycles |> List.head
    match instruction with
    | Add x -> (x + x)::x::cycles
    | NoOp -> x::cycles

let checkAt = [ 20; 60; 100; 140; 180;220 ] 

let sumSignals total (cycle, signal) =
    if checkAt |> List.contains cycle then
        total + cycle*signal
    else 
        total

let cycles instructions =
    instructions    
    |> List.fold updateRegister [1]
    |> List.rev
    |> List.mapi (fun signal i -> 
        signal+1, i
    )

let firstStar () =
    input
    |> parse
    |> cycles
    |> List.fold sumSignals 0


let width = 40
let height = 6

let crt = Array2D.init height width (fun row column -> false)

let writeCycle (crt: bool[,]) cycleData =
    let cycle, signal = cycleData
    let spritePosition = signal
    let row = cycle / width
    let column = (cycle - 1) % width
    let lit = spritePosition - 1 = column || spritePosition = column || spritePosition + 1 = column
    crt[row, column] <- lit

let printScreen (crt: bool[,]) =
    for row in 0..height-1 do
        for column in 0..width-1 do
            let on = crt[row, column]
            printf (if on then "#" else ".")            
        printfn ""

let onScreen cycleData = fst cycleData < width * height

let secondStar () = 
    input
    |> parse    
    |> cycles
    |> List.filter onScreen
    |> List.iter (writeCycle crt)
    crt |> printScreen
    // FCJAPJRE
    "^^^ Read the screen above ^^^"