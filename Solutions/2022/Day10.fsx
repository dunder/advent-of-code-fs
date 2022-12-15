// --- Day 10: Cathode-Ray Tube ---

open System.IO
open System.Text.RegularExpressions

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day10.txt") |> List.ofSeq

type Operation = 
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



let updateRegister (history: int list) instruction =            
    let value = history |> List.head
    match instruction with
    | Add x -> (value + x)::value::history
    | NoOp -> value::history

let example = 
    [
        "addx 15"
        "addx -11"
        "addx 6"
        "addx -3"
        "addx 5"
        "addx -1"
        "addx -8"
        "addx 13"
        "addx 4"
        "noop"
        "addx -1"
        "addx 5"
        "addx -1"
        "addx 5"
        "addx -1"
        "addx 5"
        "addx -1"
        "addx 5"
        "addx -1"
        "addx -35"
        "addx 1"
        "addx 24"
        "addx -19"
        "addx 1"
        "addx 16"
        "addx -11"
        "noop"
        "noop"
        "addx 21"
        "addx -15"
        "noop"
        "noop"
        "addx -3"
        "addx 9"
        "addx 1"
        "addx -3"
        "addx 8"
        "addx 1"
        "addx 5"
        "noop"
        "noop"
        "noop"
        "noop"
        "noop"
        "addx -36"
        "noop"
        "addx 1"
        "addx 7"
        "noop"
        "noop"
        "noop"
        "addx 2"
        "addx 6"
        "noop"
        "noop"
        "noop"
        "noop"
        "noop"
        "addx 1"
        "noop"
        "noop"
        "addx 7"
        "addx 1"
        "noop"
        "addx -13"
        "addx 13"
        "addx 7"
        "noop"
        "addx 1"
        "addx -33"
        "noop"
        "noop"
        "noop"
        "addx 2"
        "noop"
        "noop"
        "noop"
        "addx 8"
        "noop"
        "addx -1"
        "addx 2"
        "addx 1"
        "noop"
        "addx 17"
        "addx -9"
        "addx 1"
        "addx 1"
        "addx -3"
        "addx 11"
        "noop"
        "noop"
        "addx 1"
        "noop"
        "addx 1"
        "noop"
        "noop"
        "addx -13"
        "addx -19"
        "addx 1"
        "addx 3"
        "addx 26"
        "addx -30"
        "addx 12"
        "addx -1"
        "addx 3"
        "addx 1"
        "noop"
        "noop"
        "noop"
        "addx -9"
        "addx 18"
        "addx 1"
        "addx 2"
        "noop"
        "noop"
        "addx 9"
        "noop"
        "noop"
        "noop"
        "addx -1"
        "addx 2"
        "addx -37"
        "addx 1"
        "addx 3"
        "noop"
        "addx 15"
        "addx -21"
        "addx 22"
        "addx -6"
        "addx 1"
        "noop"
        "addx 2"
        "addx 1"
        "noop"
        "addx -10"
        "noop"
        "noop"
        "addx 20"
        "addx 1"
        "addx 2"
        "addx 2"
        "addx -6"
        "addx -11"
        "noop"
        "noop"
        "noop"
    ]


let example2 = [
    "noop"
    "addx 3"
    "addx -5"
]

let signal lines = 
    lines
    |> parse
    |> List.fold updateRegister [1]

let sequence = (signal example) |> List.rev
sequence

sequence[20]
sequence[60]
sequence[100]
sequence[140]
sequence[180]
sequence[220]

let signalStrengths =
    sequence  
    |> List.mapi (fun signal i -> 
        signal + 1, i
    )

let checkAt = [ 20; 60; 100; 140; 180;220 ] 

let sumSignals total (cycle, signal) =
    if checkAt |> List.contains cycle then
        total + cycle*signal
    else 
        total

signalStrengths |> List.fold sumSignals 0


let firstStar =
    (signal input) 
    |> List.rev
    |> List.mapi (fun signal i -> 
        signal + 1, i
    )
    |> List.fold sumSignals 0

firstStar

let width = 40
let height = 6

let crt = Array2D.init height width (fun row column -> false)

let output =
    (signal example) 
    |> List.rev
    |> List.mapi (fun signal i -> 
        signal + 1, i
    )

type CrtData = int*int

let writeCycle (crt: bool[,]) (crtData: CrtData) =
    let cycle, signal = crtData
    let spritePosition = signal
    let row = cycle / width
    let column = (cycle - 1) % width
    let lit = spritePosition - 1 = column || spritePosition = column || spritePosition + 1 = column
    crt[row, column] <- lit

writeCycle crt (1,1)


output 
|> List.iter (writeCycle crt)

let printScreen (crt: bool[,]) =
    for row in 0..crt.GetLength(0)-1 do
        for column in 0..crt.GetLength(1)-1 do
            let on = crt[row, column]
            printf (if on then "#" else ".")            
        printfn ""

crt |> printScreen

let secondStar = 
    (signal input) 
    |> List.rev
    |> List.mapi (fun signal i -> 
        signal + 1, i
    )
    |> List.filter (fun (cycle, _) -> cycle < 240)
    |> List.iter (writeCycle crt)
    // FCJAPJRE
    "^^^ Read the sign ^^^"

secondStar

