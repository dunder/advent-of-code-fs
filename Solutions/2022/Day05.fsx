// --- Day 5: Supply Stacks ---

open System.IO
open System.Text.RegularExpressions

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day05.txt") |> List.ofSeq

let example = [
    "    [D]    "
    "[N] [C]    "
    "[Z] [M] [P]"
    " 1   2   3 "
    ""
    "move 1 from 2 to 1"
    "move 3 from 1 to 3"
    "move 2 from 2 to 1"
    "move 1 from 1 to 2"
]

let readStackSetup (lines: string list) =
    let setupLine =
        lines 
        |> List.find (fun line -> line.Trim().StartsWith('1'))
    let setupLine = setupLine.Trim()
    setupLine[setupLine.Length-1] |> string |> int
    
example |> readStackSetup

let readStacks (lines: string list) =
    
    let stackCount = readStackSetup lines
    let initialStacks = [1..stackCount] |> List.fold (fun stacks stack -> stacks |> Map.add stack []) Map.empty

    let nextStack (stacks: Map<int, list<char>>) (line: string) =
        let readLine (state: Map<int,list<char>>) (stackNr: int) =
            let stackIndex = stackNr + (stackNr-1)*3
            if line[stackIndex] <> ' ' then
                state |> Map.add stackNr (line[stackIndex]::state[stackNr])
            else 
                state

        [1..stacks.Count]
        |> List.fold readLine stacks

    lines 
    |> List.filter (fun line -> line.Contains("["))
    |> List.rev
    |> List.fold nextStack initialStacks

let parse (lines: string list) =
    lines
    |> List.filter (fun line -> line.StartsWith("move"))
    |> List.map (fun line -> 
        let m = Regex.Match(line.Trim(), @"move (\d+) from (\d+) to (\d+)")
        m.Groups[1].Value |> int, m.Groups[2].Value |> int, m.Groups[3].Value |> int
    )

let exampleStacks = example |> readStacks    
let inputStacks = input |> readStacks

let move (stacks:Map<int,list<char>>) command =
    let count, from, to' = command
    let moveFrom = stacks[from]
    let moveTo = stacks[to']
    let fromStackAfter = moveFrom |> List.skip count
    let crates = moveFrom |> List.take count |> List.rev
    let toStackAfter =  crates @ moveTo
    
    stacks |> Map.add from fromStackAfter |> Map.add to' toStackAfter

move exampleStacks (3,2,3)

let firstStar =
    input 
    |> parse
    |> List.fold move inputStacks
    |> Map.values
    |> Seq.map (fun cs -> cs |> Seq.head)
    |> Array.ofSeq
    |> System.String

firstStar


let move9001 (stacks:Map<int,list<char>>) command =
    let count, from, to' = command
    let moveFrom = stacks[from]
    let moveTo = stacks[to']
    let fromStackAfter = moveFrom |> List.skip count
    let crates = moveFrom |> List.take count
    let toStackAfter =  crates @ moveTo
    
    stacks |> Map.add from fromStackAfter |> Map.add to' toStackAfter


let secondStar = 
    input 
    |> parse
    |> List.fold move9001 inputStacks
    |> Map.values
    |> Seq.map (fun cs -> cs |> Seq.head)
    |> Array.ofSeq
    |> System.String

secondStar

