module AoC.E2022.Day05

// --- Day 5: Supply Stacks ---

open System.Text.RegularExpressions

open AoC
open IO


let input = readInputLines "2022" "Day05" |> List.ofSeq


let readStackSetup (lines: string list) =
    let setupLine =
        lines 
        |> List.find (fun line -> line.Trim().StartsWith('1'))
    let setupLine = setupLine.Trim()
    setupLine[setupLine.Length-1] |> string |> int

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

let readCommands (lines: string list) =
    lines
    |> List.filter (fun line -> line.StartsWith("move"))
    |> List.map (fun line -> 
        let m = Regex.Match(line.Trim(), @"move (\d+) from (\d+) to (\d+)")
        m.Groups[1].Value |> int, m.Groups[2].Value |> int, m.Groups[3].Value |> int
    )

let takeCrates9000 fromStack count = fromStack |> List.take count |> List.rev

let move crateMover (stacks:Map<int,list<char>>) command =
    let count, fromStack, toStack = command
    let moveFrom = stacks[fromStack]
    let moveTo = stacks[toStack]
    let fromStackAfter = moveFrom |> List.skip count
    let crates = crateMover moveFrom count
    let toStackAfter =  crates @ moveTo
    
    stacks |> Map.add fromStack fromStackAfter |> Map.add toStack toStackAfter

let crateMover9000 = move takeCrates9000

let run crateMover (lines: string list) =
    lines 
    |> readCommands
    |> List.fold crateMover (input |> readStacks)
    |> Map.values
    |> Seq.map (fun cs -> cs |> Seq.head)
    |> Array.ofSeq
    |> System.String

let firstStar () = run crateMover9000 input
    
let takeCrates9001 fromStack count = fromStack |> List.take count
let crateMover9001 = move takeCrates9001

let secondStar () = run crateMover9001 input