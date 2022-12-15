module AoC.E2022.Day11

// --- Day 11: Monkey in the Middle ---

open AoC
open IO

open System.Text.RegularExpressions

let input = readInputLines "2022" "Day11" |> List.ofSeq


type Monkey = { Items: uint64 list; Operation: uint64 -> uint64; Denominator: uint64; OnTrue: int; OnFalse: int; InspectionCount: uint64 }

let parse (lines: string list) =

    let parseItems (line: string) = 
        let part = line.Split(":")[1]
        part.Split(",", System.StringSplitOptions.TrimEntries)
        |> Array.map (fun x -> x |> uint64)
        |> List.ofArray
    
    let parseOperation (line: string) =
        let m = Regex.Match(line, "(\*|\+) (\d+|old)")
        let parts = m.Groups[0].Value.Split(" ")
        let operation = parts[0]
        let value = parts[1]

        let add x y = x + y
        let multiply x y = x * y
        let square x = multiply x x

        match operation with
        | "*" -> if value = "old" then square else multiply (value |> uint64)
        | "+" -> if value = "old" then multiply 2UL else add (value |> uint64)
        | _ -> failwithf "Not implemented: %s" operation    

    let parseUInt64 (line: string) =
        Regex.Match(line, "(\d+)").Groups[1].Value |> uint64
    
    let parseMonkey (lines: string list) =
        let items = parseItems lines[1]
        let operation = parseOperation lines[2]
        let test = parseUInt64 lines[3]
        let onTrue = parseUInt64 lines[4]
        let onFalse = parseUInt64 lines[5]
        {
            Items = items
            Operation = operation
            Denominator = test
            OnTrue = onTrue |> int
            OnFalse = onFalse |> int
            InspectionCount = 0UL
        }

    lines
    |> List.filter (fun line -> line <> "")
    |> List.chunkBySize 6
    |> List.map parseMonkey

let toMonkeyMap monkeys =
    monkeys
    |> List.indexed
    |> Map.ofList

type WorryState = { Monkeys: Map<int, Monkey> }

let takeItem monkeyId state = 
    let monkey = state.Monkeys[monkeyId]
    let newItems = monkey.Items.Tail
    let newMonkey = { monkey with Items = newItems; InspectionCount = monkey.InspectionCount + 1UL }
    let newMonkies = state.Monkeys |> Map.add monkeyId newMonkey
    monkey.Items.Head, { state with Monkeys = newMonkies }

let addItem monkeyId item state =
    let monkey = state.Monkeys[monkeyId]
    let newItems = monkey.Items @ [item]
    let newMonkey = { monkey with Items = newItems }
    let newMonkies = state.Monkeys |> Map.add monkeyId newMonkey
    { state with Monkeys = newMonkies }
    
let test1 monkeys operation item denominator = 
    let inspectionLevel = item
    let worryLevel = operation inspectionLevel
    let relivedLevel = worryLevel / 3UL
    relivedLevel, relivedLevel % denominator = 0UL
    

let inspectItem test monkeyId state = 
    let item, itemTakenState = takeItem monkeyId state
    let inspectingMonkey = state.Monkeys[monkeyId]
    let newInspectingMonkey = { inspectingMonkey with Items = inspectingMonkey.Items.Tail }
    
    let worryLevel, test = test state.Monkeys newInspectingMonkey.Operation item inspectingMonkey.Denominator

    let receivingMonkeyId = 
        if test then
            newInspectingMonkey.OnTrue
            
        else 
            newInspectingMonkey.OnFalse
    
    let itemReceivedState = addItem receivingMonkeyId worryLevel itemTakenState

    itemReceivedState

let inspectItemPart1 = inspectItem test1

let turn inspectItem monkeyId state =

    let turnFolder state item =
        inspectItem monkeyId state

    let monkey = state.Monkeys[monkeyId]
    let items = monkey.Items
    items
    |> List.fold turnFolder state

let turnPart1 = turn inspectItemPart1

let round turn state =
    let roundFolder state (monkeyId, monkey) =
        turn monkeyId state

    state.Monkeys
    |> Map.toList
    |> List.fold roundFolder state

let roundPart1 = round turnPart1

let roundFolderPart1 state i = 
    roundPart1 state

let monkeyBusiness state =
    let values =
        state.Monkeys.Values
        |> Seq.map (fun monkey -> monkey.InspectionCount)
        |> Seq.sortDescending
        |> Seq.take 2
        |> Seq.toArray
    values[0] * values[1]


let firstStar () =
    let monkeyMap = input |> parse |> toMonkeyMap
    let worryState = { Monkeys = monkeyMap }
    [1..20]
    |> List.fold roundFolderPart1 worryState
    |> monkeyBusiness


let test2 monkeys operation item denominator = 

    // cache it
    let primeFactor = monkeys |> Map.values |> Seq.map (fun m -> m.Denominator) |> Seq.reduce (*)

    let inspectionLevel = item
    let worryLevel = operation inspectionLevel % primeFactor
    
    worryLevel, worryLevel % denominator = 0UL

let inspectItemPart2 = inspectItem test2
let turnPart2 = turn inspectItemPart2
let roundPart2 = round turnPart2
let roundFolderPart2 state i = roundPart2 state

let secondStar () = 
    let monkeyMap = input |> parse |> toMonkeyMap
    let worryState = { Monkeys = monkeyMap }
    [1..10000]
    |> List.fold roundFolderPart2 worryState
    |> monkeyBusiness
    