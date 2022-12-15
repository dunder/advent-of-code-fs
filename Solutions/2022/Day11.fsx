// --- Day 11: Monkey in the Middle ---

open System.IO
open System.Text.RegularExpressions

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day11.txt") |> List.ofSeq

let example = 
    [
        "Monkey 0:"
        "    Starting items: 79, 98"
        "    Operation: new = old * 19"
        "    Test: divisible by 23"
        "        If true: throw to monkey 2"
        "        If false: throw to monkey 3"
        ""
        "Monkey 1:"
        "Starting items: 54, 65, 75, 74"
        "Operation: new = old + 6"
        "Test: divisible by 19"
        "    If true: throw to monkey 2"
        "    If false: throw to monkey 0"
        ""
        "Monkey 2:"
        "Starting items: 79, 60, 97"
        "Operation: new = old * old"
        "Test: divisible by 13"
        "    If true: throw to monkey 1"
        "    If false: throw to monkey 3"
        ""
        "Monkey 3:"
        "Starting items: 74"
        "Operation: new = old + 3"
        "Test: divisible by 17"
        "    If true: throw to monkey 0"
        "    If false: throw to monkey 1"
    ]

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

    let parseInt (line: string) =
        Regex.Match(line, "(\d+)").Groups[1].Value |> int
    
    let parseMonkey (lines: string list) =
        // printfn "lines: %A" lines
        let items = parseItems lines[1]
        // printfn "items: %A" items
        let operation = parseOperation lines[2]
        // printfn "operation: %A" (operation 4)
        let test = parseUInt64 lines[3]
        // printfn "test: %A" test
        let onTrue = parseUInt64 lines[4]
        // printfn "onTrue: %A" onTrue
        let onFalse = parseUInt64 lines[5]
        // printfn "onFalse: %A" onFalse
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

let monkeysExample = example |> parse

let toMonkeyMap monkeys =
    monkeys
    |> List.indexed
    |> Map.ofList

monkeysExample |> toMonkeyMap

type WorryState = { Monkeys: Map<int, Monkey> }

let exampleMonkeyMap = example |> parse |> toMonkeyMap

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
    // printfn "Monkey inspects an item with a worry level of %i" item
    let inspectingMonkey = state.Monkeys[monkeyId]
    
    // let inspectionLevel = item
    // let worryLevel = inspectingMonkey.Operation inspectionLevel    
    // let relivedLevel = relivedReduction worryLevel

    // printfn "  Monkey gets bored with item. Worry level is divided by 3 to %i" relivedLevel

    let newInspectingMonkey = { inspectingMonkey with Items = inspectingMonkey.Items.Tail }
    
    let worryLevel, test = test state.Monkeys newInspectingMonkey.Operation item inspectingMonkey.Denominator

    let receivingMonkeyId = 
        if test then
            // printfn "  Current worry level is divisible by %i" relivedLevel
            newInspectingMonkey.OnTrue
            
        else 
            // printfn "  Current worry level is not divisible by %i" relivedLevel
            newInspectingMonkey.OnFalse
    
    // printfn "  Item with worry level %i is thrown to monkey %i" relivedLevel receivingMonkeyId

    let itemReceivedState = addItem receivingMonkeyId worryLevel itemTakenState

    itemReceivedState

let inspectItemPart1 = inspectItem test1

let exampleWorryState = { Monkeys = exampleMonkeyMap }

exampleWorryState |> inspectItemPart1 0 |> inspectItemPart1 0

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

exampleWorryState |> roundPart1

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

let monkeyMap = input |> parse |> toMonkeyMap
let worryState = { Monkeys = monkeyMap }

[1..20]
|> List.fold roundFolderPart1 exampleWorryState
|> monkeyBusiness


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


[1..10000]
|> List.fold roundFolderPart2 exampleWorryState
|> monkeyBusiness

[1..10000]
|> List.fold roundFolderPart2 worryState
|> monkeyBusiness


// 294552720:  That's not the right answer; your answer is too low
// 29387694902: That's not the right answer; your answer is too low.
// 29410328972: That's not the right answer; your answer is too low.

let firstStar =
    0

firstStar

let secondStar = 
    0

secondStar

