// --- Day 21: Monkey Math ---

open System.IO
open System.Text.RegularExpressions

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day21.txt") |> List.ofSeq

let example = [
    "root: pppw + sjmn"
    "dbpl: 5"
    "cczh: sllz + lgvd"
    "zczc: 2"
    "ptdq: humn - dvpt"
    "dvpt: 3"
    "lfqf: 4"
    "humn: 5"
    "ljgn: 2"
    "sjmn: drzm * dbpl"
    "sllz: 4"
    "pppw: cczh / lfqf"
    "lgvd: ljgn * ptdq"
    "drzm: hmdt - zczc"
    "hmdt: 32"
]

let (|Regex|_|) pattern input = 
    let m = Regex.Match(input, pattern)
    
    if m.Success 
    then Some(List.tail [for g in m.Groups -> g.Value])
    else None

type Operation =
    | Add
    | Subtract
    | Multiply
    | Divide


type Job =
    | YellNumber of int64
    | YellResult of string*Operation*string
    | YellUnknown

let parse humanUnknown lines = 
    lines
    |> List.map (fun line ->
        match line with
        | Regex "([a-z]+): ([a-z]+) ([\+\-\*\/]) ([a-z]+)" [monkey; monkey1; operand; monkey2] ->
            let operation =
                match operand with 
                | "+" -> Add
                | "-" -> Subtract
                | "*" -> Multiply
                | "/" -> Divide
                | _ -> failwithf "Unexpected operand: %s" operand

            monkey, YellResult(monkey1, operation, monkey2)
        | Regex "([a-z]+): (\d+)" [monkey; number] when humanUnknown && monkey = "humn" ->
            monkey, YellUnknown
        | Regex "([a-z]+): (\d+)" [monkey; number] ->
            monkey, YellNumber(number |> int64)
        | x -> failwithf  "Unexpected format in line %s" x
    )


let resolveRoot (monkeyLookup: Map<string, Job>) =
    let root = monkeyLookup["root"]
    let rec resolve job =
        match job with
        | YellNumber number -> number
        | YellResult (monkey1, operation, monkey2) ->
            let resolvedMonkey1 = resolve monkeyLookup[monkey1]
            let resolvedMonkey2 = resolve monkeyLookup[monkey2]
            match operation with
            | Add -> resolvedMonkey1 + resolvedMonkey2
            | Subtract -> resolvedMonkey1 - resolvedMonkey2
            | Multiply -> resolvedMonkey1 * resolvedMonkey2
            | Divide -> resolvedMonkey1 / resolvedMonkey2
        | YellUnknown -> failwith "Unknown is not part of part I"

    resolve root

example |> parse false |> Map.ofList |> resolveRoot
input |> parse false |> Map.ofList |> resolveRoot


type Result =
    | Number of int64
    | Unknown

let rec resolve (monkeyLookup:Map<string,Job>) job =
    match job with
    | YellNumber number -> Number(number)
    | YellResult (monkey1, operation, monkey2) ->

        let resolvedMonkey1 = resolve monkeyLookup monkeyLookup[monkey1]
        let resolvedMonkey2 = resolve monkeyLookup monkeyLookup[monkey2]

        match resolvedMonkey1, resolvedMonkey2 with
        | Number n1, Number n2 -> 
            match operation with
            | Add -> Number(n1 + n2)
            | Subtract -> Number(n1 - n2)
            | Multiply -> Number(n1 * n2)
            | Divide -> Number(n1 / n2)
        | _ -> Unknown
    | YellUnknown -> Unknown


let opertationToString = function
    | Add -> "+"
    | Subtract -> "-"
    | Multiply -> "*"
    | Divide -> "/"

let rec solve (monkeyLookup:Map<string,Job>) target job =
    match job with 
    | YellNumber number -> Number(number)
    | YellResult(monkey1, operation, monkey2) -> 
        
        printfn "target: %A" target
        printfn "  %s %s %s" monkey1 (opertationToString operation) monkey2

        let resolvedMonkey1 = resolve monkeyLookup monkeyLookup[monkey1]
        let resolvedMonkey2 = resolve monkeyLookup monkeyLookup[monkey2]

        printfn "  %A %s %A" resolvedMonkey1 (opertationToString operation) resolvedMonkey2

        match resolvedMonkey1, resolvedMonkey2 with
        | Number n1, Number n2 ->
            match operation with
            | Add -> Number(n1 + n2)
            | Subtract -> Number(n1 - n2)
            | Multiply -> Number(n1 * n2)
            | Divide -> Number(n1 / n2)
        | Unknown, Number n ->
            match operation with
            | Add ->
                let newTarget = target - n
                let nextJob = monkeyLookup[monkey1]
                solve monkeyLookup newTarget nextJob
            | Subtract ->
                let newTarget = target + n
                let nextJob = monkeyLookup[monkey1]
                solve monkeyLookup newTarget nextJob
            | Multiply -> 
                let newTarget = target / n
                let nextJob = monkeyLookup[monkey1]
                solve monkeyLookup newTarget nextJob
            | Divide ->
                let newTarget = target * n
                let nextJob = monkeyLookup[monkey1]
                solve monkeyLookup newTarget nextJob
        | Number n, Unknown ->
            match operation with
            | Add ->
                let newTarget = target - n
                let nextJob = monkeyLookup[monkey2]
                solve monkeyLookup newTarget nextJob
            | Subtract ->
                let newTarget = n - target
                let nextJob = monkeyLookup[monkey2]
                solve monkeyLookup newTarget nextJob
            | Multiply -> 
                let newTarget = target / n
                let nextJob = monkeyLookup[monkey2]
                solve monkeyLookup newTarget nextJob
            | Divide ->
                let newTarget = n / target
                let nextJob = monkeyLookup[monkey2]
                solve monkeyLookup newTarget nextJob
        | Unknown, Unknown -> failwith "Unsolvable equation"

    | YellUnknown -> Number(target)

let leftMonkey job =
    match job with
    | YellResult(left, operation, right) -> left
    | _ -> failwithf "Only YellResult has left"


let rightMonkey job =
    match job with
    | YellResult(left, operation, right) -> right
    | _ -> failwithf "Only YellResult has right"

let findMonkey (monkeyLookup:Map<string,Job>) monkey =
    monkeyLookup |> Map.filter (fun _ job -> 
        match job with
        | YellResult(monkey1, _, monkey2) when monkey1 = monkey || monkey2 = monkey -> true
        | _ -> false
    )
    |> Map.toList
    |> List.exactlyOne

let monkeyLookup = input |> parse true |> Map.ofList

let root = monkeyLookup["root"] 

monkeyLookup[root |> rightMonkey] |> resolve monkeyLookup
monkeyLookup[root |> leftMonkey] |> resolve monkeyLookup

monkeyLookup[root |> leftMonkey] |> solve monkeyLookup 80164970870816L

// 80164970870816M % 8M


// 3441198826073L -> 
// That's not the right answer. If you're stuck, make sure you're using the full 
// input data; there are also some general tips on the about page, or you can ask
// for hints on the subreddit. Please wait one minute before trying again.
// (You guessed 3441198826073L.)

findMonkey monkeyLookup "humn" |> snd |> rightMonkey

example |> parse true
let test = example |> parse true |> Map.ofList 

test["humn"]


let firstStar =
    0

firstStar

let secondStar = 
    0

secondStar

