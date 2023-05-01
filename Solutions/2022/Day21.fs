module AoC.E2022.Day21

// --- Day 21: Monkey Math ---

open System.Text.RegularExpressions

open AoC
open IO

let input = readInputLines "2022" "Day21" |> List.ofSeq

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

let rec resolve job (monkeyLookup:Map<string,Job>) =
    match job with
    | YellNumber number -> Some(number)
    | YellResult (monkey1, operation, monkey2) ->

        let resolvedMonkey1 = resolve monkeyLookup[monkey1] monkeyLookup
        let resolvedMonkey2 = resolve monkeyLookup[monkey2] monkeyLookup

        match resolvedMonkey1, resolvedMonkey2 with
        | Some n1, Some n2 -> 
            match operation with
            | Add -> Some(n1 + n2)
            | Subtract -> Some(n1 - n2)
            | Multiply -> Some(n1 * n2)
            | Divide -> Some(n1 / n2)
        | _ -> None
    | YellUnknown -> None


let opertationToString = function
    | Add -> "+"
    | Subtract -> "-"
    | Multiply -> "*"
    | Divide -> "/"

let rec solve (monkeyLookup:Map<string,Job>) target job =
    match job with 
    | YellNumber number -> Some(number)
    | YellResult(monkey1, operation, monkey2) -> 
        
        let resolvedMonkey1 = resolve monkeyLookup[monkey1] monkeyLookup
        let resolvedMonkey2 = resolve monkeyLookup[monkey2] monkeyLookup

        match resolvedMonkey1, resolvedMonkey2 with
        | Some n1, Some n2 ->
            match operation with
            | Add -> Some(n1 + n2)
            | Subtract -> Some(n1 - n2)
            | Multiply -> Some(n1 * n2)
            | Divide -> Some(n1 / n2)
        | None, Some n ->
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
        | Some n, None ->
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
        | None, None -> failwith "Unsolvable equation"

    | YellUnknown -> Some(target)

let monkeyLookup = input |> parse true |> Map.ofList

let root = monkeyLookup["root"] 

let firstStar () =
    let root = monkeyLookup["root"] 

    input 
    |> parse false
    |> Map.ofList 
    |> resolve root

let secondStar () = 
    let monkeyLookup = input |> parse true |> Map.ofList
    let root = monkeyLookup["root"]

    match root with
    | YellResult(left, operation, right) ->
        let leftResult = resolve monkeyLookup[left] monkeyLookup
        let rightResult = resolve monkeyLookup[right] monkeyLookup
        match leftResult, rightResult with
        | None, Some n -> solve monkeyLookup n monkeyLookup[left]
        | Some n, None -> solve monkeyLookup n monkeyLookup[right]
        | _ -> failwith "Unexpected equation"
    | _ -> failwithf "Only YellResult has right"