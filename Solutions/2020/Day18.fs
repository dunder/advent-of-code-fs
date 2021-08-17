module AoC.E2020.Day18

open AoC
open IO

// --- Day 18: Operation Order ---

let input = readInputLines "2020" "Day18" |> List.ofSeq

type Operator = 
    | Addition
    | Multiplication

type InputType = 
    | Number of System.Int64
    | Operator of Operator
    | ScopeStart
    | ScopeEnd
    | Terminator

type Operand =
    | Number of System.Int64
    | Scope

type EvaluationState = { Operands : Operand list; Operators : Operator list }

module Operator =
    let fromChar c = 
        match c with 
        | '+' -> Addition
        | '*' -> Multiplication
        | c -> failwithf "Unknown operator: %c" c

    let evaluate value1 value2 operator =
        match operator with
        | Addition -> value1 + value2
        | Multiplication -> value1 * value2

module EvaluationState =
    let result state = 
        match state.Operands with
        | [Number(number)] when state.Operators |> List.isEmpty -> number
        | _ -> failwithf "Not ready: %O" state

    let evaluate left right tail state = 
        let operator = state.Operators.Head
        let result = Operator.evaluate left right operator
        { state with Operands = Number(result)::tail; Operators = state.Operators.Tail }
        
    let addNumber number state =
        { state with Operands = Number(number)::state.Operands }

    let number number state = 
        match state.Operands with 
        | [] -> { state with Operands = [Number(number)] }
        | x::_ -> 
            match x with 
            | Number(left) -> state |> evaluate left number state.Operands.Tail
            | Scope -> state |> addNumber number

    let operator operator state =
        { state with Operators = operator::state.Operators }

    let openScope state =
        { state with Operands = Scope::state.Operands }

    let closeScope state = 
        match state.Operands with
        | Number(right)::Scope::Number(left)::tail -> state |> evaluate left right tail
        | Number(right)::Scope::tail -> { state with Operands = Number(right)::tail }
        | _ -> failwithf "Unexpected state: %O" state

let (|Int64|_|) char = 
    match System.Int64.TryParse (char.ToString()) with
    | (true, int) -> Some(int)
    | _ -> None

let classify c : InputType =
    match c with
    | Int64 int -> InputType.Number(int)
    | c when c = '(' -> ScopeStart
    | c when c = ')' -> ScopeEnd
    | c -> Operator((Operator.fromChar c))

let evaluate (state: EvaluationState) (input: InputType, next: InputType) = 
    match input with
    | InputType.Number(number) -> state |> EvaluationState.number number
    | Operator(operator) -> state |> EvaluationState.operator operator
    | ScopeStart -> state |> EvaluationState.openScope
    | ScopeEnd -> state |> EvaluationState.closeScope
    | Terminator -> state

let run input = 
    let classified = 
        input
        |> Seq.filter (System.Char.IsWhiteSpace >> not)
        |> Seq.map classify
       
    [Terminator]
    |> Seq.append classified
    |> Seq.pairwise
    |> Seq.fold evaluate { Operands = []; Operators = [] }
    |> EvaluationState.result

let runAll (input:string list) =
    input
    |> Seq.sumBy run

let example1 () =
    run "1 + 2 * 3 + 4 * 5 + 6"

let example2 () =
    run "1 + (2 * 3) + (4 * (5 + 6))"

let example3 () =
    run "2 * 3 + (4 * 5)"

let example4 () =
    run "5 + (8 * 3 + 9 + 3 * 4 * 3)"

let example5 () =
    run "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"

let example6 () =
    run "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"

let firstStar () =
    runAll input

let secondStar () = 
    0


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(12956356593940L, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(-1, secondStar())
