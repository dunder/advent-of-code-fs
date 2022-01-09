// --- Day 18: Snailfish ---

open System.IO
open System.Text.RegularExpressions

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day18.txt") |> List.ofSeq

type Number = 
  | Pair of left: Number * right: Number 
  | Value of int


let inline charToInt c = int c - int '0'

let rec parse (line:seq<char>) = 

    let rec parseRight (input: seq<char>) =
        
        if input |> Seq.isEmpty then
            failwith "parseRight: Unexpected empty seq"

        match input |> Seq.head with
        | '[' -> parse input
        | ',' -> parseRight (input |> Seq.tail)
        | c when System.Char.IsDigit(c) -> 
            let value = charToInt c
            Value value, input |> Seq.tail
        | c -> failwithf "parseRight: Unexpected token %c at %O" c (input |> Seq.toList)

    let rec parseLeft (input: seq<char>) =
        if input |> Seq.isEmpty then
            failwithf "parseLeft: Unexpected empty seq"
        
        match input |> Seq.head with
        | '[' -> parse input
        | c when System.Char.IsDigit(c) -> 
            let value = charToInt c
            Value value, (input |> Seq.tail)
        | c -> failwithf "parseLeft: Unexpected token %c at %O" c (input |> Seq.toList)

    let left, remaining = parseLeft (line |> Seq.tail)
    let right, remaining = parseRight (remaining |> Seq.tail)

    Pair (left, right), (remaining |> Seq.tail)

parse "[1,2]"
parse "[[1,2],3]"
parse "[9,[8,7]]"
parse "[[1,9],[8,5]]"
parse "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]"
parse "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]"
parse "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"

let traverse (number : Number) =
    let rec loop (path: list<Number>) (number : Number) = 
        seq {

            match number with
            | Value _ -> ()
            | Pair (left, right) ->
                yield number::path
                yield! loop (number::path) left
                yield! loop (number::path) right
        }
    loop [] number

let magnitude (number : Number) =
    let rec loop sum (number : Number) =
        match number with
            | Value value -> sum + value
            | Pair (left, right) -> 3 * (loop sum left) + 2 * (loop sum right)
    loop 0 number

magnitude (fst (parse "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"))


traverse (fst (parse "[1,[2,3]]")) |> Seq.toList
traverse (fst (parse "[[[[[9,8],1],2],3],4]")) |> Seq.toList
traverse (fst (parse "[7,[6,[5,[4,[3,2]]]]]")) |> Seq.toList
traverse (fst (parse "[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")) |> Seq.toList

let example = "[[[[[9,8],1],2],3],4]"

type SearchState = { Input: string; Depth: int; Index: int; Result: string option }

module SearchState =

    let head state = state.Input.[state.Index]
    let tail state = state.Input.[state.Index+1..]
    let rest state = state.Input.[state.Index..]
    let left state = state.Input.[0..state.Index-1]
    let right skip state = state.Input.[state.Index+skip..]
    let giveUp state = state.Input.Length - 1 - state.Index < 5

    let private nextDepth state = 
        match state |> head with
        | '[' -> { state with Depth = state.Depth + 1 }
        | ']' -> { state with Depth = state.Depth - 1 }
        | _ -> state

    let private nextIndex state = { state with Index = state.Index + 1 }

    let next state = state |> nextDepth |> nextIndex

    let tryGetExplodeCandidate state = 
        let explodePattern = @"^\[(\d+),(\d+)\]"
        let explodeCandidate = state |> rest
        let m = Regex.Match(explodeCandidate, explodePattern)
        
        if m.Success then 
            let values = List.tail [for g in m.Groups -> g.Value] |> List.map int

            Some(values.[0], values.[1], m.Value.Length)
        else 
            None

    let hasResult state = 
        match state.Result with 
        | Some _ -> true 
        | None -> false

    let endOfSearch state = 
        let indexOutOfBounds = state.Index = state.Input.Length
        state |> hasResult || indexOutOfBounds

    let replaceLeft value state = 
        let left = state |> left
        let found = left |> Seq.tryFindIndexBack System.Char.IsDigit
        match found with 
        | Some index ->     
            let searchFrom = left.[0..index]
            let start = searchFrom |> Seq.findIndexBack (System.Char.IsDigit >> not)
            let replace = left.[start+1..index]
            let replacement = value + (replace |> int) |> string
            left.[0..start] + replacement + left.[index+1..]
        | None -> left

    let replaceRight value skip state =
        let right = state |> right skip
        let found = right |> Seq.tryFindIndex System.Char.IsDigit
        match found with
        | Some index ->
            let searchFrom = right.[index..]
            let stop = searchFrom |> Seq.findIndex (System.Char.IsDigit >> not)
            let replace = right.[index..index+stop-1]
            let replacement = value + (replace |> int) |> string
            right.[0..index-1] + replacement + right.[index+replace.Length..]

        | None -> right

let explode (input: string) =

    let initialSearchState = { Input = input; Depth = 0; Index = 0; Result = None }

    Seq.unfold (fun state -> 
        
        let explodeCandidate = state |> SearchState.tryGetExplodeCandidate

        match state.Depth, explodeCandidate with
        | depth, Some (left, right, skip) when depth >= 4 -> 
            let left = state |> SearchState.replaceLeft left
            let right = state |> SearchState.replaceRight right skip
            let result = left + "0" + right

            let nextState = { state with Result = Some result }
            Some (nextState, nextState)
        | _, _ ->             
            let nextState = state |> SearchState.next
            Some (nextState, nextState)
    ) initialSearchState
    |> Seq.find SearchState.endOfSearch


type SplitSearchState = { Input: string; Index: int; Result: string option }

module SplitSearchState =

    let rest state = state.Input.[state.Index..]
    
    let left state = state.Input.[0..state.Index-1]
    let right length state = state.Input.[state.Index+length..]

    let hasResult state = 
        match state.Result with 
        | Some _ -> true 
        | None -> false

    let endOfSearch state = 
        let indexOutOfBounds = state.Index = state.Input.Length
        state |> hasResult || indexOutOfBounds

    let tryGetSplitCandidate state = 
        let splitPattern = @"^(\d{2,})"
        let splitCandidate = state |> rest
        let m = Regex.Match(splitCandidate, splitPattern)

        if m.Success then
            let value = List.tail [for g in m.Groups -> g.Value] |> List.exactlyOne
            Some ((value |> int), value.Length)
        else    
            None

let split (input: string) =
    
    let initialSearchState = { Input = input; Index = 0; Result = None }

    Seq.unfold (fun state -> 
        
        let splitCandidate = state |> SplitSearchState.tryGetSplitCandidate

        match splitCandidate with
        | Some (number, length) -> 
            let left = number / 2
            let right = number / 2 + (if number % 2 <> 0 then 1 else 0)
            let newPair = sprintf "[%i,%i]" left right
            let result = Some ((state |> SplitSearchState.left) + newPair + (state |> SplitSearchState.right length))
            let nextState = { state with Index = state.Index + length; Result = result }
            Some (nextState, nextState)
        | None -> 
            let nextState = { state with Index = state.Index + 1 }
            Some (nextState, nextState)
    ) initialSearchState
    |> Seq.find SplitSearchState.endOfSearch

split "[[[[0,7],4],[15,[0,13]]],[1,1]]"
split "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]"
split "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]"

let state = { Input = "[[[[[9,8],1],2],3],4]"; Depth = 4; Index = 4; Result = None }

state |> SearchState.tryGetExplodeCandidate
state |> SearchState.replaceLeft 4
state |> SearchState.replaceRight 5 5

explode "[[[[[9,8],1],2],3],4]"
explode "[7,[6,[5,[4,[3,2]]]]]"
explode "[[6,[5,[4,[3,2]]]],1]"
explode "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
explode "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
explode "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"

explode "[[[[4,0],[5,4]],[[7,7],[0,[6,7]]]],[10,[[11,9],[11,0]]]]"

explode "[[[[12,12],[6,14]],[[15,0],[17,[8,1]]]],[2,9]]"


type ReduceState = { Number: string; Reduced: bool }

module ReduceState = 
    let reduced state = state.Reduced
    let number state = state.Number

let reduce (input:string) = 
    let explodeInitialState = { Number = input; Reduced = false }
    let afterExplode = 
        Seq.unfold (fun state ->
            let explodeResult = explode state.Number
            match explodeResult.Result with
            | Some result -> 
                // printfn "after explode: %s" result
                let nextState = { Number = result; Reduced = false }
                Some (nextState, nextState)
            | None -> 
                let nextState = { Number = explodeResult.Input; Reduced = true }
                Some (nextState, nextState)
        ) explodeInitialState
        |> Seq.find ReduceState.reduced
  
    
    let splitResult = split afterExplode.Number

    match splitResult.Result with
    | Some result -> 
        // printfn "after split: %s" result
        result
    | None -> splitResult.Input

let reduceLoop (input: string) =
    let initialState = { Number = input; Reduced = false }
    Seq.unfold (fun state -> 
        let result = reduce state.Number
        
        let nextState = 
            if result = state.Number then
                { Number = result; Reduced = true }
            else 
                { Number = result; Reduced = false }

        Some (nextState, nextState)
    ) initialState
    |> Seq.find ReduceState.reduced
    |> ReduceState.number

reduceLoop "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"

let add left right = sprintf "[%s,%s]" left right

let addLoop (lines: string list) = 

    lines
    |> List.reduce (fun left right ->
        let result = add left right
        // printfn "after add: %s" result
        reduceLoop result
    )

let numbers = 
    [
        "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
        "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
        "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
        "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
        "[7,[5,[[3,8],[1,4]]]]"
        "[[2,[2,2]],[8,[8,1]]]"
        "[2,9]"
        "[1,[[[9,3],9],[[9,0],[0,7]]]]"
        "[[[5,[7,4]],7],1]"
        "[[[[4,2],2],6],[8,7]]"
    ]

addLoop numbers

add numbers.[0] numbers.[1]

reduceLoop "[[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]"

let assignment = 
    [
        "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"
        "[[[5,[2,8]],4],[5,[[9,9],0]]]"
        "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"
        "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"
        "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"
        "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"
        "[[[[5,4],[7,7]],8],[[8,3],8]]"
        "[[9,3],[[9,9],[6,[4,9]]]]"
        "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"
        "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
    ]

let firstStar =
    let sum = addLoop input
    let number, _ = parse sum
    magnitude number

add "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]" "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"
|> reduceLoop
|> parse
|> fst
|> magnitude

firstStar

let rec combinations n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (combinations (k-1) xs) @ combinations k xs


let secondStar = 
    combinations 2 input
    |> List.map (fun combination -> 
        let left = combination |> List.head
        let right = combination |> List.last
        [
            add left right
            |> reduceLoop
            |> parse
            |> fst
            |> magnitude

            add right left
            |> reduceLoop
            |> parse
            |> fst
            |> magnitude
        ]
    )
    |> List.collect id
    |> List.max

secondStar

