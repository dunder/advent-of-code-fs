module AoC.E2021.Day10

// --- Day 10: Syntax Scoring ---

open AoC
open IO


let input = readInputLines "2021" "Day10" |> List.ofSeq


let isClosing c =
    match c with 
    | ']' -> true
    | '}' -> true
    | '>' -> true
    | ')' -> true
    | _ -> false

let expectedClosing opening =
    match opening with 
    | '[' -> ']'
    | '{' -> '}'
    | '<' -> '>'
    | '(' -> ')'
    | _ -> failwithf "Unexpected token: %c" opening

let parse (line:string) =
    line 
    |> Seq.scan (fun state c -> 
        let stack, error = state

        if stack |> List.isEmpty then
            c::stack, None
        else             
            if isClosing c then
                let toBeClosed = stack |> List.head
                let expected = expectedClosing toBeClosed
                if c = expected then
                    stack |> List.tail, None
                else   
                    stack, Some (c, expected)
            else 
                c::stack, None
    ) ([], None)
    |> Seq.toList

let corrupted (line:string) =
    parse line
    |> Seq.tryFind (fun (stack, error) -> 
            match error with
            | Some _ -> true
            | None -> false
        )

let score c =
    match c with 
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> 0

let firstStar () =
    input
    |> List.map corrupted
    |> List.choose id
    |> List.map snd
    |> List.choose id
    |> List.map (fst >> score)
    |> Seq.sum

let autocompleteScore c = 
    match c with 
    | '(' -> 1L
    | '[' -> 2L
    | '{' -> 3L
    | '<' -> 4L
    | _ -> 0L

let totalAutocompleteScore chars = 
    chars
    |> Seq.fold (fun state c -> 
        let score = autocompleteScore c
        state * 5L + score
    ) 0L

let incomplete (line: string) =
    let result = corrupted line 
    match result with 
    | Some _ -> false
    | None -> true


let secondStar () =
    let scores = 
        input
        |> List.filter incomplete
        |> List.map (parse >> Seq.last >> fst)
        |> List.map totalAutocompleteScore
        |> List.sort
    scores.[(scores.Length-1)/2]