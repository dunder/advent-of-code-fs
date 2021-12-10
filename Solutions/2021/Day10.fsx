// --- Day 10: Syntax Scoring ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day10.txt") |> List.ofSeq

let example = [
    "[({(<(())[]>[[{[]{<()<>>"
    "[(()[<>])]({[<{<<[]>>("
    "{([(<{}[<>[]}>{[]{[(<()>"
    "(((({<>}<{<{<>}{[]{[]{}"
    "[[<[([]))<([[{}[[()]]]"
    "[{[{({}]{}}([{[{{{}}([]"
    "{<[[]]>}<{[{[{[]{()[[[]"
    "[<(<(<(<{}))><([]([]()"
    "<{([([[(<>()){}]>(<<{{"
    "<{([{{}}[<[[[<>{}]]]>[]]"
]

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

let expectedOpening closing =
    match closing with 
    | ']' -> '['
    | '}' -> '{'
    | '>' -> '<'
    | ')' -> '('
    | _ -> failwithf "Unexpected closing token: %c" closing    

let closingMatch opening closing = 
    match opening with 
    | '[' -> closing = ']'
    | '{' -> closing = '}'
    | '<' -> closing = '>'
    | '(' -> closing = ')'
    | _ -> false

closingMatch '{' ']'

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

parse "{([(<{}[<>[]}>{[]{[(<()>" // corrupt
parse "[({(<(())[]>[[{[]{<()<>>" // incomplete

let firstStar =
    input
    |> List.map corrupted
    |> List.choose id
    |> List.map snd
    |> List.choose id
    |> List.map fst 
    |> List.map score
    |> Seq.sum

firstStar

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

let scoreExample = ['{'; '{'; '['; '['; '('; '{'; '('; '[']

totalAutocompleteScore scoreExample

let secondStar =
    let scores = 
        input
        |> List.filter (fun line -> 
            let result = corrupted line 
            match result with 
            | Some _ -> false
            | None -> true
        )
        |> List.map (parse >> Seq.last >> fst)
        |> List.map totalAutocompleteScore
        |> List.sort
    scores.[(scores.Length-1)/2]

// 204602241 too low

secondStar

