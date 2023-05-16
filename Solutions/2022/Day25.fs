module AoC.E2022.Day25

// --- Day 25: Full of Hot Air ---

open AoC
open IO

let input = readInputLines "2022" "Day25" |> List.ofSeq

let snafuDigitToDecimal = function
    | '2' -> 2L
    | '1' -> 1L
    | '0' -> 0L
    | '-' -> -1L
    | '=' -> -2L
    | c -> failwithf "Unrecognized input: %c" c

let decimalToSnafuDigit = function
    | 2L -> '2'
    | 1L -> '1'
    | 0L -> '0'
    | -1L -> '-'
    | -2L -> '='
    | n -> failwithf "No SNAFU digit for: %i" n

let snafuToDecimal (number: string) =
    number 
    |> Seq.rev 
    |> Seq.mapi (fun i c -> 
        let factor = if i = 0 then 1L else pown 5 i
        factor * snafuDigitToDecimal c
    ) 
    |> Seq.sum

let snafuDigits = [1L;2L;-2L;-1L; 0L]

let decimalToSnafu (n: int64) =
    List.unfold(fun (i, shift, digit, acc) ->
        if acc = n then
            None
        else
            let p = pown 5L (i-1)
            let index = ((n-shift)/p)%5L
            let digit = snafuDigits[index |> int]
            let acc = acc + digit*p
            let next = i+1, shift + 2L*p, digit, acc
            Some(next, next)
    ) (1,1L, 0L,0L)
    |> List.rev
    |> List.map (fun (_, _, digit, _) -> digit)
    |> List.map decimalToSnafuDigit
    |> List.toArray
    |> System.String


let firstStar () =
    input
    |> List.map snafuToDecimal
    |> List.sum
    |> decimalToSnafu

let secondStar () = 
    0