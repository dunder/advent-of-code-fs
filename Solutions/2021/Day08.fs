module AoC.E2021.Day08

// --- Day 8: Seven Segment Search ---

open AoC
open IO


let input = readInputLines "2021" "Day08" |> List.ofSeq


let parse (entry:string) =
    let parts = entry.Split(" | ")
    let signalPatterns = parts.[0]
    let output = parts.[1]

    let signalPattern (text:string) = text.Split(" ")

    signalPattern signalPatterns, signalPattern output

let isUniqueDigit (code:string) =
    match code.Length with
    | x when x = 2 -> true // 1
    | x when x = 3 -> true // 7
    | x when x = 4 -> true // 4
    | x when x = 7 -> true // 8
    | _ -> false

let countEasy output =    
    output 
    |> Array.filter isUniqueDigit
    |> Array.length


let firstStar () =
    let data = input
    data
    |> List.map (parse >> snd >> countEasy)
    |> List.sum


let matchDigit (code:string) =
    match code.Length with
    | x when x = 2 -> Some (1, set code)
    | x when x = 3 -> Some (7, set code)
    | x when x = 4 -> Some (4, set code)
    | x when x = 7 -> Some (8, set code)
    | _ -> None

let mapDigits input =
    let easySegments =
        input
        |> Array.map matchDigit
        |> Array.choose id
        |> Map.ofArray

    let one = easySegments.[1]
    let four = easySegments.[4]
    let seven = easySegments.[7]
    let eight = easySegments.[8]

    let candidates235 = input |> Array.filter (fun code -> code.Length = 5) |> Array.map (fun code -> set code)
    let candidates069 = input |> Array.filter (fun code -> code.Length = 6) |> Array.map (fun code -> set code)

    let nine = 
        candidates069 
        |> Array.filter (fun candidate -> 
            let rest = candidate - seven - four
            rest |> Seq.length = 1
        )
        |> Array.exactlyOne

    let candidates06 = candidates069 |> Array.filter (fun code -> code <> nine)

    let e = eight - nine
    let g = nine - seven - four

    let six = candidates06 |> Array.filter (fun code -> code - one |> Set.count = 5) |> Array.exactlyOne
    let zero = candidates06 |> Array.filter (fun code -> code <> six) |> Array.exactlyOne
    let three = candidates235 |> Array.filter (fun code -> code - one |> Set.count = 3) |> Array.exactlyOne

    let candidates25 = candidates235 |> Array.filter (fun code -> code <> three)
    let five = candidates25 |> Array.filter (fun code -> code - nine |> Set.count = 0) |> Array.exactlyOne
    let two = candidates25 |> Array.filter (fun code -> code <> five) |> Array.exactlyOne

    [(zero, 0); (one, 1); (two, 2); (three, 3); (four, 4); (five, 5); (six, 6); (seven, 7); (eight, 8); (nine, 9) ]
    |> Map.ofList


let toNumber (digits:Map<Set<char>,int>) (output:array<string>)=
    let firstDigitCode = output.[0] |> Set.ofSeq
    let secondDigitCode = output.[1] |> Set.ofSeq
    let thirdDigitCode = output.[2] |> Set.ofSeq
    let fourthDigitCode = output.[3] |> Set.ofSeq

    digits.[firstDigitCode] * 1000 + digits.[secondDigitCode] * 100 + digits.[thirdDigitCode] * 10 + digits.[fourthDigitCode]

let secondStar() = 
    let notes = input
    
    notes
    |> List.map parse 
    |> List.map (fun (input, output) -> (input |> mapDigits, output) ||> toNumber)
    |> List.sum