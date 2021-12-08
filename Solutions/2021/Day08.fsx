// --- Day 8: Seven Segment Search ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day08.txt") |> List.ofSeq

let example = [
    "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
    "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
    "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
    "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
    "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
    "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
    "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
    "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
    "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
    "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
]


let parse (line:string) =
    let parts = line.Split(" | ")
    let input = parts.[0]
    let output = parts.[1]

    let segments (text:string) = 
        text.Split(" ")

    segments input, segments output

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


let firstStar =
    let data = input
    data
    |> List.map parse
    |> List.map snd
    |> List.map countEasy
    |> List.sum

firstStar

let matchDigit (code:string) =
    match code.Length with
    | x when x = 2 -> Some (1, set code)
    | x when x = 3 -> Some (7, set code)
    | x when x = 4 -> Some (4, set code)
    | x when x = 7 -> Some (8, set code)
    | _ -> None

//   aaaa 
//  b    c
//  b    c
//   dddd 
//  e    f
//  e    f
//   gggg 

// 0 -> 6 
// 1 -> 2 *
// 2 -> 5
// 3 -> 5
// 4 -> 4 *
// 5 -> 5
// 6 -> 6 
// 7 -> 3 *
// 8 -> 7 *
// 9 -> 6 



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

let example1Line = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
let example1 = example1Line |> parse
let example1Input, example1Output = example1
mapDigits example1Input |> Map.toList |> List.map (fun (s, d) -> d, s |> Set.toArray |> System.String) |> Map.ofSeq


let toNumber (digits:Map<Set<char>,int>) (output:array<string>)=
    let firstDigitCode = output.[0] |> Set.ofSeq
    let secondDigitCode = output.[1] |> Set.ofSeq
    let thirdDigitCode = output.[2] |> Set.ofSeq
    let fourthDigitCode = output.[3] |> Set.ofSeq

    digits.[firstDigitCode] * 1000 + 
    digits.[secondDigitCode] * 100 +
    digits.[thirdDigitCode] * 10 +
    digits.[fourthDigitCode]

let secondStar = 
    let notes = input
    // let entry =
    notes
    |> List.map parse 
    |> List.map (fun (input, output) -> (input |> mapDigits, output) ||> toNumber)
    |> List.sum

