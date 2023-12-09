module AoC.E2023.Day01

// --- Day 1: Trebuchet?! ---

open AoC
open IO
open System


let input = readInputLines "2023" "Day01" |> List.ofSeq

let sumOfCalibrationValues input = 
    input
    |> List.map (fun line -> 
        let left = line |> Seq.find System.Char.IsDigit |> string
        let right = line |> Seq.findBack System.Char.IsDigit |> string
        left + right)
    |> List.map System.Int32.Parse
    |> List.sum

let numbers = ["one"; "two";"three";"four";"five";"six";"seven";"eight";"nine"]

let toDigit number =
    match number with
    | "one" -> "1"
    | "two" -> "2"
    | "three" -> "3"
    | "four" -> "4"
    | "five" -> "5"
    | "six" -> "6"
    | "seven" -> "7"
    | "eight" -> "8"
    | "nine" -> "9"
    | _ -> failwithf "Not a number: %s" number

let (|Number|_|) (number: string) index = 
    
    let rest = number[index..]
    let first = rest[0]
    if Char.IsDigit first then 
        Some(first |> string)
    else 
        match numbers |> Seq.tryFind rest.StartsWith with
        | Some n -> Some (toDigit n)
        | None -> None

let tryParseNumber text index =
    match index with
    | Number text n -> Some(n)
    | _ -> None

let findNumber (line: string) indeces = indeces |> Seq.map (tryParseNumber line) |> Seq.pick(id)
let findLeft (line: string) = seq {0..line.Length-1} |> findNumber line
let findRight (line: string) = seq {line.Length-1..-1..0} |> findNumber line
let sumOfTextedCalibrationValues input =
    input
    |> List.map(fun line -> findLeft line + findRight line)
    |> List.map Int32.Parse
    |> List.sum

let firstStar () =
    sumOfCalibrationValues input

let secondStar () = 
    sumOfTextedCalibrationValues input