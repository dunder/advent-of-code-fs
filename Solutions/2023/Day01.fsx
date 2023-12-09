// --- Day 1: Trebuchet?! ---

open System
open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day01.txt") |> List.ofSeq


let example = [
    "1abc2"
    "pqr3stu8vwx"
    "a1b2c3d4e5f"
    "treb7uchet"
]

let sumOfCalibrationValues input = 
    input
    |> List.map (fun line -> 
        let left = line |> Seq.find System.Char.IsDigit |> string
        let right = line |> Seq.findBack System.Char.IsDigit |> string
        left + right)
    |> List.map System.Int32.Parse
    |> List.sum

let firstStarExample = sumOfCalibrationValues example
let firstStar = sumOfCalibrationValues input

firstStar


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


let example2 = [
    "two1nine"
    "eightwothree"
    "abcone2threexyz"
    "xtwone3four"
    "4nineeightseven2"
    "zoneight234"
    "7pqrstsixteen"
]
let tryParseNumber text index =
    match index with
    | Number text n -> Some(n)
    | _ -> None

let findNumber (line: string) indeces = indeces |> Seq.map (tryParseNumber line) |> Seq.pick(id)

let findLeft (line: string) = seq {0..line.Length-1} |> (findNumber line)
let findRight (line: string) = seq {line.Length-1..-1..0} |> (findNumber line)

let sumOfTextedCalibrationValues input =
    input
    |> List.map(fun line -> findLeft line + findRight line)
    |> List.map Int32.Parse
    |> List.sum

let secondStarExample = sumOfTextedCalibrationValues example2
let secondStar = sumOfTextedCalibrationValues input


secondStar

