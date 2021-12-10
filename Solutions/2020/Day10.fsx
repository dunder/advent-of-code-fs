// --- Day 10: Adapter Array ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day10.txt") |> List.ofSeq

let example = [
    "16"
    "10"
    "15"
    "5"
    "1"
    "11"
    "7"
    "19"
    "6"
    "12"
    "4"
]

let parse lines = lines |> List.map System.Int32.Parse

let device adapters = (adapters |> Seq.max) + 3

let firstStar =
    let adapters = parse example
    let device = device adapters
    let supply = 0

    let devices = supply::device::adapters

    devices |> List.sort


firstStar

let secondStar = 
    0

secondStar