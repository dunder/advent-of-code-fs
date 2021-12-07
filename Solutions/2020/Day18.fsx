//  --- Day 18: Operation Order ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day18.txt") |> List.ofSeq

let examples = [
    "1 + (2 * 3) + (4 * (5 + 6))"
]

let secondStar = 
    0