// --- Day 7: Recursive Circus ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day07.txt") |> List.ofSeq

let example = [
    "pbga (66)"
    "xhth (57)"
    "ebii (61)"
    "havc (66)"
    "ktlj (57)"
    "fwft (72) -> ktlj, cntj, xhth"
    "qoyq (66)"
    "padx (45) -> pbga, havc, qoyq"
    "tknk (41) -> ugml, padx, fwft"
    "jptl (61)"
    "ugml (68) -> gyxo, ebii, jptl"
    "gyxo (61)"
    "cntj (57)"
]

let firstStar =
    0

firstStar

let secondStar = 
    0

secondStar

