// --- Day 4: Camp Cleanup ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day04.txt") |> List.ofSeq

let example = [
    "2-4,6-8"
    "2-3,4-5"
    "5-7,7-9"
    "2-8,3-7"
    "6-6,4-6"
    "2-6,4-8"
]

let parse (lines: string list) = 

    lines
    |> List.map (fun (line) -> 

        let toRange (specification: string) = 
            let range = specification.Split("-")
            let start = range[0] |> int
            let stop = range[1] |> int
            start, stop
            
        let parts = line.Split(",")
        let range1 = parts[0] |> toRange
        let range2 = parts[1] |> toRange
        range1, range2
    )

example |> parse

let contains (range1, range2) = 
    
    let contain (range1, range2) = 
        let s1, e1 = range1
        let s2, e2 = range2
        s2 >= s1 && e2 <= e1

    contain (range1, range2) || contain (range2, range1)

example |> parse |> List.filter contains |> List.length
input |> parse |> List.filter contains |> List.length

let overlaps (range1, range2) = 
    let overlap (r1, r2) = 
        let s1, e1 = r1
        let s2, e2 = r2
        e2 >= s1 && s2 <= e1
    overlap (range1, range2) || overlap (range2, range1) || contains (range1, range2)

let firstStar = input |> parse |> List.filter contains |> List.length

firstStar

let secondStar = input |> parse |> List.filter overlaps |> List.length
    

secondStar

