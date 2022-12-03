// --- Day 3: Rucksack Reorganization ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day03.txt") |> List.ofSeq

let parse (lines: string list) =

    let splitIntoCompartments (line: string) =
        line.Substring(0, line.Length / 2), line.Substring(line.Length / 2)

    let toPriority (c: char) = 
        match c with
        | c when c >= 'a'&& c <= 'z' -> int c - 96
        | c when c >= 'A'&& c <= 'Z' -> int c - 38
        | _ -> failwithf "Illegal item: %c" c

    let rucksackToPriories (comp1, comp2) = 
        comp1 |> Seq.map toPriority |> Set.ofSeq, comp2 |> Seq.map toPriority |> Set.ofSeq

    lines
    |> List.map splitIntoCompartments
    |> List.map rucksackToPriories

let firstStar =
    input 
    |> parse 
    |> List.map (fun (compartment1, compartment2) -> Set.intersect compartment1 compartment2 |> Set.toList |> List.exactlyOne)
    |> List.sum


firstStar

let findBadge rucksacks = rucksacks |> Set.intersectMany |> Set.toList |> List.exactlyOne

let secondStar = 
    input 
    |> parse 
    |> List.map (fun (comp1, comp2) -> Set.union comp1 comp2)
    |> List.chunkBySize 3
    |> List.map findBadge
    |> List.sum


secondStar

