module AoC.E2022.Day03

// --- Day 3: Rucksack Reorganization ---

open AoC
open IO


let input = readInputLines "2022" "Day03" |> List.ofSeq

let parse (lines: string list) =

    let splitIntoCompartments (line: string) = line.Substring(0, line.Length / 2), line.Substring(line.Length / 2)

    let toPriority (c: char) = 
        match c with
        | c when c >= 'a'&& c <= 'z' -> int c - 96
        | c when c >= 'A'&& c <= 'Z' -> int c - 38
        | _ -> failwithf "Illegal item: %c" c

    let rucksackToPriories (compartment1, compartment2) = 
        let prioritizedItems compartment = 
            compartment 
            |> Seq.map toPriority 
            |> Set.ofSeq

        prioritizedItems compartment1, prioritizedItems compartment2

    lines
    |> List.map splitIntoCompartments
    |> List.map rucksackToPriories

let findDisplacedItem (compartment1, compartment2) =
    Set.intersect compartment1 compartment2 
    |> Set.toList 
    |> List.exactlyOne

let firstStar () =
    input 
    |> parse 
    |> List.map findDisplacedItem
    |> List.sum


let allItemsInRucksack (compartment1, compartment2) = Set.union compartment1 compartment2

let findBadge rucksacks = rucksacks |> Set.intersectMany |> Set.toList |> List.exactlyOne


let secondStar () = 
    input 
    |> parse 
    |> List.map allItemsInRucksack
    |> List.chunkBySize 3
    |> List.map findBadge
    |> List.sum