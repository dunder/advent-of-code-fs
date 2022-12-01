module AoC.E2022.Day01

// --- Day 1: Calorie Counting ---

open AoC
open IO


let input = readInputLines "2022" "Day01" |> List.ofSeq

let firstStar () =
    // add a blank line at the end so that last elf is recognized
    input @ [""]
    |> List.fold (fun (elf, total) line -> 
        match line with 
        | "" -> 
            if elf > total then
                0, elf
            else 
                0, total
        | line -> 
            let calories = line |> int
            elf + calories, total
    ) (0, 0)
    |> snd

let secondStar () = 
    // add a blank line at the end so that last elf is recognized
    input @ [""]
    |> List.fold (fun (elf, total) line -> 
        match line with 
        | "" -> 
            0, elf::total
        | line -> 
            let calories = line |> int
            elf + calories, total
    ) (0, [])
    |> snd
    |> List.sortDescending
    |> List.take 3
    |> List.sum