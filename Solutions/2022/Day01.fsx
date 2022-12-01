// --- Day 1: Calorie Counting ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadAllLines(@".\Input\Day01.txt") |> Array.toList

let parse (lines: string array) = 
    lines

let example = [
    "1000"
    "2000"
    "3000"
    ""
    "4000"
    ""
    "5000"
    "6000"
    ""
    "7000"
    "8000"
    "9000"
    ""
    "10000"
    "" // <-- hack to make last count
]

input
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

input
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

let firstStar =    
    0

firstStar

let secondStar =         
    0

secondStar

