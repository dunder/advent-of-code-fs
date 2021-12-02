// --- Day 2: ? ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day02.txt") |> List.ofSeq

let parse (instruction:string) =
    let parts = instruction.Split(" ")
    let heading = 
        match parts.[0] with
        | "up" -> 0
        | "down" -> 1
        | "forward" -> 2
        | h -> failwithf "Unkonwn heading: %s" h
    (heading, parts.[1] |> System.Int32.Parse)

let firstStar () =   
    
    let answer = 
        input
        |> Seq.map parse
        |> Seq.fold (fun state heading -> 
            let direction, amount = heading
            let distance, depth = state
        
            match direction with
            | 0 -> distance, depth - amount
            | 1 -> distance, depth + amount
            | 2 -> distance + amount, depth
            

        ) (0,0)
    fst answer * snd answer

let secondStar () = 
    let answer = 
        input
        |> Seq.map parse
        |> Seq.fold (fun state heading -> 
            let direction, amount = heading
            let distance, depth, aim = state
        
            match direction with
            | 0 -> distance, depth, aim - amount
            | 1 -> distance, depth, aim + amount
            | 2 -> distance + amount, depth + amount*aim, aim
            

        ) (0,0,0)
    let distance, depth, aim = answer
    distance * depth