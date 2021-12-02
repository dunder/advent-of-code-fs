module AoC.E2021.Day02

open AoC
open IO

// --- Day 2: Dive! ---

let input = readInputLines "2021" "Day02" |> List.ofSeq

type Heading = Up | Down | Forward
type Command = { Heading: Heading; Value: int }
type Position = { Distance: int; Depth: int }
type SubmarineState = { Position: Position; Aim: int }

let parse (instruction:string) =
    let parts = instruction.Split(" ")
    
    let heading = 
        match parts.[0] with
        | "up" -> Up
        | "down" -> Down
        | "forward" -> Forward
        | h -> failwithf "Unkonwn heading: %s" h

    let value = parts.[1] |> System.Int32.Parse

    { Heading = heading; Value = value }

let firstStar () =
    
    let answer = 
        input
        |> Seq.map parse
        |> Seq.fold (fun position command -> 
            match command.Heading with
            | Up -> { position with Depth = position.Depth - command.Value}
            | Down -> { position with Depth = position.Depth + command.Value }
            | Forward -> { position with Distance = position.Distance + command.Value }
        ) { Distance = 0; Depth = 0 }

    answer.Distance * answer.Depth

let secondStar () = 
    let answer = 
        input
        |> Seq.map parse
        |> Seq.fold (fun state command ->
            match command.Heading with
            | Up ->  { state with Aim = state.Aim - command.Value }
            | Down -> { state with Aim = state.Aim + command.Value} 
            | Forward -> { 
                state with 
                    Position = { 
                        Distance = state.Position.Distance + command.Value
                        Depth = state.Position.Depth + command.Value*state.Aim
                    }
                }
        ) { Position = { Distance = 0; Depth = 0 }; Aim = 0 }
    
    answer.Position.Distance * answer.Position.Depth
