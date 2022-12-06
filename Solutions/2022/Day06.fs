module AoC.E2022.Day06

// --- Day 6: Tuning Trouble ---

open AoC
open IO


let input = readInputText "2022" "Day06"


let findMarker (signal: string) length =

    let uniqueSequence sequence = sequence |> Set.ofArray |> Set.count = length

    signal
    |> Seq.windowed length
    |> Seq.findIndex uniqueSequence
    |> (+) length


let firstStar () = 
    findMarker input 4


let secondStar () = 
    findMarker input 14