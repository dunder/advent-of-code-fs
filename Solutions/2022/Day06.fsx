// --- Day 6: Tuning Trouble ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadAllText(@".\Input\Day06.txt")


let findMarker (signal: string) =
    signal
    |> Seq.windowed 4
    |> Seq.findIndex (fun sequence -> sequence |> Set.ofArray |> Set.count = 4)

let indexOfEnd (signal: string) =
    let index = signal |> findMarker
    index + 4    

"mjqjpqmgbljsphdztnvjfqwrcgsmlb" |> indexOfEnd
"bvwbjplbgvbhsrlpgdmjqwftvncz" |> indexOfEnd

input |> indexOfEnd

let findMarker2 (signal: string) =
    signal
    |> Seq.windowed 14
    |> Seq.findIndex (fun sequence -> sequence |> Set.ofArray |> Set.count = 14)

let indexOfStart (signal: string) =
    let index = signal |> findMarker2
    index + 14


input |> indexOfStart

let firstStar =
    0

firstStar

let secondStar = 
    0

secondStar

