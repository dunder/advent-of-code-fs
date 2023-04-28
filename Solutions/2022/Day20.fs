module AoC.E2022.Day20

// --- Day 20: Grove Positioning System ---

open AoC
open IO

let input = readInputLines "2022" "Day20" |> List.ofSeq

let parse (lines: list<string>) key =
    lines
    |> List.map int64
    |> List.map (fun x -> x*key)
    |> List.indexed
    |> Map.ofList

let decrypt input key times =
    
    let lookup = parse input key
    
    let initialArrangment = [0..input.Length-1]

    let translateArrangement arrangement = arrangement |> List.map (fun i -> lookup[i])
    let currentArrangement = initialArrangment
    let finalArrangement =
        [
            for _ in 1..times do
                yield! initialArrangment
        ]
        |> List.fold(fun (currentArrangement:list<int>) positionInArrangement ->

            let value = lookup[positionInArrangement]

            let currentIndexInArrangement = currentArrangement |> List.findIndex (fun x -> x = positionInArrangement)
            let currentArrangement = currentArrangement |> List.removeAt currentIndexInArrangement
            
            let newIndex =  int64 currentIndexInArrangement + value
            let insertBeforeIndex =
                if newIndex < 0 then
                    int64 currentArrangement.Length - (abs newIndex) % (int64 currentArrangement.Length)
                else
                    newIndex % int64 currentArrangement.Length

            let currentArrangement = currentArrangement |> List.insertAt (int insertBeforeIndex) positionInArrangement

            currentArrangement
        ) currentArrangement

    finalArrangement |> translateArrangement

let groveCoordinates input =
    let startIndex = input |> List.findIndex (fun x -> x = 0L)

    let forward positions =
        let index = (startIndex + positions) % input.Length
        input[index]

    forward 1000 + forward 2000 + forward 3000


let firstStar () =
    decrypt input 1 1 |> groveCoordinates

let secondStar () = 
    decrypt input 811589153 10 |> groveCoordinates