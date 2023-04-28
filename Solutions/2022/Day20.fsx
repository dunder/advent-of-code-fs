// --- Day 20: Grove Positioning System ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day20.txt") |> List.ofSeq

let example = [ "1"; "2"; "-3"; "3"; "-2"; "0"; "4" ]

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



let test = [1; 2; -100]

let currentIndex = 2
let value = test[currentIndex]

let arrangement = test |> List.removeAt currentIndex

let newIndex = currentIndex + value
let insertBeforeIndex =
    if newIndex < 0 then
        (arrangement.Length - (abs newIndex)%arrangement.Length)
    else
        newIndex % arrangement.Length

arrangement |> List.insertAt insertBeforeIndex value


decrypt example 1 1 |> groveCoordinates

let firstStar =
    decrypt input 1 1 |> groveCoordinates

firstStar

decrypt example 811589153 10 |> groveCoordinates

let decryptionKey = 811589153

let secondStar = 
    
    decrypt input 811589153 10 |> groveCoordinates
secondStar

