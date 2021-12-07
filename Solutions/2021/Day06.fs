module AoC.E2021.Day06

// --- Day 6: Lanternfish ---

open AoC
open IO


let input = readInputText "2021" "Day06"

let parse (text: string) = text.Split(",") |> Seq.map System.Int32.Parse |> Seq.toList

let nextFish fish =     
    fish
    |> List.map (fun singleFish -> 
        let newFish = singleFish - 1
        if newFish = -1 then
            [6;8]
        else
            [newFish]
    )
    |> List.collect id

let firstStar() =
    let data = input
    let initialFish = parse data

    let endState =
        Seq.unfold (fun state -> 
            let fish, t = state
            if t > 80 then
                None
            else 
                let newFish = nextFish fish
                let nextState = newFish, t + 1
                Some (state, nextState)
        ) (initialFish, 0)
        |> Seq.last
    endState |> fst |> Seq.length

let addNonExisting mapOfFish =
    [0..8] 
    |> List.filter (fun x -> mapOfFish |> (Map.containsKey x) |> not)
    |> List.fold (fun m x -> m |> Map.add x 0L) mapOfFish

let fishOrder = [0..8] |> List.rev

let nextFish2 (initialState:Map<int,int64>) =     
        
    fishOrder    
    |> List.fold (fun (state:Map<int,int64>) fish ->

        match fish with
        | fish when fish > 0 -> 
            let nextFish = fish-1
            let currentCount = initialState.[fish]
            state |> Map.add nextFish currentCount            
        | fish when fish = 0 -> 
            let currentCount = initialState.[fish]
            let current6 = state.[6]
            state |> Map.add 8 currentCount |> Map.add 6 (currentCount + current6)
        | _ -> failwith "An error occurred"
    ) initialState

let secondStar () = 
    let data = input
    let initialFish = parse data 

    let initialState = 
        initialFish 
        |> List.countBy id 
        |> List.map (fun (key,value) -> key, int64(value)) 
        |> Map.ofList 
        |> addNonExisting

    let endState =
        Seq.unfold (fun state -> 
            let fish, t = state
            if t > 256 then
                None
            else 
                let newFish = nextFish2 fish
                let nextState = newFish, t + 1
                Some (state, nextState)
        ) (initialState, 0)
        |> Seq.last
    endState 
    |> fst
    |> Map.toList
    |> List.map snd
    |> Seq.sum