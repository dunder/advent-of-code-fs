// >>> insert day tagline here <<<

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadAllText(@".\Input\Day06.txt")

let example = "3,4,3,1,2"

let parse (text: string) = text.Split(",") |> Seq.map System.Int32.Parse |> Seq.toList

let initialStates = [
    [2;3;2;0;1]
    [1;2;1;6;0;8]
    [0;1;0;5;6;7;8]
    [6; 0; 6; 4; 5; 6; 7; 8; 8]
]

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

nextFish initialStates.[3] |> Seq.toList

let firstStar =
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

firstStar

let exampleState = [1;2;1;6;0;8] |> List.countBy id |> List.map (fun (key,value) -> key, int64(value)) |> Map.ofList

let addNonExisting mapOfFish =
    [0..8] 
    |> List.filter (fun x -> mapOfFish |> (Map.containsKey x) |> not)
    |> List.fold (fun m x -> m |> Map.add x 0L) mapOfFish

let exampleMapOfFish = exampleState |> addNonExisting

let fishOrder = [0..8] |> List.rev

let nextFish2 (initialState:Map<int,int64>) =     
        
    fishOrder    
    |> List.fold (fun (state:Map<int,int64>) fish ->
        // printfn ">>> %i" fish
        match fish with
        | fish when fish > 0 -> 
            let nextFish = fish-1
            let currentCount = initialState.[fish]
            // printfn ">>> fish: %i, current: %i nextFish: %i newCount: %i" fish currentCount nextFish currentCount
            state |> Map.add nextFish currentCount            
        | fish when fish = 0 -> 
            let currentCount = initialState.[fish]
            let current6 = state.[6]
            // printfn ">>> fish: %i, current: %i (added to 6  and 8)" fish currentCount
            state |> Map.add 8 currentCount |> Map.add 6 (currentCount + current6)
    ) initialState

nextFish2 exampleMapOfFish

let secondStar = 
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


secondStar

// 26984457539L too low

