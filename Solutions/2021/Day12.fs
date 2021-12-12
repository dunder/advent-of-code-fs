module AoC.E2021.Day12

// --- Day 12: Passage Pathing ---

open AoC
open IO


let input = readInputLines "2021" "Day12" |> List.ofSeq


let parse lines =
    lines
    |> List.map (fun (line:string) -> 
        let parts = line.Split("-")
        [parts.[0], parts.[1]; parts.[1], parts.[0]]
    )
    |> List.collect id
    |> List.groupBy fst
    |> List.map (fun (key, value) -> key, value |> List.map snd)
    |> Map.ofList
    |> Map.add "end" []

let isSmallCave (cave:string) = System.Char.IsLower(cave.[0])

let adjacentCaves (caveMap:Map<string, string list>) cave = caveMap.[cave]

type ExplorerState = {
        Start: string
        End: string
        Next: string
        Visited: Map<string, int>
        Paths: string list list
        CurrentPath: string list 
        Level: int }

module ExplorerState =

    let init start stop = { 
        Start = start
        End = stop
        Next = start
        Visited = Map.empty
        Paths = []
        CurrentPath = []
        Level = 0 }

    let setNext cave (state: ExplorerState) = { state with Next = cave }

    let addToCurrentPath cave (state: ExplorerState) = {
            state with 
                CurrentPath = cave::state.CurrentPath }

    let completePath (state: ExplorerState) = { 
            state with 
                Paths = state.CurrentPath::state.Paths
                CurrentPath = [] }

    let addToVisited cave (state: ExplorerState) = 
        { state with
            Visited = 
                if cave = state.Start then
                    state.Visited
                else if isSmallCave cave then 
                    if state.Visited |> Map.containsKey cave then
                        state.Visited |> Map.add cave 2
                    else
                        state.Visited |> Map.add cave 1
                else state.Visited }

    let setNextLevel (state: ExplorerState) = { state with Level = state.Level + 1 }

    let pathsFromOther (otherState: ExplorerState) (state: ExplorerState) = { state with Paths = otherState.Paths}

let explore (caveMap:Map<string, string list>) isNotVisited start stop =

    let initialState = ExplorerState.init start stop

    let rec loop state = 
        let cave = state.Next

        let state = 
            state 
            |> ExplorerState.setNextLevel
            |> ExplorerState.addToVisited cave
            |> ExplorerState.addToCurrentPath cave

        if cave = stop then
            state |> ExplorerState.completePath
        else
            adjacentCaves caveMap cave
            |> List.filter (isNotVisited state)
            |> Seq.fold (fun innerState cave -> 
                let nextState = 
                    state 
                    |> ExplorerState.setNext cave
                    |> ExplorerState.pathsFromOther innerState
                loop nextState
            ) state

    loop initialState

let firstStar () =

    let isNotVisited (state: ExplorerState) cave = 
        if cave = state.Start then
            false 
        else if isSmallCave cave then
            if state.Visited |> Map.containsKey cave then 
                false
            else 
                true
        else 
            true

    let caveMap = parse input
    let endState = explore caveMap isNotVisited "start" "end" 
    endState.Paths.Length

let secondStar () = 

    let isNotVisited (state: ExplorerState) cave =
        if cave = state.Start then
            false 
        else if isSmallCave cave then
            if state.Visited |> Map.containsKey cave then 
                state.Visited |> Map.forall (fun key value -> value < 2)
            else 
                true
        else 
            true

    let caveMap = parse input
    let endState = explore caveMap isNotVisited "start" "end" 
    endState.Paths.Length