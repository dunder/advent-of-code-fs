// --- Day 12: Passage Pathing ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day12.txt") |> List.ofSeq

let example = [
    "start-A"
    "start-b"
    "A-c"
    "A-b"
    "b-d"
    "A-end"
    "b-end"
]

let example2 = [
    "dc-end"
    "HN-start"
    "start-kj"
    "dc-start"
    "dc-HN"
    "LN-dc"
    "HN-end"
    "kj-sa"
    "kj-HN"
    "kj-dc"
]

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

parse example

let isSmallCave (cave:string) = System.Char.IsLower(cave.[0])

type queue<'a> =
    | Queue of 'a list * 'a list

module Queue =
    let empty = Queue([], [])

    let enqueue e q = 
        match q with
        | Queue(fs, bs) -> Queue(e :: fs, bs)

    let dequeue q = 
        match q with
        | Queue([], []) -> failwith "Empty queue!"
        | Queue(fs, b :: bs) -> b, Queue(fs, bs)
        | Queue(fs, []) -> 
            let bs = List.rev fs
            bs.Head, Queue([], bs.Tail)

    let isEmpty q = 
        function
        | Queue([], []) -> true
        | _ -> false

type ExplorerState = { 
        Next: string
        Visited: Set<string>
        Paths: string list list
        CurrentPath: string list 
        Level: int
        Queue: queue<string>
    }

let adjacentCaves (caveMap:Map<string, string list>) cave = caveMap.[cave]

module ExplorerState =

    let takeNext (state: ExplorerState) =
        let cave, queue = state.Queue |> Queue.dequeue
        cave, { state with Queue = queue}

    let setNext cave (state: ExplorerState) = {
        state with 
            Next = cave
    }

    let enqueue cave (state: ExplorerState) = { state with Queue = state.Queue |> Queue.enqueue cave}

    let addToCurrentPath cave (state: ExplorerState) = {
            state with 
                CurrentPath = cave::state.CurrentPath
        }

    let completePath (state: ExplorerState) = { 
            state with 
                Paths = state.CurrentPath::state.Paths
                CurrentPath = []
        }

    let addToVisited cave (state: ExplorerState) = 
        { state with
            Visited = if isSmallCave cave then state.Visited |> Set.add cave else state.Visited
        }

    let isNotVisited cave (state: ExplorerState) = not (state.Visited |> Set.contains cave)

    let setNextLevel (state: ExplorerState) = { state with Level = state.Level + 1 }

    let pathsFromOther (otherState: ExplorerState) (state: ExplorerState) = { state with Paths = otherState.Paths}

let explore (caveMap:Map<string, string list>) start stop =

    let initialState = { 
        Next = start
        Visited = Set.empty |> Set.add start
        Paths = []
        CurrentPath = [] 
        Level = 0 
        Queue = Queue.empty |> Queue.enqueue start }

    Seq.unfold (fun state -> 

        let cave, state = state |> ExplorerState.takeNext

        let state = state |> ExplorerState.addToCurrentPath cave

        printfn "Exploring: %s" cave
        printfn "    State: %O" state

        if cave = stop then
            printfn ">>>> Reached the end of this path <<<<"
            let nextState = state |> ExplorerState.completePath
            Some (state, nextState)
        else
            let adjacent = 
                adjacentCaves caveMap cave
                |> List.filter (fun cave -> state |> ExplorerState.isNotVisited cave)
            printfn "    adjacent: %O" adjacent
            let nextState = 
                adjacent
                |> List.fold (fun state cave -> 
                    state 
                    // istället för visited måste man hålla state på "current path" i varje nod
                    // som läggs på kön (kan räcka med en "prev-pekare")
                    |> ExplorerState.addToVisited cave // skit i den här, ha en "current path visited" istället.
                    |> ExplorerState.enqueue cave) state
            Some (state, nextState)
    ) initialState


let firstStar =
    let caveMap = parse example
    let endState = explore caveMap "start" "end" 
    endState
    |> Seq.take 2
    |> Seq.toList
    
firstStar

type ExplorerState2 = {
        Start: string
        End: string
        Next: string
        Visited: Map<string, int>
        Paths: string list list
        CurrentPath: string list 
        Level: int
    }


module ExplorerState2 =

    let init start stop = { 
        Start = start
        End = stop
        Next = start
        Visited = Map.empty
        Paths = []
        CurrentPath = []
        Level = 0}

    let setNext cave (state: ExplorerState2) = {
        state with 
            Next = cave
    }

    let addToCurrentPath cave (state: ExplorerState2) = {
            state with 
                CurrentPath = cave::state.CurrentPath
        }

    let completePath (state: ExplorerState2) = { 
            state with 
                Paths = state.CurrentPath::state.Paths
                CurrentPath = []
        }

    let addToVisited cave (state: ExplorerState2) = 
        { state with
            Visited = 
                if cave = state.Start then
                    state.Visited
                else if isSmallCave cave then 
                    if state.Visited |> Map.containsKey cave then
                        state.Visited |> Map.add cave 2
                    else
                        state.Visited |> Map.add cave 1
                else state.Visited
        }

    let setNextLevel (state: ExplorerState2) = { state with Level = state.Level + 1 }

    let pathsFromOther (otherState: ExplorerState2) (state: ExplorerState2) = { state with Paths = otherState.Paths}

let explore2 (caveMap:Map<string, string list>) isNotVisited start stop =

    let initialState = ExplorerState2.init start stop

    let rec loop state = 
        let cave = state.Next

        let state = 
            state 
            |> ExplorerState2.setNextLevel
            |> ExplorerState2.addToVisited cave
            |> ExplorerState2.addToCurrentPath cave

        // printfn "Exploring: %s" cave
        // printfn " > : %O" state

        if cave = stop then
            // printfn "Reached end of the path:"
            let nextState = state |> ExplorerState2.completePath
            nextState
        else

            let adjacent = 
                adjacentCaves caveMap cave
                |> List.filter (isNotVisited state)

            // printfn "Adjacent to %s and not visited %O" cave adjacent
            
            adjacent
            |> Seq.fold (fun innerState cave -> 
                let nextState = 
                    state 
                    |> ExplorerState2.setNext cave
                    |> ExplorerState2.pathsFromOther innerState
                // printfn "Going deeper with: %O" nextState
                loop nextState
            ) state

    loop initialState


let secondStar = 

    let isNotVisitedProblem1 (state: ExplorerState2) cave = 
        if cave = state.Start then
            false 
        else if isSmallCave cave then
            if state.Visited |> Map.containsKey cave then 
                false
            else 
                true
        else 
            true
    
    let isNotVisited (state: ExplorerState2) cave =
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
    let endState = explore2 caveMap isNotVisitedProblem1 "start" "end" 
    endState.Paths.Length

secondStar





