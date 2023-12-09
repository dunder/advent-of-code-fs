// --- Day 23: Amphipod ---

#r """C:\Users\matjan\.nuget\packages\optimizedpriorityqueue\5.1.0\lib\netstandard1.0\PriorityQueue.dll"""

open System.IO
open System.Linq
open Priority_Queue

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day23.txt") |> List.ofSeq

type AmphipodType = Amber | Bronze | Copper | Desert
type RoomPosition = Inner | Outer

type Position = 
| Room of AmphipodType*RoomPosition
| Hallway of int

type Amphipod = { Type: AmphipodType; Position: Position }

module Amphipod = 

    let atHome pod = 
        match pod.Type, pod.Position with
        | podType, Room (roomType, _) when podType = roomType -> true
        | _ -> false

// #############
// #...........#
// ###B#C#B#D###
//   #A#D#C#A#
//   #########

let podsExample = 
    [ 
        { Type = Amber; Position = Room (Amber, Inner)}
        { Type = Amber; Position = Room (Desert, Inner)}
        { Type = Bronze; Position = Room (Amber, Outer)}
        { Type = Bronze; Position = Room (Copper, Outer)}
        { Type = Copper; Position = Room (Bronze, Outer)}
        { Type = Copper; Position = Room (Copper, Inner)}
        { Type = Desert; Position = Room (Bronze, Inner)}
        { Type = Desert; Position = Room (Desert, Outer)}
    ]

// #############
// #...........#
// ###D#B#D#A###
//   #C#C#A#B#
//   #########

let pods = 
    [ 
        { Type = Amber; Position = Room (Copper, Inner)}
        { Type = Amber; Position = Room (Desert, Outer)}
        { Type = Bronze; Position = Room (Bronze, Outer)}
        { Type = Bronze; Position = Room (Desert, Inner)}
        { Type = Copper; Position = Room (Amber, Inner)}
        { Type = Copper; Position = Room (Bronze, Inner)}
        { Type = Desert; Position = Room (Amber, Outer)}
        { Type = Desert; Position = Room (Copper, Outer)}
    ]    

let print pods =

    let positionMap = 
        [
            Hallway 1, (1,1)
            Hallway 2, (2,1)
            Hallway 4, (4,1)
            Hallway 6, (6,1)
            Hallway 8, (8,1)
            Hallway 10, (10,1)
            Hallway 11, (11,1)
            Room (Amber, Outer), (3,2)
            Room (Amber, Inner), (3,3)
            Room (Bronze, Outer), (5,2)
            Room (Bronze, Inner), (5,3)
            Room (Copper, Outer), (7,2)
            Room (Copper, Inner), (7,3)
            Room (Desert, Outer), (9,2)
            Room (Desert, Inner), (9,3)
        ]
        |> Map.ofList

    let amphipodTypeChar pod =
        match pod.Type with
        | Amber -> "A"
        | Bronze -> "B"
        | Copper -> "C"
        | Desert -> "D"

    let podPositions = pods |> List.map (fun pod -> positionMap.[pod.Position], amphipodTypeChar pod) |> Map.ofList
            
    // #############
    // #...........#
    // ###D#B#D#A###
    //   #C#C#A#B#
    //   #########

    for j in 0..4 do
        for i in 0..12 do
            match podPositions |> Map.tryFind (i,j) with
            | Some c -> printf "%s" c
            | None ->
                match j,i with  
                | 0,_ -> printf "#"
                | 1,0 | 1, 12 -> printf "#"
                | 1,_ -> printf "."
                | j,i when (j = 2 || j = 3) && (i = 3 || i = 5 || i = 7 || i = 9) -> printf "."
                | j,i when j = 3 && (i < 2 || i > 10) -> printf " "
                | j,_ when (j = 2 || j = 3) -> printf "#"
                | 4, i when (i > 1 && i < 11) -> printf "#"
                | 4, _ -> printf " "
                | _ -> printf "X"
        printfn ""

print podsExample
// type SolverState<'vertice when 'vertice: comparison> = { CurrentVertice: 'vertice; Vertices: Set<'vertice>; Dist: Map<'vertice, int>; Prev: Map<'vertice, option<'vertice>> }

// module SolverState =

//     let nextVertice solverState = solverState.Vertices |> Seq.minBy (fun vertice -> solverState.Dist.[vertice])
//     let nextVertices nextVertice solverState = solverState.Vertices |> Set.remove nextVertice

// type SolverConfig<'vertice when 'vertice: comparison> = 
//     {
//         init: 'vertice -> SolverState<'vertice>
//         nextVertice: SolverState<'vertice> -> 'vertice
//         nextVertices: 'vertice -> SolverState<'vertice> -> Set<'vertice>
//         neighbors: 'vertice -> SolverState<'vertice> -> list<'vertice*int>
//         isTarget: SolverState<'vertice> -> bool
//     }


type UcsSolverState<'node when 'node: comparison> = { Frontier: SimplePriorityQueue<'node, int>; Explored: Map<'node, int>; Prev: Map<'node, option<'node>> }

module UcsSolverState =

    let init startNode = 
        let priorityQueue = new SimplePriorityQueue<'node, int>()
        priorityQueue.Enqueue(startNode, 0)
        { Frontier = priorityQueue; Explored = Map.empty; Prev = Map.empty }

    let isGoal state = 
        if state.Frontier.Count = 0 then
            false
        else
            state.Frontier.First |> List.forall Amphipod.atHome

type UcsSolverConfig<'node when 'node: comparison> = 
    {
        init: 'node -> UcsSolverState<'node>
        // neighbors: 'node -> UcsSolverState<'node> -> list<'node*int>
        neighbors: 'node -> list<'node*int>
        isGoal: UcsSolverState<'node> -> bool
    }

module AmphipodSolver = 

    let memoize fn =
        let cache = new System.Collections.Generic.Dictionary<_,_>()
        (fun x ->
            match cache.TryGetValue x with
            | true, v -> v
            | false, _ -> 
                let v = fn (x)
                cache.Add(x,v)
                v)

    let hallwaySteps = [ 1..11 ]
    let hallwayStops = [ 1; 2; 4; 6; 8; 10; 11 ]
    let hallwayExits = hallwaySteps |> Set.ofList |> Set.difference (hallwayStops |> Set.ofList) |> Set.map Hallway
    let hallway = hallwaySteps |> List.map Hallway
    let potentialHallwayTargets = hallwayStops |> List.map Hallway

    let calculatePotentialTargets pod =
        match pod.Type, pod.Position with
        | _, Room _ -> potentialHallwayTargets |> Set.ofList
        | podType, Hallway _ -> [Room (podType, Inner); Room (podType, Outer)] |> Set.ofList

    let potentialTargets = memoize calculatePotentialTargets

    let hallwayExit roomType = 
        match roomType with
        | Amber -> 3
        | Bronze -> 5
        | Copper -> 7
        | Desert -> 9

    let stepsLeftFrom = memoize (fun startPosition -> hallwaySteps.[..startPosition-1] |> List.rev |> List.map Hallway)
    let stepsRightFrom = memoize (fun startPosition -> hallwaySteps.[startPosition-1..] |> List.map Hallway)

    let stepsToHallway steps = steps |> List.map Hallway

    let stepsLeft = memoize (fun fromStep entranceStep -> hallway.[entranceStep-1..fromStep-1] |> List.rev)
    let stepsRight = memoize (fun fromStep entranceStep -> hallway.[fromStep-1..entranceStep-1])

    let calculatePaths amphoid =
        let pathFromOuterToEndOfHallwayFrom roomType = let exit = hallwayExit roomType in [stepsLeftFrom exit; stepsRightFrom exit]
        let pathFromInnerToEndOfHallwayFrom roomType = pathFromOuterToEndOfHallwayFrom roomType |> List.map (fun path -> Room(roomType, Outer)::path)

        match amphoid.Position with
        | Room (roomType, Outer) -> pathFromOuterToEndOfHallwayFrom roomType
        | Room (roomType, Inner) -> pathFromInnerToEndOfHallwayFrom roomType
        | Hallway startStep -> 
            let entranceStep = hallwayExit amphoid.Type
            let roomPath =  [Room (amphoid.Type, Outer); Room (amphoid.Type, Inner)]
            let leftPath = if startStep > entranceStep then (stepsLeft startStep entranceStep) @ roomPath else []
            let rightPath = if startStep < entranceStep then (stepsRight startStep entranceStep) @ roomPath else []
            [leftPath; rightPath] |> List.filter (fun path -> not (path |> List.isEmpty ))

    let paths = memoize calculatePaths

    let unblockedPath pods path = 
        let taken = pods |> List.map (fun p -> p.Position) |> Set.ofList
        path 
        |> List.takeWhile (fun position -> taken |> Set.contains position |> not)

    let isAtHome pods pod =
        match pod.Position with
        | Room (roomType, Inner) -> 
            roomType = pod.Type
        | Room (roomType, Outer) when roomType = pod.Type -> 
            pods 
            |> List.exists (fun p -> 
                match p.Type, p.Position with 
                | otherType, Room(otherRoomType, Inner) when otherType = pod.Type && otherRoomType = pod.Type -> true
                | _ -> false)
        | _ -> 
            false

    let isExit position = hallwayExits |> Set.contains position

    let pathTrimExit path =
        if path |> List.last |> isExit then
            path |> List.rev |> List.tail |> List.rev
        else
            path

    let energy pod distance = 
        match pod.Type with
        | Amber -> distance * 1
        | Bronze -> distance * 10
        | Copper -> distance * 100
        | Desert -> distance * 1000

    let validMoves pods pod = 
        let podPaths = paths pod
        let unblockedPaths = podPaths |> List.map (unblockedPath pods)
        let other = pods |> List.except [pod]
        
        unblockedPaths
        |> List.filter (List.isEmpty >> not)
        |> List.map (fun path -> 
            let targets = potentialTargets pod
            path 
            |> List.mapi (fun i position ->
                if targets |> Set.contains position then
                    Some ({ Type = pod.Type; Position = position}, energy pod i) 
                else
                    None
            )
            |> List.choose id
        )
        |> List.collect id
        |> List.map (fun (newPod, dist) -> newPod::other, dist)

    let neighbors pods = 
        pods        
        |> List.fold (fun state pod ->     
            if isAtHome pods pod then
                state
            else
                let moves = validMoves pods pod

                if moves |> List.isEmpty then
                    state
                else
                    moves::state
        ) []
        |> List.collect id

AmphipodSolver.calculatePotentialTargets { Type = Amber; Position = Hallway 1 }
AmphipodSolver.potentialTargets { Type = Desert; Position = Hallway 1 }


AmphipodSolver.paths { Type = Bronze; Position = Hallway (6) }
AmphipodSolver.paths podsExample.[0]

AmphipodSolver.paths podsExample.[2] |> List.map (AmphipodSolver.unblockedPath pods)
print podsExample

AmphipodSolver.validMoves podsExample podsExample.[2]

AmphipodSolver.validMoves podsExample podsExample.[2] |> List.map fst |> List.iter print

let anotherExample = 
    [ 
        { Type = Amber; Position = Room (Amber, Inner)}
        { Type = Amber; Position = Room (Desert, Inner)}
        { Type = Bronze; Position = Hallway 11}
        { Type = Bronze; Position = Room (Copper, Outer)}
        { Type = Copper; Position = Hallway 1}
        { Type = Copper; Position = Room (Copper, Inner)}
        { Type = Desert; Position = Hallway 2}
        { Type = Desert; Position = Room (Desert, Outer)}
    ]


AmphipodSolver.paths anotherExample.[2]

// Den här borde ge en giltig väg hem men är inte det, undersök vidare
AmphipodSolver.validMoves anotherExample anotherExample.[2] |> List.map fst |> List.iter print

AmphipodSolver.neighbors podsExample |> List.map fst |> List.iter print

let state = { Type = Amber; Position = Hallway 4}::podsExample.Tail

AmphipodSolver.unblockedPath state (AmphipodSolver.paths podsExample.[2]).[1]


// procedure uniform_cost_search(start) is
//     node ← start
//     frontier ← priority queue containing node only
//     explored ← empty set
//     do
//         if frontier is empty then
//             return failure
//         node ← frontier.pop()
//         if node is a goal state then
//             return solution(node)
//         explored.add(node)
//         for each of node's neighbors n do
//             if n is not in explored and not in frontier then
//                 frontier.add(n)
//             else if n is in frontier with higher cost
//                 replace existing node with n

let uniformCostSearch<'node when 'node: comparison> config startNode =
    
    let initialState = config.init startNode

    Seq.unfold (fun solverState -> 

        if not (solverState.Frontier.Any()) then 
            failwithf "No solution found - Frontier is empty"

        let node = solverState.Frontier.Dequeue()
        // TODO: why did I set priority to 1 here? OK explored borde vara ett set, prio har ingen funktion
        let priority = 1
        let newExplored = solverState.Explored |> Map.add node priority

        // TODO: gå igenom neighbors?

        let neighbors = config.neighbors node

        // Kolla upp SimplePriorityQueue är den mutable?

        neighbors
        |> List.iter( fun (pods, priority) -> 
            if true then
                solverState.Frontier.Enqueue(pods, priority) |> ignore
            else 
                solverState.Frontier.UpdatePriority(pods, priority) |> ignore
        )

        let newState = { solverState with Explored = newExplored }

        Some(newState, newState)
    ) initialState
    |> Seq.find (fun solverState -> solverState |> config.isGoal)

let firstStar =

    let config = 
        {
            init = UcsSolverState.init
            neighbors = AmphipodSolver.neighbors
            isGoal = UcsSolverState.isGoal
        }

    uniformCostSearch config podsExample

let secondStar = 
    0

secondStar