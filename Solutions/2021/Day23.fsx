// --- Day 23: Amphipod ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day23.txt") |> List.ofSeq

type AmphipodType = Amber | Bronze | Copper | Desert
type Amphipod = { Type: AmphipodType; Position: int*int }

// #############
// #...........#
// ###B#C#B#D###
//   #A#D#C#A#
//   #########

let podsExample = 
    [ 
        { Type = Amber; Position = (2,2)}
        { Type = Amber; Position = (8,2)}
        { Type = Bronze; Position = (2,1)}
        { Type = Bronze; Position = (6,1)}
        { Type = Copper; Position = (4,1)}
        { Type = Copper; Position = (6,2)}
        { Type = Desert; Position = (4,2)}
        { Type = Desert; Position = (8,1)}
    ]

// #############
// #...........#
// ###D#B#D#A###
//   #C#C#A#B#
//   #########

let pods = 
    [ 
        { Type = Amber; Position = (6,2)}
        { Type = Amber; Position = (8,1)}
        { Type = Bronze; Position = (4,1)}
        { Type = Bronze; Position = (8,2)}
        { Type = Copper; Position = (2,2)}
        { Type = Copper; Position = (4,2)}
        { Type = Desert; Position = (2,1)}
        { Type = Desert; Position = (6,1)}
    ]    

type SolverState = { Current: int*int; Vertices: Set<int*int>; Dist: Map<int*int, int>; Prev: Map<int*int, option<int*int>> }

let allPositions = [(0,0); (0,1); (0,2); (0,3); (0,4); (0,5); (0,6); (0,7); (0,8); (0,9); (0,10); (2,1); (2,2); (4,1); (4,2); (6,1); (6,2); (8,1); (8,2)] |> Seq.ofList

let adjacent node = []

module SolverState = 

    let init current =

            let vertices = allPositions |> Set.ofSeq
            let dist = allPositions |> Seq.map (fun pos -> pos, System.Int32.MaxValue) |> Map.ofSeq
            let dist = dist |> Map.add current 0
            let prev = allPositions |> Seq.map (fun pos -> pos, None) |> Map.ofSeq

            { Current = current; Vertices = vertices; Dist = dist; Prev = prev }

let djikstra source target =

//  1  function Dijkstra(Graph, source):
//  2
//  3      create vertex set Q
//  4
//  5      for each vertex v in Graph:            
//  6          dist[v] ← INFINITY                 
//  7          prev[v] ← UNDEFINED                
//  8          add v to Q                     
//  9      dist[source] ← 0                       
// 10     
// 11      while Q is not empty:
// 12          u ← vertex in Q with min dist[u]   
// 13                                             
// 14          remove u from Q
// 15         
// 16          for each neighbor v of u still in Q:
// 17              alt ← dist[u] + length(u, v)
// 18              if alt < dist[v]:              
// 19                  dist[v] ← alt
// 20                  prev[v] ← u
// 21
// 22      return dist[], prev[]

    let length (x,y) (x',y') = 0

    let initialState = SolverState.init source

    Seq.unfold (fun solverState -> 

        let nextPos = solverState.Vertices |> Seq.minBy (fun pos -> solverState.Dist.[pos])
        let nextVertices = solverState.Vertices |> Set.remove nextPos

        let neighbors = 
            adjacent nextPos 
            |> Seq.filter (fun pos -> nextVertices |> Set.contains pos)

        let nextDist, nextPrev =
            neighbors
            |> Seq.fold (fun (state:Map<int*int,int>*Map<int*int, option<int*int>>) pos -> 
                let dist, prev = state
                let alt = dist.[nextPos] + (length pos nextPos)
                if alt < dist.[pos] then
                    let u = Some nextPos
                    dist |> Map.add pos alt, prev |> Map.add pos u
                else
                    dist, prev
            ) (solverState.Dist, solverState.Prev)

        let nextSolverState = { Current = nextPos; Vertices = nextVertices; Dist = nextDist; Prev = nextPrev }
        Some (nextSolverState, nextSolverState)
    ) initialState
    |> Seq.find (fun solverState -> solverState.Vertices |> Set.isEmpty || solverState.Current = target)

let unwind solverState =

// 1  S ← empty sequence
// 2  u ← target
// 3  if prev[u] is defined or u = source:          // Do something only if the vertex is reachable
// 4      while u is defined:                       // Construct the shortest path with a stack S
// 5          insert u at the beginning of S        // Push the vertex onto the stack
// 6          u ← prev[u]                           // Traverse from target to source

    let initialPath = []
    let initialNode = Some solverState.Current    

    Seq.unfold (fun (path, (node:option<int*int>)) ->            
        let nextState =
            match node with
            | Some n -> node::path, solverState.Prev.[n]
            | None -> path, None

        Some(nextState, nextState)
    ) (initialPath, initialNode)
    |> Seq.find (fun (_, node) -> node = None)
    |> fst


let firstStar =
    0

firstStar

let secondStar = 
    0

secondStar

