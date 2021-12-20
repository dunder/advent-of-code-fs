// --- Day 15: Chiton ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day15.txt") |> List.ofSeq

let example = [
    "1163751742"
    "1381373672"
    "2136511328"
    "3694931569"
    "7463417111"
    "1319128137"
    "1359912421"
    "3125421639"
    "1293138521"
    "2311944581"
]

let allElements (a:'a[,]) =
    seq { 
        for row in 0 .. a.GetLength(0)-1 do
          for column in 0 .. a.GetLength(1)-1 do 
            yield (row, column), a.[row,column] 
    }

let allValues (a:'a[,]) = a |>allElements |> Seq.map snd
let allPositions (a:'a[,]) = a |>allElements |> Seq.map fst

let parse (lines: string list) =
    let rows = lines |> Seq.length
    let columns = lines.[0] |> Seq.length

    let inline charToInt c = int c - int '0'

    Array2D.init rows columns (fun row column -> lines.[row].[column] |> charToInt)

let adjacent (a:'a[,]) p =
    let row, column = p
    let height = a.GetLength(0)
    let width = a.GetLength(1)
    
    let north = row, column - 1
    let east = row + 1, column
    let south = row, column + 1
    let west = row - 1, column

    let withinBounds width height (row, column) = row >= 0 && row < height && column >= 0 && column < width

    let within = withinBounds width height
    
    let directions = [north; east; south; west]
    
    seq {
        for x in 0..directions.Length-1 do
            let direction = directions.[x]
            if within direction then
                yield direction
    }


type SolverState = { Current: int*int; Vertices: Set<int*int>; Dist: Map<int*int, int>; Prev: Map<int*int, option<int*int>> }

module SolverState = 

    let init (matrix: int[,]) current =

            let vertices = matrix |> allPositions |> Set.ofSeq
            let dist = matrix |> allPositions |> Seq.map (fun pos -> pos, System.Int32.MaxValue) |> Map.ofSeq
            let dist = dist |> Map.add current 0
            let prev = matrix |> allPositions |> Seq.map (fun pos -> pos, None) |> Map.ofSeq

            { Current = current; Vertices = vertices; Dist = dist; Prev = prev }

let djikstra (matrix:int[,]) source target =

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

    let length (x,y) (x',y') = matrix.[x', y']

    let initialState = SolverState.init matrix source

    Seq.unfold (fun solverState -> 

        let nextPos = solverState.Vertices |> Seq.minBy (fun pos -> solverState.Dist.[pos])
        let nextVertices = solverState.Vertices |> Set.remove nextPos

        let neighbors = 
            adjacent matrix nextPos 
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
    let riskLevels = parse input
    let maxRow = riskLevels.GetLength(0)-1
    let maxColumn = riskLevels.GetLength(1)-1
    let goal = (maxRow, maxColumn)
    let state = djikstra riskLevels (0,0) goal
    
    state 
    |> unwind 
    |> Seq.choose id 
    |> Seq.skip 1
    |> Seq.sumBy (fun (row, column) -> riskLevels.[row, column])

firstStar

let extend  (riskLevels:int[,]) =
    let rows = riskLevels.GetLength(0)
    let columns = riskLevels.GetLength(1)

    let extended = Array2D.zeroCreate (rows*5) (columns*5)
    for row in 0..rows-1 do
        for column in 0..columns-1 do
            for i in 1..4 do
                let value = (riskLevels.[row, column] + i) % 10
                extended[row*i, column*i] <- (riskLevels.[row, column] + i) % 10

let secondStar = 
    let riskLevels = parse input



    let maxRow = riskLevels.GetLength(0)-1
    let maxColumn = riskLevels.GetLength(1)-1
    let goal = (maxRow, maxColumn)
    let state = djikstra riskLevels (0,0) goal
    
    state 
    |> unwind 
    |> Seq.choose id 
    |> Seq.skip 1
    |> Seq.sumBy (fun (row, column) -> riskLevels.[row, column])

secondStar

