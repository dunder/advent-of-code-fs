module AoC.E2021.Day15

// --- Day 15: Chiton ---

open AoC
open IO


let input = readInputLines "2021" "Day15" |> List.ofSeq


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


let firstStar () =
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


let extend  (riskLevels:int[,]) =
    let rows = riskLevels.GetLength(0)
    let columns = riskLevels.GetLength(1)

    let extended = Array2D.zeroCreate (rows*5) (columns*5)

    for row in 0..rows-1 do
        for column in 0..columns-1 do
            for r in 0..4 do
                for c in 0..4 do
                    let value = riskLevels.[row, column] + r + c
                    let value = if value > 9 then value % 9 else value
                    let eRow = row + r*rows
                    let eColumn = column + c*columns

                    extended.[eRow, eColumn] <- value

    extended

let print (image:int[,]) =
    let rows = image.GetLength(0)
    let columns = image.GetLength(1)

    printfn ""
    for row in 0..rows-1 do
        for column in 0..columns-1 do
            printf "%i" image.[row,column]
        printfn ""
    printfn ""

module AStar =
    type Config<'a> = 
        {
            neighbours: 'a -> seq<'a>
            gCost: 'a -> 'a -> int
            fCost: 'a -> 'a -> int
            maxIterations: int option
        }

    let search<'a when 'a : comparison> start goal config : seq<'a> option =

        let rec reconstructPath cameFrom current =
            seq {
                yield current
                match Map.tryFind current cameFrom with
                | None -> ()
                | Some next -> yield! reconstructPath cameFrom next
            }

        let rec crawler closedSet (openSet, gScores, fScores, cameFrom) =
            match config.maxIterations with 
            | Some n when n = Set.count closedSet -> None
            | _ ->
                match List.sortBy (fun n -> Map.find n fScores) openSet with
                | current::_ when current = goal -> Some <| reconstructPath cameFrom current 
                | current::rest ->
                    let gScore = Map.find current gScores
                    let next =
                        config.neighbours current 
                        |> Seq.filter (fun n -> closedSet |> Set.contains n |> not)
                        |> Seq.fold (fun (openSet, gScores, fScores, cameFrom) neighbour ->
                            let tentativeGScore = gScore + config.gCost current neighbour
                            if List.contains neighbour openSet && tentativeGScore >= Map.find neighbour gScores 
                            then (openSet, gScores, fScores, cameFrom)
                            else
                                let newOpenSet = if List.contains neighbour openSet then openSet else neighbour::openSet
                                let newGScores = Map.add neighbour tentativeGScore gScores
                                let newFScores = Map.add neighbour (tentativeGScore + config.fCost neighbour goal) fScores
                                let newCameFrom = Map.add neighbour current cameFrom
                                newOpenSet, newGScores, newFScores, newCameFrom
                            ) (rest, gScores, fScores, cameFrom)
                    crawler (Set.add current closedSet) next
                | _ -> None

        let gScores = Map.ofList [start, 0]
        let fScores = Map.ofList [start, config.fCost start goal]
        crawler Set.empty ([start], gScores, fScores, Map.empty)

let secondStar () = 
    let riskLevels = parse input
    let riskLevels = extend riskLevels

    let maxRow = riskLevels.GetLength(0)-1
    let maxColumn = riskLevels.GetLength(1)-1
    let goal = (maxRow, maxColumn)

    let manhattanDistance (x, y) (x', y') = abs (x' - x) + abs (y' - y)

    let riskDistance pos1 pos2 = 
        let row, column = pos2
        riskLevels.[row, column]

    let hasReachedTarget pos = pos = goal

    let config:AStar.Config<int*int> = { neighbours = adjacent riskLevels; gCost = riskDistance; fCost = manhattanDistance; maxIterations = None}

    match AStar.search (0,0) goal config with
    | Some path -> 
        path 
        |> Seq.rev
        |> Seq.skip 1
        |> Seq.sumBy (fun (row, column) -> riskLevels.[row, column])        
    | None -> failwith "Found no shortest path"