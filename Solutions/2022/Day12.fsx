// --- Day 12: Hill Climbing Algorithm ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day12.txt") |> List.ofSeq

let example = 
    [
        "Sabqponm"
        "abcryxxl"
        "accszExk"
        "acctuvwj"
        "abdefghi"
    ]

let inline charToInt c = int c - int '0'

let parse (input: string list) =
    let data = 
        input
        |> Seq.map (fun line -> line |> Seq.toList)
        |> Seq.toList

    let rows = input.Length
    let columns = input.Head.Length

    Array2D.init rows columns (fun x y -> data[x][y])

example |> parse


let allElements (a: 'a[,]) =
    seq { 
        for row in 0 .. a.GetLength(0)-1 do
          for column in 0 .. a.GetLength(1)-1 do 
            yield (row, column), a[row,column] 
    }

let findPositionOf c (a: char[,]) =
    a |> allElements |> Seq.find (fun (_, value) -> value = c)

let findStart (a: char[,]) = findPositionOf 'S' a
let findEnd (a: char[,]) = findPositionOf 'E' a

let updateSourceAndTargetHeights (map: char[,]) =
    let startRow, startColumn = findStart map |> fst
    let endRow, endColumn = findEnd map |> fst
    map[startRow, startColumn] <- 'a'
    map[endRow, endColumn] <- 'z'
    map

example |> parse |> findStart
example |> parse |> findEnd

let within (row, column) (map: char[,]) =
    let width = map.GetLength(1)
    let height = map.GetLength(0)
    row >= 0 && row <= height-1 &&
    column >= 0 && column <= width-1

let neigbours (map: char[,]) (row, column) =
    let width = map.GetLength(1)
    let height = map.GetLength(0)
    seq {
        if row > 0 then yield row-1, column
        if column < width-1 then yield row, column+1
        if row < height-1 then yield row+1, column
        if column > 0 then yield row, column-1
    }

let noClimbingNeighbours (map: char[,]) (row, column) =
    let sourceHeight = map[row, column]

    let mustClimb (targetRow, targetColumn) = 
        let targetHeight = map[targetRow, targetColumn]
        (targetHeight |> int) - (sourceHeight |> int) > 1
        
    neigbours map (row, column)
    |> Seq.filter (mustClimb >> not)

type queue<'a> =
    | Queue of 'a list * 'a list

module Queue =
    let empty = Queue([], [])

    let enqueue q e = 
        match q with
        | Queue(fs, bs) -> Queue(e :: fs, bs)

    let dequeue q = 
        match q with
        | Queue([], []) -> failwith "Empty queue!"
        | Queue(fs, b :: bs) -> b, Queue(fs, bs)
        | Queue(fs, []) -> 
            let bs = List.rev fs
            bs.Head, Queue([], bs.Tail)


type Path<'node> = 'node seq

module Algorithm =

    type Config<'node when 'node : comparison> =
        {
            neighbours : 'node -> 'node seq
            isGoal : 'node -> bool
        }

    type private Runtime<'node when 'node : comparison> =
        {
            neighbours : 'node -> 'node seq
            isGoal : 'node -> bool
            openNodes : queue<'node>
            visitedNodes : Set<'node>
            cameFrom : Map<'node,'node>
        }

    let private initRuntime (start : 'node) (config : Config<'node>) =
        {
            neighbours = config.neighbours
            isGoal = config.isGoal
            openNodes =  Queue.enqueue Queue.empty start
            visitedNodes = Set.empty
            cameFrom = Map.empty
        }

    let rec private reconstructPath' (acc : 'node list) (toNode : 'node) (runtime : Runtime<'node>) =
        match runtime.cameFrom.TryFind toNode with
        | None -> toNode :: acc
        | Some parent -> reconstructPath' (toNode :: acc) parent runtime


    let private reconstructPath (toNode : 'node) (runtime : Runtime<'node>) =
        reconstructPath' [] toNode runtime |> Seq.ofList

    let private processChild (node : 'node) (runtime : Runtime<'node>) (child : 'node) =
        { runtime with
            openNodes = Queue.enqueue runtime.openNodes child
            cameFrom = runtime.cameFrom.Add (child, node)
        }

    let rec private runAlgorithm (runtime : Runtime<'node>) =
        let current, openNodes = Queue.dequeue runtime.openNodes
        if runtime.isGoal current then
            runtime |> reconstructPath current
        else
            let open' = openNodes
            let visited' = runtime.visitedNodes |> Set.add current
            let runtime' = { runtime with openNodes = open'; visitedNodes = visited' }
            let children =
                runtime.neighbours current
                |> Seq.filter (visited'.Contains >> not)
            let runtime'' =
                children
                |> Seq.fold (processChild current) runtime'
            runAlgorithm runtime''

    let bfs (start : 'node) (config : Config<'node>) =
        config
        |> initRuntime start
        |> runAlgorithm

let exampleMap = example |> parse |> updateSourceAndTargetHeights

noClimbingNeighbours exampleMap (0,0)


let exampleGoal = example |> parse |> findEnd |> fst

let isExampleGoal (row, column) = (row, column) = exampleGoal

let exampleBfsConfig: Algorithm.Config<int*int> = 
    {
        neighbours = noClimbingNeighbours exampleMap
        isGoal = isExampleGoal
    }

let exampleStart = example |> parse |> findStart |> fst
Algorithm.bfs exampleStart exampleBfsConfig |> Seq.length


let goal = input |> parse |> findEnd |> fst
let map = input |> parse |> updateSourceAndTargetHeights
let isGoal (row, column) = (row, column) = goal


let bfsConfig: Algorithm.Config<int*int> = 
    {
        neighbours = noClimbingNeighbours map
        isGoal = isGoal
    }

let start = input |> parse |> findStart |> fst
Algorithm.bfs start bfsConfig |> Seq.length



let firstStar =
    0

firstStar

let secondStar = 
    0

secondStar

