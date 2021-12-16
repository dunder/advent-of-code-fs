// >>> insert day tagline here <<<

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

type Queue<'a> =
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

let allElements (a:'a[,]) =
    seq { 
        for row in 0 .. a.GetLength(0)-1 do
          for column in 0 .. a.GetLength(1)-1 do 
            yield (row, column), a.[row,column] 
    }

let allValues (a:'a[,]) = a |>allElements |> Seq.map snd

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


type Score = int

// poor-mans priority queue
// via Sets
type Priority<'node when 'node : comparison> =
    { 
        nMap : Map<Score, Set<'node>>
        pMap : Map<'node, Score>
    }

module Priority =

    let empty()  : Priority<'node> =
        {
            nMap = Map.empty
            pMap = Map.empty
        }

    let mininmum (pq : Priority<'node>) =
        pq.nMap
        |> Map.pick (fun _ ns -> Some (Seq.head ns))

    let insert (n : 'node) (p : Score) (pq : Priority<'node>) =
        let nMap' =
            let lp = defaultArg (Map.tryFind p pq.nMap) Set.empty
            let lp' = Set.add n lp
            Map.add p lp' pq.nMap
        let pMap' =
            Map.add n p pq.pMap
        { nMap = nMap'; pMap = pMap' }

    let remove (n : 'node) (pq : Priority<'node>) =
        match pq.pMap.TryFind n with
        | None -> pq
        | Some p ->
            let nMap' =
                let lp = defaultArg (Map.tryFind p pq.nMap) Set.empty
                let lp' = Set.remove n lp
                if Set.isEmpty lp' then
                    Map.remove p pq.nMap
                else
                    Map.add p lp' pq.nMap
            let pMap' = Map.remove n pq.pMap
            { nMap = nMap'; pMap = pMap' }

type Path<'node> = 'node seq

module Algorithm =

    type Config<'node when 'node : comparison> =
        {
            heuristic : 'node -> Score
            neighbours : 'node -> 'node seq
            distance : 'node -> 'node -> Score
            isGoal : 'node -> bool
        }

    type private Runtime<'node when 'node : comparison> =
        {
            heuristic : 'node -> Score
            neighbours : 'node -> 'node seq
            isGoal : 'node -> bool
            distance : 'node -> 'node -> Score
            visitedNodes : Set<'node>
            openNodes : Priority<'node>
            gScores : Map<'node,Score>
            fScores : Map<'node,Score>
            cameFrom : Map<'node,'node>
        }
        member this.GScore node =
            defaultArg (this.gScores.TryFind node) System.Int32.MaxValue
        member this.FScore node =
            defaultArg (this.gScores.TryFind node) System.Int32.MaxValue


    let private initRuntime (start : 'node) (config : Config<'node>) =
        {
            heuristic = config.heuristic
            neighbours = config.neighbours
            isGoal = config.isGoal
            distance = config.distance
            visitedNodes = Set.empty
            openNodes = Priority.empty() |> Priority.insert start 0
            gScores = Map.empty |> Map.add start 0
            fScores = Map.empty |> Map.add start (config.heuristic start)
            cameFrom = Map.empty
        }


    let rec private reconstructPath' (acc : 'node list) (toNode : 'node) (runtime : Runtime<'node>) =
        match runtime.cameFrom.TryFind toNode with
        | None -> toNode :: acc
        | Some parent -> reconstructPath' (toNode :: acc) parent runtime


    let private reconstructPath (toNode : 'node) (runtime : Runtime<'node>) =
        reconstructPath' [] toNode runtime |> Seq.ofList


    let private processChild (node : 'node) (runtime : Runtime<'node>) (child : 'node)=
        let tentativeGScore = runtime.GScore node + runtime.distance node child
        let fScoreChild = tentativeGScore + runtime.heuristic child
        let open' = runtime.openNodes |> Priority.insert child fScoreChild
        let gScoreChild = runtime.GScore child
        if tentativeGScore >= gScoreChild then
            { runtime with openNodes = open' }
        else
            { runtime with
                openNodes = open'
                cameFrom = runtime.cameFrom.Add (child, node)
                gScores = runtime.gScores.Add (child, tentativeGScore)
                fScores = runtime.fScores.Add (child, fScoreChild)
            }


    let rec private runAlgorithm (runtime : Runtime<'node>) =
        let current = runtime.openNodes |> Priority.mininmum
        if runtime.isGoal current then
            runtime |> reconstructPath current
        else
            let open' = runtime.openNodes |> Priority.remove current
            let visited' = runtime.visitedNodes |> Set.add current
            let runtime' = { runtime with openNodes = open'; visitedNodes = visited' }
            let children =
                runtime.neighbours current
                |> Seq.filter (visited'.Contains >> not)
            let runtime'' =
                children
                |> Seq.fold (processChild current) runtime'
            runAlgorithm runtime''


    let aStar (start : 'node) (config : Config<'node>) =
        config
        |> initRuntime start
        |> runAlgorithm

let firstStar =
    let riskLevels = parse example
    
    let dist (x,y) (x',y') = abs (x'-x) + abs (y'-y)


    let maxRow = riskLevels.GetLength(0)-1
    let maxColumn = riskLevels.GetLength(1)-1
    let goal = (maxRow, maxColumn)

    let config : Algorithm.Config<_> =
        {
            heuristic = fun pos -> dist pos goal
            neighbours = adjacent riskLevels
            distance = fun _ _ -> 1
            isGoal = fun pos -> pos = goal
        }
    in config |> Algorithm.aStar (0,0)

firstStar

let secondStar = 
    0

secondStar

