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

let findPositionOf (map: char[,]) c = map |> allElements |> Seq.find (fun (_, value) -> value = c)

let findStart (map: char[,]) = findPositionOf map 'S'
let findEnd (map: char[,]) = findPositionOf map 'E'

let updateSourceAndTargetHeights (map: char[,]) =
    let startRow, startColumn = findStart map |> fst
    let endRow, endColumn = findEnd map |> fst
    map[startRow, startColumn] <- 'a'
    map[endRow, endColumn] <- 'z'
    map

example |> parse |> findStart
example |> parse |> findEnd

let adjacent (map: char[,]) (node: int*int) =
    let row, column = node
    let width = map.GetLength(1)
    let height = map.GetLength(0)
    [
        if row > 0 then yield row-1, column
        if column < width-1 then yield row, column+1
        if row < height-1 then yield row+1, column
        if column > 0 then yield row, column-1
    ]

let neighbours (map: char[,]) (node: int*int) =
    let row, column = node
    let sourceHeight = map[row, column]

    let mustClimb (targetRow, targetColumn) = 
        let targetHeight = map[targetRow, targetColumn]
        (targetHeight |> int) - (sourceHeight |> int) > 1
        
    adjacent map node
    |> List.filter (mustClimb >> not)
    // |> List.filter(visited.Contains >> not)
    // |> Set


let exampleMap = example |> parse |> updateSourceAndTargetHeights

neighbours exampleMap (0,0)


let exampleGoal = example |> parse |> findEnd |> fst |> Set.singleton
let exampleStart = example |> parse |> findStart |> fst |> Set.singleton

let goal = input |> parse |> findEnd |> fst |> Set.singleton
let map = input |> parse |> updateSourceAndTargetHeights
let start = input |> parse |> findStart |> fst |> Set.singleton
    
type NodeSet<'node when 'node : comparison> = Set<'node>

let shortestPath neighbours startExploring destination = 

    let neighboursNotVisited (visited: NodeSet<'node>) nodeSet =
        neighbours nodeSet
        |> List.filter(visited.Contains >> not) 
        |> Set

    Seq.unfold(fun ((exploring, visited): NodeSet<'node>*NodeSet<'node>) -> 
            if visited.IsSupersetOf destination then 
                None 
            else
                let newExploring = exploring |> Seq.map (neighboursNotVisited visited) |> Set.unionMany
                let newVisited = visited + newExploring
                let newState = newExploring, newVisited
                Some(newState, newState)) (startExploring, startExploring)
    |> Seq.length

let exampleResult = shortestPath (neighbours exampleMap) exampleStart exampleGoal

let firstStar = shortestPath (neighbours map) start goal

firstStar

let exampleScenicStart = exampleMap |> allElements |> Seq.filter (fun ((row, column), c) -> c = 'a') |> Seq.map fst |> Set

let exampleScenicPath = shortestPath (neighbours exampleMap) exampleScenicStart exampleGoal


let secondStar = 
    let scenicStart = map |> allElements |> Seq.filter (fun (_, c) -> c = 'a' ) |> Seq.map fst |> Set
    shortestPath (neighbours map) scenicStart goal
    

secondStar

