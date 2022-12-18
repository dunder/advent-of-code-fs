module AoC.E2022.Day12

// --- Day 12: Hill Climbing Algorithm ---

open AoC
open IO


let input = readInputLines "2022" "Day12" |> List.ofSeq


let parse (input: string list) =
    let data = 
        input
        |> Seq.map (fun line -> line |> Seq.toList)
        |> Seq.toList

    let rows = input.Length
    let columns = input.Head.Length

    Array2D.init rows columns (fun x y -> data[x][y])

let allElements (a: 'a[,]) =
    seq { 
        for row in 0 .. a.GetLength(0)-1 do
          for column in 0 .. a.GetLength(1)-1 do 
            yield (row, column), a[row,column] 
    }

let findPositionsOf (map: char[,]) c = map |> allElements |> Seq.filter (fun (_, value) -> value = c)

let findStart (map: char[,]) = findPositionsOf map 'S' |> Seq.exactlyOne |> fst
let findEnd (map: char[,]) = findPositionsOf map 'E' |> Seq.exactlyOne |> fst

let updateSourceAndTargetHeights (map: char[,]) =
    let startRow, startColumn = findStart map
    let endRow, endColumn = findEnd map
    map[startRow, startColumn] <- 'a'
    map[endRow, endColumn] <- 'z'
    map

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

let goal = input |> parse |> findEnd |> Set.singleton
let map = input |> parse |> updateSourceAndTargetHeights
let start = input |> parse |> findStart |> Set.singleton
    
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


let firstStar () = shortestPath (neighbours map) start goal

let secondStar () =
    let scenicStart = 
        findPositionsOf map 'a'
        |> Seq.map fst 
        |> Set
    shortestPath (neighbours map) scenicStart goal