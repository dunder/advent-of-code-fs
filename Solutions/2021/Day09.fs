module AoC.E2021.Day09

// --- Day 9: Smoke Basin ---

open AoC
open IO


let input = readInputLines "2021" "Day09" |> List.ofSeq


let inline charToInt c = int c - int '0'

let parse (input: string list) =
    let data = 
        input
        |> Seq.map (fun line -> line |> Seq.map charToInt |> Seq.toList)
        |> Seq.toList

    let rows = input.Length
    let columns = input.Head.Length

    Array2D.init rows columns (fun x y -> data.[x].[y])

let allElements (a:'a[,]) =
    seq { 
        for row in 0 .. a.GetLength(0)-1 do
          for column in 0 .. a.GetLength(1)-1 do 
            yield (row, column), a.[row,column] 
    }

let withinBounds width height (row, column) = row >= 0 && row < height && column >= 0 && column < width

let adjacent (a:'a[,]) p =
    let row, column = p
    let height = a.GetLength(0)
    let width = a.GetLength(1)
    let up = row, column - 1
    let right = row + 1, column
    let down = row, column + 1
    let left = row - 1, column
    let within = withinBounds width height

    seq {
        if within up then
            yield up
        if within right then
            yield right
        if within down then 
            yield down
        if within left then 
            yield left
    }

let adjacentValues (a:'a[,]) p =
    adjacent a p
    |> Seq.map (fun (x,y) -> a.[x,y])

let isLowPoint heightmap element =
    let p, height = element
    let surroundingHeights = adjacentValues heightmap p |> Seq.toList
    surroundingHeights |> Seq.forall (fun x -> height < x)    

let risk height = height + 1

let firstStar () =
    let heightmap = parse input
    heightmap
    |> allElements
    |> Seq.filter (isLowPoint heightmap)
    |> Seq.map (snd >> risk)
    |> Seq.sum

type BasinSearchState = { Terminate: bool; SearchQueue: list<int*int>; Basin: Set<int*int> }

let nextBasinSearchState (heightmap:int[,]) state =   
    if state.SearchQueue.IsEmpty then 
        if state.Terminate then
            None
        else
            Some (state, { state with Terminate = true })
    else 
        let nextPoint = state.SearchQueue.Head
        let nextBasin = state.Basin |> Set.add nextPoint

        let notAlreadyFound point = not (nextBasin |> Seq.contains point)
        let notHighPoint point = heightmap.[fst point, snd point] <> 9
        
        let neighbors = 
            adjacent heightmap nextPoint
            |> Seq.filter notAlreadyFound
            |> Seq.filter notHighPoint
            |> Seq.toList

        let nextSearchQueue = state.SearchQueue.Tail |> List.append neighbors
        let nextState = { state with SearchQueue = nextSearchQueue; Basin = nextBasin }

        Some (state, nextState)

let basinFrom heightmap lowPoint =
    let startState = { Terminate = false; SearchQueue = [lowPoint]; Basin = Set.empty }
    Seq.unfold (nextBasinSearchState heightmap) startState
    |> Seq.last

let secondStar () = 
    
    let heightmap = parse input
    heightmap
    |> allElements
    |> Seq.filter (isLowPoint heightmap)
    |> Seq.map (fst >> basinFrom heightmap)
    |> Seq.sortByDescending (fun state -> state.Basin.Count)
    |> Seq.take 3
    |> Seq.map (fun state -> state.Basin.Count)
    |> Seq.reduce (*)