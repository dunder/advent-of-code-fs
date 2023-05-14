module AoC.E2022.Day24

// --- Day 24: Blizzard Basin ---

open System.Collections.Generic

open AoC
open IO

let input = readInputLines "2022" "Day24" |> List.ofSeq


type Direction = | Up | Down | Left | Right

type Blizzard = { Position: int*int; Direction: Direction }

type ValleyMap = { Walls: list<int*int>; Blizzards: list<Blizzard>; Width: int; Height: int }

let parse (lines: string list) =
    lines
    |> List.mapi (fun i c -> i-1, c)
    |> List.fold (fun map (y, line) ->
        line
        |> Seq.mapi (fun i c -> i-1, c)
        |> Seq.fold (fun map (x, c) -> 
            let createBlizzard direction = { Position = x,y; Direction = direction }
            match c with
            | '#' -> { map with Walls = (x, y)::map.Walls }
            | '^' -> { map with Blizzards = (createBlizzard Up)::map.Blizzards }
            | 'v' -> { map with Blizzards = (createBlizzard Down)::map.Blizzards }
            | '<' -> { map with Blizzards = (createBlizzard Left)::map.Blizzards }
            | '>' -> { map with Blizzards = (createBlizzard Right)::map.Blizzards }
            | '.' -> map
            | _ -> failwithf "Unrecognized input %c" c
        ) map
    ) { Walls = List.empty; Blizzards = List.empty; Width = lines.Head.Length-2; Height = lines.Length-2 }

module Blizzard = 
    let move map time blizzard =
        let positive position steps length = (position + steps) % (length)

        let negative position steps length =
            let x = (length - position - 1)
            let x' = (x + steps) % length
            length - x' - 1

        let x,y = blizzard.Position

        let newPosition = 
            match blizzard.Direction with
            | Up ->
                let x', y' = x, negative y time map.Height
                (x', y')
            | Right ->
                let x', y' = positive x time map.Width, y
                (x', y')
            | Down ->
                let x', y' = x, positive y time map.Height
                (x', y')
            | Left ->
                let x', y' = negative x time map.Width, y
                (x', y')

        { blizzard with Position = newPosition }

    let position blizzard = blizzard.Position

module ValleyMap =

    let ofTime time map = 
        let nextBlizzards = 
            map.Blizzards 
            |> List.map (Blizzard.move map time)
        { map with Blizzards = nextBlizzards }

    let toOccupied map =
        let bs = map.Blizzards |> List.map Blizzard.position 
        let ws = map.Walls
        bs @ ws |> Set.ofList

    let ofAllTimes map =
        [0..map.Height*map.Width]
        |> List.fold (fun state i -> 
            state |> Map.add i (map |> ofTime i |> toOccupied)
        ) Map.empty

let memoize fn =
    let cache = new Dictionary<_,_>()
    (fun x ->
        match cache.TryGetValue x with
        | true, v -> v
        | false, _ -> 
            let v = fn (x)
            cache.Add(x,v)
            v)
let buildLookup (map: ValleyMap) lcd =

    let horizontal, vertical = map.Blizzards |> List.partition(fun b -> b.Direction = Left || b.Direction = Right)

    let movedHorizontal= memoize (fun t -> horizontal |> List.map (Blizzard.move map t) |> List.map Blizzard.position |> Set.ofList)
    let movedVertical = memoize (fun t -> vertical |> List.map(Blizzard.move map t) |> List.map Blizzard.position |> Set.ofList)
    let walls = map.Walls |> Set.ofList

    [0..map.Width*map.Height/lcd-1]
    |> List.fold (fun state time ->

        let horizontalTime = time % map.Width
        let verticalTime = time % map.Height
        let hs = movedHorizontal horizontalTime
        let vs = movedVertical verticalTime
        
        let occupied = walls |> Set.union hs |> Set.union vs

        state |> Map.add time occupied

    ) Map.empty

type SearchState = { Position: int*int; Time: int; Previous: option<SearchState> }

let neighbours (map:Map<int,Set<int*int>>) (x, y) time =   
    let occupied = map[(time+1)%map.Count]
    [
        x,y
        x+1, y
        x-1, y
        x, y+1
        x, y-1
    ]
    |> List.filter (fun (x,y) -> x > -2 && y > -2 && (occupied |> Set.contains (x,y) |> not))
    |> List.map (fun pos -> pos, time+1)

let nonVisitedNeighbours map (visited:HashSet<(int*int)*int>) state =
    neighbours map state.Position state.Time
    |> List.filter (fun (pos, time) -> visited.Add(pos, time))

let bfs target nonVisitedNeighbours startState =
    let visited = new HashSet<(int*int)*int>()
    let queue = new Queue<SearchState>()
    let rec bfsSearch() =
        if queue.Count = 0 then
            failwith "no solution found"
        else
            let head = queue.Dequeue()
            if target = head.Position then 
                head
            else                 
                for (position, time) in nonVisitedNeighbours visited head do
                    queue.Enqueue { Position = position; Time = time; Previous = Some(head) }
                bfsSearch()
    
    queue.Enqueue startState
    
    bfsSearch()


let firstStar () =
    let inputMap = input |> parse
    let inputLookup = buildLookup inputMap 5
    let goal = bfs (119,25) (nonVisitedNeighbours inputLookup) { Position = (0,-1); Time = 0; Previous = None }
    goal.Time

let secondStar () =
    let inputMap = input |> parse
    let inputLookup = buildLookup inputMap 5
    let there = bfs (119,25) (nonVisitedNeighbours inputLookup) { Position = (0,-1); Time = 0; Previous = None }
    let back = bfs (0,-1) (nonVisitedNeighbours inputLookup) { Position = (119,25); Time = there.Time; Previous = None }
    let thereAgain = bfs (119,25) (nonVisitedNeighbours inputLookup) { Position = (0,-1); Time = back.Time; Previous = None }
    thereAgain.Time