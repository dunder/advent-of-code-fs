// --- Day 24: Blizzard Basin ---

open System.Collections.Generic
open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day24.txt") |> List.ofSeq

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

let example = [
    "#.######"
    "#>>.<^<#"
    "#.<..<<#"
    "#>v.><>#"
    "#<^v^^>#"
    "######.#"
]

let exampleMap = example |> parse

let memoize fn =
    let cache = new Dictionary<_,_>()
    (fun x ->
        match cache.TryGetValue x with
        | true, v -> v
        | false, _ -> 
            let v = fn (x)
            cache.Add(x,v)
            v)

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

    let blockedAt map = memoize (fun time ->
        let horizontal, vertical = map.Blizzards |> List.partition(fun b -> b.Direction = Left || b.Direction = Right)

        let horizontalTime = time % map.Width
        let movedHorizontal= memoize (fun t -> horizontal |> List.map (Blizzard.move map t) |> List.map Blizzard.position)

        
        let verticalTime = time % map.Height
        let movedVertical = memoize (fun t -> vertical |> List.map(Blizzard.move map t) |> List.map Blizzard.position)

        movedHorizontal horizontalTime @ movedVertical verticalTime @ map.Walls |> Set.ofList
    )

    let toOccupied map =
        let bs = map.Blizzards |> List.map Blizzard.position 
        let ws = map.Walls
        bs @ ws |> Set.ofList

    let ofAllTimes map =
        [0..map.Height*map.Width]
        |> List.fold (fun state i -> 
            state |> Map.add i (map |> ofTime i |> toOccupied)
        ) Map.empty

    let print map =
        let walls = map.Walls |> Set.ofList

        let blizzardMap =
            map.Blizzards
            |> List.fold (fun (state: Map<(int*int), string>) blizzard -> 
                let position = blizzard.Position
                let direction = blizzard.Direction
                if state |> Map.containsKey position then
                    state |> Map.add position "+"
                else 
                    match direction with
                    | Up -> state |> Map.add position "^"
                    | Right -> state |> Map.add position ">"
                    | Down -> state |> Map.add position "v"
                    | Left -> state |> Map.add position "<"

            ) Map.empty

        let maxx = map.Walls |> List.map fst |> List.max
        let maxy = map.Walls|> List.map snd |> List.max

        for y in -1..maxy do
            for x in -1..maxx do
                if walls |> Set.contains (x,y) then
                    printf "#"
                else if blizzardMap |> Map.containsKey (x,y) then
                    printf "%s" blizzardMap[x,y]
                else
                    printf "."
            printfn ""

type ValleyMapPrediction = { Blizzards: list<Blizzard> }

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
    // printfn "time: %i, values for %i" time (map |> Map.keys |> Seq.max)
    // let occupied = map[time%map.Count]
    // let occupied = ValleyMap.blockedAt map time
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


 
exampleMap |> ValleyMap.ofTime 1 |> ValleyMap.toOccupied |> Set.toList
exampleMap |> ValleyMap.print

let exampleLookup = buildLookup exampleMap 1

exampleLookup[0]

let exampleMapOfAllTimes = exampleMap |> ValleyMap.ofAllTimes


neighbours exampleLookup (0,-1) 0
neighbours exampleLookup (0,-1) 1
neighbours exampleLookup (0,0) 2
neighbours exampleLookup (0,1) 3
neighbours exampleLookup (0,1) 4
neighbours exampleLookup (0,0) 5
neighbours exampleLookup (1,0) 6
neighbours exampleLookup (2,0) 7
neighbours exampleLookup (2,1) 8
neighbours exampleLookup (1,1) 9
neighbours exampleLookup (1,0) 10
neighbours exampleLookup (2,0) 11
neighbours exampleLookup (2,0) 12
neighbours exampleLookup (2,1) 13
neighbours exampleLookup (2,2) 14
neighbours exampleLookup (3,2) 15
neighbours exampleLookup (4,2) 16
neighbours exampleLookup (5,2) 17
neighbours exampleLookup (5,3) 18
neighbours exampleLookup (5,4) 19


bfs (5,4) (nonVisitedNeighbours (buildLookup exampleMap 1)) { Position = (0,-1); Time = 0; Previous = None }

let inputMap = input |> parse

inputMap |> ValleyMap.ofTime (120*25) |> ValleyMap.print





let inputMapOfAllTimes = inputMap |> ValleyMap.ofAllTimes

let inputLookup = buildLookup inputMap 5
inputLookup.Count


[0..700] |> List.forall (fun t -> 
    let setA = inputMap |> ValleyMap.ofTime t |> ValleyMap.toOccupied
    let setB = inputLookup[t%inputLookup.Count]
    setA <> setB
)

let inputLookup2 = 
    [0..700]
    |> List.fold (fun state i -> 
        state |> Map.add i (inputMap |> ValleyMap.ofTime i |> ValleyMap.toOccupied)
    ) Map.empty

inputLookup[120%inputLookup.Count] = inputLookup2[601%inputLookup.Count]

inputMap |> ValleyMap.ofTime (120*5) |> ValleyMap.print


let exampleFirst = bfs (5,4) (nonVisitedNeighbours (buildLookup exampleMap 1)) { Position = (0,-1); Time = 0; Previous = None }
let exampleSecond = bfs (0,-1) (nonVisitedNeighbours (buildLookup exampleMap 1)) { Position = (5,4); Time = exampleFirst.Time; Previous = None }
let exampleLast = bfs (5,4) (nonVisitedNeighbours (buildLookup exampleMap 1)) { Position = (0,-1); Time = exampleSecond.Time; Previous = None }



exampleFirst.Time
exampleSecond.Time
exampleLast.Time

let there = bfs (119,25) (nonVisitedNeighbours inputLookup) { Position = (0,-1); Time = 0; Previous = None }
let back = bfs (0,-1) (nonVisitedNeighbours inputLookup) { Position = (119,25); Time = there.Time; Previous = None }
let thereAgain = bfs (119,25) (nonVisitedNeighbours inputLookup) { Position = (0,-1); Time = back.Time; Previous = None }
thereAgain.Time

// That's not the right answer; your answer is too low. If you're stuck, make sure you're using the full input data;
// there are also some general tips on the about page, or you can ask for hints on the subreddit.
// Please wait one minute before trying again. (You guessed 214.) [Return to Day 24]

// That's not the right answer; your answer is too high. If you're stuck, make sure you're using the full input data;
// there are also some general tips on the about page, or you can ask for hints on the subreddit.
// Please wait one minute before trying again. (You guessed 714.) [Return to Day 24]

// That's not the right answer; your answer is too high. If you're stuck, make sure you're using the full input data;
// there are also some general tips on the about page, or you can ask for hints on the subreddit.
// Please wait one minute before trying again. (You guessed 662.) [Return to Day 24]

exampleMap |> ValleyMap.ofTime 0 |> ValleyMap.print
exampleMap |> ValleyMap.ofTime 4 |> ValleyMap.print
exampleMap |> ValleyMap.ofTime 8 |> ValleyMap.print
exampleMap |> ValleyMap.ofTime 24 |> ValleyMap.print

let firstStar =
    0

firstStar

let secondStar = 
    0

secondStar

