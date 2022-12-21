// --- Day 17: Pyroclastic Flow ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadAllText(@".\Input\Day17.txt")

let example = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"


type Shape = HorizontalPipe | Plus | Angle | VerticalPipe | Square
let shapeOrder = [ HorizontalPipe; Plus; Angle; VerticalPipe; Square ]

type RockState = { Rocks: Set<int*int>; Position: int*int; AtRest: bool; JetIndex: int; Count: int }

module RockState =
    let shape state = shapeOrder[(state.Count-1) % shapeOrder.Length]

let chamberWidth = 7
let rocksReleased = 2022
let rockReleaseHeight = 3

let shapeWidth shape =
    match shape with
    | HorizontalPipe -> 4
    | Plus -> 3
    | Angle -> 3
    | VerticalPipe -> 1
    | Square -> 2

let shapeHeight shape =
    match shape with
    | HorizontalPipe -> 1
    | Plus -> 3
    | Angle -> 3
    | VerticalPipe -> 4
    | Square -> 2

let bottom = [1..chamberWidth] |> List.mapi (fun i _ -> i, 0) |> Set.ofList

let firstRelease = 
    let releasePosition = 2, shapeHeight HorizontalPipe + rockReleaseHeight
    { Rocks = bottom;  Position = releasePosition; AtRest = false; JetIndex = 0; Count = 1; }

let collisionPointsLeft state = 
    let shape = RockState.shape state
    let x, y = state.Position
    match shape with
    | HorizontalPipe -> [x-1, y] |> Set.ofList
    | Plus -> [x, y; x-1, y-1; x, y-2] |> Set.ofList
    | Angle -> [x+1, y; x+1, y-1; x-1, y-2] |> Set.ofList
    | VerticalPipe -> [x-1, y; x-1, y-1; x-1, y-2; x-1, y-3] |> Set.ofList
    | Square -> [x-1, y; x-1, y-1] |> Set.ofList

let collisionPointsRight state = 
    let shape = RockState.shape state
    let x, y = state.Position
    match shape with
    | HorizontalPipe -> [x+4, y] |> Set.ofList
    | Plus -> [x+2, y; x+3, y-1; x+2, y-2] |> Set.ofList
    | Angle -> [x+3, y; x+3, y-1; x+3, y-2] |> Set.ofList
    | VerticalPipe -> [x+1, y; x+1, y-1; x+1, y-2; x+1, y-3] |> Set.ofList
    | Square -> [x+2, y; x+2, y-1] |> Set.ofList

let pushLeft state = 
    let x, y = state.Position
    let newX = 
        if x = 0 then 
            0 
        else
            let collisionPonits = collisionPointsLeft state
            if collisionPonits |> Set.intersect state.Rocks |> Set.isEmpty then
                x-1
            else 
                x
    { state with Position = newX, y }

let pushRight state =
    let x, y = state.Position
    let shape = RockState.shape state
    let width = shapeWidth shape
    let stop = chamberWidth - width
    let newX = 
        if x = stop then 
            stop 
        else
            let collisionPonits = collisionPointsRight state
            if collisionPonits |> Set.intersect state.Rocks |> Set.isEmpty then
                x+1
            else
                x
    { state with Position = newX, y }


firstRelease |> pushRight |> pushRight |> pushRight
firstRelease |> pushLeft |> pushLeft |> pushLeft |> pushLeft


let collisionPointsDown state =
    let shape = RockState.shape state
    let x, y = state.Position
    match shape with
    | HorizontalPipe -> [x, y-1; x+1, y-1; x+2, y-1; x+3, y-1 ] |> Set.ofList
    | Plus -> [x, y-2; x+1, y-3; x+2, y-2] |> Set.ofList
    | Angle -> [x, y-3; x+1, y-3; x+2, y-3] |> Set.ofList
    | VerticalPipe -> [x, y-4] |> Set.ofList
    | Square -> [x, y-2; x+1, y-2] |> Set.ofList

let rockPoints state =
    let shape = RockState.shape state
    let x, y = state.Position
    match shape with
    | HorizontalPipe -> [x, y; x+1, y; x+2, y; x+3, y ] |> Set.ofList
    | Plus -> [x, y-1; x+1, y; x+1, y-1; x+1, y-2; x+2, y-1] |> Set.ofList
    | Angle -> [x, y-2; x+1, y-2; x+2, y; x+2, y-1; x+2, y-2] |> Set.ofList
    | VerticalPipe -> [x, y; x, y-1; x, y-2; x, y-3] |> Set.ofList
    | Square -> [x, y; x, y-1; x+1, y; x+1, y-1] |> Set.ofList 

let toRest state =
    let points = rockPoints state
    { state with Rocks = state.Rocks |> Set.union points; AtRest = true }

let rockDrop (jetPattern: string) state =
    // printfn ">dropping a %A: %A" (RockState.shape state) state
    Seq.unfold (fun state ->
        let c = jetPattern[(state.JetIndex)%jetPattern.Length]
        let pushedState = 
            match c with
            | '<' -> pushLeft state
            | '>' -> pushRight state
            | _ -> failwithf "Flaky jet push: %c" c

        let x, y = pushedState.Position
       
        let collisionPoints = collisionPointsDown pushedState

        let collisionUpdated =
            if Set.intersect collisionPoints state.Rocks |> Set.isEmpty then
                { pushedState with Position = x, y - 1 }
            else
                pushedState |> toRest

        let nextState = { collisionUpdated with JetIndex = state.JetIndex + 1 }
        Some(nextState, nextState)
    ) state

let rockDropToNth (jetPattern: string) nth state =
    state
    |> rockDrop jetPattern
    |> Seq.take nth
    |> Seq.last

let rockDropToRest (jetPattern: string) state =
    state
    |> rockDrop jetPattern
    |> Seq.find (fun state -> state.AtRest)

let calculateHeight state =
    state.Rocks |> Set.ofSeq |> Seq.map snd |> Seq.max

let loadNextRock state = 
    state 
    |> calculateHeight 
    |> fun y -> 
        let resetState = { state with Count = state.Count + 1; AtRest = false }
        let shapeHeight = shapeHeight (RockState.shape resetState)
        let releasePosition = 2, y + rockReleaseHeight + shapeHeight
        { resetState with Position = releasePosition; AtRest = false }

let dropUntilCount jetPattern =
    let initialState = firstRelease
    Seq.unfold (fun state -> 
        let dropState = rockDropToRest jetPattern state
        let nextRelease = loadNextRock dropState
        if nextRelease.Count > rocksReleased + 1 then
            None
        else
            Some(nextRelease, nextRelease)
    ) initialState

bottom

shapeHeight Square

collisionPointsDown { Rocks = bottom; Position = (2, 1); AtRest = false; JetIndex = 0; Count = 1 }

rockDropToRest example firstRelease
|> calculateHeight

rockDropToRest example { firstRelease with Count = 2; Position = 2, 4 }
|> calculateHeight

let printChamber fromHeight toHeight state =
    for y = toHeight downto fromHeight do
        for x = -1 to chamberWidth do
            let position = x, y
            let fallingRockPositions = 
                if state.AtRest then 
                    Set.empty
                else    
                    state |> rockPoints
            if x = -1 || x = chamberWidth then
                if y = 0 then printf "+" else printf "|"
            else if y = 0 then
                printf "-"
            else if fallingRockPositions |> Set.contains position then
                printf "@"
            else if state.Rocks |> Set.contains position then
                printf "#"
            else
                printf "."
        printfn ""

let nextRock = firstRelease |> rockDropToRest example |> loadNextRock

firstRelease 
|> rockDropToNth example 4
|> loadNextRock
|> printChamber 0 20

dropUntilCount example 
|> Seq.take 100
|> Seq.last
|> printChamber 0 200
|> calculateHeight

dropUntilCount input 
|> Seq.last
|> calculateHeight


dropUntilCount input
|> Seq.last
|> printChamber 2900 3100
|> calculateHeight


firstRelease
|> rockDropToRest example
|> loadNextRock
//|> rockDropToNth example 1
|> printChamber 0 20

let firstStar =
    0

firstStar

let secondStar = 
    0

secondStar

