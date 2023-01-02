// --- Day 17: Pyroclastic Flow ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadAllText(@".\Input\Day17.txt")

let example = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

type Shape = HorizontalPipe | Plus | Angle | VerticalPipe | Square
let shapeOrder = [ HorizontalPipe; Plus; Angle; VerticalPipe; Square ]

type Rock = { Shape: Shape; X: int; Y: int; Content: int list }
    with 
    member this.Height = this.Content.Length
    member this.Collides y pixel = this.Overlaps y && (this.Layer y &&& pixel = pixel)
    member this.Overlaps y =
        let index = this.Index y
        index >= 0 && index < this.Height
    member this.Layer y = 
        if this.Overlaps y then
            this.Content[this.Index y]
        else
            failwithf "No such layer y: %i. (Height = %i)" y this.Height
    member this.Index y = this.Y - y
        

type CaveState = { Rocks: list<int>; FallingRock: Rock; AtRest: bool; JetIndex: int; Count: int }

let chamberWidth = 7
let walls = 256 + 1
let rockReleaseHeight = 3
let bottom = 256 + 128 + 64 + 32 + 16 + 8 + 4 + 2 + 1

shapeOrder[5 % shapeOrder.Length]
module CaveState =
    
    let release y count =
        let shape = shapeOrder[count % shapeOrder.Length]
        
        let content = 
            match shape with
            | HorizontalPipe -> [ 60 ]
            | Plus -> [ 16; 56; 16 ]
            | Angle -> [ 8; 8; 56 ]
            | VerticalPipe -> [ 32; 32; 32; 32 ]
            | Square -> [ 48; 48 ]

        { Shape = shape; X = 0; Y = y + rockReleaseHeight + content.Length; Content = content }

    let toRest cave = 
        let rock = cave.FallingRock
        let y1 = cave.Rocks.Length - 1 - rock.Y
        let y2 = y1 + rock.Height - 1
        let before = cave.Rocks |> List.take y1
        let newRocks = 
            cave.Rocks[y1..y2] 
            |> List.zip rock.Content
            |> List.map (fun (rock, rest) -> rock ||| rest)

        let after = cave.Rocks |> List.skip (y2 + 1)

        { cave with Rocks = before @ newRocks @ after; AtRest = true }

    let height cave = 
        let index =
            cave.Rocks 
            |> List.findIndex (fun layer -> layer ||| walls <> walls) 
        cave.Rocks.Length - index - 1

    let loadNextRock cave =
        let newFallingRock = release (height cave) (cave.Count + 1)
        let rockHeight = newFallingRock.Height
        let fillCount = newFallingRock.Y - cave.Rocks.Length
        let newRocks = [1..rockHeight] |> List.fold (fun state i -> walls::state) cave.Rocks
        { cave with Rocks = newRocks; FallingRock = newFallingRock; Count = cave.Count + 1; AtRest = false }

    let isCollision rock cave = 
        cave.Rocks
        |> List.skip (cave.Rocks.Length - 1 - rock.Y)
        |> List.take (rock.Content |> List.length)
        |> List.zip rock.Content 
        |> List.map (fun (rock, rest) -> rock &&& rest)
        |> List.exists ((<>) 0)

    let push transform cave = 
        let newContent = cave.FallingRock.Content |> List.map transform
        let newRock = { cave.FallingRock with Content = newContent }

        if cave |> isCollision newRock then
            cave
        else 
            let movedRock = { cave.FallingRock with Content = newContent }
            { cave with FallingRock = movedRock }

    let pushLeft cave = push (fun r -> r <<< 1) cave
    let pushRight cave = push (fun r -> r >>> 1) cave

    let fall cave =
        let newRock = { cave.FallingRock with Y = cave.FallingRock.Y - 1 }
        if cave |> isCollision newRock then
            cave |> toRest
        else
            { cave with FallingRock = newRock }

    let rockDrop (jetPattern: string) cave =
        Seq.unfold (fun cave ->
            let c = jetPattern[(cave.JetIndex)%jetPattern.Length]
            let fallenState = 
                match c with
                | '<' -> pushLeft cave
                | '>' -> pushRight cave
                | _ -> failwithf "Flaky jet push: %c" c
                |> fall

            let nextState = { fallenState with JetIndex = cave.JetIndex + 1 }
            Some(nextState, nextState)
        ) cave

    let rockDropToRest (jetPattern: string) cave =
        cave
        |> rockDrop jetPattern
        |> Seq.find (fun cave -> cave.AtRest)

    let dropUntilCount count jetPattern =
        let firstRock = release 0 0
        let initialCave = 
            { 
                Rocks = [walls;walls;walls;walls;walls;walls;walls;bottom]
                FallingRock = firstRock
                AtRest = false
                JetIndex = 0
                Count = 0
            }

        Seq.unfold (fun state -> 
            let dropState = rockDropToRest jetPattern state
            let nextRelease = loadNextRock dropState
            if nextRelease.Count > count then
                None
            else
                Some(nextRelease, nextRelease)
        ) initialCave

let firstRock = CaveState.release 0 0
let initialCave = { Rocks = [walls;walls;walls;walls;walls;walls;walls;bottom]; FallingRock = firstRock; AtRest = false; JetIndex = 0; Count = 0 }

CaveState.pushLeft initialCave

CaveState.height initialCave
let exampleRock = CaveState.release 0 3

exampleRock.Index 4
exampleRock.Overlaps 3
exampleRock.Overlaps 4
exampleRock.Overlaps 5
exampleRock
exampleRock.Layer 4
exampleRock.Collides 4 128
exampleRock.Height

let printChamber fromHeight toHeight state =    
    for y = toHeight downto fromHeight do
        if y = 0 then
            printfn "+-------+"
        else
            let layer = state.Rocks[state.Rocks.Length-1-y]
            printf "|"
            for x = chamberWidth downto 1 do
                let pixel = pown 2 x
                if state.FallingRock.Collides y pixel then
                    printf "@"
                else if layer &&& pixel = pixel then
                    printf "#"
                else
                    printf "."
            printfn "|"

printChamber 0 6 initialCave

let newExample = 
    initialCave 
    |> CaveState.pushRight
    |> CaveState.pushRight
    |> CaveState.pushRight
    |> CaveState.pushRight
    |> CaveState.pushLeft
    |> CaveState.fall
    |> CaveState.fall
    |> CaveState.fall
    |> CaveState.fall
    |> CaveState.loadNextRock

let newExampleCave = 
    CaveState.dropUntilCount 2 example
    |> Seq.last
    |> CaveState.rockDrop example
    |> Seq.take 1
    |> Seq.last

let check = CaveState.dropUntilCount 2022 example |> Seq.last

printChamber 0 (CaveState.height check + 3 + (check.FallingRock.Height)) check

check |> CaveState.height

let firstStar =
    input
    |> CaveState.dropUntilCount 2022
    |> Seq.last
    |> CaveState.height

firstStar

let secondStar = 
    0

secondStar
