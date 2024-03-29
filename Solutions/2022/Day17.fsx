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
        

type CaveState = { Rocks: list<int>; FallingRock: Rock; AtRest: bool; JetIndex: int; Count: int; Height: int }

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

        let newHeight = max cave.Height cave.FallingRock.Y

        { cave with Rocks = before @ newRocks @ after; AtRest = true; Height = newHeight }

    let height cave = 
        let index =
            cave.Rocks 
            |> List.findIndex (fun layer -> layer ||| walls <> walls) 
        cave.Rocks.Length - index - 1

    let top cave = cave.Rocks |> List.find (fun layer -> layer <> walls)

    let loadNextRock cave =
        let newFallingRock = release (height cave) (cave.Count + 1)
        let fillCount = newFallingRock.Y - cave.Rocks.Length
        let newRocks = [0..fillCount] |> List.fold (fun state i -> walls::state) cave.Rocks
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
                Height = 0
            }

        Seq.unfold (fun state -> 
            let dropState = rockDropToRest jetPattern state
            let nextRelease = loadNextRock dropState
            if nextRelease.Count > count then
                None
            else
                Some(nextRelease, nextRelease)
        ) initialCave

    let dropInfinite jetPattern = 
        let firstRock = release 0 0
        let initialCave = 
            { 
                Rocks = [walls;walls;walls;walls;walls;walls;walls;bottom]
                FallingRock = firstRock
                AtRest = false
                JetIndex = 0
                Count = 0
                Height = 0
            }

        Seq.unfold (fun cave -> 
            let dropState = rockDropToRest jetPattern cave
            let nextRelease = loadNextRock dropState
            Some(nextRelease, nextRelease)
        ) initialCave

let firstRock = CaveState.release 0 0
let initialCave = { Rocks = [walls;walls;walls;walls;walls;walls;walls;bottom]; FallingRock = firstRock; AtRest = false; JetIndex = 0; Count = 0; Height = 0 }

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

let printChamberToFile fromHeight toHeight state =
    async {
        use sw = new StreamWriter(new FileStream(@".\Input\Day17-cave.txt",  FileMode.Create, FileAccess.Write, FileShare.None, bufferSize = 4096, useAsync= true))
        for y = toHeight downto fromHeight do
            if y = 0 then
                do! sw.WriteLineAsync("+-------+") |> Async.AwaitTask
            else
                let layer = state.Rocks[state.Rocks.Length-1-y]
                do! sw.WriteAsync("|") |> Async.AwaitTask
                for x = chamberWidth downto 1 do
                    let pixel = pown 2 x
                    if state.FallingRock.Collides y pixel then
                        do! sw.WriteAsync("@") |> Async.AwaitTask
                    else if layer &&& pixel = pixel then
                        do! sw.WriteAsync("#") |> Async.AwaitTask
                    else
                        do! sw.WriteAsync(".") |> Async.AwaitTask
                let line = sprintf "| (%3i)" layer
                do! sw.WriteLineAsync(line) |> Async.AwaitTask
    }
    |> Async.RunSynchronously
    
let printChamberToFileWithRockMarker fromHeight toHeight (rockHeighMap: Map<int, list<int*int>>) state =
    async {
        use sw = new StreamWriter(new FileStream(@".\Input\Day17-cave.txt",  FileMode.Create, FileAccess.Write, FileShare.None, bufferSize = 4096, useAsync= true))
        for y = toHeight downto fromHeight do
            if y = 0 then
                do! sw.WriteLineAsync("+-------+") |> Async.AwaitTask
            else
                let layer = state.Rocks[state.Rocks.Length-1-y]
                do! sw.WriteAsync("|") |> Async.AwaitTask
                for x = chamberWidth downto 1 do
                    let pixel = pown 2 x
                    if state.FallingRock.Collides y pixel then
                        do! sw.WriteAsync("@") |> Async.AwaitTask
                    else if layer &&& pixel = pixel then
                        do! sw.WriteAsync("#") |> Async.AwaitTask
                    else
                        do! sw.WriteAsync(".") |> Async.AwaitTask

                let line = sprintf "| (%3i)" layer
                do! sw.WriteAsync(line) |> Async.AwaitTask

                if rockHeighMap |> Map.containsKey y then
                    let rocksAtHeight = rockHeighMap[y] |> List.map snd
                    let line = sprintf " height: %i rocks: %s" y (System.String.Join(",", rocksAtHeight))
                    do! sw.WriteLineAsync(line) |> Async.AwaitTask
                else
                    do! sw.WriteLineAsync("") |> Async.AwaitTask
    }
    |> Async.RunSynchronously
    
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

input
|> CaveState.dropUntilCount 5000
|> Seq.last
|> printChamberToFile 0 7667

let result = 
    example
    |> CaveState.dropUntilCount 2022
    |> Seq.last    

let identifier = result.Rocks |> List.groupBy (id) |> List.sortBy (fun (key, group) -> group.Length) |> List.filter (fun (key, group) -> group |> Seq.length = 5) 

// helps identifying pattern:
let result2 = 
    input
    |> CaveState.dropUntilCount 2022
    |> Seq.last    
let identifier2 = result2.Rocks |> List.groupBy (id) |> List.sortBy (fun (key, group) -> group.Length) |> List.filter (fun (key, group) -> group |> Seq.length = 5) 


result.Rocks
|> List.indexed 
|> List.filter (fun (index, layer) -> layer = 313)
|> List.pairwise 
|> List.map (fun (left, right) -> fst right - fst left)


let output = System.String.Join(",", result.Rocks)
File.WriteAllText(@".\Input\Day17-cave-numbers.txt", output)

let m = System.Text.RegularExpressions.Regex.Match(output, ",419,")
m.Groups

let rockHeights windPattern count = 
    windPattern
    |> CaveState.dropUntilCount count
    |> Seq.take count
    |> Seq.toList
    |> List.map (fun cave -> cave.Height, cave.Count)
    |> List.groupBy fst
    |> Map


let follow windPattern indexInShapes dropCount = 
    windPattern
    |> CaveState.dropUntilCount dropCount
    |> Seq.toList
    |> List.map (fun cave -> cave.Count, cave.Height)
    |> List.filter (fun (count, height) -> (shapeOrder.Length - indexInShapes - 1 + count) % shapeOrder.Length = 0)

let next = follow example 0 2022 |> List.take 20

next |> List.pairwise |> List.map (fun ((count1, height1),(count2, height2)) -> height2 - height1)

let heightsForShape windPattern shape dropCount =
    let shapeIndex = shapeOrder |> List.findIndex (fun s -> s = shape)

    follow windPattern shapeIndex dropCount

let heightDifferences windPattern shape dropCount =
    heightsForShape windPattern shape dropCount
    |> List.pairwise 
    |> List.map (fun ((count1, height1),(count2, height2)) -> height2 - height1)

heightDifferences input HorizontalPipe 2022 |> List.take 40

let caveInput = 
    input
    |> CaveState.dropUntilCount 5000
    |> Seq.last

printChamberToFileWithRockMarker 0 7667 (rockHeights input 5000) caveInput


let predictHeight windPattern (drops: int64) recurringFrom frequency = 

    let shapeCount = shapeOrder |> List.length

    let blockCount = frequency * shapeCount

    let sample = 
        windPattern
        |> CaveState.dropUntilCount (2 * blockCount + recurringFrom)
        |> Seq.toList
        |> List.map (fun cave -> cave.Count, cave.Height)

    let startBlock = sample |> List.find (fun (count, height) -> count = recurringFrom)
    let startCount = startBlock |> fst
    let endBlock = sample |> List.find (fun (count, height) -> count = recurringFrom + blockCount)
    
    
    let blockHeight = (snd endBlock - snd startBlock )
    let blockCount = frequency * shapeCount

    let blocks = (drops - int64 startCount) / int64 blockCount 
    let rest = (drops - int64 startCount) % int64 blockCount

    let restDrop = sample |> List.find (fun (count, height) -> count = (endBlock |> fst) + (rest |> int))

    let restHeight = (restDrop |> snd) - (endBlock |> snd) |> int64

    let skipHeight = startBlock |> snd |> int64
    let newPredictHeight = skipHeight + int64 blockHeight * blocks + restHeight

    // startBlock, endBlock, blocks, rest, skipHeight, blockHeight, restHeight, newPredictHeight, sample
    newPredictHeight

predictHeight example 2022 21 7
predictHeight example 1000000000000L 21 7


// |###..##| (487) height: 5023 rocks: 3280
// |###..##| (487) height: 2350 rocks: 1545

let inputFrequency = 3280 - 1545
let inputFrequencyByShapes = inputFrequency / 5

predictHeight input 2022 1545 347
predictHeight input 1000000000000L 1545 inputFrequencyByShapes

// 1527170868348: That's not the right answer; your answer is too low.
// 1540634005751L

let printListOfInts (ints: int list) =
    async {
        use sw = new StreamWriter(new FileStream(@".\Input\Day17-height-diffs.txt",  FileMode.Create, FileAccess.Write, FileShare.None, bufferSize = 4096, useAsync= true))
        let output = System.String.Join(",", ints)
        do! sw.WriteLineAsync(output) |> Async.AwaitTask
                    
    }
    |> Async.RunSynchronously

heightDifferences input HorizontalPipe 5000 |> printListOfInts

let firstStar =
    input
    |> CaveState.dropUntilCount 2022
    |> Seq.last
    |> CaveState.height

firstStar

let secondStar = 
    predictHeight input 1000000000000L 1545 inputFrequencyByShapes

secondStar
