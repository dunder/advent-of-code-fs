// --- Day 15: Beacon Exclusion Zone ---

open System.IO

open System.Text.RegularExpressions

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day15.txt") |> List.ofSeq

let example =
    [
        "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
        "Sensor at x=9, y=16: closest beacon is at x=10, y=16"
        "Sensor at x=13, y=2: closest beacon is at x=15, y=3"
        "Sensor at x=12, y=14: closest beacon is at x=10, y=16"
        "Sensor at x=10, y=20: closest beacon is at x=10, y=16"
        "Sensor at x=14, y=17: closest beacon is at x=10, y=16"
        "Sensor at x=8, y=7: closest beacon is at x=2, y=10"
        "Sensor at x=2, y=0: closest beacon is at x=2, y=10"
        "Sensor at x=0, y=11: closest beacon is at x=2, y=10"
        "Sensor at x=20, y=14: closest beacon is at x=25, y=17"
        "Sensor at x=17, y=20: closest beacon is at x=21, y=22"
        "Sensor at x=16, y=7: closest beacon is at x=15, y=3"
        "Sensor at x=14, y=3: closest beacon is at x=15, y=3"
        "Sensor at x=20, y=1: closest beacon is at x=15, y=3"
    ]

type Position = int*int
type Line = int*int

let parse (lines: string list) =
    lines
    |> List.map (fun line ->
        let toCoordinate text =
            let m = Regex.Match(text, "x=(?<x>-?\d+), y=(?<y>-?\d+)")
            let x = m.Groups["x"].Value |> int
            let y = m.Groups["y"].Value |> int
            x, y

        let parts = line.Split(":")
        let sensor = toCoordinate parts[0]
        let beacon = toCoordinate parts[1]
        sensor, beacon
    )

let distance sensor beacon =
    let x1, y1 = sensor
    let x2, y2 = beacon
    abs(y2-y1) + abs(x2-x1)

distance (8,7) (2,10)

let field sensor beacon =
    let distance = distance sensor beacon
    let sensorx, sensory = sensor
    let coordinates =
        seq {
            for y in sensory-distance..sensory+distance do
                let width = distance - abs(sensory-y)
                let fromx = sensorx-width
                let tox = sensorx+width
                for x in fromx..tox do
                    yield x, y
        }

    coordinates |> Seq.fold (fun field (x,y) ->
        field |> Set.add (x,y)
    ) Set.empty


let s = (8,7)
let d = 9
let sx, sy = s
let y = sy-d
let w = d - abs(sy - y) + 1

field (8,7) (2,10)

let printSensor sensor beacon =
    let sensorx, sensory = sensor
    let field = field sensor beacon
    let distance = distance sensor beacon
    for y in sensory-distance..sensory+distance do
        for x in sensorx-distance..sensorx+distance do
            let position = x, y
            let marker =
                match position with
                | (x, y) when (x, y) = sensor -> Some("S")
                | (x, y) when (x, y) = beacon -> Some("B")
                | _ -> None
            match marker with
            | Some m -> printf "%s" m
            | None ->
                if  field |> Set.contains position then
                    printf "#"
                else
                    printf "."
        printfn ""

printSensor (8,7) (2,10)

let exampleSensors = example |> parse
let sensors = input |> parse


let withinField sensor beacon position =
    if position = beacon then 
        false
    else
        let distance = distance sensor beacon
        let x, y = position
        let sensorx, sensory = sensor        

        let minx = sensorx-distance
        let maxx = sensorx+distance

        let miny = sensory-distance
        let maxy = sensory+distance

        if x >= minx && x <= maxx && y >= miny && y <= maxy then

            let width = distance - abs(sensory - y)
            let height = distance - abs(sensorx - x)
            let withinx = x >= sensorx-width && x <= sensorx+width
            let withiny = y >= sensory-height && y <= sensory+height
            withinx && withiny
        else
            false

withinField (8,7) (2,10) (8,-3)


let lineCovered (sensors: list<Position*Position>) line =
    let endpoints =
        sensors
        |> List.map (fun (sensor, beacon) ->
            let sx, sy = sensor            
            let distance = distance sensor beacon
            let width = distance - abs(sy - line)
            sx-width, sx+width
        )

    let minx = endpoints |> List.map fst |> List.min
    let maxx = endpoints |> List.map snd |> List.max

    minx, maxx

let lines line (sensors: list<Position*Position>) =
    sensors
    |> List.map (fun (sensor, beacon) ->
        let sx, sy = sensor
        let distance = distance sensor beacon
        let miny = sy - distance
        let maxy = sy + distance
        if line >= miny && line <= maxy then
            let width = distance - abs(sy - line)
            Some(sx-width, sx+width)
        else 
            None
    )
    |> List.choose (id)

let lineLength (x1, x2) = abs(x2-x1) + 1

let overlaps (line1: Line) (line2: Line) =
    let xs1, xe1 = line1
    let xs2, xe2 = line2
    xs1 >= xs2 && xs1 <= xe2 ||
    xe1 >= xs2 && xe1 <= xe2 ||
    xs1 <= xs2 && xe1 >= xe2

let mergeLines (line1: Line) (line2: Line) =
    if overlaps line1 line2 then
        let xs1, xe1 = line1
        let xs2, xe2 = line2
        let minx = min xs1 xs2
        let maxx = max xe1 xe2
        minx, maxx
    else
        failwithf "Lines do not overlap: %A and %A" line1 line2

let merge (lines: (int*int) list) =    

    lines 
    |> List.sortBy fst
    |> List.fold (fun state line ->
        if state = [] then
            [line]
        else
            let head::tail = state

            if overlaps head line then
                let merged = mergeLines head line
                merged::tail
            else 
                line::state
    ) []
    
let countTotal line (sensors: list<Position*Position>) =
    let lines =
        sensors
        |> lines line
        |> merge

    let count = lines |> List.sumBy lineLength
    
    let beaconsOnLine =
        sensors 
        |> List.map snd
        |> List.filter (fun (x,y) -> y = line)
        |> List.distinct
        |> List.length

    count - beaconsOnLine


exampleSensors |> countTotal 10
sensors |> countTotal 2000000

let beaconmin = 0
let beaconmax = 4000000

let tuningFactor = 4000000

// let gaps (sensors: list<Position*Position>) =
//     [start..stop]
//     |> List.map lines


let tuningFrequency beacon =
    let x, y = beacon
    x * 4000000L + y

let exbeaconmin = 0
let exbeaconmax = 20

let test line (sensors: list<Position*Position>) =
    sensors
    |> lines line
    |> merge
    |> List.sortBy fst

let gaps minx maxx (lines: Line list)=
    lines
    |> List.pairwise
    |> List.map (fun (line1, line2) ->
        let xs1, xe1 = line1
        let xs2, xe2 = line2
        if xs2 - xe1 > 1 then // && xe1 + 1 >= minx && xe1 + 1 <= maxx then
            Some (xe1+1)
        else 
            None
    )

test 14 exampleSensors |> gaps exbeaconmin exbeaconmax |> List.choose (id)

[beaconmin..beaconmax]
|> List.map (fun y ->
    let hits = test y sensors |> gaps beaconmin beaconmin |> List.choose (id)
    if hits <> [] then
        Some(hits[0], y)
    else 
        None
    
)
|> List.choose (id)
|> List.head
|> fun (x, y) -> x |> int64, y |> int64


// 1898262264 (too low)

(3135800L, 2766584L) |> tuningFrequency
// beacon (14,11) -> tuningFrequency = 56000011

tuningFrequency (14,11)

let covered (lines: (int*int) list) beacon =
    let bx, by = beacon
    bx, by

let beaconsWithin (sensors: list<Position*Position>) =
    sensors
    |> List.map snd

lines 10 exampleSensors |> List.map lineLength

let walkTheLine (sensors: list<Position*Position>) line =
    let minx, maxx = lineCovered sensors line
    seq {
        for x in minx..maxx do
            yield x, line
    }

let countWithin (sensors: list<Position*Position>) line =

    let beacons = sensors |> List.map snd |> Set.ofList

    walkTheLine sensors line
    |> Seq.fold (fun count position -> 
        if beacons |> Set.contains position then
            count
        else 
            count + 1
    ) 0


countWithin exampleSensors 10


let fieldCoverage (sensors: list<Position*Position>) =
    sensors
    |> List.fold (fun covered (sensor,beacon) ->
        let field = field sensor beacon
        covered |> Set.union field
    ) Set.empty

let beacons (sensors: list<Position*Position>) =
    sensors
    |> List.map snd
    |> Set.ofList

let countCoveredLine line (sensors: list<Position*Position>) =
    
    sensors
    |> beacons
    |> Set.difference (sensors |> fieldCoverage)
    |> Set.toList
    |> List.filter (fun (x,y) -> y = line)
    |> List.length

countCoveredLine 10 exampleSensors

// countCoveredLine 2000000 sensors


let firstStar =
    0

firstStar

let secondStar =
    0

secondStar

