module AoC.E2022.Day15

// --- Day 15: Beacon Exclusion Zone ---

open AoC
open IO

open System.Text.RegularExpressions

let input = readInputLines "2022" "Day15" |> List.ofSeq


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
    abs(y2 - y1) + abs(x2 - x1)

let fieldWidthAtY y (sensors: list<Position*Position>) =
    sensors
    |> List.map (fun (sensor, beacon) ->
        let sx, sy = sensor
        let distance = distance sensor beacon
        let miny = sy - distance
        let maxy = sy + distance
        if y >= miny && y <= maxy then
            let width = distance - abs(sy - y)
            Some(sx-width, sx+width)
        else 
            None
    )
    |> List.choose (id)

let lineLength (line: Line) = 
    let x1, x2 = line
    abs(x2 - x1) + 1

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

let mergeAllLines (lines: Line list) =    

    lines 
    |> List.sortBy fst
    |> List.fold (fun state line ->
        if state = [] then
            [line]
        else
            if overlaps state.Head line then
                let merged = mergeLines state.Head line
                merged::state.Tail
            else 
                line::state
    ) []
    
let countTotal line (sensors: list<Position*Position>) =
    let lines =
        sensors
        |> fieldWidthAtY line
        |> mergeAllLines

    let count = lines |> List.sumBy lineLength
    
    let beaconOnLine (bx, by) = by = line

    let beaconsOnLine =
        sensors 
        |> List.map snd
        |> List.filter beaconOnLine
        |> List.distinct
        |> List.length

    count - beaconsOnLine


let firstStar () =
    input
    |> parse
    |> countTotal 2000000


let beaconmin = 0
let beaconmax = 4000000

let tuningFactor = 4000000

let tuningFrequency beacon =
    let x, y = beacon
    x * 4000000L + y

let mergeAllLinesAtY line (sensors: list<Position*Position>) =
    sensors
    |> fieldWidthAtY line
    |> mergeAllLines
    |> List.sortBy fst

let gaps (lines: Line list) =
    
    let gap (line1, line2) =
        let xs1, xe1 = line1
        let xs2, xe2 = line2
        if xs2 - xe1 > 1 then
            Some (xe1+1)
        else 
            None

    lines
    |> List.pairwise
    |> List.map gap


let secondStar () = 

    let sensors = input |> parse

    [beaconmin..beaconmax]
    |> List.map (fun y ->
        let gaps = 
            mergeAllLinesAtY y sensors 
            |> gaps 
            |> List.choose (id)
        if gaps.IsEmpty then
            None
        else 
            Some(gaps |> List.exactlyOne, y)
        
    )
    |> List.choose (id)
    |> List.head
    |> fun (x, y) -> x |> int64, y |> int64
    |> tuningFrequency    