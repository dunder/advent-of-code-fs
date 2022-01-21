module AoC.E2021.Day22

// --- Day 22: Reactor Reboot ---

open AoC
open IO

open System.Text.RegularExpressions

let input = readInputLines "2021" "Day22" |> List.ofSeq


type Cuboid = { XRange: int*int; YRange: int*int; ZRange: int*int }

let parse lines =
    lines 
    |> List.mapi( fun i line ->
        let m = Regex.Match(line, @"^(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)$")
        if 
            m.Success
        then 
            let values = [for g in m.Groups -> g.Value] |> List.tail

            let turn = 
                values 
                |> List.take 1
                |> List.map (fun value -> if value = "on" then true else false)
                |> List.exactlyOne

            let ranges =
                values
                |> List.tail
                |> List.map System.Int32.Parse

            turn, { XRange = ranges.[0], ranges.[1]; YRange = ranges.[2], ranges.[3]; ZRange = ranges.[4], ranges.[5] }
        else 
            failwithf "%i: Bad line input: %s" i line
    )
    
let allCubes cuboid =
    
    let xMin = max -50 (fst cuboid.XRange)
    let xMax = min 50 (snd cuboid.XRange)

    let yMin = max -50 (fst cuboid.YRange)
    let yMax = min 50 (snd cuboid.YRange)

    let zMin = max -50 (fst cuboid.ZRange)
    let zMax = min 50 (snd cuboid.ZRange)

    seq {
        for x in xMin..xMax do
            for y in yMin..yMax do
                for z in zMin..zMax do
                    yield (x,y,z)
    }
    |> Set.ofSeq

let apply turnedOn rebootStep =
    let turnOn, cuboid = rebootStep
    if turnOn then
        let onSet = allCubes cuboid 
        turnedOn |> Set.union onSet
    else
        let offSet = allCubes cuboid
        offSet |> Set.difference turnedOn

let applyAll rebootSteps =
    rebootSteps
    |> Seq.fold apply Set.empty

let firstStar () =
    let rebootSteps = parse input
    applyAll rebootSteps
    |> Seq.length

module Range = 

    let sort range1 range2 = 
        if fst range1 > fst range2 then
            range2, range1
        else 
            range1, range2

    let intersects range1 range2 =
        let range1, range2 = sort range1 range2

        let start1, stop1 = range1
        let start2, stop2 = range2

        start2 >= start1 && start2 <= stop1

    let intersection range1 range2 =

        if intersects range1 range2 then
            let range1, range2 = sort range1 range2

            let intersectionStart = fst range2
            let intersectionEnd = min (snd range1) (snd range2)

            Some (intersectionStart, intersectionEnd)
        else 
            None

    // true if range totally includes rangeMaybeIncluded
    // note: order dependent
    let includes rangeMaybeIncluded range =
        if intersects range rangeMaybeIncluded then
            let rStart, rEnd = range
            let candidateStart, candidateEnd = rangeMaybeIncluded
            rStart <= candidateStart && rEnd >= candidateEnd
        else 
            false

    // return a list of the intersection ranges of range2 with range1
    // intersections on "the outside" from range1 (think of range1 as
    // the solid part)
    // note that the order of arguments matter for this function
    let intersections range1 range2 = 
        match range1, range2 with
        | (r1start, r1end), (r2start, r2end)  when r2start < r1start && r2end > r1end -> 
            [(r2start, r1start-1); (r1start, r1end); (r1end+1, r2end)]
        | (r1start, r1end), (r2start, r2end)  when r2start = r1start && r2end > r1end -> 
            [(r1start, r1end); (r1end+1, r2end)]
        | (r1start, r1end), (r2start, r2end)  when r2start < r1start && r2end = r1end ->
            [(r2start, r1start-1); (r1start, r2end)]
        | (r1start, r1end), (r2start, r2end)  when r2start > r1start && r2end > r1end -> 
            [(r2start, r1end); (r1end+1, r2end)]
        | (r1start, r1end), (r2start, r2end)  when r2start < r1start && r2end < r1end -> 
            [(r2start, r1start-1); (r1start, r2end)]
        | _ -> 
            [range2]

    let length range = abs (snd range - fst range + 1) |> int64
module Cuboid =

    let intersects cuboid1 cuboid2 =

        let xIntersect = Range.intersects cuboid1.XRange cuboid2.XRange
        let yIntersect = Range.intersects cuboid1.YRange cuboid2.YRange
        let zIntersect = Range.intersects cuboid1.ZRange cuboid2.ZRange

        xIntersect && yIntersect && zIntersect

    let intersection cuboid1 cuboid2 = 
        
        let xIntersection = Range.intersection cuboid1.XRange cuboid2.XRange
        let yIntersection = Range.intersection cuboid1.YRange cuboid2.YRange
        let zIntersection = Range.intersection cuboid1.ZRange cuboid2.ZRange

        match xIntersection, yIntersection, zIntersection with
        | Some xIntersection, Some yIntersection, Some zIntersection -> Some { XRange = xIntersection; YRange = yIntersection; ZRange = zIntersection }
        | _ -> None

    // true if cuboid1 totally includes cuboid2
    // note that order of the arguments matter
    let includes cuboidMaybeIncluded cuboid = 
        if intersects cuboid cuboidMaybeIncluded then
            let xRangeIncluded = cuboid.XRange |> Range.includes cuboidMaybeIncluded.XRange
            let yRangeIncluded = cuboid.YRange |> Range.includes cuboidMaybeIncluded.YRange
            let zRangeIncluded = cuboid.ZRange |> Range.includes cuboidMaybeIncluded.ZRange
            xRangeIncluded && yRangeIncluded && zRangeIncluded
        else 
            false

    // split range2 according to the intersection points with range1
    let intersectionSplit cuboid1 cuboid2 = 
        if intersects cuboid1 cuboid2 then
            let xIntersections = Range.intersections cuboid1.XRange cuboid2.XRange
            let yIntersections = Range.intersections cuboid1.YRange cuboid2.YRange
            let zIntersections = Range.intersections cuboid1.ZRange cuboid2.ZRange
            seq {
                for xRange in xIntersections do
                    for yRange in yIntersections do
                        for zRange in zIntersections do
                            yield { XRange = xRange; YRange = yRange; ZRange = zRange }
            }
            |> Seq.toList
        else 
            [cuboid2]
        
    // splits cuboid2 into cuboids based on the intersections with cuboid1 and keeps the
    // cuboids that does not intersect with cuboid1
    let split cuboid1 cuboid2 =
        intersectionSplit cuboid1 cuboid2
        |> List.filter (fun cuboid -> not <| intersects cuboid cuboid1)

    let count cuboid = Range.length cuboid.XRange * Range.length cuboid.YRange * Range.length cuboid.ZRange

// turnedOn is a list of non overlapping cuboids that are already turned on
let turnOn turnedOn cuboid =
    let turnedOnIntersects = turnedOn |> List.filter (Cuboid.intersects cuboid)
    let newTurnedOn =
        turnedOnIntersects
        |> List.fold (fun parts intersectingCuboid -> 
            let result = 
                parts
                |> List.fold (fun currentParts cuboidPart ->
                    let newParts = cuboidPart |> Cuboid.split intersectingCuboid |> Set.ofList
                    currentParts |> Set.remove cuboidPart |> Set.union newParts
                ) (parts |> Set.ofList)
            result |> Set.toList
        ) [cuboid]

    turnedOn @ newTurnedOn

let turnOff turnedOn cuboid = 
    let newTurnedOn =
        turnedOn
        |> List.fold (fun stillTurnedOn intersectingCuboid -> 
            let stillOn = intersectingCuboid |> Cuboid.split cuboid
                
            stillTurnedOn @ stillOn
        ) []
    newTurnedOn

let init turnedOn rebootStep =
    let turnOnOperation, cuboid = rebootStep
    if turnOnOperation then
        turnOn turnedOn cuboid 
    else
        turnOff turnedOn cuboid

let initAll rebootSteps =
    rebootSteps
    |> Seq.fold init []

let secondStar () = 
    parse input
    |> initAll
    |> List.sumBy Cuboid.count

