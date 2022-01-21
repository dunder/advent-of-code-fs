// --- Day 22: Reactor Reboot ---

open System.IO
open System.Text.RegularExpressions

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day22.txt") |> List.ofSeq

let example = [
    "on x=10..12,y=10..12,z=10..12"
    "on x=11..13,y=11..13,z=11..13"
    "off x=9..11,y=9..11,z=9..11"
    "on x=10..10,y=10..10,z=10..10"
]

let example2 = [
    "on x=-20..26,y=-36..17,z=-47..7"
    "on x=-20..33,y=-21..23,z=-26..28"
    "on x=-22..28,y=-29..23,z=-38..16"
    "on x=-46..7,y=-6..46,z=-50..-1"
    "on x=-49..1,y=-3..46,z=-24..28"
    "on x=2..47,y=-22..22,z=-23..27"
    "on x=-27..23,y=-28..26,z=-21..29"
    "on x=-39..5,y=-6..47,z=-3..44"
    "on x=-30..21,y=-8..43,z=-13..34"
    "on x=-22..26,y=-27..20,z=-29..19"
    "off x=-48..-32,y=26..41,z=-47..-37"
    "on x=-12..35,y=6..50,z=-50..-2"
    "off x=-48..-32,y=-32..-16,z=-15..-5"
    "on x=-18..26,y=-33..15,z=-7..46"
    "off x=-40..-22,y=-38..-28,z=23..41"
    "on x=-16..35,y=-41..10,z=-47..6"
    "off x=-32..-23,y=11..30,z=-14..3"
    "on x=-49..-5,y=-3..45,z=-29..18"
    "off x=18..30,y=-20..-8,z=-3..13"
    "on x=-41..9,y=-7..43,z=-33..15"
    "on x=-54112..-39298,y=-85059..-49293,z=-27449..7877"
    "on x=967..23432,y=45373..81175,z=27513..53682"
]

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

let set1 = set [0;1;2;3;4]
let set2 = set [2;3]

Set.difference set1 set2

let firstStar =
    let rebootSteps = parse input
    applyAll rebootSteps
    |> Seq.length

firstStar


let example3 = [
    "on x=-5..47,y=-31..22,z=-19..33"
    "on x=-44..5,y=-27..21,z=-14..35"
    "on x=-49..-1,y=-11..42,z=-10..38"
    "on x=-20..34,y=-40..6,z=-44..1"
    "off x=26..39,y=40..50,z=-2..11"
    "on x=-41..5,y=-41..6,z=-36..8"
    "off x=-43..-33,y=-45..-28,z=7..25"
    "on x=-33..15,y=-32..19,z=-34..11"
    "off x=35..47,y=-46..-34,z=-11..5"
    "on x=-14..36,y=-6..44,z=-16..29"
    "on x=-57795..-6158,y=29564..72030,z=20435..90618"
    "on x=36731..105352,y=-21140..28532,z=16094..90401"
    "on x=30999..107136,y=-53464..15513,z=8553..71215"
    "on x=13528..83982,y=-99403..-27377,z=-24141..23996"
    "on x=-72682..-12347,y=18159..111354,z=7391..80950"
    "on x=-1060..80757,y=-65301..-20884,z=-103788..-16709"
    "on x=-83015..-9461,y=-72160..-8347,z=-81239..-26856"
    "on x=-52752..22273,y=-49450..9096,z=54442..119054"
    "on x=-29982..40483,y=-108474..-28371,z=-24328..38471"
    "on x=-4958..62750,y=40422..118853,z=-7672..65583"
    "on x=55694..108686,y=-43367..46958,z=-26781..48729"
    "on x=-98497..-18186,y=-63569..3412,z=1232..88485"
    "on x=-726..56291,y=-62629..13224,z=18033..85226"
    "on x=-110886..-34664,y=-81338..-8658,z=8914..63723"
    "on x=-55829..24974,y=-16897..54165,z=-121762..-28058"
    "on x=-65152..-11147,y=22489..91432,z=-58782..1780"
    "on x=-120100..-32970,y=-46592..27473,z=-11695..61039"
    "on x=-18631..37533,y=-124565..-50804,z=-35667..28308"
    "on x=-57817..18248,y=49321..117703,z=5745..55881"
    "on x=14781..98692,y=-1341..70827,z=15753..70151"
    "on x=-34419..55919,y=-19626..40991,z=39015..114138"
    "on x=-60785..11593,y=-56135..2999,z=-95368..-26915"
    "on x=-32178..58085,y=17647..101866,z=-91405..-8878"
    "on x=-53655..12091,y=50097..105568,z=-75335..-4862"
    "on x=-111166..-40997,y=-71714..2688,z=5609..50954"
    "on x=-16602..70118,y=-98693..-44401,z=5197..76897"
    "on x=16383..101554,y=4615..83635,z=-44907..18747"
    "off x=-95822..-15171,y=-19987..48940,z=10804..104439"
    "on x=-89813..-14614,y=16069..88491,z=-3297..45228"
    "on x=41075..99376,y=-20427..49978,z=-52012..13762"
    "on x=-21330..50085,y=-17944..62733,z=-112280..-30197"
    "on x=-16478..35915,y=36008..118594,z=-7885..47086"
    "off x=-98156..-27851,y=-49952..43171,z=-99005..-8456"
    "off x=2032..69770,y=-71013..4824,z=7471..94418"
    "on x=43670..120875,y=-42068..12382,z=-24787..38892"
    "off x=37514..111226,y=-45862..25743,z=-16714..54663"
    "off x=25699..97951,y=-30668..59918,z=-15349..69697"
    "off x=-44271..17935,y=-9516..60759,z=49131..112598"
    "on x=-61695..-5813,y=40978..94975,z=8655..80240"
    "off x=-101086..-9439,y=-7088..67543,z=33935..83858"
    "off x=18020..114017,y=-48931..32606,z=21474..89843"
    "off x=-77139..10506,y=-89994..-18797,z=-80..59318"
    "off x=8476..79288,y=-75520..11602,z=-96624..-24783"
    "on x=-47488..-1262,y=24338..100707,z=16292..72967"
    "off x=-84341..13987,y=2429..92914,z=-90671..-1318"
    "off x=-37810..49457,y=-71013..-7894,z=-105357..-13188"
    "off x=-27365..46395,y=31009..98017,z=15428..76570"
    "off x=-70369..-16548,y=22648..78696,z=-1892..86821"
    "on x=-53470..21291,y=-120233..-33476,z=-44150..38147"
    "off x=-93533..-4276,y=-16170..68771,z=-104985..-24507"
]

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
        // printfn "intersections: range1 = %O, range2 = %O" range1 range2
        match range1, range2 with
        // r1:     |-|
        // r2:   |-----|
        //       |-|-|-|
        | (r1start, r1end), (r2start, r2end)  when r2start < r1start && r2end > r1end -> 
            // printfn "case 1: %O" [(r2start, r1start-1); (r1start, r1end); (r1end+1, r2end)]
            [(r2start, r1start-1); (r1start, r1end); (r1end+1, r2end)]
        // r1:   |-|
        // r2:   |-----|
        //       |-|---|
        | (r1start, r1end), (r2start, r2end)  when r2start = r1start && r2end > r1end -> 
            // printfn "case 2: %O" [(r1start, r1end); (r1end+1, r2end)]
            [(r1start, r1end); (r1end+1, r2end)]
        // r1:       |-|
        // r2:   |-----|
        | (r1start, r1end), (r2start, r2end)  when r2start < r1start && r2end = r1end -> 
            // printfn "case 3: %O" [(r2start, r1start-1); (r1start, r2end)]
            [(r2start, r1start-1); (r1start, r2end)]
        // r1:   |-----|
        // r2:       |---|
        | (r1start, r1end), (r2start, r2end)  when r2start > r1start && r2end > r1end -> 
            // printfn "case 4: %O" [(r2start, r1end); (r1end+1, r2end)]
            [(r2start, r1end); (r1end+1, r2end)]
        // r1:   |-----|
        // r2: |---|
        | (r1start, r1end), (r2start, r2end)  when r2start < r1start && r2end < r1end -> 
            // printfn "case 5: %O" [(r2start, r1start-1); (r1start, r2end)]
            [(r2start, r1start-1); (r1start, r2end)]
        // r1:  |-----|    |-----|    |-----|
        // r2:    |---|     |---|     |---|    
        | _ -> 
            // printfn "default case"
            [range2]

    let length range = abs (snd range - fst range + 1) |> int64

// -19         33
//  |------//---|
//     |---//-----|
//    -14         35

let testRange1 = (-14, 35)
let testRange2 = (-19, 33)

Range.intersections testRange1 testRange2

let rangeIncludeChecks = [    
    Range.includes (2,4) (1,5) = true
    Range.includes (1,5) (1,5) = true
    Range.includes (2,5) (1,5) = true
    Range.includes (1,4) (1,5) = true
    Range.includes (1,5) (2,4) = false
    Range.includes (2,6) (1,5) = false
    Range.includes (0,5) (1,5) = false
    Range.includes (1,6) (1,5) = false
    Range.includes (6,8) (1,5) = false
]

rangeIncludeChecks |> List.forall id

let r1 = 1,4
let rCover = 0,5
let coverIntersection = Range.intersection r1 rCover
let rCovered = 2,3 
let coveredIntersection = Range.intersection r1 rCovered
let uncovered = 5,6
let uncoveredIntersection = Range.intersection r1 uncovered
Range.intersection rCover r1 = Range.intersection r1 rCover
Range.intersection rCovered r1 = Range.intersection r1 rCovered

let r4 = 2,3
let r5 = 1,4
Range.intersections r5 r4

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


let c1 = { XRange = 0,4; YRange = 0,4; ZRange = 0,4 }
let c2 = { XRange = 1,5; YRange = 2,6; ZRange = 1,3 }

Cuboid.intersection c1 c2

let cuboidLarge = { XRange = 0,4; YRange = 0,4; ZRange = 0,4 }
let cuboidIncluded = { XRange = 1,3; YRange = 1,3; ZRange = 1,3 }

cuboidLarge.XRange |> Range.includes cuboidIncluded.XRange
cuboidLarge.YRange |> Range.includes cuboidIncluded.YRange
cuboidLarge.ZRange |> Range.includes cuboidIncluded.ZRange


cuboidLarge |> Cuboid.intersects cuboidIncluded
cuboidLarge |> Cuboid.includes cuboidIncluded

let cuboid1 = { XRange = 0,4; YRange = 0,4; ZRange = 0,4 }
let cuboid2 = { XRange = 1,5; YRange = 2,6; ZRange = 1,3 }

Range.intersections cuboid1.XRange cuboid2.XRange
Range.intersections cuboid1.YRange cuboid2.YRange
Range.intersections cuboid1.ZRange cuboid2.ZRange

let intersections = Cuboid.intersectionSplit cuboid1 cuboid2

let overlap = intersections |> List.filter (fun cuboid -> not <| Cuboid.intersects cuboid cuboid1)

cuboid2 |> Cuboid.split cuboid1


// turnedOn is a list of non overlapping cuboids that are already turned on
// recursion?

let turnOn turnedOn cuboid =
    // printfn ">>>>>>>>> turnOn:"
    // printfn ">>>>>>>>> turnedOn: %O" turnedOn
    let turnedOnIntersects = turnedOn |> List.filter (Cuboid.intersects cuboid)
    let newTurnedOn =
        turnedOnIntersects
        |> List.fold (fun parts intersectingCuboid -> 
            // printfn ">>> intersectingCuboid: %O" intersectingCuboid
            // printfn ">>> parts: %O" parts
            let result = 
                parts
                |> List.fold (fun currentParts cuboidPart ->
                    // printfn "currentParts: %O" currentParts
                    // printfn "split %O on %O" cuboidPart intersectingCuboid
                    let newParts = cuboidPart |> Cuboid.split intersectingCuboid |> Set.ofList
                    
                    let result = currentParts |> Set.remove cuboidPart |> Set.union newParts
                    // printfn "result: %O" result
                    result
                ) (parts |> Set.ofList)
            // printfn "new state is: %O" result
            result |> Set.toList
        ) [cuboid]

    // printfn ">>>>>>>>> turnOn <<<<<<<<<"
    // printfn ">>>>>>>>> result: %O" (turnedOn @ newTurnedOn)
    turnedOn @ newTurnedOn

let turnOff turnedOn cuboid = 
    let newTurnedOn =
        turnedOn
        |> List.fold (fun stillTurnedOn intersectingCuboid -> 
            let stillOn = intersectingCuboid |> Cuboid.split cuboid
                
            stillTurnedOn @ stillOn
        ) []
    newTurnedOn

let cube1 = { XRange = 0,3; YRange = 0,2; ZRange = 0,0 }
let cube2 = { XRange = 1,2; YRange = 2,3; ZRange = 0,0 }
let cube3 = { XRange = 2,4; YRange = 2,4; ZRange = 0,0 }
let cube4 = { XRange = 0,8; YRange = 0,8; ZRange = 0,0 }

let after1 = turnOn [cube1] cube2
let after2 = turnOn after1 cube3
let after3 = turnOn after2 cube4
after3 |> List.sumBy Cuboid.count


// on x=10..12,y=10..12,z=10..12
// on x=11..13,y=11..13,z=11..13

let cuboidX = { XRange = 10,12; YRange = 10,12; ZRange = 10,12 }
let cuboidY = { XRange = 11,13; YRange = 11,13; ZRange = 11,13 }
let cuboidZ = { XRange = 9,11; YRange = 9,11; ZRange = 9,11 }

let xUnionY = cuboidY |> Cuboid.split cuboidX



let turnedOn = turnOn [cuboidX] cuboidY

turnedOn |> List.sumBy Cuboid.count

let result = turnOff turnedOn cuboidZ

result |> List.sumBy Cuboid.count

turnOn result { XRange = 10,10; YRange = 10,10; ZRange = 10,10 } |> List.sumBy Cuboid.count


let init turnedOn rebootStep =
    let turnOnOperation, cuboid = rebootStep
    if turnOnOperation then
        turnOn turnedOn cuboid 
    else
        turnOff turnedOn cuboid

let initAll rebootSteps =
    rebootSteps
    |> Seq.fold init []

let exampleSteps = parse example3
let firstStarExampleCount = 
    applyAll exampleSteps
    |> Seq.length

// on x=-5..47,y=-31..22,z=-19..33
// on x=-44..5,y=-27..21,z=-14..35

// range1 
//        -5       47
//        |--------|
//  -44      5
//  |---/----|

//  | (r1start, r1end), (r2start, r2end)  when r2start < r1start && r2end < r1end -> 
//             printfn "case 5: %O" [(r2start, r1end-1); (r1end, r2end)]
//             [(r2start, r1end-1); (r1end, r2end)]

Range.intersections (-5, 47) (-44, 5)

initAll exampleSteps |> List.sumBy Cuboid.count

let example3Steps = parse example3
initAll example3Steps |> List.sumBy Cuboid.count


let xx = exampleSteps |> List.map snd

xx.[1] |> Cuboid.intersectionSplit xx.[0]

let zIntersections = Range.intersections xx.[1].ZRange xx.[0].ZRange

let secondStar = 
    parse input
    |> initAll
    |> List.sumBy Cuboid.count

secondStar

