// --- Day 5: Hydrothermal Venture ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day05.txt") |> List.ofSeq

let example = [
    "0,9 -> 5,9"
    "8,0 -> 0,8"
    "9,4 -> 3,4"
    "2,2 -> 2,1"
    "7,0 -> 7,4"
    "6,4 -> 2,0"
    "0,9 -> 2,9"
    "3,4 -> 1,4"
    "0,0 -> 8,8"
    "5,5 -> 8,2"
]

let parse lines = 
    lines
    |> Seq.map (fun (line:string) ->
        let parts = line.Split(" -> ")
        let startPart, stopPart = parts.[0], parts.[1]
        let splitCoordinates (part:string) =
            let parts = part.Split(",");
            parts.[0] |> System.Int32.Parse, parts.[1] |> System.Int32.Parse
        splitCoordinates startPart, splitCoordinates stopPart
    )
    |> Seq.toList

let swap pair =
    let first, second = pair
    second, first

let sortLowestXFirst points =
    let start, stop = points
    let (x1, _), (x2, _) = start, stop
    if x1 > x2 then
        swap points
    else
        points

let sortLowestYFirst points =
    let start, stop = points
    let (_, y1), (_, y2) = start, stop
    if y1 > y2 then
        swap points
    else
        points

let isHorizontalLine points = 
    let (_, y1), (_, y2) = points
    y1 = y2

let isVerticalLine points =
    let (x1, _), (x2, _) = points
    x1 = x2

let isStraightLine points = isHorizontalLine points || isVerticalLine points

let isDiagonal points =
    let (x1, y1), (x2, y2) = points
    abs (x2 - x1) = abs (y2 - y1)

let isDiagonalPositive points =
    if isDiagonal points then
        let start, stop = sortLowestXFirst points
        let (_, y1), (_, y2) = start, stop
        y2 > y1
    else
        false

let isDiagonalNegative (points:(int*int)*(int*int)) =
    isDiagonal points && not (isDiagonalPositive points)
    
let isStraightOrDiagonal points = points |> isStraightLine || points |> isDiagonal

let sortPoints points = 
    let start, stop = points
    let (x1, y1), (x2, y2) = start, stop

    if isDiagonalPositive points || isDiagonalNegative points then
        if x1 > x2 then
            stop, start
        else 
            points
    else if x1 = x2 then
        if y1 > y2 then
            (stop, start)
        else 
            points
    else if y1 = y2 then
        if x1 > x2 then
            stop, start
        else 
            points
    else
        points

let points points =
    let (x1, y1), (x2, y2) = points

    if x1 = x2 then
        seq {            
            for y in y1..y2 do
                yield x1, y
        }
    else if y1 = y2 then

        seq {
            for x in x1..x2 do
                yield x, y1
        }
    else 
        failwithf "not a straight line"
    |> Seq.toList

let isMany (point, count) = count > 1
    
    
let firstStar =
    let lines = parse input
    let x = 
        lines 
        |> Seq.filter isStraightLine 
        |> Seq.map sortPoints
        |> Seq.map points 
        |> Seq.collect id
        |> Seq.countBy id
        |> Seq.toList
        |> Seq.filter isMany
        |> Seq.length
    x

// 4369 is wrong (too low) x/y bug in sort

firstStar

let points2 points =    
    if isDiagonalPositive points then
        let (x1, y1), (x2, y2) = sortLowestXFirst points
        // 2,0 6,4 -> 2,0, 3,1, 4, 2
        printfn ">>> diag pos x1 = %i, y1 = %i, x2 = %i, y2 = %i" x1 y1 x2 y2
        seq {
            for x in x1..x2 do
                yield x, y1 + (x - x1)
        }
    else if isDiagonalNegative points then
        printfn ">>> diag neg"
        let (x1, y1), (x2, _) = sortLowestXFirst points
        seq {
            for x in x1..x2 do
                yield x, y1 - (x - x1)
        }
    else if isVerticalLine points then
        printfn ">>> vertical line"
        let (x1, y1), (_, y2) = sortLowestYFirst points
        seq {            
            for y in y1..y2 do
                yield x1, y
        }
    else if isHorizontalLine points then
        let (x1, y1), (x2, _) = sortLowestXFirst points
        seq {
            for x in x1..x2 do
                yield x, y1
        }
    else
        failwithf "Not a straight line"
        
    |> Seq.toList

let print (points:((int*int)*int) seq) =

    let lookup = points |> dict

    for y in 0..9 do
        for x in 0..9 do
            if lookup.ContainsKey((x,y)) then
                printf "%i" lookup.[(x,y)]
            else 
                printf "."
        printfn ""

(0,9),(5,9)
(8,0),(0,8)
(9,4),(3,4)
(2,2),(2,1)
(7,0),(7,4)
(6,4),(2,0)
(0,9),(2,9)
(3,4),(1,4)
(0,0),(8,8)
(5,5),(8,2)

sortLowestYFirst ((2,2),(2,1))
isVerticalLine ((2,2),(2,1))
let exampleLine = points2 ((6,4),(2,0))
let examplePoints = exampleLine |> Seq.countBy id
print examplePoints

let secondStar = 

    let lines = parse input

    let answer = 
        lines 
        |> Seq.filter isStraightOrDiagonal     
        |> Seq.map points2 
        |> Seq.collect id
        |> Seq.countBy id
        |> Seq.filter isMany
        |> Seq.toList
        |> Seq.length

    answer

firstStar
secondStar

