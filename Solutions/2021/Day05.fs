module AoC.E2021.Day05

// --- Day 5: Hydrothermal Venture ---

open AoC
open IO


let input = readInputLines "2021" "Day05" |> List.ofSeq


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

let sortByX points =
    let start, stop = points
    let (x1, _), (x2, _) = start, stop
    if x1 > x2 then
        swap points
    else
        points

let sortByY points =
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
        let start, stop = sortByX points
        let (_, y1), (_, y2) = start, stop
        y2 > y1
    else
        false

let isDiagonalNegative points = isDiagonal points && not (isDiagonalPositive points)
    
let isStraightOrDiagonal points = isStraightLine points || isDiagonal points

let isMany (point, count) = count > 1

let points points =
    if isDiagonalPositive points then
        let (x1, y1), (x2, y2) = sortByX points
        seq {
            for x in x1..x2 do
                yield x, y1 + (x - x1)
        }
    else if isDiagonalNegative points then
        let (x1, y1), (x2, _) = sortByX points
        seq {
            for x in x1..x2 do
                yield x, y1 - (x - x1)
        }
    else if isVerticalLine points then
        let (x1, y1), (_, y2) = sortByY points
        seq {            
            for y in y1..y2 do
                yield x1, y
        }
    else if isHorizontalLine points then
        let (x1, y1), (x2, _) = sortByX points
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

   
let firstStar () =
    let lines = parse input
    let answer =
        lines
        |> Seq.filter isStraightLine
        |> Seq.map points 
        |> Seq.collect id
        |> Seq.countBy id
        |> Seq.filter isMany
        |> Seq.length
    answer

let secondStar () = 
    let lines = parse input

    let answer =
        lines
        |> Seq.filter isStraightOrDiagonal
        |> Seq.map points
        |> Seq.collect id
        |> Seq.countBy id
        |> Seq.filter isMany
        |> Seq.length

    answer
