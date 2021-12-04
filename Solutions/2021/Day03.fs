module AoC.E2021.Day03

// --- Day 3: Binary Diagnostic ---

open AoC
open IO

let input = readInputLines "2021" "Day03" |> List.ofSeq

let countColumns input =
    input
    |> Seq.head
    |> String.length

let columnOrder (data: string list) =
    let rows = Seq.length data
    let columns = countColumns data
    seq {
        for column in 0..columns-1 do
            yield seq {
                for row in 0..rows-1 do
                    yield data.[row].[column]
            } |> List.ofSeq
    }
    |> List.ofSeq

let firstStar() =
    let data = input
    let columns =
        data
        |> columnOrder
    
    let commonBits = 
        columns
        |> List.map (fun column ->
            column 
            |> List.countBy id
            |> List.sortByDescending snd
            |> List.head
        )
        |> List.map fst

    let uncommonBits =
        columns
        |> List.map (fun column ->
            column 
            |> List.countBy id
            |> List.sortBy snd
            |> List.head
        )
        |> List.map fst

    let toBinary s = System.Convert.ToInt32(s, 2);
        
    let gammaRate = commonBits |> Array.ofSeq |> System.String |> toBinary
    let epsilonRate = uncommonBits |> Array.ofSeq |> System.String |> toBinary

    gammaRate * epsilonRate

let criteria resultOnTie (sort:(char*int->int)->(char*int) list->(char*int) list) (lines:list<string>) (columnIndex:int) =
    let column = 
        lines 
        |> columnOrder 
    
    let columnCounts =
        column.[columnIndex]
        |> List.countBy id
        |> sort snd

    let criteria =
        if columnCounts |> Seq.length = 1 then
            columnCounts 
            |> Seq.exactlyOne 
            |> fst
        else 
            if snd columnCounts.[0] = snd columnCounts.[1] then
                resultOnTie
            else
                columnCounts |> List.head |> fst
    lines |> List.filter (fun line -> line.[columnIndex] = criteria)

let oxygenCriteria = criteria '1' List.sortByDescending

let co2ScrubberCriteria = criteria '0' List.sortBy

let secondStar() = 
    let data = input
    
    let rec evaluateOxygen lines column =
        if lines |> Seq.length = 1 then
            lines
        else
            let nextLines = oxygenCriteria lines column
            evaluateOxygen nextLines (column + 1)

    let toBinary s = System.Convert.ToInt32(s, 2);

    let oxygenResult = evaluateOxygen data 0 |> Seq.head |> toBinary

    let rec evaluateco2Scrubber lines column =
        if lines |> Seq.length = 1 then
            lines
        else
            let nextLines = co2ScrubberCriteria lines column
            evaluateco2Scrubber nextLines (column + 1)
    
    let co2ScrubberResult = evaluateco2Scrubber data 0 |> Seq.head |> toBinary

    oxygenResult * co2ScrubberResult