// --- Day 3: Binary Diagnostic ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day03.txt") |> List.ofSeq

let example = [
    "00100"
    "11110"
    "10110"
    "10111"
    "10101"
    "01111"
    "00111"
    "11100"
    "10000"
    "11001"
    "00010"
    "01010"
]

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

let firstStar =
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

firstStar

let oxygenCriteria (lines:list<string>) (columnIndex:int) =
    let column = 
        lines 
        |> columnOrder 
    
    let columnCounts =
        column.[columnIndex]
        |> List.countBy id
        |> List.sortByDescending snd

    let criteria =
        if columnCounts |> Seq.length = 1 then
            columnCounts 
            |> Seq.exactlyOne 
            |> fst
        else 
            if snd columnCounts.[0] = snd columnCounts.[1] then
                '1'
            else
                columnCounts |> List.head |> fst
    lines |> List.filter (fun line -> line.[columnIndex] = criteria)


let co2ScrubberCriteria (lines:list<string>) (columnIndex:int) =
    let column = 
        lines 
        |> columnOrder 
    
    let columnCounts =
        column.[columnIndex]
        |> List.countBy id
        |> List.sortBy snd

    let criteria =
        if columnCounts |> Seq.length = 1 then
            columnCounts 
            |> Seq.exactlyOne 
            |> fst
        else
            if snd columnCounts.[0] = snd columnCounts.[1] then
                '0'
            else
                columnCounts |> List.head |> fst
    
    lines |> List.filter (fun line -> line.[columnIndex] = criteria)

let secondStar = 
    let data = input
    let width = data |> Seq.head |> Seq.length
    
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