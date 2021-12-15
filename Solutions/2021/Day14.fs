module AoC.E2021.Day14

// >>> insert day tagline here <<<

open AoC
open IO


let input = readInputLines "2021" "Day14" |> List.ofSeq


let parseTemplate (lines:string list) = lines |> List.head |> List.ofSeq

let parseReplacements (lines:string list) =
    lines
    |> List.skip 2
    |> List.map (fun line ->
        let parts = line.Split(" -> ")
        (parts.[0].[0], parts.[0].[1]), parts.[1] |> char
    )

let buildReplacementMap (replacements: list<(char*char)*char>) = 
    replacements
    |> List.map (fun (pair, element) -> pair, [fst pair, element; element, snd pair])
    |> Map.ofList

let nextPairs (replacementMap:Map<char*char,list<char*char>>) (counters:list<(char*char)*int64>)  =
    
    let newPairCounts =
        counters 
        |> List.map (fun (pair, count) -> replacementMap.[pair] |> List.map (fun pair -> pair, count))
    newPairCounts

let nextCounters (pairCounts: ((char*char) * int64) list list) =
    pairCounts
    |> List.collect id
    |> List.groupBy fst
    |> List.map (fun (pair, pairCounts) -> pair, pairCounts |> List.map snd |> List.sum)
    
let nextStep (replacementMap:Map<char*char,list<char*char>>) (pairsIn: ((char * char) * int64) list list) =
    let counters = nextCounters pairsIn
    nextPairs replacementMap counters 

let countLetters (template: char list) (pairCounts: ((char * char) * int64) list list) =
    let letterCount =
        pairCounts
        |> List.map (fun pairCount -> 
            let leftPair = pairCount.[0]
            let rightPair = pairCount.[1]
            [fst (fst leftPair), snd leftPair; fst (fst rightPair), snd rightPair] )
        |> List.collect id
        |> List.append [template |> List.last, 1L]
        |> List.groupBy fst
        |> List.map (fun (letter, counts) -> letter, counts |> List.map snd |> List.sum)
        
    let max = letterCount |> List.maxBy snd |> snd
    let min = letterCount |> List.minBy snd |> snd
    max - min
        
let run data steps =
    let template = parseTemplate data
    let replacements = parseReplacements data
    let replacementMap = replacements |> buildReplacementMap
    let templatePairs = template |> List.pairwise
    let templatePairCount = templatePairs |> List.countBy id |> List.map ( fun (pair, count) -> pair, count |> int64)
    let nextTemplatePairs = templatePairCount |> nextPairs replacementMap
    
    Seq.unfold (fun pairs ->
        let nextPairs = nextStep  replacementMap pairs
        Some (pairs, nextPairs)
        ) nextTemplatePairs
    |> Seq.skip (steps - 1)
    |> Seq.take 1
    |> Seq.exactlyOne
    |> countLetters template

let firstStar () =
    run input 10

let secondStar () = 
    run input 40
