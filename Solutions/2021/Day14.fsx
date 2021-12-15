// --- Day 14: Extended Polymerization ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day14.txt") |> List.ofSeq

let example = [
    "NNCB"
    ""
    "CH -> B"
    "HH -> N"
    "CB -> H"
    "NH -> C"
    "HB -> C"
    "HC -> B"
    "HN -> C"
    "NN -> C"
    "BH -> H"
    "NC -> B"
    "NB -> B"
    "BN -> B"
    "BB -> N"
    "BC -> B"
    "CC -> N"
    "CN -> C"
]

let parseTemplate (lines:string list) = lines |> List.head |> List.ofSeq

let parseReplacements (lines:string list) =
    lines
    |> List.skip 2
    |> List.map (fun line ->
        let parts = line.Split(" -> ")
        (parts.[0].[0], parts.[0].[1]), parts.[1] |> char
    )

let replace replacment insertion = [fst replacment; insertion]
let leave replacement = [fst replacement; snd replacement]

let processReplacments replacementMap polymer =
    let last = polymer |> List.last
    let pairs = polymer |> List.pairwise
    let count = pairs.Length
    pairs
    |> List.mapi (fun i pair -> 
        if replacementMap |> Map.containsKey pair then
            let insertion = replacementMap.[pair]
            let result = replace pair insertion
            let result = 
                if i = count - 1 then
                    result @ [last]
                else 
                    result

            result
        else    
            leave pair
    )
    |> List.collect id

let run data steps = 
    let template = parseTemplate data
    let replacements = parseReplacements data
    let replacementMap = replacements |> Map.ofSeq

    Seq.unfold (fun polymer ->
        let nextPolymer = processReplacments replacementMap polymer
        Some (polymer, nextPolymer)
    ) template
    |> Seq.skip steps
    |> Seq.take 1
    |> Seq.collect id
    |> List.ofSeq


let countPolymer polymer =
    let counts = 
        polymer
        |> List.countBy id
        |> List.map snd
        |> List.sort
    (counts |> List.last) - counts.Head

run example 10 |> Array.ofList |> System.String

let firstStar =
    run input 10 |> countPolymer

firstStar

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
        

let secondStar = 
    let data = input
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
    |> Seq.skip 39
    |> Seq.take 1
    |> Seq.exactlyOne
    |> countLetters template

secondStar