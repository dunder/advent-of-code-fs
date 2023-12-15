// --- Day 3: Gear Ratios ---

open System.IO
open System.Text.RegularExpressions

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day03.txt") |> List.ofSeq

let example = [
    "467..114.."
    "...*......"
    "..35..633."
    "......#..."
    "617*......"
    ".....+.58."
    "..592....."
    "......755."
    "...$.*...."
    ".664.598.."
]

let adjacent pos =
    let x, y = pos
    [
        yield x, y-1
        yield x+1, y-1
        yield x+1, y
        yield x+1, y+1
        yield x, y+1
        yield x-1, y+1
        yield x-1, y
        yield x-1, y-1
    ]

type Symbol = { Position: int*int; Type: char; Adjacent: list<int*int> }

module Symbol =
    let create pos t = { Position = pos; Type = t; Adjacent = adjacent pos }

type PartNumber = { Positions: list<int*int>; Value: string; Number: int }

module PartNumber = 

    let number partNumber = partNumber.Value |> int

    let create pos (value: string) =
        
        let space = seq { 
            let start = pos |> fst
            for i in start..start+value.Length-1 do
                yield i, pos |> snd
        }
        
        { Positions = space |> Seq.toList; Value = value; Number = value |> int }

type Gear = { Part1: PartNumber; Part2: PartNumber}

module Gear =
    let ratio gear = (gear.Part1 |> PartNumber.number) * (gear.Part2 |> PartNumber.number)

type EngineSchematic = { PartNumbers: list<PartNumber>; Symbols: list<Symbol> }

module EngineSchematic = 



    let overlaps partNumber (symbol: Symbol) =
        symbol.Position 
        |> adjacent 
        |> Seq.exists(fun pos -> 
            partNumber.Positions
            |> Seq.exists (fun p -> p = pos))

    let isValidPartNumber schematic partNumber =
        schematic.Symbols 
        |> Seq.exists (overlaps partNumber)

    let partNumbers schematic = 
        schematic.PartNumbers
        |> List.filter (isValidPartNumber schematic)

    let overlappingParts schematic symbol = 
        schematic.PartNumbers
        |> List.filter (fun partNumber -> overlaps partNumber symbol)

    let withOverlappingParts schematic symbol =
        symbol, overlappingParts schematic symbol

    let gears schematic =

        let toGear symbolWithParts =
            let symbol, parts = symbolWithParts
            match symbol.Type, parts with 
            | '*', [part1; part2] -> Some({ Part1 = part1; Part2 = part2})
            | _ -> None

        schematic.Symbols 
        |> List.map ((withOverlappingParts schematic) >> toGear)
        |> List.choose (id)

let parse (input: list<string>) =

    let partsRegex = new Regex(@"\d+")

    let parseParts i (line: string) =
        partsRegex.Matches line
        |> Seq.map (fun m -> PartNumber.create (m.Index, i) m.Value)
        
    let symbolRegex = new Regex(@"[^0-9\.]")
    
    let parseSymbols i (line: string) =
        symbolRegex.Matches line
        |> Seq.map (fun m -> 
            let pos = m.Index, i
            Symbol.create pos m.Value[0])
            

    let parts = input |> List.mapi parseParts |> Seq.collect (id) |> Seq.toList
    let symbols = input |> List.mapi parseSymbols |> Seq.collect (id) |> Seq.toList

    { PartNumbers = parts; Symbols = symbols }

parse example



let p = PartNumber.create (6,2) "633"
let s = Symbol.create (6,3) '#' 

let exampleSchematic = example |> parse

s.Position |> adjacent |> List.ofSeq


EngineSchematic.overlaps p s

EngineSchematic.isValidPartNumber exampleSchematic (PartNumber.create (6,2) "633")

example
|> parse
|> EngineSchematic.partNumbers
|> List.map PartNumber.number
|> List.sum

example
|> parse
|> EngineSchematic.gears
|> List.map Gear.ratio
|> List.sum

let firstStar =
    input |> parse
    |> EngineSchematic.partNumbers
    |> List.map PartNumber.number
    |> List.sum
    
    

firstStar

let secondStar = 
    input
    |> parse
    |> EngineSchematic.gears
    |> List.map Gear.ratio
    |> List.sum


secondStar

