module AoC.E2023.Day03

// --- Day 3: Gear Ratios ---

open AoC
open IO

open System.Text.RegularExpressions

let input = readInputLines "2023" "Day03" |> List.ofSeq


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

    let number partNumber = partNumber.Number
    let create pos (value: string) =
        
        let space = seq { 
            let start = pos |> fst
            for i in start..start+value.Length-1 do
                yield i, pos |> snd
        }
        
        { Positions = space |> Seq.toList; Value = value; Number = value |> int }

type Gear = { Part1: PartNumber; Part2: PartNumber}

module Gear =
    let ratio gear = gear.Part1.Number * gear.Part2.Number

type EngineSchematic = { PartNumbers: list<PartNumber>; Symbols: list<Symbol> }

module EngineSchematic = 
    let overlaps partNumber (symbol: Symbol) =
        symbol.Adjacent
        |> Seq.exists(fun pos -> 
            partNumber.Positions
            |> Seq.exists (fun p -> p = pos))
    let isValidPartNumber schematic partNumber =
        schematic.Symbols 
        |> Seq.exists (overlaps partNumber)
    let partNumbers schematic = 
        schematic.PartNumbers
        |> List.filter (isValidPartNumber schematic)
    let withOverlappingParts schematic symbol =
        symbol, 
        schematic.PartNumbers 
        |> List.filter (fun partNumber -> overlaps partNumber symbol)
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


let firstStar () =
    input
    |> parse
    |> EngineSchematic.partNumbers
    |> List.map PartNumber.number
    |> List.sum

let secondStar () = 
    input
    |> parse
    |> EngineSchematic.gears
    |> List.map Gear.ratio
    |> List.sum