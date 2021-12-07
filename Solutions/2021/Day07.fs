module AoC.E2021.Day07

// >>> insert day tagline here <<<

open AoC
open IO


let input = readInputText "2021" "Day07"

let parse (text:string) = 
    text.Split(",")
    |> Array.map System.Int32.Parse
    |> Array.toList

let distance a b = abs (a-b)

let totalFuelCost positions target = 
    positions 
    |> Seq.map (fun position -> distance position target) 
    |> Seq.sum

let firstStar() =
    let data = input
    let positions = parse data
    
    seq {
            for x in 1..System.Int32.MaxValue do
                yield totalFuelCost positions x
    }
    |> Seq.take 1000
    |> Seq.min

// - part 2

let fuelCost a b =
    let d = distance a b
    [0..d]
    |> Seq.reduce (fun distance x -> distance + x)

let totalFuelCost2 positions target = 
    positions 
    |> Seq.map (fun position -> fuelCost position target) 
    |> Seq.sum

let secondStar() = 
    let data = input
    let positions = parse data

    seq {
            for x in 1..System.Int32.MaxValue do
                yield totalFuelCost2 positions x
    }
    |> Seq.take 1000
    |> Seq.min