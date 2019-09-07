module AoC.E2015.Day02

open Aoc
open IO
open System

// --- Day 2: I Was Told There Would Be No Math ---

let input = readInputLines "2015" "Day02"

type Present = { W: int; H: int; L: int} with
    member x.Sides = [(x.W,x.H);(x.H,x.L);(x.W,x.L)]
    member x.Faces = x.Sides @ x.Sides
    member x.Perimiters = x.Sides |> Seq.map (fun (w,h) -> w*h)
    member x.Area = x.Faces |> Seq.fold (fun acc (w,h) -> acc + w*h) 0
    member x.Volume = x.L * x.W * x.H
    member x.Slack = x.Perimiters |> Seq.min
    member x.Ribbon = [2*(x.W+x.H);2*(x.H+x.L);2*(x.W+x.L)] |> Seq.min
    member x.Bow = x.Volume

let parse (input : seq<String>) =
    input
    |> Seq.map (fun line -> line.Split('x') |> Seq.map (fun value -> value |> int) |> Seq.toArray)
    |> Seq.map (fun dimensions -> { W = dimensions.[0]; H = dimensions.[1]; L = dimensions.[2] })
    |> Seq.toList

let totalPaperDemand (input : seq<String>) =
    let presents = parse input
    presents 
    |> List.fold (fun acc present -> acc + present.Area + present.Slack) 0

let totalRibbonDemand (input : seq<String>) =
    let presents = parse input
    presents
    |> List.fold (fun acc present -> acc + present.Ribbon + present.Bow) 0
    
let firstStar () =
    totalPaperDemand input

let secondStar () = 
    totalRibbonDemand input


module Tests =

    open Xunit

    [<Theory>]
    [<InlineData(2,3,4,10,24)>]
    let ``ribbon`` w h l expectedRibbon expectedBow =
        
        let present = { W=w; H=h; L=l}
        let ribbon = present.Ribbon
        let bow = present.Bow

        Assert.Equal(expectedRibbon, ribbon)
        Assert.Equal(expectedBow, bow)

    [<Fact>]
    let ``first star`` () =
        let paperArea = totalPaperDemand input

        Assert.Equal(1588178, paperArea)

    [<Fact>]
    let ``second star`` () =
        let ribbon = totalRibbonDemand input

        Assert.Equal(3783758, ribbon)
