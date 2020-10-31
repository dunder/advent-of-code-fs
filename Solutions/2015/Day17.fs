module AoC.E2015.Day17

open AoC
open Combinatorics
open IO


// --- Day 17: No Such Thing as Too Much ---

let input = readInputLines "2015" "Day17"
 
let firstStar () =
    let containers = input |> Seq.map int |> List.ofSeq
    
    let summedUp = containers |> List.sort |> List.scan (+) 0
    let max = (summedUp |> List.findIndex (fun x -> x > 150))
    
    let summedUpDesc = containers |> List.sortDescending |> List.scan (+) 0
    let min = (summedUpDesc |> List.findIndex (fun x -> x > 150))

    [min..max] 
    |> List.map (fun n -> combinations n containers)
    |> List.collect (fun combo -> combo)
    |> List.filter (fun combo -> combo |> List.sum = 150)
    |> List.length

let secondStar () = 
    let containers = input |> Seq.map int |> List.ofSeq
    
    let summedUp = containers |> List.sort |> List.scan (+) 0
    let max = (summedUp |> List.findIndex (fun x -> x > 150))
    
    let summedUpDesc = containers |> List.sortDescending |> List.scan (+) 0
    let min = (summedUpDesc |> List.findIndex (fun x -> x > 150))

    let xs = 
        [min..max] 
        |> List.map (fun n -> combinations n containers)
        |> List.collect (fun combo -> combo)
        |> List.filter (fun combo -> combo |> List.sum = 150)
        |> List.map (fun combo -> (List.length combo, combo))
        |> List.sortBy (fun item -> fst item)

    let min = fst xs.Head

    xs 
    |> List.filter (fun x -> fst x = min)
    |> List.length

module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =

        Assert.Equal(4372, firstStar())

    [<Fact>]
    let ``second star`` () =

        Assert.Equal(4, secondStar())