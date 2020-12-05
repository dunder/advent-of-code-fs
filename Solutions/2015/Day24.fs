module AoC.E2015.Day24

open AoC
open IO
open Combinatorics

// --- Day 24: It Hangs in the Balance ---

let input = readInputLines "2015" "Day24" |> List.ofSeq

let parse lines = lines |> List.map int64

let quantumEntanglement packages =
    packages |> Seq.fold (*) 1L

let idealConfiguration compartments packages = 
    let targetWeight = (packages |> Seq.sum) / (compartments |> int64)
    [2..input.Length] 
    |> Seq.map (fun n -> 
        packages |> combinations n |> Seq.filter (fun x -> x |> Seq.sum = targetWeight)
    )
    |> Seq.find (fun cs -> not (cs |> Seq.isEmpty))
    |> Seq.sortBy (fun gs -> quantumEntanglement gs)
    |> Seq.head
    

let firstStar () =
    parse input
    |> idealConfiguration 3
    |> quantumEntanglement 

let secondStar () = 
    parse input
    |> idealConfiguration 4
    |> quantumEntanglement 


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(11846773891L, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(80393059L, secondStar())

    [<Fact>]
    let ``first star example`` () =

        let result =
            [1L..5L] @ [7L..11L]
            |> idealConfiguration 3
            |> quantumEntanglement 

        Assert.Equal(99L, result)
