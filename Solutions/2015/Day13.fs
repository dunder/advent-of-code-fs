module AoC.E2015.Day13

open System.Collections.Generic
open System.Text.RegularExpressions

open AoC
open IO
open Combinatorics

// --- Day 13: Knights of the Dinner Table ---

let input = readInputLines "2015" "Day13"

// active pattern, captured groups are returned in a list option
let (|Regex|_|) pattern input = 
    let m = Regex.Match(input, pattern)

    if m.Success
    then Some(List.tail [for g in m.Groups -> g.Value])
    else None

let parse lines = 
    lines
    |> Seq.map (fun line ->
        match line with
        | Regex "([A-Z][a-z]+) would gain (\d+) happiness units by sitting next to ([A-Z][a-z]+)." 
            [name; hapiness; neighbor] -> (name, neighbor, hapiness |> int)
        | Regex "([A-Z][a-z]+) would lose (\d+) happiness units by sitting next to ([A-Z][a-z]+)."
            [name; hapiness; neighbor] -> (name, neighbor, -(hapiness |> int))
        | x -> failwithf  "Unexpected format in line %s" x
    )

// build a dict of dict from a tuple of three (a,b,c) where
// a will be the key in the first dict and its value ia another dict where
// b will be the key and c the value
let buildMap tuplesOfThree =
    tuplesOfThree
    |> Seq.groupBy (fun (a, _, _) -> a)
    |> Seq.map (fun (a, group) -> (a, group |> Seq.map (fun (_, b, c) -> (b, c)) |> dict))
    |> dict

let optimalHapiness input =
    let specs = parse input
    let hapinessMap = buildMap specs

    let permutations = permutations <| List.ofSeq hapinessMap.Keys
    permutations
    |> Seq.map (fun xs -> 
        let a = Array.ofSeq xs
        let first = a.[0]
        let last = a.[Array.length a - 1]
        (xs 
        |> Seq.pairwise 
        |> Seq.map (fun (a,b) -> hapinessMap.[a].[b] + hapinessMap.[b].[a]) 
        |> Seq.sum) + 
        hapinessMap.[first].[last] + 
        hapinessMap.[last].[first]
    )
    |> Seq.sortDescending
    |> List.ofSeq
    |> List.head
    

let firstStar () =
    optimalHapiness input

let secondStar () = 
    0


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =

        Assert.Equal(709, firstStar())

    [<Fact>]
    let ``second star`` () =

        Assert.Equal(668, secondStar())

    [<Fact>]
    let ``runner`` () =
        let input = [
            "Alice would gain 54 happiness units by sitting next to Bob."
            "Alice would lose 79 happiness units by sitting next to Carol."
            "Alice would lose 2 happiness units by sitting next to David."
            "Bob would gain 83 happiness units by sitting next to Alice."
            "Bob would lose 7 happiness units by sitting next to Carol."
            "Bob would lose 63 happiness units by sitting next to David."
            "Carol would lose 62 happiness units by sitting next to Alice."
            "Carol would gain 60 happiness units by sitting next to Bob."
            "Carol would gain 55 happiness units by sitting next to David."
            "David would gain 46 happiness units by sitting next to Alice."
            "David would lose 7 happiness units by sitting next to Bob."
            "David would gain 41 happiness units by sitting next to Carol."
        ]
        let hapiness = optimalHapiness input
        Assert.Equal(330, hapiness)
