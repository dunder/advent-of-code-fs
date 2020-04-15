module AoC.E2015.Day13

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
let buildMap hapiness =
    hapiness
    |> Seq.groupBy (fun (a, _, _) -> a)
    |> Seq.map (fun (a, group) -> (a, group |> Seq.map (fun (_, b, c) -> (b, c)) |> dict))
    |> dict

let addMe happiness = 
    let guests = happiness |> Seq.map (fun (a, _, _) -> a) |> Seq.distinct 
    let meWithGuests = guests |> Seq.map (fun g -> ("me", g, 0)) |> List.ofSeq
    let guestsWithMe = guests |> Seq.map (fun g -> (g, "me", 0)) |> List.ofSeq
    List.ofSeq happiness @ meWithGuests @ guestsWithMe

let optimalHapiness hapiness =
    let happinessMap = buildMap hapiness

    permutations (List.ofSeq happinessMap.Keys)
    |> Seq.map (fun xs -> 
        let a = Array.ofSeq xs
        let first = a.[0]
        let last = a.[Array.length a - 1]
        (xs 
        |> Seq.pairwise 
        |> Seq.map (fun (a,b) -> happinessMap.[a].[b] + happinessMap.[b].[a]) 
        |> Seq.sum) + 
        happinessMap.[first].[last] + 
        happinessMap.[last].[first]
    )
    |> Seq.sortDescending
    |> Seq.head

let firstStar () =
    let hapiness = parse input
    optimalHapiness hapiness

let secondStar () = 
    let hapiness = addMe (parse input)
    optimalHapiness hapiness


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
        let hapiness = optimalHapiness (parse input)
        Assert.Equal(330, hapiness)
