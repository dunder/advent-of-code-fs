module AoC.E2020.Day07

open AoC
open IO
open System.Text.RegularExpressions

// --- Day 7: Handy Haversacks ---

let input = readInputLines "2020" "Day07" |> List.ofSeq

// Example 1: light red bags contain 1 bright white bag, 2 muted yellow bags.
// Example 2: bright white bags contain 1 shiny gold bag.
// Example 3: faded blue bags contain no other bags.

let parse lines = 
    lines
    |> Seq.map (fun (line:string) ->
        let bagAndContent = line.Split(" contain ")
        let bag = bagAndContent.[0].Replace(" bags", "")
        let content = bagAndContent.[1]
        if content = "no other bags." then
            (bag, [])
        else
            let bags = content.Split(", ") |> Seq.map (fun b -> 
                let m = Regex.Match(b, "(\d) (.*) bags?\.?")
                let count = m.Groups.[1].Value |> int
                let color = m.Groups.[2].Value
                (color, count)
            )
            (bag, bags |> List.ofSeq)
    )

let directlyContainsBag  (bagSearchedFor: string) (currentBagConsidered: string) (bagMap: Map<string, list<string*int>>) =
    bagMap.[currentBagConsidered] |> Seq.exists (fun c -> (c |> fst) = bagSearchedFor)

let contains (bagSearchedFor: string) (currentBagConsidered: string) (bagMap: Map<string, list<string*int>>) =

    let rec loop (bagSearchedFor: string) (currentBagConsidered: string) (bagMap: Map<string, list<string*int>>) (searched: Set<string>) =
        if searched |> Set.contains currentBagConsidered then
            false
        elif directlyContainsBag bagSearchedFor currentBagConsidered bagMap then
            true
        else 
            let contained = bagMap.[currentBagConsidered] |> Seq.map fst
            contained |> Seq.exists (fun c -> loop bagSearchedFor c bagMap (searched |> Set.add currentBagConsidered))

    loop bagSearchedFor currentBagConsidered bagMap Set.empty

let count (bagMap: Map<string, list<string*int>>) (bag: string) =

    let rec loop (bagMap: Map<string, list<string*int>>) (bag: string) =
        
        let contained = bagMap.[bag]
        if contained |> Seq.isEmpty then
            0
        else
            let self = contained |> Seq.sumBy snd 
            
            let rest = 
                contained |> Seq.fold (fun acc (b, count) -> 
                    acc + count * loop bagMap b 
                ) 0

            self + rest

    loop bagMap bag 

let firstStar () =
    let mapOfBags = parse input |> Map.ofSeq

    mapOfBags |> Seq.filter (fun c ->
        let currentBag = c.Key
        contains "shiny gold" currentBag mapOfBags
    )
    |> Seq.length

let secondStar () = 

    let mapOfBags = parse input |> Map.ofSeq

    count mapOfBags "shiny gold"


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(257, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(1038, secondStar())
        


