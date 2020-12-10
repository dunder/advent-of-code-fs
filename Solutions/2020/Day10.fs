module AoC.E2020.Day10

open AoC
open IO
open Sequences

// --- Day 10: Adapter Array ---

let input = readInputLines "2020" "Day10" |> List.ofSeq

let adjacent nodes node =
    nodes |> Seq.filter (fun x -> x >node && x <= node + 3)

let countAll (nodes:list<int*list<int>>) =

    let nodes = nodes |> List.sortByDescending fst
    
    let mapOfCounts = Map.empty |> Map.add (nodes.Head |> fst) 1L

    let rec loop (nodes:list<int*list<int>>) (node:int*list<int>) (map:Map<int,int64>) =
        let n = node |> fst
        let reaches = node |> snd
        let myCount = reaches |> Seq.sumBy (fun x -> map.[x])

        if nodes |> Seq.isEmpty then
            myCount
        else
            let newMap = map |> Map.add n myCount
            loop nodes.Tail nodes.Head newMap

    loop nodes.Tail.Tail nodes.Tail.Head mapOfCounts
    

let firstStar () =

    let myJolt = (input |> List.map int |> List.max) + 3
    let sorted = 0::myJolt::(input |> List.map int) |> List.sort
    let diffs = sorted |> List.pairwise |> List.map (fun (a,b) -> b - a)
    let ones = diffs |> count 1
    let threes = diffs |> count 3
    ones * threes

let secondStar () = 

    let myJolt = (input |> List.map int |> List.max) + 3
    let sorted = 0::myJolt::(input |> List.map int) |> List.sort
    let diffs = sorted |> List.map (fun x -> (x, adjacent sorted x |> List.ofSeq))
    
    countAll diffs


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(2470, firstStar()) 

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(1973822685184L, secondStar())
        


