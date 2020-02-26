module AoC.E2015.Day09

open System.Collections.Generic
open System.Text.RegularExpressions;

open AoC
open Combinatorics
open IO

// --- Day 9: All in a Single Night ---

let input = readInputLines "2015" "Day09"

let (|Regex|_|) pattern input = 
    let m = Regex.Match(input, pattern)
    
    if m.Success 
    then Some(List.tail [for g in m.Groups -> g.Value])
    else None

type Leg = { Start:string; Stop:string; Distance:int }

let parseLine line = 
    match line with 
    | Regex "(\w+) to (\w+) = (\d+)" [start;stop;distance] -> { Start = start; Stop = stop; Distance = int distance}
    | _ -> failwith <| sprintf "Unrecongnizable input %s" line

let parse lines =
    lines |> Seq.map parseLine  

let allDistancePairs input =
    parse input
        |> Seq.collect (fun leg -> leg::[{Start = leg.Stop; Stop = leg.Start; Distance = leg.Distance}])

let createDistanceMap distancePairs =
    distancePairs
    |> Seq.groupBy (fun leg -> leg.Start)
    |> Seq.map (fun (start, group) -> (start, group |> Seq.map (fun leg -> (leg.Stop, leg.Distance)) |> dict))
    |> dict

let nextNotInRoute (current:string) (route:string list) (distanceMap:IDictionary<string, IDictionary<string,int>>) =
    distanceMap.[current].Keys 
    |> Seq.filter (fun next -> route |> List.exists (fun next' -> next' = next) |> not) 
    |> List.ofSeq

let shortestRoute input =
    let distances = createDistanceMap <| allDistancePairs input
    let distance route =
        route
        |> List.pairwise 
        |> List.map (fun (start,stop) -> distances.[start].[stop]) 
        |> List.sum
    
    let cities = distances.Keys |> List.ofSeq
    let routes = permutations cities

    routes |> Seq.map distance |> Seq.min

let firstStar () =
    shortestRoute input

let secondStar () = 
    0


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =

        Assert.Equal(117, firstStar())

    [<Fact>]
    let ``second star`` () =

        Assert.Equal(-1, secondStar())


