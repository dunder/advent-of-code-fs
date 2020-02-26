module AoC.E2015.Day09

open System.Text.RegularExpressions;

open AoC
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
  
open System.Collections.Generic

let nextNotInRoute (current:string) (route:string list) (distanceMap:IDictionary<string, IDictionary<string,int>>) =
    distanceMap.[current].Keys 
    |> Seq.filter (fun next -> route |> List.exists (fun next' -> next' = next) |> not) 
    |> List.ofSeq

let allRoutes (distanceMap:IDictionary<string, IDictionary<string,int>>) =
    let starts = distanceMap.Keys |> List.ofSeq
    let routes = []

    let rec _route (current:string) (route:string list) (distanceMap:IDictionary<string, IDictionary<string,int>>) = 
        
        let next = nextNotInRoute current route distanceMap

        match next with
        | head::tail -> 
            current::route
        | [] -> route


        //seq {
        //    // create one route per nextNotInRoute
        //    match nextNotInRoute with
        //    | head::tail -> 
        //        let part1 = _route head (current::route) distanceMap
        //        //let part2 = tail |> Seq.fold (fun routes newCurrent -> (_route newCurrent (head::route) distanceMap)::routes) route
        //        yield! part1
        //        //yield! part2
        //    | [] -> yield route
        //}

    let m = starts |> Seq.fold (fun state item -> (_route item [] distanceMap)::state) routes 

    0

let rec traverse (starts:string list) (routes:string list list) (distanceMap:IDictionary<string, IDictionary<string,int>>) =
    match starts with 
    | x::xs -> 
        
        traverse xs ([x] @ ) distanceMap
    | [] -> routes

let l1 = [1;2;3]
let l2 = [2;3;4]

let firstStar () =
    let m = createDistanceMap <| allDistancePairs input
    let starts = m.Keys |> List.ofSeq
    let n = traverse starts [] m
    0

let secondStar () = 
    0


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =

        Assert.Equal(-1, firstStar())

    [<Fact>]
    let ``second star`` () =

        Assert.Equal(-1, secondStar())


