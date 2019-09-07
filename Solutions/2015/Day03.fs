module AoC.E2015.Day03

open Aoc
open IO

// --- Day 3: Perfectly Spherical Houses in a Vacuum ---

let input = readInputText "2015" "Day03"

type Position = { X: int; Y: int} with
    member x.North = { X = x.X; Y = x.Y - 1}
    member x.East = { X = x.X + 1; Y = x.Y}
    member x.South = { X = x.X; Y = x.Y + 1}
    member x.West = { X = x.X - 1; Y = x.Y}

let visitedHouses input = 
    input 
    |> Seq.fold (fun (acc: List<Position>) c -> 
        match c with
        | '^' -> acc.Head.North::acc
        | '>' -> acc.Head.East::acc
        | 'v' -> acc.Head.South::acc
        | '<' -> acc.Head.West::acc
        | _ -> failwithf "Elf has had to much eggnog giving false instruction %c" c) [{X = 0; Y = 0}]
    |> Seq.distinct

let firstStar () =
    visitedHouses input |> Seq.length

let secondStar () = 
    let (santa, robot) = 
        input
        |> Seq.indexed 
        |> Seq.toList
        |> List.partition (fun (i, _) -> i % 2 = 0)

    let santasVisits = visitedHouses (santa |> List.map snd) |> Seq.toList
    let robotsVisits = visitedHouses (robot |> List.map snd) |> Seq.toList

    santasVisits @ robotsVisits |> Seq.distinct |> Seq.length


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(2592, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(2360, secondStar())
