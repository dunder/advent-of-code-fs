module AoC.E2015.Day10


open AoC
open IO

// --- Day 10: Elves Look, Elves Say ---

let input = "3113322113"

// so far it is just a copy of https://theburningmonk.com/2015/12/advent-of-code-f-day-10/

let read (input : string) =
    input
    |> Seq.fold (fun acc x ->
        match acc with
        | (n, x')::tl when x = x' -> (n+1, x')::tl
        | _ -> (1, x)::acc) []
    |> List.rev
    |> Seq.collect (fun (n, x) -> sprintf "%d%c" n x)
    |> fun xs -> System.String.Join("", xs)

let firstStar () =
    { 1..40 }
    |> Seq.fold (fun last _ -> read last) input
    |> Seq.length

let secondStar () = 
    { 1..50 }
    |> Seq.fold (fun last _ -> read last) input
    |> Seq.length

module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =

        Assert.Equal(329356, firstStar())

    [<Fact>]
    let ``second star`` () =

        Assert.Equal(4666278, secondStar())