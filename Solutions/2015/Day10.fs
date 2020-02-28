module AoC.E2015.Day10


// --- Day 10: Elves Look, Elves Say ---

let input = "3113322113"


let split input =
   input
   |> Seq.map (fun c -> int c - int '0')

// matching inspired by https://theburningmonk.com/2015/12/advent-of-code-f-day-10/
let segments (digits:int seq) =
    digits
    |> Seq.fold (fun acc x ->
        match acc with
        | (n, x')::tl when x = x' -> (n+1, x')::tl
        | _ -> (1, x)::acc) []
    |> Seq.rev

let flatten (segments:seq<(int*int)>) =

    seq {
        for (count, digit) in segments do
            yield count
            yield digit
    }

let firstStar () =

    let ds = split input

    {1..40}
    |> Seq.fold (fun last _ ->
        last |> segments |> flatten
    ) ds
    |> Seq.length

let secondStar () =

    let ds = split input
   
    {1..50}
    |> Seq.fold (fun last _ ->
        last |> segments |> flatten
    ) ds
    |> Seq.length

module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =

        Assert.Equal(329356, firstStar())

    [<Fact>]
    let ``second star`` () =

        Assert.Equal(4666278, secondStar())