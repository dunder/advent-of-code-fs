module AoC.E2016.Day04

open AoC
open IO

// --- Day 4: Security Through Obscurity ---

let input = readInputLines "2016" "Day04" |> List.ofSeq


let firstStar () =
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