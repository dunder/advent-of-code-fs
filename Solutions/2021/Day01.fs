module AoC.E2021.Day01

open AoC
open IO

// Template for new problems

let input = readInputLines "2021" "Day01" |> List.ofSeq


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
