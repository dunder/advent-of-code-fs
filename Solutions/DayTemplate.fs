module AoC.E20XX.DayXX

open AoC
open IO

// Template for new problems

let input = readInputLines "20XX" "DayXX" |> List.ofSeq


let firstStar () =
    0

let secondStar () = 
    0


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =

        Assert.Equal(0, firstStar())

    [<Fact>]
    let ``second star`` () =

        Assert.Equal(0, secondStar())
