module AoC.E2020.Day06

open AoC
open IO

// 

let input = readInputLines "2020" "Day06" |> List.ofSeq


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
        


