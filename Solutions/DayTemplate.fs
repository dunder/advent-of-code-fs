module AoC.DayXX

open Aoc
open IO

// Template for new problems

let input = readLines @".\Input\DayXX.txt"


let firstStar () =
    printfn "First star: %A" 0

let secondStar () = 
    printfn "Second star: %A" 0


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =

        Assert.Equal(0, 0)

    [<Fact>]
    let ``second star`` () =

        Assert.Equal(0, 0)
