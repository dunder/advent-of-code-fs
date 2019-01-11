module AoC.E2015.Day02

open Aoc
open IO

// --- Day 2: I Was Told There Would Be No Math ---

let input = readLines @".\2015\Input\Day02.txt"


let firstStar () =
    printfn "First star: %A" 0

let secondStar () = 
    printfn "Second star: %A" 0


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =

        Assert.Equal(1588178, 1588178)

    [<Fact>]
    let ``second star`` () =

        Assert.Equal(3783758, 3783758)
