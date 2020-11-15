module AoC.E2015.Day18

open AoC
open IO


// --- Day 18: Like a GIF For Your Yard ---

let input = readInputLines "2015" "Day18"
 
type Position = { X : int; Y : int }

let adjacent (matrix:'a[,], p1:Position, p2:Position) : seq<'a> =
    
    Seq.empty

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