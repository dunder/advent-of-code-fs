module AoC.E2015.Day25

open AoC

// --- Day 25: Let It Snow ---

let row =2947
let column = 3029

//     | 1   2   3   4   5   6  
//  ---+---+---+---+---+---+---+
//   1 |  1   3   6  10  15  21
//   2 |  2   5   9  14  20
//   3 |  4   8  13  19
//   4 |  7  12  18
//   5 | 11  17
//   6 | 16

let codeAt row column = 
    let atCol1 = [1..row] |> Seq.fold (fun acc n -> acc + (n-1)) 1
    if column = 1 then
        atCol1
    else
        [2..column] |> Seq.fold (fun acc n -> 
            acc + n + row - 1
        ) atCol1

let code nr = [1..nr-1] |> Seq.fold (fun acc n -> acc * 252533L % 33554393L) 20151125L 

let firstStar () =
    
    codeAt row column |> code


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(-1L, firstStar())
