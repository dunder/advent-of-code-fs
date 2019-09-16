module AoC.E2015.Day04

open AoC
open Cryptography

let input = "bgvyzdsv"

let findFirstMatchingHash (input : string, target : string) =
    // unfold takes a function that should return an option tuple of the next value and the next state
    // this implementation generates the next int starting with 1 and keeps on while iterated over
    let values = Seq.unfold (fun state -> Some(state, state + 1)) 1
    Seq.find (fun x -> (md5 (input + x.ToString())).StartsWith(target)) values
    

let firstStar () =
    findFirstMatchingHash (input, "00000")


let secondStar () = 
    findFirstMatchingHash (input, "000000")


module Tests =

    open Xunit

    [<Theory>]
    [<InlineData("abcdef", 609043)>]
    [<InlineData("pqrstuv", 1048970)>]

    let ``examples for first star`` (input, expected) = 
        Assert.Equal(expected, findFirstMatchingHash (input,"00000"))

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(254575, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(1038736, secondStar())
