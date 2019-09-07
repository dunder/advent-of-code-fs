module AoC.E2015.Day04

open AoC
open Cryptography

let input = "bgvyzdsv"

let add (input : string, x : int) = 
    md5 (input + x.ToString())

let findFirstMatchingHash (input : string, target : string) =
    let values = Seq.unfold (fun state -> Some(state, state + 1)) 0
    Seq.find (fun x -> (add (input, x)).StartsWith(target)) values
    

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
