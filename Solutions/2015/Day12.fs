module AoC.E2015.Day12

open AoC.IO
open System.Text.RegularExpressions

let input = readInputText "2015" "Day12"

let countNumbers input =
    let m = Regex.Matches(input, "(-?\d+)")
    m
    |> Seq.map (fun m -> m.Value |> int)
    |> Seq.sum

let firstStar () =
    
    countNumbers input

let secondStar () =

    0

module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        
        Assert.Equal(156366, firstStar())

    [<Fact>]
    let ``second star`` () =

        Assert.Equal(96852, secondStar())