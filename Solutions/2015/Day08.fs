module AoC.E2015.Day08

open System.Text.RegularExpressions

open AoC
open IO

let input = readInputLines "2015" "Day08"

let escape text =
    text 
    |> fun (x:string) -> x.Trim('"')
    |> fun s -> Regex.Replace(s, @"\\\\", "?")
    |> fun s -> Regex.Replace(s, @"\\""", "?")
    |> fun s -> Regex.Replace(s, @"\\x([0-9a-f][0-9a-f])", "?")

let encode text =
    text 
    |> fun s -> Regex.Replace(s, @"\\", "??")
    |> fun s -> Regex.Replace(s, @"""", "??")
    |> fun s -> sprintf "%c%s%c" '"' s '"'


let firstStar () =
    let coded = 
        input
        |> Seq.map escape
        |> Seq.map String.length
        |> Seq.sum

    let memory =
        input
        |> Seq.map String.length
        |> Seq.sum

    memory - coded

let secondStar () = 
   let encoded = 
       input
       |> Seq.map encode
       |> Seq.map String.length
       |> Seq.sum

   let memory =
       input
       |> Seq.map String.length
       |> Seq.sum

   encoded - memory


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =

        Assert.Equal(1350, firstStar())

    [<Fact>]
    let ``second star`` () =

        Assert.Equal(2085, secondStar())

