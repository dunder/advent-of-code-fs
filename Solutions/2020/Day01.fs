module AoC.E2020.Day01

open AoC
open IO

// --- Day 1: Report Repair ---

let input = readInputLines "2020" "Day01"


let firstStar () =
    let report = input |> Seq.map (int) |> Array.ofSeq
    let (x,y) = 
        seq {
            for x in 0..Seq.length input-1 do
                for y in 0..Seq.length input-1 do
                    yield (report.[x],report.[y])
        }
        |> Seq.filter (fun (x,y) -> x + y = 2020)
        |> Seq.head
    
    x*y

let secondStar () = 
    let report = input |> Seq.map (int) |> Array.ofSeq
    let (x,y,z) = 
        seq {
            for x in 0..Seq.length input-1 do
                for y in 0..Seq.length input-1 do
                    for z in 0..Seq.length input-1 do
                        yield (report.[x],report.[y], report.[z])
        }
        |> Seq.filter (fun (x,y,z) -> x + y + z = 2020)
        |> Seq.head

    x*y*z


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(646779, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(246191688, secondStar())
        


