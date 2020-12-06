module AoC.E2020.Day06

open AoC
open IO

// --- Day 6: Custom Customs ---

let input = readInputLines "2020" "Day06" |> List.ofSeq

let parse lines =

    lines
    |> Seq.mapFold (fun acc line -> 
       if line = "" then
            ((-1,line), acc + 1)
       else
            ((acc,line), acc)
    ) 0
    |> fst
    |> Seq.filter (fun (i, _) -> i <> -1)
    |> Seq.groupBy (fun (i, _) -> i)
    

let count line =
    line 
    |> Seq.distinct 
    |> Seq.length

let count2 lines = 
    lines 
    |> Seq.distinct 
    |> Seq.map Set.ofSeq 
    |> Seq.reduce Set.intersect 
    |> Seq.length

let firstStar () =
    parse input
    |> Seq.map (fun x -> snd x |> Seq.map snd |> String.concat "")
    |> Seq.map count 
    |> Seq.reduce (+)

let secondStar () = 
    parse input 
    |> Seq.map (fun x -> snd x |> Seq.map snd)
    |> Seq.map count2
    |> Seq.reduce (+)


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(6310, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(3193, secondStar())
        


