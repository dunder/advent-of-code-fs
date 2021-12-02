// --- Day 1: Sonar Sweep ---

#load "../IO.fs"

open AoC.IO

module Day01 =

    let input = readInputLines "2021" "Day01"

    let firstStar () =
        input
        |> Seq.map System.Int32.Parse
        |> Seq.pairwise
        |> Seq.filter (fun (d1, d2) -> d2 > d1)
        |> Seq.length

    let secondStar () = 
        input
        |> Seq.map System.Int32.Parse
        |> Seq.windowed 3
        |> Seq.map Seq.sum
        |> Seq.pairwise
        |> Seq.filter (fun (d1, d2) -> d2 > d1)
        |> Seq.length