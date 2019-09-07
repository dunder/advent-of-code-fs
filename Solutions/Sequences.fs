module AoC.Sequences

let count x xs =
    xs
    |> Seq.filter (fun x' -> x' = x)
    |> Seq.length

