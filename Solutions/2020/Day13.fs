module AoC.E2020.Day13

open AoC
open IO

// --- Day 13: Shuttle Search ---

let input = readInputLines "2020" "Day13" |> List.ofSeq

let parse (lines: list<string>) = 
    let earliest = lines.[0] |> int
    let buses = lines.[1].Split(',') |> Seq.filter (fun x -> x <> "x") |> Seq.map int
    earliest, buses

let parse2 (lines: list<string>) = 
    lines.[1].Split(',') 
    |> Array.mapi (fun i s -> 
        if s = "x" then 
            (i |> int64, None) 
        else 
            (i |> int64, Some (s |> int64))
    )
    |> List.ofArray

let firstStar () =
    let at, buses = parse input
    let firstDeparture = 
        Seq.unfold (fun (time, _) -> 
            let bus = buses |> Seq.tryFind(fun bus -> time % bus = 0)
            
            Some ((time, bus), (time + 1, bus))) (at, None)
        |> Seq.find (fun (time, bus) -> 
            match bus with
            | Some x -> true
            | None -> false
        )
    (firstDeparture |> snd).Value * ((firstDeparture |> fst) - at)

let secondStar () = 

    let buses = 
        parse2 input 
        |> List.choose (fun (t, bus) -> 
            match bus with
            | Some x -> Some ((t, x))
            | None -> None
         )

    let _, bus = buses |> List.head

    let result = 
        buses.Tail 
        |> Seq.fold (fun (time, step) (idx, bus) -> 
            let t = 
                Seq.unfold (fun t -> 
                    if (t + idx) % bus = 0L then
                        None
                    else 
                        Some (t + step, t + step)
                ) time
                |> Seq.last
            (t, bus * step)
        ) (0L, bus)

    result |> fst
 
module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(153, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(471793476184394L, secondStar())
