module AoC.E2020.Day09

open AoC
open IO
open Combinatorics

// --- Day 9: Encoding Error ---

let input = readInputLines "2020" "Day09" |> List.ofSeq


let firstStar () =

    let preambleCount = 25
    let numbers = input |> List.map int64
    let preamble = numbers |> List.take preambleCount
    let code = numbers |> List.skip preambleCount

    let x = List.unfold (fun (p, c) -> 
        let sums = p |> combinations 2 |> List.map (fun cs -> (cs |> List.head) + (cs |> List.last)) |> Set.ofSeq
        let x = c |> List.head

        if not (sums |> Set.contains x) then 
            None 
        else
            let pTail = p |> List.tail
            let cRest = c |> List.tail
            Some ((p, c), (pTail @ [x], cRest))) (preamble, code)

    x |> List.last |> snd |> List.skip 1 |> List.head


let secondStar () = 
    let searchFor = 15690279L

    let numbers = input |> List.map int64
    
    let potentialRange = 
        [0..numbers.Length-2] |> List.map (fun idx ->
            let ns = numbers |> List.skip idx
            let sums = ns |> List.scan (+) 0L
            (ns, sums)
        )
        |> List.find (fun range -> range |> snd |> List.exists (fun x -> x = searchFor))

    let idx = potentialRange |> snd |> List.findIndex (fun x -> x = searchFor)
    let actualRange = potentialRange |> fst |> List.take idx
    let min = actualRange |> List.min
    let max = actualRange |> List.max

    min + max


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(15690279L, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(2174232L, secondStar())
        


