module AoC.E2015.Day14

open System.Text.RegularExpressions

open AoC
open IO

// --- Day 14: Reindeer Olympics ---

let input = readInputLines "2015" "Day14"

type Raindeer = { Name: string; Speed : int; Flight : int; Rest : int} with
    member x.Distance = (fun seconds -> 
        let interval = x.Flight + x.Rest
        let fullCycles = seconds / interval
        let remaining = seconds % interval
        let distance = x.Speed * fullCycles * x.Flight + x.Speed * min remaining x.Flight
        distance
    )

let (|Regex|_|) pattern input = 
    let m = Regex.Match(input, pattern)

    if m.Success
    then Some(List.tail [for g in m.Groups -> g.Value])
    else None

let parse input = 
    input
    |> Seq.map (fun line -> 
        match line with 
        | Regex "^(.*) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds." 
            [name; speed; flight; rest] -> { Name = name; Speed = speed |> int; Flight = flight |> int; Rest = rest |> int }
        | x -> failwithf "Cannot parse: %s" x
    )

let maxDistance input seconds =
    let deers = parse input
    
    deers 
    |> Seq.map (fun deer -> deer.Distance seconds)
    |> Seq.max

let leaders (deers:seq<Raindeer>) seconds = 
    let standing =
        deers 
        |> Seq.sortByDescending (fun d -> d.Distance seconds)
        |> Seq.toList

    let leadingDistance = standing.Head.Distance seconds
    
    standing |> Seq.takeWhile (fun d -> d.Distance seconds = leadingDistance)

let maxScore input seconds =
    let deers = parse input
    
    {1 .. seconds}
    |> Seq.collect(fun t  ->
        leaders deers t
    )
    |> Seq.groupBy (fun d -> d.Name)
    |> Seq.map (fun (key, group) -> Seq.length group)
    |> Seq.max
    

let firstStar () =
    maxDistance input 2503

let secondStar () = 
    maxScore input 2503


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =

        Assert.Equal(2640, firstStar())

    [<Fact>]
    let ``second star`` () =

        Assert.Equal(1102, secondStar())
        
    [<Theory>]
    [<InlineData(1, 14)>]
    [<InlineData(10, 140)>]
    [<InlineData(11, 140)>]
    [<InlineData(1000, 1120)>]
    let ``distance calculation comet`` seconds expected =
        let comet = { Name = "Comet"; Speed = 14; Flight = 10; Rest = 127 }

        let distance = comet.Distance seconds 
        
        Assert.Equal(expected, distance)
    
    [<Theory>]
    [<InlineData(1, 16)>]
    [<InlineData(10, 160)>]
    [<InlineData(11, 176)>]
    [<InlineData(1000, 1056)>]
    let ``distance calculation dancer`` seconds expected =

        let dancer = { Name = "Dancer"; Speed = 16; Flight = 11; Rest = 162 }

        let distance = dancer.Distance seconds
        
        Assert.Equal(expected, distance)

    [<Fact>]
    let ``first star example`` () =
        let input = [|
            "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."
            "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."
        |]

        let maxDistance = parse input |> Seq.map (fun deer -> deer.Distance 1000) |> Seq.max
        Assert.Equal(1120, maxDistance)

    [<Fact>]
    let ``second star example`` () =
        let input = [|
            "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."
            "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."
        |]

        let maxScore = maxScore input 1000

        Assert.Equal(689, maxScore)