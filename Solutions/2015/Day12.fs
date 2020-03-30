module AoC.E2015.Day12

open AoC.IO
open System.Text.RegularExpressions

let input = readInputText "2015" "Day12"

let countNumbers input =
    let m = Regex.Matches(input, "(-?\d+)")
    m
    |> Seq.map (fun m -> m.Value |> int)
    |> Seq.sum


let hasValue object v =
    let reversed = object |> Seq.rev

    let asString = new System.String(Array.ofSeq reversed)

    Regex.IsMatch(asString, sprintf ":\"%s\"" v)

type State = { parsed : char list; liveObjectCounters : int list }

let parse input =
    
    input 
    |> Seq.fold (fun state c -> 
        match c with 
        | '{' -> { parsed = c::state.parsed; liveObjectCounters = 1::state.liveObjectCounters }
        | '}' -> 
            if hasValue (state.parsed |> List.take state.liveObjectCounters.Head) "red" then
                match state.liveObjectCounters with
                | [_] -> { parsed = c::(state.parsed |> List.skip (state.liveObjectCounters.Head - 1)); liveObjectCounters = [] }
                | _::xs -> { parsed = c::(state.parsed |> List.skip (state.liveObjectCounters.Head - 1)); liveObjectCounters = (xs.Head + 2)::xs.Tail }
                | _ -> failwith "Unexpected emptyOpenCount when closing discarded object"
            else 
                match state.liveObjectCounters with
                | x::x'::xs -> { parsed = c::state.parsed; liveObjectCounters = (x + x' + 1)::xs}
                | x::xs -> { parsed = c::state.parsed; liveObjectCounters = (x + 1)::xs }
                | _ ->  failwith "Unexpected empty openCount when closing object"
        | _ -> 
            if List.length state.liveObjectCounters > 0 then
                match state.liveObjectCounters with
                | x::xs -> { parsed = c::state.parsed; liveObjectCounters = (x + 1)::xs }
                | _ -> failwith "Unexpected empty openCount"
            else
                { parsed = c::state.parsed; liveObjectCounters = state.liveObjectCounters }
    ) { parsed = []; liveObjectCounters = [] }

let firstStar () =
    
    countNumbers input

let secondStar () =

    let noRedState = parse input
    let noRedString = new System.String(noRedState.parsed |> Seq.rev |> Array.ofSeq)
    countNumbers noRedString


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        
        Assert.Equal(156366, firstStar())

    [<Fact>]
    let ``second star`` () =

        Assert.Equal(96852, secondStar())

    [<Fact>]
    let ``second star example 1`` () =
        let input = "[1,2,3]"

        let noRedState = parse input
        let noRedString = new System.String(noRedState.parsed |> Seq.rev |> Array.ofSeq)
        let sum = countNumbers noRedString
        
        Assert.Equal(6, sum)     
        
    [<Fact>]
    let ``second star example 2`` () =
        let input = """[1,{"c":"red","b":2},3]"""

        let noRedState = parse input
        let noRedString = new System.String(noRedState.parsed |> Seq.rev |> Array.ofSeq)
        let sum = countNumbers noRedString
        
        Assert.Equal(4, sum)        

    [<Fact>]
    let ``second start example 3`` () =
        let input = """{"d":"red","e":[1,2,3,4],"f":5}"""

        let noRedState = parse input
        let noRedString = new System.String(noRedState.parsed |> Seq.rev |> Array.ofSeq)
        let sum = countNumbers noRedString

        Assert.Equal(0, sum)   

    [<Fact>]
    let ``second star example 4`` () =
        let input = """[1,"red",5]"""

        let noRedState = parse input
        let noRedString = new System.String(noRedState.parsed |> Seq.rev |> Array.ofSeq)
        let sum = countNumbers noRedString

        Assert.Equal(6, sum)

    [<Fact>]
    let ``second star advanced example`` () =
        let input = """[{"a":{"e":{"e":161,"a":"blue","d":{"e":-14,"a":"red","d":{"c":"yellow","a":[-35,0],"b":"orange","d":{"e":70,"a":"green","d":"blue","j":12,"c":69,"h":"orange","b":92,"g":"yellow","f":"green","i":121}},"c":"blue","h":14,"b":46,"g":62,"f":[179]}}}}]"""

        let noRedState = parse input
        let noRedString = new System.String(noRedState.parsed |> Seq.rev |> Array.ofSeq)
        let sum = countNumbers noRedString

        Assert.Equal(161, sum)