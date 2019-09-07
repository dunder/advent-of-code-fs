module AoC.E2015.Day01

open AoC
open Sequences
open IO

// --- Day 1: Not Quite Lisp ---

let input = readInputText "2015" "Day01"

let elevetorStopsAt input = (input |> count '(') - (input |> count ')')

let positionForFirstTargetFloor input targetFloor =
    input
    |> Seq.scan (fun floor c -> 
        match c with
        | '(' -> floor + 1
        | _ -> floor - 1) 0
    |> Seq.findIndex (fun i -> i = targetFloor)

let positionOfFirstBasement input = 
    positionForFirstTargetFloor input -1

let firstStar () =
    elevetorStopsAt input

let secondStar () = 
    positionOfFirstBasement input


module Tests =

    open Xunit

    [<Theory>]
    [<InlineData("(())", 0)>]
    [<InlineData("(((", 3)>]
    [<InlineData("))(((((", 3)>]
    let ``when floorVisitor is provided with input resulting floor meets expectation`` input expectedFloor =
        let floor = elevetorStopsAt input

        Assert.Equal(expectedFloor, floor)

    [<Theory>]
    [<InlineData(")", 1)>]    
    [<InlineData("()())", 5)>]    
    let ``when positionOfFirstBasement is provided with input resulting position meets expectation`` input expectedPosition =
        let position = positionOfFirstBasement input

        Assert.Equal(expectedPosition, position)

    [<Fact>]
    let ``first star`` () =
        let floor = elevetorStopsAt input

        Assert.Equal(74, floor)

    [<Fact>]
    let ``second star`` () =
        let position = positionOfFirstBasement input

        Assert.Equal(1795, position)
