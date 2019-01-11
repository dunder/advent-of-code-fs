module AoC.E2015.Day01

open Aoc
open Sequences
open IO

// --- Day 1: Not Quite Lisp ---

let input = readText @".\2015\Input\Day01.txt"

let elevetorStopsAt input = (input |> count '(') - (input |> count ')')

let rec positionWhenFloorReached position floor movements targetFloor =
    match movements with 
    | [] -> position
    | head::tail ->
        let newFloor = match head with
        | ')' -> floor - 1
        | '(' -> floor + 1
        | x -> failwithf "Unexpected token '%c' in input" x

        if newFloor = targetFloor then
            position
        else 
            positionWhenFloorReached (position+1) newFloor tail targetFloor

let positionOfFirstBasement input = 
    positionWhenFloorReached 1 0 (Seq.toList input) -1

let firstStar () =
    let floor = elevetorStopsAt input
    printfn "First star: %A" floor

let secondStar () = 
    let position = positionOfFirstBasement input
    printfn "Second star: %A" position


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
