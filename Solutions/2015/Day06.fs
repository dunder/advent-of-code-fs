module AoC.E2015.Day06

open AoC
open IO
open System.Text.RegularExpressions

let input = readInputLines "2015" "Day06"

type Operation = On | Off | Toggle
type Position = { X : int; Y : int }
type Command = { TopLeft : Position; BottomRight : Position; Operation : Operation }


let parseOperation op = 
    let matchExpression = Regex @"(toggle|turn on|turn off) ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)"
    let matchResult = matchExpression.Match op
    let groups = matchResult.Groups
    let operationValue = groups.[1].Value

    let operation = 
        match operationValue with 
        | "toggle" -> Toggle
        | "turn on" -> On
        | "turn off" -> Off
        | _ -> failwithf "Unknown command %A" operationValue

    let x1 = groups.[2].Value |> int
    let y1 = groups.[3].Value |> int
    let x2 = groups.[4].Value |> int
    let y2 = groups.[5].Value |> int

    { TopLeft = { X = x1; Y = y1}; BottomRight = { X = x2; Y = y2}; Operation = operation }


let updateRegion (matrix:'a[,], p1:Position, p2:Position, action:'a->'a) =
    for x in p1.X..p2.X do
        for y in p1.Y..p2.Y do
            matrix.[x,y] <- action(matrix.[x,y])

let executeCommand (matrix : bool[,], command : Command) = 
    let op = 
        match command.Operation with
        | Toggle -> (fun b -> not b)
        | On -> (fun b -> true)
        | Off -> (fun b -> false)

    updateRegion(matrix, command.TopLeft, command.BottomRight, op)

    ()

let executeBrightnessCommand (matrix : int[,], command : Command) = 
    let op = 
        match command.Operation with
        | Toggle -> (fun x -> x + 2)
        | On -> (fun x -> x + 1)
        | Off -> (fun x -> max 0 (x - 1))

    updateRegion(matrix, command.TopLeft, command.BottomRight, op)

    () 

let firstStar () =
    let display = Array2D.create 1000 1000 false

    input 
    |> Seq.map parseOperation 
    |> Seq.iter (fun c -> executeCommand(display, c))

    display
    |> Seq.cast<bool>
    |> Seq.filter (fun b -> b)
    |> Seq.length


let secondStar () = 
    let display = Array2D.create 1000 1000 0
    
    input 
    |> Seq.map parseOperation 
    |> Seq.iter (fun c -> executeBrightnessCommand(display, c))
    
    display
    |> Seq.cast<int>
    |> Seq.sum


module Tests =

    open Xunit

    [<Fact>]
    let ``parse operation`` () =
        let result = parseOperation "toggle 100,200 through 300,400"
        let expected = { TopLeft = { X = 100; Y = 200}; BottomRight = { X = 300; Y = 400}; Operation = Toggle }
        Assert.Equal(expected, result)

    [<Fact>]
    let ``handleOperation when toggling`` () =
        let command = { TopLeft = { X = 1; Y = 1}; BottomRight = { X = 2; Y = 2}; Operation = Toggle }

        let display = Array2D.create 3 3 true

        executeCommand(display, command)

        Assert.Equal(true, display.[0,0])
        Assert.Equal(true, display.[0,1])
        Assert.Equal(true, display.[0,2])
        Assert.Equal(true, display.[1,0])
        Assert.Equal(false, display.[1,1])
        Assert.Equal(false, display.[1,2])
        Assert.Equal(true, display.[2,0])
        Assert.Equal(false, display.[2,1])
        Assert.Equal(false, display.[2,2])

    [<Fact>]
    let ``handleOperation when turning on`` () =
        let command = { TopLeft = { X = 1; Y = 1}; BottomRight = { X = 2; Y = 2}; Operation = Toggle }

        let matrix = Array2D.create 3 3 false

        executeCommand(matrix, command)

        Assert.Equal(false, matrix.[0,0])
        Assert.Equal(false, matrix.[0,1])
        Assert.Equal(false, matrix.[0,2])
        Assert.Equal(false, matrix.[1,0])
        Assert.Equal(true, matrix.[1,1])
        Assert.Equal(true, matrix.[1,2])
        Assert.Equal(false, matrix.[2,0])
        Assert.Equal(true, matrix.[2,1])
        Assert.Equal(true, matrix.[2,2])

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(543903, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(14687245, secondStar())
