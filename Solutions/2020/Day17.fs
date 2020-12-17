module AoC.E2020.Day17

open AoC
open IO
open System.Collections.Generic

// --- Day 17: Conway Cubes ---

let input = readInputLines "2020" "Day17" |> List.ofSeq

type Point3D = { X: int; Y: int; Z: int}
type Point4D = { X: int; Y: int; Z: int; W: int}

type Cube3D = Set<Point3D>
type Cube4D = Set<Point4D>

module Point3D = 
    let neighbors (p: Point3D) =
        [
            for z in p.Z-1..p.Z+1 do
                for y in p.Y-1..p.Y+1 do
                    for x in p.X-1..p.X+1 do
                        let neighbor = { X = x; Y = y; Z = z }
                        if neighbor <> p then
                            yield neighbor
        ]
        
module Point4D = 

    let neighbors p =
        [
            for w in p.W-1..p.W+1 do
                for z in p.Z-1..p.Z+1 do
                    for y in p.Y-1..p.Y+1 do
                        for x in p.X-1..p.X+1 do
                            let neighbor = { X = x; Y = y; Z = z; W = w}
                            if neighbor <> p then
                                yield neighbor
        ]

module Cube3D =
    
    let parse (lines: list<string>) = 
        let rows = Seq.length lines
        let columns = String.length <| Seq.head lines
        let cube =
            [
                for column in 0..columns-1 do
                    for row in 0..rows-1 do
                        let c = lines.[row].[column]
                        if c = '#' then
                            yield { X = column; Y = row; Z = 0 }
            ]
            |> Set.ofSeq
                
        cube

    let activate p (cube: Cube3D) = cube |> Set.add p
    let inactivate p (cube: Cube3D) = cube |> Set.remove p

    let active p (cube: Cube3D) = cube |> Set.contains p

    let space (cube: Cube3D) = 
        cube 
        |> Seq.collect (fun p -> p |> Point3D.neighbors)
        |> Seq.append cube

    let activeNeighbors p (cube: Cube3D) = Point3D.neighbors p |> Seq.filter (fun n -> cube |> active n)

    let oneCycle (cube: Cube3D): Cube3D =

        cube
        |> space
        |> Seq.map (fun p -> 
            let count = 
                cube 
                |> activeNeighbors p
                |> Seq.length
                
            if cube |> active p then 
                if count = 2 || count = 3 then
                    Some(p)
                else 
                    None
            else 
                if count = 3 then
                    Some(p)
                else
                    None
        )
        |> Seq.choose (id)
        |> Set.ofSeq
        
    let cycle (cube: Cube3D) =
        [1..6]
        |> Seq.fold (fun cube i -> 
            cube |> oneCycle
        ) cube

module Cube4D =
    
    let parse (lines: list<string>) = 
        let rows = Seq.length lines
        let columns = String.length <| Seq.head lines
        let cube =
            [
                for column in 0..columns-1 do
                    for row in 0..rows-1 do
                        let c = lines.[row].[column]
                        if c = '#' then
                            yield { X = column; Y = row; Z = 0; W = 0 }
            ]
            |> Set.ofSeq
                
        cube

    let activate p (cube: Cube4D) = cube |> Set.add p
    let inactivate p (cube: Cube4D) = cube |> Set.remove p

    let active p (cube: Cube4D) = cube |> Set.contains p

    let space (cube: Cube4D) = 
        cube 
        |> Seq.collect (fun p -> p |> Point4D.neighbors)
        |> Seq.append cube

    let neighborLookup (cube: Cube4D) = 
        cube 
        |> space
        |> Seq.map (fun p -> (p, p |> Point4D.neighbors))
        |> dict

    let activeNeighbors p (cube: Cube4D) = p |> Point4D.neighbors |> Seq.filter (fun n -> cube |> active n)

    let oneCycle (cube: Cube4D): Cube4D =
        cube
        |> space
        |> Seq.map (fun p -> 
            let count = 
                cube 
                |> activeNeighbors p
                |> Seq.length
                
            if cube |> active p then 
                if count = 2 || count = 3 then
                    Some(p)
                else 
                    None
            else 
                if count = 3 then
                    Some(p)
                else
                    None
        )
        |> Seq.choose (id)
        |> Set.ofSeq
        
    let cycle (cube: Cube4D) =
        [1..6]
        |> Seq.fold (fun (cube) i -> 
            cube |> oneCycle 
        ) cube

let firstStar () =

    Cube3D.parse input
    |> Cube3D.cycle 
    |> Seq.length

let secondStar () = 

    Cube4D.parse input
    |> Cube4D.cycle 
    |> Seq.length


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(368, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(2696, secondStar())

    [<Fact>]
    let ``first star example`` () =

        let input = [
            ".#."
            "..#"
            "###"
        ]

        let cube = Cube3D.parse input

        let cycled = Cube3D.cycle cube |> Seq.length

        Assert.Equal(112, cycled)
