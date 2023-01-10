module AoC.E2022.Day18

// --- Day 18: Boiling Boulders ---

open AoC
open IO


let input = readInputLines "2022" "Day18" |> List.ofSeq


let parse (lines: string list) =
    lines
    |> List.map (fun line ->
        let ints = line.Split(",") |> Array.map int
        ints[0], ints[1], ints[2]
    )

let sides cube = 
    let x, y, z = cube
    [
        x+1,y,z
        x-1,y,z
        x,y+1,z
        x,y-1,z
        x,y,z+1
        x,y,z-1
    ]

let surfaceArea cubes = 
    let cubeSet = cubes |> Set
    cubes
    |> List.fold (fun exposed cube ->
        let sides = sides cube |> Set

        exposed + (cubeSet |> Set.difference sides |> Set.count)
    ) 0

let firstStar () =
    input
    |> parse
    |> surfaceArea
    
let mapX (x, y, z) = x
let mapY (x, y, z) = y
let mapZ (x, y, z) = z

let surroundingCube (cubes: Set<int*int*int>) : int*int*int = 
    let maxx = cubes |> Set.map mapX |> Set.maxElement
    let maxy = cubes |> Set.map mapY |> Set.maxElement
    let maxz = cubes |> Set.map mapZ |> Set.maxElement

    // add one layer of air to be able to traverse outside of the droplet
    maxx + 1, maxy + 1, maxz + 1

let isNeighbour (maxx, maxy, maxz) lavaDroplets (x,y,z) = 

    // allow walk one step outside sourrounding cube
    if x < -1 || y < -1 || z < -1 then
        false
    else if x > maxx || y > maxy || z > maxz then
        false
    else 
        not <| Set.contains (x,y,z) lavaDroplets

let neighbours (limitingCube: int*int*int) (lavaDroplets: Set<int*int*int>) (x,y,z) = 
    sides (x, y, z)
    |> List.filter (isNeighbour limitingCube lavaDroplets)

// some position known to be air
let startSet = (-1,-1,-1) |> Set.singleton

type NodeSet<'node when 'node : comparison> = Set<'node>

let bfs neighbours startExploring = 

    let neighboursNotVisited (visited: NodeSet<'node>) nodeSet =
        neighbours nodeSet
        |> List.filter(visited.Contains >> not) 
        |> Set

    Seq.unfold(fun ((exploring, visited): NodeSet<'node>*NodeSet<'node>) -> 
            if exploring |> Set.isEmpty then 
                None 
            else                
                let newExploring = exploring |> Seq.map (neighboursNotVisited visited) |> Set.unionMany                
                let newVisited = visited + newExploring
                let newState = newExploring, newVisited
                Some(newState, newState)) (startExploring, startExploring)
    |> Seq.last

let countExteriorFaces droplet =

    let surroundingCube = surroundingCube droplet

    bfs (neighbours surroundingCube droplet) startSet
    |> snd
    |> Set.toList
    |> List.fold (fun exteriorFaces air ->
        let sides = sides air |> Set
        let touches = sides |> Set.intersect droplet |> Set.count
        exteriorFaces + touches
    ) 0

let secondStar () = 
    input
    |> parse
    |> Set
    |> countExteriorFaces