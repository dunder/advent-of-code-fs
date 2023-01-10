// --- Day 18: Boiling Boulders ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day18.txt") |> List.ofSeq

let parse (lines: string list) =
    lines
    |> List.map (fun line ->
        let ints = line.Split(",") |> Array.map int
        ints[0], ints[1], ints[2]
    )

let example = 
    [
        "2,2,2"
        "1,2,2"
        "3,2,2"
        "2,1,2"
        "2,3,2"
        "2,2,1"
        "2,2,3"
        "2,2,4"
        "2,2,6"
        "1,2,5"
        "3,2,5"
        "2,1,5"
        "2,3,5"
    ]

let exampleDroplet = example |> parse
let droplet = input |> parse

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

let example1 = sides exampleDroplet[0]

let exampleCubeSet = exampleDroplet |> Set.ofList
let cubeSet = droplet |> Set.ofList

((1,1,1) |> Set.singleton) |> Set.difference (sides (2,1,1) |> Set)

let surfaceArea cubes = 
    let cubeSet = cubes |> Set
    cubes
    |> List.fold (fun exposed cube ->
        let sides = sides cube |> Set

        exposed + (cubeSet |> Set.difference sides |> Set.count)
    ) 0

surfaceArea exampleDroplet 

let firstStar = droplet |> surfaceArea
    
let mapX (x, y, z) = x
let mapY (x, y, z) = y
let mapZ (x, y, z) = z

// all zeros
droplet |> List.map mapX |> List.min 
droplet |> List.map mapY |> List.min
droplet |> List.map mapZ |> List.min

let surroundingCube (cubes: list<int*int*int>) : int*int*int = 
    let maxx = cubes |> List.map mapX |> List.max
    let maxy = cubes |> List.map mapY |> List.max
    let maxz = cubes |> List.map mapZ |> List.max

    // add one layer of air to be able to traverse outside of the droplet
    maxx + 1, maxy + 1, maxz + 1

surroundingCube exampleDroplet

let isNeighbour (maxx, maxy, maxz) lavaDroplets (x,y,z) = 

    // allow walk one step outside sourrounding cube
    if x < -1 || y < -1 || z < -1 then
        false
    else if x > maxx || y > maxy || z > maxz then
        false
    else 
        not <| Set.contains (x,y,z) lavaDroplets

// x = -2 is out of bounds
isNeighbour (surroundingCube exampleDroplet) (exampleDroplet |> Set) (-2,-1,-1)
// Inside bounds but not droplet
isNeighbour (surroundingCube exampleDroplet) (exampleDroplet |> Set) (4,2,2)
// Inside bounds but droplet
isNeighbour (surroundingCube exampleDroplet) (exampleDroplet |> Set) (2,2,2)
// x = 5 is out of bounds
isNeighbour (surroundingCube exampleDroplet) (exampleDroplet |> Set) (5,2,2)

let neighbours (limitingCube: int*int*int) (lavaDroplets: Set<int*int*int>) (x,y,z) = 
    sides (x, y, z)
    |> List.filter (isNeighbour limitingCube lavaDroplets)

(3,2,6) |> neighbours (surroundingCube exampleDroplet) (exampleDroplet |> Set)

// some position known to be air
let startSet = (-1,-1,-1) |> Set.singleton

type NodeSet<'node when 'node : comparison> = Set<'node>

let shortestPath neighbours startExploring = 

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

let exampleNeighbours = neighbours (surroundingCube exampleDroplet) (exampleDroplet |> Set)
let actualNeighbours = neighbours (surroundingCube  droplet) (droplet |> Set)
let visited = shortestPath actualNeighbours startSet |> snd

// räkna ut hur många "faces" som gränsar mot de "visited"

let countExteriorFaces droplet =
    visited
    |> Set.toList
    |> List.fold (fun exteriorFaces air ->
        let sides = sides air |> Set
        let touches = sides |> Set.intersect droplet |> Set.count
        exteriorFaces + touches
    ) 0

countExteriorFaces (droplet |> Set)
firstStar


// 1692 That's not the right answer; your answer is too low

let secondStar = 
    0

secondStar

