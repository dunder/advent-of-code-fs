// --- Day 11: Dumbo Octopus ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day11.txt") |> List.ofSeq

let example = [
    "5483143223"
    "2745854711"
    "5264556173"
    "6141336146"
    "6357385478"
    "4167524645"
    "2176841721"
    "6882881134"
    "4846848554"
    "5283751526"
]

let withinBounds width height (row, column) = row >= 0 && row < height && column >= 0 && column < width

let adjacent (a:'a[,]) p =
    let row, column = p
    let height = a.GetLength(0)
    let width = a.GetLength(1)
    
    let north = row, column - 1
    let northEast = row + 1, column - 1
    let east = row + 1, column
    let southEast = row + 1, column + 1
    let south = row, column + 1
    let southWest = row - 1, column + 1
    let west = row - 1, column
    let northWest = row - 1, column - 1
    
    let within = withinBounds width height
    
    let directions = [north; northEast; east; southEast; south; southWest; west; northWest]
    
    seq {
        for x in 0..directions.Length-1 do
            let direction = directions.[x]
            if within direction then
                yield direction
    }

let adjacentValues (a:'a[,]) p =    
    adjacent a p
    |> Seq.map (fun (x,y) -> a.[x,y])

let allElements (a:'a[,]) =
    seq { 
        for row in 0 .. a.GetLength(0)-1 do
          for column in 0 .. a.GetLength(1)-1 do 
            yield (row, column), a.[row,column] 
    }

let parse (lines:string list) = 
    let rows = lines.Length
    let columns = lines.[0].Length

    
    let inline charToInt c = int c - int '0'

    Array2D.init rows columns (fun row column -> lines.[row].[column] |> charToInt, false)

let exampleMap = parse example

adjacentValues exampleMap (9,9)
adjacent exampleMap (1,1) |> Seq.length

let nextEnergyLevel energyMap = energyMap |> Array2D.map (fun (value, flashed) -> value + 1, flashed)

let flash energyMap =
    let flashEnergy currentMap = 
        currentMap
        |> allElements
        |> Seq.filter (fun (p, (energy, flashed)) -> 
            energy > 9 && (not flashed)
        )
    
    let rec recurseFlash currentMap =
        let withFlasEnergy = flashEnergy currentMap
        if Seq.length withFlasEnergy > 0 then
            withFlasEnergy
            |> Seq.iter (fun (p, (value, flashed)) -> 
                adjacent currentMap p
                |> Seq.iter (fun (row, column) -> 
                    let value, flashed = currentMap.[row,column]
                    currentMap.[row,column] <- value + 1, flashed
                )
                let row, column = p
                currentMap.[row, column] <- value, true

            )
            recurseFlash currentMap
        else
            currentMap

    recurseFlash energyMap

let resetEnergyLevel energyMap = energyMap |> Array2D.map (fun (value, flashed) -> if value > 9 then 0, false else value, false)

let step energyMap =
    energyMap
    |> nextEnergyLevel
    |> flash
    
let countFlashes energyMap = 
    energyMap 
    |> allElements 
    |> Seq.fold (fun state (p, (value, flashed)) -> if flashed then state + 1 else state) 0

let countSteps n energyMap = 
    [1..n]
    |> Seq.fold (fun (map, totalCount) n ->
        let nextMap = step map
        let flashes = countFlashes nextMap
        nextMap |> resetEnergyLevel, totalCount + flashes
    ) (energyMap, 0)


let smallExample = [
        "11111"
        "19991"
        "19191"
        "19991"
        "11111"
    ]

let smallExampleMap = smallExample |> parse

smallExampleMap
let map1 = nextEnergyLevel smallExampleMap
map1
let map2 = flash map1
map2
let count = countFlashes map2
let map3 = resetEnergyLevel map2
map3


step smallExampleMap
resetEnergyLevel (step smallExampleMap)

let step1 = step exampleMap
let step2 = step step1

exampleMap |> countSteps 100
smallExampleMap |> countSteps 2

let firstStar =
    parse input
    |> countSteps 100
    
firstStar   

let isSynchronizedFlash energyMap =
    energyMap
    |> allElements
    |> Seq.forall (fun (p, (energy, flashed)) -> flashed)



let secondStar = 
    let step0 = parse input
    Seq.unfold (fun state -> 
        let energyMap, stepNr = state
        let nextFlashState = energyMap |> step

        if isSynchronizedFlash nextFlashState then
            None
        else
            Some (state, (nextFlashState |> resetEnergyLevel, stepNr + 1))
    ) (step0, 0)
    |> Seq.last


secondStar

