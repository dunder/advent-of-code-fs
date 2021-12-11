module AoC.E2021.Day11

// --- Day 11: Dumbo Octopus ---

open AoC
open IO


let input = readInputLines "2021" "Day11" |> List.ofSeq


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

let step energyMap =
    energyMap
    |> nextEnergyLevel
    |> flash
    
let resetEnergyLevel energyMap = 
    energyMap 
    |> Array2D.map (fun (value, flashed) -> if value > 9 then 0, false else value, false)

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


let firstStar () =
    parse input
    |> countSteps 100
    |> snd


let isSynchronizedFlash energyMap =
    energyMap
    |> allElements
    |> Seq.forall (fun (p, (energy, flashed)) -> flashed)

let allValues (a:'a[,]) =
    seq { 
        for row in 0 .. a.GetLength(0)-1 do
          for column in 0 .. a.GetLength(1)-1 do 
            yield a.[row,column] 
    }

let isEndState energyMap =
    energyMap
    |> allValues
    |> Seq.forall (fun (value, flashed) -> value = 0)

let unfoldNextStep state = 
    let energyMap, stepNr = state
    let nextFlashState = energyMap |> step
    Some (state, (nextFlashState |> resetEnergyLevel, stepNr + 1))

let secondStar () = 
    let step0 = parse input
    let endStep =
        Seq.unfold unfoldNextStep (step0, 0)
        |> Seq.find (fun (energyMap, step) -> isEndState energyMap)
    let _, stepNr = endStep
    stepNr