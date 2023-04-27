module AoC.E2022.Day19

// --- Day 19: Not Enough Minerals ---

open System.Collections.Generic
open System.Text.RegularExpressions

open AoC
open IO


let input = readInputLines "2022" "Day19" |> List.ofSeq

type Mineral = | Ore = 0 | Clay = 1 | Obsidian = 2 | Geode = 3

module Mineral =
    let all = [Mineral.Ore; Mineral.Clay; Mineral.Obsidian; Mineral.Geode]

type AssetArray = Map<Mineral, int>

type Blueprint = Map<Mineral, AssetArray>

module AssetArray =

    let addMineral (mineral: Mineral) (assets: AssetArray) = 
        assets |> Map.add mineral (assets[mineral] + 1)

    let multiply (toMultiply: AssetArray) times (assets: AssetArray) =
        assets 
        |> Map.keys 
        |> Seq.map (fun mineral -> mineral, assets[mineral] + times * toMultiply[mineral])
        |> Map.ofSeq

    let remove (toRemove: AssetArray) (assets: AssetArray) =
        assets 
        |> Map.keys 
        |> Seq.map (fun mineral -> mineral, assets[mineral] - toRemove[mineral])
        |> Map.ofSeq

    let canRemove (toRemove: AssetArray) (assets: AssetArray) = assets |> Map.forall (fun mineral value -> value >= toRemove[mineral])

    let create ore clay obsidian geode = [Mineral.Ore, ore; Mineral.Clay, clay; Mineral.Obsidian, obsidian; Mineral.Geode, geode] |> Map.ofList

    let empty = create 0 0 0 0

module Blueprint = 
    let maxCost mineral (blueprint:Blueprint) = 
        blueprint
        |> Map.values
        |> Seq.fold (fun state cost -> max state cost[mineral]) 0

let parse (lines: string list) : list<Blueprint> =

    let toResources line = 
        Regex.Matches(line, @"(\d+)")
        |> Seq.map (fun m -> m.Value)
        |> Seq.map int
        |> Seq.tail
        |> Seq.toList

    let toBluePrint (resources: list<int>) =
        [
            Mineral.Ore, AssetArray.create resources[0] 0 0 0
            Mineral.Clay, AssetArray.create resources[1] 0 0 0
            Mineral.Obsidian, AssetArray.create resources[2] resources[3] 0 0
            Mineral.Geode, AssetArray.create resources[4] 0 resources[5] 0
        ]
        |> Map.ofList

    lines 
    |> List.map (toResources >> toBluePrint)

type MiningConfig = { Blueprint: Blueprint; Rig: AssetArray; Assets: AssetArray; Parent: MiningConfig option; Time: int }

module MiningConfig = 

    let blueprint (mineral: Mineral) config = config.Blueprint[mineral]

    let canAffordRobot mineral config = config.Assets |> AssetArray.canRemove (config |> blueprint mineral)

    let payRobot mineral config =
        let newAssets = config.Assets |> AssetArray.remove (config |> blueprint mineral)        
        { config with Assets = newAssets }

    let manifactureRobot mineral config =
        {config with Rig = config.Rig |> AssetArray.addMineral mineral }

    let canMineRobot mineral config = 
        config.Blueprint[mineral] 
        |> Map.filter (fun mineral cost -> cost > 0) 
        |> Map.keys
        |> Seq.forall (fun mineral -> config.Rig[mineral] > 0)

    let mine time config = 
        { config with Time = config.Time + time; Assets = config.Assets |> AssetArray.multiply config.Rig time }

    let timeToMine mineral config =
        [Mineral.Ore; Mineral.Clay; Mineral.Obsidian]
        |> List.filter (fun purchaseMineral -> config.Blueprint[mineral][purchaseMineral] > 0)
        |> List.map (fun purchaseMineral ->
            let cost = config.Blueprint[mineral][purchaseMineral]
            let robots = config.Rig[purchaseMineral]
            let deficit = max 0 (cost - config.Assets[purchaseMineral])

            let timeToMine = deficit / robots + min 1 (deficit % robots)

            timeToMine
        )
        |> List.max

    let nextPossibleRobotPurchase timeAvailable config mineral =
        let rigAtMaxCapacity = config.Rig[mineral] >= (config.Blueprint |> Blueprint.maxCost mineral)
        if mineral <> Mineral.Geode && rigAtMaxCapacity then
            None
        else
            if config |> canMineRobot mineral then
                let timeLeft = timeAvailable - config.Time
                let miningTime = config |> timeToMine mineral
                let manifactureTime = 1
                            
                if miningTime + manifactureTime <= timeLeft then
                    let newConfig = 
                        config
                        |> mine miningTime
                        |> payRobot mineral
                        |> mine 1
                        |> manifactureRobot mineral
                    Some(newConfig)
                else
                    Some(config |> mine timeLeft)
            else
                None
                
    let options timeAvailable currentBest config =

        let removeUnprofitableOptions timeAvailable currentBest (options: list<MiningConfig>) =
            options
            |> List.filter(fun config ->
                let timeLeft = timeAvailable - config.Time
                let optimal = config.Assets[Mineral.Geode] + timeLeft * config.Rig[Mineral.Geode] + timeLeft * (timeLeft - 1) / 2
                currentBest < optimal
            )

        let purchaseOptions =
            Mineral.all
            |> List.map (nextPossibleRobotPurchase timeAvailable config)
            |> List.choose (id)
        let mineOrPurchaseOptions = 
            if purchaseOptions |> List.isEmpty then
                let timeLeft = timeAvailable - config.Time
                [config |> mine timeLeft] 
            else
                purchaseOptions

        mineOrPurchaseOptions |> removeUnprofitableOptions timeAvailable currentBest

    let identity (config:MiningConfig) = config.Assets, config.Rig

    let isSolution timeAvailable config = config.Time = timeAvailable

let nonVisitedNeighbours timeAvailable currentBest (visited:HashSet<AssetArray*AssetArray>) state =
    state
    |> MiningConfig.options timeAvailable currentBest
    |> List.filter (fun neighbour -> visited.Add(neighbour |> MiningConfig.identity))    


let dfs isSolution nonVisitedNeighbours startState =
    let visited = new HashSet<AssetArray*AssetArray>()
    let stack = new Stack<MiningConfig>()
    let rec bfsSearch best =
        if stack.Count = 0 then
            best
        else
            let head = stack.Pop()
            let newBest = (max best head.Assets[Mineral.Geode])
            if isSolution head then
                bfsSearch newBest
            else
                for neighbour in nonVisitedNeighbours newBest visited head do
                    stack.Push {neighbour with Parent = Some(head)}
                bfsSearch newBest
    
    stack.Push startState
    
    bfsSearch 0

let qualityLevel timeAvailable (id: int) (blueprint: Blueprint) =
    let startSetup = { Assets = AssetArray.empty; Rig = AssetArray.create 1 0 0 0; Blueprint = blueprint; Parent = None; Time = 0 }
    let bestGeodeCount = dfs (MiningConfig.isSolution timeAvailable) (nonVisitedNeighbours timeAvailable) startSetup
    id * bestGeodeCount

let firstStar () =
    parse input
    |> List.indexed
    |> List.map (fun (i, blueprint) -> qualityLevel 24 (i+1) blueprint)
    |> List.sum

let geodeCount timeAvailable (blueprint: Blueprint) =
    let startSetup = { Assets = AssetArray.empty; Rig = AssetArray.create 1 0 0 0; Blueprint = blueprint; Parent = None; Time = 0 }
    dfs (MiningConfig.isSolution timeAvailable) (nonVisitedNeighbours timeAvailable) startSetup


let secondStar () = 
    parse input
    |> List.take 3
    |> List.map (fun blueprint -> geodeCount 32 blueprint)
    |> List.fold (*) 1