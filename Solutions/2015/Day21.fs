module AoC.E2015.Day21

open AoC
open Combinatorics
open IO

// --- Day 21: RPG Simulator 20XX ---

let input = readInputLines "2015" "Day21"

let parseNumber (lines:seq<string>, startsWith:string) =
    let line = lines |> Seq.find (fun l -> l.StartsWith(startsWith))
    line.[(line |> Seq.findIndex (fun c -> c = ':'))+2..] |> int

let bossHitPoints = parseNumber(input, "Hit Points")
let bossDamage = parseNumber(input, "Damage")
let bossArmor = parseNumber(input, "Armor")

type Equipment = { Name:string; Cost:int; Damage:int; Armor: int }
type EquipmentSet = { Equipment:seq<Equipment> } with
    member x.Cost = x.Equipment |> Seq.sumBy (fun e -> e.Cost)
    member x.Damage = x.Equipment |> Seq.sumBy (fun e -> e.Damage)
    member x.Armor = x.Equipment |> Seq.sumBy (fun e -> e.Armor)

type FightConfig = { Damage:int; Armor: int }

let weapons = [
    { Name = "Dagger     "; Cost =  8; Damage = 4; Armor = 0 }
    { Name = "Shortsword "; Cost = 10; Damage = 5; Armor = 0 }
    { Name = "Warhammer  "; Cost = 25; Damage = 6; Armor = 0 }
    { Name = "Longsword  "; Cost = 40; Damage = 7; Armor = 0 }
    { Name = "Greataxe   "; Cost = 74; Damage = 8; Armor = 0 }
]

let armor = [
    { Name = "Leather   "; Cost =  13; Damage = 0; Armor = 1 }
    { Name = "Chainmail "; Cost =  31; Damage = 0; Armor = 2 }
    { Name = "Splintmail"; Cost =  53; Damage = 0; Armor = 3 }
    { Name = "Bandedmail"; Cost =  75; Damage = 0; Armor = 4 }
    { Name = "Platemail "; Cost = 102; Damage = 0; Armor = 5 }
]

let rings = [
    { Name = "Damage +1 "; Cost =  25; Damage = 1; Armor = 0 }
    { Name = "Damage +2 "; Cost =  50; Damage = 2; Armor = 0 }
    { Name = "Damage +3 "; Cost = 100; Damage = 3; Armor = 0 }
    { Name = "Defense +1"; Cost =  20; Damage = 0; Armor = 1 }
    { Name = "Defense +2"; Cost =  40; Damage = 0; Armor = 2 }
    { Name = "Defense +3"; Cost =  80; Damage = 0; Armor = 3 }
]

let equipmentSets = 
    let weaponOnly = 
        seq {
            for w in 0..weapons.Length-1 do
                yield { Equipment = [weapons.[w]] }
        }
    let weaponWithArmor = 
         seq {
             for w in 0..weapons.Length-1 do
                 for a in 1..armor.Length-1 do
                     yield { Equipment = [weapons.[w];armor.[a]] }
         }
    
    let weaponOneRing = 
         seq {
             for w in 0..weapons.Length-1 do
                 for r in 0..rings.Length-1 do
                     yield {Equipment = [weapons.[w];rings.[r]]}
         }
    
    let ringCombinations = combinations 2 rings
    
    let weaponTwoRings = 
         seq {
             for w in 0..weapons.Length-1 do
                 for r in 0..ringCombinations.Length-1 do
                     yield { Equipment = weapons.[w]::ringCombinations.[r] }
         }
    
    let weaponArmorOneRing = 
        seq {
            for w in 0..weapons.Length-1 do
                for a in 0..armor.Length-1 do
                    for r in 0..rings.Length-1 do
                        yield { Equipment = [weapons.[w];armor.[a];rings.[r]] }
        }
    
    let weaponArmorTwoRings = 
        seq {
            for w in 0..weapons.Length-1 do
                for a in 0..armor.Length-1 do
                    for r in 0..ringCombinations.Length-1 do
                        yield { Equipment = weapons.[w]::armor.[a]::ringCombinations.[r] }
        }
    
    seq {
        yield! weaponOnly
        yield! weaponOneRing
        yield! weaponTwoRings
        yield! weaponWithArmor
        yield! weaponArmorOneRing
        yield! weaponArmorTwoRings
     }
    
let fightSequence ((boss, player), bossConfig:FightConfig, playerConfig:FightConfig) =

    let reduction armor damage = 
        max 1 (damage - armor)

    Seq.unfold (fun (last, current) -> 
        let boss, player, playerTurn = current
        if playerTurn then
            let r = reduction bossConfig.Armor playerConfig.Damage
            Some (last, (current, (boss - r, player, false)))
        else
            let r = reduction playerConfig.Armor bossConfig.Damage
            Some (last, (current, (boss, player - r, true)))
            
    ) ((boss, player, true), (boss, player, true))

let playerWins ((boss, player), bossConfig:FightConfig, playerConfig:FightConfig) =
    let (_, player, _) = 
        fightSequence ((boss, player), bossConfig, playerConfig)
        |> Seq.find (fun (boss, player, _) -> boss <= 0 || player <=0)
    player > 0


let firstStar () =
    
    let bossConfig = { Damage = bossDamage; Armor = bossArmor}
    let winningEquipment = 
        equipmentSets 
        |> Seq.sortBy (fun e -> e.Cost)
        |> Seq.find (fun e ->
               let playerConfig = { Damage = e.Damage; Armor = e.Armor }
               playerWins ((bossHitPoints, 100), bossConfig, playerConfig)
            )
        
    winningEquipment.Cost

let secondStar () = 
    let bossConfig = { Damage = bossDamage; Armor = bossArmor}
    let winningEquipment = 
        equipmentSets 
        |> Seq.sortByDescending (fun e -> e.Cost)
        |> Seq.find (fun e ->
               let playerConfig = { Damage = e.Damage; Armor = e.Armor }
               not <| playerWins ((bossHitPoints, 100), bossConfig, playerConfig)
            )
    
    winningEquipment.Cost     

module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(121, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(201, secondStar())
        


