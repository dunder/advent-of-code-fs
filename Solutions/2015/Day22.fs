module AoC.E2015.Day22

open AoC
open IO

// --- Day 22: Wizard Simulator 20XX ---

let input = readInputLines "2015" "Day22"

let parseNumber (lines:seq<string>, startsWith:string) =
    let line = lines |> Seq.find (fun l -> l.StartsWith(startsWith))
    line.[(line |> Seq.findIndex (fun c -> c = ':'))+2..] |> int

let bossHitPoints = parseNumber(input, "Hit Points")
let bossDamage = parseNumber(input, "Damage")

type SpellType = MagicMissile | Drain | Shield | Poison | Recharge
type Spell = { Type: SpellType; Cost: int; Damage: int; Armor: int; Healing: int; Mana: int; Duration: int }

let magicMissile = { Type = MagicMissile; Cost = 53;  Damage = 4; Armor = 0; Healing = 0; Mana = 0;   Duration = 1 }
let drain =        { Type = Drain;        Cost = 73;  Damage = 2; Armor = 0; Healing = 2; Mana = 0;   Duration = 1 }
let shield =       { Type = Shield;       Cost = 113; Damage = 0; Armor = 7; Healing = 0; Mana = 0;   Duration = 6 }
let poison =       { Type = Poison;       Cost = 173; Damage = 3; Armor = 0; Healing = 0; Mana = 0;   Duration = 6 }
let recharge =     { Type = Recharge;     Cost = 229; Damage = 0; Armor = 0; Healing = 0; Mana = 101; Duration = 5 }


let spells = [ magicMissile; drain; shield; poison; recharge ]

type GameState = { 
    PlayersTurn: bool
    BossHitPoints: int
    PlayerHitPoints: int
    Mana: int
    ActiveSpells: seq<Spell>
}
    
let applyEffects (state:GameState) = 
    let damageEffect = state.ActiveSpells |> Seq.sumBy (fun s -> s.Damage)
    let manaEffect = state.ActiveSpells |> Seq.sumBy (fun s -> s.Mana)
    let healingEffect = state.ActiveSpells |> Seq.sumBy (fun s -> s.Healing)

    let activeSpells = 
        state.ActiveSpells 
        |> Seq.map (fun spell -> 
            {
                spell with
                    Duration = spell.Duration - 1
            }
        )
        |> Seq.filter (fun spell -> spell.Duration > 0)

    {
        state with
            BossHitPoints = state.BossHitPoints - damageEffect
            PlayerHitPoints = state.PlayerHitPoints + healingEffect
            Mana = state.Mana + manaEffect
            ActiveSpells = activeSpells
    }

let castSpell spell (state:GameState) = 
    {
        state with
            Mana = state.Mana - spell.Cost
            PlayersTurn = false
            ActiveSpells = seq { 
                yield! state.ActiveSpells
                yield spell
            }
    }

let bossAttack (bossDamage:int) (state:GameState) =
    let armorEffect = state.ActiveSpells |> Seq.sumBy (fun spell -> spell.Armor)
    let reduction = max 1 (bossDamage - armorEffect)
    {
        state with
            PlayersTurn = true
            PlayerHitPoints = state.PlayerHitPoints - reduction
    }

let rec fight (spent:int) (maxSpent:int) (hard:bool) (state:GameState) (bossDamage:int): seq<int option> =
    let state = state |> applyEffects

    let maxSpent = if state.BossHitPoints <= 0 then min spent maxSpent else maxSpent
    
    if state.BossHitPoints <= 0 then 
        if spent > maxSpent then
            Seq.singleton None
        else
            Seq.singleton (Some(spent))
    else 
        if state.PlayersTurn then
            let state = if hard then { state with PlayerHitPoints = state.PlayerHitPoints - 1 } else state
            if state.PlayerHitPoints <= 0 then
                Seq.singleton None
            else 
                let affordableNonActiveSpells = 
                    spells
                    |> Seq.filter (fun spell -> not (state.ActiveSpells |> Seq.exists (fun s -> s.Type = spell.Type)))
                    |> Seq.filter (fun spell -> spell.Cost <= state.Mana)
                    |> List.ofSeq

                match affordableNonActiveSpells with
                | [] -> Seq.singleton None
                | spells -> 
                    seq {
                        for spell in spells do
                            let spent = spent + spell.Cost
                            let state = castSpell spell state
                            yield! fight spent maxSpent hard state bossDamage
                    }
        else 
            let state = bossAttack bossDamage state
            if state.PlayerHitPoints <= 0 then
                Seq.singleton None
            else
                fight spent maxSpent hard state bossDamage
        

let firstStar () =

    let initialGameState = {
        PlayersTurn = true
        BossHitPoints = bossHitPoints
        PlayerHitPoints = 50
        Mana = 500
        ActiveSpells = Seq.empty
    }
    
    fight 0 System.Int32.MaxValue false initialGameState bossDamage |> Seq.choose (fun x -> x) |> Seq.min
    
let secondStar () = 
    
    let initialGameState = {
        PlayersTurn = true
        BossHitPoints = bossHitPoints
        PlayerHitPoints = 50
        Mana = 500
        ActiveSpells = Seq.empty
    }

    fight 0 System.Int32.MaxValue true initialGameState bossDamage |> Seq.choose (fun x -> x) |> Seq.min


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(1824, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(-1, secondStar())

    [<Fact>]
    let ``verify first example scenario`` () =
        let state = 
            {
                PlayersTurn = true
                BossHitPoints = 13
                PlayerHitPoints = 10
                Mana = 250
                ActiveSpells = Seq.empty
            }
            |> applyEffects
            |> castSpell poison
            |> applyEffects 
            |> bossAttack 8
            |> applyEffects
            |> castSpell magicMissile
            |> applyEffects


        Assert.Equal(0, state.BossHitPoints)
        Assert.Equal(2, state.PlayerHitPoints)
        Assert.Equal(24, state.Mana)

    [<Fact>]
    let ``verify second example scenario`` () =
        let state = 
            {
                PlayersTurn = true
                BossHitPoints = 14
                PlayerHitPoints = 10
                Mana = 250
                ActiveSpells = Seq.empty
            }
            |> applyEffects
            |> castSpell recharge 
            |> applyEffects 
            |> bossAttack 8
            |> applyEffects
            |> castSpell shield
            |> applyEffects
            |> bossAttack 8
            |> applyEffects
            |> castSpell drain
            |> applyEffects
            |> bossAttack 8
            |> applyEffects
            |> castSpell poison
            |> applyEffects
            |> bossAttack 8
            |> applyEffects
            |> castSpell magicMissile
            |> applyEffects


        Assert.Equal(-1, state.BossHitPoints)
        Assert.Equal(1, state.PlayerHitPoints)
        Assert.Equal(114, state.Mana)

