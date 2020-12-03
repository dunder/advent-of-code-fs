module AoC.E2015.Day22

open AoC
open IO
open Astar

// --- Day 22: Wizard Simulator 20XX ---

let input = readInputLines "2015" "Day22"

let parseNumber (lines:seq<string>, startsWith:string) =
    let line = lines |> Seq.find (fun l -> l.StartsWith(startsWith))
    line.[(line |> Seq.findIndex (fun c -> c = ':'))+2..] |> int

let bossHitPoints = parseNumber(input, "Hit Points")
let bossDamage = parseNumber(input, "Damage")

type SpellType = MagicMissile | Drain | Shield | Posison | Recharge
type Spell = { Type: SpellType; ManaReduction: int }
type EffectType = Shield | Poison | Recharge
type EffectImpact = { Damage: int; Armor: int; Mana: int }
type Effect = { Type: EffectType; Duration: int; Impact: EffectImpact}

type GameState = { 
    PlayersTurn: bool
    BossHitPoints: int
    PlayerHitPoints: int
    Mana: int
    ShieldEffectDuration: int
    PoisonEffectDuration: int
    RechargeEffectDuration: int
}
    
let applyEffects (state:GameState) = 
    let poisonImpact = if state.PoisonEffectDuration > 0 then 3 else 0
    let rechargeImpact = if state.RechargeEffectDuration > 0 then 101 else 0

    {
        state with
            BossHitPoints = state.BossHitPoints - poisonImpact
            Mana = state.Mana + rechargeImpact
            ShieldEffectDuration = max 0 (state.ShieldEffectDuration - 1)
            PoisonEffectDuration = max 0 (state.PoisonEffectDuration - 1)
            RechargeEffectDuration = max 0 (state.RechargeEffectDuration - 1)
    }

let bossAttack (damage:int) (state:GameState) = 
    let shieldImpact = if state.ShieldEffectDuration > 0 then 7 else 0
    let playerHitPointsReduction = max 1 (damage - shieldImpact)
    {
        state with
            PlayersTurn = true
            PlayerHitPoints = state.PlayerHitPoints - playerHitPointsReduction
    }

let castMagicMissile (state:GameState) = 
    {
        state with
            PlayersTurn = false
            BossHitPoints = state.BossHitPoints - 4
            Mana = state.Mana - 53
    }

let castDrain (state:GameState) = 
    {
        state with
            PlayersTurn = false
            BossHitPoints = state.BossHitPoints - 2
            PlayerHitPoints = state.PlayerHitPoints + 2
            Mana = state.Mana - 73
    }

let castShield (state:GameState) = 
    {
        state with
            PlayersTurn = false
            Mana = state.Mana - 113
            ShieldEffectDuration = 6
    }

let castPoison (state:GameState) = 
    {
        state with 
            PlayersTurn = false
            Mana = state.Mana - 173
            PoisonEffectDuration = 6
    }
    
let castRecharge (state:GameState) = 
    {
        state with
            PlayersTurn = false
            Mana = state.Mana - 229
            RechargeEffectDuration = 5
    }

let nextPossibleStates (state:GameState) =
    let state = state |> applyEffects
    if state.PlayerHitPoints <= 0 || state.BossHitPoints <= 0 then
        Seq.empty
    else
        if state.PlayersTurn then
            let magicMissile = castMagicMissile state
            let drain = castDrain state
            let shield = castShield state
            let poison = castPoison state
            let recharge = castRecharge state
            seq { 
                if magicMissile.Mana >= 0 then
                    yield magicMissile
                if drain.Mana >= 0 then
                    yield drain
                if state.ShieldEffectDuration = 0 && shield.Mana >= 0 then
                    yield shield
                if state.PoisonEffectDuration = 0 && poison.Mana >= 0 then
                    yield poison
                if state.RechargeEffectDuration = 0 && recharge.Mana >= 0 then
                    yield recharge
            }
        else
            Seq.singleton (bossAttack bossDamage state)
                

let firstStar () =

    let initialGameState = {
        PlayersTurn = true
        BossHitPoints = bossHitPoints
        PlayerHitPoints = 50
        Mana = 250
        ShieldEffectDuration = 0
        PoisonEffectDuration = 0
        RechargeEffectDuration = 0
    }
    
    let p =
        let config : Algorithm.Config<_> =
        {
            heuristic = fun state -> state.Mana
            neighbours = nextPossibleStates
            distance = fun fromState toState -> fromState.Mana - toState.Mana
            isGoal = fun state -> state.BossHitPoints <= 0
        }
        in config |> Algorithm.aStar initialGameState 

    0

let secondStar () = 
    
    0  

module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(-1, firstStar())

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
                ShieldEffectDuration = 0
                PoisonEffectDuration = 0
                RechargeEffectDuration = 0
            }
            |> applyEffects
            |> castPoison 
            |> applyEffects 
            |> bossAttack 8
            |> applyEffects
            |> castMagicMissile
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
                ShieldEffectDuration = 0
                PoisonEffectDuration = 0
                RechargeEffectDuration = 0
            }
            |> applyEffects
            |> castRecharge 
            |> applyEffects 
            |> bossAttack 8
            |> applyEffects
            |> castShield
            |> applyEffects
            |> bossAttack 8
            |> applyEffects
            |> castDrain
            |> applyEffects
            |> bossAttack 8
            |> applyEffects
            |> castPoison
            |> applyEffects
            |> bossAttack 8
            |> applyEffects
            |> castMagicMissile
            |> applyEffects


        Assert.Equal(-1, state.BossHitPoints)
        Assert.Equal(1, state.PlayerHitPoints)
        Assert.Equal(114, state.Mana)

