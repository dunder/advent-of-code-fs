module AoC.E2021.Day17

// --- Day 17: Trick Shot ---

open AoC
open IO
open System.Text.RegularExpressions


let input = readInputText "2021" "Day17"


type XYBoundaries = { XRange: int*int; YRange: int*int }

type ProbeState = { Position: int*int; Velocity: int*int; Alive: bool; Hit: bool; YMax: int }

module TargetArea = 

    let within p target = 
        let x, y = p
        let x1, x2 = target.XRange
        let y1, y2 = target.YRange

        x >= x1 && x <= x2 && y >= y1 && y <= y2

    let isAlive probe target =
        let x, y = probe.Position
        let vx, vy = probe.Velocity
        let x1, x2 = target.XRange
        let y1, y2 = target.YRange

        if y < y1 || x > x2 then
            false
        else 
            if vx = 0 then
                if x < x1 then 
                    false
                else 
                    true
            else 
                true

    let initialVelocityBoundaries targetArea =
        
        let x1, x2 = targetArea.XRange
        let y1, y2 = targetArea.YRange
        
        // must reach target area before speed drops to zero
        let _, _, vxMin = 
            Seq.unfold (fun (x1, x, vx) -> 
                let x' = x + vx
                let vx' = vx + 1
                if x' < x1 then
                    Some ((x1, x, vx), (x1, x', vx'))
                else 
                    None
            ) (x1, 0, 0)
            |> Seq.last
        
        // do not overshoot on first step
        let vxMax = x2 
        
        let accumulatedSum steps = [0..steps] |> Seq.reduce (+)

        // steps to vx = 0 -> vx

        // how much y drop for vxMin?

        // the easy and not so performant solution
        let vyMin = y1

        let yDropvxMax = accumulatedSum vxMax
        let vyMax = y2 + yDropvxMax

        { XRange = vxMin, vxMax; YRange = vyMin, vyMax }

    let candidateVelocities targetArea = 
        let velocityBoundaries = initialVelocityBoundaries targetArea

        let vx1, vx2 = velocityBoundaries.XRange
        let vy1, vy2 = velocityBoundaries.YRange

        seq {
            for vx in vx1..vx2 do
                for vy in vy1..vy2 do
                    yield (vx, vy)
        }

module ProbeState = 

    let init v = { Position = 0,0; Velocity = v; Alive = true; Hit = false; YMax = 0 }

    let step state =
        let x, y = state.Position
        let vx, vy = state.Velocity

        let vx' = if vx < 0 then vx + 1 else if vx > 0 then vx - 1 else 0

        { state with Position = (x + vx, y + vy); Velocity = (vx', vy - 1)}

let parse text =
    let m = Regex.Match(text, @"^target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)$")
    if 
        m.Success
    then 
        let values =
            [for g in m.Groups -> g.Value]
            |> List.tail
            |> List.map System.Int32.Parse
        { XRange = values.[0], values.[1]; YRange = values.[2], values.[3] }
    else 
        failwithf "Bad input: %s" text

let trace targetArea initialVelocity =

    let probe = initialVelocity |> ProbeState.init

    Seq.unfold (fun probe ->
        let movedProbe = probe |> ProbeState.step

        let hit = targetArea |> TargetArea.within movedProbe.Position
        let alive = targetArea |> TargetArea.isAlive movedProbe
        let _, y = movedProbe.Position
        let yMax = max probe.YMax y

        let nextProbe = { movedProbe with Alive = alive; Hit = hit; YMax = yMax }

        Some (nextProbe, nextProbe)
    ) (probe)
    |> Seq.find (fun probe -> not probe.Alive || probe.Hit)


let firstStar () =
    let targetArea = parse input

    TargetArea.candidateVelocities targetArea
    |> Seq.map (trace targetArea)
    |> Seq.filter (fun probe -> probe.Hit)
    |> Seq.maxBy (fun probe -> probe.YMax)


let secondStar () =
    let targetArea = parse input

    TargetArea.candidateVelocities targetArea
    |> Seq.map (trace targetArea)
    |> Seq.filter (fun probe -> probe.Hit)
    |> Seq.length