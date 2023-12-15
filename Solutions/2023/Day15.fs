module AoC.E2023.Day15

// --- Day 15: Lens Library ---

open AoC
open IO

open System.Text.RegularExpressions
let input = readInputText "2023" "Day15"

let hash (text: string) = text |> Seq.fold(fun current c -> (c |> int |> (+) current |> (*) 17) % 256) 0

let sumHashes (input : string) = input.Split ',' |> Seq.map hash |> Seq.sum

type Lens = { Label: string; FocalLength: int }

let (|Regex|_|) pattern input = 
    let m = Regex.Match(input, pattern)

    if m.Success then 
        Some(List.tail [for g in m.Groups -> g.Value])
    else
        None
let addOrRemove label focalLength (boxes: Map<int,list<Lens>>) =
    let box = hash label
    match boxes |> Map.tryFind box with
    | Some (lenses) ->
        let newLens = {Label = label; FocalLength = focalLength}
        let newLenses =
            if lenses |> List.exists (fun lens -> lens.Label = label) then
                lenses 
                |> List.map (fun oldLens -> if oldLens.Label = label then newLens else oldLens)   
            else
                lenses @[newLens]
        boxes |> Map.add box newLenses
    | None ->
        boxes |> Map.add box [{ Label = label; FocalLength = focalLength}]

let remove label (boxes: Map<int,list<Lens>>) =
    let box = hash label
    match boxes |> Map.tryFind box with
    | Some (lenses) ->
        boxes |> Map.add box (lenses |> List.filter (fun lens -> lens.Label <> label))
    | None -> 
        boxes
let toCommand command = 
    match command with 
    | Regex "(.+)=(\d)" [label; focalLength] -> addOrRemove label (focalLength |> int)
    | Regex "(.+)-" [label] -> remove label
    | _ -> failwithf "Unrecognized command: %s" command
let execute (boxes: Map<int, list<Lens>>) (command: Map<int, list<Lens>> -> Map<int, list<Lens>>) = 
    command boxes
let focusingPower (box, slot, focalLength) = (box + 1) * slot * focalLength
let toLensSlots (box, lenses) = lenses |> List.mapi (fun i lens -> box, i + 1, lens.FocalLength)

let focusingPowerOfConfiguration (input: string) =
    input.Split ',' 
    |> Seq.map toCommand 
    |> Seq.fold execute Map.empty
    |> Map.toList
    |> List.collect toLensSlots
    |> List.map focusingPower
    |> List.sum

let firstStar () =
    input |> sumHashes

let secondStar () = 
    input |> focusingPowerOfConfiguration
    