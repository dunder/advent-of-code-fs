// --- Day 15: Lens Library ---

open System.IO
open System.Text.RegularExpressions

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadAllText(@".\Input\Day15.txt")

let example = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

let hash (text: string) = text |> Seq.fold(fun current c -> (c |> int |> (+) current |> (*) 17) % 256) 0

let sumHashes (input : string) = input.Split ',' |> Seq.map hash |> Seq.sum


hash "HASH"

sumHashes example

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

let toInput (box, lenses) = lenses |> List.mapi (fun i lens -> box, i + 1, lens.FocalLength)

Map [
    0, [{ Label = "rn"; FocalLength = 1}]
] |> remove "cm"

"rn=1,cm-,qp=3,cm=2".Split ',' 
|> Seq.map toCommand 
|> Seq.fold execute Map.empty

example.Split ',' 
|> Seq.map toCommand 
|> Seq.fold execute Map.empty
|> Map.toList
|> List.collect toInput
|> List.map focusingPower
|> List.sum

let focusingPowerOfConfiguration (input: string) =
    input.Split ',' 
    |> Seq.map toCommand 
    |> Seq.fold execute Map.empty
    |> Map.toList
    |> List.collect toInput
    |> List.map focusingPower
    |> List.sum


let firstStar =
    input |> sumHashes

firstStar

let secondStar = 
    input |> focusingPowerOfConfiguration

secondStar

