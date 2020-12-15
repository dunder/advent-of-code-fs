module AoC.E2020.Day14

open AoC
open IO
open System
open System.Text.RegularExpressions

// --- Day 14: Docking Data ---

let input = readInputLines "2020" "Day14" |> List.ofSeq

type MemoryUpdate = { address: int64; value: int64 }
type InstructionSet = { bitmask: string; instructions: list<MemoryUpdate> }

let parse (lines: list<string>) = 
    lines
    |> List.mapFold (fun acc line -> 
       if line.StartsWith("mask") then
            ((acc + 1,line), acc + 1)
       else
            ((acc,line), acc)
    ) 0
    |> fst
    |> List.groupBy (fun (i, _) -> i)
    |> List.map (snd >> (List.map snd))

let toMemoryUpdate (memoryUpdateDescription: string) =
    let regex = new Regex("mem\[(\d+)\] = (.+)")
    let m = regex.Match(memoryUpdateDescription)
    let address = m.Groups.[1].Value |> int64
    let value = m.Groups.[2].Value |> int64
    { address = address; value = value  }

let toInstruction (instructionGroup: list<string>) =
    let maskLine = instructionGroup |> List.head
    let mask = maskLine.Split(" = ") |> Array.last

    let instructions = 
        instructionGroup 
        |> List.tail
        |> List.map toMemoryUpdate

    { bitmask = mask; instructions = instructions }
   
let maskToSet (mask: string) =
    mask.Replace("X", "0")

let maskToClear (mask: string) = 
    mask.Replace("X", "1")

let memoryWrite (memoryUpdate: MemoryUpdate) (mask: string) (memory: Map<int64, int64>) =

    let maskForSet = maskToSet mask 
    let maskValue = Convert.ToInt64(maskForSet, 2)

    let result = maskValue ||| memoryUpdate.value

    let maskForClear = maskToClear mask
    let maskValueForClear = Convert.ToInt64(maskForClear, 2)

    let next = maskValueForClear &&& result

    memory |> Map.add memoryUpdate.address next

let generateAddresses (mask: string) (address: int64) =
    let maskForSet = maskToSet mask 
    let maskValue = Convert.ToInt64(maskForSet, 2)
    
    let result = maskValue ||| address

    let modifiedMask = 
        Convert.ToString(result, 2).PadLeft(36, '0')
        |> String.mapi (fun i c -> if mask.[i] = 'X' then 'X' else c)

    let idxs = mask |> Seq.mapi (fun i c -> i, c) |> Seq.filter (fun (_, c) -> c = 'X') |> Seq.map fst
    let floatingCount = idxs |> Seq.length
    let times = floatingCount |> float
    let binaryCombinations = (2.0**times |> int)-1

    let applyMask mask idxs binaryCombination = 
        let floatingIndexLookup = binaryCombination |> Seq.zip idxs |> Map.ofSeq
        mask |> String.mapi (fun i c -> 
            if c = 'X' then 
                floatingIndexLookup.[i]
            else
                c
        )

    [0..binaryCombinations] 
    |> List.map (fun x -> Convert.ToString(x, 2).PadLeft(floatingCount, '0'))
    |> List.map (applyMask modifiedMask idxs)
    |> List.ofSeq
    |> List.map (fun binary -> Convert.ToInt64(binary, 2))

let memoryFloatWrite (memoryUpdate: MemoryUpdate) (mask: string) (memory: Map<int64, int64>) =

    generateAddresses mask memoryUpdate.address
    |> List.fold(fun m address -> 
        m |> Map.add address memoryUpdate.value
    ) memory

let run program memoryWriter =

    let evaluate (program: InstructionSet) (state: Map<int64, int64>) memoryWriter =
        program.instructions
        |> List.fold (fun (map: Map<int64, int64>) memoryUpdate -> 
            map |> memoryWriter memoryUpdate program.bitmask
        ) state

    let memory = 
        parse program 
        |> Seq.map toInstruction
        |> Seq.fold (fun (state: Map<int64,int64>) program -> 
            evaluate program state memoryWriter
        ) Map.empty

    let result =
        memory 
        |> List.ofSeq
        |> List.sumBy (fun x -> x.Value)

    result

let firstStar () =

    run input memoryWrite

let secondStar () = 
    
    run input memoryFloatWrite


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(5902420735773L, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(3801988250775L, secondStar())
        


